{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import System.IO
import System.IO.Strict as S
import System.Environment (getArgs)

import Control.Monad (liftM)
import Control.Applicative

import Data.Aeson.Types
import Data.Hourglass
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text.Lazy as LT
import Data.List (sort)
import qualified Data.Map.Strict as M

import Text.HTML.TagSoup
import qualified Web.Scotty as WS
import Network.Wai.Middleware.RequestLogger

import Control.Monad.State.Strict

-- isDataTable == table with a header
table_ :: String
table_ = "<table>"

th_ :: String
th_    = "<th>"

tr_ :: String
tr_    = "<tr>"

td_ :: String
td_    = "<td>"

isDataTable :: [Tag String] -> Bool
isDataTable = not.null.partitions (~== th_).head.partitions (~== tr_)

data Attack = Attack { date :: DateTime
                     , country :: String
                     , city :: String
                     , killed :: Integer
                     , injured :: Integer
                     , description :: String
                     }
                     deriving (Eq,Show)

instance Ord Attack where
    compare a b = compare (date a) (date b)

instance ToJSON Attack where
    toJSON (Attack date country city killed injured description) = object
      ["date" .= date
      ,"country" .= country
      ,"city" .= city
      ,"killed" .= killed
      ,"injured" .= injured
      ,"description" .= description
      ]

instance ToJSON DateTime where
    toJSON time = String $ pack $ timePrint ISO8601_Date time

type Lens a s = forall f.Functor f => (a -> f a) -> s -> f s

-- takes a html text and returns a list of attacks
extractData :: String -> [[String]]
extractData = map (normaliseColumns . extractColumns) . extractRows . extractTable . extractTags

extractTags :: String -> [Tag String]
extractTags = canonicalizeTags.parseTags

extractTable :: [Tag String] -> [Tag String]
extractTable = concatMap tail . filter isDataTable . partitions (~== table_)

extractRows :: [Tag String] -> [[Tag String]]
extractRows = partitions (~== tr_)

extractColumns :: [Tag String] -> [[String]]
extractColumns = map (map (unwords.words.fromTagText).filter isTagText).partitions (~== td_)

normaliseColumns :: [[String]] -> [String]
normaliseColumns = map $ fromMaybe "" . listToMaybe

readInt :: String -> Maybe Integer
readInt "" = Just 0
readInt s  = Just $ read s

readDate :: String -> Maybe DateTime
readDate  = timeParse [Format_Year4,Format_Text '.',Format_Month2, Format_Text '.', Format_Day2]

attackFromColumns :: [String] -> Maybe Attack
attackFromColumns r = Attack
  <$> readDate (head r)
  <*> Just (r!!1)
  <*> Just (r!!2)
  <*> readInt (r!!3)
  <*> readInt (r!!4)
  <*> Just (r!!5)

type Dict = M.Map String [Attack]

mkDict :: (Attack -> String) -> [Attack] -> Dict
mkDict keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs] where

paginate   :: [a] -> WS.ActionM [a]
paginate xs = do
    limit <- WS.param "limit" `WS.rescue` (return . const 25)
    page  <- WS.param "page"  `WS.rescue` (return . const 0)
    return $ paginate' page limit xs where
        paginate' page limit = take limit . drop (page * limit)

filterByCity :: [Attack] ->WS.ActionM [Attack]
filterByCity xs = liftM filterCities $ WS.param "city" where
  filterCities c = filterOn city (c==) xs
  filterOn f p = filter (p . f)

lookupParam x dict = liftM (`find` dict) $ WS.param x where
  find k = fromMaybe [] . M.lookup k

lookupCountry :: Dict -> WS.ActionM [Attack]
lookupCountry  = lookupParam "country"

lookupCity :: Dict -> WS.ActionM [Attack]
lookupCity  = lookupParam "city"

readAttacks :: FilePath -> StateT [Attack] IO ()
readAttacks fname = do
  a <- liftIO $ withFile fname ReadMode $ \h -> do
    hSetEncoding h latin1
    putStrLn $ "Reading " ++ fname
    S.hGetContents h

  let attacks = map (fromJust . attackFromColumns) . filter (not . null) . extractData $ a
  modify' (mappend attacks)

loadAll :: [FilePath] -> IO (Dict,Dict)
loadAll files = do
    unsorted <- execStateT (mapM_ readAttacks files) []
    let atks = sort unsorted

    putStrLn "Building dictionaries..."

    let countriesDict = mkDict country atks
    let citiesDict    = mkDict city atks

    -- force evaluation of dictionaries
    putStrLn $ "Total attacks: " ++ (show . length) atks
    putStrLn $ "In countries: " ++ (show . length) (M.keys countriesDict)
    putStrLn $ "In cities: " ++ (show . length) (M.keys citiesDict)
    putStrLn $ "Total killed: " ++ (show . sum . map killed) atks
    putStrLn $ "Total injured: " ++ (show . sum . map injured) atks
    return (countriesDict,citiesDict)


startServer :: Int -> (Dict, Dict) -> IO ()
startServer port (countriesDict,citiesDict) = WS.scotty port $ do
    WS.middleware logStdoutDev

    WS.get "/cities" . WS.json . M.keys $ citiesDict
    WS.get "/countries" . WS.json . M.keys $ countriesDict

    WS.get "/cities/:city" $
        lookupCity citiesDict >>= paginate >>= WS.json

    WS.get "/countries/:country" $
        lookupCountry countriesDict >>= paginate >>= WS.json

    WS.get "/countries/:country/:city" $
        lookupCountry countriesDict >>= filterByCity >>= paginate >>= WS.json

    WS.get "/headers" $ liftM (map (\(k,v) -> k `LT.append` LT.pack ": " `LT.append` v)) WS.headers >>= WS.json


main :: IO ()
main = getArgs >>= loadAll >>= startServer 3000
