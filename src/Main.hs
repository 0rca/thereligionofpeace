{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import System.IO
import qualified System.IO.Strict as S (hGetContents)
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
import Web.Scotty
import Network.Wai.Middleware.RequestLogger

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
                     } deriving (Eq,Show)

instance Ord Attack where
    compare a b = compare (date a) (date b)

instance ToJSON Attack where
    toJSON (Attack date country city killed injured description) = object $ [
        "date" .= date,
        "country" .= country,
        "city" .= city,
        "killed" .= killed,
        "injured" .= injured,
        "description" .= description ]

instance ToJSON DateTime where
    toJSON time = String $ pack $ timePrint ISO8601_Date time

type Lens a s = forall f.Functor f => (a -> f a) -> s -> f s

-- takes a html text and returns a list of attacks
extractData :: String -> [[String]]
extractData = map normaliseColumns.map extractColumns.extractRows.extractTable.extractTags

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
attackFromColumns r = Attack <$>
                    (readDate $ r!!0) <*>
                    Just (r!!1) <*>
                    Just (r!!2) <*>
                    (readInt $ r!!3) <*>
                    (readInt $ r!!4) <*>
                    Just (r!!5)

getContentsOf :: FilePath -> IO String
getContentsOf filepath =
    withFile filepath ReadMode $ \h -> do
        hSetEncoding h latin1
        putStrLn $ "Reading " ++ filepath
        S.hGetContents h

type Dict = M.Map String [Attack]

dictionaryWithKey :: (Attack -> String) -> [Attack] -> Dict
dictionaryWithKey keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs] where

paginate   :: [a] -> ActionM [a]
paginate xs = do
    limit <- param "limit" `rescue` (return . const 25)
    page  <- param "page"  `rescue` (return . const 0)
    return $ paginate' page limit xs where
        paginate' page limit = take limit . drop (page * limit)

filterByCity   :: [Attack] ->ActionM [Attack]
filterByCity xs = do city' <- param "city"
                     return . filter (\x ->city' == city x) $ xs

-- lookupParam  :: String -> Dict -> ActionM [Attack]
lookupParam n dict = do x <- param n
                        return $ list x dict

lookupCountry :: Dict -> ActionM [Attack]
lookupCountry  = lookupParam "country"

lookupCity :: Dict -> ActionM [Attack]
lookupCity  = lookupParam "city"

list :: String -> Dict -> [Attack]
list k = fromMaybe [] . M.lookup k

attacks :: [FilePath] -> IO [Attack]
attacks files = do
    contents <- concat <$> mapM getContentsOf files
    return . sort . map (fromJust . attackFromColumns) . filter (not . null) . extractData $ contents

loadAll :: [FilePath] -> IO (Dict,Dict)
loadAll files = do
    atks <- attacks files

    putStrLn "Building dictionaries..."

    let countriesDict = dictionaryWithKey country atks
    let citiesDict    = dictionaryWithKey city atks

    -- force evaluation of dictionaries
    putStrLn $ "Total attacks: " ++ (show . length) atks
    putStrLn $ "In countries: " ++ (show . length) (M.keys countriesDict)
    putStrLn $ "In cities: " ++ (show . length) (M.keys citiesDict)
    putStrLn $ "Total killed: " ++ (show . sum . map killed) atks
    putStrLn $ "Total injured: " ++ (show . sum . map injured) atks
    return (countriesDict,citiesDict)


startServer :: Int -> Dict -> Dict -> IO ()
startServer port citiesDict countriesDict = scotty port $ do
    middleware logStdoutDev

    get "/cities" . json . M.keys $ citiesDict
    get "/countries" . json . M.keys $ countriesDict

    get "/cities/:city" $
        lookupCity citiesDict >>= paginate >>= json

    get "/countries/:country" $
        lookupCountry countriesDict >>= paginate >>= json

    get "/countries/:country/:city" $
        lookupCountry countriesDict >>= filterByCity >>= paginate >>= json

    get "/headers" $ headers >>= return.map (\(k,v) -> k `LT.append` (LT.pack ": ") `LT.append` v) >>= json


main :: IO ()
main = do
    (countriesDict,citiesDict) <- getArgs >>= loadAll
    startServer 3000 citiesDict countriesDict

