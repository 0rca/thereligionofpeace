{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (liftM)
import Control.Applicative
import Data.Maybe (fromMaybe,listToMaybe)
import System.Environment (getArgs)
import Control.Arrow
import Web.Scotty
import Data.Aeson.Types
import Data.Hourglass
import Data.Maybe
import Data.Text (pack)
import Data.Function (on)
import Data.List (sort)

import qualified Data.Map.Lazy as M

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

fromRow :: [String] -> Maybe Attack
fromRow r = Attack <$>
                    (readDate $ r!!0) <*>
                    Just (r!!1) <*>
                    Just (r!!2) <*>
                    (readInt $ r!!3) <*>
                    (readInt $ r!!4) <*>
                    Just (r!!5)

loadData :: FilePath -> IO [[String]]
loadData filepath =
    withFile filepath ReadMode $ \h -> do
        hSetEncoding h latin1
        putStrLn $ "Reading " ++ filepath
        liftM extractData $ S.hGetContents h

dictionaryWithKey :: (Attack -> String) -> [Attack] -> M.Map String [Attack]
dictionaryWithKey keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs] where

data Example = Example {name :: String, age :: Int}

instance ToJSON Example where
    toJSON (Example name age) = object ["name" .= name, "age" .= age]

main :: IO ()
main = do
    filenames <- getArgs
    parts <- mapM loadData filenames

    let attacks = sort $ map fromJust $ map fromRow $ filter (not.null) $ concat parts

    putStrLn "Building dictionaries..."

    let countriesDict = dictionaryWithKey country attacks
    let citiesDict    = dictionaryWithKey city attacks

    -- force evaluation of dictionaries
    putStrLn $ "Total attacks: " ++ (show.length) attacks
    putStrLn $ "In countries: " ++ (show.length) (M.keys countriesDict)
    putStrLn $ "In cities: " ++ (show.length) (M.keys citiesDict)
    putStrLn $ "Total killed: " ++ (show.sum.map killed) attacks
    putStrLn $ "Total injured: " ++ (show.sum.map injured) attacks

    scotty 3000 $ do
        get "/cities" $ json $ M.keys citiesDict
        get "/cities/:city" $ do
            cityP <- param "city"
            json $ fromMaybe [] $ M.lookup cityP citiesDict

        get "/countries" $ json $ M.keys countriesDict
        get "/countries/:country" $ do
            countryP <- param "country"
            -- limit <- param "limit" `rescue` (\_ -> return 10)
            json $ fromMaybe [] $ M.lookup countryP countriesDict

        get "/countries/:country/:city" $ do
            countryP <- param "country"
            cityP <- param "city"
            let byCity = \x -> cityP == city x
            json $ filter byCity $ fromMaybe [] $ M.lookup countryP countriesDict


        get "/example" $ json $ Example "Hemingway" 21

