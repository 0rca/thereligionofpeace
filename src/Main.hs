{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Environment (getArgs)
import Control.Arrow
import Web.Scotty
import Data.Aeson.Types

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

-- takes a html text and returns a list of
-- [date,country,city,killed,injured,description] :: [String]
extractRows :: String -> [[String]]
extractRows =
    parseTags                                       >>> -- [a]
    canonicalizeTags                                >>>
    partitions (~== table_)                         >>> -- [[a]]
    filter isDataTable                              >>>
    concatMap tail                                  >>> -- [a]
    partitions (~== tr_)                            >>> -- [[a]]
    (map $ partitions (~== td_))                    >>> -- [[[a]]]
    (map.map $ filter isTagText)                    >>> -- [[[a]]]
    (map.map.map $ unwords.words.fromTagText)       >>> -- [[[String]]]
    (map.map $ fromMaybe "" . listToMaybe)              -- [[String]]


loadData :: FilePath -> IO [[String]]
loadData filepath =
    withFile filepath ReadMode $ \h -> do
        hSetEncoding h latin1
        putStrLn $ "Reading " ++ filepath
        liftM extractRows $ S.hGetContents h

type CrimeData = [String]

dictionaryWithKey :: (CrimeData -> String) -> [CrimeData] -> M.Map String [CrimeData]
dictionaryWithKey keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs] where

data Example = Example {name :: String, age :: Int}

instance ToJSON Example where
    toJSON (Example name age) = object ["name" .= name, "age" .= age]

main :: IO ()
main = do
    filenames <- getArgs
    parts <- mapM loadData filenames

    let rows = filter (not.null) $ concat parts

    putStrLn "Building dictionaries..."

    let countriesDict = dictionaryWithKey (!!1) rows
    let citiesDict    = dictionaryWithKey (\rec -> rec!!2 ++ ", " ++ rec!!1) rows

    -- force evaluation of dictionaries
    putStrLn $ "Total rows: " ++ (show.length) rows
    putStrLn $ "Total countries: " ++ (show.length) (M.keys countriesDict)
    putStrLn $ "Total cities: " ++ (show.length) (M.keys citiesDict)

    scotty 3000 $ do
        get "/cities" $ json $ M.keys citiesDict
        get "/countries" $ json $ M.keys countriesDict
        get "/countries/:country" $ do
            country <- param "country"
            -- limit <- param "limit" `rescue` (\_ -> return 10)
            json $ fromMaybe [] $ M.lookup country countriesDict

        get "/countries/:country/:city" $ do
            country <- param "country"
            city <- param "city"
            let byCity = (==city).(!!2)
            json $ filter byCity $ fromMaybe [] $ M.lookup country countriesDict


        get "/example" $ json $ Example "Hemingway" 21

