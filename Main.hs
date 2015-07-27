{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Environment (getArgs)
import Control.Arrow

import Web.Scotty

import qualified Data.Map.Strict as Map

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

type Dictionary = Map.Map String [[String]]

populateDictionary :: ([String] -> (String,[String])) -> [[String]] -> Dictionary
populateDictionary f = foldl (\acc x -> let (k,v) = f x in Map.insertWith (++) k [v] acc) Map.empty

main :: IO ()
main = do
    filenames <- getArgs
    parts <- mapM loadData filenames

    let rows = filter (not.null) $ concat parts

    putStrLn "Building dictionaries..."

    let countriesDict = populateDictionary (\row@(_:c:_) -> (c,row)) rows
    let citiesDict    = populateDictionary (\row@(_:co:ci:_) -> (ci ++ ", " ++ co, row)) rows

    -- force evaluation of dictionaries
    putStrLn $ "Total rows: " ++ (show.length) rows
    putStrLn $ "Total countries: " ++ (show.length) (Map.keys countriesDict)
    putStrLn $ "Total cities: " ++ (show.length) (Map.keys citiesDict)

    scotty 3000 $ do
        get "/countries" $ json $ Map.keys countriesDict

        get "/cities" $ json $ Map.keys citiesDict

        get "/:country" $ do
            country <- param "country"
            limit <- param "limit" `rescue` (\_ -> return 10)
            json $ take limit $ fromMaybe [] $ Map.lookup country countriesDict

        get "/:country/:city" $ do
            country <- param "country"
            city <- param "city"
            let byCity (_:_:c:_) = c == city
            json $ filter byCity $ fromMaybe [] $ Map.lookup country countriesDict

