{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (forM_,liftM)
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe,listToMaybe, maybeToList)
import System.Environment (getArgs)
import Control.Arrow
import Data.List (intercalate, nub, sort, sortBy)
import Data.Ord (comparing)
import Control.Monad.IO.Class (liftIO)

import Web.Scotty
import Data.Monoid (mconcat)
import Data.String (fromString)
import Control.Parallel.Strategies

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
isDataTable = not.null.(partitions (~== th_)).head.(partitions (~== tr_))

extractRows :: String -> [[String]]
extractRows =
    parseTags                                       >>> -- [a]
    canonicalizeTags                                >>>
    partitions (~== table_)                         >>> -- [[a]]
    filter isDataTable                              >>>
    concatMap tail                                  >>> -- [a]
    partitions (~== tr_)                            >>> -- [[a]]
    liftA (partitions (~== td_))                    >>> -- [[[a]]]
    (liftA.liftA) (filter isTagText)                >>> -- [[[a]]]
    (liftA.liftA.liftA) (unwords.words.fromTagText) >>> -- [[[String]]]
    (liftA.liftA) (fromMaybe "" . listToMaybe)          -- [[String]]

loadData :: FilePath -> IO [[String]]
loadData filepath =
    withFile filepath ReadMode $ \h -> do
        hSetEncoding h latin1
        putStrLn $ "Reading " ++ filepath
        liftM extractRows $ S.hGetContents h

type Dictionary = Map.Map String [[String]]

populateDictionary :: ([String] -> (String,[String])) -> [[String]] -> Dictionary
populateDictionary f = foldl (\acc x -> let (k,v) = f x in Map.insertWith (++) k [v] acc) Map.empty

complement :: (a -> Bool) -> a -> Bool
complement f = not.f

main :: IO ()
main = do
    filenames <- getArgs
    rows <- (liftM $ filter (complement null) . concat) $ mapM loadData filenames
    putStrLn "Building dictionaries..."
    let countriesDict = populateDictionary (\row@(_:c:_) -> (c,row)) rows
    let citiesDict    = populateDictionary (\row@(_:co:ci:_) -> (ci ++ ", " ++ co, row)) rows
    putStrLn $ "Total rows: " ++ (show.length) rows
    scotty 3000 $ do
        get "/countries" $ json $ Map.keys countriesDict
        get "/cities" $ json $ Map.keys citiesDict
        get "/:country" $ do
            country <- param "country"
            limit <- (param "limit") `rescue` (\_ -> return 10)
            json $ take limit $ fromMaybe [] $ Map.lookup country countriesDict
        get "/:country/:city" $ do
            country <- param "country"
            city <- param "city"

            json $ filter (\(_:_:c:_) -> c == city) $ fromMaybe [] $ Map.lookup country countriesDict

