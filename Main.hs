{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (forM_,liftM)
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Environment (getArgs)
import Control.Arrow
import Data.List (intercalate, nub, sort, sortBy)
import Data.Ord (comparing)
import Control.Monad.IO.Class (liftIO)

import Web.Scotty
import Data.Monoid (mconcat)
import Data.String (fromString)
import Control.Parallel.Strategies

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

toTable :: [String] -> String
toTable xs = mconcat ["<div>", intercalate ";" xs, "</div>"]

inParallel = (`using` parList rpar)

main :: IO ()
main = do
    files <- getArgs
    rows <- liftM inParallel $ (liftM $ filter (not.null).concat) $ mapM loadData files
    -- rows <- map (concat.liftIO.loadData) $ files
    -- forM_ rows $ putStrLn . intercalate ";"
    putStrLn $ "Total rows: " ++ (show.length) rows
    scotty 3000 $ do
        get "/countries" $ json $ sort $ nub $ map (\(_:c:_) -> c) rows
        get "/cities" $ json $ sort $ map (intercalate ", ") $ nub $ map (\(_:co:ci:_) -> [ci,co]) rows
        get "/:country" $ do
            country <- param "country"
            json $ filter (\(_:c:_) -> c == country) $ rows
        get "/:country/:city" $ do
            country <- param "country"
            city <- param "city"
            json $ filter (\(_:c:_) -> c == country) $
                   filter (\(_:_:c:_) -> c == city) $ rows

