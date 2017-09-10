module Scraper where

import Data.Hourglass
import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike
import Types

table_ :: String
table_ = "<table>"

th_ :: String
th_ = "<th>"

tr_ :: String
tr_ = "<tr>"

td_ :: String
td_ = "<td>"

isDataRow :: StringLike str => [Tag str] -> Bool
isDataRow r = 6 == fst (countTd r)

isDataTable :: StringLike str => [Tag str] -> Bool
isDataTable = not . null . partitions (~== th_) . head . partitions (~== tr_)

-- takes a html text and returns a list of attack column data
extractData :: (Show str, StringLike str) => str -> [[str]]
extractData tags = undefined
    -- map (normaliseColumns . extractColumns) .
    -- L.filter . extractRows . extractTags

--     foldr f [] (extractRows $ extractTags tags)
--   where
--     f r acc
--         | isDataRow r =
extractTags :: StringLike str => str -> [Tag str]
extractTags = canonicalizeTags . parseTags

extractTable :: StringLike str => [Tag str] -> [Tag str]
extractTable = concatMap tail . filter isDataTable . partitions (~== table_)

extractRows :: StringLike str => [Tag str] -> [[Tag str]]
extractRows = partitions (~== tr_)

countTd :: StringLike str => [Tag str] -> (Integer, Integer)
countTd = foldr f (0, 0)
  where
    f (TagOpen "td" _) (n, k) = (succ n, k)
    f (TagClose "td") (n, k) = (n, succ k)
    f _ acc = acc

extractColumns :: (Show str, StringLike str) => [Tag str] -> [[str]]
extractColumns tags =
    let tds = partitions (~== td_) tags
    in map process tds
  where
    process td =
        map (castString . unwords . words . castString . fromTagText) $
        filter isTagText td

normaliseColumns :: StringLike str => [[str]] -> [str]
normaliseColumns = map $ fromMaybe "" . listToMaybe

readInt :: StringLike str => str -> Maybe Integer
readInt "" = Just 0
readInt s = Just $ read (toString s)

readDate :: StringLike str => str -> Maybe DateTime
readDate =
    timeParse
        [ Format_Year4
        , Format_Text '.'
        , Format_Month2
        , Format_Text '.'
        , Format_Day2
        ] .
    toString

attackFromColumns :: StringLike str => [str] -> Maybe Attack
attackFromColumns r =
    Attack <$> readDate (head r) <*> Just (castString $ r !! 1) <*>
    Just (castString $ r !! 2) <*>
    readInt (r !! 3) <*>
    readInt (r !! 4) <*>
    Just (castString $ r !! 5)
