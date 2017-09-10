module Scraper where

import Data.Hourglass
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.Megaparsec
import Text.Megaparsec.TagSoup
import Text.StringLike

import Types

attackRow ::
       (Ord str, StringLike str) => TagParser str (str, str, str, str, str, str)
attackRow = do
    tagOpen "tr"
    c1 <- tdCell
    c2 <- tdCell
    c3 <- tdCell
    c4 <- tdCell
    c5 <- tdCell
    c6 <- tdCell
    tagClose "tr"
    return (c1, c2, c3, c4, c5, c6)

tdCell :: (Ord str, StringLike str) => TagParser str str
tdCell = do
    tagOpen "td"
    TagText t <- tagText
    tagClose "td"
    return (castString . T.strip . castString $ t)

-- takes a html text and returns a list of attack column data
extractData ::
       (Ord str, StringLike str) => str -> [(str, str, str, str, str, str)]
extractData tags = foldr f [] (extractRows $ extractTags tags)
  where
    f r acc =
        case parseMaybe attackRow r of
            Nothing -> acc
            Just x -> x : acc

extractTags :: StringLike str => str -> [Tag str]
extractTags = canonicalizeTags . parseTags

extractRows :: StringLike str => [Tag str] -> [[Tag str]]
extractRows = partitions (isTagOpenName "tr")

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

attackFromColumns ::
       StringLike str => (str, str, str, str, str, str) -> Maybe Attack
attackFromColumns (c1, c2, c3, c4, c5, c6) =
    Attack <$> readDate c1 <*> Just (castString c2) <*> Just (castString c3) <*>
    readInt c4 <*>
    readInt c5 <*>
    Just (castString c6)
