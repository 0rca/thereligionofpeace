module Scraper where

import Control.Monad.Trans (liftIO)
import qualified Data.Conduit as C
import Data.Hourglass
import Data.Maybe
import Text.HTML.TagSoup

table_ :: String
table_ = "<table>"

th_ :: String
th_ = "<th>"

tr_ :: String
tr_ = "<tr>"

td_ :: String
td_ = "<td>"

-- isDataTable == table with a header
isDataTable :: [Tag String] -> Bool
isDataTable = not . null . partitions (~== th_) . head . partitions (~== tr_)

data Attack = Attack
    { date :: !DateTime
    , country :: !String
    , city :: !String
    , killed :: !Integer
    , injured :: !Integer
    , description :: !String
    } deriving (Eq, Show)

-- takes a html text and returns a list of attacks
extractData :: String -> [[String]]
extractData =
    map (normaliseColumns . extractColumns) .
    extractRows . extractTable . extractTags

extractTags :: String -> [Tag String]
extractTags = canonicalizeTags . parseTags

extractTable :: [Tag String] -> [Tag String]
extractTable = concatMap tail . filter isDataTable . partitions (~== table_)

extractRows :: [Tag String] -> [[Tag String]]
extractRows = partitions (~== tr_)

extractColumns :: [Tag String] -> [[String]]
extractColumns =
    map (map (unwords . words . fromTagText) . filter isTagText) .
    partitions (~== td_)

normaliseColumns :: [[String]] -> [String]
normaliseColumns = map $ fromMaybe "" . listToMaybe

readInt :: String -> Maybe Integer
readInt "" = Just 0
readInt s = Just $ read s

readDate :: String -> Maybe DateTime
readDate =
    timeParse
        [ Format_Year4
        , Format_Text '.'
        , Format_Month2
        , Format_Text '.'
        , Format_Day2
        ]

attackFromColumns :: [String] -> Maybe Attack
attackFromColumns r =
    Attack <$> readDate (head r) <*> Just (r !! 1) <*> Just (r !! 2) <*>
    readInt (r !! 3) <*>
    readInt (r !! 4) <*>
    Just (r !! 5)

sourceList :: Monad m => [o] -> C.Source m o
sourceList = mapM_ C.yield

sink :: C.Sink String IO ()
sink = C.awaitForever (liftIO . putStrLn)
-- main1 :: IO ()
-- main1 = do
--     _files <- getArgs
--     runResourceT $ sourceDirectory "." C.$$ C.awaitForever (liftIO . putStrLn)
--     -- runResourceT $ CB.sourceFile (head files) C.$$ C.awaitForever (liftIO . print)
--     -- request <- parseUrl "http://thereligionofpeace.com/"
--     -- manager <- newManager tlsManagerSettings
--     -- runResourceT $ do
--     --   response <- http request manager
--     --   responseBody response C.$$+- CB.sinkFile "current.htm"
--     sourceList [1, 2, 3] C.$$ CL.mapM_ print
