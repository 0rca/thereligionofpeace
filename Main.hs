import Text.HTML.TagSoup
import System.IO
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Trans.Maybe
import Data.Text (strip)
import Data.String (fromString)
import Data.Time.Format
import Data.Time.Calendar
import System.Locale
import Data.Maybe (fromMaybe)
import Control.Applicative
import System.Environment (getArgs)

-- tables :: StringLike a => [Tag a] -> [[Tag a]]
tables xs = tab1 xs ++ tab2 xs where
    tab1 = partitions (~== "<TABLE")
    tab2 = partitions (~== "<table")

rows t = row1 t ++ row2 t where
    row1 = partitions (~== "<TR")
    row2 = partitions (~== "<tr")

cols r = col1 r ++ col2 r where
    col1 = partitions (~== "<TD")
    col2 = partitions (~== "<td")

isHeader r = not . null $ h1 r ++ h2 r where
    h1 = partitions (~== "<TH")
    h2 = partitions (~== "<th")

isDataTable t = isHeader $ head $ rows t

filterText        = filter isTagText

columnToText    t = fromMaybe "" (maybeTagText $ head $ filterText t)
columnToInteger t = fromMaybe 0 (read <$> maybeTagText (head $ filterText t))

data Attack = Attack { date :: Day
                      ,country :: String
                      ,city :: String
                      ,killed :: Integer
                      ,injured :: Integer
                      ,desc :: String } deriving (Show)

main = do
    (filename:_) <- getArgs
    withFile filename ReadMode $ \h -> do
        hSetEncoding h latin1
        tags <- liftM parseTags $ liftIO $ hGetContents h
        let dataTables = filter isDataTable $ tables tags
        putStrLn $ "Tables with data: " ++ show (length dataTables)
        let rs = rows $ concatMap tail dataTables
        putStrLn $ "Rows with data: " ++ show (length rs)
        putStrLn "Processing..."
        forM_ rs $ \r -> do
            putStrLn "-------------------------"
            case cols r of
                all@(date:country:city:killed:injured:desc:_) -> do
                    let date' =  parseTime defaultTimeLocale "%_Y.%0m.%0d" . columnToText $ date
                    let a = Attack <$>
                                date' <*>
                                Just (columnToText country) <*>
                                Just (columnToText city) <*>
                                Just (columnToInteger killed) <*>
                                Just (columnToInteger injured) <*>
                                Just (unwords.words.columnToText $ desc )
                    case a of
                        Just a -> print a
                        _      -> print all
                otherwise -> print otherwise
        putStrLn $ "Total: " ++ show (length rs)
