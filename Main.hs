import Text.HTML.TagSoup
import System.IO
import qualified System.IO.Strict as S (hGetContents)
import Control.Monad (forM_,liftM)
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Environment (getArgs)
import Control.Arrow
import Data.List (intercalate)

-- isDataTable == table with a header
isDataTable = not.null.(partitions (~== "<th")).head.(partitions (~== "<tr"))

extractRows :: String -> [[String]]
extractRows =
    parseTags                                       >>> -- [a]
    canonicalizeTags                                >>>
    partitions (~== "<table>")                      >>> -- [[a]]
    filter isDataTable                              >>>
    concatMap tail                                  >>> -- [a]
    partitions (~== "<tr>")                         >>> -- [[a]]
    liftA (partitions (~== "<td>"))                 >>> -- [[[a]]]
    (liftA.liftA) (filter isTagText)                >>> -- [[[a]]]
    (liftA.liftA.liftA) (unwords.words.fromTagText) >>> -- [[[String]]]
    (liftA.liftA) (fromMaybe "" . listToMaybe)          -- [[String]]

loadData :: FilePath -> IO [[String]]
loadData filepath =
    withFile filepath ReadMode $ \h -> do
        hSetEncoding h latin1
        putStrLn $ "Reading " ++ filepath
        liftM extractRows $ S.hGetContents h

main :: IO ()
main = do
    files <- getArgs
    rows <- liftM concat $ mapM loadData files
    forM_ rows $ putStrLn . intercalate ";"
    putStrLn $ "Total rows: " ++ (show.length) rows
