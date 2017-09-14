{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List as L
import qualified Data.Map.Strict as M
import Data.Text
import System.Environment (getArgs, lookupEnv)

import Scraper
import Types
import Web

mkDict :: (Attack -> Text) -> [Attack] -> Dict
mkDict keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs]

readFiles :: [FilePath] -> IO LBS.ByteString
readFiles xs = LBS.fromChunks `liftM` mapM BS.readFile xs

parse' :: LBS.ByteString -> [Attack]
parse' contents = L.foldr f [] (attackFromColumns `fmap` extractData contents)
  where
    f (Just x) acc = x : acc
    f _ acc = acc

mkDataBase :: [Attack] -> IO DataBase
mkDataBase atks = do
    putStrLn "Sorting records..."
    let atks' = sort atks
    putStrLn "Building dictionaries..."
    let countryDict = mkDict country atks'
    let cityDict = mkDict city atks'
    -- force evaluation of dictionaries
    let (murdered, wounded, total) = casualties atks'
    putStrLn $
        "Total attacks: " ++
        show total ++
        "\nIn countries: " ++
        show (M.size countryDict) ++
        "\nIn cities: " ++
        show (M.size cityDict) ++
        "\nTotal killed: " ++
        show murdered ++ "\nTotal injured: " ++ show wounded
    return $ DataBase countryDict cityDict atks'

main :: IO ()
main = do
    port <- maybe 3000 read `liftM` lookupEnv "PORT"
    paths <- getArgs
    db <-
        do putStrLn "Reading files..."
           bytes <- readFiles paths
           putStrLn "Parsing records..."
           let atks = parse' bytes
           mkDataBase atks
    startServer port db
