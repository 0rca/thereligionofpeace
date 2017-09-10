{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State.Strict
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Hourglass
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text.Lazy as LT
import Network.Wai.Middleware.RequestLogger

import System.Environment (getArgs, lookupEnv)
import System.IO
import qualified Web.Scotty as Web

import Scraper

instance Ord Attack where
    compare a b = compare (date a) (date b)

instance ToJSON Attack where
    toJSON Attack {..} =
        object
            [ "date" .= date
            , "country" .= country
            , "city" .= city
            , "killed" .= killed
            , "injured" .= injured
            , "description" .= description
            ]

instance ToJSON DateTime where
    toJSON time = String $ pack $ timePrint ISO8601_Date time

type Dict = M.Map String [Attack]

data DataBase = DataBase
    { countriesDict :: !Dict
    , citiesDict :: !Dict
    }

mkDict :: (Attack -> String) -> [Attack] -> Dict
mkDict keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs]

paginate :: [a] -> Web.ActionM [a]
paginate xs = do
    limit <- Web.param "limit" `Web.rescue` (return . const 25)
    page <- Web.param "page" `Web.rescue` (return . const 0)
    return $ paginate' page limit xs
  where
    paginate' page limit = take limit . drop (page * limit)

filterByCity :: [Attack] -> Web.ActionM [Attack]
filterByCity xs = filterCities <$> Web.param "city"
  where
    filterCities c = filterOn city (c ==) xs
    filterOn f p = filter (p . f)

lookupCountry :: Dict -> Web.ActionM [Attack]
lookupCountry dict =
    fromMaybe [] `liftM` (`M.lookup` dict) `liftM` Web.param "country"

lookupCity :: Dict -> Web.ActionM [Attack]
lookupCity dict =
    fromMaybe [] `liftM` (`M.lookup` dict) `liftM` Web.param "city"

readFiles :: [FilePath] -> IO LBS.ByteString
readFiles xs = LBS.fromChunks `liftM` mapM BS.readFile xs

readAttacks :: FilePath -> StateT [Attack] IO ()
readAttacks fname = do
    a <-
        liftIO $
        withFile fname ReadMode $ \h -> do
            hSetEncoding h latin1
            putStrLn $ "Reading " ++ fname
            hGetContents h
    let attacks =
            map (fromJust . attackFromColumns) .
            filter (not . null) . extractData $
            a
    modify' (mappend attacks)

parse' :: LBS.ByteString -> [Attack]
parse' contents =
    fmap (fromJust . attackFromColumns) . filter (not . null) . extractData $
    LBS.unpack contents

loadAllAttacks :: [FilePath] -> IO [Attack]
loadAllAttacks paths = parse' `liftM` readFiles paths

mkDataBase :: [Attack] -> IO DataBase
mkDataBase atks = do
    let atks' = sort atks
    putStrLn "Building dictionaries..."
    let countriesDict = mkDict country atks'
    let citiesDict = mkDict city atks'
    -- force evaluation of dictionaries
    putStrLn $ "Total attacks: " ++ (show . length) atks'
    putStrLn $ "In countries: " ++ (show . length) (M.keys countriesDict)
    putStrLn $ "In cities: " ++ (show . length) (M.keys citiesDict)
    putStrLn $ "Total killed: " ++ (show . sum . map killed) atks'
    putStrLn $ "Total injured: " ++ (show . sum . map injured) atks'
    return $ DataBase countriesDict citiesDict

loadAll :: [FilePath] -> IO DataBase
loadAll files = do
    unsorted <- execStateT (mapM_ readAttacks files) []
    let atks = sort unsorted
    putStrLn "Building dictionaries..."
    let countriesDict = mkDict country atks
    let citiesDict = mkDict city atks
    -- force evaluation of dictionaries
    putStrLn $ "Total attacks: " ++ (show . length) atks
    putStrLn $ "In countries: " ++ (show . length) (M.keys countriesDict)
    putStrLn $ "In cities: " ++ (show . length) (M.keys citiesDict)
    putStrLn $ "Total killed: " ++ (show . sum . map killed) atks
    putStrLn $ "Total injured: " ++ (show . sum . map injured) atks
    return $ DataBase countriesDict citiesDict

startServer :: Int -> DataBase -> IO ()
startServer port (DataBase countriesDict citiesDict) =
    Web.scotty port $ do
        Web.middleware logStdoutDev
        Web.get "/cities" $ do
            let cities = M.keys citiesDict
            Web.json cities
        Web.get "/countries" $ do
            let countries = M.keys countriesDict
            Web.json countries
        Web.get "/cities/:city" $ do
            cities <- paginate =<< lookupCity citiesDict
            Web.json cities
        Web.get "/countries/:country" $ do
            countries <- paginate =<< lookupCountry countriesDict
            Web.json countries
        Web.get "/countries/:country/:city" $ do
            countries <- lookupCountry countriesDict
            filtered <- paginate =<< filterByCity countries
            Web.json filtered

main :: IO ()
main = do
    port <- maybe 3000 read `liftM` lookupEnv "PORT"
    files <- getArgs
    -- db <- loadAll files
    db <- mkDataBase =<< loadAllAttacks files
    startServer port db
