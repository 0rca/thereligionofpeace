{-# LANGUAGE RankNTypes #-}

module Web where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Network.Wai.Middleware.RequestLogger
import Types
import Web.Scotty

paginate :: [a] -> ActionM [a]
paginate xs = do
    limit <- param "limit" `rescue` (return . const 25)
    page <- param "page" `rescue` (return . const 0)
    return $ paginate' page limit xs
  where
    paginate' page limit = take limit . drop (page * limit)

filterByCity :: [Attack] -> ActionM [Attack]
filterByCity xs = filterCities <$> param "city"
  where
    filterCities c = filterOn city (c ==) xs
    filterOn f p = filter (p . f)

lookupCountry :: Dict -> ActionM [Attack]
lookupCountry dict =
    fromMaybe [] `liftM` (`M.lookup` dict) `liftM` param "country"

lookupCity :: Dict -> ActionM [Attack]
lookupCity dict = fromMaybe [] `liftM` (`M.lookup` dict) `liftM` param "city"

startServer :: Int -> DataBase -> IO ()
startServer port (DataBase condict citdict) =
    scotty port $ do
        middleware logStdoutDev
        get "/cities" $ do
            let cities = M.keys citdict
            json cities
        get "/countries" $ do
            let countries = M.keys condict
            json countries
        get "/cities/:city" $ do
            cities <- paginate =<< lookupCity citdict
            json cities
        get "/countries/:country" $ do
            countries <- paginate =<< lookupCountry condict
            json countries
        get "/countries/:country/:city" $ do
            countries <- lookupCountry condict
            filtered <- paginate =<< filterByCity countries
            json filtered
