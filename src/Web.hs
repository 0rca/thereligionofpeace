{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web where

import Control.Monad

-- import qualified Data.Map.Lazy as M
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (pack)
import Network.Wai.Middleware.RequestLogger
import Types
import Web.Scotty

paginate :: [a] -> ActionM [a]
paginate xs = do
    limit <- param "limit" `rescue` (return . const (-1))
    page <- param "page" `rescue` (return . const 0)
    return $
        if limit > 0
            then paginate' page limit xs
            else xs
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

casualties :: [Attack] -> (Integer, Integer, Integer)
casualties = foldr f (0, 0, 0)
  where
    f x (k, i, t) = (k + killed x, i + injured x, succ t)

startServer :: Int -> DataBase -> IO ()
startServer port (DataBase condict citdict attacks) =
    scotty port $ do
        middleware logStdoutDev
        get "/cities" $ do
            let cities = M.keys citdict
            json =<< paginate cities
        get "/countries" $ do
            let countries = M.keys condict
            json =<< paginate countries
        get "/cities/:city" $ do
            cities <- lookupCity citdict
            json =<< paginate cities
        get "/countries/:country" $ do
            countries <- lookupCountry condict
            json =<< paginate countries
        get "/countries/:country/:city" $ do
            countries <- lookupCountry condict
            filtered <- filterByCity countries
            json =<< paginate filtered
        get "/" $ do
            let (murdered, wounded, total) = casualties attacks
            html $
                "<html><body><pre>" <> "\nTotal attacks since 9/11: " <>
                pack (show total) <>
                "\nIn countries: " <>
                pack (show (M.size condict)) <>
                "\nIn cities: " <>
                pack (show (M.size citdict)) <>
                "\nPeople murdered: " <>
                pack (show murdered) <>
                "\nPeople injured: " <>
                pack (show wounded) <>
                "\n\nAPI Documentation:" <>
                "\nGET /cities - list of cities attacked" <>
                "\nGET /cities/:city - list of attacks per city" <>
                "\nGET /countries - list countries attacked" <>
                "\nGET /countries/:country - list of attacks per country" <>
                "\nGET /countries/:country/:city - narrow search to a certain city within a country" <>
                "\n\nPagination:" <>
                "\n  limit: number of result per page" <>
                "\n  page: page number" <>
                "\n\nExample:" <>
                "\n</pre>" <>
                "\n<a href='http://localhost:3000/cities?limit=5&page=2'>http://localhost:3000/cities?limit=5&page=2</a>" <>
                "\n</body></html>"
