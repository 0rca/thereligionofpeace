{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson.Types
import Data.Hourglass
import qualified Data.Map.Strict as M
import Data.Text

data Attack = Attack
    { date :: !DateTime
    , country :: !Text
    , city :: !Text
    , killed :: !Integer
    , injured :: !Integer
    , description :: !Text
    } deriving (Eq, Show)

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

data DataBase = DataBase
    { countriesDict :: !Dict
    , citiesDict :: !Dict
    }

type Dict = M.Map Text [Attack]
