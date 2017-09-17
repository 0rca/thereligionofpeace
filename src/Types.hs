{-# LANGUAGE RecordWildCards #-}

module Types where

import Protolude
import Data.Aeson.Types
import Data.Hourglass
import qualified Data.Map.Strict as M

data Attack = Attack
    { date :: !DateTime
    , country :: !Text
    , city :: !Text
    , killed :: !Int
    , injured :: !Int
    , description :: !Text
    } deriving (Eq, Show)

instance Ord Attack where
    compare a b = compare (date a) (date b)

instance ToJSON Attack where
    toJSON Attack {..} =
        object
            [ "date" .= toJSON (timePrint ISO8601_Date date)
            , "country" .= country
            , "city" .= city
            , "killed" .= killed
            , "injured" .= injured
            , "description" .= description
            ]

data DataBase = DataBase
    { countriesDict :: !Dict
    , citiesDict :: !Dict
    , attacks :: ![Attack]
    }

type Dict = M.Map Text [Attack]
