{-# LANGUAGE RankNTypes #-}

module Main where

import Protolude hiding (option)
import Options.Applicative

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as M

import Scraper
import Types
import Web

mkDict :: (Attack -> Text) -> [Attack] -> Dict
mkDict keyFn xs = M.fromListWith (++) [(keyFn x, [x]) | x <- xs]

readFiles :: [FilePath] -> IO LBS.ByteString
readFiles xs = LBS.fromChunks `liftM` mapM BS.readFile xs

parse' :: LBS.ByteString -> [Attack]
parse' contents = foldr f [] (attackFromColumns `fmap` extractData contents)
  where
    f (Just x) acc = x : acc
    f _ acc = acc

mkDataBase :: [Attack] -> IO DataBase
mkDataBase atks = do
    putText "Sorting records..."
    let atks' = sort atks
    putText "Building dictionaries..."
    let countryDict = mkDict country atks'
    let cityDict = mkDict city atks'
    -- force evaluation of dictionaries
    let (murdered, wounded, total) = casualties atks'
    putText $
        "Total attacks: " <>
        show total <>
        "\nIn countries: " <>
        show (M.size countryDict) <>
        "\nIn cities: " <>
        show (M.size cityDict) <>
        "\nTotal killed: " <>
        show murdered <> "\nTotal injured: " <> show wounded
    return $ DataBase countryDict cityDict atks'

data Config = Config
    { port  :: Int
    , paths :: [FilePath]
    }

main :: IO ()
main = do
    config <- execParser $ info (configParser <**> helper) mempty
    putText $ show (paths config)
    db <-
        do putText "Reading files..."
           bytes <- readFiles (paths config)
           putText "Parsing records..."
           let atks = parse' bytes
           mkDataBase atks
    startServer (port config) db
  where
    configParser :: Parser Config
    configParser = Config <$> portParser
        <*> (some pathParser)

    portParser :: Parser Int
    portParser =
        option auto $ long "port"
            <> short 'p'
            <> metavar "PORT"
            <> value 3000
            <> help "Server port (default: 3000)"

    pathParser :: Parser FilePath
    pathParser = argument str (metavar "FILES...")
