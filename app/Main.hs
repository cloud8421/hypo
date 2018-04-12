{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad (join)
import Data.Semigroup ((<>))
import Options.Applicative
import Data.Text

startApp :: Text -> Bool -> IO ()
startApp databasePath False = startAppDev databasePath
startApp databasePath True = startAppProd databasePath

parser :: Parser (IO ())
parser = subparser
    (  command "start"   (info start startDesc)
    <> command "migrate" (info migrate migrateDesc) )
    where start = startApp <$> databaseFileOption <*> isProdOption
          migrate = dbMigrate <$> databaseFileOption
          startDesc = progDesc "Start the API server"
          migrateDesc = progDesc "Setup the initial database"

databaseFileOption :: Parser Text
databaseFileOption =
    strOption (short 'd' <>
               metavar "PATH" <>
               long  "database-file" <>
               value "data/hypo.db" <>
               help  "Which database file to use")

isProdOption :: Parser Bool
isProdOption =
    switch (short 'p' <>
            long  "production" <>
            help  "Run the app in production mode")

main :: IO ()
main = join $ execParser opts
    where opts = info (parser <**> helper) desc
          desc = fullDesc
                 <> progDesc "Manage your patients with ease"
                 <> header "Hypo API server"
