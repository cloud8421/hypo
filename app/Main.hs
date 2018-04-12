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

opts :: Parser (IO ())
opts = subparser
    (  command "start"   (info (startApp <$> databaseFileOption <*> isProdOption) idm)
    <> command "migrate" (info (runMigrations <$> databaseFileOption) idm) )

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
main = join $ execParser (info opts idm)
