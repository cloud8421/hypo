module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let subcommand = if (length args > 0) then Just (args !! 0) else Nothing
    case subcommand of
        Just "migrate" -> runMigrations
        Just "prod" -> startAppProd
        _ -> startAppDev
