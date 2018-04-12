{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
  ( main
  ) where


import           Servant                 (Application)
import           Control.Monad.Logger
import           Database.Persist.Sqlite (withSqlitePool)
import           Lib                     (app, runMigrations)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
  liftIO $ runMigrations pool
  liftIO $ hspec $ spec $ return $ app pool

spec :: IO Application -> Spec
spec application =
  with application $ do
    describe "GET /patients" $ do
      it "responds with 200" $ do get "/patients" `shouldRespondWith` 200
      it "responds with [patient]" $ do
        let patients =
              [json|[]|]
        get "/patients" `shouldRespondWith` patients
    describe "GET /exams" $ do
      it "responds with 200" $ do get "/exams" `shouldRespondWith` 200
      it "responds with [exam]" $ do
        let patients =
              [json|[]|]
        get "/exams" `shouldRespondWith` patients
