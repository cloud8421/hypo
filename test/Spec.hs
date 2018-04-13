{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
  ( main
  ) where


import           Servant                 (Application)
import           Control.Monad.Logger
import           Database.Persist.Sqlite (withSqlitePool)
import           Lib                     (app)
import           Schema
import           Store                   (runMigrations, insertPatient)
import           Test.Hspec
import           Test.Hspec.Wai
import           Network.HTTP.Types
import           Network.Wai.Test        (SResponse)
import           Test.Hspec.Wai.JSON
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy as LB


main :: IO ()
main = runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
  liftIO $ runMigrations pool
  _ <- liftIO $ insertPatient pool examplePatient
  liftIO $ hspec $ spec $ return $ app pool
  where examplePatient = Patient "Claudio" "Ortolina"

spec :: IO Application -> Spec
spec application = with application $ do
  describe "GET /patients" $ do
    let req = jsonGet "/patients"
    let respBody = [json|[{"id":1,"firstName":"Claudio","lastName":"Ortolina"}]|]
    it "responds with 200" $ req `shouldRespondWith` 200
    it "responds with [patient]" $ req `shouldRespondWith` respBody

  describe "GET /patients/:patient_id with existing patient" $ do
    let req = jsonGet "/patients/1"
    let respBody = [json|{"id":1,"firstName":"Claudio","lastName":"Ortolina"}|]
    it "responds with 200" $ req `shouldRespondWith` 200
    it "responds with patient" $ req `shouldRespondWith` respBody

  describe "GET /patients/:patient_id with non existing patient" $ do
    let req = jsonGet "/patients/999"
    it "responds with 404" $ req `shouldRespondWith` 404

  describe "POST /patients" $ do
    let reqBody = [json|{"firstName":"Ada","lastName":"Lovelace"}|]
    let req = jsonPost "/patients" reqBody
    let respBody = "3"
    it "responds with 200" $ req `shouldRespondWith` 200
    it "responds with patient id" $ req `shouldRespondWith` respBody

  describe "GET /exams" $ do
    let req = jsonGet "/exams"
    let respBody = [json|[]|]

    it "responds with 200" $ req `shouldRespondWith` 200
    it "responds with [exam]" $ req `shouldRespondWith` respBody

jsonPost :: ByteString -> LB.ByteString -> WaiSession SResponse
jsonPost path = request methodPost path [(hContentType, "application/json")]

jsonGet :: ByteString -> WaiSession SResponse
jsonGet path = request methodGet path [(hContentType, "application/json")] ""
