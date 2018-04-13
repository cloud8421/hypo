{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
  ( main
  ) where


import           Servant                 (Application)
import           Control.Monad.Logger
import           Database.Persist.Sqlite (withSqlitePool)
import           Lib                     (app, runMigrations, insertPatient)
import           Schema
import           Test.Hspec
import           Test.Hspec.Wai
import           Network.HTTP.Types
import           Test.Hspec.Wai.JSON


main :: IO ()
main = runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
  liftIO $ runMigrations pool
  liftIO $ insertPatient pool examplePatient
  liftIO $ hspec $ spec $ return $ app pool
  where examplePatient = Patient "Claudio" "Ortolina"

spec :: IO Application -> Spec
spec application = with application $ do
  describe "GET /patients" $ do
    it "responds with 200" $ get "/patients" `shouldRespondWith` 200
    it "responds with [patient]" $ do
      let patients = [json|[{"first_name":"Claudio","last_name":"Ortolina"}]|]
      jsonGet "/patients" `shouldRespondWith` patients
  describe "GET /patients/:patient_id with existing patient" $ do
    it "responds with 200" $ jsonGet "/patients/1" `shouldRespondWith` 200
    it "responds with patient" $ do
      let patients = [json|{"first_name":"Claudio","last_name":"Ortolina"}|]
      jsonGet "/patients/1" `shouldRespondWith` patients
  describe "GET /patients/:patient_id with non existing patient"
    $                   it "responds with 404"
    $                   jsonGet "/patients/999"
    `shouldRespondWith` 404
  describe "POST /patients" $ do
    let patientData = [json|{"first_name":"Ada","last_name":"Lovelace"}|]
    it "responds with 200"
      $                   jsonPost "/patients" patientData
      `shouldRespondWith` 200
    it "responds with patient id"
      $                   jsonPost "/patients" patientData
      `shouldRespondWith` "3"
  describe "GET /exams" $ do
    it "responds with 200" $ jsonGet "/exams" `shouldRespondWith` 200
    it "responds with [exam]" $ do
      let exams = [json|[]|]
      jsonGet "/exams" `shouldRespondWith` exams

jsonPost path = request methodPost path [(hContentType, "application/json")]

jsonGet path = request methodGet path [(hContentType, "application/json")] ""
