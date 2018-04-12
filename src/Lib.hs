{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( startAppProd
  , startAppDev
  , app
  , dbMigrate
  , runMigrations
  )
where

import           Control.Monad.Logger
import           Control.Monad.IO.Class               ( liftIO )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev, logStdout )
import           Servant
import           Database.Persist
import           Database.Persist.Sqlite
import           Schema
import           Data.Text                            (Text)

type GetPatients = "patients" :> Get '[JSON] [Patient]

type PostPatients = "patients" :> ReqBody '[JSON] Patient
                               :> Post '[JSON] (Key Patient)

type PutPatient = "patients" :> Capture "patient_id" (Key Patient)
                             :> ReqBody '[JSON] Patient
                             :> Put '[JSON] ()

type DeletePatient = "patients" :> Capture "patient_id" (Key Patient)
                                :> Delete '[JSON] ()

type GetExams = "exams" :> Get '[JSON] [Exam]

type API =
    GetPatients
    :<|> PostPatients
    :<|> PutPatient
    :<|> DeletePatient
    :<|> GetExams

startAppDev :: Text -> IO ()
startAppDev databasePath = runStderrLoggingT $
  withSqlitePool databasePath 5 $ \pool -> liftIO $ do
    run 8080 (logStdoutDev (app pool))

startAppProd :: Text -> IO ()
startAppProd databasePath = runStderrLoggingT $
  withSqlitePool databasePath 5 $ \pool -> liftIO $ do
    run 8080 (logStdout (app pool))

app :: ConnectionPool -> Application
app pool = serve api (server pool)

api :: Proxy API
api = Proxy

runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runSqlPool (runMigration migrateAll) pool

dbMigrate :: Text -> IO ()
dbMigrate databasePath = runStderrLoggingT $
  withSqlitePool databasePath 5 $ \pool -> liftIO $ runMigrations pool

server :: ConnectionPool -> Server API
server pool = getPatients :<|> postPatient :<|> putPatient :<|> deletePatient :<|> getExams
  where
    getPatients = liftIO $ selectPatients pool
    postPatient patient = liftIO $ insertPatient pool patient
    putPatient patientId newPatient = liftIO $ updatePatient pool patientId newPatient
    deletePatient patientId = liftIO $ dbDeletePatient pool patientId
    getExams    = liftIO $ selectExams pool

selectPatients :: ConnectionPool -> IO [Patient]
selectPatients pool = do
  patientList <- runSqlPool (selectList [] []) pool
  return $ map (\(Entity _ u) -> u) patientList

insertPatient :: ConnectionPool -> Patient -> IO (Key Patient)
insertPatient pool patient = runSqlPool (insert patient) pool

updatePatient :: ConnectionPool -> Key Patient -> Patient -> IO ()
updatePatient pool patientId newPatient =
    runSqlPool (replace patientId newPatient) pool

dbDeletePatient :: ConnectionPool -> Key Patient -> IO ()
dbDeletePatient pool patientId =
    runSqlPool (delete patientId) pool

selectExams :: ConnectionPool -> IO [Exam]
selectExams pool = do
  examList <- runSqlPool (selectList [] []) pool
  return $ map (\(Entity _ u) -> u) examList
