{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( startAppProd
  , startAppDev
  , app
  , runMigrations
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev, logStdout )
import           Servant
import           Database.Persist
import           Database.Persist.Sqlite
import           Schema

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

startAppDev :: IO ()
startAppDev =
  run 8080 (logStdoutDev app)

startAppProd :: IO ()
startAppProd =
  run 8080 (logStdout app)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

runDb = runSqlite "data/hypo.db"

runMigrations :: IO ()
runMigrations = runDb $ runMigration migrateAll

server :: Server API
server = getPatients :<|> postPatient :<|> putPatient :<|> deletePatient :<|> getExams
  where
    getPatients = liftIO selectPatients
    postPatient patient = liftIO $ insertPatient patient
    putPatient patientId newPatient = liftIO $ updatePatient patientId newPatient
    deletePatient patientId = liftIO $ dbDeletePatient patientId
    getExams    = liftIO selectExams

selectPatients :: IO [Patient]
selectPatients = do
  patientList <- runDb $ selectList [] []
  return $ map (\(Entity _ u) -> u) patientList

insertPatient :: Patient -> IO (Key Patient)
insertPatient = runDb . insert

updatePatient :: Key Patient -> Patient -> IO ()
updatePatient patientId newPatient =
    runDb . replace patientId $ newPatient

dbDeletePatient :: Key Patient -> IO ()
dbDeletePatient patientId =
    runDb . delete $ patientId

selectExams :: IO [Exam]
selectExams = do
  examList <- runDb $ selectList [] []
  return $ map (\(Entity _ u) -> u) examList
