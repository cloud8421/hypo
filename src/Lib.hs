{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( startAppProd
  , startAppDev
  , app
  , runMigrations
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.TH
import           Data.Aeson.Casing              ( aesonPrefix
                                                , snakeCase
                                                )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev, logStdout )
import           Servant
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           Database.Persist
import           Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient
    firstName Text
    lastName  Text
    deriving  Eq Show Generic
Exam
    type     Text
    patientId Int64
    value    Text
    range    Text
    notes    Text
    deriving Eq Show Generic
|]

deriveJSON (aesonPrefix snakeCase) ''Patient
deriveJSON (aesonPrefix snakeCase) ''Exam

type API =
    "patients" :> Get '[JSON] [Patient]
    :<|> "patients" :> ReqBody '[JSON] Patient :> Post '[JSON] (Key Patient)
    :<|> "patients" :> Capture "patient_id" (Key Patient)
                    :> ReqBody '[JSON] Patient
                    :> Put '[JSON] ()
    :<|> "patients" :> Capture "patient_id" (Key Patient)
                    :> Delete '[JSON] ()
    :<|> "exams" :> Get '[JSON] [Exam]

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
