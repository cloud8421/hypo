{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( startAppProd
  , startAppDev
  , app
  , dbMigrate
  )
where

import           Control.Monad.Logger
import           Control.Monad.IO.Class               ( liftIO )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev, logStdout )
import           Servant
import           Database.Persist.Sqlite
import           Schema
import           Store
import           Data.Text                            (Text)

type GetPatients = "patients" :> Get '[JSON] [Entity Patient]

type GetPatient = "patients" :> Capture "patient_id" (Key Patient)
                             :> Get '[JSON] (Entity Patient)

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
    :<|> GetPatient
    :<|> PostPatients
    :<|> PutPatient
    :<|> DeletePatient
    :<|> GetExams

startAppDev :: Text -> IO ()
startAppDev databasePath =
  runStderrLoggingT $ withSqlitePool databasePath 5 $ \pool ->
    liftIO $ run 8080 $ logStdoutDev $ app pool

startAppProd :: Text -> IO ()
startAppProd databasePath =
  runStderrLoggingT $ withSqlitePool databasePath 5 $ \pool ->
    liftIO $ run 8080 $ logStdout $ app pool

app :: ConnectionPool -> Application
app pool = serve api (server pool)

api :: Proxy API
api = Proxy

dbMigrate :: Text -> IO ()
dbMigrate databasePath =
  runStderrLoggingT $ withSqlitePool databasePath 5 $ \pool ->
    liftIO $ runMigrations pool

server :: ConnectionPool -> Server API
server pool =
  getPatients
    :<|> getPatient
    :<|> postPatient
    :<|> putPatient
    :<|> deletePatient
    :<|> getExams
 where
  getPatients = liftIO $ selectPatients pool
  getPatient patientId = do
    maybePatient <- liftIO $ selectPatient pool patientId
    case maybePatient of
      Nothing      -> Handler $ throwError err404
      Just patient -> return patient
  postPatient patient = liftIO $ insertPatient pool patient
  putPatient patientId newPatient =
    liftIO $ updatePatient pool patientId newPatient
  deletePatient patientId = liftIO $ dbDeletePatient pool patientId
  getExams = liftIO $ selectExams pool
