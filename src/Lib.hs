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
  ( startApp
  , app
  , runMigrations
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types         (camelTo2)
import           Data.Text                (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           Database.Persist.TH      (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import           Database.Persist
import           Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient
    firstName Text
    lastName  Text
    deriving  Eq Show Generic
Exam
    type     Text
    deriving Eq Show Generic
|]

concat <$> mapM
  (deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' })
  [''Patient, ''Exam]

type API =
    "patients" :> Get '[JSON] [Patient]
    :<|> "exams" :> Get '[JSON] [Exam]

startApp :: IO ()
startApp =
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

runDb = runSqlite "data/hypo.db"

runMigrations :: IO ()
runMigrations =
    runDb $ do runMigration migrateAll

server :: Server API
server = getPatients :<|> getExams
    where
        getPatients = liftIO selectPatients
        getExams = liftIO selectExams

selectPatients :: IO [Patient]
selectPatients = do
    patientList <- runDb $ selectList [] []
    return $ map (\(Entity _ u) -> u) patientList

selectExams :: IO [Exam]
selectExams = do
    examList <- runDb $ selectList [] []
    return $ map (\(Entity _ u) -> u) examList
