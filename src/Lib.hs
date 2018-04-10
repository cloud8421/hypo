{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( startApp
  , app
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types         (camelTo2)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant

data Exam = Exam
  { id :: Int }

data Patient = Patient
  { id        :: Int
  , firstName :: String
  , lastName  :: String
  } deriving (Eq, Show)

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

server :: Server API
server = return patients
         :<|> return exams

patients :: [Patient]
patients = [Patient 1 "Isaac" "Newton", Patient 2 "Albert" "Einstein"]

exams :: [Exam]
exams = [Exam 1, Exam 2, Exam 3]
