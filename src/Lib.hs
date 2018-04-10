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

$(deriveJSON defaultOptions ''Patient)

type API = "patients" :> Get '[ JSON] [Patient]

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

patients :: [Patient]
patients = [Patient 1 "Isaac" "Newton", Patient 2 "Albert" "Einstein"]
