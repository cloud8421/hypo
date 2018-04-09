{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Patient = Patient
  { patientId :: Int
  , patientFirstName :: String
  , patientLastName :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Patient)

type API = "patients" :> Get '[JSON] [Patient]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return patients

patients :: [Patient]
patients = [ Patient 1 "Isaac" "Newton"
           , Patient 2 "Albert" "Einstein"
           ]
