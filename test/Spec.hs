{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
  ( main
  ) where

import           Lib                 (app)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app) $ do
    describe "GET /patients" $ do
      it "responds with 200" $ do get "/patients" `shouldRespondWith` 200
      it "responds with [patient]" $ do
        let patients =
              "[{\"id\":1,\"firstName\":\"Isaac\",\"lastName\":\"Newton\"},{\"id\":2,\"firstName\":\"Albert\",\"lastName\":\"Einstein\"}]"
        get "/patients" `shouldRespondWith` patients
