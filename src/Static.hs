{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Static
  ( staticMiddleware
  )
where

import Network.Wai                           (Middleware)
import Network.Wai.Middleware.StaticEmbedded
import Data.FileEmbed

staticMiddleware :: Middleware
staticMiddleware = static $(embedDir "./static")
