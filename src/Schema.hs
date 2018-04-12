{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}

module Schema
where

import           Data.Aeson.TH
import           Data.Aeson.Casing              ( aesonPrefix
                                                , snakeCase
                                                )
import           Data.Text                      ( Text )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient
    firstName Text
    lastName  Text
    deriving  Eq Show Generic
Exam
    type      Text
    patientId PatientId
    value     Text
    range     Text
    notes     Text
    deriving Eq Show Generic
|]

deriveJSON (aesonPrefix snakeCase) ''Patient
deriveJSON (aesonPrefix snakeCase) ''Exam
