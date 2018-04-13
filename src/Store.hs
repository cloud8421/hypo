module Store where

import           Database.Persist
import           Database.Persist.Sqlite
import           Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

selectPatients :: ConnectionPool -> IO [Patient]
selectPatients pool = do
  patientList <- runSqlPool (selectList [] []) pool
  return $ map (\(Entity _ u) -> u) patientList

selectPatient :: ConnectionPool -> Key Patient -> IO (Maybe Patient)
selectPatient pool patientId = runSqlPool (get patientId) pool

insertPatient :: ConnectionPool -> Patient -> IO (Key Patient)
insertPatient pool patient = runSqlPool (insert patient) pool

updatePatient :: ConnectionPool -> Key Patient -> Patient -> IO ()
updatePatient pool patientId newPatient =
  runSqlPool (replace patientId newPatient) pool

dbDeletePatient :: ConnectionPool -> Key Patient -> IO ()
dbDeletePatient pool patientId = runSqlPool (delete patientId) pool

selectExams :: ConnectionPool -> IO [Exam]
selectExams pool = do
  examList <- runSqlPool (selectList [] []) pool
  return $ map (\(Entity _ u) -> u) examList
