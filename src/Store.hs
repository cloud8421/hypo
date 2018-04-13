module Store where

import           Database.Persist
import           Database.Persist.Sqlite
import           Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

selectPatients :: ConnectionPool -> IO [Entity Patient]
selectPatients pool = do
  runSqlPool (selectList [] []) pool

selectPatient :: ConnectionPool -> Key Patient -> IO (Maybe (Entity Patient))
selectPatient pool patientId = runSqlPool (getEntity patientId) pool

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
