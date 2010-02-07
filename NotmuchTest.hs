import Control.Monad
import IO
import Notmuch
import System.Environment
    
main = do
  argv <- getArgs
  putStrLn $ statusToString StatusUnbalancedFreezeThaw
  when (length argv > 0) $ do
         db <- databaseCreate "/tmp"
         databaseClose db
  db <- databaseOpen "/tmp" DatabaseModeReadOnly
  dbPath <- databaseGetPath db
  putStrLn $ "database is at " ++ dbPath
  databaseClose db
  return ()
