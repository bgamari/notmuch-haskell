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
  version <- databaseGetVersion db
  putStrLn $ "version is " ++ show version
  upgrade <- databaseNeedsUpgrade db
  when upgrade $
       putStrLn "database needs upgrade"
  databaseClose db
  return ()
