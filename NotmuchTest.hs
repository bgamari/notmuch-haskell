import IO
import Notmuch

main = do
  putStrLn $ statusToString StatusUnbalancedFreezeThaw
  db <- databaseCreate "/tmp"
  databaseClose db
  db <- databaseOpen "/tmp" DatabaseModeReadOnly
  databaseClose db
  return ()
