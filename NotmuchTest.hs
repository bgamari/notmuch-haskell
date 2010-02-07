import IO
import Notmuch

main = do
  putStrLn $ statusToString StatusUnbalancedFreezeThaw
  databaseCreate "/tmp"
  db <- databaseOpen "/tmp" DatabaseModeReadOnly
  return ()
