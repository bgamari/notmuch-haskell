import IO
import Notmuch

main = do
  putStrLn $ statusToString StatusUnbalancedFreezeThaw
  db <- databaseCreate "/tmp"
  return ()
