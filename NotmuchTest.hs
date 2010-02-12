import Control.Monad
import Data.Time
import IO
import Notmuch
import System.Environment
    
main = do
  argv <- getArgs
  db <- if (length argv == 0)
        then
          (do
            db <- databaseCreate "/tmp"
            databaseClose db
            databaseOpen "/tmp" DatabaseModeReadOnly)
        else
          databaseOpen (head argv) DatabaseModeReadOnly
  dbPath <- databaseGetPath db
  putStrLn $ "database is at " ++ dbPath
  version <- databaseGetVersion db
  putStrLn $ "version is " ++ show version
  upgrade <- databaseNeedsUpgrade db
  when upgrade $
       putStrLn "database needs upgrade"
  query <- queryCreate db "subject:notmuch"
  nquery <- queryCountMessages query
  putStrLn $ "subject:notmuch returns " ++ show nquery ++ " results..."
  threads <- queryThreads query
  putStr $ show (length threads) ++ ": "
  nthreadss <- mapM threadCountMessages threads
  print nthreadss
  let thread = last threads
  subject <- threadGetSubject thread
  putStrLn subject
  messages <- threadGetToplevelMessages thread
  let message = head messages
  subject' <- messageGetHeader message "Subject"
  putStrLn subject'
  date <- messageGetDate message
  print date
  tz <- getTimeZone date
  let localdate = utcToLocalTime tz date
  print localdate
  databaseClose db
  return ()
