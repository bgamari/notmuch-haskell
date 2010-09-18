-- notmuch-haskell: notmuch MUA Haskell binding
-- test / demo program
-- Copyright © 2010 Bart Massey
-- Licensed LGPL v3: please see the file COPYING in this
-- source distribution for licensing information.

-- This program has two modes.  When invoked with no
-- arguments, it creates a new notmuch database in /tmp.
-- When given a database argument, it does a search for
-- "subject:notmuch" in the notmuch database.  It will
-- upgrade the database if necessary.

import Control.Monad
import Data.Time
import IO
import Notmuch
import System.Environment
import System.Locale
    
dateString :: FormatTime t => t -> String
dateString = formatTime defaultTimeLocale "%c"

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
  when upgrade $ do
    let cb msg progress = putStrLn $ msg ++ show progress
    putStrLn "Upgrading database"
    databaseUpgrade db (Just cb)
  query <- queryCreate db "subject:notmuch"
  nquery <- queryCountMessages query
  putStrLn $ "subject:notmuch returns " ++ show nquery ++ " results..."
  threads <- queryThreads query
  nthreadss <- mapM threadCountMessages threads
  putStr $ show (sum nthreadss) ++ "/" ++ show (length threads) ++ ": "
  print nthreadss
  let thread = last threads
  subject <- threadGetSubject thread
  putStrLn subject
  messages <- threadGetToplevelMessages thread
  let message = head messages
  subject' <- messageGetHeader message "Subject"
  putStrLn subject'
  date' <- messageGetHeader message "Date"
  putStrLn date'
  date <- messageGetDate message
  putStrLn $ dateString date
  localdate <- utcToLocalZonedTime date
  putStrLn $ dateString localdate
  databaseClose db
  return ()
