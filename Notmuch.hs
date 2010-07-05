-- notmuch-haskell: notmuch MUA Haskell binding
-- high-level interface
-- Copyright © 2010 Bart Massey
-- Licensed LGPL v3: please see the file COPYING in this
-- source distribution for licensing information.

module Notmuch (
  Database, databaseCreate, DatabaseMode(..),
  databaseOpen, databaseClose, databaseGetPath,
  databaseGetVersion, databaseNeedsUpgrade,
  UpgradeCallback, databaseUpgrade,
  Directory, databaseGetDirectory,
  Message, Messages, databaseAddMessage,
  databaseRemoveMessage, databaseFindMessage,
  Tags, databaseGetAllTags,
  Query, queryCreate, SortOrder(..), querySetSortOrder,
  Thread, Threads, queryThreads,
  queryMessages, queryCountMessages,
  getThreadID, threadCountMessages, threadCountMatchedMessages,
  threadGetToplevelMessages, threadGetAuthors,
  threadGetSubject, threadGetOldestDate, threadGetNewestDate,
  threadGetTags,
  messagesCollectTags, messageGetMessageID, messageGetThreadID,
  messageGetReplies, messageGetFilePath,
  MessageFlag(..), messageGetFlag, messageSetFlag,
  messageGetDate, messageGetHeader,
  messageGetTags, messageAddTag,
  messageRemoveTag, messageRemoveAllTags,
  messageFreeze, messageThaw,
  directorySetMtime, directoryGetMtime,
  directoryGetChildFiles, directoryGetChildDirectories
) where

import NOTMUCH_H

import Control.Monad
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import System.FilePath

newtype Database = Database (Ptr S__notmuch_database)

databaseCreate :: FilePath -> IO Database
databaseCreate name = do
  db <- withCString name f_notmuch_database_create
  when (db == nullPtr) $
       fail "database create failed"
  return $ Database db

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
data DatabaseMode = 
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
    deriving Enum

databaseOpen :: FilePath -> DatabaseMode -> IO Database
databaseOpen name databaseMode = do
  db <- withCString name $
        flip f_notmuch_database_open $
        fromIntegral $ fromEnum databaseMode
  when (db == nullPtr) $
       fail "database open failed"
  return $ Database db

databaseClose :: Database -> IO ()
databaseClose (Database db) = f_notmuch_database_close db

databaseGetPath :: Database -> IO FilePath
databaseGetPath (Database db) =
    resultString $ f_notmuch_database_get_path db

databaseGetVersion :: Database -> IO Int
databaseGetVersion (Database db) = do
  v <- f_notmuch_database_get_version db
  return $ fromIntegral v

resultBool :: IO CInt -> IO Bool
resultBool = fmap (/= 0)

resultString :: IO (Ptr CChar) -> IO String
resultString = (>>= peekCString)

resultInt :: IO CInt -> IO Int
resultInt = fmap fromIntegral

resultWord :: IO CUInt -> IO Word
resultWord = fmap fromIntegral

databaseNeedsUpgrade :: Database -> IO Bool
databaseNeedsUpgrade (Database db) =
    resultBool $ f_notmuch_database_needs_upgrade db

statusCheck :: CInt -> IO ()
statusCheck 0 = return ()
statusCheck s = do
  msg <- resultString $ f_notmuch_status_to_string s
  fail msg

type UpgradeCallback = String -> Double -> IO ()

databaseUpgrade :: Database -> Maybe UpgradeCallback -> IO ()
databaseUpgrade (Database db) (Just callback) = do
  let ccb msg progress = do
        cmsg <- peekCString msg
        let cprogress = realToFrac progress
        callback cmsg cprogress
  cb <- w_notmuch_database_upgrade_1 ccb
  s <- f_notmuch_database_upgrade db cb nullPtr
  statusCheck s
databaseUpgrade (Database db) Nothing = do
  s <- f_notmuch_database_upgrade db nullFunPtr nullPtr
  statusCheck s

newtype Directory = Directory (ForeignPtr S__notmuch_directory)

databaseGetDirectory :: Database -> FilePath -> IO Directory
databaseGetDirectory (Database db) path = withCString path $ (\p -> do
  dir <- f_notmuch_database_get_directory db p
  dirp <- newForeignPtr pf_notmuch_directory_destroy dir
  return $ Directory dirp)
  
type MessagesPtr = ForeignPtr S__notmuch_messages

type MessagePtr = ForeignPtr S__notmuch_message

data MessagesRef = QueryMessages { qmpp :: Query, msp :: MessagesPtr }
                 | ThreadMessages { tmpp :: Thread, msp :: MessagesPtr }
                 | MessageMessages { mmspp :: Message, msp :: MessagesPtr }

data Message = MessagesMessage { msmpp :: MessagesRef, mp :: MessagePtr }
             | Message { mp :: MessagePtr }

type Messages = [Message]

-- XXX We provide no way to request a null message pointer,
-- so the message is always returned.  The finalizer will
-- then eventually kill it if it is not needed.

-- XXX This function will fail on dup adds, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseAddMessage :: Database -> FilePath -> IO Message
databaseAddMessage (Database db) filename = alloca msgFun where
    msgFun msgPtr = do
      let addMessage fn =
              f_notmuch_database_add_message db fn msgPtr
      s <- withCString filename addMessage
      statusCheck s
      cmsg <- peek msgPtr
      m <- newForeignPtr pf_notmuch_message_destroy cmsg
      return $ Message m

-- XXX This function will fail on dup remove, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseRemoveMessage :: Database -> FilePath -> IO ()
databaseRemoveMessage (Database db) filename = do
  let removeMessage fn = f_notmuch_database_remove_message db fn
  s <- withCString filename removeMessage
  statusCheck s

-- XXX This might want to return a Maybe Message instead
-- of failing if the message is not found.  I don't quite
-- understand the use case yet.
databaseFindMessage :: Database -> String -> IO Message
databaseFindMessage (Database db) msgid = do
  let findMessage mid =
          f_notmuch_database_find_message db mid
  cmsg <- withCString msgid findMessage
  when (cmsg == nullPtr) $
       fail "database find message failed"
  m <- newForeignPtr pf_notmuch_message_destroy cmsg
  return $ Message m
  
iterM :: Monad m => a -> (a -> m Bool) -> (a -> m b) -> m [b]
iterM coln test get = go [] coln test get
    where go acc coln test get = do
            cont <- test coln
            case cont of
              True -> do
                elem <- get coln
                go (elem : acc) coln test get
              False -> return acc

iterUnpack :: Ptr a -> (Ptr a -> IO CInt) ->
              (Ptr a -> IO b) -> (Ptr a -> IO ()) ->
              IO [b]
iterUnpack coln f_valid f_get f_move_to_next =
    iterM coln has_more get
    where
      has_more = resultBool . f_valid
      get coln' = do
        e <- f_get coln'
        f_move_to_next coln'
        return e

type TagsPtr = Ptr S__notmuch_tags

type Tags = [String]

unpackTags :: TagsPtr -> IO Tags
unpackTags tags = do
  result <- iterUnpack tags
            f_notmuch_tags_valid
            (resultString . f_notmuch_tags_get)
            f_notmuch_tags_move_to_next
  f_notmuch_tags_destroy tags
  return result


databaseGetAllTags :: Database -> IO Tags
databaseGetAllTags (Database db) = do
  tags <- f_notmuch_database_get_all_tags db
  when (tags == nullPtr) $
       fail "database get all tags failed"
  unpackTags tags

newtype Query = Query (ForeignPtr S__notmuch_query)

queryCreate :: Database -> String -> IO Query
queryCreate (Database db) queryString = do
    query <- withCString queryString $ f_notmuch_query_create db
    when (query == nullPtr) $
         fail "query create failed"
    queryp <- newForeignPtr pf_notmuch_query_destroy query
    return $ Query queryp

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
data SortOrder = 
    SortOldestFirst |
    SortNewestFirst |
    SortMessageID
    deriving Enum

querySetSortOrder :: Query -> SortOrder -> IO ()
querySetSortOrder (Query query) sortOrder =
    let setSort query' =
            f_notmuch_query_set_sort query' $
            fromIntegral $ fromEnum sortOrder in
    withForeignPtr query setSort

type ThreadsPtr = ForeignPtr S__notmuch_threads

type ThreadPtr = ForeignPtr S__notmuch_thread

data ThreadsRef = QueryThreads { qtspp :: Query,
                                 tsp :: ThreadsPtr }

data Thread = QueryThread { qtpp :: Query,
                            tp :: ThreadPtr }
            | ThreadsThread { ttpp :: ThreadsRef,
                              tp :: ThreadPtr }

type Threads = [Thread]

queryThreads :: Query -> IO Threads
queryThreads (Query query) = withForeignPtr query $ \q -> do
  threads <- f_notmuch_query_search_threads q
  when (threads == nullPtr) $
       fail "query threads failed"
  tsp <- newForeignPtr pf_notmuch_threads_destroy threads
  let qts = QueryThreads (Query query) tsp
  iterUnpack threads
      f_notmuch_threads_valid
      (\ts -> do
         t <- f_notmuch_threads_get ts
         tp <- newForeignPtr pf_notmuch_thread_destroy t
         let tst = ThreadsThread qts tp
         return tst)
      f_notmuch_threads_move_to_next

unpackMessages :: MessagesRef -> IO Messages
unpackMessages messages = withForeignPtr (msp messages) $ \ms -> do
  iterUnpack ms
      f_notmuch_messages_valid
      (\t -> do
         m <- f_notmuch_messages_get t
         mp <- newForeignPtr pf_notmuch_message_destroy m
         let msm = MessagesMessage messages mp
         return msm)
      f_notmuch_messages_move_to_next

queryMessages :: Query -> IO Messages
queryMessages (Query query) = withForeignPtr query $ \q -> do
  messages <- f_notmuch_query_search_messages q
  when (messages == nullPtr) $
       fail "query messages failed"
  ms <- newForeignPtr pf_notmuch_messages_destroy messages
  let qms = QueryMessages (Query query) ms
  unpackMessages qms

queryCountMessages :: Query -> IO Word
queryCountMessages (Query query) = withForeignPtr query $
    resultWord . f_notmuch_query_count_messages

getThreadID :: Thread -> IO String
getThreadID thread = withForeignPtr (tp thread) $
    resultString . f_notmuch_thread_get_thread_id

threadCountMessages :: Thread -> IO Int
threadCountMessages thread = withForeignPtr (tp thread) $
    resultInt . f_notmuch_thread_get_total_messages

threadCountMatchedMessages :: Thread -> IO Int
threadCountMatchedMessages thread = withForeignPtr (tp thread) $
    resultInt . f_notmuch_thread_get_matched_messages

threadGetToplevelMessages :: Thread -> IO Messages
threadGetToplevelMessages thread = withForeignPtr (tp thread) $ \t -> do
  messages <- f_notmuch_thread_get_toplevel_messages t
  when (messages == nullPtr) $
       fail "thread get top-level messages failed"
  ms <- newForeignPtr pf_notmuch_messages_destroy messages
  let tms = ThreadMessages thread ms
  unpackMessages tms

-- XXX This pretty clearly wants to return a list of authors
-- rather than a single string containing a comma-separated
-- list of authors, but I was too lazy to write the Haskell
-- yet.
threadGetAuthors :: Thread -> IO String
threadGetAuthors thread = withForeignPtr (tp thread) $ \t -> do
  authors <- f_notmuch_thread_get_authors t
  when (authors == nullPtr) $
       fail "thread get authors failed"
  peekCString authors

threadGetSubject :: Thread -> IO String
threadGetSubject thread = withForeignPtr (tp thread) $ \t -> do
  subject <- f_notmuch_thread_get_subject t
  when (subject == nullPtr) $
       fail "thread get subject failed"
  peekCString subject

threadGetOldestDate :: Thread -> IO UTCTime
threadGetOldestDate thread = withForeignPtr (tp thread) $ \t -> do
  date <- f_notmuch_thread_get_oldest_date t
  return $ posixSecondsToUTCTime $ realToFrac date

threadGetNewestDate :: Thread -> IO UTCTime
threadGetNewestDate thread = withForeignPtr (tp thread) $ \t -> do
  date <- f_notmuch_thread_get_newest_date t
  return $ posixSecondsToUTCTime $ realToFrac date

threadGetTags :: Thread -> IO Tags
threadGetTags thread = withForeignPtr (tp thread) $ \t -> do
  tags <-  f_notmuch_thread_get_tags t
  when (tags == nullPtr) $
       fail "thread get tags failed"
  unpackTags tags

-- XXX Because of the peculiar way this is implemented and
-- interfaced in notmuch, we provide a Haskell re-implementation
-- instead of trying to use the underlying native function.
messagesCollectTags :: Messages -> IO Tags
messagesCollectTags messages = do
  tagses <- mapM messageGetTags messages
  return $ nub $ concat tagses

messageGetMessageID :: Message -> IO String
messageGetMessageID message = withForeignPtr (mp message) $ \m -> do
  msgid <- f_notmuch_message_get_message_id m
  when (msgid == nullPtr) $
       fail "message get message ID failed"
  peekCString msgid
  

messageGetThreadID :: Message -> IO String
messageGetThreadID message = withForeignPtr (mp message) $ \m -> do
  tid <- f_notmuch_message_get_thread_id m
  when (tid == nullPtr) $
       fail "message get thread ID failed"
  peekCString tid
  
messageGetReplies :: Message -> IO Messages
messageGetReplies message = withForeignPtr (mp message) $ \m -> do
  messages <- f_notmuch_message_get_replies m
  when (messages == nullPtr) $
       fail "message get replies failed"
  ms <- newForeignPtr pf_notmuch_messages_destroy messages
  let mms = MessageMessages message ms
  unpackMessages mms
  
messageGetFilePath :: Message -> IO FilePath
messageGetFilePath message = withForeignPtr (mp message) $ \m -> do
  path <- f_notmuch_message_get_filename m
  when (path == nullPtr) $
       fail "message get file path failed"
  peekCString path

data MessageFlag =
    MessageFlagMatch
    deriving Enum             

messageGetFlag :: Message -> MessageFlag -> IO Bool
messageGetFlag message flag =
    let cflag = fromIntegral $ fromEnum flag in
    withForeignPtr (mp message) $ 
      resultBool . (\m -> f_notmuch_message_get_flag m cflag)

messageSetFlag :: Message -> MessageFlag -> Bool -> IO ()
messageSetFlag message flag sense =
    let cflag = fromIntegral $ fromEnum flag
        csense = case sense of True -> 1; False -> 0 in
    withForeignPtr (mp message) $ (\m ->
      f_notmuch_message_set_flag m cflag csense)

messageGetDate :: Message -> IO UTCTime
messageGetDate message = withForeignPtr (mp message) $ \m -> do
  date <- f_notmuch_message_get_date m
  return $ posixSecondsToUTCTime $ realToFrac date

messageGetHeader :: Message -> String -> IO String
messageGetHeader message header = withForeignPtr (mp message) $ \m ->
  withCString header (resultString . f_notmuch_message_get_header m)

messageGetTags :: Message -> IO Tags
messageGetTags message = withForeignPtr (mp message) $ \m -> do
  tags <- f_notmuch_message_get_tags m
  when (tags == nullPtr) $
       fail "message get tags failed"
  unpackTags tags

messageAddTag :: Message -> String -> IO ()
messageAddTag message tag = withForeignPtr (mp message) $ \m -> do
  s <- withCString tag $ f_notmuch_message_add_tag m
  statusCheck s

messageRemoveTag :: Message -> String -> IO ()
messageRemoveTag message tag = withForeignPtr (mp message) $ \m -> do
  s <- withCString tag $ f_notmuch_message_remove_tag m
  statusCheck s

messageRemoveAllTags :: Message -> IO ()
messageRemoveAllTags message = withForeignPtr (mp message) $ \m -> do
  s <- f_notmuch_message_remove_all_tags m
  statusCheck s

messageFreeze :: Message -> IO ()
messageFreeze message = withForeignPtr (mp message) $ \m -> do
  s <- f_notmuch_message_freeze m
  statusCheck s

messageThaw :: Message -> IO ()
messageThaw message = withForeignPtr (mp message) $ \m -> do
  s <- f_notmuch_message_thaw m
  statusCheck s

directorySetMtime :: Directory -> UTCTime -> IO ()
directorySetMtime (Directory dir) time = withForeignPtr dir $ \d -> do
  let t = fromIntegral $ floor $ realToFrac $ utcTimeToPOSIXSeconds time
  when (t <= 0) $
       fail "directory set mtime with invalid mtime"
  s <- f_notmuch_directory_set_mtime d t
  statusCheck s

directoryGetMtime :: Directory -> IO UTCTime
directoryGetMtime (Directory dir) = withForeignPtr dir $ \d -> do
  t <- f_notmuch_directory_get_mtime d
  when (t <= 0) $
       fail "directory get mtime failed"
  return $ posixSecondsToUTCTime $ realToFrac t

directoryGetChildFiles :: Directory -> IO [FilePath]
directoryGetChildFiles (Directory dir) = withForeignPtr dir $ \d -> do
  filenames <- f_notmuch_directory_get_child_files d
  iterUnpack filenames
    f_notmuch_filenames_valid
    (resultString . f_notmuch_filenames_get)
    f_notmuch_filenames_move_to_next

directoryGetChildDirectories :: Directory -> IO [FilePath]
directoryGetChildDirectories (Directory dir) = withForeignPtr dir $ \d -> do
  filenames <- f_notmuch_directory_get_child_directories d
  iterUnpack filenames
    f_notmuch_filenames_valid
    (resultString . f_notmuch_filenames_get)
    f_notmuch_filenames_move_to_next
