module Notmuch
where

import NOTMUCH_H

import Control.Monad

requiredDatabaseVersion :: Int
requiredDatabaseVersion = 1

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
-- XXX This should probably be thrown away as hidden
-- internally.
data Status = 
  StatusSuccess |
  StatusOutOfMemory |
  StatusReadOnlyDatabase |
  StatusXapianException |
  StatusFileError |
  StatusFileNotEmail |
  StatusDuplicateMessageId |
  StatusNullPointer |
  StatusTagTooLong |
  StatusUnbalancedFreezeThaw
  deriving Enum

statusToString :: Status -> String
statusToString status =
    unsafePerformIO $ do
      cs <- f_notmuch_status_to_string $ fromIntegral $ fromEnum status
      peekCString cs

type Database = Ptr S__notmuch_database

databaseCreate :: String -> IO Database
databaseCreate name = do
  db <- withCString name f_notmuch_database_create
  when (db == nullPtr) $
       fail "database create failed"
  return db

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
data DatabaseMode = 
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
    deriving Enum

databaseOpen :: String -> DatabaseMode -> IO Database
databaseOpen name databaseMode = do
  db <- withCString name $
        flip f_notmuch_database_open $
        fromIntegral $ fromEnum databaseMode
  when (db == nullPtr) $
       fail "database open failed"
  return db

databaseClose :: Database -> IO ()
databaseClose db = f_notmuch_database_close db

databaseGetPath :: Database -> IO String
databaseGetPath db = do
  cs <- f_notmuch_database_get_path db
  peekCString cs

databaseGetVersion :: Database -> IO Int
databaseGetVersion db = do
  v <- f_notmuch_database_get_version db
  return $ fromIntegral v

resultBool :: IO CInt -> IO Bool
resultBool a = do
  cb <- a
  case cb of
    0 -> return False
    _ -> return True

databaseNeedsUpgrade :: Database -> IO Bool
databaseNeedsUpgrade =
  resultBool . f_notmuch_database_needs_upgrade

statusCheck :: CInt -> IO ()
statusCheck 0 = return ()
statusCheck s = fail $ statusToString $ toEnum $ fromIntegral s

type UpgradeCallback = String -> Double -> IO ()

databaseUpgrade :: Database -> Maybe UpgradeCallback -> IO ()
databaseUpgrade db (Just callback) = do
  let ccb msg progress = do
        cmsg <- peekCString msg
        let cprogress = realToFrac progress
        callback cmsg cprogress
  cb <- w_notmuch_database_upgrade_1 ccb
  s <- f_notmuch_database_upgrade db cb nullPtr
  statusCheck s
databaseUpgrade db Nothing = do
  s <- f_notmuch_database_upgrade db nullFunPtr nullPtr
  statusCheck s

type Directory = Ptr S__notmuch_directory

databaseGetDirectory :: Database -> String -> IO Directory
databaseGetDirectory db path =
    withCString path $ f_notmuch_database_get_directory db
  
type Message = ForeignPtr S__notmuch_message

-- XXX We provide no way to request a null message pointer,
-- so the message is always returned.  The finalizer will
-- then eventually kill it if it is not needed.

-- XXX This function will fail on dup adds, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseAddMessage :: Database -> String -> IO Message
databaseAddMessage db filename = alloca msgFun where
    msgFun msgPtr = do
      let addMessage fn =
              f_notmuch_database_add_message db fn msgPtr
      s <- withCString filename addMessage
      statusCheck s
      cmsg <- peek msgPtr
      newForeignPtr pf_notmuch_message_destroy cmsg

-- XXX This function will fail on dup remove, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseRemoveMessage :: Database -> String -> IO ()
databaseRemoveMessage db filename = do
  let removeMessage fn = f_notmuch_database_remove_message db fn
  s <- withCString filename removeMessage
  statusCheck s

-- XXX This might want to return a Maybe Message instead
-- of failing if the message is not found.  I don't quite
-- understand the use case yet.
databaseFindMessage :: Database -> String -> IO Message
databaseFindMessage db msgid = do
  let findMessage mid =
          f_notmuch_database_find_message db mid
  cmsg <- withCString msgid findMessage
  when (cmsg == nullPtr) $
       fail "database find message failed"
  newForeignPtr pf_notmuch_message_destroy cmsg
  
iterM :: Monad m => a -> (a -> m Bool) -> (a -> m b) -> m [b]
iterM coln test get = do
  cont <- test coln
  case cont of
    True -> do
      elem <- get coln
      rest <- iterM coln test get
      return $ elem : rest
    False -> return []

unpackTags :: Ptr S__notmuch_tags -> IO [String]
unpackTags tags =
    iterM tags test get
    where
      test tags' =
        resultBool $ f_notmuch_tags_has_more tags'
      get tags' = do
        tag <- f_notmuch_tags_get tags'
        f_notmuch_tags_advance tags'
        peekCString tag

databaseGetAllTags :: Database -> IO [String]
databaseGetAllTags db = do
  tags <- f_notmuch_database_get_all_tags db
  unpackTags tags

