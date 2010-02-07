module Notmuch
where

import Foreign
import Foreign.C.String

import NOTMUCH_H

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
