-- notmuch-haskell: notmuch MUA Haskell binding
-- low-level interface
-- Copyright (C) 2010 Bart Massey
-- Licensed LGPL v3: please see the file COPYING in this
-- source distribution for licensing information.

-- Originally produced automatically from notmuch.h
-- by hsffig
--   gcc -E -dD notmuch.h | hsffig >NOTMUCH_H.hsc
-- Later hand-edited

-- XXX As of GHC 7.2, the #include directives are simply
-- ignored, even with -fvia-C. This is pretty painful, as it
-- means that changes to APIs are not noticed by the
-- compiler. Uggh.

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409
#include <Rts.h>
#endif
#include <HsFFI.h>

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#def void _dummy_force_NOTMUCH_H_hsc_c (void) { }

{-# OPTIONS -XForeignFunctionInterface #-}

#include "notmuch.h"

module Foreign.NOTMUCH_H(
  module Foreign.NOTMUCH_H,
  module Foreign,
  module Foreign.C.String,
  module Foreign.C.Types) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

newtype S__notmuch_database = S__notmuch_database ()
newtype S__notmuch_directory = S__notmuch_directory ()
newtype S__notmuch_filenames = S__notmuch_filenames ()
newtype S__notmuch_message = S__notmuch_message ()
newtype S__notmuch_messages = S__notmuch_messages ()
newtype S__notmuch_query = S__notmuch_query ()
newtype S__notmuch_tags = S__notmuch_tags ()
newtype S__notmuch_thread = S__notmuch_thread ()
newtype S__notmuch_threads = S__notmuch_threads ()

foreign import ccall "static notmuch.h notmuch_status_to_string"
  f_notmuch_status_to_string :: CInt -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_database_create"
  f_notmuch_database_create :: Ptr CChar -> Ptr (Ptr S__notmuch_database) -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_open"
  f_notmuch_database_open :: Ptr CChar -> CInt -> Ptr (Ptr S__notmuch_database) -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_close"
  f_notmuch_database_close :: Ptr S__notmuch_database -> IO ()

foreign import ccall "static notmuch.h notmuch_database_destroy"
  f_notmuch_database_destroy :: Ptr S__notmuch_database -> IO ()

foreign import ccall "static notmuch.h &notmuch_database_destroy"
  pf_notmuch_database_destroy :: FunPtr (Ptr S__notmuch_database -> IO ())

foreign import ccall "static notmuch.h notmuch_database_get_path"
  f_notmuch_database_get_path :: Ptr S__notmuch_database -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_database_get_version"
  f_notmuch_database_get_version :: Ptr S__notmuch_database -> IO CUInt

foreign import ccall "static notmuch.h notmuch_database_needs_upgrade"
  f_notmuch_database_needs_upgrade :: Ptr S__notmuch_database -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_upgrade"
  f_notmuch_database_upgrade :: Ptr S__notmuch_database -> FunPtr (Ptr CChar -> CDouble -> IO ()) -> Ptr CChar -> IO CInt
foreign import ccall "wrapper"
  w_notmuch_database_upgrade_1 :: (Ptr CChar -> CDouble -> IO ()) -> IO (FunPtr (Ptr CChar -> CDouble -> IO ()))

foreign import ccall "static notmuch.h notmuch_database_begin_atomic"
  f_notmuch_database_begin_atomic :: Ptr S__notmuch_database -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_end_atomic"
  f_notmuch_database_end_atomic :: Ptr S__notmuch_database -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_get_directory"
  f_notmuch_database_get_directory :: Ptr S__notmuch_database -> Ptr CChar -> Ptr (Ptr S__notmuch_directory) -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_add_message"
  f_notmuch_database_add_message :: Ptr S__notmuch_database -> Ptr CChar -> Ptr (Ptr S__notmuch_message) -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_remove_message"
  f_notmuch_database_remove_message :: Ptr S__notmuch_database -> Ptr CChar -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_find_message"
  f_notmuch_database_find_message :: Ptr S__notmuch_database -> Ptr CChar -> Ptr (Ptr S__notmuch_message) -> IO CInt

foreign import ccall "static notmuch.h notmuch_database_get_all_tags"
  f_notmuch_database_get_all_tags :: Ptr S__notmuch_database -> IO (Ptr S__notmuch_tags)

foreign import ccall "static notmuch.h notmuch_query_create"
  f_notmuch_query_create :: Ptr S__notmuch_database -> Ptr CChar -> IO (Ptr S__notmuch_query)

foreign import ccall "static notmuch.h notmuch_query_set_omit_excluded"
  f_notmuch_query_set_omit_excluded :: Ptr S__notmuch_query -> CInt -> IO ()

foreign import ccall "static notmuch.h notmuch_query_set_sort"
  f_notmuch_query_set_sort :: Ptr S__notmuch_query -> CInt -> IO ()

foreign import ccall "static notmuch.h notmuch_query_search_threads"
  f_notmuch_query_search_threads :: Ptr S__notmuch_query -> IO (Ptr S__notmuch_threads)

foreign import ccall "static notmuch.h notmuch_query_search_messages"
  f_notmuch_query_search_messages :: Ptr S__notmuch_query -> IO (Ptr S__notmuch_messages)

foreign import ccall "static notmuch.h notmuch_query_destroy"
  f_notmuch_query_destroy :: Ptr S__notmuch_query -> IO ()

foreign import ccall "static notmuch.h &notmuch_query_destroy"
  pf_notmuch_query_destroy :: FunPtr (Ptr S__notmuch_query -> IO ())

foreign import ccall "static notmuch.h notmuch_threads_valid"
  f_notmuch_threads_valid :: Ptr S__notmuch_threads -> IO CInt

foreign import ccall "static notmuch.h notmuch_query_count_threads"
  f_notmuch_query_count_threads :: Ptr S__notmuch_query -> IO CUInt

foreign import ccall "static notmuch.h notmuch_threads_get"
  f_notmuch_threads_get :: Ptr S__notmuch_threads -> IO (Ptr S__notmuch_thread)

foreign import ccall "static notmuch.h notmuch_threads_move_to_next"
  f_notmuch_threads_move_to_next :: Ptr S__notmuch_threads -> IO ()

foreign import ccall "static notmuch.h notmuch_threads_destroy"
  f_notmuch_threads_destroy :: Ptr S__notmuch_threads -> IO ()

foreign import ccall "static notmuch.h & notmuch_threads_destroy"
  pf_notmuch_threads_destroy :: FunPtr (Ptr S__notmuch_threads -> IO ())

foreign import ccall "static notmuch.h notmuch_query_count_messages"
  f_notmuch_query_count_messages :: Ptr S__notmuch_query -> IO CUInt

foreign import ccall "static notmuch.h notmuch_thread_get_thread_id"
  f_notmuch_thread_get_thread_id :: Ptr S__notmuch_thread -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_thread_get_total_messages"
  f_notmuch_thread_get_total_messages :: Ptr S__notmuch_thread -> IO CInt

foreign import ccall "static notmuch.h notmuch_thread_get_toplevel_messages"
  f_notmuch_thread_get_toplevel_messages :: Ptr S__notmuch_thread -> IO (Ptr S__notmuch_messages)

foreign import ccall "static notmuch.h notmuch_thread_get_matched_messages"
  f_notmuch_thread_get_matched_messages :: Ptr S__notmuch_thread -> IO CInt

foreign import ccall "static notmuch.h notmuch_thread_get_authors"
  f_notmuch_thread_get_authors :: Ptr S__notmuch_thread -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_thread_get_subject"
  f_notmuch_thread_get_subject :: Ptr S__notmuch_thread -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_thread_get_oldest_date"
  f_notmuch_thread_get_oldest_date :: Ptr S__notmuch_thread -> IO CTime

foreign import ccall "static notmuch.h notmuch_thread_get_newest_date"
  f_notmuch_thread_get_newest_date :: Ptr S__notmuch_thread -> IO CTime

foreign import ccall "static notmuch.h notmuch_thread_get_tags"
  f_notmuch_thread_get_tags :: Ptr S__notmuch_thread -> IO (Ptr S__notmuch_tags)

foreign import ccall "static notmuch.h notmuch_thread_destroy"
  f_notmuch_thread_destroy :: Ptr S__notmuch_thread -> IO ()

foreign import ccall "static notmuch.h &notmuch_thread_destroy"
  pf_notmuch_thread_destroy :: FunPtr(Ptr S__notmuch_thread -> IO ())

foreign import ccall "static notmuch.h notmuch_messages_valid"
  f_notmuch_messages_valid :: Ptr S__notmuch_messages -> IO CInt

foreign import ccall "static notmuch.h notmuch_messages_get"
  f_notmuch_messages_get :: Ptr S__notmuch_messages -> IO (Ptr S__notmuch_message)

foreign import ccall "static notmuch.h notmuch_messages_move_to_next"
  f_notmuch_messages_move_to_next :: Ptr S__notmuch_messages -> IO ()

foreign import ccall "static notmuch.h notmuch_messages_destroy"
  f_notmuch_messages_destroy :: Ptr S__notmuch_messages -> IO ()

foreign import ccall "static notmuch.h & notmuch_messages_destroy"
  pf_notmuch_messages_destroy :: FunPtr(Ptr S__notmuch_messages -> IO ())

foreign import ccall "static notmuch.h notmuch_messages_collect_tags"
  f_notmuch_messages_collect_tags :: Ptr S__notmuch_messages -> IO (Ptr S__notmuch_tags)

foreign import ccall "static notmuch.h notmuch_message_get_message_id"
  f_notmuch_message_get_message_id :: Ptr S__notmuch_message -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_message_get_thread_id"
  f_notmuch_message_get_thread_id :: Ptr S__notmuch_message -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_message_get_replies"
  f_notmuch_message_get_replies :: Ptr S__notmuch_message -> IO (Ptr S__notmuch_messages)

foreign import ccall "static notmuch.h notmuch_message_get_filename"
  f_notmuch_message_get_filename :: Ptr S__notmuch_message -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_message_get_flag"
  f_notmuch_message_get_flag :: Ptr S__notmuch_message -> CInt -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_set_flag"
  f_notmuch_message_set_flag :: Ptr S__notmuch_message -> CInt -> CInt -> IO ()

foreign import ccall "static notmuch.h notmuch_message_get_date"
  f_notmuch_message_get_date :: Ptr S__notmuch_message -> IO CTime

foreign import ccall "static notmuch.h notmuch_message_get_header"
  f_notmuch_message_get_header :: Ptr S__notmuch_message -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_message_get_tags"
  f_notmuch_message_get_tags :: Ptr S__notmuch_message -> IO (Ptr S__notmuch_tags)

foreign import ccall "static notmuch.h notmuch_message_add_tag"
  f_notmuch_message_add_tag :: Ptr S__notmuch_message -> Ptr CChar -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_remove_tag"
  f_notmuch_message_remove_tag :: Ptr S__notmuch_message -> Ptr CChar -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_remove_all_tags"
  f_notmuch_message_remove_all_tags :: Ptr S__notmuch_message -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_freeze"
  f_notmuch_message_freeze :: Ptr S__notmuch_message -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_thaw"
  f_notmuch_message_thaw :: Ptr S__notmuch_message -> IO CInt

foreign import ccall "static notmuch.h notmuch_message_destroy"
  f_notmuch_message_destroy :: Ptr S__notmuch_message -> IO ()

foreign import ccall "static notmuch.h &notmuch_message_destroy"
  pf_notmuch_message_destroy :: FunPtr (Ptr S__notmuch_message -> IO ())

foreign import ccall "static notmuch.h notmuch_tags_valid"
  f_notmuch_tags_valid :: Ptr S__notmuch_tags -> IO CInt

foreign import ccall "static notmuch.h notmuch_tags_get"
  f_notmuch_tags_get :: Ptr S__notmuch_tags -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_tags_move_to_next"
  f_notmuch_tags_move_to_next :: Ptr S__notmuch_tags -> IO ()

foreign import ccall "static notmuch.h notmuch_tags_destroy"
  f_notmuch_tags_destroy :: Ptr S__notmuch_tags -> IO ()

foreign import ccall "static notmuch.h notmuch_directory_set_mtime"
  f_notmuch_directory_set_mtime :: Ptr S__notmuch_directory -> CTime -> IO CInt

foreign import ccall "static notmuch.h notmuch_directory_get_mtime"
  f_notmuch_directory_get_mtime :: Ptr S__notmuch_directory -> IO CTime

foreign import ccall "static notmuch.h notmuch_directory_get_child_files"
  f_notmuch_directory_get_child_files :: Ptr S__notmuch_directory -> IO (Ptr S__notmuch_filenames)

foreign import ccall "static notmuch.h notmuch_directory_get_child_directories"
  f_notmuch_directory_get_child_directories :: Ptr S__notmuch_directory -> IO (Ptr S__notmuch_filenames)

foreign import ccall "static notmuch.h notmuch_directory_destroy"
  f_notmuch_directory_destroy :: Ptr S__notmuch_directory -> IO ()

foreign import ccall "static notmuch.h & notmuch_directory_destroy"
  pf_notmuch_directory_destroy :: FunPtr (Ptr S__notmuch_directory -> IO ())

foreign import ccall "static notmuch.h notmuch_filenames_valid"
  f_notmuch_filenames_valid :: Ptr S__notmuch_filenames -> IO CInt

foreign import ccall "static notmuch.h notmuch_filenames_get"
  f_notmuch_filenames_get :: Ptr S__notmuch_filenames -> IO (Ptr CChar)

foreign import ccall "static notmuch.h notmuch_filenames_move_to_next"
  f_notmuch_filenames_move_to_next :: Ptr S__notmuch_filenames -> IO ()

foreign import ccall "static notmuch.h notmuch_filenames_destroy"
  f_notmuch_filenames_destroy :: Ptr S__notmuch_filenames -> IO ()
