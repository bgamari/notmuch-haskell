-- Copyright © 2010 Bart Massey
-- notmuch mail library low-level interface

-- Originally produced automatically from notmuch.h
-- by hsffig
--   gcc -E -dD notmuch.h | hsffig >NOTMUCH_H.hsc
-- Later hand-edited

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409
#include <Rts.h>
#endif
#include <HsFFI.h>

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#ifndef __quote__
#define __quote__(x...) x
#endif

#ifndef offsetof
#define offsetof(t, f) ((size_t) &((t *)0)->f)
#endif


#if __NHC__
#define hsc_line(line, file) \
    printf ("# %d \"%s\"\n", line, file);
#else
#define hsc_line(line, file) \
    printf ("{-# LINE %d \"%s\" #-}\n", line, file);
#endif

#define hsc_const(x)                        \
    if ((x) < 0)                            \
        printf ("%ld", (long)(x));          \
    else                                    \
        printf ("%lu", (unsigned long)(x));

#define hsc_const_str(x)                                          \
    {                                                             \
        const char *s = (x);                                      \
        printf ("\"");                                            \
        while (*s != '\0')                                        \
        {                                                         \
            if (*s == '"' || *s == '\\')                          \
                printf ("\\%c", *s);                              \
            else if (*s >= 0x20 && *s <= 0x7E)                    \
                printf ("%c", *s);                                \
            else                                                  \
                printf ("\\%d%s",                                 \
                        (unsigned char) *s,                       \
                        s[1] >= '0' && s[1] <= '9' ? "\\&" : ""); \
            ++s;                                                  \
        }                                                         \
        printf ("\"");                                            \
    }

#define hsc_type(t)                                         \
    if ((t)(int)(t)1.4 == (t)1.4)                           \
        printf ("%s%d",                                     \
                (t)(-1) < (t)0 ? "Int" : "Word",            \
                sizeof (t) * 8);                            \
    else                                                    \
        printf ("%s",                                       \
                sizeof (t) >  sizeof (double) ? "LDouble" : \
                sizeof (t) == sizeof (double) ? "Double"  : \
                "Float");

#ifndef hsc_peek
#define hsc_peek(t, f) \
    printf ("(\\hsc_ptr -> peekByteOff hsc_ptr %ld)", (long) offsetof (__quote__(t), f));
#endif

#ifndef hsc_poke
#define hsc_poke(t, f) \
    printf ("(\\hsc_ptr -> pokeByteOff hsc_ptr %ld)", (long) offsetof (__quote__(t), f));
#endif
           
#ifndef hsc_ptr
#define hsc_ptr(t, f) \
    printf ("(\\hsc_ptr -> hsc_ptr `plusPtr` %ld)", (long) offsetof (__quote__(t), f));
#endif

#ifndef hsc_offset
#define hsc_offset(t, f) \
    printf("(%ld)", (long) offsetof (__quote__(t), f));
#endif

#define hsc_size(t) \
    printf("(%ld)", (long) sizeof(t));

#define hsc_enum(t, f, print_name, x)         \
    print_name;                               \
    printf (" :: %s\n", #t);                  \
    print_name;                               \
    printf (" = %s ", #f);                    \
    if ((x) < 0)                              \
        printf ("(%ld)\n", (long)(x));        \
    else                                      \
        printf ("%lu\n", (unsigned long)(x));

#define hsc_haskellize(x)                                          \
    {                                                              \
        const char *s = (x);                                       \
        int upper = 0;                                             \
        if (*s != '\0')                                            \
        {                                                          \
            putchar (tolower (*s));                                \
            ++s;                                                   \
            while (*s != '\0')                                     \
            {                                                      \
                if (*s == '_')                                     \
                    upper = 1;                                     \
                else                                               \
                {                                                  \
                    putchar (upper ? toupper (*s) : tolower (*s)); \
                    upper = 0;                                     \
                }                                                  \
                ++s;                                               \
            }                                                      \
        }                                                          \
    }

#def void _dummy_force_NOTMUCH_H_hsc_c (void) { }

{-# OPTIONS -fglasgow-exts -XForeignFunctionInterface #-}

#include "notmuch.h"

module NOTMUCH_H(
  module NOTMUCH_H,
  module HSFFIG.FieldAccess,
  module Foreign,
  module Foreign.C.String,
  module Foreign.C.Types) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import HSFFIG.FieldAccess

c_NOTMUCH_TAG_MAX = #const NOTMUCH_TAG_MAX

type T_notmuch_bool_t = CInt
type T_notmuch_database_mode_t = CInt
type T_notmuch_database_t = S__notmuch_database
type T_notmuch_directory_t = S__notmuch_directory
type T_notmuch_filenames_t = S__notmuch_filenames
type T_notmuch_message_flag_t = CInt
type T_notmuch_message_t = S__notmuch_message
type T_notmuch_messages_t = S__notmuch_messages
type T_notmuch_query_t = S__notmuch_query
type T_notmuch_sort_t = CInt
type T_notmuch_status_t = CInt
type T_notmuch_tags_t = S__notmuch_tags
type T_notmuch_thread_t = S__notmuch_thread
type T_notmuch_threads_t = S__notmuch_threads

newtype S__notmuch_database = S__notmuch_database ()
newtype S__notmuch_directory = S__notmuch_directory ()
newtype S__notmuch_filenames = S__notmuch_filenames ()
newtype S__notmuch_message = S__notmuch_message ()
newtype S__notmuch_messages = S__notmuch_messages ()
newtype S__notmuch_query = S__notmuch_query ()
newtype S__notmuch_tags = S__notmuch_tags ()
newtype S__notmuch_thread = S__notmuch_thread ()
newtype S__notmuch_threads = S__notmuch_threads ()

e_NOTMUCH_DATABASE_MODE_READ_ONLY = #const  0 
e_NOTMUCH_DATABASE_MODE_READ_WRITE = #const ( 0 ) + 1
e_NOTMUCH_SORT_OLDEST_FIRST = #const 0
e_NOTMUCH_SORT_NEWEST_FIRST = #const (0) + 1
e_NOTMUCH_SORT_MESSAGE_ID = #const ((0) + 1) + 1
e_NOTMUCH_MESSAGE_FLAG_MATCH = #const 0
e_NOTMUCH_STATUS_SUCCESS = #const  0 
e_NOTMUCH_STATUS_OUT_OF_MEMORY = #const ( 0 ) + 1
e_NOTMUCH_STATUS_READ_ONLY_DATABASE = #const (( 0 ) + 1) + 1
e_NOTMUCH_STATUS_XAPIAN_EXCEPTION = #const ((( 0 ) + 1) + 1) + 1
e_NOTMUCH_STATUS_FILE_ERROR = #const (((( 0 ) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_FILE_NOT_EMAIL = #const ((((( 0 ) + 1) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID = #const (((((( 0 ) + 1) + 1) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_NULL_POINTER = #const ((((((( 0 ) + 1) + 1) + 1) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_TAG_TOO_LONG = #const (((((((( 0 ) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW = #const ((((((((( 0 ) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1
e_NOTMUCH_STATUS_LAST_STATUS = #const (((((((((( 0 ) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1

foreign import ccall "static notmuch.h notmuch_status_to_string"
  f_notmuch_status_to_string :: CInt -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_database_create"
  f_notmuch_database_create :: Ptr (CChar) -> IO (Ptr (S__notmuch_database))

foreign import ccall "static notmuch.h notmuch_database_open"
  f_notmuch_database_open :: Ptr (CChar) -> CInt -> IO (Ptr (S__notmuch_database))

foreign import ccall "static notmuch.h notmuch_database_close"
  f_notmuch_database_close :: Ptr (S__notmuch_database) -> IO (())

foreign import ccall "static notmuch.h notmuch_database_get_path"
  f_notmuch_database_get_path :: Ptr (S__notmuch_database) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_database_get_version"
  f_notmuch_database_get_version :: Ptr (S__notmuch_database) -> IO (CUInt)

foreign import ccall "static notmuch.h notmuch_database_needs_upgrade"
  f_notmuch_database_needs_upgrade :: Ptr (S__notmuch_database) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_database_upgrade"
  f_notmuch_database_upgrade :: Ptr (S__notmuch_database) -> FunPtr (Ptr (CChar) -> CDouble -> IO (())) -> Ptr (CChar) -> IO (CInt)
foreign import ccall "wrapper"
  w_notmuch_database_upgrade_1 :: (Ptr (CChar) -> CDouble -> IO (())) -> IO (FunPtr (Ptr (CChar) -> CDouble -> IO (())))

foreign import ccall "static notmuch.h notmuch_database_get_directory"
  f_notmuch_database_get_directory :: Ptr (S__notmuch_database) -> Ptr (CChar) -> IO (Ptr (S__notmuch_directory))

foreign import ccall "static notmuch.h notmuch_database_add_message"
  f_notmuch_database_add_message :: Ptr (S__notmuch_database) -> Ptr (CChar) -> Ptr (Ptr (S__notmuch_message)) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_database_remove_message"
  f_notmuch_database_remove_message :: Ptr (S__notmuch_database) -> Ptr (CChar) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_database_find_message"
  f_notmuch_database_find_message :: Ptr (S__notmuch_database) -> Ptr (CChar) -> IO (Ptr (S__notmuch_message))

foreign import ccall "static notmuch.h notmuch_database_get_all_tags"
  f_notmuch_database_get_all_tags :: Ptr (S__notmuch_database) -> IO (Ptr (S__notmuch_tags))

foreign import ccall "static notmuch.h notmuch_query_create"
  f_notmuch_query_create :: Ptr (S__notmuch_database) -> Ptr (CChar) -> IO (Ptr (S__notmuch_query))

foreign import ccall "static notmuch.h notmuch_query_set_sort"
  f_notmuch_query_set_sort :: Ptr (S__notmuch_query) -> CInt -> IO (())

foreign import ccall "static notmuch.h notmuch_query_search_threads"
  f_notmuch_query_search_threads :: Ptr (S__notmuch_query) -> IO (Ptr (S__notmuch_threads))

foreign import ccall "static notmuch.h notmuch_query_search_messages"
  f_notmuch_query_search_messages :: Ptr (S__notmuch_query) -> IO (Ptr (S__notmuch_messages))

foreign import ccall "static notmuch.h notmuch_query_destroy"
  f_notmuch_query_destroy :: Ptr (S__notmuch_query) -> IO (())

foreign import ccall "static notmuch.h notmuch_threads_has_more"
  f_notmuch_threads_has_more :: Ptr (S__notmuch_threads) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_threads_get"
  f_notmuch_threads_get :: Ptr (S__notmuch_threads) -> IO (Ptr (S__notmuch_thread))

foreign import ccall "static notmuch.h notmuch_threads_advance"
  f_notmuch_threads_advance :: Ptr (S__notmuch_threads) -> IO (())

foreign import ccall "static notmuch.h notmuch_threads_destroy"
  f_notmuch_threads_destroy :: Ptr (S__notmuch_threads) -> IO (())

foreign import ccall "static notmuch.h notmuch_query_count_messages"
  f_notmuch_query_count_messages :: Ptr (S__notmuch_query) -> IO (CUInt)

foreign import ccall "static notmuch.h notmuch_thread_get_thread_id"
  f_notmuch_thread_get_thread_id :: Ptr (S__notmuch_thread) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_thread_get_total_messages"
  f_notmuch_thread_get_total_messages :: Ptr (S__notmuch_thread) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_thread_get_toplevel_messages"
  f_notmuch_thread_get_toplevel_messages :: Ptr (S__notmuch_thread) -> IO (Ptr (S__notmuch_messages))

foreign import ccall "static notmuch.h notmuch_thread_get_matched_messages"
  f_notmuch_thread_get_matched_messages :: Ptr (S__notmuch_thread) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_thread_get_authors"
  f_notmuch_thread_get_authors :: Ptr (S__notmuch_thread) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_thread_get_subject"
  f_notmuch_thread_get_subject :: Ptr (S__notmuch_thread) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_thread_get_oldest_date"
  f_notmuch_thread_get_oldest_date :: Ptr (S__notmuch_thread) -> IO (CTime)

foreign import ccall "static notmuch.h notmuch_thread_get_newest_date"
  f_notmuch_thread_get_newest_date :: Ptr (S__notmuch_thread) -> IO (CTime)

foreign import ccall "static notmuch.h notmuch_thread_get_tags"
  f_notmuch_thread_get_tags :: Ptr (S__notmuch_thread) -> IO (Ptr (S__notmuch_tags))

foreign import ccall "static notmuch.h notmuch_thread_destroy"
  f_notmuch_thread_destroy :: Ptr (S__notmuch_thread) -> IO (())

foreign import ccall "static notmuch.h notmuch_messages_has_more"
  f_notmuch_messages_has_more :: Ptr (S__notmuch_messages) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_messages_get"
  f_notmuch_messages_get :: Ptr (S__notmuch_messages) -> IO (Ptr (S__notmuch_message))

foreign import ccall "static notmuch.h notmuch_messages_advance"
  f_notmuch_messages_advance :: Ptr (S__notmuch_messages) -> IO (())

foreign import ccall "static notmuch.h notmuch_messages_destroy"
  f_notmuch_messages_destroy :: Ptr (S__notmuch_messages) -> IO (())

foreign import ccall "static notmuch.h notmuch_messages_collect_tags"
  f_notmuch_messages_collect_tags :: Ptr (S__notmuch_messages) -> IO (Ptr (S__notmuch_tags))

foreign import ccall "static notmuch.h notmuch_message_get_message_id"
  f_notmuch_message_get_message_id :: Ptr (S__notmuch_message) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_message_get_thread_id"
  f_notmuch_message_get_thread_id :: Ptr (S__notmuch_message) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_message_get_replies"
  f_notmuch_message_get_replies :: Ptr (S__notmuch_message) -> IO (Ptr (S__notmuch_messages))

foreign import ccall "static notmuch.h notmuch_message_get_filename"
  f_notmuch_message_get_filename :: Ptr (S__notmuch_message) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_message_get_flag"
  f_notmuch_message_get_flag :: Ptr (S__notmuch_message) -> CInt -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_set_flag"
  f_notmuch_message_set_flag :: Ptr (S__notmuch_message) -> CInt -> CInt -> IO (())

foreign import ccall "static notmuch.h notmuch_message_get_date"
  f_notmuch_message_get_date :: Ptr (S__notmuch_message) -> IO (CTime)

foreign import ccall "static notmuch.h notmuch_message_get_header"
  f_notmuch_message_get_header :: Ptr (S__notmuch_message) -> Ptr (CChar) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_message_get_tags"
  f_notmuch_message_get_tags :: Ptr (S__notmuch_message) -> IO (Ptr (S__notmuch_tags))

foreign import ccall "static notmuch.h notmuch_message_add_tag"
  f_notmuch_message_add_tag :: Ptr (S__notmuch_message) -> Ptr (CChar) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_remove_tag"
  f_notmuch_message_remove_tag :: Ptr (S__notmuch_message) -> Ptr (CChar) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_remove_all_tags"
  f_notmuch_message_remove_all_tags :: Ptr (S__notmuch_message) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_freeze"
  f_notmuch_message_freeze :: Ptr (S__notmuch_message) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_thaw"
  f_notmuch_message_thaw :: Ptr (S__notmuch_message) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_message_destroy"
  f_notmuch_message_destroy :: Ptr (S__notmuch_message) -> IO (())

foreign import ccall "static notmuch.h notmuch_tags_has_more"
  f_notmuch_tags_has_more :: Ptr (S__notmuch_tags) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_tags_get"
  f_notmuch_tags_get :: Ptr (S__notmuch_tags) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_tags_advance"
  f_notmuch_tags_advance :: Ptr (S__notmuch_tags) -> IO (())

foreign import ccall "static notmuch.h notmuch_tags_destroy"
  f_notmuch_tags_destroy :: Ptr (S__notmuch_tags) -> IO (())

foreign import ccall "static notmuch.h notmuch_directory_set_mtime"
  f_notmuch_directory_set_mtime :: Ptr (S__notmuch_directory) -> CTime -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_directory_get_mtime"
  f_notmuch_directory_get_mtime :: Ptr (S__notmuch_directory) -> IO (CTime)

foreign import ccall "static notmuch.h notmuch_directory_get_child_files"
  f_notmuch_directory_get_child_files :: Ptr (S__notmuch_directory) -> IO (Ptr (S__notmuch_filenames))

foreign import ccall "static notmuch.h notmuch_directory_get_child_directories"
  f_notmuch_directory_get_child_directories :: Ptr (S__notmuch_directory) -> IO (Ptr (S__notmuch_filenames))

foreign import ccall "static notmuch.h notmuch_directory_destroy"
  f_notmuch_directory_destroy :: Ptr (S__notmuch_directory) -> IO (())

foreign import ccall "static notmuch.h notmuch_filenames_has_more"
  f_notmuch_filenames_has_more :: Ptr (S__notmuch_filenames) -> IO (CInt)

foreign import ccall "static notmuch.h notmuch_filenames_get"
  f_notmuch_filenames_get :: Ptr (S__notmuch_filenames) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h notmuch_filenames_advance"
  f_notmuch_filenames_advance :: Ptr (S__notmuch_filenames) -> IO (())

foreign import ccall "static notmuch.h notmuch_filenames_destroy"
  f_notmuch_filenames_destroy :: Ptr (S__notmuch_filenames) -> IO (())
