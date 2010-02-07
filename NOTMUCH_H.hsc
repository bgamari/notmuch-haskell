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

c_CLOCKS_PER_SEC = #const CLOCKS_PER_SEC
c_CLOCK_MONOTONIC = #const CLOCK_MONOTONIC
c_CLOCK_PROCESS_CPUTIME_ID = #const CLOCK_PROCESS_CPUTIME_ID
c_CLOCK_REALTIME = #const CLOCK_REALTIME
c_CLOCK_THREAD_CPUTIME_ID = #const CLOCK_THREAD_CPUTIME_ID
c_FALSE = #const FALSE
c_NOTMUCH_TAG_MAX = #const NOTMUCH_TAG_MAX
c_NULL = #const NULL
c_TIMER_ABSTIME = #const TIMER_ABSTIME
c_TRUE = #const TRUE
c__ATFILE_SOURCE = #const _ATFILE_SOURCE
c__BITS_TIME_H = #const _BITS_TIME_H
c__BITS_TYPESIZES_H = #const _BITS_TYPESIZES_H
c__BITS_TYPES_H = #const _BITS_TYPES_H
c__BSD_SOURCE = #const _BSD_SOURCE
c__FEATURES_H = #const _FEATURES_H
c__POSIX_C_SOURCE = #const _POSIX_C_SOURCE
c__POSIX_SOURCE = #const _POSIX_SOURCE
c__SVID_SOURCE = #const _SVID_SOURCE
c__SYS_CDEFS_H = #const _SYS_CDEFS_H
c__TIME_H = #const _TIME_H
c__XLOCALE_H = #const _XLOCALE_H
c___CHAR_BIT__ = #const __CHAR_BIT__
c___DBL_DENORM_MIN__ = #const __DBL_DENORM_MIN__
c___DBL_DIG__ = #const __DBL_DIG__
c___DBL_EPSILON__ = #const __DBL_EPSILON__
c___DBL_HAS_DENORM__ = #const __DBL_HAS_DENORM__
c___DBL_HAS_INFINITY__ = #const __DBL_HAS_INFINITY__
c___DBL_HAS_QUIET_NAN__ = #const __DBL_HAS_QUIET_NAN__
c___DBL_MANT_DIG__ = #const __DBL_MANT_DIG__
c___DBL_MAX_10_EXP__ = #const __DBL_MAX_10_EXP__
c___DBL_MAX_EXP__ = #const __DBL_MAX_EXP__
c___DBL_MAX__ = #const __DBL_MAX__
c___DBL_MIN_10_EXP__ = #const __DBL_MIN_10_EXP__
c___DBL_MIN_EXP__ = #const __DBL_MIN_EXP__
c___DBL_MIN__ = #const __DBL_MIN__
c___DEC128_DEN__ = #const __DEC128_DEN__
c___DEC128_EPSILON__ = #const __DEC128_EPSILON__
c___DEC128_MANT_DIG__ = #const __DEC128_MANT_DIG__
c___DEC128_MAX_EXP__ = #const __DEC128_MAX_EXP__
c___DEC128_MAX__ = #const __DEC128_MAX__
c___DEC128_MIN_EXP__ = #const __DEC128_MIN_EXP__
c___DEC128_MIN__ = #const __DEC128_MIN__
c___DEC32_DEN__ = #const __DEC32_DEN__
c___DEC32_EPSILON__ = #const __DEC32_EPSILON__
c___DEC32_MANT_DIG__ = #const __DEC32_MANT_DIG__
c___DEC32_MAX_EXP__ = #const __DEC32_MAX_EXP__
c___DEC32_MAX__ = #const __DEC32_MAX__
c___DEC32_MIN_EXP__ = #const __DEC32_MIN_EXP__
c___DEC32_MIN__ = #const __DEC32_MIN__
c___DEC64_DEN__ = #const __DEC64_DEN__
c___DEC64_EPSILON__ = #const __DEC64_EPSILON__
c___DEC64_MANT_DIG__ = #const __DEC64_MANT_DIG__
c___DEC64_MAX_EXP__ = #const __DEC64_MAX_EXP__
c___DEC64_MAX__ = #const __DEC64_MAX__
c___DEC64_MIN_EXP__ = #const __DEC64_MIN_EXP__
c___DEC64_MIN__ = #const __DEC64_MIN__
c___DECIMAL_BID_FORMAT__ = #const __DECIMAL_BID_FORMAT__
c___DECIMAL_DIG__ = #const __DECIMAL_DIG__
c___DEC_EVAL_METHOD__ = #const __DEC_EVAL_METHOD__
c___ELF__ = #const __ELF__
c___FD_SETSIZE = #const __FD_SETSIZE
c___FINITE_MATH_ONLY__ = #const __FINITE_MATH_ONLY__
c___FLT_DENORM_MIN__ = #const __FLT_DENORM_MIN__
c___FLT_DIG__ = #const __FLT_DIG__
c___FLT_EPSILON__ = #const __FLT_EPSILON__
c___FLT_EVAL_METHOD__ = #const __FLT_EVAL_METHOD__
c___FLT_HAS_DENORM__ = #const __FLT_HAS_DENORM__
c___FLT_HAS_INFINITY__ = #const __FLT_HAS_INFINITY__
c___FLT_HAS_QUIET_NAN__ = #const __FLT_HAS_QUIET_NAN__
c___FLT_MANT_DIG__ = #const __FLT_MANT_DIG__
c___FLT_MAX_10_EXP__ = #const __FLT_MAX_10_EXP__
c___FLT_MAX_EXP__ = #const __FLT_MAX_EXP__
c___FLT_MAX__ = #const __FLT_MAX__
c___FLT_MIN_10_EXP__ = #const __FLT_MIN_10_EXP__
c___FLT_MIN_EXP__ = #const __FLT_MIN_EXP__
c___FLT_MIN__ = #const __FLT_MIN__
c___FLT_RADIX__ = #const __FLT_RADIX__
c___GCC_HAVE_SYNC_COMPARE_AND_SWAP_1 = #const __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
c___GCC_HAVE_SYNC_COMPARE_AND_SWAP_2 = #const __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
c___GCC_HAVE_SYNC_COMPARE_AND_SWAP_4 = #const __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
c___GLIBC_HAVE_LONG_LONG = #const __GLIBC_HAVE_LONG_LONG
c___GLIBC_MINOR__ = #const __GLIBC_MINOR__
c___GLIBC__ = #const __GLIBC__
c___GNUC_GNU_INLINE__ = #const __GNUC_GNU_INLINE__
c___GNUC_MINOR__ = #const __GNUC_MINOR__
c___GNUC_PATCHLEVEL__ = #const __GNUC_PATCHLEVEL__
c___GNUC__ = #const __GNUC__
c___GNU_LIBRARY__ = #const __GNU_LIBRARY__
c___GXX_ABI_VERSION = #const __GXX_ABI_VERSION
c___INTMAX_MAX__ = #const __INTMAX_MAX__
c___INT_MAX__ = #const __INT_MAX__
c___LDBL_DENORM_MIN__ = #const __LDBL_DENORM_MIN__
c___LDBL_DIG__ = #const __LDBL_DIG__
c___LDBL_EPSILON__ = #const __LDBL_EPSILON__
c___LDBL_HAS_DENORM__ = #const __LDBL_HAS_DENORM__
c___LDBL_HAS_INFINITY__ = #const __LDBL_HAS_INFINITY__
c___LDBL_HAS_QUIET_NAN__ = #const __LDBL_HAS_QUIET_NAN__
c___LDBL_MANT_DIG__ = #const __LDBL_MANT_DIG__
c___LDBL_MAX_10_EXP__ = #const __LDBL_MAX_10_EXP__
c___LDBL_MAX_EXP__ = #const __LDBL_MAX_EXP__
c___LDBL_MAX__ = #const __LDBL_MAX__
c___LDBL_MIN_10_EXP__ = #const __LDBL_MIN_10_EXP__
c___LDBL_MIN_EXP__ = #const __LDBL_MIN_EXP__
c___LDBL_MIN__ = #const __LDBL_MIN__
c___LONG_LONG_MAX__ = #const __LONG_LONG_MAX__
c___LONG_MAX__ = #const __LONG_MAX__
c___NO_INLINE__ = #const __NO_INLINE__
c___SCHAR_MAX__ = #const __SCHAR_MAX__
c___SHRT_MAX__ = #const __SHRT_MAX__
c___SIZEOF_DOUBLE__ = #const __SIZEOF_DOUBLE__
c___SIZEOF_FLOAT__ = #const __SIZEOF_FLOAT__
c___SIZEOF_INT__ = #const __SIZEOF_INT__
c___SIZEOF_LONG_DOUBLE__ = #const __SIZEOF_LONG_DOUBLE__
c___SIZEOF_LONG_LONG__ = #const __SIZEOF_LONG_LONG__
c___SIZEOF_LONG__ = #const __SIZEOF_LONG__
c___SIZEOF_POINTER__ = #const __SIZEOF_POINTER__
c___SIZEOF_PTRDIFF_T__ = #const __SIZEOF_PTRDIFF_T__
c___SIZEOF_SHORT__ = #const __SIZEOF_SHORT__
c___SIZEOF_SIZE_T__ = #const __SIZEOF_SIZE_T__
c___SIZEOF_WCHAR_T__ = #const __SIZEOF_WCHAR_T__
c___SIZEOF_WINT_T__ = #const __SIZEOF_WINT_T__
c___STDC_HOSTED__ = #const __STDC_HOSTED__
c___STDC_IEC_559_COMPLEX__ = #const __STDC_IEC_559_COMPLEX__
c___STDC_IEC_559__ = #const __STDC_IEC_559__
c___STDC_ISO_10646__ = #const __STDC_ISO_10646__
c___STDC__ = #const __STDC__
c___USE_ANSI = #const __USE_ANSI
c___USE_ATFILE = #const __USE_ATFILE
c___USE_BSD = #const __USE_BSD
c___USE_FORTIFY_LEVEL = #const __USE_FORTIFY_LEVEL
c___USE_ISOC99 = #const __USE_ISOC99
c___USE_MISC = #const __USE_MISC
c___USE_POSIX = #const __USE_POSIX
c___USE_POSIX199309 = #const __USE_POSIX199309
c___USE_POSIX199506 = #const __USE_POSIX199506
c___USE_POSIX2 = #const __USE_POSIX2
c___USE_POSIX_IMPLICITLY = #const __USE_POSIX_IMPLICITLY
c___USE_SVID = #const __USE_SVID
c___USE_XOPEN2K = #const __USE_XOPEN2K
c___USE_XOPEN2K8 = #const __USE_XOPEN2K8
c___VERSION__ = #const __VERSION__
c___WCHAR_MAX__ = #const __WCHAR_MAX__
c___WORDSIZE = #const __WORDSIZE
c___clock_t_defined = #const __clock_t_defined
c___clockid_t_defined = #const __clockid_t_defined
c___gnu_linux__ = #const __gnu_linux__
c___i386 = #const __i386
c___i386__ = #const __i386__
c___i486 = #const __i486
c___i486__ = #const __i486__
c___linux = #const __linux
c___linux__ = #const __linux__
c___time_t_defined = #const __time_t_defined
c___timer_t_defined = #const __timer_t_defined
c___timespec_defined = #const __timespec_defined
c___unix = #const __unix
c___unix__ = #const __unix__
c_i386 = #const i386
c_linux = #const linux
c_unix = #const unix

data V_sizeof = V_sizeof
data X_sizeof = X_sizeof
data D_sizeof = D_sizeof
data V___val = V___val
data X___val = X___val
data D___val = D___val
data V___locales = V___locales
data X___locales = X___locales
data D___locales = D___locales
data V___ctype_b = V___ctype_b
data X___ctype_b = X___ctype_b
data D___ctype_b = D___ctype_b
data V___ctype_tolower = V___ctype_tolower
data X___ctype_tolower = X___ctype_tolower
data D___ctype_tolower = D___ctype_tolower
data V___ctype_toupper = V___ctype_toupper
data X___ctype_toupper = X___ctype_toupper
data D___ctype_toupper = D___ctype_toupper
data V___names = V___names
data X___names = X___names
data D___names = D___names
data V_it_interval = V_it_interval
data X_it_interval = X_it_interval
data D_it_interval = D_it_interval
data V_it_value = V_it_value
data X_it_value = X_it_value
data D_it_value = D_it_value
data V_tv_sec = V_tv_sec
data X_tv_sec = X_tv_sec
data D_tv_sec = D_tv_sec
data V_tv_nsec = V_tv_nsec
data X_tv_nsec = X_tv_nsec
data D_tv_nsec = D_tv_nsec
data V_tm_sec = V_tm_sec
data X_tm_sec = X_tm_sec
data D_tm_sec = D_tm_sec
data V_tm_min = V_tm_min
data X_tm_min = X_tm_min
data D_tm_min = D_tm_min
data V_tm_hour = V_tm_hour
data X_tm_hour = X_tm_hour
data D_tm_hour = D_tm_hour
data V_tm_mday = V_tm_mday
data X_tm_mday = X_tm_mday
data D_tm_mday = D_tm_mday
data V_tm_mon = V_tm_mon
data X_tm_mon = X_tm_mon
data D_tm_mon = D_tm_mon
data V_tm_year = V_tm_year
data X_tm_year = X_tm_year
data D_tm_year = D_tm_year
data V_tm_wday = V_tm_wday
data X_tm_wday = X_tm_wday
data D_tm_wday = D_tm_wday
data V_tm_yday = V_tm_yday
data X_tm_yday = X_tm_yday
data D_tm_yday = D_tm_yday
data V_tm_isdst = V_tm_isdst
data X_tm_isdst = X_tm_isdst
data D_tm_isdst = D_tm_isdst
data V_tm_gmtoff = V_tm_gmtoff
data X_tm_gmtoff = X_tm_gmtoff
data D_tm_gmtoff = D_tm_gmtoff
data V_tm_zone = V_tm_zone
data X_tm_zone = X_tm_zone
data D_tm_zone = D_tm_zone

type T___blkcnt64_t = CLLong
type T___blkcnt_t = CLong
type T___blksize_t = CLong
type T___caddr_t = Ptr (CChar)
type T___clock_t = CLong
type T___clockid_t = CInt
type T___daddr_t = CInt
type T___dev_t = CULLong
type T___fsblkcnt64_t = CULLong
type T___fsblkcnt_t = CULong
type T___fsfilcnt64_t = CULLong
type T___fsfilcnt_t = CULong
type T___fsid_t = S_363
type T___gid_t = CUInt
type T___id_t = CUInt
type T___ino64_t = CULLong
type T___ino_t = CULong
type T___int16_t = CShort
type T___int32_t = CInt
type T___int64_t = CLLong
type T___int8_t = CSChar
type T___intptr_t = CInt
type T___key_t = CInt
type T___locale_t = Ptr (S___locale_struct)
type T___loff_t = CLLong
type T___mode_t = CUInt
type T___nlink_t = CUInt
type T___off64_t = CLLong
type T___off_t = CLong
type T___pid_t = CInt
type T___qaddr_t = Ptr (CLLong)
type T___quad_t = CLLong
type T___rlim64_t = CULLong
type T___rlim_t = CULong
type T___socklen_t = CUInt
type T___ssize_t = CInt
type T___suseconds_t = CLong
type T___swblk_t = CLong
type T___time_t = CLong
type T___timer_t = Ptr (CChar)
type T___u_char = CUChar
type T___u_int = CUInt
type T___u_long = CULong
type T___u_quad_t = CULLong
type T___u_short = CUShort
type T___uid_t = CUInt
type T___uint16_t = CUShort
type T___uint32_t = CUInt
type T___uint64_t = CULLong
type T___uint8_t = CUChar
type T___useconds_t = CUInt
type T_clock_t = CClock
type T_clockid_t = CInt
type T_locale_t = Ptr (S___locale_struct)
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
type T_pid_t = CInt
type T_size_t = CSize
type T_time_t = CTime
type T_timer_t = Ptr (CChar)

newtype S_363 = S_363 ()
newtype S___locale_struct = S___locale_struct ()
newtype S__notmuch_database = S__notmuch_database ()
newtype S__notmuch_directory = S__notmuch_directory ()
newtype S__notmuch_filenames = S__notmuch_filenames ()
newtype S__notmuch_message = S__notmuch_message ()
newtype S__notmuch_messages = S__notmuch_messages ()
newtype S__notmuch_query = S__notmuch_query ()
newtype S__notmuch_tags = S__notmuch_tags ()
newtype S__notmuch_thread = S__notmuch_thread ()
newtype S__notmuch_threads = S__notmuch_threads ()
newtype S_itimerspec = S_itimerspec ()
newtype S_locale_data = S_locale_data ()
newtype S_sigevent = S_sigevent ()
newtype S_timespec = S_timespec ()
newtype S_tm = S_tm ()

instance HSFFIG.FieldAccess.FieldAccess S_363 (Ptr (CInt)) V___val where
  z --> V___val = (#peek __quote__(struct {int  __val[ 2 ] ; }), __val) z
  (z, V___val) <-- v = (#poke __quote__(struct {int  __val[ 2 ] ; }), __val) z v

instance HSFFIG.FieldAccess.FieldAccess S_363 ([Int]) D___val where
  z --> D___val = return [(#const( 2 ))]
  (z, D___val) <-- v = error $ "dimensions of a field  cannot be set"

instance HSFFIG.FieldAccess.FieldAccess S_363 (CInt) V_sizeof where
  z --> V_sizeof = return $ (#size __quote__(struct {int  __val[ 2 ] ; }))

instance Storable S_363 where
  sizeOf _ = (#size __quote__(struct {int  __val[ 2 ] ; }))
  alignment _ = 1
  peek _ = error $ "peek and poke cannot be used with struct {int  __val[ 2 ] ; }"
  poke _ = error $ "peek and poke cannot be used with struct {int  __val[ 2 ] ; }"

instance HSFFIG.FieldAccess.FieldAccess S_itimerspec ((Ptr S_timespec)) V_it_interval where
  z --> V_it_interval = return $ (#ptr __quote__(struct itimerspec), it_interval) z
  (z, V_it_interval) <-- v = error $ "field it_interval is a structure or an array: cannot be set"

instance HSFFIG.FieldAccess.FieldAccess S_itimerspec ((Ptr S_timespec)) V_it_value where
  z --> V_it_value = return $ (#ptr __quote__(struct itimerspec), it_value) z
  (z, V_it_value) <-- v = error $ "field it_value is a structure or an array: cannot be set"

instance HSFFIG.FieldAccess.FieldAccess S_itimerspec (CInt) V_sizeof where
  z --> V_sizeof = return $ (#size __quote__(struct itimerspec))

instance Storable S_itimerspec where
  sizeOf _ = (#size __quote__(struct itimerspec))
  alignment _ = 1
  peek _ = error $ "peek and poke cannot be used with struct itimerspec"
  poke _ = error $ "peek and poke cannot be used with struct itimerspec"

instance HSFFIG.FieldAccess.FieldAccess S_timespec ((CLong)) V_tv_sec where
  z --> V_tv_sec = (#peek __quote__(struct timespec), tv_sec) z
  (z, V_tv_sec) <-- v = (#poke __quote__(struct timespec), tv_sec) z v

instance HSFFIG.FieldAccess.FieldAccess S_timespec ((CLong)) V_tv_nsec where
  z --> V_tv_nsec = (#peek __quote__(struct timespec), tv_nsec) z
  (z, V_tv_nsec) <-- v = (#poke __quote__(struct timespec), tv_nsec) z v

instance HSFFIG.FieldAccess.FieldAccess S_timespec (CInt) V_sizeof where
  z --> V_sizeof = return $ (#size __quote__(struct timespec))

instance Storable S_timespec where
  sizeOf _ = (#size __quote__(struct timespec))
  alignment _ = 1
  peek _ = error $ "peek and poke cannot be used with struct timespec"
  poke _ = error $ "peek and poke cannot be used with struct timespec"

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_sec where
  z --> V_tm_sec = (#peek __quote__(struct tm), tm_sec) z
  (z, V_tm_sec) <-- v = (#poke __quote__(struct tm), tm_sec) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_min where
  z --> V_tm_min = (#peek __quote__(struct tm), tm_min) z
  (z, V_tm_min) <-- v = (#poke __quote__(struct tm), tm_min) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_hour where
  z --> V_tm_hour = (#peek __quote__(struct tm), tm_hour) z
  (z, V_tm_hour) <-- v = (#poke __quote__(struct tm), tm_hour) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_mday where
  z --> V_tm_mday = (#peek __quote__(struct tm), tm_mday) z
  (z, V_tm_mday) <-- v = (#poke __quote__(struct tm), tm_mday) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_mon where
  z --> V_tm_mon = (#peek __quote__(struct tm), tm_mon) z
  (z, V_tm_mon) <-- v = (#poke __quote__(struct tm), tm_mon) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_year where
  z --> V_tm_year = (#peek __quote__(struct tm), tm_year) z
  (z, V_tm_year) <-- v = (#poke __quote__(struct tm), tm_year) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_wday where
  z --> V_tm_wday = (#peek __quote__(struct tm), tm_wday) z
  (z, V_tm_wday) <-- v = (#poke __quote__(struct tm), tm_wday) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_yday where
  z --> V_tm_yday = (#peek __quote__(struct tm), tm_yday) z
  (z, V_tm_yday) <-- v = (#poke __quote__(struct tm), tm_yday) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CInt)) V_tm_isdst where
  z --> V_tm_isdst = (#peek __quote__(struct tm), tm_isdst) z
  (z, V_tm_isdst) <-- v = (#poke __quote__(struct tm), tm_isdst) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((CLong)) V_tm_gmtoff where
  z --> V_tm_gmtoff = (#peek __quote__(struct tm), tm_gmtoff) z
  (z, V_tm_gmtoff) <-- v = (#poke __quote__(struct tm), tm_gmtoff) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm ((Ptr (CChar))) V_tm_zone where
  z --> V_tm_zone = (#peek __quote__(struct tm), tm_zone) z
  (z, V_tm_zone) <-- v = (#poke __quote__(struct tm), tm_zone) z v

instance HSFFIG.FieldAccess.FieldAccess S_tm (CInt) V_sizeof where
  z --> V_sizeof = return $ (#size __quote__(struct tm))

instance Storable S_tm where
  sizeOf _ = (#size __quote__(struct tm))
  alignment _ = 1
  peek _ = error $ "peek and poke cannot be used with struct tm"
  poke _ = error $ "peek and poke cannot be used with struct tm"

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


foreign import ccall "static notmuch.h clock"
  f_clock :: IO (CClock)

foreign import ccall "static notmuch.h time"
  f_time :: Ptr (CTime) -> IO (CTime)

foreign import ccall "static notmuch.h mktime"
  f_mktime :: Ptr (S_tm) -> IO (CTime)

foreign import ccall "static notmuch.h strftime"
  f_strftime :: Ptr (CChar) -> CSize -> Ptr (CChar) -> Ptr (S_tm) -> IO (CSize)

foreign import ccall "static notmuch.h strftime_l"
  f_strftime_l :: Ptr (CChar) -> CSize -> Ptr (CChar) -> Ptr (S_tm) -> Ptr (S___locale_struct) -> IO (CSize)

foreign import ccall "static notmuch.h gmtime"
  f_gmtime :: Ptr (CTime) -> IO (Ptr (S_tm))

foreign import ccall "static notmuch.h localtime"
  f_localtime :: Ptr (CTime) -> IO (Ptr (S_tm))

foreign import ccall "static notmuch.h gmtime_r"
  f_gmtime_r :: Ptr (CTime) -> Ptr (S_tm) -> IO (Ptr (S_tm))

foreign import ccall "static notmuch.h localtime_r"
  f_localtime_r :: Ptr (CTime) -> Ptr (S_tm) -> IO (Ptr (S_tm))

foreign import ccall "static notmuch.h asctime"
  f_asctime :: Ptr (S_tm) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h ctime"
  f_ctime :: Ptr (CTime) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h asctime_r"
  f_asctime_r :: Ptr (S_tm) -> Ptr (CChar) -> IO (Ptr (CChar))

foreign import ccall "static notmuch.h ctime_r"
  f_ctime_r :: Ptr (CTime) -> Ptr (CChar) -> IO (Ptr (CChar))

foreign import ccall "notmuch.h &__tzname" 
  ___476___ :: Ptr (Ptr (CChar))
p___tzname = ___476___
v___tzname = peek ___476___
s___tzname = poke ___476___

foreign import ccall "notmuch.h &__daylight" 
  ___477___ :: Ptr (CInt)
p___daylight = ___477___
v___daylight = peek ___477___
s___daylight = poke ___477___

foreign import ccall "notmuch.h &__timezone" 
  ___478___ :: Ptr (CLong)
p___timezone = ___478___
v___timezone = peek ___478___
s___timezone = poke ___478___

foreign import ccall "notmuch.h &tzname" 
  ___479___ :: Ptr (Ptr (CChar))
p_tzname = ___479___
v_tzname = peek ___479___
s_tzname = poke ___479___

foreign import ccall "static notmuch.h tzset"
  f_tzset :: IO (())

foreign import ccall "notmuch.h &daylight" 
  ___481___ :: Ptr (CInt)
p_daylight = ___481___
v_daylight = peek ___481___
s_daylight = poke ___481___

foreign import ccall "notmuch.h &timezone" 
  ___482___ :: Ptr (CLong)
p_timezone = ___482___
v_timezone = peek ___482___
s_timezone = poke ___482___

foreign import ccall "static notmuch.h stime"
  f_stime :: Ptr (CTime) -> IO (CInt)

foreign import ccall "static notmuch.h timegm"
  f_timegm :: Ptr (S_tm) -> IO (CTime)

foreign import ccall "static notmuch.h timelocal"
  f_timelocal :: Ptr (S_tm) -> IO (CTime)

foreign import ccall "static notmuch.h nanosleep"
  f_nanosleep :: Ptr (S_timespec) -> Ptr (S_timespec) -> IO (CInt)

foreign import ccall "static notmuch.h clock_getres"
  f_clock_getres :: CInt -> Ptr (S_timespec) -> IO (CInt)

foreign import ccall "static notmuch.h clock_gettime"
  f_clock_gettime :: CInt -> Ptr (S_timespec) -> IO (CInt)

foreign import ccall "static notmuch.h clock_settime"
  f_clock_settime :: CInt -> Ptr (S_timespec) -> IO (CInt)

foreign import ccall "static notmuch.h clock_nanosleep"
  f_clock_nanosleep :: CInt -> CInt -> Ptr (S_timespec) -> Ptr (S_timespec) -> IO (CInt)

foreign import ccall "static notmuch.h clock_getcpuclockid"
  f_clock_getcpuclockid :: CInt -> Ptr (CInt) -> IO (CInt)

foreign import ccall "static notmuch.h timer_create"
  f_timer_create :: CInt -> Ptr (S_sigevent) -> Ptr (Ptr (CChar)) -> IO (CInt)

foreign import ccall "static notmuch.h timer_delete"
  f_timer_delete :: Ptr (CChar) -> IO (CInt)

foreign import ccall "static notmuch.h timer_settime"
  f_timer_settime :: Ptr (CChar) -> CInt -> Ptr (S_itimerspec) -> Ptr (S_itimerspec) -> IO (CInt)

foreign import ccall "static notmuch.h timer_gettime"
  f_timer_gettime :: Ptr (CChar) -> Ptr (S_itimerspec) -> IO (CInt)

foreign import ccall "static notmuch.h timer_getoverrun"
  f_timer_getoverrun :: Ptr (CChar) -> IO (CInt)

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
