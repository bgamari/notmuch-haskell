# notmuch-haskell
Copyright © 2010 Bart Massey

This is a Haskell FFI binding to the
[notmuch](http://notmuchmail.org) email client library.
It's pretty slap-together and not well-maintained.  There's
no Haddock, though there should be--use the `notmuch`
docs. Patches welcome.

This binding is currently for `libnotmuch3` as exported by
the Debian package `libnotmuch3=0.13.2-1`. Your mileage may
vary.

XXX As of GHC 7.2, the compiler no longer checks that the
API description in the hsc file matches that in `notmuch.h`,
no matter how this stuff is built. Thus, when API changes
happen, this code will be silently hosed. If you're lucky,
there will be crashes.

This work is available under the LGPL version 3.0. See the
file COPYING in this distribution for license terms.
