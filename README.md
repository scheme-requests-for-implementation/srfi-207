# Bytestrings

This is an implementation of a
[pre-SRFI specification](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/BytestringsCowan.md)
by John Cowan which provides bytestrings for Scheme.  It should be
portable to any R7RS-small implementation with
[SRFI 1/(scheme list)](https://srfi.schemers.org/srfi-145);
[SRFI 145](https://srfi.schemers.org/srfi-145) and
[(scheme bytevector)](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2)
are optional dependencies.

The implementation of base64 encoding and decoding is from
[Chibi Scheme](http://synthcode.com/wiki/chibi-scheme).

# Author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy minus the zy

# License

This is free software released under the MIT/X license.  See
LICENSE for details.
