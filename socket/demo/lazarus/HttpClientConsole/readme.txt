HttpClientConsole - console HTTP client demo for the MiniLib socket library
============================================================================

Build (FPC)
-----------
  fpc -Mdelphi -Fu..\..\..\..\lib -Fu..\..\..\source HttpClientConsole.lpr

or open HttpClientConsole.lpi in Lazarus and press Run (F9).

Usage
-----
  HttpClientConsole [url] [--save <file>]

  url      - the URL to test against.
             The default is http://www.parmaja.org/
  --save   - save the last downloaded body to a file

It runs a battery of client tests and always prints the actual socket family
(IPv4 / IPv6) used for each connection:

  Test 1: GetString       - one-shot GET
  Test 2: GetFileSize     - HEAD request (Content-Length)
  Test 3: Manual HEAD     - Connect + SendHead + ReceiveHeader
  Test 4: Keep-Alive      - two GETs on the same connection

IPv6
----
IPv6 addresses must be written in the standard bracketed form:

  HttpClientConsole http://[::1]:8000/       (localhost IPv6)
  HttpClientConsole "http://[2a00:1450:4001:82f::200e]/"   (any public IPv6)

The socket family line shows "IPv6" when the connection really went through
the IPv6 stack.

Tip: quick local IPv6 test. The bundled serve_ipv6.py is a tiny HTTP/1.1
server on ::1 (keeps connections alive, so Test 4 also passes):

  python serve_ipv6.py          (serves this folder at http://[::1]:8000/)

  in another console:

  HttpClientConsole http://[::1]:8000/

or, without keep-alive support, the built-in server also works on IPv6:

  python -m http.server 8000 --bind ::1

Note: by default the Windows firewall may block python on ::1, allow access
when prompted, or use -4 to test IPv4 first.

Files
-----
  HttpClientConsole.lpr   - the console program
  HttpClientConsole.lpi   - Lazarus project