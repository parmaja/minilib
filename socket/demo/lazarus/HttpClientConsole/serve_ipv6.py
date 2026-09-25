#!/usr/bin/env python3
"""Minimal HTTP/1.1 server on IPv6 loopback (::1) for testing the MiniLib
socket client. Unlike `python -m http.server`, it keeps connections alive
(HTTP/1.1), so HttpClientConsole's Test 4 (keep-alive double GET) passes.

Usage:  python serve_ipv6.py [port]
Serve the current directory at  http://[::1]:PORT/  (default port 8000)
"""
import http.server
import sys

PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8000


class Handler(http.server.SimpleHTTPRequestHandler):
    protocol_version = "HTTP/1.1"


class Server(http.server.ThreadingHTTPServer):
    address_family = __import__("socket").AF_INET6


if __name__ == "__main__":
    print("Serving http://[::1]:%d/  (Ctrl+C to stop)" % PORT)
    Server(("::1", PORT), Handler).serve_forever()