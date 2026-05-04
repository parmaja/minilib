# MiniSockets

A lightweight, cross-platform socket library for Delphi and Free Pascal (Lazarus).

## Overview

MiniSockets provides a unified socket abstraction with high-level client/server components, HTTP/HTTPS support, SSL/TLS encryption, and modular web frameworks — all in native Pascal.

## Features

- **Core Sockets** (`mnSockets`)
  - Cross-platform TCP socket wrapper (Windows, Linux)
  - Blocking and non-blocking I/O with timeout support
  - Stream-based socket communication
  - Socket options: keep-alive, Nagle's algorithm, quick ACK, reuse address

- **SSL / TLS** (`mnOpenSSL`)
  - OpenSSL 1.1.1 integration for client and server sockets
  - Certificate and private key management
  - Automatic handshake for secure connections

- **Client Components** (`mnClients`)
  - TCP client sockets with connect/bind options
  - Threaded client connection management

- **Server Components** (`mnServers`)
  - Multi-threaded TCP server with listener framework
  - Simple server (single-procedure handler)
  - Event-driven server with lifecycle hooks
  - Per-connection thread management

- **HTTP Client** (`mnHttpClient`)
  - HTTP/1.1 and HTTPS support
  - GET, POST, PATCH, HEAD methods
  - File download, stream upload, and memory operations
  - Chunked transfer and compression support

- **HTTP Server** (`mnHttpServer`)
  - Basic HTTP/1.1 web server
  - Static file serving with MIME type detection
  - Keep-alive and 404 handling
  - Configurable document root and default pages

- **Web Modules** (`mnWebModules`)
  - Modular web application framework
  - URI routing with namespaces, aliases, and schemas
  - RESTful API support
  - File serving with smart index and authorization hooks

- **Web Elements** (`mnWebElements`)
  - Server-side HTML UI component framework
  - Forms, tables, cards, navbars, sidebars
  - Bootstrap and Tailwind CSS integration
  - Event-driven component model

- **Protocol Modules** (`mnModules`)
  - Reusable protocol layer for custom communication
  - Header/state management for request/response cycles
  - Compression support

- **IRC Client** (`mnIRCClients`)
  - Internet Relay Chat protocol implementation
  - Standard IRC commands and numeric replies

- **SMTP Client** (`mnSMTPClient`)
  - Email sending via SMTP with authentication
  - STARTTLS / SSL support

- **ZKTeco Client** (`mnZKTClients`)
  - Communication with ZKTeco biometric/access-control devices

## Platform Support

| Platform | Delphi | Lazarus (FPC) |
|----------|--------|---------------|
| Windows  | ✅     | ✅            |
| Linux    | —      | ✅            |

## Requirements

- Delphi (Win32/Win64) or Free Pascal 3.2+
- Lazarus IDE (for package installation)
- OpenSSL libraries (for SSL/TLS features)
- **MiniLib** base package (required dependency)

## Installation

1. Open `MiniSockets.lpk` in Lazarus.
2. Compile and install the package.
3. Add the package to your project requirements.

For Delphi, include the source path in your project search directories.

## License

- **Core library**: modified LGPL
- **Some components** (e.g., HTTP Client, IRC, SMTP): MIT

See individual source file headers for exact licensing terms.

## Authors

- Zaher Dirkey <zaherdirkey>
- Belal Hamed <belalhamed@gmail.com>
