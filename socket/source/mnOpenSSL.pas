unit mnOpenSSL;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "MiniLib"
 *
 * @license   Mit
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey zaherdirkey
 * #thanks    To all who i get some code from him
 *
 *}

 //https://www.xolphin.com/support/OpenSSL/OpenSSL_-_Installation_under_Windows
 //https://wiki.openssl.org/index.php/Libcrypto_API
 //https://slproweb.com/products/Win32OpenSSL.html
 //https://github.com/sota2502/mpstation/blob/master/lib/IdSSLOpenSSLHeaders.pas
 //https://fuchsia-docs.firebaseapp.com/rust/src/boringssl_sys/lib.rs.html#1929

interface

uses
  Types, Classes, SysUtils,
  ctypes,
  mnLibraries; // take it from github/parmaja/minilib

{$MINENUMSIZE 4} //All enum must be sized as Integer
{$Z4}
{$A8}

const
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

  SSL_MODE_ENABLE_PARTIAL_WRITE                 = $00000001;
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER           = $00000002;
  SSL_MODE_AUTO_RETRY                           = $00000004;
  SSL_MODE_NO_AUTO_CHAIN                        = $00000008;
  SSL_MODE_RELEASE_BUFFERS                      = $00000010;

  SSL_OP_MICROSOFT_SESS_ID_BUG                  = $00000001;
  SSL_OP_NETSCAPE_CHALLENGE_BUG                 = $00000002;
  SSL_OP_LEGACY_SERVER_CONNECT                  = $00000004;
  SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG       = $00000008;
  SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG            = $00000010;
  SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER             = $00000020;
  SSL_OP_MSIE_SSLV2_RSA_PADDING                 = $00000040;
  SSL_OP_SAFARI_ECDHE_ECDSA_BUG                 = $00000040;
  SSL_OP_SSLEAY_080_CLIENT_DH_BUG               = $00000080;
  SSL_OP_TLS_D5_BUG                             = $00000100;
  SSL_OP_TLS_BLOCK_PADDING_BUG                  = $00000200;
  SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS            = $00000800;
  SSL_OP_NO_QUERY_MTU                           = $00001000;
  SSL_OP_COOKIE_EXCHANGE                        = $00002000;
  SSL_OP_NO_TICKET                              = $00004000;
  SSL_OP_CISCO_ANYCONNECT                       = $00008000;
  SSL_OP_ALL                                    = $000FFFFF;
  SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = $00010000;
  SSL_OP_NO_COMPRESSION                         = $00020000;
  SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION      = $00040000;
  SSL_OP_SINGLE_ECDH_USE                        = $00080000;
  SSL_OP_SINGLE_DH_USE                          = $00100000;
  SSL_OP_EPHEMERAL_RSA                          = $00200000;
  SSL_OP_CIPHER_SERVER_PREFERENCE               = $00400000;
  SSL_OP_TLS_ROLLBACK_BUG                       = $00800000;
  SSL_OP_NO_SSLv2                               = $01000000;
  SSL_OP_NO_SSLv3                               = $02000000;
  SSL_OP_NO_TLSv1                               = $04000000;
  SSL_OP_NO_TLSv1_2                             = $08000000;
  SSL_OP_NO_TLSv1_1                             = $10000000;
  SSL_OP_NETSCAPE_CA_DN_BUG                     = $20000000;
  SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG        = $40000000;
  SSL_OP_CRYPTOPRO_TLSEXT_BUG                   = $80000000;

//bio.h
  BIO_C_SET_CONNECT                             = 100;

type

  TOPENSSL_INIT_SETTINGS =record
    FileName: PAnsiChar;
    AppName: PAnsiChar;
   end;

  POPENSSL_INIT_SETTINGS = ^TOPENSSL_INIT_SETTINGS;

  PSSL = Pointer;
  PSSL_CTX = Pointer;
  PSSL_METHOD = Pointer;
  PBIO = Pointer;

  PX509_STORE_CTX = Pointer;

  TSSLVerifyCallback = function(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;

  { TmncRayLib }

  TmnOpenSSLLib = class(TmnLibrary)
  public
  protected
    procedure Loaded; override;
  end;

  { TCryptoLibLib }

  TmnCryptoLib = class(TmnLibrary)
  public
  protected
    procedure Loaded; override;
  end;

var
  OPENSSL_init_ssl: procedure(opts: UInt64; settings: POPENSSL_INIT_SETTINGS); cdecl;
  OPENSSL_init_crypto: function(opts: uint64; settings: POPENSSL_INIT_SETTINGS): Integer; cdecl;
  SSL_CTX_new: function(Method: PSSL_METHOD): PSSL_CTX; cdecl;
  TLS_method: function(): PSSL_METHOD; cdecl;
  SSL_CTX_set_verify: procedure(ctx: PSSL_CTX; Mode: Integer; Callback: TSSLVerifyCallback); cdecl;
  SSL_CTX_set_verify_depth: procedure(ctx: PSSL_CTX; Depth: integer); cdecl;
  SSL_CTX_set_options: function(ctx: PSSL_CTX; Options: culong): culong; cdecl;
  SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; CAfile: PChar; CApath: PChar): Integer; cdecl;
  BIO_new_ssl_connect: function(ctx: PSSL_CTX): PBIO; cdecl;

  //https://www.openssl.org/docs/man1.1.1/man3/BIO_ctrl.html
  BIO_ctrl: function(bp: PBIO; cmd: Integer; Larg: clong; Parg: Pointer): clong; cdecl;

  function BIO_set_conn_hostname(b: PBIO; Name: PChar): clong; //inline;

var
  OpenSSLLib: TmnOpenSSLLib = nil;
  CryptoLib: TmnCryptoLib = nil;

implementation

function BIO_set_conn_hostname(b: PBIO; Name: PChar): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_CONNECT, 0, Name);
end;

{ TmnOpenSSLLib }

procedure TmnOpenSSLLib.Loaded;
begin
  RaiseError := True; //Raise error of one of this functions not exists
  OPENSSL_init_ssl := GetAddress('OPENSSL_init_ssl');
  SSL_CTX_new := GetAddress('SSL_CTX_new');
  TLS_method := GetAddress('TLS_method');
  SSL_CTX_set_verify := GetAddress('SSL_CTX_set_verify');
  SSL_CTX_set_verify_depth := GetAddress('SSL_CTX_set_verify_depth');
  SSL_CTX_set_options := GetAddress('SSL_CTX_set_options');
  SSL_CTX_load_verify_locations := GetAddress('SSL_CTX_load_verify_locations');
  BIO_new_ssl_connect := GetAddress('BIO_new_ssl_connect');
end;

{ TCryptoLibLib }

procedure TmnCryptoLib.Loaded;
begin
  RaiseError := True; //Raise error of one of this functions not exists
  OPENSSL_init_crypto := GetAddress('OPENSSL_init_crypto');
  BIO_ctrl := GetAddress('BIO_ctrl');
end;

initialization
  OpenSSLLib := TmnOpenSSLLib.Create('libssl-1_1');
  CryptoLib := TmnCryptoLib.Create('libcrypto-1_1');
finalization
  FreeAndNil(OpenSSLLib);
  FreeAndNil(CryptoLib);
end.

