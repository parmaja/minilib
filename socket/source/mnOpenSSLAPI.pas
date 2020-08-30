unit mnOpenSSLAPI;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 *  This file is part of the "MiniLib"
 *
 * @license   Mit
 * @author    Zaher Dirkey zaherdirkey
 * @thanks    To all who i get some code from them
 *
 *}

 //https://aticleworld.com/ssl-server-client-using-openssl-in-c/
 //https://eclipsesource.com/blogs/2016/09/07/tutorial-code-signing-and-verification-with-openssl/

 //https://www.xolphin.com/support/OpenSSL/OpenSSL_-_Installation_under_Windows
 //https://wiki.openssl.org/index.php/Libcrypto_API
 //https://slproweb.com/products/Win32OpenSSL.html
 //https://github.com/sota2502/mpstation/blob/master/lib/IdSSLOpenSSLHeaders.pas
 //https://fuchsia-docs.firebaseapp.com/rust/src/boringssl_sys/lib.rs.html#1929

 //official examples
 //https://www.openssl.org/docs/man1.0.2/man3/BIO_get_ssl.html

interface

uses
  SysUtils,
  mnLibraries; // take it from github/parmaja/minilib

type
  clong = NativeInt;
  culong = NativeUInt;

  BN_ULONG = culong;

{$MINENUMSIZE 4} //All enum must be sized as Integer
{$Z4}
{$A8}

const
  SSL_VERIFY_NONE                 = $00;
  SSL_VERIFY_PEER                 = $01;
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;
  SSL_VERIFY_CLIENT_ONCE          = $04;
  SSL_VERIFY_POST_HANDSHAKE       = $08;

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

  (* Standard initialisation options *)
  OPENSSL_INIT_NO_LOAD_CRYPTO_STRINGS = $00000001;
  OPENSSL_INIT_LOAD_CRYPTO_STRINGS    = $00000002;
  OPENSSL_INIT_ADD_ALL_CIPHERS        = $00000004;
  OPENSSL_INIT_ADD_ALL_DIGESTS        = $00000008;
  OPENSSL_INIT_NO_ADD_ALL_CIPHERS     = $00000010;
  OPENSSL_INIT_NO_ADD_ALL_DIGESTS     = $00000020;
  OPENSSL_INIT_LOAD_CONFIG            = $00000040;
  OPENSSL_INIT_NO_LOAD_CONFIG         = $00000080;
  OPENSSL_INIT_ASYNC                  = $00000100;
  OPENSSL_INIT_ENGINE_RDRAND          = $00000200;
  OPENSSL_INIT_ENGINE_DYNAMIC         = $00000400;
  OPENSSL_INIT_ENGINE_OPENSSL         = $00000800;
  OPENSSL_INIT_ENGINE_CRYPTODEV       = $00001000;
  OPENSSL_INIT_ENGINE_CAPI            = $00002000;
  OPENSSL_INIT_ENGINE_PADLOCK         = $00004000;
  OPENSSL_INIT_ENGINE_AFALG           = $00008000;
  (* OPENSSL_INIT_ZLIB                         = $00010000; *)
  OPENSSL_INIT_ATFORK                 = $00020000;
  (* OPENSSL_INIT_BASE_ONLY                    = $00040000; *)
  OPENSSL_INIT_NO_ATEXIT              = $00080000;
  (* OPENSSL_INIT flag range = $fff00000 reserved for OPENSSL_init_ssl() *)
  (* Max OPENSSL_INIT flag value is = $80000000 *)

  (* OPENSSL_INIT flag 0x010000 reserved for internal use *)
  OPENSSL_INIT_NO_LOAD_SSL_STRINGS    = $00100000;
  OPENSSL_INIT_LOAD_SSL_STRINGS       = $00200000;

  OPENSSL_INIT_SSL_DEFAULT           = (OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS);

  SSL_ERROR_NONE                 = 0;
  SSL_ERROR_SSL                  = 1;
  SSL_ERROR_WANT_READ            = 2;
  SSL_ERROR_WANT_WRITE           = 3;
  SSL_ERROR_WANT_X509_LOOKUP     = 4;
  SSL_ERROR_SYSCALL              = 5; (* look at error stack/return * value/errno *)
  SSL_ERROR_ZERO_RETURN          = 6;
  SSL_ERROR_WANT_CONNECT         = 7;
  SSL_ERROR_WANT_ACCEPT          = 8;
  SSL_ERROR_WANT_ASYNC           = 9;
  SSL_ERROR_WANT_ASYNC_JOB       = 10;
  SSL_ERROR_WANT_CLIENT_HELLO_CB = 11;

  SSL_CTRL_SET_TMP_DH                    = 3;
  SSL_CTRL_SET_TMP_ECDH                  = 4;
  SSL_CTRL_SET_TMP_DH_CB                 = 6;
  SSL_CTRL_GET_CLIENT_CERT_REQUEST       = 9;
  SSL_CTRL_GET_NUM_RENEGOTIATIONS        = 10;
  SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS      = 11;
  SSL_CTRL_GET_TOTAL_RENEGOTIATIONS      = 12;
  SSL_CTRL_GET_FLAGS                     = 13;
  SSL_CTRL_EXTRA_CHAIN_CERT              = 14;
  SSL_CTRL_SET_MSG_CALLBACK              = 15;
  SSL_CTRL_SET_MSG_CALLBACK_ARG          = 16;
  (* only applies to datagram connections *)
  SSL_CTRL_SET_MTU               = 17;
  (* Stats *)
  SSL_CTRL_SESS_NUMBER                   = 20;
  SSL_CTRL_SESS_CONNECT                  = 21;
  SSL_CTRL_SESS_CONNECT_GOOD             = 22;
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE      = 23;
  SSL_CTRL_SESS_ACCEPT                   = 24;
  SSL_CTRL_SESS_ACCEPT_GOOD              = 25;
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE       = 26;
  SSL_CTRL_SESS_HIT                      = 27;
  SSL_CTRL_SESS_CB_HIT                   = 28;
  SSL_CTRL_SESS_MISSES                   = 29;
  SSL_CTRL_SESS_TIMEOUTS                 = 30;
  SSL_CTRL_SESS_CACHE_FULL               = 31;
  SSL_CTRL_MODE                          = 33;
  SSL_CTRL_GET_READ_AHEAD                = 40;
  SSL_CTRL_SET_READ_AHEAD                = 41;
  SSL_CTRL_SET_SESS_CACHE_SIZE           = 42;
  SSL_CTRL_GET_SESS_CACHE_SIZE           = 43;
  SSL_CTRL_SET_SESS_CACHE_MODE           = 44;
  SSL_CTRL_GET_SESS_CACHE_MODE           = 45;
  SSL_CTRL_GET_MAX_CERT_LIST             = 50;
  SSL_CTRL_SET_MAX_CERT_LIST             = 51;
  SSL_CTRL_SET_MAX_SEND_FRAGMENT         = 52;
  (* see tls1.h for macros based on these *)
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB      = 53;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG     = 54;
  SSL_CTRL_SET_TLSEXT_HOSTNAME           = 55;
  SSL_CTRL_SET_TLSEXT_DEBUG_CB           = 56;
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG          = 57;
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS        = 58;
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS        = 59;
  (*SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT    60 *)
  (*SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB 61 *)
  (*SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG 62 *)
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB      = 63;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG  = 64;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE    = 65;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS    = 66;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS    = 67;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS     = 68;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS     = 69;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP       = 70;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP       = 71;
  SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB      = 72;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB   = 75;
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB               = 76;
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB            = 77;
  SSL_CTRL_SET_SRP_ARG           = 78;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME              = 79;
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH              = 80;
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD              = 81;
  {$ifdef OPENSSL_NO_HEARTBEATS};
  SSL_CTRL_DTLS_EXT_SEND_HEARTBEAT              = 85;
  SSL_CTRL_GET_DTLS_EXT_HEARTBEAT_PENDING       = 86;
  SSL_CTRL_SET_DTLS_EXT_HEARTBEAT_NO_REQUESTS   = 87;
  {$endif}
  DTLS_CTRL_GET_TIMEOUT          = 73;
  DTLS_CTRL_HANDLE_TIMEOUT       = 74;
  SSL_CTRL_GET_RI_SUPPORT                = 76;
  SSL_CTRL_CLEAR_MODE                    = 78;
  SSL_CTRL_SET_NOT_RESUMABLE_SESS_CB     = 79;
  SSL_CTRL_GET_EXTRA_CHAIN_CERTS         = 82;
  SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS       = 83;
  SSL_CTRL_CHAIN                         = 88;
  SSL_CTRL_CHAIN_CERT                    = 89;
  SSL_CTRL_GET_GROUPS                    = 90;
  SSL_CTRL_SET_GROUPS                    = 91;
  SSL_CTRL_SET_GROUPS_LIST               = 92;
  SSL_CTRL_GET_SHARED_GROUP              = 93;
  SSL_CTRL_SET_SIGALGS                   = 97;
  SSL_CTRL_SET_SIGALGS_LIST              = 98;
  SSL_CTRL_CERT_FLAGS                    = 99;
  SSL_CTRL_CLEAR_CERT_FLAGS              = 100;
  SSL_CTRL_SET_CLIENT_SIGALGS            = 101;
  SSL_CTRL_SET_CLIENT_SIGALGS_LIST       = 102;
  SSL_CTRL_GET_CLIENT_CERT_TYPES         = 103;
  SSL_CTRL_SET_CLIENT_CERT_TYPES         = 104;
  SSL_CTRL_BUILD_CERT_CHAIN              = 105;
  SSL_CTRL_SET_VERIFY_CERT_STORE         = 106;
  SSL_CTRL_SET_CHAIN_CERT_STORE          = 107;
  SSL_CTRL_GET_PEER_SIGNATURE_NID        = 108;
  SSL_CTRL_GET_PEER_TMP_KEY              = 109;
  SSL_CTRL_GET_RAW_CIPHERLIST            = 110;
  SSL_CTRL_GET_EC_POINT_FORMATS          = 111;
  SSL_CTRL_GET_CHAIN_CERTS               = 115;
  SSL_CTRL_SELECT_CURRENT_CERT           = 116;
  SSL_CTRL_SET_CURRENT_CERT              = 117;
  SSL_CTRL_SET_DH_AUTO                   = 118;
  DTLS_CTRL_SET_LINK_MTU                 = 120;
  DTLS_CTRL_GET_LINK_MIN_MTU             = 121;
  SSL_CTRL_GET_EXTMS_SUPPORT             = 122;
  SSL_CTRL_SET_MIN_PROTO_VERSION         = 123;
  SSL_CTRL_SET_MAX_PROTO_VERSION         = 124;
  SSL_CTRL_SET_SPLIT_SEND_FRAGMENT       = 125;
  SSL_CTRL_SET_MAX_PIPELINES             = 126;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE    = 127;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB      = 128;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG  = 129;
  SSL_CTRL_GET_MIN_PROTO_VERSION         = 130;
  SSL_CTRL_GET_MAX_PROTO_VERSION         = 131;
  SSL_CTRL_GET_SIGNATURE_NID             = 132;
  SSL_CTRL_GET_TMP_KEY                   = 133;

  SSL_CERT_SET_FIRST                     = 1;
  SSL_CERT_SET_NEXT                      = 2;
  SSL_CERT_SET_SERVER                    = 3;

  X509_FILETYPE_PEM      = 1;
  X509_FILETYPE_ASN1     = 2;
  X509_FILETYPE_DEFAULT  = 3;

  SSL_FILETYPE_ASN1      = X509_FILETYPE_ASN1;
  SSL_FILETYPE_PEM       = X509_FILETYPE_PEM;

//bio.h
  BIO_C_SET_CONNECT                             = 100;
  BIO_C_DO_STATE_MACHINE                        = 101;
  BIO_C_SET_NBIO                                = 102;
  //BIO_C_SET_PROXY_PARAM               = 103;
  BIO_C_SET_FD                                  = 104;
  BIO_C_GET_FD                                  = 105;
  BIO_C_SET_FILE_PTR                            = 106;
  BIO_C_GET_FILE_PTR                            = 107;
  BIO_C_SET_FILENAME                            = 108;
  BIO_C_SET_SSL                                 = 109;
  BIO_C_GET_SSL                                 = 110;

  BIO_FLAGS_READ          = $01;
  BIO_FLAGS_WRITE         = $02;
  BIO_FLAGS_IO_SPECIAL    = $04;
  BIO_FLAGS_RWS           = (BIO_FLAGS_READ or BIO_FLAGS_WRITE or BIO_FLAGS_IO_SPECIAL);
  BIO_FLAGS_SHOULD_RETRY  = $08;

  BIO_CTRL_DGRAM_SET_RECV_TIMEOUT = 33;(* setsockopt, essentially *)
  BIO_CTRL_DGRAM_GET_RECV_TIMEOUT = 34;(* getsockopt, essentially *)
  BIO_CTRL_DGRAM_SET_SEND_TIMEOUT = 35;(* setsockopt, essentially *)
  BIO_CTRL_DGRAM_GET_SEND_TIMEOUT = 36;(* getsockopt, essentially *)


  EVP_PK_RSA       = $0001;
  EVP_PK_DSA       = $0002;
  EVP_PK_DH        = $0004;
  EVP_PK_EC        = $0008;
  EVP_PKT_SIGN     = $0010;
  EVP_PKT_ENC      = $0020;
  EVP_PKT_EXCH     = $0040;
  EVP_PKS_RSA      = $0100;
  EVP_PKS_DSA      = $0200;
  EVP_PKS_EC       = $0400;

  NID_undef                     =  0;

  LN_rsaEncryption               = 'rsaEncryption';
  NID_rsaEncryption              = 6;
  //OBJ_rsaEncryption              = OBJ_pkcs1,1L

  SN_subject_key_identifier              = 'subjectKeyIdentifier';
  LN_subject_key_identifier              = 'X509v3 Subject Key Identifier';
  NID_subject_key_identifier             = 82;
  //OBJ_subject_key_identifier           = OBJ_id_ce,14L

  SN_key_usage          =  'keyUsage';
  LN_key_usage          =  'X509v3 Key Usage';
  NID_key_usage         =  83;
  //OBJ_key_usage         =  OBJ_id_ce,15L;

  SN_basic_constraints            = 'basicConstraints';
  LN_basic_constraints            = 'X509v3 Basic Constraints';
  NID_basic_constraints           = 87;
  //OBJ_basic_constraints          = OBJ_id_ce,19L

  EVP_PKEY_NONE   = NID_undef;
  EVP_PKEY_RSA    = NID_rsaEncryption;

{TODO

  EVP_PKEY_RSA2   NID_rsa;
  EVP_PKEY_RSA_PSS NID_rsassaPss;
  EVP_PKEY_DSA    NID_dsa;
  EVP_PKEY_DSA1   NID_dsa_2;
  EVP_PKEY_DSA2   NID_dsaWithSHA;
  EVP_PKEY_DSA3   NID_dsaWithSHA1;
  EVP_PKEY_DSA4   NID_dsaWithSHA1_2;
  EVP_PKEY_DH     NID_dhKeyAgreement;
  EVP_PKEY_DHX    NID_dhpublicnumber;
  EVP_PKEY_EC     NID_X9_62_id_ecPublicKey;
  EVP_PKEY_SM2    NID_sm2;
  EVP_PKEY_HMAC   NID_hmac;
  EVP_PKEY_CMAC   NID_cmac;
  EVP_PKEY_SCRYPT NID_id_scrypt;
  EVP_PKEY_TLS1_PRF NID_tls1_prf;
  EVP_PKEY_HKDF   NID_hkdf;
  EVP_PKEY_POLY1305 NID_poly1305;
  EVP_PKEY_SIPHASH NID_siphash;
  EVP_PKEY_X25519 NID_X25519;
  EVP_PKEY_ED25519 NID_ED25519;
  EVP_PKEY_X448 NID_X448;
  EVP_PKEY_ED448 NID_ED448;}

  EVP_PKEY_MO_SIGN         = $0001;
  EVP_PKEY_MO_VERIFY       = $0002;
  EVP_PKEY_MO_ENCRYPT      = $0004;
  EVP_PKEY_MO_DECRYPT      = $0008;


  RSA_3  = $3;
  RSA_F4 = $10001;

  {* For use with ASN1_mbstring_copy() *}
  MBSTRING_FLAG         =  $1000;
  MBSTRING_UTF8         =  (MBSTRING_FLAG);
  MBSTRING_ASC          =  (MBSTRING_FLAG or 1);
  MBSTRING_BMP          =  (MBSTRING_FLAG or 2);
  MBSTRING_UNIV         =  (MBSTRING_FLAG or 4);
  SMIME_OLDMIME         =  $400;
  SMIME_CRLFEOL         =  $800;
  SMIME_STREAM          =  $1000;

  TLSEXT_NAMETYPE_host_name                     = 0;

  (*
   * BIO_FILENAME_READ|BIO_CLOSE to open or close on free.
   * BIO_set_fp(in,stdin,BIO_NOCLOSE);
   *)
  BIO_NOCLOSE           = $00;
  BIO_CLOSE             = $01;

  (*
   * The following can be used to detect memory leaks in the library. If
   * used, it turns on malloc checking
   *)
  CRYPTO_MEM_CHECK_OFF     = $0;   (* Control only *)
  CRYPTO_MEM_CHECK_ON      = $1;   (* Control and mode bit *)
  CRYPTO_MEM_CHECK_ENABLE  = $2;   (* Control and mode bit *)
  CRYPTO_MEM_CHECK_DISABLE = $3;   (* Control only *)

type

  TOPENSSL_INIT_SETTINGS =record
    FileName: PAnsiChar;
    AppName: PAnsiChar;
   end;

  POPENSSL_INIT_SETTINGS = ^TOPENSSL_INIT_SETTINGS;

  //PSLLObject = class(TObject);
  PSLLObject = type Pointer;

  PSSL = PSLLObject;
  PSSL_CTX = PSLLObject;
  PSSL_METHOD = PSLLObject;
  PBIO = PSLLObject;

  PBIGNUM = PSLLObject;
  PBN_GENCB = PSLLObject;
  PRSA = Pointer;

  PASN1_INTEGER = PSLLObject;
  PASN1_TIME = PSLLObject;

  PX509 = type PSLLObject;
  PX509_STORE_CTX = PSLLObject;
  PX509_REQ = PSLLObject;
  PX509_CRL =PSLLObject;
  PX509V3_CONF_METHOD = PSLLObject;
  PPX509_REQ = ^PX509_REQ;
  PX509_NAME = PSLLObject;
  PX509_sign = PSLLObject;
  PX509_EXTENSION = PSLLObject;
  PLHASH = PSLLObject;

  Ppem_password_cb = Pointer;

  PEVP_CIPHER = PSLLObject;
  PEVP_PKEY = PSLLObject;
  PEVP_MD = PSLLObject;

  TSSLVerifyCallback = function(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;

  { Context specific info }
  //https://abi-laboratory.pro/index.php?view=type_view&l=openssl&v=1.0.2e&obj=c93f7&t=1ede6
  //CTX_TEST 0x1
  //X509V3_CTX_REPLACE 0x2
  Tv3_ext_ctx = packed record
      flags: Integer;
      issuer_cert: PX509;
      subject_cert: PX509;
      subject_req: PX509_REQ;
      crl: PX509_CRL;
      db_meth: PX509V3_CONF_METHOD;
      db: Pointer;
    // Maybe more here
  end;
  Pv3_ext_ctx = ^Tv3_ext_ctx;

  TX509V3_CTX = Tv3_ext_ctx;
  PX509V3_CTX = ^TX509V3_CTX;

  { TmnOpenSSLLib }

  TmnOpenSSLLib = class(TmnLibrary)
  public
  protected
    procedure Link; override;
  end;

  { TmnCryptoLib }

  TmnCryptoLib = class(TmnLibrary)
  public
  protected
    procedure Link; override;
  end;

var
  OPENSSL_init_ssl: procedure(opts: UInt64; settings: POPENSSL_INIT_SETTINGS); cdecl;
  OPENSSL_init_crypto: function(opts: uint64; settings: POPENSSL_INIT_SETTINGS): Integer; cdecl;

  OPENSSL_config: procedure(AppName: PUTF8Char); cdecl;
  SSL_get_error: function(s: PSSL; ret_code: Integer): Integer; cdecl;

  ERR_load_SSL_strings: procedure(); cdecl;
  ERR_load_CRYPTO_strings: function(): Integer; cdecl;
  ERR_error_string: function(e: culong; bug: PUTF8Char): PUTF8Char; cdecl;
  ERR_get_error: function(): clong; cdecl;

  SSL_get_peer_certificate: function(ssl: PSSL): PX509; cdecl;
  SSL_set_cipher_list: function(ssl: PSSL; str: PUTF8Char): Integer; cdecl;
  SSL_set_verify: procedure(ssl: PSSL; Mode: Integer; Callback: TSSLVerifyCallback); cdecl;
  SSL_get_verify_result: function(ssl: PSSL): clong; cdecl;

  SSL_ctrl: function(ssl: PSSL; cmd: Integer; Larg: clong; PArg: Pointer): clong; cdecl;
  SSL_new: function(ctx: PSSL_CTX): PSSL; cdecl;
  SSL_free: procedure(ssl: PSSL); cdecl;
  SSL_get_version: function(const ssl: PSSL): PUTF8Char; cdecl;
  SSL_set_fd: function(ssl: PSSL; d: Integer): integer; cdecl;
  SSL_connect: function(ssl: PSSL): Integer; cdecl;
  SSL_accept: function(ssl: PSSL): Integer; cdecl;

  SSL_read: function(ssl: PSSL; var buf; size: integer): integer; cdecl;
  SSL_write: function(ssl: PSSL; const buf; size: integer): integer; cdecl;

  SSL_pending: function(ssl: PSSL): Integer; cdecl;
  SSL_has_pending: function(ssl: PSSL): Integer; cdecl;

  SSL_CTX_new: function(Method: PSSL_METHOD): PSSL_CTX; cdecl;
  SSL_CTX_set_verify: procedure(ctx: PSSL_CTX; Mode: Integer; Callback: TSSLVerifyCallback); cdecl;
  SSL_CTX_set_verify_depth: procedure(ctx: PSSL_CTX; Depth: integer); cdecl;
  SSL_CTX_set_options: function(ctx: PSSL_CTX; Options: culong): culong; cdecl;
  SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; CAfile: PUTF8Char; CApath: PUTF8Char): Integer; cdecl;
  SSL_CTX_free: procedure(ctx: PSSL_CTX); cdecl;
  SSL_CTX_use_certificate_file: function(ctx: PSSL_CTX; afile: PUTF8Char; atype: Integer): Integer; cdecl;
  SSL_CTX_use_PrivateKey_file: function(ctx: PSSL_CTX; const afile: PUTF8Char; atype: Integer): Integer; cdecl;
  SSL_CTX_check_private_key: function(ctx: PSSL_CTX): Integer; cdecl;

  TLS_method: function(): PSSL_METHOD; cdecl;
  TLS_server_method: function(): PSSL_METHOD; cdecl;

  X509_new: function(): PX509; cdecl;
  X509_STORE_CTX_get_error_depth: function(ctx: PX509_STORE_CTX): Integer; cdecl;
  X509_free: procedure(a: PX509); cdecl;
  X509_verify_cert_error_string: function(n: clong): PUTF8Char; cdecl;
  X509_sign: function(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): PX509_sign; cdecl;
  X509_REQ_new: function(): PX509_REQ; cdecl;
  X509_REQ_get_subject_name: function(req: PX509_REQ): PX509_NAME; cdecl;
  X509_REQ_set_pubkey: function(x: PX509_REQ; pkey: PEVP_PKEY): Integer; cdecl;
  X509_REQ_sign: function(x: PX509_REQ; pkey: PEVP_PKEY; const md: PEVP_MD): integer; cdecl;
  X509_NAME_add_entry_by_txt: function(name: PX509_NAME; field: PUTF8Char; aType: Integer; const Bytes: PByte; Len: integer; Loc: Integer; ASet: integer): Integer; cdecl;
  X509_REQ_free: procedure(a: PX509_REQ); cdecl;
  X509_add_ext: function(x: PX509; ex: PX509_EXTENSION; loc: Integer): integer; cdecl;
  X509_EXTENSION_free: procedure(a: PX509_EXTENSION); cdecl;
  X509_set_issuer_name: function(x: PX509; name: PX509_NAME): Integer; cdecl;


  X509V3_set_ctx: procedure(ctx: PX509V3_CTX; issuer: PX509; subject: PX509; req: PX509_REQ; crl: PX509_CRL; flags: integer); cdecl;

  X509V3_EXT_conf_nid: function(conf: PLHASH; ctx: PX509V3_CTX; ext_nid: integer; value: PUTF8Char): PX509_EXTENSION; cdecl;
  X509_set_version: function(x: PX509; version: clong): Integer; cdecl;

  X509_REQ_set_version: function(x: PX509_REQ; version: clong): Integer; cdecl;

  PEM_read_bio_X509_REQ: function(bp: PBIO; x: PPX509_REQ; cb: Ppem_password_cb; var u): PX509_REQ; cdecl;
  PEM_write_bio_X509_REQ: function(bp: PBIO; x: PX509_REQ): Integer; cdecl;
  PEM_write_bio_PrivateKey: function(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr:PByte; klen: Integer; cb: Ppem_password_cb; u: Pointer): integer; cdecl;
  PEM_write_bio_X509: function(bp: PBIO; x: PX509): Integer; cdecl;


  ASN1_INTEGER_set_int64: function(a: PASN1_INTEGER; r: Int64): Integer; cdecl;
  ASN1_INTEGER_set: function(const a: PASN1_INTEGER; v: Integer): Integer; cdecl;
  X509_get_serialNumber: function(x: PX509): PASN1_INTEGER; cdecl;
  X509_gmtime_adj: function(s: PASN1_TIME; adj: clong): PASN1_TIME; cdecl;
  X509_getm_notBefore: function(x: PX509): PASN1_TIME; cdecl;
  X509_getm_notAfter: function(x: PX509): PASN1_TIME; cdecl;
  X509_set_pubkey: function(x: PX509; pkey: PEVP_PKEY): Integer; cdecl;
  X509_get_subject_name: function(x: PX509): PX509_NAME; cdecl;

  EVP_PKEY_new: function(): PEVP_PKEY; cdecl;
  EVP_PKEY_assign: function(pkey: PEVP_PKEY; AType: integer; key: Pointer): Integer; cdecl;
  EVP_PKEY_get0_RSA: function(pkey: PEVP_PKEY): PRSA; cdecl;
  EVP_PKEY_get1_RSA: function(pkey: PEVP_PKEY): PRSA; cdecl;
  EVP_PKEY_free: procedure(key: PEVP_PKEY); cdecl;

  //EVP_cleanup: procedure(); cdecl;
  EVP_md_null: function(): PEVP_MD; cdecl;
  //EVP_md2: function(): PEVP_MD; cdecl; not exists in 1.1
  EVP_md5: function(): PEVP_MD; cdecl;
  EVP_sha1: function(): PEVP_MD; cdecl;
  //EVP_mdc2: function(): PEVP_MD; cdecl;
  EVP_ripemd160: function(): PEVP_MD; cdecl;
  EVP_blake2b512: function(): PEVP_MD; cdecl;
  EVP_blake2s256: function(): PEVP_MD; cdecl;

  EVP_sha224: function(): PEVP_MD; cdecl;
  EVP_sha256: function(): PEVP_MD; cdecl;
  EVP_sha384: function(): PEVP_MD; cdecl;
  EVP_sha512: function(): PEVP_MD; cdecl;

  BN_new: function(): PBIGNUM; cdecl;
  BN_set_word: function(a: PBIGNUM; w: BN_ULONG): integer; cdecl;
  BN_free: procedure(a: PBIGNUM); cdecl;

  RSA_new: function(): PRSA; cdecl;
  RSA_generate_key_ex: function(rsa: PRSA; bits: integer; e: PBIGNUM; cb: PBN_GENCB): Integer; cdecl;
  RSA_print: function(bp: PBIO; x: PRSA; offset: integer): Integer; cdecl;
  RSA_print_fp: function(fp: Pointer; x: PRSA; offset: integer): Integer; cdecl;

  CRYPTO_mem_ctrl: function(mode: integer): integer; cdecl;

  BIO_new_ssl_connect: function(ctx: PSSL_CTX): PBIO; cdecl;
  BIO_new_fp: function(handle: THandle; close_flag: Integer): PBIO; cdecl; //dosnt work
  BIO_new_fd: function(handle: THandle; close_flag: Integer): PBIO; cdecl; //idk
  BIO_new_file: function(filename: PUTF8Char; Mode: PUTF8Char): PBIO; cdecl;

  BIO_read: function(b: PBIO; var data; dlen: integer): Integer; cdecl;
  BIO_write: function(b: PBIO; const data; dlen: Integer): Integer; cdecl;
  BIO_gets: function(b: PBIO; buf: PAnsiChar; Size: Integer): Integer; cdecl;
  BIO_puts: function(bio: PBIO; buf: PUTF8Char): Integer; cdecl;
  BIO_test_flags: function(b: PBIO; flags: Integer): integer; cdecl;
  BIO_free_all: procedure(b: PBIO); cdecl;
  BIO_free: function(bio: PBIO): Integer; cdecl;

  {TODO
  int BIO_read_ex(BIO *b, void *data, size_t dlen, size_t *readbytes);
  int BIO_write_ex(BIO *b, const void *data, size_t dlen, size_t *written);
  }

  //https://www.openssl.org/docs/man1.1.1/man3/BIO_ctrl.html
  BIO_ctrl: function(bp: PBIO; cmd: Integer; Larg: clong; PArg: Pointer): clong; cdecl;

  //Aliases functions

  function BIO_set_conn_hostname(b: PBIO; Name: PUTF8Char): clong; inline;
  function BIO_set_conn_port(b: PBIO; Port: PUTF8Char): clong; inline;
  function BIO_set_conn_address(b: PBIO; Address: PUTF8Char): clong; inline;
  function BIO_set_recv_timeout(b: PBIO; timeout: Integer): clong; inline;
  function BIO_set_send_timeout(b: PBIO; timeout: Integer): clong; inline;
  function BIO_get_ssl(b: PBIO; out ssl: PSSL): clong; inline;
  function BIO_do_connect(b: PBIO): clong; inline;
  function BIO_do_handshake(b: PBIO): clong; inline;
  function BIO_set_nbio(b: PBIO; n: Integer): clong; inline; //set blocking mode or not
  function BIO_should_retry(b: PBIO): Boolean; inline;

  function SSL_set_mode(ssl: PSSL; op: Integer): clong; inline;

  function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer;

  //tls1.h
  function SSL_set_tlsext_host_name(ssl: PSSL; Name: PUTF8Char): Integer;

  procedure X509V3_set_ctx_nodb(ctx: PX509V3_CTX); inline;

var
  OpenSSLLib: TmnOpenSSLLib = nil;
  CryptoLib: TmnCryptoLib = nil;

implementation

type
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

function BIO_set_conn_hostname(b: PBIO; Name: PUTF8Char): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_CONNECT, 0, Name);
end;

function BIO_set_conn_port(b: PBIO; Port: PUTF8Char): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_CONNECT, 1, Port);
end;

function BIO_set_recv_timeout(b: PBIO; timeout: Integer): clong;
var
  TimeVal: TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  Result := BIO_ctrl(b, BIO_CTRL_DGRAM_SET_RECV_TIMEOUT, 0, @TimeVal);
end;

function BIO_set_send_timeout(b: PBIO; timeout: Integer): clong;
var
  TimeVal: TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  Result := BIO_ctrl(b, BIO_CTRL_DGRAM_SET_SEND_TIMEOUT, 0, @TimeVal);
end;

function BIO_set_conn_address(b: PBIO; Address: PUTF8Char): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_CONNECT, 2, Address);
end;

function BIO_get_ssl(b: PBIO; out ssl: PSSL): clong;
begin
  Result := BIO_ctrl(b, BIO_C_GET_SSL, 0, @ssl);
end;

function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer;
begin
  Result := EVP_PKEY_assign(pkey, EVP_PKEY_RSA, key);
end;

function SSL_set_tlsext_host_name(ssl: PSSL; Name: PUTF8Char): Integer;
begin
  Result := SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, Name);
end;

procedure X509V3_set_ctx_nodb(ctx: PX509V3_CTX);
begin
  ctx^.db := nil;
end;

function BIO_do_handshake(b: PBIO): clong;
begin
  Result := BIO_ctrl(b, BIO_C_DO_STATE_MACHINE, 0, nil);
end;

function BIO_do_connect(b: PBIO): clong;
begin
  Result := BIO_do_handshake(b);
end;

function BIO_set_nbio(b: PBIO; n: Integer): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_NBIO, n, nil);
end;

function BIO_should_retry(b: PBIO): Boolean;
begin
  Result := BIO_test_flags(b, BIO_FLAGS_SHOULD_RETRY) > 0;
end;

function SSL_set_mode(ssl: PSSL; op: Integer): clong; inline;
begin
  Result := SSL_ctrl(ssl, SSL_CTRL_MODE, op, nil);
end;

{ TmnOpenSSLLib }

procedure TmnOpenSSLLib.Link;
begin
  RaiseError := True; //Raise error of one of this functions not exists
  OPENSSL_init_ssl := GetAddress('OPENSSL_init_ssl');
  //EVP_cleanup := GetAddress('EVP_cleanup');

  SSL_get_error := GetAddress('SSL_get_error');
  SSL_set_cipher_list := GetAddress('SSL_set_cipher_list');
  SSL_set_verify := GetAddress('SSL_set_verify');
  SSL_get_verify_result := GetAddress('SSL_get_verify_result');
  SSL_ctrl := GetAddress('SSL_ctrl');
  SSL_new := GetAddress('SSL_new');
  SSL_free := GetAddress('SSL_free');
  SSL_get_version := GetAddress('SSL_get_version');
  SSL_set_fd := GetAddress('SSL_set_fd');
  SSL_connect := GetAddress('SSL_connect');
  SSL_accept := GetAddress('SSL_accept');
  SSL_read := GetAddress('SSL_read');
  SSL_write := GetAddress('SSL_write');
  SSL_pending := GetAddress('SSL_pending');
  SSL_has_pending := GetAddress('SSL_has_pending');

  SSL_get_peer_certificate := GetAddress('SSL_get_peer_certificate');

  TLS_method := GetAddress('TLS_method');
  TLS_server_method := GetAddress('TLS_server_method');

  SSL_CTX_new := GetAddress('SSL_CTX_new');
  SSL_CTX_set_verify := GetAddress('SSL_CTX_set_verify');
  SSL_CTX_set_verify_depth := GetAddress('SSL_CTX_set_verify_depth');
  SSL_CTX_set_options := GetAddress('SSL_CTX_set_options');
  SSL_CTX_load_verify_locations := GetAddress('SSL_CTX_load_verify_locations');
  SSL_CTX_free := GetAddress('SSL_CTX_free');
  SSL_CTX_use_certificate_file := GetAddress('SSL_CTX_use_certificate_file');
  SSL_CTX_use_PrivateKey_file := GetAddress('SSL_CTX_use_PrivateKey_file');
  SSL_CTX_check_private_key := GetAddress('SSL_CTX_check_private_key');

  BIO_new_ssl_connect := GetAddress('BIO_new_ssl_connect');

  ERR_load_SSL_strings := GetAddress('ERR_load_SSL_strings');
end;

{ TCryptoLibLib }

procedure TmnCryptoLib.Link;
begin
  RaiseError := True; //Raise error of one of this functions not exists

  OPENSSL_init_crypto := GetAddress('OPENSSL_init_crypto');
  OPENSSL_config := GetAddress('OPENSSL_config');

  X509_new := GetAddress('X509_new');
  X509_free := GetAddress('X509_free');
  X509_verify_cert_error_string := GetAddress('X509_verify_cert_error_string');
  X509_REQ_new := GetAddress('X509_REQ_new');
  X509_sign := GetAddress('X509_sign');
  X509_REQ_set_version := GetAddress('X509_REQ_set_version');
  X509_REQ_get_subject_name := GetAddress('X509_REQ_get_subject_name');
  X509_REQ_set_pubkey := GetAddress('X509_REQ_set_pubkey');
  X509_REQ_sign := GetAddress('X509_REQ_sign');
  X509_NAME_add_entry_by_txt := GetAddress('X509_NAME_add_entry_by_txt');
  X509_REQ_free := GetAddress('X509_REQ_free');
  X509_add_ext := GetAddress('X509_add_ext');
  X509V3_set_ctx := GetAddress('X509V3_set_ctx');
  X509V3_EXT_conf_nid := GetAddress('X509V3_EXT_conf_nid');
  X509_set_version := GetAddress('X509_set_version');
  X509_EXTENSION_free := GetAddress('X509_EXTENSION_free');
  X509_set_issuer_name := GetAddress('X509_set_issuer_name');

  ASN1_INTEGER_set := GetAddress('ASN1_INTEGER_set');
  ASN1_INTEGER_set_int64 := GetAddress('ASN1_INTEGER_set_int64');
  X509_get_serialNumber := GetAddress('X509_get_serialNumber');
  X509_gmtime_adj := GetAddress('X509_gmtime_adj');
  X509_getm_notBefore := GetAddress('X509_getm_notBefore');
  X509_getm_notAfter := GetAddress('X509_getm_notAfter');
  X509_set_pubkey := GetAddress('X509_set_pubkey');
  X509_get_subject_name := GetAddress('X509_get_subject_name');

  X509_STORE_CTX_get_error_depth := GetAddress('X509_STORE_CTX_get_error_depth');
  PEM_read_bio_X509_REQ := GetAddress('PEM_read_bio_X509_REQ');
  PEM_write_bio_X509_REQ := GetAddress('PEM_write_bio_X509_REQ');
  PEM_write_bio_PrivateKey := GetAddress('PEM_write_bio_PrivateKey');
  PEM_write_bio_X509 := GetAddress('PEM_write_bio_X509');

  EVP_PKEY_new := GetAddress('EVP_PKEY_new');
  EVP_PKEY_assign := GetAddress('EVP_PKEY_assign');
  EVP_PKEY_get0_RSA := GetAddress('EVP_PKEY_get0_RSA');
  EVP_PKEY_get1_RSA := GetAddress('EVP_PKEY_get1_RSA');

  EVP_PKEY_free := GetAddress('EVP_PKEY_free');

  EVP_md_null := GetAddress('EVP_md_null');
  EVP_md5 := GetAddress('EVP_md5');
  EVP_sha1 := GetAddress('EVP_sha1');
  //EVP_mdc2 := GetAddress('EVP_mdc2');
  EVP_ripemd160 := GetAddress('EVP_ripemd160');
  EVP_blake2b512 := GetAddress('EVP_blake2b512');
  EVP_blake2s256 := GetAddress('EVP_blake2s256');

  EVP_sha224 := GetAddress('EVP_sha224');
  EVP_sha256 := GetAddress('EVP_sha256');
  EVP_sha384 := GetAddress('EVP_sha384');
  EVP_sha512 := GetAddress('EVP_sha512');

  BIO_ctrl := GetAddress('BIO_ctrl');
  BIO_new_fp := GetAddress('BIO_new_fp');
  BIO_new_fd := GetAddress('BIO_new_fd');
  BIO_new_file := GetAddress('BIO_new_file');
  BIO_test_flags := GetAddress('BIO_test_flags');
  BIO_free := GetAddress('BIO_free');
  BIO_free_all := GetAddress('BIO_free_all');
  BIO_write := GetAddress('BIO_write');
  BIO_read := GetAddress('BIO_read');
  BIO_puts := GetAddress('BIO_puts');
  BIO_gets := GetAddress('BIO_gets');

  BN_new := GetAddress('BN_new');
  BN_set_word := GetAddress('BN_set_word');
  BN_free := GetAddress('BN_free');

  RSA_new := GetAddress('RSA_new');
  RSA_generate_key_ex := GetAddress('RSA_generate_key_ex');
  RSA_print := GetAddress('RSA_print');
  RSA_print_fp := GetAddress('RSA_print_fp');

  CRYPTO_mem_ctrl := GetAddress('CRYPTO_mem_ctrl');

  ERR_get_error := GetAddress('ERR_get_error');
  ERR_error_string := GetAddress('ERR_error_string');
  ERR_load_CRYPTO_strings := GetAddress('ERR_load_CRYPTO_strings');
end;

initialization
  {$ifdef MSWINDOWS}
  {$ifdef win64}
  OpenSSLLib := TmnOpenSSLLib.Create('libssl-1_1-x64');
  CryptoLib := TmnCryptoLib.Create('libcrypto-1_1-x64');
  {$else}
  OpenSSLLib := TmnOpenSSLLib.Create('libssl-1_1');
  CryptoLib := TmnCryptoLib.Create('libcrypto-1_1');
  {$endif}
  {$else}
  OpenSSLLib := TmnOpenSSLLib.Create('libssl.so.1.1');
  CryptoLib := TmnCryptoLib.Create('libcrypto.so.1.1');
  {$endif}
finalization
  FreeAndNil(OpenSSLLib);
  FreeAndNil(CryptoLib);
end.

