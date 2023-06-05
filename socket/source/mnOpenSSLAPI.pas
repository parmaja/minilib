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
  {$i mnOpenSSLConsts.inc}

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

  PASN1_VALUE = Pointer;
  PPASN1_VALUE = ^PASN1_VALUE;

  PX509 = type PSLLObject;
  PX509_STORE_CTX = PSLLObject;
  PX509_REQ = type PSLLObject;
  PX509_CRL =PSLLObject;
  PX509V3_CONF_METHOD = PSLLObject;
  PPX509_REQ = ^PX509_REQ;
  PX509_NAME = PSLLObject;
  PX509_sign = PSLLObject;
  PX509_EXTENSION = PSLLObject;
  PLHASH = PSLLObject;

  PEC_KEY = PSLLObject;
  PPEC_KEY = ^PEC_KEY;
  PEC_KEY_METHOD = PSLLObject;
  PPEC_KEY_METHOD = ^PEC_KEY_METHOD;

  PBIO_METHOD = PSLLObject;
  PPBIO_METHOD = ^PBIO_METHOD;

  Pevp_md_ctx = PSLLObject;
  PPevp_md_ctx = ^Pevp_md_ctx;

  Ppem_password_cb = Pointer;

  PEVP_CIPHER = PSLLObject;
  PEVP_PKEY = PSLLObject;
  PEVP_MD = PSLLObject;

  PEC_GROUP = PSLLObject;
  PPEC_GROUP = ^PEC_GROUP;
  PEC_POINT = PSLLObject;
  PPEC_POINT = ^PEC_POINT;

  TSSLVerifyCallback = function(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;
  TCTXInfoCallback = procedure(ssl: PSSL; where: cint; ret: cint); cdecl;
  TCTXAlpnSelectCallback = function(ssl: PSSL; var outdata: PByte; var outlen: integer; const indata: PByte; inlen: Byte; arg: Pointer): Integer; cdecl;

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

  POPENSSL_STACK = PSLLObject;
  PPOPENSSL_STACK = ^POPENSSL_STACK;

  asn1_string_st = record
    length: Integer;
    &type: Integer;
    data: PByte;
    flags: Integer;
  end;

  ASN1_INTEGER = asn1_string_st;
  PASN1_INTEGER = ^ASN1_INTEGER;
  PPASN1_INTEGER = ^PASN1_INTEGER;
  ASN1_ENUMERATED = asn1_string_st;
  PASN1_ENUMERATED = ^ASN1_ENUMERATED;
  PPASN1_ENUMERATED = ^PASN1_ENUMERATED;
  ASN1_BIT_STRING = asn1_string_st;
  PASN1_BIT_STRING = ^ASN1_BIT_STRING;
  PPASN1_BIT_STRING = ^PASN1_BIT_STRING;
  ASN1_OCTET_STRING = asn1_string_st;
  PASN1_OCTET_STRING = ^ASN1_OCTET_STRING;
  PPASN1_OCTET_STRING = ^PASN1_OCTET_STRING;
  ASN1_PRINTABLESTRING = asn1_string_st;
  PASN1_PRINTABLESTRING = ^ASN1_PRINTABLESTRING;
  PPASN1_PRINTABLESTRING = ^PASN1_PRINTABLESTRING;
  ASN1_T61STRING = asn1_string_st;
  PASN1_T61STRING = ^ASN1_T61STRING;
  PPASN1_T61STRING = ^PASN1_T61STRING;
  ASN1_IA5STRING = asn1_string_st;
  PASN1_IA5STRING = ^ASN1_IA5STRING;
  PPASN1_IA5STRING = ^PASN1_IA5STRING;
  ASN1_GENERALSTRING = asn1_string_st;
  PASN1_GENERALSTRING = ^ASN1_GENERALSTRING;
  PPASN1_GENERALSTRING = ^PASN1_GENERALSTRING;
  ASN1_UNIVERSALSTRING = asn1_string_st;
  PASN1_UNIVERSALSTRING = ^ASN1_UNIVERSALSTRING;
  PPASN1_UNIVERSALSTRING = ^PASN1_UNIVERSALSTRING;
  ASN1_BMPSTRING = asn1_string_st;
  PASN1_BMPSTRING = ^ASN1_BMPSTRING;
  PPASN1_BMPSTRING = ^PASN1_BMPSTRING;
  ASN1_UTCTIME = asn1_string_st;
  PASN1_UTCTIME = ^ASN1_UTCTIME;
  PPASN1_UTCTIME = ^PASN1_UTCTIME;
  ASN1_TIME = asn1_string_st;
  PASN1_TIME = ^ASN1_TIME;
  PPASN1_TIME = ^PASN1_TIME;
  ASN1_GENERALIZEDTIME = asn1_string_st;
  PASN1_GENERALIZEDTIME = ^ASN1_GENERALIZEDTIME;
  PPASN1_GENERALIZEDTIME = ^PASN1_GENERALIZEDTIME;
  ASN1_VISIBLESTRING = asn1_string_st;
  PASN1_VISIBLESTRING = ^ASN1_VISIBLESTRING;
  PPASN1_VISIBLESTRING = ^PASN1_VISIBLESTRING;
  ASN1_UTF8STRING = asn1_string_st;
  PASN1_UTF8STRING = ^ASN1_UTF8STRING;
  PPASN1_UTF8STRING = ^PASN1_UTF8STRING;
  ASN1_STRING = asn1_string_st;
  PASN1_STRING = ^ASN1_STRING;
  PPASN1_STRING = ^PASN1_STRING;
  ASN1_BOOLEAN = Integer;
  ASN1_NULL = Integer;
  PASN1_NULL = ^ASN1_NULL;
  PPASN1_NULL = ^PASN1_NULL;
  PASN1_OBJECT = Pointer;
  PPASN1_OBJECT = ^PASN1_OBJECT;
  PASN1_PCTX = Pointer;
  PPASN1_PCTX = ^PASN1_PCTX;
  PASN1_SCTX = Pointer;
  PPASN1_SCTX = ^PASN1_SCTX;

  Pstack_st_X509_EXTENSION = PSLLObject;
  PPstack_st_X509_EXTENSION = ^Pstack_st_X509_EXTENSION;

  ASN1_ENCODING_st = record
    enc: PByte;
    len: Integer;
    modified: Integer;
  end;

  ASN1_ENCODING = ASN1_ENCODING_st;

  asn1_string_table_st = record
    nid: Integer;
    minsize: Integer;
    maxsize: Integer;
    mask: Cardinal;
    flags: Cardinal;
  end;

  ASN1_STRING_TABLE = asn1_string_table_st;
  PASN1_STRING_TABLE = ^ASN1_STRING_TABLE;
  PPASN1_STRING_TABLE = ^PASN1_STRING_TABLE;

  PASN1_TYPE = ^ASN1_TYPE;
  PPASN1_TYPE = ^PASN1_TYPE;

  otherName_st = record
    type_id: PASN1_OBJECT;
    value: PASN1_TYPE;
  end;

  OTHERNAME = otherName_st;
  POTHERNAME = ^OTHERNAME;
  PPOTHERNAME = ^POTHERNAME;

  _anonymous_type_1 = record
    case Integer of
      0: (ptr: PUTF8Char);
      1: (boolean: ASN1_BOOLEAN);
      2: (asn1_string: PASN1_STRING);
      3: (&object: PASN1_OBJECT);
      4: (&integer: PASN1_INTEGER);
      5: (enumerated: PASN1_ENUMERATED);
      6: (bit_string: PASN1_BIT_STRING);
      7: (octet_string: PASN1_OCTET_STRING);
      8: (printablestring: PASN1_PRINTABLESTRING);
      9: (t61string: PASN1_T61STRING);
      10: (ia5string: PASN1_IA5STRING);
      11: (generalstring: PASN1_GENERALSTRING);
      12: (bmpstring: PASN1_BMPSTRING);
      13: (universalstring: PASN1_UNIVERSALSTRING);
      14: (utctime: PASN1_UTCTIME);
      15: (generalizedtime: PASN1_GENERALIZEDTIME);
      16: (visiblestring: PASN1_VISIBLESTRING);
      17: (utf8string: PASN1_UTF8STRING);
      18: (&set: PASN1_STRING);
      19: (sequence: PASN1_STRING);
      20: (asn1_value: PASN1_VALUE);
  end;
  P_anonymous_type_1 = ^_anonymous_type_1;

  EDIPartyName_st = record
    nameAssigner: PASN1_STRING;
    partyName: PASN1_STRING;
  end;

  EDIPARTYNAME = EDIPartyName_st;
  PEDIPARTYNAME = ^EDIPARTYNAME;
  PPEDIPARTYNAME = ^PEDIPARTYNAME;

  _anonymous_type_5 = record
    case Integer of
      0: (ptr: PUTF8Char);
      1: (otherName: POTHERNAME);
      2: (rfc822Name: PASN1_IA5STRING);
      3: (dNSName: PASN1_IA5STRING);
      4: (x400Address: PASN1_TYPE);
      5: (directoryName: PX509_NAME);
      6: (ediPartyName: PEDIPARTYNAME);
      7: (uniformResourceIdentifier: PASN1_IA5STRING);
      8: (iPAddress: PASN1_OCTET_STRING);
      9: (registeredID: PASN1_OBJECT);
      10: (ip: PASN1_OCTET_STRING);
      11: (dirn: PX509_NAME);
      12: (ia5: PASN1_IA5STRING);
      13: (rid: PASN1_OBJECT);
      14: (other: PASN1_TYPE);
  end;
  P_anonymous_type_5 = ^_anonymous_type_5;


  buf_mem_st = record
    length: NativeUInt;
    data: PUTF8Char;
    max: NativeUInt;
    flags: Cardinal;
  end;
  BUF_MEM = buf_mem_st;
  PBUF_MEM = ^BUF_MEM;

  asn1_type_st = record
    &type: Integer;
    value: _anonymous_type_1;
  end;

  ASN1_TYPE = asn1_type_st;

  GENERAL_NAME_st = record
    &type: Integer;
    d: _anonymous_type_5;
  end;

  GENERAL_NAME = GENERAL_NAME_st;
  PGENERAL_NAME = ^GENERAL_NAME;
  PPGENERAL_NAME = ^PGENERAL_NAME;

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
  SSL_shutdown: function(ssl: PSSL): Integer; cdecl;
  SSL_set_shutdown: procedure(ssl: PSSL; mode: Integer); cdecl;
  SSL_clear: function(ssl: PSSL): Integer; cdecl;
  SSL_free: procedure(ssl: PSSL); cdecl;
  SSL_get_version: function(const ssl: PSSL): PUTF8Char; cdecl;
  SSL_set_fd: function(ssl: PSSL; d: Integer): integer; cdecl;
  SSL_connect: function(ssl: PSSL): Integer; cdecl;
  SSL_accept: function(ssl: PSSL): Integer; cdecl;
  SSL_set_bio: procedure(ssl: PSSL; rbio: PBIO; wbio: PBIO); cdecl;

  SSL_read: function(ssl: PSSL; var buf; size: integer): integer; cdecl;
  SSL_write: function(ssl: PSSL; const buf; size: integer): integer; cdecl;

  SSL_pending: function(ssl: PSSL): Integer; cdecl;
  SSL_has_pending: function(ssl: PSSL): Integer; cdecl;

  SSL_state_string: function(ssl: PSSL): PUTF8Char; cdecl;
  SSL_state_string_long: function(ssl: PSSL): PUTF8Char; cdecl;
  SSL_alert_type_string_long: function(val: integer): PUTF8Char; cdecl;
  SSL_alert_desc_string_long: function(val: integer): PUTF8Char; cdecl;
  SSL_select_next_proto: function(var outdata: PUTF8Char; var outlen: Integer; server: PUTF8Char; serverlen: Integer; client: PUTF8Char; clientlen: Integer): Integer; cdecl;
  SSL_get0_alpn_selected: procedure (ssl: PSSL; var outdata: PUTF8Char; var len: Integer); cdecl;

  SSL_CTX_new: function(Method: PSSL_METHOD): PSSL_CTX; cdecl;
  SSL_CTX_set_verify: procedure(ctx: PSSL_CTX; Mode: Integer; Callback: TSSLVerifyCallback); cdecl;
  SSL_CTX_set_verify_depth: procedure(ctx: PSSL_CTX; Depth: integer); cdecl;
  SSL_CTX_set_options: function(ctx: PSSL_CTX; Options: culong): culong; cdecl;
  SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; CAfile: PUTF8Char; CApath: PUTF8Char): Integer; cdecl;
  SSL_CTX_free: procedure(ctx: PSSL_CTX); cdecl;
  SSL_CTX_use_certificate_file: function(ctx: PSSL_CTX; afile: PUTF8Char; atype: Integer): Integer; cdecl;
  SSL_CTX_use_PrivateKey_file: function(ctx: PSSL_CTX; const afile: PUTF8Char; atype: Integer): Integer; cdecl;
  SSL_CTX_check_private_key: function(ctx: PSSL_CTX): Integer; cdecl;
  SSL_CTX_use_RSAPrivateKey_file: function(ctx: PSSL_CTX; const afile: PUTF8Char; atype: Integer): Integer; cdecl;
  SSL_CTX_ctrl: function(ctx: PSSL_CTX; cmd: Integer; Larg: clong; PArg: Pointer): clong; cdecl;
  SSL_CTX_set_info_callback: procedure(ctx: PSSL_CTX; Callback: TCTXInfoCallback); cdecl;
  SSL_CTX_set_alpn_select_cb: function(ctx: PSSL_CTX; Callback: TCTXAlpnSelectCallback; args: Pointer): Integer; cdecl;
  SSL_CTX_set_alpn_protos: function(ctx: PSSL_CTX; prots: PUTF8Char; len: integer): Integer; cdecl;

  TLS_method: function(): PSSL_METHOD; cdecl;
  TLS_client_method: function(): PSSL_METHOD; cdecl;
  TLS_server_method: function(): PSSL_METHOD; cdecl;

  RAND_bytes: function(buf: PByte; num: Integer): Integer; cdecl;

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
  X509_to_X509_REQ: function(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): PX509_REQ; cdecl;


  X509V3_set_ctx: procedure(ctx: PX509V3_CTX; issuer: PX509; subject: PX509; req: PX509_REQ; crl: PX509_CRL; flags: integer); cdecl;

  X509V3_EXT_conf_nid: function(conf: PLHASH; ctx: PX509V3_CTX; ext_nid: integer; value: PUTF8Char): PX509_EXTENSION; cdecl;
  X509_set_version: function(x: PX509; version: clong): Integer; cdecl;

  X509_REQ_set_version: function(x: PX509_REQ; version: clong): Integer; cdecl;

  PEM_read_bio_X509_REQ: function(bp: PBIO; x: PPX509_REQ; cb: Ppem_password_cb; var u): PX509_REQ; cdecl;
  PEM_write_bio_X509_REQ: function(bp: PBIO; x: PX509_REQ): Integer; cdecl;
  PEM_write_bio_PrivateKey: function(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: Ppem_password_cb; u: Pointer): integer; cdecl;
  PEM_write_bio_X509: function(bp: PBIO; x: PX509): Integer; cdecl;
  PEM_write_X509: function(fh: NativeInt; x: PX509): Integer; cdecl;
  PEM_write_bio_RSAPublicKey: function(bp: PBIO; x: PRSA): Integer; cdecl;

  PEM_read_bio_X509: function(bp: PBIO; x: PX509; cb: Ppem_password_cb; u: Pointer): PX509; cdecl;
  PEM_read_bio_PrivateKey: function(bp: PBIO; x: PEVP_PKEY; cb: Ppem_password_cb; u: Pointer): PEVP_PKEY; cdecl;

  ASN1_INTEGER_set_int64: function(a: PASN1_INTEGER; r: Int64): Integer; cdecl;
  ASN1_INTEGER_set: function(const a: PASN1_INTEGER; v: Integer): Integer; cdecl;

  X509_get_serialNumber: function(x: PX509): PASN1_INTEGER; cdecl;
  X509_gmtime_adj: function(s: PASN1_TIME; adj: clong): PASN1_TIME; cdecl;
  X509_getm_notBefore: function(x: PX509): PASN1_TIME; cdecl;
  X509_getm_notAfter: function(x: PX509): PASN1_TIME; cdecl;
  X509_set_pubkey: function(x: PX509; pkey: PEVP_PKEY): Integer; cdecl;
  X509_get_subject_name: function(x: PX509): PX509_NAME; cdecl;
  X509_add1_ext_i2d: function(x: PX509; nid: Integer; value: Pointer; crit: Integer; flags: Cardinal): Integer; cdecl;

  X509_set_subject_name: function(x: PX509; name: PX509_NAME): Integer; cdecl;
  X509_REQ_set_subject_name: function(req: PX509_REQ; name: PX509_NAME): Integer; cdecl;

  X509_REQ_get_pubkey: function(req: PX509_REQ): PEVP_PKEY; cdecl;
  X509_REQ_add1_attr_by_txt: function(req: PX509_REQ; attrname: PByte; &type: Integer; bytes: PByte; len: Integer): Integer; cdecl;

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
  EVP_DigestUpdate: function(ctx: PEVP_MD_CTX; d: Pointer; cnt: NativeUInt): Integer; cdecl;

  BN_new: function(): PBIGNUM; cdecl;
  BN_set_word: function(a: PBIGNUM; w: BN_ULONG): integer; cdecl;
  BN_free: procedure(a: PBIGNUM); cdecl;

  RSA_new: function(): PRSA; cdecl;
  RSA_generate_key_ex: function(rsa: PRSA; bits: integer; e: PBIGNUM; cb: PBN_GENCB): Integer; cdecl;
  RSA_print: function(bp: PBIO; x: PRSA; offset: integer): Integer; cdecl;
  RSA_print_fp: function(fp: Pointer; x: PRSA; offset: integer): Integer; cdecl;
  RSA_free: procedure(r: PRSA); cdecl;

  CRYPTO_mem_ctrl: function(mode: integer): integer; cdecl;

  BIO_new: function(typ: PBIO_METHOD): PBIO; cdecl;
  BIO_new_ssl_connect: function(ctx: PSSL_CTX): PBIO; cdecl;
  BIO_new_fp: function(handle: THandle; close_flag: Integer): PBIO; cdecl; //dosnt work
  BIO_new_fd: function(handle: THandle; close_flag: Integer): PBIO; cdecl; //idk
  BIO_new_file: function(filename: PUTF8Char; Mode: PUTF8Char): PBIO; cdecl;
  BIO_new_mem_buf: function(buf: PByte; len: Integer): PBIO; cdecl;
  BIO_f_base64: function(): PBIO_METHOD; cdecl;

  BIO_s_mem: function(): PBIO_METHOD; cdecl;
  HMAC: function(evp_md: PEVP_MD; key: Pointer; key_len: Integer; d: PByte; n: NativeUInt; md: PByte; var md_len: Cardinal): PByte; cdecl;

  BIO_read: function(b: PBIO; var data; dlen: integer): Integer; cdecl;
  BIO_write: function(b: PBIO; const data; dlen: Integer): Integer; cdecl;
  BIO_gets: function(b: PBIO; buf: PByte; Size: Integer): Integer; cdecl;
  BIO_puts: function(bio: PBIO; buf: PUTF8Char): Integer; cdecl;
  BIO_push: function(b: PBIO; append: PBIO): PBIO; cdecl;
  BIO_set_flags: function(b: PBIO; flags: Integer): integer; cdecl;
  BIO_test_flags: function(b: PBIO; flags: Integer): integer; cdecl;
  BIO_free_all: procedure(b: PBIO); cdecl;
  BIO_free: function(bio: PBIO): Integer; cdecl;

  {TODO
  int BIO_read_ex(BIO *b, void *data, size_t dlen, size_t *readbytes);
  int BIO_write_ex(BIO *b, const void *data, size_t dlen, size_t *written);
  }

  //https://www.openssl.org/docs/man1.1.1/man3/BIO_ctrl.html
  BIO_ctrl: function(bp: PBIO; cmd: Integer; Larg: clong; PArg: Pointer): clong; cdecl;


  OPENSSL_sk_new_null: function(): POPENSSL_STACK; cdecl;
  OPENSSL_sk_push: procedure(sk: POPENSSL_STACK; vData: Pointer); cdecl;
  OPENSSL_sk_free: procedure(sk: POPENSSL_STACK); cdecl;

  ASN1_STRING_set: function(str: PASN1_STRING; data: Pointer; len: Integer): Integer; cdecl;
  ASN1_STRING_new: function(): PASN1_STRING; cdecl;
  ASN1_OCTET_STRING_new: function():PASN1_OCTET_STRING; cdecl;
  ASN1_OCTET_STRING_set: function(str: PASN1_OCTET_STRING; data: PByte; len: Integer): Integer; cdecl;

  GENERAL_NAME_new: function(): PGENERAL_NAME; cdecl;
  GENERAL_NAME_set0_value: procedure(a: PGENERAL_NAME; typ: Integer; value: Pointer); cdecl;

  X509_NAME_add_entry_by_NID: function(name: PX509_NAME; nid: Integer; typ: Integer; bytes: PByte; len: Integer; loc: Integer; &set: Integer): Integer; cdecl;
  X509_REQ_add_extensions: function(req: PX509_REQ; sk: Pstack_st_X509_EXTENSION): Integer; cdecl;
  X509_REQ_add_extensions_nid: function(req: PX509_REQ; sk: Pstack_st_X509_EXTENSION; nid: Integer): Integer; cdecl;
  X509V3_add1_i2d: function(var sk: POPENSSL_STACK; nid: Integer; value: Pointer; crit: Integer; flags: Cardinal): Integer; cdecl;
  OBJ_create: function(name: PUTF8Char; sn: PUTF8Char; ln: PUTF8Char): Integer; cdecl;
  OBJ_txt2nid: function(s: PUTF8Char): Integer; cdecl;

  EC_KEY_new:  function(): PEC_KEY; cdecl;
  EC_KEY_get_flags: function(key: PEC_KEY): Integer; cdecl;
  EC_KEY_set_flags: procedure(key: PEC_KEY; flags: Integer); cdecl;
  EC_KEY_clear_flags: procedure(key: PEC_KEY; flags: Integer); cdecl;
  EC_KEY_new_by_curve_name: function(nid: Integer): PEC_KEY; cdecl;
  EC_KEY_free: procedure(key: PEC_KEY); cdecl;

  EC_GROUP_new_by_curve_name: function(nid: Integer): PEC_GROUP; cdecl;
  EC_KEY_set_group: function(key: PEC_KEY; group: PEC_GROUP): Integer; cdecl;
  EC_KEY_generate_key: function(key: PEC_KEY): Integer; cdecl;

  PEM_read_bio_ECPrivateKey: function(bp: PBIO; x: PPEC_KEY; cb: Ppem_password_cb; u: Pointer): PEC_KEY; cdecl;
  ECDSA_size: function(eckey: PEC_KEY): Integer; cdecl;
  ECDSA_sign: function(&type: Integer; dgst: PByte; dgstlen: Integer; sig: PByte; siglen: PCardinal; eckey: PEC_KEY): Integer; cdecl;


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
  function BIO_get_mem_ptr(b: PBIO; var pp: PBUF_MEM): clong; inline;
  function BIO_flush(b: PBIO): clong; inline;
  function BIO_set_close(b: PBIO; c: clong): clong; inline;

  function SSL_set_mode(ssl: PSSL; op: Integer): clong; inline;

  function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer;
  function EVP_PKEY_assign_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer;


  //tls1.h
  function SSL_set_tlsext_host_name(ssl: PSSL; Name: PUTF8Char): Integer;

  procedure X509V3_set_ctx_nodb(ctx: PX509V3_CTX); inline;
  function BIO_get_mem_data(b : PBIO; var pp : PByte) : NativeInt; inline;

  function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: integer): integer;
  function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: integer): integer;
  procedure OpenSSL_add_all_algorithms;
const
  TLS1_VERSION    = $0301;
  TLS1_1_VERSION  = $0302;
  TLS1_2_VERSION  = $0303;
  TLS1_3_VERSION  = $0304;
  TLS_MAX_VERSION = TLS1_3_VERSION;
  TLS_ANY_VERSION = $10000;


var
  OpenSSLLib: TmnOpenSSLLib = nil;
  CryptoLib: TmnCryptoLib = nil;

implementation

type
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

procedure OpenSSL_add_all_algorithms;
begin
  OPENSSL_init_crypto(OPENSSL_INIT_ADD_ALL_CIPHERS or OPENSSL_INIT_ADD_ALL_DIGESTS or OPENSSL_INIT_LOAD_CONFIG, nil);
end;

function BIO_set_conn_hostname(b: PBIO; Name: PUTF8Char): clong;
begin
  Result := BIO_ctrl(b, BIO_C_SET_CONNECT, 0, Name);
end;

function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, version, nil);
end;

function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MAX_PROTO_VERSION, version, nil);
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

function BIO_get_mem_ptr(b: PBIO; var pp: PBUF_MEM) : clong;
begin
  Result := BIO_ctrl(b, BIO_C_GET_BUF_MEM_PTR, 0, @pp);
end;

function BIO_flush(b: PBIO): clong; inline;
begin
  Result := BIO_ctrl(b, BIO_CTRL_FLUSH, 0, nil);
end;

function BIO_set_close(b: PBIO; c: clong): clong; inline;
begin
  Result := BIO_ctrl(b, BIO_CTRL_SET_CLOSE, c, nil);
end;

function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer;
begin
  Result := EVP_PKEY_assign(pkey, EVP_PKEY_RSA, key);
end;

function EVP_PKEY_assign_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer;
begin
  Result := EVP_PKEY_assign(pkey, EVP_PKEY_EC, key);
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
  SSL_shutdown := GetAddress('SSL_shutdown');
  SSL_set_shutdown := GetAddress('SSL_set_shutdown');
  SSL_clear := GetAddress('SSL_clear');
  SSL_free := GetAddress('SSL_free');
  SSL_get_version := GetAddress('SSL_get_version');
  SSL_set_fd := GetAddress('SSL_set_fd');
  SSL_connect := GetAddress('SSL_connect');
  SSL_accept := GetAddress('SSL_accept');
  SSL_read := GetAddress('SSL_read');
  SSL_write := GetAddress('SSL_write');
  SSL_pending := GetAddress('SSL_pending');
  SSL_has_pending := GetAddress('SSL_has_pending');
  SSL_set_bio := GetAddress('SSL_set_bio');

  SSL_state_string := GetAddress('SSL_state_string');
  SSL_state_string_long := GetAddress('SSL_state_string_long');
  SSL_alert_type_string_long := GetAddress('SSL_alert_type_string_long');
  SSL_alert_desc_string_long := GetAddress('SSL_alert_desc_string_long');
  SSL_select_next_proto := GetAddress('SSL_select_next_proto');
  SSL_get0_alpn_selected := GetAddress('SSL_get0_alpn_selected');

  SSL_get_peer_certificate := GetAddress('SSL_get_peer_certificate');

  TLS_method := GetAddress('TLS_method');
  TLS_client_method := GetAddress('TLS_client_method');
  TLS_server_method := GetAddress('TLS_server_method');

  SSL_CTX_new := GetAddress('SSL_CTX_new');
  SSL_CTX_set_verify := GetAddress('SSL_CTX_set_verify');
  SSL_CTX_set_verify_depth := GetAddress('SSL_CTX_set_verify_depth');
  SSL_CTX_set_options := GetAddress('SSL_CTX_set_options');
  SSL_CTX_load_verify_locations := GetAddress('SSL_CTX_load_verify_locations');
  SSL_CTX_free := GetAddress('SSL_CTX_free');
  SSL_CTX_use_certificate_file := GetAddress('SSL_CTX_use_certificate_file');
  SSL_CTX_use_PrivateKey_file := GetAddress('SSL_CTX_use_PrivateKey_file');
  SSL_CTX_use_RSAPrivateKey_file := GetAddress('SSL_CTX_use_RSAPrivateKey_file');
  SSL_CTX_check_private_key := GetAddress('SSL_CTX_check_private_key');
  SSL_CTX_ctrl := GetAddress('SSL_CTX_ctrl');
  SSL_CTX_set_info_callback := GetAddress('SSL_CTX_set_info_callback');
  SSL_CTX_set_alpn_select_cb := GetAddress('SSL_CTX_set_alpn_select_cb');
  SSL_CTX_set_alpn_protos := GetAddress('SSL_CTX_set_alpn_protos');


  BIO_new_ssl_connect := GetAddress('BIO_new_ssl_connect');

  ERR_load_SSL_strings := GetAddress('ERR_load_SSL_strings');
end;

{ TCryptoLibLib }

procedure TmnCryptoLib.Link;
begin
  RaiseError := True; //Raise error of one of this functions not exists

  OPENSSL_init_crypto := GetAddress('OPENSSL_init_crypto');
  OPENSSL_config := GetAddress('OPENSSL_config');

  RAND_bytes := GetAddress('RAND_bytes');

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
  X509_to_X509_REQ := GetAddress('X509_to_X509_REQ');

  ASN1_INTEGER_set := GetAddress('ASN1_INTEGER_set');
  ASN1_INTEGER_set_int64 := GetAddress('ASN1_INTEGER_set_int64');
  X509_get_serialNumber := GetAddress('X509_get_serialNumber');
  X509_gmtime_adj := GetAddress('X509_gmtime_adj');
  X509_getm_notBefore := GetAddress('X509_getm_notBefore');
  X509_getm_notAfter := GetAddress('X509_getm_notAfter');
  X509_set_pubkey := GetAddress('X509_set_pubkey');
  X509_get_subject_name := GetAddress('X509_get_subject_name');
  X509_add1_ext_i2d := GetAddress('X509_add1_ext_i2d');


  X509_set_subject_name := GetAddress('X509_set_subject_name');
  X509_REQ_set_subject_name := GetAddress('X509_REQ_set_subject_name');
  X509_REQ_get_pubkey := GetAddress('X509_REQ_get_pubkey');
  X509_REQ_add1_attr_by_txt := GetAddress('X509_REQ_add1_attr_by_txt');

  X509_STORE_CTX_get_error_depth := GetAddress('X509_STORE_CTX_get_error_depth');
  PEM_read_bio_X509_REQ := GetAddress('PEM_read_bio_X509_REQ');
  PEM_write_bio_X509_REQ := GetAddress('PEM_write_bio_X509_REQ');
  PEM_write_bio_PrivateKey := GetAddress('PEM_write_bio_PrivateKey');
  PEM_write_bio_X509 := GetAddress('PEM_write_bio_X509');
  PEM_write_X509 := GetAddress('PEM_write_X509');
  PEM_write_bio_RSAPublicKey := GetAddress('PEM_write_bio_RSAPublicKey');
  PEM_read_bio_X509 := GetAddress('PEM_read_bio_X509');
  PEM_read_bio_PrivateKey := GetAddress('PEM_read_bio_PrivateKey');

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
  EVP_DigestUpdate := GetAddress('EVP_DigestUpdate');

  BIO_ctrl := GetAddress('BIO_ctrl');
  BIO_new := GetAddress('BIO_new');
  BIO_new_fp := GetAddress('BIO_new_fp');
  BIO_new_fd := GetAddress('BIO_new_fd');
  BIO_new_file := GetAddress('BIO_new_file');
  BIO_new_mem_buf := GetAddress('BIO_new_mem_buf');
  BIO_f_base64 := GetAddress('BIO_f_base64');
  BIO_set_flags := GetAddress('BIO_set_flags');
  BIO_test_flags := GetAddress('BIO_test_flags');
  BIO_free := GetAddress('BIO_free');
  BIO_free_all := GetAddress('BIO_free_all');
  BIO_write := GetAddress('BIO_write');
  BIO_read := GetAddress('BIO_read');
  BIO_puts := GetAddress('BIO_puts');
  BIO_push := GetAddress('BIO_push');
  BIO_gets := GetAddress('BIO_gets');

  OPENSSL_sk_new_null := GetAddress('OPENSSL_sk_new_null');
  OPENSSL_sk_push := GetAddress('OPENSSL_sk_push');
  OPENSSL_sk_free := GetAddress('OPENSSL_sk_free');

  ASN1_STRING_new := GetAddress('ASN1_STRING_new');
  ASN1_STRING_set := GetAddress('ASN1_STRING_set');
  GENERAL_NAME_new := GetAddress('GENERAL_NAME_new');
  GENERAL_NAME_set0_value := GetAddress('GENERAL_NAME_set0_value');
  ASN1_OCTET_STRING_new := GetAddress('ASN1_OCTET_STRING_new');
  ASN1_OCTET_STRING_set := GetAddress('ASN1_OCTET_STRING_set');


  BIO_s_mem := GetAddress('BIO_s_mem');
  HMAC := GetAddress('HMAC');

  BN_new := GetAddress('BN_new');
  BN_set_word := GetAddress('BN_set_word');
  BN_free := GetAddress('BN_free');

  RSA_new := GetAddress('RSA_new');
  RSA_generate_key_ex := GetAddress('RSA_generate_key_ex');
  RSA_print := GetAddress('RSA_print');
  RSA_print_fp := GetAddress('RSA_print_fp');
  RSA_free := GetAddress('RSA_free');

  CRYPTO_mem_ctrl := GetAddress('CRYPTO_mem_ctrl');

  ERR_get_error := GetAddress('ERR_get_error');
  ERR_error_string := GetAddress('ERR_error_string');
  ERR_load_CRYPTO_strings := GetAddress('ERR_load_CRYPTO_strings');

  X509_NAME_add_entry_by_NID := GetAddress('X509_NAME_add_entry_by_NID');
  X509_REQ_add_extensions := GetAddress('X509_REQ_add_extensions');
  X509_REQ_add_extensions_nid := GetAddress('X509_REQ_add_extensions_nid');
  X509V3_add1_i2d := GetAddress('X509V3_add1_i2d');

  OBJ_create  := GetAddress('OBJ_create');
  OBJ_txt2nid := GetAddress('OBJ_txt2nid');

  EC_KEY_new := GetAddress('EC_KEY_new');
  EC_KEY_get_flags := GetAddress('EC_KEY_get_flags');
  EC_KEY_set_flags := GetAddress('EC_KEY_set_flags');
  EC_KEY_clear_flags := GetAddress('EC_KEY_clear_flags');
  EC_KEY_new_by_curve_name := GetAddress('EC_KEY_new_by_curve_name');
  EC_KEY_free := GetAddress('EC_KEY_free');

  EC_GROUP_new_by_curve_name := GetAddress('EC_GROUP_new_by_curve_name');
  EC_KEY_set_group := GetAddress('EC_KEY_set_group');
  EC_KEY_generate_key := GetAddress('EC_KEY_generate_key');

  PEM_read_bio_ECPrivateKey := GetAddress('PEM_read_bio_ECPrivateKey');
  ECDSA_size := GetAddress('ECDSA_size');
  ECDSA_sign := GetAddress('ECDSA_sign');
end;

function BIO_get_mem_data(b : PBIO; var pp : PByte) : NativeInt;
begin
  Result := BIO_ctrl(b, BIO_CTRL_INFO, 0, @pp);
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
  {$ifdef macos}
  OpenSSLLib := TmnOpenSSLLib.Create('libssl.1.1');
  CryptoLib := TmnCryptoLib.Create('libcrypto.1.1');
  {$else}
  OpenSSLLib := TmnOpenSSLLib.Create('libssl.so.1.1');
  CryptoLib := TmnCryptoLib.Create('libcrypto.so.1.1');
  {$endif}
  {$endif}
finalization
  FreeAndNil(OpenSSLLib);
  FreeAndNil(CryptoLib);
end.



