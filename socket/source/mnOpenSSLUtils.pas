unit mnOpenSSLUtils;
{**
 *  This file is part of the "Mini Library"/Sockets
 *
 * @license   Mit
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

//I mixed of 2 of examples
//https://github.com/irtimmer/moonlight-embedded/blob/master/libgamestream/mkcert.c
//http://www.opensource.apple.com/source/OpenSSL/OpenSSL-22/openssl/demos/x509/mkcert.c

interface

uses
  Classes,
  SysUtils,
  mnSockets,
  mnOpenSSLAPI;

type
  EmnOpenSSLException = EmnSocketException;

function MakeCert(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;
function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

procedure InitOpenSSLLibrary(All: Boolean = True);
procedure CleanupOpenSSL;

procedure RaiseLastSSLError;
procedure RaiseSSLError(Message: utf8string);

implementation

procedure RaiseSSLError(Message: utf8string);
begin
  raise EmnOpenSSLException.Create(Message);
end;

procedure RaiseLastSSLError;
begin
  RaiseSSLError(ERR_error_string(ERR_get_error(), nil));
end;

procedure InitOpenSSLLibrary(All: Boolean);
begin
  if OpenSSLLib.Load then
  begin
    if All then
      OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS, nil)
    else
      OPENSSL_init_ssl(0, nil);
    //ERR_load_SSL_strings();//IDK

    if CryptoLib.Load then
    begin
      if All then
        OPENSSL_init_crypto(OPENSSL_INIT_ADD_ALL_CIPHERS or OPENSSL_INIT_ADD_ALL_DIGESTS, nil)
      else
        OPENSSL_init_crypto(0, nil);
    end;

    //ERR_load_CRYPTO_strings();//IDK
  end;
end;

procedure CleanupOpenSSL;
begin
  //EVP_Cleanup;
end;

function AddExt(cert: PX509; nid: integer; value: PUTF8Char): Integer;
var
  ex: PX509_EXTENSION;
  ctx: TX509V3_CTX;
begin
  // This sets the 'context' of the extensions.
  // No configuration database
  X509V3_set_ctx_nodb(@ctx);

  {
    Issuer and subject certs: both the target since it is self signed, no request and no CRL
  }

  X509V3_set_ctx(@ctx, cert, cert, nil, nil, 0);
  ex := X509V3_EXT_conf_nid(nil, @ctx, nid, value);

  if (ex = nil) then
  	exit(0);

  X509_add_ext(cert, ex, -1);
  X509_EXTENSION_free(ex);
  exit(1);
end;

//TODO need to make more clean when exit, some objects most not freed if assigned successed
function MakeCert(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
var
  x: PX509;
  pk: PEVP_PKEY;
  rsa: PRSA;
  name: PX509_NAME;
  bne: PBIGNUM;
  sign: PX509_sign;
  res: Integer;
begin
  x := nil;
  pk := nil;
  name := nil;
  bne := nil;
  sign := nil;
  Result := False;
  try
    InitOpenSSLLibrary;
  	if (pkeyp = nil) then
    begin
      pk := EVP_PKEY_new();
  		if (pk = nil) then
      begin
  			exit(False);
      end
    end
  	else
      pk := pkeyp;

  	if (x509p = nil) then
    begin
      x := X509_new();
  		if (x = nil) then
        exit(False);
    end
  	else
  		x := x509p;

    rsa := RSA_new();

    if (rsa = nil) then
      exit(False);

    bne := BN_new();
    if (bne = nil) then
      exit(False);

    BN_set_word(bne, RSA_F4);

    res := RSA_generate_key_ex(rsa, bits, bne, nil);

    if (res = 0) then
      exit(False);

    res := EVP_PKEY_assign_RSA(pk, rsa);
    if (res = 0) then
      exit(False);

    X509_set_version(x, 2);

    ASN1_INTEGER_set(X509_get_serialNumber(x), serial);
    X509_gmtime_adj(X509_getm_notBefore(x), 0);
    X509_gmtime_adj(X509_getm_notAfter(x), 60 * 60 * 24 * Days);

    X509_set_pubkey(x, pk);
    name := X509_get_subject_name(x);

    (* This function creates and adds the entry, working out the
     * correct utf8string type and performing checks on its length.
     *)
    if CN <> '' then
      X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC, PByte(CN), -1, -1, 0);
    if O <> '' then
      X509_NAME_add_entry_by_txt(name, 'O', MBSTRING_ASC, PByte(O), -1, -1, 0);
    if C <> '' then
   	  X509_NAME_add_entry_by_txt(name, 'C', MBSTRING_ASC, PByte(C), -1, -1, 0);
    if OU <> '' then
      X509_NAME_add_entry_by_txt(name, 'OU', MBSTRING_ASC, PByte(OU), -1, -1, 0);

    (* Its self signed so set the issuer name to be the same as the
     * subject.
     *)
    X509_set_issuer_name(x, name);

    (* Add various extensions: standard extensions *)

    AddExt(x, NID_basic_constraints, 'critical,CA:TRUE');
    //AddExt(x, NID_key_usage, PUTF8Char('critical,digitalSignature,keyEncipherment'));
    AddExt(x, NID_key_usage, PUTF8Char('critical,cRLSign,digitalSignature,keyCertSign')); //Self-Signed
    AddExt(x, NID_subject_key_identifier, 'hash');

    sign := X509_sign(x, pk, EVP_sha256());
    if (sign = nil) then
      exit(False);
    x509p := x;
    pkeyp := pk;
    exit(True);
  finally
    if not Result then
    begin
      if x <> nil then
      	X509_free(x);
      if pk <> nil then
      	EVP_PKEY_free(pk);
    end;
  end;
end;

function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
var
	x509: PX509;
	pkey: PEVP_PKEY;
  outbio: PBIO;
begin
	x509 :=nil;
	pkey := nil;
  try
    Result := MakeCert(x509, pkey, CN, O, C, OU, Bits, Serial, Days);
    outbio := BIO_new_file(PUTF8Char(PrivateKeyFile), 'w');
	  PEM_write_bio_PrivateKey(outbio, pkey, nil, nil, 0, nil, nil);
    BIO_free(outbio);

    outbio := BIO_new_file(PUTF8Char(CertificateFile), 'w');
	  PEM_write_bio_X509(outbio, x509);
    BIO_free(outbio);
  finally
    if x509 <> nil then
  	  X509_free(x509);
    if pkey <> nil then
  	  EVP_PKEY_free(pkey);
  end;
end;

end.
