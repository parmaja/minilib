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
//https://stackoverflow.com/questions/256405/programmatically-create-x509-certificate-using-openssl
//https://cpp.hotexamples.com/site/file?hash=0x2e00cb0b64f97a0732527bb45deb58b695a7ca4910566693882c7e54c90bcf57&fullName=Webinos-Platform-master/webinos/core/manager/certificate_manager/openssl_wrapper.cpp&project=AlexWei2013/Webinos-Platform

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  mnSockets,
  mnOpenSSL,
  mnOpenSSLAPI;

type

  TsslConfig = class(TMemIniFile)
  public
    constructor Create;
    procedure UpdateFile; override;
  end;

  TPX509Helper = record helper for PX509
    procedure SetSerial(vSerial: Integer);
    procedure AdjTime(vDays: NativeInt); overload;
    procedure AdjTime(vFrom, vTo: NativeInt); overload;

    procedure SetSubjectName(const vField, vData: string);
    procedure SetExt(NID: Integer; const vData: string);
  end;


function MakeCert(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;
function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

function MakeX509(vConfig: TsslConfig): PX509;
function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;

implementation

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
//  name := nil;
//  bne := nil;
//  sign := nil;
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

function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
var
  rsa: PRSA;
  bn: PBIGNUM;
  sign: PX509_sign;
  bits: Integer;
begin
  bits := vConfig.ReadInteger('req', 'default_bits', 2048);

  Result := EVP_PKEY_new();
  try
    rsa := RSA_new();
    bn := BN_new();
    try
      BN_set_word(bn, RSA_F4);

      if RSA_generate_key_ex(rsa, bits, bn, nil)=0 then raise Exception.Create('Error RSA_generate_key_ex');
      if EVP_PKEY_assign_RSA(Result, rsa)=0 then raise Exception.Create('Error EVP_PKEY_assign_RSA');

      X509_set_pubkey(X509, Result);

      if X509_sign(X509, Result, EVP_sha256())=nil then raise Exception.Create('Error X509_sign');
    finally
      //RSA_free(rsa);
      //BN_free(bn);
    end;
  except
    EVP_PKEY_free(Result);
    Result := nil;
  end;
end;

function MakeX509(vConfig: TsslConfig): PX509;
var
  n: string;
begin
  InitOpenSSLLibrary;

  Result := X509_new();
  try
    X509_set_version(Result, 2);
    Result.SetSerial(vConfig.ReadInteger('req', 'Serial', 0));
    Result.AdjTime(vConfig.ReadInteger('req', 'Days', 1));

    n := vConfig.ReadString('req', 'distinguished_name', '');
    if n<>'' then
    begin
      Result.SetSubjectName('CN', vConfig.ReadString(n, 'CN', ''));
      Result.SetSubjectName('O', vConfig.ReadString(n, 'O', ''));
      Result.SetSubjectName('C', vConfig.ReadString(n, 'C', ''));
      Result.SetSubjectName('OU', vConfig.ReadString(n, 'OU', ''));

      (* Its self signed so set the issuer name to be the same as the
       * subject.
       *)
      //X509_set_issuer_name(x, name);
      X509_set_issuer_name(Result, X509_get_subject_name(Result));
    end;

    n := vConfig.ReadString('req', 'req_extensions', '');
    if n<>'' then
    begin
      Result.SetExt(NID_basic_constraints, vConfig.ReadString(n, SN_basic_constraints, ''));
      Result.SetExt(NID_key_usage, vConfig.ReadString(n, SN_key_usage, ''));
      Result.SetExt(NID_subject_key_identifier, 'hash');
    end;

  except
    FreeAndNil(Result);
  end;
end;

function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;
var
  px: PX509;
  pk: PEVP_PKEY;
  rsa: PRSA;

  outbio: PBIO;

  cn, rn, pn, vn: PUTF8Char;
  b: PByte;
begin
  px := MakeX509(vConfig);
  if px<>nil then
  begin
    pk := SignX509(px, vConfig);
    if pk<>nil then
    begin
      cn := PUTF8Char(UTF8Encode(vName+'.cer'));
      rn := PUTF8Char(UTF8Encode(vName+'.csr'));
      pn := PUTF8Char(UTF8Encode(vName+'.private.pem'));
      vn := PUTF8Char(UTF8Encode(vName+'.public.pem'));

      outbio := BIO_new(BIO_s_mem());
      PEM_write_bio_PrivateKey(outbio, pk, nil, nil, 0, nil, nil);

      var aLen := BIO_get_mem_data(outbio, b);
      //var ss := TEncoding.ANSI.GetString(b);

      BIO_free(outbio);

      outbio := BIO_new_file(vn, 'w');
      PEM_write_bio_PrivateKey(outbio, pk, nil, nil, 0, nil, nil);

      BIO_free(outbio);

      rsa := EVP_PKEY_get1_RSA(pk);
      outbio := BIO_new_file(pn, 'w');
      PEM_write_bio_RSAPublicKey(outbio, rsa);
      BIO_free(outbio);

      outbio := BIO_new_file(cn, 'w');
	    PEM_write_bio_X509(outbio, px);
      BIO_free(outbio);

      var xx := X509_to_X509_REQ(px, pk, EVP_sha256);
      outbio := BIO_new_file(rn, 'w');
      PEM_write_bio_X509_REQ(outbio, xx);
      BIO_free(outbio);
      X509_REQ_free(xx);

  	  EVP_PKEY_free(pk);

      Exit(True);
    end;

    X509_free(px);
  end;
  Result := False;
end;

function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
var
	x509: PX509;
	pkey: PEVP_PKEY;
  outbio: PBIO;
  s: utf8string;
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


    s := ChangeFileExt(CertificateFile, '.csr');
    var xx := X509_to_X509_REQ(x509, pkey, EVP_sha256);
    outbio := BIO_new_file(PUTF8Char(s), 'w');
	  PEM_write_bio_X509_REQ(outbio, xx);
    BIO_free(outbio);
    X509_REQ_free(xx);

  finally
    if x509 <> nil then
  	  X509_free(x509);
    if pkey <> nil then
  	  EVP_PKEY_free(pkey);
  end;
end;

{ TsslConfig }

constructor TsslConfig.Create;
begin
  inherited Create('');
end;

procedure TsslConfig.UpdateFile;
begin
  //inherited; do nothing
end;

{ TPX509Helper }

procedure TPX509Helper.AdjTime(vFrom, vTo: NativeInt);
begin
  X509_gmtime_adj(X509_getm_notBefore(Self), 0);
  X509_gmtime_adj(X509_getm_notAfter(Self), 60 * 60 * 24 * vTo);
end;

procedure TPX509Helper.AdjTime(vDays: NativeInt);
begin
  AdjTime(0, vDays);
end;

procedure TPX509Helper.SetSubjectName(const vField, vData: string);
var
  n: PX509_NAME;
  f, d: UTF8String;
begin
  if vData <> '' then
  begin
    n := X509_get_subject_name(Self);
    f := UTF8Encode(vField);
    d := UTF8Encode(vData);

    X509_NAME_add_entry_by_txt(n, PUTF8Char(f), MBSTRING_ASC, PByte(d), -1, -1, 0);
  end;
end;

procedure TPX509Helper.SetExt(NID: Integer; const vData: string);
var
  ex: PX509_EXTENSION;
  ctx: TX509V3_CTX;
  d: UTF8String;
begin
  if vData<>'' then
  begin
    d := UTF8Encode(vData);

    X509V3_set_ctx_nodb(@ctx);
    X509V3_set_ctx(@ctx, Self, Self, nil, nil, 0);

    ex := X509V3_EXT_conf_nid(nil, @ctx, nid, putf8char(d));
    try
      X509_add_ext(Self, ex, -1);
    finally
      X509_EXTENSION_free(ex);
    end;
  end;
end;

procedure TPX509Helper.SetSerial(vSerial: Integer);
begin
  ASN1_INTEGER_set(X509_get_serialNumber(Self), vSerial);
end;

end.
