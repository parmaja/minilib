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
{$error 'Delphi Only'}
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
  mnClasses,
  mnUtils,
  mnSockets,
  mnOpenSSL,
  mnOpenSSLAPI;

type

  TsslConfig = class(TMemIniFile)
  public
    constructor Create;
    procedure UpdateFile; override;
    function AltNames: TStrings;
  end;

  TPX509Helper = record helper for PX509
    procedure SetSerial(vSerial: Integer);
    procedure AdjTime(vDays: NativeInt); overload;
    procedure AdjTime(vFrom, vTo: NativeInt); overload;

    procedure SetSubjectName(const vField, vData: string); overload;
    procedure SetSubjectName(vNID: Integer; const vData: string); overload;

    function RegisterOID(const vName, vSNName: string): Integer;
    function SetExt(NID: Integer; const vData: string): Integer;
    function BIOstr(vProc: TProc<PBIO>): string;
    class function Generate(vConfig: TsslConfig; vProc: TProc<PX509, PEVP_PKEY>): Boolean; static;
  end;

  TPX509ReqHelper = record helper for PX509_REQ
    procedure SetSubjectName(const vField, vData: string); overload;
    procedure SetSubjectName(vNID: Integer; const vData: string); overload;
    function SetExt(sk: POPENSSL_STACK; const vName, vData: string): Integer; overload;
  end;

  TSSLStackHelper = record helper for POPENSSL_STACK
    function SetExt(NID: Integer; const vData: string): Integer; overload;
    function SetExt(req: PX509_REQ; const vName, vData: string): Integer; overload;
    function SetStack(typ: Integer; const vData: TStrings): Integer;
  end;

  TSSLStackData = record
    Name: UTF8String;
    Gen: PGENERAL_NAME;
    ASN: PASN1_OCTET_STRING;
  end;

  TSSLStackArr = TArray<TSSLStackData>;



function MakeX509(vConfig: TsslConfig): PX509;
function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
function MakeCertReq(vConfig: TsslConfig; px: PX509; pk: PEVP_PKEY): Boolean; overload;
function MakeCert(vConfig: TsslConfig): Boolean; overload;
function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;
function BuildAltStack(AltType: Integer; Names: TStrings; var vArr: TSSLStackArr): POPENSSL_STACK;


function MakeCert2(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;
function MakeCert2(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

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

function MakeCert2(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
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

function MakeCert2(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
var
	x509: PX509;
	pkey: PEVP_PKEY;
  outbio: PBIO;
begin
	x509 :=nil;
	pkey := nil;
  try
    Result := MakeCert2(x509, pkey, CN, O, C, OU, Bits, Serial, Days);
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

function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
var
  ecp: PEC_KEY;
  ecg: PEC_GROUP;
  rsa: PRSA;
  bn: PBIGNUM;
  sign: PX509_sign;
  bits: Integer;
begin
  bits := vConfig.ReadInteger('req', 'default_bits', 2048);

  Result := EVP_PKEY_new();
  try
    ecp := EC_KEY_new();
    ecg := EC_GROUP_new_by_curve_name(NID_secp256k1);
    try
      if EC_KEY_set_group(ecp,ecg)=0 then raise Exception.Create('Error EC_KEY_set_group');
      if EC_KEY_generate_key(ecp)=0 then raise Exception.Create('Error EC_KEY_generate_key');
      if EVP_PKEY_assign_EC_KEY(Result, ecp)=0 then raise Exception.Create('Error EVP_PKEY_assign_EC_KEY');

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

  {Result := EVP_PKEY_new();
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
  end;}
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

    Result.SetSubjectName(NID_pkcs9_emailAddress, vConfig.ReadString('req', 'emailAddress', ''));
    Result.RegisterOID('1.3.6.1.4.1.311.20.2', 'MSOID');

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

      Result.SetExt(NID_key_usage, vConfig.ReadString(n, SN_key_usage, ''));
    end;

  except
    FreeAndNil(Result);
  end;
end;

function MakeCertReq(vConfig: TsslConfig; px: PX509; pk: PEVP_PKEY): Boolean; overload;
var
  req: PX509_REQ;
  n: string;

  s: TStrings;
  sk: POPENSSL_STACK;
  res: Integer;
begin
  //C:\temp\openssl-master\apps\req.c

  //req := X509_to_X509_REQ(px, pk, EVP_sha256);
  req := X509_REQ_new;
  try

    n := vConfig.ReadString('req', 'distinguished_name', '');
    if n<>'' then
    begin
      req.SetSubjectName('CN', vConfig.ReadString(n, 'CN', ''));
      req.SetSubjectName('O', vConfig.ReadString(n, 'O', ''));
      req.SetSubjectName('C', vConfig.ReadString(n, 'C', ''));
      req.SetSubjectName('OU', vConfig.ReadString(n, 'OU', ''));

      (* Its self signed so set the issuer name to be the same as the
       * subject.
       *)
      //X509_set_issuer_name(x, name);
      //X509_set_issuer_name(req, X509_get_subject_name(Result));
      X509_REQ_set_subject_name(req, X509_REQ_get_subject_name(req));
    end;


    req.SetSubjectName(NID_pkcs9_emailAddress, vConfig.ReadString('req', 'emailAddress', ''));
    sk := OPENSSL_sk_new_null;
    try

      s := vConfig.AltNames;
      try
        if s.Text<>'' then
          sk.SetStack(GEN_DIRNAME, s);
      finally
        s.Free;
      end;

      sk.SetExt(NID_key_usage, vConfig.ReadString('v3_req', SN_key_usage, ''));
      req.SetExt(sk, 'MSOID', 'ASN1:PRINTABLESTRING:TSTZATCACodeSigning');

      X509_REQ_add_extensions(req, sk);
    finally
      OPENSSL_sk_free(sk);
    end;


    X509_REQ_set_pubkey(req, pk);
    X509_REQ_sign(req, pk, EVP_sha256);

    vConfig.WriteString('Result', 'Csr', px.BIOstr(procedure(bio: PBIO)
    begin
      PEM_write_bio_X509_REQ(bio, req);
    end));
  finally
    X509_REQ_free(req);
  end;

  Result := True;
end;

function MakeCert(vConfig: TsslConfig): Boolean; overload;
begin
  Result := PX509.Generate(vConfig, procedure(px: PX509; pk: PEVP_PKEY)
  begin
    vConfig.WriteString('Result', 'PrvKey', px.BIOstr(procedure(bio: PBIO)
    begin
      PEM_write_bio_PrivateKey(bio, pk, nil, nil, 0, nil, nil);
    end));

    vConfig.WriteString('Result', 'PubKey', px.BIOstr(procedure(bio: PBIO)
    var
      rsa: PRSA;
    begin
      rsa := EVP_PKEY_get1_RSA(pk);
      PEM_write_bio_RSAPublicKey(bio, rsa);
    end));

    vConfig.WriteString('Result', 'Cer', px.BIOstr(procedure(bio: PBIO)
    begin
      PEM_write_bio_X509(bio, px);
    end));

    MakeCertReq(vConfig, px, pk);
  end);
end;

function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;

  procedure _Write(const vFile, vData: string);
  var
    m: TMemoryStream;
    b: TBytes;
  begin
    if vData<>'' then
    begin
      b := TEncoding.UTF8.GetBytes(vData);
      m := TMemoryStream.Create;
      try
        m.WriteData(b, Length(b));
        m.SaveToFile(vFile);
      finally
        m.Free;
      end;
    end;
  end;

var
  cn, rn, pn, vn: string;
begin
  Result := MakeCert(vConfig);
  if Result then
  begin
    _Write(vName+'.cer', vConfig.ReadString('Result', 'Cer', ''));
    _Write(vName+'.csr', vConfig.ReadString('Result', 'Csr', ''));
    _Write(vName+'.private.pem', vConfig.ReadString('Result', 'PrvKey', ''));
    _Write(vName+'.public.pem', vConfig.ReadString('Result', 'PubKey', ''));
  end;
end;

{ TsslConfig }

constructor TsslConfig.Create;
begin
  inherited Create('');
end;

function TsslConfig.AltNames: TStrings;
begin
  Result := TStringList.Create;
  try
    if SectionExists('alt_names') then
      ReadSectionValues('alt_names', Result);
  except
    FreeAndNil(Result);
  end;
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

function TPX509Helper.BIOstr(vProc: TProc<PBIO>): string;
var
  bio: PBIO;
  b: PByte;
  aLen: Integer;
begin
  bio := BIO_new(BIO_s_mem());
  try
    vProc(bio);
    aLen := BIO_get_mem_data(bio, b);
    Result := TEncoding.ANSI.GetString(b, aLen);
  finally
    BIO_free(bio);
  end;
end;

class function TPX509Helper.Generate(vConfig: TsslConfig; vProc: TProc<PX509, PEVP_PKEY>): Boolean;
var
  px: PX509;
  pk: PEVP_PKEY;
begin
  px := MakeX509(vConfig);
  if px<>nil then
  begin
    try
      pk := SignX509(px, vConfig);
      if pk<>nil then
      begin
        try
          vProc(px, pk);
        finally
          EVP_PKEY_free(pk);
        end;
        Exit(True);
      end;
    finally
      X509_free(px);
    end;
  end;
  Result := False;
end;

function TPX509Helper.RegisterOID(const vName, vSNName: string): Integer;
var
  n, sn: UTF8String;
begin
  n := UTF8Encode(vName);
  sn := UTF8Encode(vSNName);
  Result := OBJ_create(PUTF8Char(n), PUTF8Char(sn), PUTF8Char(sn));
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

function TPX509Helper.SetExt(NID: Integer; const vData: string): Integer;
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
      Result := X509_add_ext(Self, ex, -1);
    finally
      X509_EXTENSION_free(ex);
    end;
  end
  else
    Result := 0;
end;

procedure TPX509Helper.SetSerial(vSerial: Integer);
begin
  ASN1_INTEGER_set(X509_get_serialNumber(Self), vSerial);
end;

procedure TPX509Helper.SetSubjectName(vNID: Integer; const vData: string);
var
  n: PX509_NAME;
  d: UTF8String;
begin
  if vData <> '' then
  begin
    n := X509_get_subject_name(Self);
    d := UTF8Encode(vData);

    X509_NAME_add_entry_by_NID(n, vNID, MBSTRING_ASC, PByte(d), -1, -1, 0);
  end;
end;

function BuildAltStack(AltType: Integer; Names: TStrings; var vArr: TSSLStackArr): POPENSSL_STACK;
var
  g: PGENERAL_NAME;
  v: PASN1_OCTET_STRING;
  t: UTF8String;
  i: Integer;
begin
  vArr := nil;
  Result := OPENSSL_sk_new_null;
  if Names.Count<>0 then
  begin
    i := 0;
    SetLength(vArr, Names.Count);

    for var s in Names do
    begin
      g := GENERAL_NAME_new;
      v := ASN1_OCTET_STRING_new;
      t := UTF8Encode(s);

      var x := ASN1_OCTET_STRING_set(v, PByte(t), Length(t));
      GENERAL_NAME_set0_value(g, AltType, v);
      OPENSSL_sk_push(Result, g);

      vArr[i].Name := t;
      vArr[i].Gen := g;
      vArr[i].Asn := v;
      Inc(i);
      Exit;
    end;

    {Tot := Names.Count;
    if Tot = 0 then Exit;
    SetLength (FAltAnsiStr, Tot);
    SetLength (FAltIa5Str, Tot);
    SetLength (FAltGenStr, Tot);
    for I := 0 to Tot - 1 do begin
        FAltGenStr[I] := f_GENERAL_NAME_new;
        if NOT Assigned(FAltGenStr[I]) then Exit;
        FAltIa5Str[I] := f_ASN1_STRING_new;
        if NOT Assigned(FAltIa5Str[I]) then Exit;
        FAltAnsiStr[I] := AnsiString(trim(Names[I]));
        if FAltAnsiStr[I] <> '' then begin
            f_ASN1_STRING_set(FAltIa5Str[I], PAnsiChar(FAltAnsiStr[I]), Length(FAltAnsiStr[I]));
            f_GENERAL_NAME_set0_value(FAltGenStr[I], AltType, FAltIa5Str[I]);
            f_OPENSSL_sk_push(result, Pointer(FAltGenStr[I]));
        end;
    end;}
  end;
end;

{ TSSLStackHelper }

function TSSLStackHelper.SetExt(NID: Integer; const vData: string): Integer;
var
  Ext : PX509_EXTENSION;
  d: UTF8String;
begin
  Result := 0;
  if vData<>'' then
  begin
    d := UTF8Encode(vData);
    Ext := X509V3_EXT_conf_nid(nil, nil, NID, PUTF8Char(d));
    OPENSSL_sk_push(Self, ext);
  end;
end;

function TSSLStackHelper.SetExt(req: PX509_REQ; const vName, vData: string): Integer;
var
  ex: PX509_EXTENSION;
  ctx: TX509V3_CTX;
  n, d: UTF8String;
  nid: Integer;
begin
  if vData<>'' then
  begin
    n := UTF8Encode(vName);
    d := UTF8Encode(vData);

    X509V3_set_ctx_nodb(@ctx);
    X509V3_set_ctx(@ctx, nil, nil, Self, nil, 0);
    nid := OBJ_txt2nid(PUTF8Char(n));


    ex := X509V3_EXT_conf_nid(nil, @ctx, nid, putf8char(d));
    try
      Result := X509_REQ_add_extensions_nid(req, Self, nid);
    finally
      X509_EXTENSION_free(ex);
    end;
  end
  else
    Result := 0;
end;

function TSSLStackHelper.SetStack(typ: Integer; const vData: TStrings): Integer;
var
  sk: POPENSSL_STACK;
  aArr: TSSLStackArr;
begin
  sk := BuildAltStack(typ, vData, aArr);
  try
    Result := X509V3_add1_i2d(Self, NID_subject_alt_name, sk, 0, X509V3_ADD_REPLACE);
  finally
    OPENSSL_sk_free(sk);
  end;
end;


{ TPX509ReqHelper }

function TPX509ReqHelper.SetExt(sk: POPENSSL_STACK; const vName, vData: string): Integer;
var
  ex: PX509_EXTENSION;
  ctx: TX509V3_CTX;
  n, d: UTF8String;
  nid: Integer;
begin
  Result := 0;
  if vData<>'' then
  begin
    n := UTF8Encode(vName);
    d := UTF8Encode(vData);

    X509V3_set_ctx_nodb(@ctx);
    X509V3_set_ctx(@ctx, nil, nil, Self, nil, 0);
    nid := OBJ_txt2nid(PUTF8Char(n));


    ex := X509V3_EXT_conf_nid(nil, @ctx, nid, putf8char(d));
    try
      OPENSSL_sk_push(sk, ex);
      //Result := X509_REQ_add_extensions_nid(Self, sk, nid);
    finally
      //X509_EXTENSION_free(ex);
    end;
  end;
end;

procedure TPX509ReqHelper.SetSubjectName(vNID: Integer; const vData: string);
var
  n: PX509_NAME;
  d: UTF8String;
begin
  if vData <> '' then
  begin
    n := X509_REQ_get_subject_name(Self);
    d := UTF8Encode(vData);

    X509_NAME_add_entry_by_NID(n, vNID, MBSTRING_ASC, PByte(d), -1, -1, 0);
  end;
end;

procedure TPX509ReqHelper.SetSubjectName(const vField, vData: string);
var
  n: PX509_NAME;
  f, d: UTF8String;
begin
  if vData <> '' then
  begin
    n := X509_REQ_get_subject_name(Self);
    f := UTF8Encode(vField);
    d := UTF8Encode(vData);

    X509_NAME_add_entry_by_txt(n, PUTF8Char(f), MBSTRING_ASC, PByte(d), -1, -1, 0);
  end;
end;

end.
