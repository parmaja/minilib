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

    function SetExt(NID: Integer; const vData: string): Integer;
    function BIOstr(vProc: TProc<PBIO>): string;
    class function Generate(vConfig: TsslConfig; vProc: TProc<PX509, PEVP_PKEY>): Boolean; static;
  end;

  TSSLStackHelper = record helper for POPENSSL_STACK
    function SetExt(NID: Integer; const vData: string): Integer;
    function SetStack(typ: Integer; const vData: TStrings): Integer;
  end;

  TSSLStackData = record
    Name: UTF8String;
    Gen: PGENERAL_NAME;
    ASN: PASN1_STRING;
  end;

  TSSLStackArr = TArray<TSSLStackData>;



function MakeX509(vConfig: TsslConfig): PX509;
function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
function MakeCertReq(vConfig: TsslConfig; px: PX509; pk: PEVP_PKEY): Boolean; overload;
function MakeCert(vConfig: TsslConfig): Boolean; overload;
function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;
function BuildAltStack(AltType: Integer; Names: TStrings; var vArr: TSSLStackArr): POPENSSL_STACK;

implementation

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

    Result.SetSubjectName(NID_pkcs9_emailAddress, vConfig.ReadString('req', 'emailAddress', ''));

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
  n, v: UTF8String;
  i: Integer;

  s: TStrings;
  t, sk: POPENSSL_STACK;
  res: Integer;
begin
  //C:\temp\openssl-master\apps\req.c

  req := X509_to_X509_REQ(px, pk, EVP_sha256);
  try

    {n := UTF8Encode('1.3.6.1.4.1.311.20.2');
    n := UTF8Encode('TSTZATCACodeSigning');
    //vConfig.ReadSection

    i := px.SetExt(NID_subject_alt_name, 'SN=1-TST|2-TST|3-ed22f1d8-e6a2-1118-9b58-d9a8f11e445f');
    i := X509_REQ_add1_attr_by_txt(req, PByte(n), MBSTRING_ASC, PByte(v), -1);}

    sk := OPENSSL_sk_new_null;
    try
      sk.SetExt(NID_key_usage, vConfig.ReadString('v3_req', SN_key_usage, ''));

      s := vConfig.AltNames;
      try
        sk.SetStack(GEN_DNS, s);
      finally
        s.Free;
      end;

      X509_REQ_add_extensions(req, sk);
    finally
      OPENSSL_sk_free(sk);
    end;

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
  i: Integer;
  c: Integer;
  g: PGENERAL_NAME;
  v: PASN1_STRING;
begin
  Result := OPENSSL_sk_new_null;
  if Names.Count<>0 then
  begin
    SetLength(vArr, Names.Count);
    for i:=0 to Names.Count-1 do
    begin
      vArr[i].Gen := GENERAL_NAME_new;
      vArr[i].ASN := ASN1_STRING_new;
      vArr[i].Name := UTF8Encode(Names[i]);

      ASN1_STRING_set(vArr[i].ASN, PByte(vArr[i].Name), Length(vArr[i].Name));
      GENERAL_NAME_set0_value(vArr[i].Gen, AltType, vArr[i].ASN);
      OPENSSL_sk_push(Result, vArr[i].Gen);
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

function TSSLStackHelper.SetStack(typ: Integer; const vData: TStrings): Integer;
var
  sk: POPENSSL_STACK;
  aArr: TSSLStackArr;
begin
  sk := BuildAltStack(GEN_DNS, vData, aArr);
  try
    Result := X509V3_add1_i2d(Self, NID_subject_alt_name, sk, 0, 0);
  finally
    OPENSSL_sk_free(sk);
  end;
end;

end.
