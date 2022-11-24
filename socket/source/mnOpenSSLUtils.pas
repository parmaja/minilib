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

    procedure SetSubjectName(const vField, vData: string);
    function SetExt(NID: Integer; const vData: string): Integer;
    function BIOstr(vProc: TProc<PBIO>): string;
    class function Generate(vConfig: TsslConfig; vProc: TProc<PX509, PEVP_PKEY>): Boolean; static;
  end;


function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

function MakeX509(vConfig: TsslConfig): PX509;
function SignX509(X509: PX509; vConfig: TsslConfig): PEVP_PKEY;
function MakeCert(vConfig: TsslConfig): Boolean; overload;
function MakeCert(const vName: string; vConfig: TsslConfig): Boolean; overload;
function BuildAltStack(AltType: Integer; Names: TStrings): POPENSSL_STACK;

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
  s: TStrings;
  sk: POPENSSL_STACK;
  var res: Integer;
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

      Result.SetExt(NID_key_usage, vConfig.ReadString(n, SN_key_usage, ''));
    end;

    s := vConfig.AltNames;
    try
      sk := BuildAltStack(GEN_DNS, s);
      try
        res := X509_add1_ext_i2d(Result, NID_subject_alt_name, sk, 0, 0);
      finally
        OPENSSL_sk_free(sk);
      end;

    finally
      s.Free;
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
begin
  //C:\temp\openssl-master\apps\req.c

  req := X509_to_X509_REQ(px, pk, EVP_sha256);
  try
    n := UTF8Encode('1.3.6.1.4.1.311.20.2');
    n := UTF8Encode('TSTZATCACodeSigning');
    //vConfig.ReadSection

    i := px.SetExt(NID_subject_alt_name, 'SN=1-TST|2-TST|3-ed22f1d8-e6a2-1118-9b58-d9a8f11e445f');
    i := X509_REQ_add1_attr_by_txt(req, PByte(n), MBSTRING_ASC, PByte(v), -1);

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

function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean;
var
	x509: PX509;
	pkey: PEVP_PKEY;
  outbio: PBIO;
  s: utf8string;
  xx: PX509_REQ;
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
    xx := X509_to_X509_REQ(x509, pkey, EVP_sha256);
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

function BuildAltStack(AltType: Integer; Names: TStrings): POPENSSL_STACK;
var
  i: Integer;
  c: Integer;
  g: PGENERAL_NAME;
  v: PASN1_STRING;
  t: UTF8String;
begin
  Result := OPENSSL_sk_new_null;
  if Names.Count<>0 then
  begin
    for var s in Names do
    begin
      g := GENERAL_NAME_new;
      v := ASN1_STRING_new;
      t := UTF8Encode(s);

      ASN1_STRING_set(v, PByte(t), Length(t));
      GENERAL_NAME_set0_value(g, AltType, v);
      OPENSSL_sk_push(Result, g)
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

end.
