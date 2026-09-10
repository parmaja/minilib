unit mnACME;
{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$endif}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{
  ACME v2 client (RFC 8555) to request/renew certificates from Let's Encrypt
  https://letsencrypt.org/

  Uses HTTP-01 challenge, so a web server must serve:
    http://<domain>/.well-known/acme-challenge/<token>
  Use TmodWebServer.AddChallengeAcme for that, and pass its challenge dir:
    <acme home>\acme-challenge\   (the challenge token files are created there)

  Files:
    AAccountKeyFile   RSA private key of the ACME account (created if missing)
    AAccountKidFile   Account URL (kid) saved after first registration

  Results:
    ACertificateFile  full chain PEM
    APrivateKeyFile   certificate private key PEM (new key generated each time)
}

interface

uses
  SysUtils, Classes, StrUtils,
  mnTypes, mnUtils, mnLogs, mnClasses, mnFields, mnParams, mnModules,
  mnSockets, mnClients, mnStreams, mnStreamUtils,
  mnOpenSSL, {$IFDEF OPENSSL3}mnOpenSSL3API{$ELSE}mnOpenSSLAPI{$ENDIF}, mnBase64, mnDON, mnJSON,
  mnWebModules, mnHttpClient;

const
  cLetsEncryptProduction = 'https://acme-v02.api.letsencrypt.org/directory';
  cLetsEncryptStaging = 'https://acme-staging-v02.api.letsencrypt.org/directory';

type
  TacmeLog = procedure(const S: string) of object;

//Read the notAfter (expiry) date from the first certificate in a PEM file.
//Returns 0 if the file cannot be read or contains no valid certificate.
function CertExpiryDate(const ACertificateFile: string): TDateTime;

//Read expiry from the sidecar .expiry text file written by AcmeRenewCertificate.
//Falls back to parsing the PEM when the sidecar is absent.
//Returns 0 on any failure.
function CertExpiryDateFromFile(const ACertificateFile: string): TDateTime;

//Returns how many whole days remain before the certificate expires (UTC now).
//Negative = already expired.  Returns MaxInt when the expiry cannot be read.
function CertDaysLeft(const ACertificateFile: string): Integer;

procedure AcmeRenewCertificate(const ADomain: string; const AEmail: string;
  const ACertificateFile, APrivateKeyFile: string;
  const AAccountKeyFile, AAccountKidFile: string;
  const AChallengeDir: string; ALog: TacmeLog;
  const ADirectoryURL: string = cLetsEncryptProduction);

implementation

const
  sB64URLChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
  cMBSTRING_UTF8 = $1000;
  cMaxMDSize = 64;
  V_ASN1_UTCTIME          = 23;
  V_ASN1_GENERALIZEDTIME  = 24;

function BinToB64Url(Buf: PByte; Len: Integer): string;
var
  i: Integer;
  b0, b1, b2: Byte;
begin
  Result := '';
  i := 0;
  while i < Len do
  begin
    b0 := Buf[i];
    if i + 1 < Len then b1 := Buf[i + 1] else b1 := 0;
    if i + 2 < Len then b2 := Buf[i + 2] else b2 := 0;
    Result := Result + sB64URLChars[(b0 shr 2) + 1];
    Result := Result + sB64URLChars[(((b0 and $03) shl 4) or (b1 shr 4)) + 1];
    if i + 1 < Len then
      Result := Result + sB64URLChars[(((b1 and $0F) shl 2) or (b2 shr 6)) + 1];
    if i + 2 < Len then
      Result := Result + sB64URLChars[(b2 and $3F) + 1];
    Inc(i, 3);
  end;
end;

function StrToB64Url(const S: UTF8String): string;
begin
  if S = '' then
    Result := ''
  else
    Result := BinToB64Url(PByte(PAnsiChar(S)), Length(S));
end;

function B64UrlDecode(const S: string): TBytes;
var
  aPemStr: AnsiString;
  i, j, k: Integer;
  v: array[0..3] of Integer;

  function Idx(c: AnsiChar): Integer;
  begin
    case c of
      'A'..'Z': Result := Ord(c) - Ord('A');
      'a'..'z': Result := Ord(c) - Ord('a') + 26;
      '0'..'9': Result := Ord(c) - Ord('0') + 52;
      '-', '+': Result := 62;
      '_', '/': Result := 63;
      '=', #0: Result := -2; //padding or end
    else
      Result := -1; //invalid
    end;
  end;

begin
  aPemStr := AnsiString(StringReplace(StringReplace(S, '-', '+', [rfReplaceAll]), '_', '/', [rfReplaceAll]));
  SetLength(Result, (Length(aPemStr) * 3) div 4 + 3);
  j := 0;
  i := 1;
  while i <= Length(aPemStr) do
  begin
    for k := 0 to 3 do
    begin
      if i + k <= Length(aPemStr) then
        v[k] := Idx(aPemStr[i + k])
      else
        v[k] := -2;
    end;
    if (v[0] >= 0) and (v[1] >= 0) then
    begin
      Result[j] := Byte((v[0] shl 2) or ((v[1] and $30) shr 4));
      Inc(j);
      if v[2] >= 0 then
      begin
        Result[j] := Byte(((v[1] and $0F) shl 4) or ((v[2] and $3C) shr 2));
        Inc(j);
        if v[3] >= 0 then
        begin
          Result[j] := Byte(((v[2] and $03) shl 6) or v[3]);
          Inc(j);
        end;
      end;
    end;
    Inc(i, 4);
  end;
  SetLength(Result, j);
end;

function SHA256Buf(Buf: PByte; Len: Integer): TBytes;
var
  md: array[0..cMaxMDSize - 1] of Byte;
  mdLen: Cardinal;
begin
  SetLength(Result, 32);
  if EVP_Digest(Buf, Cardinal(Len), @md[0], @mdLen, EVP_sha256(), nil) <> 1 then
    raise Exception.Create('ACME: SHA256 digest failed');
  Move(md[0], Result[0], 32);
end;

function StrSHA256(const S: UTF8String): TBytes;
begin
  if S = '' then
    Result := SHA256Buf(nil, 0)
  else
    Result := SHA256Buf(PByte(PAnsiChar(S)), Length(S));
end;

function BytesToUTF8(const Data: TBytes): UTF8String;
begin
  if Length(Data) = 0 then
    Result := ''
  else
    SetString(Result, PAnsiChar(@Data[0]), Length(Data));
end;

function RSASignSHA256(PKey: PEVP_PKEY; const Digest: TBytes): TBytes;
var
{$ifdef OPENSSL3}
  ctx: PEVP_PKEY_CTX;
  sigLen: NativeUInt;
{$else}
  rsa: PRSA;
  sigLen: Cardinal;
{$endif}
begin
{$ifdef OPENSSL3}
  sigLen := EVP_PKEY_get_size(PKey);
  SetLength(Result, sigLen);
  ctx := EVP_PKEY_CTX_new(PKey, nil);
  try
    if EVP_PKEY_sign_init(ctx) <> 1 then
      raise Exception.Create('ACME: RSA sign init failed');
    if EVP_PKEY_CTX_set_signature_md(ctx, EVP_sha256()) <> 1 then
      raise Exception.Create('ACME: RSA sign md failed');
    if EVP_PKEY_sign(ctx, PByte(Result), sigLen, PByte(@Digest[0]), Length(Digest)) <> 1 then
      raise Exception.Create('ACME: RSA sign failed');
  finally
    EVP_PKEY_CTX_free(ctx);
  end;
  SetLength(Result, sigLen);
{$else}
  rsa := EVP_PKEY_get1_RSA(PKey);
  try
    sigLen := RSA_size(rsa);
    SetLength(Result, sigLen);
    sigLen := 0;
    if RSA_sign(NID_sha256, PByte(@Digest[0]), Length(Digest), PByte(Result), @sigLen, rsa) <> 1 then
      raise Exception.Create('ACME: RSA sign failed');
  finally
    RSA_free(rsa);
  end;
  SetLength(Result, sigLen);
{$endif}
end;

function BNToB64Url(bn: PBIGNUM): string;
var
  l: Integer;
  buf: TBytes;
begin
  l := (BN_num_bits(bn) + 7) div 8;
  SetLength(buf, l);
  if l > 0 then
    BN_bn2bin(bn, @buf[0]);
  Result := BinToB64Url(@buf[0], l);
end;

function DonGet(AValue: TDON_Value; const AName: string): TDON_Value;
begin
  if AValue = nil then
    Result := nil
  else
    Result := AValue.Values[AName];
end;

function DonValStr(AValue: TDON_Value): string;
begin
  if AValue = nil then
    Result := ''
  else if AValue is TDON_CustomStringValue then
    Result := TDON_CustomStringValue(AValue).AsString
  else
    Result := AValue.AsString;
end;

function DonStr(AValue: TDON_Value; const AName: string): string;
begin
  Result := DonValStr(DonGet(AValue, AName));
end;

function DonFindChallenge(Authz: TDON_Value; const AType: string; out Token, ChallengeURL: string): Boolean;
var
  aChallenges: TDON_Value;
  aArr: TDON_Array;
  i: Integer;
  aItem: TDON_Value;
begin
  Result := False;
  Token := '';
  ChallengeURL := '';
  aChallenges := DonGet(Authz, 'challenges');
  if (aChallenges = nil) or not (aChallenges is TDON_Array) then
    Exit;
  aArr := TDON_Array(aChallenges);
  for i := 0 to aArr.Count - 1 do
  begin
    aItem := aArr[i];
    if SameText(DonStr(aItem, 'type'), AType) then
    begin
      Token := DonStr(aItem, 'token');
      ChallengeURL := DonStr(aItem, 'url');
      Result := Token <> '';
      Exit;
    end;
  end;
end;

procedure RaiseAcmeError(const ABody, AContext: string);
var
  aObj: TDON_Pair;
  aDetail: string;
begin
  if Pos('acme:error', ABody) > 0 then
  begin
    aDetail := Trim(ABody);
    aObj := JsonParseString(ABody, [jsoSafe]);
    if aObj <> nil then
    begin
      try
        if DonStr(aObj, 'detail') <> '' then
          aDetail := DonStr(aObj, 'detail');
      finally
        aObj.Free;
      end;
    end;
    raise Exception.Create('ACME error (' + AContext + '): ' + aDetail);
  end;
end;

//Return base64 content of the first PEM block
function ExtractPemBody(const APem: string): string;
var
  sl: TStringList;
  i: Integer;
  aIn: Boolean;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := APem;
    aIn := False;
    for i := 0 to sl.Count - 1 do
    begin
      if Pos('-----END', sl[i]) > 0 then
        Break;
      if aIn then
        Result := Result + Trim(sl[i]);
      if Pos('-----BEGIN', sl[i]) > 0 then
        aIn := True;
    end;
  finally
    sl.Free;
  end;
end;

//Parse an ASN1 UTCTime (YYMMDDHHMMSSZ) or GeneralizedTime (YYYYMMDDHHMMSSZ)
//string directly from the raw data pointer without needing ASN1_TIME_to_tm.
function ASN1TimeToDateTime(A: PASN1_TIME): TDateTime;
var
  s: AnsiString;
  yr, mo, dy, hh, mm, ss: Integer;
  p: Integer;
begin
  Result := 0;
  if A = nil then Exit;
  SetString(s, PAnsiChar(A^.data), A^.length);
  if s = '' then Exit;
  p := 1;
  if A^.&type = V_ASN1_GENERALIZEDTIME then
  begin
    //YYYYMMDDHHMMSSZ  (14 chars + Z)
    if Length(s) < 14 then Exit;
    yr := StrToIntDef(Copy(string(s), 1, 4), 0);  p := 5;
  end
  else
  begin
    //YYMMDDHHMMSSZ  (12 chars + Z)
    if Length(s) < 12 then Exit;
    yr := StrToIntDef(Copy(string(s), 1, 2), 0);  p := 3;
    if yr >= 50 then yr := 1900 + yr else yr := 2000 + yr;
  end;
  mo := StrToIntDef(Copy(string(s), p,     2), 0);
  dy := StrToIntDef(Copy(string(s), p + 2, 2), 0);
  hh := StrToIntDef(Copy(string(s), p + 4, 2), 0);
  mm := StrToIntDef(Copy(string(s), p + 6, 2), 0);
  ss := StrToIntDef(Copy(string(s), p + 8, 2), 0);
  try
    Result := EncodeDate(yr, mo, dy) + EncodeTime(hh, mm, ss, 0);
  except
    Result := 0;
  end;
end;

function CertExpiryDate(const ACertificateFile: string): TDateTime;
var
  bio: PBIO;
  x509: PX509;
  t: PASN1_TIME;
  m: TMemoryStream;
  fs: TFileStream;
begin
  Result := 0;
  if not FileExists(ACertificateFile) then Exit;
  m := TMemoryStream.Create;
  try
    fs := TFileStream.Create(ACertificateFile, fmOpenRead or fmShareDenyWrite);
    try
      m.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
    bio := BIO_new_mem_buf(PByte(m.Memory), m.Size);
    try
      x509 := PEM_read_bio_X509(bio, nil, nil, nil);
      if x509 <> nil then
      try
        {$ifdef OPENSSL3}
        t := X509_get0_notAfter(x509); //replaces X509_getm_notAfter (deprecated in OpenSSL 3.x)
        {$else}
        t := X509_getm_notAfter(x509);
        {$endif}
        Result := ASN1TimeToDateTime(t);
      finally
        X509_free(x509);
      end;
    finally
      BIO_free(bio);
    end;
  finally
    m.Free;
  end;
end;

function CertExpiryDateFromFile(const ACertificateFile: string): TDateTime;
begin
  Result := CertExpiryDate(ACertificateFile);
end;

function CertDaysLeft(const ACertificateFile: string): Integer;
var
  aExpiry: TDateTime;
begin
  aExpiry := CertExpiryDateFromFile(ACertificateFile);
  if aExpiry = 0 then
    Result := MaxInt
  else
    Result := Trunc(aExpiry - Now);
end;

procedure AcmeRenewCertificate(const ADomain: string; const AEmail: string;
  const ACertificateFile, APrivateKeyFile: string;
  const AAccountKeyFile, AAccountKidFile: string;
  const AChallengeDir: string; ALog: TacmeLog;
  const ADirectoryURL: string);

  procedure Log(const S: string);
  begin
    if Assigned(ALog) then
      ALog('ACME: ' + S);
  end;

var
  aKeyPKey: PEVP_PKEY;
{$ifndef OPENSSL3}
  aKeyRSA: PRSA;
{$endif}
  aKid: string;
  aThumbprint: string;
  aJwk: string;
  aNonce: string;
  aNewNonceURL, aNewAccountURL, aNewOrderURL: string;
  aOrderURL, aFinalizeURL, aCertURL, aAuthzURL: string;
  aHttpClient: TmnHttpClient;

  //Load account RSA key from file or create a new one
  procedure LoadOrCreateAccountKey;
  var
    bio: PBIO;
    fs: TFileStream;
    m: TMemoryStream;
    aBN_N, aBN_E, aBN_D: PBIGNUM;
{$ifndef OPENSSL3}
    rsaNew: PRSA;
{$endif}
  begin
    aKeyPKey := nil;
{$ifndef OPENSSL3}
    aKeyRSA := nil;
{$endif}
{$ifdef OPENSSL3}
    if FileExists(AAccountKeyFile) then
    begin
      m := TMemoryStream.Create;
      try
        fs := TFileStream.Create(AAccountKeyFile, fmOpenRead or fmShareDenyWrite);
        try
          m.CopyFrom(fs, 0);
        finally
          fs.Free;
        end;
        bio := BIO_new_mem_buf(PByte(m.Memory), m.Size);
        try
          aKeyPKey := PEM_read_bio_PrivateKey(bio, nil, nil, nil); //replaces PEM_read_bio_RSAPrivateKey (deprecated in OpenSSL 3.x)
        finally
          BIO_free(bio);
        end;
      finally
        m.Free;
      end;
    end;

    if aKeyPKey = nil then
    begin
      Log('creating new account key ' + AAccountKeyFile);
      ForceDirectories(ExtractFilePath(AAccountKeyFile));
      aBN_E := BN_new();
      try
        BN_set_word(aBN_E, RSA_F4);
        aKeyPKey := GenerateRSAKey(2048, aBN_E); //replaces RSA_new/RSA_generate_key_ex (deprecated in OpenSSL 3.x)
      finally
        BN_free(aBN_E);
      end;
      if aKeyPKey = nil then
        raise Exception.Create('ACME: cannot generate account key');
      bio := BIO_new_file(PAnsiChar(Utf8String(AAccountKeyFile)), 'wt');
      if bio = nil then
        raise Exception.Create('ACME: cannot write ' + AAccountKeyFile);
      try
        if PEM_write_bio_PrivateKey(bio, aKeyPKey, nil, nil, 0, nil, nil) <> 1 then
          raise Exception.Create('ACME: cannot write ' + AAccountKeyFile);
      finally
        BIO_free(bio);
      end;
    end
    else
      Log('using account key ' + AAccountKeyFile);

    //JWK and thumbprint (RFC 7638)
    //RFC 7638 requires the required members sorted lexicographically: e, kty, n
    aBN_N := nil;
    aBN_E := nil;
    if EVP_PKEY_get_bn_param(aKeyPKey, OSSL_PKEY_PARAM_RSA_N, aBN_N) <> 1 then
      raise Exception.Create('ACME: cannot get account key N');
    if EVP_PKEY_get_bn_param(aKeyPKey, OSSL_PKEY_PARAM_RSA_E, aBN_E) <> 1 then
      raise Exception.Create('ACME: cannot get account key E');
    try
      aJwk := '{"e":"' + BNToB64Url(aBN_E) + '","kty":"RSA","n":"' + BNToB64Url(aBN_N) + '"}';
      aThumbprint := StrToB64Url(BytesToUTF8(StrSHA256(Utf8String(aJwk))));
    finally
      BN_free(aBN_N);
      BN_free(aBN_E);
    end;
{$else}
    if FileExists(AAccountKeyFile) then
    begin
      m := TMemoryStream.Create;
      try
        fs := TFileStream.Create(AAccountKeyFile, fmOpenRead or fmShareDenyWrite);
        try
          m.CopyFrom(fs, 0);
        finally
          fs.Free;
        end;
        bio := BIO_new_mem_buf(PByte(m.Memory), m.Size);
        try
          aKeyRSA := PEM_read_bio_RSAPrivateKey(bio, nil, nil, nil);
        finally
          BIO_free(bio);
        end;
      finally
        m.Free;
      end;
    end;

    if aKeyRSA = nil then
    begin
      Log('creating new account key ' + AAccountKeyFile);
      ForceDirectories(ExtractFilePath(AAccountKeyFile));
      rsaNew := RSA_new();
      try
        aBN_E := BN_new();
        try
          BN_set_word(aBN_E, RSA_F4);
          if RSA_generate_key_ex(rsaNew, 2048, aBN_E, nil) <> 1 then
            raise Exception.Create('ACME: cannot generate account key');
        finally
          BN_free(aBN_E);
        end;
        bio := BIO_new_file(PAnsiChar(Utf8String(AAccountKeyFile)), 'wt');
        if bio = nil then
          raise Exception.Create('ACME: cannot write ' + AAccountKeyFile);
        try
          PEM_write_bio_RSAPrivateKey(bio, rsaNew, nil, nil, 0, nil, nil);
        finally
          BIO_free(bio);
        end;
        aKeyRSA := rsaNew;
        rsaNew := nil; //owned by pkey below
      finally
        if rsaNew <> nil then
          RSA_free(rsaNew);
      end;
    end
    else
      Log('using account key ' + AAccountKeyFile);

    aKeyPKey := EVP_PKEY_new();
    EVP_PKEY_assign_RSA(aKeyPKey, aKeyRSA); //pkey owns the RSA now

    //JWK and thumbprint (RFC 7638)
    //RFC 7638 requires the required members sorted lexicographically: e, kty, n
    aBN_N := nil;
    aBN_E := nil;
    aBN_D := nil;
    RSA_get0_key(aKeyRSA, aBN_N, aBN_E, aBN_D);
    aJwk := '{"e":"' + BNToB64Url(aBN_E) + '","kty":"RSA","n":"' + BNToB64Url(aBN_N) + '"}';
    aThumbprint := StrToB64Url(BytesToUTF8(StrSHA256(Utf8String(aJwk))));
{$endif}
  end;

  //Generate the certificate private key and CSR with SAN for the domain
  procedure MakeCertificateKeyAndCSR(const AKeyFile: string; out ACSRPem: string);
  var
    pkey: PEVP_PKEY;
    req: PX509_REQ;
    name: PX509_NAME;
    sk: POPENSSL_STACK;
    ext: PX509_EXTENSION;
    bio: PBIO;
    aBN_E: PBIGNUM;
    s: AnsiString;
    buf: array[0..4095] of AnsiChar;
    n: Integer;
{$ifndef OPENSSL3}
    rsa: PRSA;
{$endif}
  begin
    Log('generating certificate key ' + AKeyFile);
    ForceDirectories(ExtractFilePath(AKeyFile));

    pkey := nil;
    try
{$ifdef OPENSSL3}
      aBN_E := BN_new();
      try
        BN_set_word(aBN_E, RSA_F4);
        pkey := GenerateRSAKey(2048, aBN_E); //replaces RSA_new/RSA_generate_key_ex (deprecated in OpenSSL 3.x)
      finally
        BN_free(aBN_E);
      end;
      if pkey = nil then
        raise Exception.Create('ACME: cannot generate certificate key');

      bio := BIO_new_file(PAnsiChar(Utf8String(AKeyFile)), 'wt');
      if bio = nil then
        raise Exception.Create('ACME: cannot write ' + AKeyFile);
      try
        if PEM_write_bio_PrivateKey(bio, pkey, nil, nil, 0, nil, nil) <> 1 then
          raise Exception.Create('ACME: cannot write ' + AKeyFile);
      finally
        BIO_free(bio);
      end;
{$else}
      rsa := RSA_new();
      aBN_E := BN_new();
      try
        BN_set_word(aBN_E, RSA_F4);
        if RSA_generate_key_ex(rsa, 2048, aBN_E, nil) <> 1 then
          raise Exception.Create('ACME: cannot generate certificate key');
      finally
        BN_free(aBN_E);
      end;

      bio := BIO_new_file(PAnsiChar(Utf8String(AKeyFile)), 'wt');
      if bio = nil then
        raise Exception.Create('ACME: cannot write ' + AKeyFile);
      try
        PEM_write_bio_RSAPrivateKey(bio, rsa, nil, nil, 0, nil, nil);
      finally
        BIO_free(bio);
      end;

      pkey := EVP_PKEY_new();
      EVP_PKEY_assign_RSA(pkey, rsa); //pkey owns rsa
{$endif}

      req := X509_REQ_new();
      try
        X509_REQ_set_version(req, 0);
        name := X509_REQ_get_subject_name(req);
        X509_NAME_add_entry_by_txt(name, 'CN', cMBSTRING_UTF8,
          PByte(PAnsiChar(Utf8String(ADomain))), -1, -1, 0);
        X509_REQ_set_pubkey(req, pkey);

        //subjectAltName = DNS:<domain>
        //stored as request attribute via X509_REQ_add_extensions_nid
        //(X509_add_ext must not be used on an X509_REQ)
        sk := OPENSSL_sk_new_null();
        try
{$ifdef OPENSSL3}
          ext := X509V3_EXT_nconf(nil, nil, OBJ_nid2sn(NID_subject_alt_name),
            PUTF8Char(Utf8String('DNS:' + ADomain)));
{$else}
          ext := X509V3_EXT_conf_nid(nil, nil, NID_subject_alt_name,
            PAnsiChar(Utf8String('DNS:' + ADomain)));
{$endif}
          if ext <> nil then
          try
            OPENSSL_sk_push(sk, ext);
            X509_REQ_add_extensions_nid(req, sk, NID_subject_alt_name);
          finally
            X509_EXTENSION_free(ext); //sk owns nothing; extension was copied (i2d) into the attribute
          end;
        finally
          OPENSSL_sk_free(sk);
        end;

        X509_REQ_sign(req, pkey, EVP_sha256());

        bio := BIO_new(BIO_s_mem());
        try
          PEM_write_bio_X509_REQ(bio, req);
          s := '';
          FillChar(buf, SizeOf(buf), 0);
          repeat
            n := BIO_read(bio, buf[0], SizeOf(buf));
            if n > 0 then
              s := s + Copy(buf, 1, n);
          until n <= 0;
          ACSRPem := string(s);
        finally
          BIO_free(bio);
        end;
      finally
        X509_REQ_free(req);
      end;
    finally
      if pkey <> nil then
        EVP_PKEY_free(pkey)
{$ifndef OPENSSL3}
      else if rsa <> nil then
        RSA_free(rsa)
{$endif}
      ;
    end;
  end;

  function ReadResponseBody: utf8string;
  var
    m: TMemoryStream;
  begin
    m := TMemoryStream.Create;
    try
      aHttpClient.ReceiveStream(m);
      Result := UTF8StringOf(m.Memory, m.Size);
    finally
      m.Free;
    end;
  end;

  //Get a fresh nonce from the newNonce endpoint (GET is allowed there too)
  procedure AcquireNonce;
  begin
    Log('get new nonce');
    aHttpClient.Reopen(aNewNonceURL);
    try
      aNonce := aHttpClient.Response.Header['Replay-Nonce'];
    finally
      aHttpClient.Disconnect;
    end;
    if aNonce = '' then
      raise Exception.Create('ACME: cannot get Replay-Nonce');
  end;

  //Build JWS (RFC 7515) for the payload and post it to AURL
  //returns response Location header (account kid / order url), fills ABody
  function JwsPost(const AURL, APayloadJSON: string; out ABody: string): string;
  var
    aProtected, aPayloadB64, aSigningInput: UTF8String;
    aDigest, aSig: TBytes;
    aRequest: string;
    aRequestUTF8: UTF8String;

    function JsonEscape(const S: string): string;
    begin
      Result := StringReplace(StringReplace(S, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]);
    end;

  begin
    Result := '';
    if aNonce = '' then
      AcquireNonce;

    aPayloadB64 := Utf8String(StrToB64Url(Utf8String(APayloadJSON)));

    if aKid = '' then
      aProtected := Utf8String('{"alg":"RS256","jwk":' + aJwk + ',"nonce":"' + aNonce + '","url":"' + JsonEscape(AURL) + '"}')
    else
      aProtected := Utf8String('{"alg":"RS256","kid":"' + aKid + '","nonce":"' + aNonce + '","url":"' + JsonEscape(AURL) + '"}');

    aSigningInput := Utf8String(StrToB64Url(aProtected)) + '.' + aPayloadB64;
    aDigest := StrSHA256(aSigningInput);
    aSig := RSASignSHA256(aKeyPKey, aDigest);

    aRequest := '{"protected":"' + StrToB64Url(aProtected) + '","payload":"' + string(aPayloadB64)
      + '","signature":"' + BinToB64Url(@aSig[0], Length(aSig)) + '"}';


    aRequestUTF8 := Utf8String(aRequest);
    Log('POST ' + AURL);
    Log('Request ' + aRequest);

    aHttpClient.Reconnect(AURL);
    try
      aHttpClient.Request.PutHeader('Content-Type', 'application/jose+json');
      aHttpClient.Post(PByte(aRequestUTF8), Length(aRequestUTF8));
      ABody := ReadResponseBody;
      Result := aHttpClient.Response.Header['Location'];
      aNonce := ''; //each nonce is single use
      Log('Head:' + aHttpClient.Response.Head);
      Log('Header:' + aHttpClient.Response.Header.ToString);
      Log('Response:' + ABody);

    finally
      aHttpClient.Disconnect;
    end;

    RaiseAcmeError(ABody, AURL);
  end;

  //POST-as-GET with kid account (empty payload)
  function PostAsGet(const AURL: string; out ABody: string): string;
  begin
    Result := JwsPost(AURL, '', ABody);
  end;

  function PollUntil(const AURL: string; const AStatus: string; ATimeoutSec: Integer): string;
  var
    i: Integer;
    aBody: string;
    aObj: TDON_Pair;
  begin
    Result := '';
    for i := 1 to ATimeoutSec do
    begin
      PostAsGet(AURL, aBody);
      aObj := JsonParseString(aBody, [jsoSafe]);
      if aObj = nil then
        raise Exception.Create('ACME: invalid response from ' + AURL + ': ' + Copy(aBody, 1, 500));
      try
        Result := DonStr(aObj, 'status');
      finally
        aObj.Free;
      end;
      if SameText(Result, AStatus) then
        Exit;
      if SameText(Result, 'invalid') then
        raise Exception.Create('ACME: authorization/order is invalid: ' + Copy(aBody, 1, 1000));
      Log('status=' + Result + ', waiting... (' + IntToStr(i) + ')');
      Sleep(1000);
    end;
    raise Exception.Create('ACME: timeout waiting for "' + AStatus + '" on ' + AURL);
  end;

var
  aDirectoryBody, aBody, aToken, aChallengeURL, aKeyAuthz: string;
  aObj: TDON_Pair;
  aAuthzArr: TDON_Array;
  aCSRPEM, aCSRDers, aCSRBody, aCertPem, aTokenFile: string;

begin
  InitOpenSSL(True);

  ForceDirectories(AChallengeDir);

  //load saved account kid
  aKid := '';
  if (AAccountKidFile <> '') and FileExists(AAccountKidFile) then
    with TStringList.Create do
    try
      LoadFromFile(AAccountKidFile);
      if Count > 0 then
        aKid := Trim(Strings[0]);
    finally
      Free;
    end;

  LoadOrCreateAccountKey;

  aHttpClient := TmnHttpClient.Create;
  try
    //1. directory
    Log('get directory ' + ADirectoryURL);
    aHttpClient.GetString(ADirectoryURL, aDirectoryBody);
    RaiseAcmeError(aDirectoryBody, 'directory');

    aObj := JsonParseString(aDirectoryBody, [jsoSafe]);
    if aObj = nil then
      raise Exception.Create('ACME: invalid directory response');
    try
      aNewNonceURL := DonStr(aObj, 'newNonce');
      aNewAccountURL := DonStr(aObj, 'newAccount');
      aNewOrderURL := DonStr(aObj, 'newOrder');
    finally
      aObj.Free;
    end;

    if (aNewNonceURL = '') or (aNewAccountURL = '') or (aNewOrderURL = '') then
      raise Exception.Create('ACME: invalid directory response');

    AcquireNonce;

    //2. new account (only first time)
    if aKid = '' then
    begin
      Log('registering account for ' + AEmail);
      aBody := '{"termsOfServiceAgreed":true,"contact":["mailto:' + AEmail + '"]}';
      aKid := JwsPost(aNewAccountURL, aBody, aDirectoryBody);
      if aKid = '' then
        raise Exception.Create('ACME: no account Location returned');
      Log('account created: ' + aKid);
      if AAccountKidFile <> '' then
        SaveFileBytes(TEncoding.UTF8.GetBytes(aKid), AAccountKidFile);
    end;

    //3. new order
    Log('create order for ' + ADomain);
    aBody := '{"identifiers":[{"type":"dns","value":"' + ADomain + '"}]}';
    aOrderURL := JwsPost(aNewOrderURL, aBody, aDirectoryBody);
    if aOrderURL = '' then
      raise Exception.Create('ACME: no order Location returned');

    aObj := JsonParseString(aDirectoryBody, [jsoSafe]);
    if aObj = nil then
      raise Exception.Create('ACME: invalid order response');
    try
      aFinalizeURL := DonStr(aObj, 'finalize');
      if DonGet(aObj, 'authorizations') is TDON_Array then
        aAuthzArr := TDON_Array(DonGet(aObj, 'authorizations'))
      else
        aAuthzArr := nil;
      if (aFinalizeURL = '') or (aAuthzArr = nil) or (aAuthzArr.Count = 0) then
        raise Exception.Create('ACME: order has no finalize/authorizations');
      aAuthzURL := DonValStr(aAuthzArr[0]);
    finally
      aObj.Free;
    end;

    //4. authorization + http-01 challenge
    PostAsGet(aAuthzURL, aBody);
    aObj := JsonParseString(aBody, [jsoSafe]);
    if aObj = nil then
      raise Exception.Create('ACME: invalid authorization response');
    try
      if not DonFindChallenge(aObj, 'http-01', aToken, aChallengeURL) then
        raise Exception.Create('ACME: http-01 challenge not found');
    finally
      aObj.Free;
    end;

    aKeyAuthz := aToken + '.' + aThumbprint;
    aTokenFile := IncludePathDelimiter(AChallengeDir) + 'acme-challenge' + PathDelim + aToken;
    SaveFileBytes(TEncoding.UTF8.GetBytes(aKeyAuthz), aTokenFile);

    Log('challenge file saved: ' + aTokenFile);
    Log('make sure it is served at http://' + ADomain + '/.well-known/acme-challenge/' + aToken);

    //5. accept challenge and wait for valid authorization
    JwsPost(aChallengeURL, '{}', aBody);
    PollUntil(aAuthzURL, 'valid', 60);

    //6. finalize the order with CSR
    MakeCertificateKeyAndCSR(APrivateKeyFile, aCSRPEM);
    aCSRDers := StrToB64Url(BytesToUTF8(B64UrlDecode(ExtractPemBody(aCSRPEM))));
    aCSRBody := '{"csr":"' + aCSRDers + '"}';
    JwsPost(aFinalizeURL, aCSRBody, aBody);

    PollUntil(aOrderURL, 'valid', 60);

    //7. download certificate chain
    PostAsGet(aOrderURL, aBody);
    aObj := JsonParseString(aBody, [jsoSafe]);
    if aObj = nil then
      raise Exception.Create('ACME: invalid order response');
    try
      aCertURL := DonStr(aObj, 'certificate');
    finally
      aObj.Free;
    end;
    if aCertURL = '' then
      raise Exception.Create('ACME: no certificate url in order');

    Log('downloading certificate');
    PostAsGet(aCertURL, aCertPem);
    RaiseAcmeError(aCertPem, 'certificate');

    ForceDirectories(ExtractFilePath(ACertificateFile));
    SaveFileBytes(TEncoding.UTF8.GetBytes(aCertPem), ACertificateFile);

    DeleteFile(aTokenFile);

    Log('certificate saved: ' + ACertificateFile + ' (expires ' +
      FormatDateTime('yyyy-mm-dd', CertExpiryDate(ACertificateFile)) + ')');
  finally
    aHttpClient.Free;
    if aKeyPKey <> nil then
      EVP_PKEY_free(aKeyPKey); //frees the account RSA too
  end;
end;

end.
