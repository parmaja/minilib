unit mnOpenSSL;
{**
 *  This file is part of the "Mini Library"/Sockets
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *}

 {**
    openssl s_client -connect community.cloudflare.com:443 -nextprotoneg ''
    curl -v --http1.1 -A "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/81.0" "https://www.hepsiburada.com/" > 1.txt
    curl -v --http1.1 -A "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/81.0" "https://community.cloudflare.com" > 1.txt

    //failed with 403
    curl -v --http1.1 --no-alpn -A "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/81.0" "https://community.cloudflare.com" > 1.txt
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$ifdef MSWINDOWS}{$ifdef FPC}
  JwaWinCrypt, JwaWinType,
  {$endif}{$endif}
  mnLogs, mnLibraries,
  mnOpenSSLAPI;

type

  { EmnOpenSSLException }

  EmnOpenSSLException = class(Exception)
  public
    constructor CreateLastError(const msg : string); overload;
  end;

  { TOpenSSLObject }

  TsslError = (seSuccess, seTimeout, seClosed, seInvalid);

  TOpenSSLObject = class abstract(TObject)
  public
    constructor Create;
  end;

  { TSSLMethod }

  TSSLMethod = class abstract(TOpenSSLObject)
  protected
    Handle: PSSL_METHOD;
    procedure CreateHandle; virtual;abstract;
  public
    constructor Create; virtual;
  end;

  TSSLMethodClass = class of TSSLMethod;

  { TTLS_SSLMethod }

  TTLS_SSLMethod = class(TSSLMethod)
  protected
    procedure CreateHandle; override;
  end;

  TTLS_SSLClientMethod = class(TSSLMethod)
  protected
    procedure CreateHandle; override;
  end;

  { TTLS_SSLServerMethod }

  TTLS_SSLServerMethod = class(TSSLMethod)
  protected
    procedure CreateHandle; override;
  end;

  { TCTX }

  TContextOptions = set of (
    coServer,
    coNoCompressing,
    coALPN,
    coALPNHttp2,
    coDebug
  );

  { TContext }

  TContext = class(TOpenSSLObject)
  private
  protected
    Handle: PSSL_CTX;
    FMethod: TSSLMethod;
    FOwnMethod: Boolean; //created internally
    FPrivateKey: PEVP_PKEY;
    FCertificate: PX509;
  public
    constructor Create(AMethod: TSSLMethod; Options: TContextOptions = [coNoCompressing]); overload;
    constructor Create(AMethodClass: TSSLMethodClass; Options: TContextOptions = []); overload;
    destructor Destroy; override;
    procedure SetVerifyLocation(Location: utf8string);
    procedure SetVerifyFile(AFileName: utf8string);

    procedure LoadCertFile(FileName: utf8string);
    procedure LoadFullChainFile(CertificateFile, PrivateKeyFile: utf8string);
    //procedure LoadPrivateKeyFile(PrivateFile: utf8string);
    //procedure LoadFullCertFile(FileName: utf8string);

    procedure LoadPFXFromBio(bio: PBIO; Password: utf8string);
    procedure LoadPFXFile(FileName, Password: utf8string);
    procedure LoadPFXMemory(Data: PByte; Size: Integer; Password: utf8string);
    procedure LoadSysStore(Name: utf8string);

    procedure CheckPrivateKey;
    procedure SetVerifyNone;
    procedure SetVerifyPeer;
  end;

  TSSL = record//class(TOpenSSLObject)
  //protected
    bio_out: PBIO;
    Handle: PSSL;
    CTX: TContext;
    Connected: Boolean;
    FSocket: Integer;
    Active: Boolean;
  //public
    constructor Init(ACTX: TContext); overload;
    constructor Init(ASSL: PSSL); overload;
    procedure SetHostName(AHostName: UTF8String);
    procedure SetALPN(Alpns: TArray<string>);
    procedure Free;
    procedure ShutDown;
    procedure SetSocket(ASocket: Integer);
    function ClientHandshake: Boolean;
    function ServerHandshake: Boolean;
    procedure SetVerifyNone;
    procedure SetVerifyPeer;
    //Return False if failed
    function Read(var Buf; Size: Integer; out ReadSize: Integer): TsslError;
    function Write(const Buf; Size: Integer; out WriteSize: Integer): Boolean;
    function Pending: Boolean;
  end;

  //*****************************************************
  //                      BIO
  //*****************************************************

  //https://stackoverflow.com/questions/51672133/what-are-openssl-bios-how-do-they-work-how-are-bios-used-in-openssl

  { TBIOStream }

  TBIOStream = class abstract(TStream) //BIO stream
  protected
    Handle: PBIO;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBIO: PBIO;
    function GetSSL: TSSL;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TBIOStreamFile }

  TBIOStreamFile = class(TBIOStream)
  public
    constructor Create(AFileName: utf8string; Mode: utf8string); //read doc of BIO_new_file
  end;

  { TBIOStreamSSL }

  TBIOStreamSSL = class(TBIOStream) //Socket
  protected
    FCTX: TContext;
    CTXOwned: Boolean;
    Active: Boolean;
  public
    constructor Create(ACTX: TContext = nil);
    destructor Destroy; override;

    //Host name with port like 'example.com:80';
    procedure SetHostName(AHostName: utf8string);
    procedure SetHostPort(APort: utf8string);
    procedure SetHost(AHost, APort: utf8string);

    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;
    property CTX: TContext read FCTX;
  end;

  TBIOWriteProc = reference to procedure(bio: PBIO);

  TDirName = record
    FieldName: UTF8String;
    Value: UTF8String;
    constructor Create(AFieldName: UTF8String; AValue: UTF8String); overload;
  end;

  TDirNames = array of TDirName;

  TDirNamesHelper = record helper for TDirNames
  public
    function Add(FieldName: UTF8String; Value: UTF8String): Integer;
  end;

  TAltName = record
    NameType: Integer; //GEN_DNS, GEN_IPADD, GEN_DIRNAME...
    Value: UTF8String;
    DirNames: TDirNames;
    constructor Create(ANameType: Integer; AValue: UTF8String); overload;
    constructor Create(ANameType: Integer; ADirNames: TDirNames); overload;
  end;

  TDNInfo = record
    Cn: UTF8String;
    O: UTF8String;
    Ou: UTF8String;
    C: UTF8String;
    State: UTF8String;
    City: UTF8String;
    Email: UTF8String;
  end;

  TAltNameEntries = array of TAltName;

  TAltNameEntriesHelper = record helper for TAltNameEntries
  public
    function Add(ANameType: Integer; AValue: UTF8String): Integer; overload;
    function Add(ANameType: Integer; ADirs: TDirNames): Integer; overload;
  end;

procedure InitOpenSSL(All: Boolean = True);
procedure InitOpenSSLLibrary(All: Boolean = True);
procedure CleanupOpenSSL;

procedure RaiseLastSSLError;
procedure RaiseSSLError(Message: utf8string);

function MakeCert(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;
function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

function ECDSASign(const vData, vKey: utf8string): TBytes; overload;
function ECDSASignBase64(const vData, vKey: utf8string): UTF8String; overload;

procedure X509SaveToFile(X509: PX509; const FileName: string);

function BioBase64Encode(vBuf: PByte; vLen: Integer): UTF8String;

function BIOToString(vProc: TBIOWriteProc): UTF8String; overload;
procedure BIOToStream(vProc: TBIOWriteProc; Stream: TStream); overload;
procedure BIOToFile(vProc: TBIOWriteProc; FileName: string); overload;

function GenerateEckey(vNid: Integer): PEVP_PKEY;
function LoadEckey(FileName: string): PEVP_PKEY;

function BuildSanStack(const vAltNames: TAltNameEntries): PSTACK_OF_GENERAL_NAME;
procedure AddNameEntry(Name: PX509_NAME; const Field, Value: UTF8String); overload;
function AddSanToReq(req: PX509_REQ; const vAltNames: TAltNameEntries): Integer; overload;
//Decode openssl req -in mycsr.csr -noout -text
function CreateECCsr(const dn: TDNInfo; const vAltNames: TAltNameEntries; pkey: PEVP_PKEY): PX509_REQ;

function CsrToString(req: PX509_REQ): UTF8String;
procedure CsrToFile(req: PX509_REQ; FileName: string);
function KeyToString(pkey: PEVP_PKEY): UTF8String;
procedure KeyToFile(pkey: PEVP_PKEY; FileName: string);

const
  sUID = 'UID';
  sSN = 'SN';
  sTitle = 'title';
  sRegisteredAddress = 'registeredAddress';
  sBusinessCategory = 'businessCategory';

  sSubjectAltName = 'subjectAltName';
  sCertificateTemplateName = 'certificateTemplateName';

implementation

uses
  mnSockets;

const
  sALPNProts: UTF8String = 'http/1.1, h2';
  sHttp1_1_ALPN: string = #08'http/1.1';
  sHttp2_ALPN: string = #08'http/1.1'#2'h2';

function alpn_select_cb(ssl: PSSL; var outdata: PByte; var outlen: integer; const indata: PByte; inlen: Byte; arg: Pointer): Integer; cdecl;
var
  ret: Integer;
  b: byte; 
begin
  ret := SSL_select_next_proto(outdata, b, PUTF8Char(indata), inlen, PUTF8Char(sALPNProts), Length(sALPNProts));
  outlen := b;

  if ret <> OPENSSL_NPN_NEGOTIATED then
    Result := SSL_TLSEXT_ERR_NOACK
  else
    Result := SSL_TLSEXT_ERR_OK;
end;

procedure SSL_CTX_msg_callback(write_p: integer; version: integer; content_type: integer; buf: pointer; len: NativeUInt; ssl: PSSL; arg: pointer); cdecl;
var
  b: TBytes;
  s: string;
begin
  if Len<>0 then
  begin
    SetLength(b, len);
    Move(buf, b[0], len);
    s := TEncoding.UTF8.GetString(b); //TODO Check it in FPC
    Log.WriteLn(s);
  end;
end;

procedure debug_callback(ssl: PSSL; where: cint; ret: cint); cdecl;
var
  s: UTF8String;
begin
  s := SSL_state_string_long(ssl);
  Log.writeln(s);

  s := SSL_state_string(ssl);
  Log.writeln(s);

  case where of
    SSL_CB_ALERT: Log.writeln('SSL alert: '+ SSL_alert_type_string_long(ret)+ ':'+ SSL_alert_desc_string_long(ret)+ ':');
    SSL_CB_LOOP: Log.writeln('SSL state: '+ SSL_state_string(ssl)+ ':'+ SSL_state_string_long(ssl));
    SSL_CB_HANDSHAKE_START: Log.writeln(lglDebug, 'SSL handshake started');
    SSL_CB_HANDSHAKE_DONE: Log.writeln(lglDebug, 'SSL handshake completed');
    else
      Log.writeln(lglDebug, 'SSL alert: '+ SSL_alert_type_string_long(ret)+ ':'+ SSL_alert_desc_string_long(ret));
      //Log.writeln('where %d ret %d state:', [where, ret]);
  end;
end;

procedure X509SaveToFile(X509: PX509; const FileName: string);
var
  bio: PBIO;
begin
  bio := BIO_new_file(PUTF8Char(UTF8Encode(FileName)), PUTF8Char('wb'));
  try
    PEM_write_bio_X509(bio, X509);
  finally
    BIO_free(bio);
  end;
end;

function BioBase64Encode(vBuf: PByte; vLen: Integer): UTF8String;
var
  bio, b64: PBIO;
  aBuf: PBUF_MEM;
begin
  InitOpenSSLLibrary;

	b64 := BIO_new(BIO_f_base64());
	bio := BIO_new(BIO_s_mem());
	bio := BIO_push(b64, bio);

	BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL); //Ignore newlines - write everything in one line
	BIO_write(bio, vBuf, vLen);
	BIO_flush(bio);
  BIO_get_mem_ptr(bio, aBuf);

  Result := aBuf.data;
  SetLength(Result, aBuf.length);

	//BIO_set_close(bio, BIO_NOCLOSE);
	BIO_free_all(bio);
end;

function ECDSASign(const vData, vKey: utf8string): TBytes; overload;
var
	aKey: PEC_KEY;
  bio: PBIO;
  aLen: Integer;
begin
  InitOpenSSLLibrary;

  bio := BIO_new_mem_buf(PByte(vKey), Length(vKey));
  try
    aKey := PEM_read_bio_ECPrivateKey(bio, nil, nil, nil);
    try
      {$ifdef FPC}Result := nil;{$endif}
      aLen := ECDSA_size(aKey);
      SetLength(Result, aLen);

      ECDSA_sign(0, PByte(vData), Length(vData), PByte(Result), @aLen, aKey);
      SetLength(Result, aLen);
    finally
    end;
  finally
    BIO_free(bio);
  end;
end;

function ECDSASignBase64(const vData, vKey: utf8string): UTF8String; overload;
var
  b: TBytes;
begin
  b := ECDSASign(vData, vKey);
  Result := BioBase64Encode(PByte(b[0]), Length(b)); //TODO check warning in FPC
  //Result := tnet
end;

function BIOToString(vProc: TBIOWriteProc): UTF8String;
var
  bio: PBIO;
  b: PByte;
  aLen: NativeInt;
  a: AnsiString;
begin
  Result := '';
  bio := BIO_new(BIO_s_mem());
  if bio = nil then
    raise EmnOpenSSLException.Create('Error BIO_new mem');
  try
    vProc(bio);
    aLen := BIO_get_mem_data(bio, b);
    if (aLen <= 0) or (b = nil) then
      raise EmnOpenSSLException.Create('Error BIO_get_mem_data');
    SetLength(a, aLen);
    Move(b^, a[1], aLen);
    Result := a;
  finally
    BIO_free(bio);
  end;
end;

procedure BIOToStream(vProc: TBIOWriteProc; Stream: TStream);
var
  bio: PBIO;
  b: PByte;
  aLen: NativeInt;
begin
  bio := BIO_new(BIO_s_mem());
  if bio = nil then
    raise EmnOpenSSLException.Create('Error BIO_new mem');
  try
    vProc(bio);
    aLen := BIO_get_mem_data(bio, b);
    if (aLen <= 0) or (b = nil) then
      raise EmnOpenSSLException.Create('Error BIO_get_mem_data');
    Stream.Write(b^, aLen);
  finally
    BIO_free(bio);
  end;
end;

procedure BIOToFile(vProc: TBIOWriteProc; FileName: string);
var
  bio: PBIO;
  b: PByte;
  aLen: NativeInt;
begin
  bio := BIO_new_file(PUTF8Char(UTF8Encode(FileName)), PUTF8Char('wb'));
  if bio = nil then
    raise EmnOpenSSLException.Create('Error BIO_new_file');
  try
    vProc(bio);
  finally
    BIO_free(bio);
  end;
end;

function CsrToString(req: PX509_REQ): UTF8String;
begin
  Result := BIOToString(procedure(bio: PBIO)
  begin
    PEM_write_bio_X509_REQ(bio, req);
  end);
end;

procedure CsrToFile(req: PX509_REQ; FileName: string);
begin
  BIOToFile(procedure(bio: PBIO)
  begin
    PEM_write_bio_X509_REQ(bio, req);
  end, FileName);
end;

function KeyToString(pkey: PEVP_PKEY): UTF8String;
begin
  Result := BIOToString(procedure(bio: PBIO)
  begin
    PEM_write_bio_PrivateKey(bio, pkey, nil, nil, 0, nil, nil);
  end);
end;

procedure KeyToFile(pkey: PEVP_PKEY; FileName: string);
begin
  BIOToFile(procedure(bio: PBIO)
  begin
    PEM_write_bio_PrivateKey(bio, pkey, nil, nil, 0, nil, nil);
  end, FileName);
end;

function CreateECCsr(const dn: TDNInfo; const vAltNames: TAltNameEntries; pkey: PEVP_PKEY): PX509_REQ;
var
  name: PX509_NAME;
begin
  Result := X509_REQ_new();
  if Result = nil then
    raise Exception.Create('X509_REQ_new failed');

  try
    X509_REQ_set_version(Result, 0);

    name := X509_REQ_get_subject_name(Result);
    AddNameEntry(name, 'CN', dn.Cn);
    AddNameEntry(name, 'O', dn.O);
    AddNameEntry(name, 'OU', dn.Ou);
    AddNameEntry(name, 'C', dn.C);
    AddNameEntry(name, 'ST', dn.State);
    AddNameEntry(name, 'L', dn.City);
    AddNameEntry(name, 'emailAddress', dn.Email);

    if Length(vAltNames) > 0 then
    begin
      if AddSanToReq(Result, vAltNames) = 0 then
        raise Exception.Create('AddSanToReq failed');
    end;

    if X509_REQ_set_pubkey(Result, pkey) <> 1 then
      raise Exception.Create('X509_REQ_set_pubkey failed');

    if X509_REQ_sign(Result, pkey, EVP_sha256()) = 0 then
      raise Exception.Create('X509_REQ_sign failed');
  except
    X509_REQ_free(Result);
    Result := nil;
    raise;
  end;
end;

procedure AddNameEntry(Name: PX509_NAME; const Field, Value: UTF8String);
begin
  if Name = nil then
    Exit;
  if Value = '' then
    Exit;

  if X509_NAME_add_entry_by_txt(name, PUtf8Char(Field), MBSTRING_UTF8,
      PByte(Value), -1, -1, 0) <> 1 then
    raise Exception.CreateFmt('X509_NAME_add_entry_by_txt failed for %s', [UTF8String(Field)]);
end;

function GenerateEckey(vNid: Integer): PEVP_PKEY;
var
  ec: PEC_KEY;
begin
  Result := EVP_PKEY_new();
  if Result = nil then
    raise EmnOpenSSLException.Create('Error EVP_PKEY_new');

  ec := EC_KEY_new_by_curve_name(vNid);
  if ec = nil then
  begin
    EVP_PKEY_free(Result);
    Result := nil;
    raise EmnOpenSSLException.Create('Error EC_KEY_new_by_curve_name');
  end;

  if EC_KEY_generate_key(ec) = 0 then
  begin
    EC_KEY_free(ec);
    EVP_PKEY_free(Result);
    Result := nil;
    raise EmnOpenSSLException.Create('Error EC_KEY_generate_key');
  end;

  if EVP_PKEY_assign_EC_KEY(Result, ec) = 0 then
  begin
    EC_KEY_free(ec);
    EVP_PKEY_free(Result);
    Result := nil;
    raise EmnOpenSSLException.Create('Error EVP_PKEY_assign_EC_KEY');
  end;
end;

function LoadEckey(FileName: string): PEVP_PKEY;
begin
//TODO
end;

function BuildSanStack(const vAltNames: TAltNameEntries): PSTACK_OF_GENERAL_NAME;
var
  g: PGENERAL_NAME;
  ia5: PASN1_IA5STRING;
  dirName: PX509_NAME;
  i, j: Integer;
  ok: Boolean;
  v: UTF8String;
begin
  Result := nil;
  if Length(vAltNames) = 0 then
    Exit;

  Result := sk_GENERAL_NAME_new_null();
  if Result = nil then
    raise EmnOpenSSLException.Create('Error sk_GENERAL_NAME_new_null');

  for i := Low(vAltNames) to High(vAltNames) do
  begin
    g := GENERAL_NAME_new();
    if g = nil then
    begin
      Log.writeln('Error GENERAL_NAME_new');
      Continue;
    end;

    if vAltNames[i].NameType = GEN_DIRNAME then
    begin
      dirName := X509_NAME_new();
      if dirName = nil then
      begin
        GENERAL_NAME_free(g);
        Continue;
      end;

      ok := True;
      for j := Low(vAltNames[i].DirNames) to High(vAltNames[i].DirNames) do
      begin
        if X509_NAME_add_entry_by_txt(dirName,
            PUTF8Char(vAltNames[i].DirNames[j].FieldName), MBSTRING_UTF8,
            PByte(vAltNames[i].DirNames[j].Value), -1, -1, 0) <> 1 then
        begin
          ok := False;
          Break;
        end;
      end;

      if not ok then
      begin
        X509_NAME_free(dirName);
        GENERAL_NAME_free(g);
        Continue;
      end;

      //Ownership of dirName is transferred to g
      GENERAL_NAME_set0_value(g, GEN_DIRNAME, Pointer(dirName));
    end
    else
    begin
      v := vAltNames[i].Value;
      if v = '' then
      begin
        GENERAL_NAME_free(g);
        Continue;
      end;

      ia5 := ASN1_IA5STRING_new();
      if ia5 = nil then
      begin
        GENERAL_NAME_free(g);
        Continue;
      end;

      if ASN1_STRING_set(ia5, PByte(v), Length(v)) <> 1 then
      begin
        ASN1_IA5STRING_free(ia5);
        GENERAL_NAME_free(g);
        Continue;
      end;

      GENERAL_NAME_set0_value(g, vAltNames[i].NameType, Pointer(ia5));
    end;

    if sk_GENERAL_NAME_push(Result, g) = 0 then
      GENERAL_NAME_free(g);
  end;
end;

//Returns 1 on success, 0 on failure
function AddSanToReq(req: PX509_REQ; const vAltNames: TAltNameEntries): Integer;
var
  gens: PSTACK_OF_GENERAL_NAME;
  extStack: Pstack_st_X509_EXTENSION;
begin
  Result := 0;
  if req = nil then
    Exit;

  gens := BuildSanStack(vAltNames);
  if gens = nil then
    Exit;

  try
    extStack := nil;

    if X509V3_add1_i2d(Pstack_st_X509_EXTENSION(@extStack), NID_subject_alt_name,
        Pointer(gens), 0, X509V3_ADD_APPEND) <> 1 then
      raise EmnOpenSSLException.Create('Error X509V3_add1_i2d failed for SAN');

    if X509_REQ_add_extensions(req, extStack) <> 1 then
      raise EmnOpenSSLException.Create('Error X509_REQ_add_extensions');

    Result := 1;
  finally
    sk_GENERAL_NAME_pop_free(gens, Pointer(@GENERAL_NAME_free));
    if extStack <> nil then
      sk_X509_EXTENSION_pop_free(extStack, Pointer(@X509_EXTENSION_free));
  end;
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
  sign: Integer;
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
    AddExt(x, NID_key_usage, PUTF8Char(UTF8String('critical,cRLSign,digitalSignature,keyCertSign'))); //Self-Signed
    AddExt(x, NID_subject_key_identifier, 'hash');

    sign := X509_sign(x, pk, EVP_sha256());
    if (sign = 0) then
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

procedure InitOpenSSL(All: Boolean);
begin
  InitOpenSSLLibrary(All);
end;

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
  if OpenSSLLib.Load=lsInit then
  begin
    if All then
      OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS, nil)
    else
      OPENSSL_init_ssl(0, nil);
    //ERR_load_SSL_strings();//IDK

    if CryptoLib.Load=lsInit then
    begin
      if All then
        OpenSSL_add_all_algorithms
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



{ TTLS_SSLServerMethod }

procedure TTLS_SSLServerMethod.CreateHandle;
begin
  Handle := TLS_server_method();
end;

{ TBIOStreamSSL }

function TBIOStreamSSL.Connected: Boolean;
begin
  Result := Active;
end;

constructor TBIOStreamSSL.Create(ACTX: TContext);
begin
  inherited Create;
  if (ACTX = nil) then
  begin
    FCTX := TContext.Create(TTLS_SSLClientMethod, [coNoCompressing, coALPN{, coALPNHttp2}]);
    CTXOwned := True;
  end
  else
    FCTX := ACTX;

  Handle := BIO_new_ssl_connect(CTX.Handle);
end;

destructor TBIOStreamSSL.Destroy;
begin
  if CTXOwned then
    FreeAndNil(FCTX);
  inherited;
end;

procedure TBIOStreamSSL.Disconnect;
begin
  Active := False;
end;

procedure TBIOStreamSSL.SetHost(AHost, APort: utf8string);
begin
  SetHostName(AHost);
  SetHostPort(APort);
end;

procedure TBIOStreamSSL.SetHostName(AHostName: utf8string);
begin
  BIO_set_conn_hostname(Handle, PUTF8Char(AHostName)); //Always return 1
end;

procedure TBIOStreamSSL.SetHostPort(APort: utf8string);
begin
  BIO_set_conn_port(Handle, PUTF8Char(APort)); //Always return 1
end;

procedure TBIOStreamSSL.Connect;
var
  res: Integer;
begin
  res := BIO_do_connect(Handle);
  if res <> 1 then
    RaiseLastSSLError;
  res := BIO_do_handshake(Handle);
  if res <> 1 then
    RaiseLastSSLError;
  Active := True;
end;

{ TBIOStreamFile }

constructor TBIOStreamFile.Create(AFileName: utf8string; Mode: utf8string);
begin
  inherited Create;
  Handle := BIO_new_file(PUTF8Char(AFileName), PUTF8Char(Mode));
end;

{ EmnOpenSSLException }

function CollectOpenSSLErrors: string;
var
  e: NativeUInt;
  buf: array[0..255] of AnsiChar;
begin
  Result := '';
  e := ERR_get_error();
  while e <> 0 do
  begin
    ERR_error_string_n(e, @buf, SizeOf(buf));
    Result := Result + #13#10 + buf;
    e := ERR_get_error();
  end;
end;

constructor EmnOpenSSLException.CreateLastError(const msg: string);
var
  s: string;
  e: NativeUInt;
  buf: array[0..255] of AnsiChar;
begin
  //s := ERR_error_string(ERR_peek_error, nil);
  s := CollectOpenSSLErrors;
  raise EmnOpenSSLException.CreateFmt(msg + ' [%s]', [s]);
end;

{ TOpenSSLObject }

constructor TOpenSSLObject.Create;
begin
  inherited Create;
  InitOpenSSL;
end;

{ TBIOStream }

constructor TBIOStream.Create;
begin
  inherited Create;
  InitOpenSSL;
end;

destructor TBIOStream.Destroy;
begin
  inherited Destroy;
  if Handle <> nil then
    BIO_free(Handle);
end;

function TBIOStream.GetBIO: PBIO;
begin
  Result := Handle;
end;

function TBIOStream.GetSSL: TSSL;
var
  ssl: PSSL;
begin
  BIO_get_ssl(Handle, ssl);
  Result := TSSL.Init(ssl);
end;

function TBIOStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := BIO_read(Handle, Buffer, Count);
end;

function TBIOStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := BIO_write(Handle, Buffer, Count);
end;

{ TSSL }

constructor TSSL.Init(ACTX: TContext);
begin
  //inherited Create;
  {$ifdef FPC}
  Initialize(Self);
  {$endif}
  CTX := ACTX;
  //SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);

  Handle := SSL_new(CTX.Handle);

  {$ifdef DEBUG}
  //Log.WriteLn(SSL_get_version(Handle));
  //SSL_ctrl(Handle, SSL_CTRL_SET_DEBUG_LEVEL, SSL_DEBUG_CONNECT or SSL_DEBUG_HANDSHAKE or SSL_DEBUG_ERROR, nil);
  //bio_out := BIO_new_file('xdebugx.log', 'w');
  //SSL_set_bio(Handle, nil, bio_out);
  {$endif}
  Active := True;
end;

procedure SSL_msg_callback(write_p: integer; version: integer; content_type: integer; buf: pointer; len: Cardinal; ssl: PSSL; arg: pointer); cdecl;
var
  b: TBytes;
  s: string;
begin
  if Len<>0 then
  begin
    SetLength(b, len);
    Move(buf, b[0], len);
    s := TEncoding.UTF8.GetString(b);
    Log.WriteLn(s);
  end;
end;


constructor TSSL.Init(ASSL: PSSL);
begin
  //inherited Create;
  Handle := ASSL;
  Active := True;
  {$ifopt D+}
  //SSL_set_msg_callback(ASSL, SSL_msg_callback);
  {$endif}
end;

procedure TSSL.SetALPN(Alpns: TArray<string>);
begin
//  SSL_CTX_set_alpn_protos(CTX.Handle, PUTF8Char(sHttp1_1_ALPN), Length(sHttp1_1_ALPN));
end;

procedure TSSL.SetHostName(AHostName: UTF8String);
begin
  //when connect error to cloudflare site https://www.discogs.com/forum/thread/861907
  //s := 'community.cloudflare.com';
  //* thanks to "p.sanders" https://www.discogs.com/forum/thread/861907#8612173
  SSL_set_tlsext_host_name(Handle, PUTF8Char(AHostName));
end;

procedure TSSL.ShutDown;
begin
  if Active and (Handle <> nil) then
  begin
    SSL_shutdown(Handle);
  end;
  Active := False;
end;

procedure TSSL.Free;
begin

  if Handle <> nil then
  begin
    SSL_free(Handle);
    Handle := nil;
  end;
  //BIO_free_all(bio_out);
  Active := False;
end;

procedure TSSL.SetSocket(ASocket: Integer);
var
  ret, err: Integer;
begin
  FSocket := ASocket;
  ret := SSL_set_fd(Handle, FSocket);
  if ret <= 0  then
  begin
    err := SSL_get_error(Handle, ret);
    Log.WriteLn(lglDebug, 'ServerHandshake: ' + ERR_error_string(err, nil));
  end
end;

function TSSL.ClientHandshake: Boolean;
var
  ret: Integer;
  s: string;
begin
  ret := SSL_connect(Handle);

  if ret < 0  then
  begin
    Result := False;
    s := ERR_error_string(ERR_get_error(), nil);
    Log.WriteLn(lglDebug, 'Connect: ' + s);
  end
  else if ret = 0 then //error
    Result := False
  else
    Result := True;
  //Log.WriteLn(lglInfo, 'version: ' + SSL_get_version(Handle));
end;

function TSSL.ServerHandshake: Boolean;
var
  ret, err: Integer;
begin
  ret := SSL_accept(Handle);
  if ret <= 0  then
  begin
    err := SSL_get_error(Handle, ret);
    Log.WriteLn(lglDebug, 'ServerHandshake: ' + ERR_error_string(err, nil));
    Result := False;
  end
  else
    Result := True;
end;

procedure TSSL.SetVerifyNone;
begin
  SSL_set_verify(Handle, SSL_VERIFY_NONE, nil);
end;

procedure TSSL.SetVerifyPeer;
begin
  SSL_set_verify(Handle, SSL_VERIFY_PEER, nil);
end;

function TSSL.Read(var Buf; Size: Integer; out ReadSize: Integer): TsslError;
var
  err, errno: Integer;
  errStr: PUTF8Char;
begin
  if not Active then
    raise EmnOpenSSLException.Create('SSL object is not Active');

  ReadSize := SSL_read(Handle, Buf, Size);
  if ReadSize <= 0  then
  begin
    err := SSL_get_error(Handle, ReadSize);
    errno := WallSocket.GetSocketError(FSocket);

    case err of
      SSL_ERROR_ZERO_RETURN:
        Result := seClosed;
      SSL_ERROR_SYSCALL:
        begin
          errStr := ERR_error_string(err, nil);
          Log.WriteLn(lglInfo, 'Read: ' + string(errStr) + ', Socket Error: ' + IntToStr(errno));
          Result := seInvalid;
        end;
    else
      errStr := ERR_error_string(err, nil);
      Log.WriteLn('Read: ' + string(errStr) + ', Socket Error: ' + IntToStr(errno));
      Result := seInvalid;
    end;

    ReadSize := 0;
  end
  else
    Result := seSuccess;
end;

function TSSL.Write(const Buf; Size: Integer; out WriteSize: Integer): Boolean;
var
  err: Integer;
begin
  if not Active then
    raise EmnOpenSSLException.Create('SSL object is not Active');
  WriteSize := SSL_write(Handle, Buf, Size);
  if WriteSize <= 0  then
  begin
    err := SSL_get_error(Handle, WriteSize);
    Log.WriteLn('Write: ' + ERR_error_string(err, nil));
    Result := False;
  end
  else
    Result := True;
end;

function TSSL.Pending: Boolean;
begin
  Result := SSL_has_pending(Handle) > 0; //SSL_has_pending no SSL_pending, SSL_pending check for processed data only
end;

{ TSSLMethod }

constructor TSSLMethod.Create;
begin
  inherited Create;
  CreateHandle;
  if Handle = nil then
    raise EmnOpenSSLException.Create('Can not create Method: ' + ClassName);
end;

{ TTLS_SSLMethod }

procedure TTLS_SSLMethod.CreateHandle;
begin
  Handle := TLS_method();
end;

{ TContext }

procedure TContext.LoadPFXFromBio(bio: PBIO; Password: utf8string);
var
  p12: PKCS12;
  cert: PX509;
  chain: POPENSSL_STACK;
  c, i: Integer;
  pPassword: Pointer;
begin
  p12 := d2i_PKCS12_bio(bio, nil);
  if p12 = nil then
    raise EmnOpenSSLException.CreateLastError('Fail to load pfx certificate');
  try
    EVP_PKEY_free(FPrivateKey);
    X509_free(FCertificate);

    chain := OPENSSL_sk_new_null;
    try
      if Password = '' then
        pPassword := nil
      else
        pPassword := PUTF8Char(Password);
      if PKCS12_parse(p12, pPassword, FPrivateKey, FCertificate, chain) <=0 then
        raise EmnOpenSSLException.CreateLastError('Error PKCS12_parse, maybe the pfx password');

      if SSL_CTX_use_PrivateKey(Handle, FPrivateKey) <= 0 then
        raise EmnOpenSSLException.Create('fail to load private key');

      if (SSL_CTX_use_certificate(Handle, FCertificate) <= 0) then
         raise EmnOpenSSLException.CreateLastError('Error SSL_CTX_use_certificate');

      c := sk_X509_num(chain);
      for  i := 0 to c-1 do
      begin
          cert := sk_X509_value(chain, i);
          if SSL_CTX_add_extra_chain_cert(Handle, cert) <=0 then
            raise EmnOpenSSLException.CreateLastError('Error SSL_CTX_add_extra_chain_cert');
      end;

    finally
      OPENSSL_sk_free(chain);
    end;
  finally
		PKCS12_free(p12);
  end;
  CheckPrivateKey;
end;

constructor TContext.Create(AMethod: TSSLMethod; Options: TContextOptions);
var
  o: Cardinal;
  ret , err: Integer;
begin
  inherited Create;
  FMethod := AMethod;
  //SSL_library_init
  Handle := SSL_CTX_new(AMethod.Handle);
  if Handle = nil then
    EmnOpenSSLException.Create('Can not create CTX handle');

  {$ifopt D+}
  if coDebug in Options then
  begin
    SSL_CTX_set_msg_callback(Handle, SSL_CTX_msg_callback);
    SSL_CTX_set_info_callback(Handle, debug_callback);
  end;
  {$endif}

  //SSL_CTX_set_min_proto_version(Handle, TLS1_3_VERSION);
  //SSL_CTX_set_max_proto_version(Handle, TLS1_3_VERSION);

  if coALPN in Options then
  begin
    //SSL_CTX_set_alpn_select_cb(Handle, alpn_select_cb, nil);
    if coALPNHttp2 in Options then
      ret := SSL_CTX_set_alpn_protos(Handle, PUTF8Char(utf8string(sHttp2_ALPN)), Length(sHttp2_ALPN))
    else
      ret := SSL_CTX_set_alpn_protos(Handle, PUTF8Char(utf8string(sHttp1_1_ALPN)), Length(sHttp1_1_ALPN));
    if ret <> 0  then
    begin
      err := SSL_get_error(Handle, ret);
      Log.WriteLn('Set Alpn: ' + ERR_error_string(err, nil));
    end;
  end;

  //o := SSL_OP_ALL or SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3 or SSL_OP_SINGLE_DH_USE or SSL_OP_SINGLE_ECDH_USE or SSL_OP_CIPHER_SERVER_PREFERENCE;
  o := SSL_OP_ALL or SSL_OP_SINGLE_DH_USE or SSL_OP_SINGLE_ECDH_USE;

  o := o or SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION;

  if coNoCompressing in Options then
    o := o or SSL_OP_NO_COMPRESSION;

  if coServer in Options then
    o := o or SSL_OP_CIPHER_SERVER_PREFERENCE;

  o := o or SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3;

  SSL_CTX_set_options(Handle, o);

  //SSL_CTX_set_cipher_list(Handle, 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256');
end;

constructor TContext.Create(AMethodClass: TSSLMethodClass; Options: TContextOptions);
begin
  Create(AMethodClass.Create, Options);
  FOwnMethod := True;
end;

destructor TContext.Destroy;
begin
  EVP_PKEY_free(FPrivateKey);
  X509_free(FCertificate);
  if FOwnMethod then
    FMethod.Free;
  if Handle <> nil then
    SSL_CTX_free(Handle);
  inherited Destroy;
end;

procedure TContext.LoadCertFile(FileName: utf8string);
begin
  if not FileExists(FileName) then
    raise EmnOpenSSLException.CreateLastError('Certificate file not exist:' + FileName);
  if SSL_CTX_use_certificate_file(Handle, PUTF8Char(FileName), SSL_FILETYPE_PEM) <= 0 then
    raise EmnOpenSSLException.CreateLastError('fail to load certificate');
end;

procedure TContext.LoadFullChainFile(CertificateFile, PrivateKeyFile: utf8string
  );
begin
  //LoadSysStore('MY');
  if not FileExists(CertificateFile) then
    raise EmnOpenSSLException.CreateLastError('Full chain certificate file not exist:' + CertificateFile);

  if SSL_CTX_use_certificate_chain_file(Handle, PUTF8Char(CertificateFile)) <= 0 then
    raise EmnOpenSSLException.CreateLastError('fail to load full chain certificate');

  if not FileExists(PrivateKeyFile) then
    raise EmnOpenSSLException.CreateLastError('Private key file not exist:' + PrivateKeyFile);
  if SSL_CTX_use_PrivateKey_file(Handle, PUTF8Char(PrivateKeyFile), SSL_FILETYPE_PEM) <= 0 then
    raise EmnOpenSSLException.Create('fail to load private key');

  CheckPrivateKey;
end;

//https://stackoverflow.com/questions/6371775/how-to-load-a-pkcs12-file-in-openssl-programmatically
// https://stackoverflow.com/questions/43119053/does-ssl-ctx-use-certificate-copy-used-certificate-bytes
{  Or maybe use

  SSL_CTX_use_certificate_file(ctx, "cert.pfx", SSL_FILETYPE_PEM);
  SSL_CTX_use_PrivateKey_file(ctx, "cert.pfx", SSL_FILETYPE_PEM, NULL);

}

procedure TContext.LoadPFXFile(FileName, Password: utf8string);
var
  bio: PBIO;
begin
  if not FileExists(FileName) then
    raise EmnOpenSSLException.CreateLastError('PFX file not exist:' + FileName);
  bio := BIO_new_file(PUTF8Char(FileName), PUTF8Char('rb'));
  if (bio = nil) then
    raise EmnOpenSSLException.CreateLastError('Error reading file by BIO_new_file');
  try
    LoadPFXFromBio(bio, Password);
  finally
    BIO_free(bio);
  end;
end;

procedure TContext.LoadPFXMemory(Data: PByte; Size: Integer; Password: utf8string);
var
  bio: PBIO;
begin
  bio := BIO_new_mem_buf(Data, Size);
  if (bio = nil) then
    raise EmnOpenSSLException.CreateLastError('Error reading file by BIO_new_mem_buf');
  try
    LoadPFXFromBio(Bio, Password);
  finally
    BIO_free(bio);
  end;
end;

procedure TContext.LoadSysStore(Name: utf8string);
{$ifdef MSWINDOWS}
{$ifdef FPC}
  function GetCertName(CertContext: PCCERT_CONTEXT): string;
  var
    NameLen: DWORD;
  begin
    NameLen := CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, nil, 0);
    SetLength(Result, NameLen);
    CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, PChar(Result), NameLen);
    Result := TrimRight(Result);
  end;
var
  hStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT;
  CertName: string;
  CertStream: TMemoryStream;
  x: PX509;
  {hProvOrKey: HCRYPTPROV = 0;
  dwKeySpec: DWORD;
  fCallerFreeProv: BOOL;}
begin
  hStore := CertOpenStore(CERT_STORE_PROV_SYSTEM, 0, 0, CERT_STORE_READONLY_FLAG or CERT_STORE_MAXIMUM_ALLOWED_FLAG or CERT_SYSTEM_STORE_LOCAL_MACHINE, PWideChar('MY'));
  if hStore = nil then
    RaiseLastOSError;

  try
    pCertContext := nil;
    while True do
    begin
      pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
      if not Assigned(pCertContext) then
        break;

      CertName := GetCertName(pCertContext);
      if SameText(Name, CertName) then
      begin
        CertStream := TMemoryStream.Create;
        try
          x := d2i_X509(nil, @pCertContext.pbCertEncoded, pCertContext.cbCertEncoded);

          if (SSL_CTX_use_certificate(Handle, x) <= 0) then
             raise EmnOpenSSLException.CreateLastError('Error SSL_CTX_use_certificate');

          {if not CryptAcquireCertificatePrivateKey(pCertContext, CRYPT_ACQUIRE_COMPARE_KEY_FLAG, nil, hProvOrKey, @dwKeySpec, @fCallerFreeProv) then
            raise EmnOpenSSLException.CreateLastError('Error CryptAcquireCertificatePrivateKey');
            }

          // Copy raw DER cert data into string stream
          //CertStream.SetSize(pCertContext.cbCertEncoded);
          //Move(pCertContext.pbCertEncoded^, CertStream.Memory^, pCertContext.cbCertEncoded);
          //LoadPFXMemory(pCertContext^.pbCertEncoded, pCertContext.cbCertEncoded, '');
        finally
          CertStream.Free;
        end;
        break;
      end;
      Log.writeln(Format('Subject: %s', [CertName]));
    end;
  finally
    CertCloseStore(hStore, 0);
  end;
end;
{$else}
begin
  raise EmnOpenSSLException.CreateLastError('Not implemented yet');
end;
{$endif}
{$else}
begin
  raise EmnOpenSSLException.CreateLastError('Not implemented yet');
end;
{$endif}

{procedure TContext.LoadPrivateKeyFile(PrivateFile: utf8string);
begin
  if not FileExists(PrivateFile) then
    raise EmnOpenSSLException.CreateLastError('Private key file not exist:' + PrivateFile);
  if SSL_CTX_use_PrivateKey_file(Handle, PUTF8Char(PrivateFile), SSL_FILETYPE_PEM) <= 0 then
    raise EmnOpenSSLException.Create('fail to load private key');
end;}

procedure TContext.CheckPrivateKey;
begin
  if SSL_CTX_check_private_key(Handle) = 0 then
    raise EmnOpenSSLException.CreateLastError('Private key does not match the public certificate');
end;

procedure TContext.SetVerifyFile(AFileName: utf8string);
begin
  if SSL_CTX_load_verify_locations(Handle, PUTF8Char(AFileName), nil) <0 then
    raise EmnOpenSSLException.CreateLastError('SSL_CTX_load_verify_locations');
end;

procedure TContext.SetVerifyLocation(Location: utf8string);
begin
  if SSL_CTX_load_verify_locations(Handle, nil, PUTF8Char(Location)) <=0 then
    raise EmnOpenSSLException.CreateLastError('Private key does not match the public certificate');
end;

procedure TContext.SetVerifyNone;
begin
  SSL_CTX_set_verify(Handle, SSL_VERIFY_NONE, nil);
end;

procedure TContext.SetVerifyPeer;
begin
  SSL_CTX_set_verify(Handle, SSL_VERIFY_PEER, nil);
end;

{ TTLS_SSLClientMethod }

procedure TTLS_SSLClientMethod.CreateHandle;
begin
  Handle := TLS_client_method();
end;

{ TAltName }

constructor TAltName.Create(ANameType: Integer; AValue: UTF8String);
begin
  NameType:= ANameType;
  Value := AValue;
end;

constructor TAltName.Create(ANameType: Integer; ADirNames: TDirNames);
begin
  NameType:= ANameType;
  DirNames := ADirNames;
end;

{ TDirName }

constructor TDirName.Create(AFieldName, AValue: UTF8String);
begin
  FieldName := AFieldName;
  Value := AValue;
end;

{ TAltNameEntriesHelper }

function TAltNameEntriesHelper.Add(ANameType: Integer; AValue: UTF8String): Integer;
begin
  Self := Self + [TAltName.Create(ANameType, AValue)];
  Result := Length(Self);
end;

function TAltNameEntriesHelper.Add(ANameType: Integer; ADirs: TDirNames): Integer;
begin
  Self := Self + [TAltName.Create(ANameType, ADirs)];
  Result := Length(Self);
end;

{ TDirNamesHelper }

function TDirNamesHelper.Add(FieldName, Value: UTF8String): Integer;
begin
  Self := Self + [TDirName.Create(FieldName, Value)];
  Result := Length(Self);
end;

end.
