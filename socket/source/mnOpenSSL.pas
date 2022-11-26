unit mnOpenSSL;
{**
 *  This file is part of the "Mini Library"/Sockets
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  mnLogs,
  mnOpenSSLAPI;

type

  EmnOpenSSLException = Exception;
  { TOpenSSLObject }

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
  public
  end;

  { TTLS_SSLServerMethod }

  TTLS_SSLServerMethod = class(TSSLMethod)
  protected
    procedure CreateHandle; override;
  public
  end;

  { TCTX }

  TContextOptions = set of (coNoComppressing);

  { TContext }

  TContext = class(TOpenSSLObject)
  protected
    Handle: PSSL_CTX;
    FMethod: TSSLMethod;
    FOwnMethod: Boolean; //created internally
  public
    constructor Create(AMethod: TSSLMethod; Options: TContextOptions = [coNoComppressing]); overload;
    constructor Create(AMethodClass: TSSLMethodClass); overload;
    destructor Destroy; override;
    procedure LoadCertFile(FileName: utf8string);
    procedure LoadPrivateKeyFile(FileName: utf8string);
    procedure CheckPrivateKey;
    procedure SetVerifyNone;
  end;

  TSSL = record//class(TOpenSSLObject)
  //protected
    Handle: PSSL;
    CTX: TContext;
    Connected: Boolean;
    FSocket: Integer;
    Active: Boolean;
  //public
    constructor Init(ACTX: TContext); overload;
    constructor Init(ASSL: PSSL); overload;
    procedure Free;
    procedure ShutDown;
    procedure SetSocket(ASocket: Integer);
    function Connect: Boolean;
    function Handshake: Boolean;
    procedure SetVerifyNone;
    //Return False if failed
    function Read(var Buf; Size: Integer; out ReadSize: Integer): Boolean;
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
    CTX: TContext;
  public
    constructor Create(ACTX: TContext);
    //Host name with port like 'example.com:80';
    procedure SetHostName(AHostName: utf8string);
    procedure Connect;
  end;

procedure InitOpenSSL(All: Boolean = True);
procedure InitOpenSSLLibrary(All: Boolean = True);
procedure CleanupOpenSSL;

procedure RaiseLastSSLError;
procedure RaiseSSLError(Message: utf8string);

function MakeCert(var x509p: PX509; var pkeyp: PEVP_PKEY; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;
function MakeCert(CertificateFile, PrivateKeyFile: utf8string; CN, O, C, OU: utf8string; Bits: Integer; Serial: Integer; Days: Integer): Boolean; overload;

implementation

uses
  mnSockets;

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



{ TTLS_SSLServerMethod }

procedure TTLS_SSLServerMethod.CreateHandle;
begin
  Handle := TLS_method();
end;

{ TBIOStreamSSL }

constructor TBIOStreamSSL.Create(ACTX: TContext);
begin
  inherited Create;
  CTX := ACTX;
  Handle := BIO_new_ssl_connect(CTX);
end;

procedure TBIOStreamSSL.SetHostName(AHostName: utf8string);
begin
  BIO_set_conn_hostname(Handle, PUTF8Char(AHostName)); //Always return 1
end;

procedure TBIOStreamSSL.Connect;
var
  res: Integer;
begin
  res := BIO_do_connect(Handle);
  if res <> 1 then
    RaiseLastSSLError;
end;

{ TBIOStreamFile }

constructor TBIOStreamFile.Create(AFileName: utf8string; Mode: utf8string);
begin
  inherited Create;
  Handle := BIO_new_file(PUTF8Char(AFileName), PUTF8Char(Mode));
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
  Handle := SSL_new(CTX.Handle);
  {$ifdef DEBUG}
  Log.WriteLn(SSL_get_version(Handle));
  {$endif}
  Active := True;
end;

constructor TSSL.Init(ASSL: PSSL);
begin
  //inherited Create;
  Handle := ASSL;
  Active := True;
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
  Active := False;
end;

procedure TSSL.SetSocket(ASocket: Integer);
begin
  FSocket := ASocket;
  SSL_set_fd(Handle, FSocket);
end;

function TSSL.Connect: Boolean;
var
  ret: Integer;
begin
  ret := SSL_connect(Handle);
  if ret < 0  then
  begin
    Result := False;
    Log.WriteLn('Connect: ' + ERR_error_string(ERR_get_error(), nil));
  end
  else if ret = 0 then //error
    Result := False
  else
    Result := True;
end;

function TSSL.Handshake: Boolean;
var
  ret, err: Integer;
begin
  ret := SSL_accept(Handle);
  if ret <= 0  then
  begin
    err := SSL_get_error(Handle, ret);
    Log.WriteLn('Handshake: ' + ERR_error_string(err, nil));
    Result := False;
  end
  else
    Result := True;
end;

procedure TSSL.SetVerifyNone;
begin
  SSL_set_verify(Handle, SSL_VERIFY_NONE, nil);
end;

function TSSL.Read(var Buf; Size: Integer; out ReadSize: Integer): Boolean;
var
  err: Integer;
begin
  if not Active then
    raise EmnOpenSSLException.Create('SSL object is not Active');

  ReadSize := SSL_read(Handle, Buf, Size);
  if ReadSize <= 0  then
  begin
    err := SSL_get_error(Handle, ReadSize);
    Log.WriteLn('Read: ' + ERR_error_string(err, nil));
    {
      Here we have a problem some are not real error, Disconnected gracefully, or read time out
    }
    if err = 5 then
    begin
      err := WallSocket.GetSocketError(FSocket);
      Log.WriteLn('Read: Socket Error: ' + IntToStr(err));
    end;
    ReadSize := 0;
    Result := False;
  end
  else
    Result := True;
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

constructor TContext.Create(AMethod: TSSLMethod; Options: TContextOptions);
var
  o: Cardinal;
begin
  inherited Create;
  FMethod := AMethod;
  Handle := SSL_CTX_new(AMethod.Handle);
  if Handle = nil then
    EmnOpenSSLException.Create('Can not create CTX handle');
  o := SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3;
  if coNoComppressing in Options then
    o := o or SSL_OP_NO_COMPRESSION;
  SSL_CTX_set_options(Handle, o);
end;

constructor TContext.Create(AMethodClass: TSSLMethodClass);
begin
  Create(AMethodClass.Create);
  FOwnMethod := True;
end;

destructor TContext.Destroy;
begin
  if FOwnMethod then
    FMethod.Free;
  if Handle <> nil then
    SSL_CTX_free(Handle);
  inherited Destroy;
end;

procedure TContext.LoadCertFile(FileName: utf8string);
begin
  if SSL_CTX_use_certificate_file(Handle, PUTF8Char(FileName), SSL_FILETYPE_PEM) <= 0 then
    raise EmnOpenSSLException.Create('fail to load certificate');
end;

procedure TContext.LoadPrivateKeyFile(FileName: utf8string);
begin
  if SSL_CTX_use_PrivateKey_file(Handle, PUTF8Char(FileName), SSL_FILETYPE_PEM) <= 0 then
    raise EmnOpenSSLException.Create('fail to load private key');
end;

procedure TContext.CheckPrivateKey;
begin
  if SSL_CTX_check_private_key(Handle) = 0 then
    raise EmnOpenSSLException.Create('Private key does not match the public certificate');
end;

procedure TContext.SetVerifyNone;
begin
  SSL_CTX_set_verify(Handle, SSL_VERIFY_PEER, nil);
end;

end.
