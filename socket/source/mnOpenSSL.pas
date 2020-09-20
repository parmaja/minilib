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
  //public
    constructor Init(ACTX: TContext); overload;
    constructor Init(ASSL: PSSL); overload;
    procedure Free;
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

implementation

uses
  mnOpenSSLUtils;

procedure InitOpenSSL(All: Boolean);
begin
  InitOpenSSLLibrary(All);
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
  Initialize(Self);
  CTX := ACTX;
  Handle := SSL_new(CTX.Handle);
  {$ifdef DEBUG}
  Log.WriteLn(SSL_get_version(Handle));
  {$endif}
end;

constructor TSSL.Init(ASSL: PSSL);
begin
  //inherited Create;
  Handle := ASSL;
end;

procedure TSSL.Free;
begin
  SSL_free(Handle);
  Handle := nil;
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
  ReadSize := SSL_read(Handle, Buf, Size);
  if ReadSize <= 0  then
  begin
    err := SSL_get_error(Handle, ReadSize);
    Log.WriteLn('Read: ' + ERR_error_string(err, nil));
    {
      Here we have a problem some are not real error, Disconnected gracefully, or read time out
    }
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
