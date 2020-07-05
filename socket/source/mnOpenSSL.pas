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
  Classes,
  SysUtils,
  mnOpenSSLAPI,
  mnOpenSSLUtils,
  mnSockets;

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

  TCTX = class(TOpenSSLObject)
  protected
    Handle: PSSL_CTX;
    FMethod: TSSLMethod;
    FOwnMethod: Boolean; //created internally
  public
    constructor Create(AMethod: TSSLMethod); overload;
    constructor Create(AMethodClass: TSSLMethodClass); overload;
    destructor Destroy; override;
    procedure LoadCertFile(FileName: string);
    procedure LoadPrivateKeyFile(FileName: string);
    procedure CheckPrivateKey;
  end;

  TSSL = record//class(TOpenSSLObject)
  //protected
    Handle: PSSL;
    CTX: TCTX;
  //public
    constructor Create(ACTX: TCTX); overload;
    constructor Create(ASSL: PSSL); overload;
    procedure SetSocket(ASocket: Integer);
    procedure Connect;
    procedure Accept;
    function Read(var Buf; Size: Integer): Integer;
    function Write(const Buf; Size: Integer): Integer;
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
    constructor Create(AFileName: string; Mode: string); //read doc of BIO_new_file
  end;

  { TBIOStreamSSL }

  TBIOStreamSSL = class(TBIOStream) //Socket
  protected
    CTX: TCTX;
  public
    constructor Create(ACTX: TCTX);
    //Host name with port like 'example.com:80';
    procedure SetHostName(AHostName: string);
    procedure Connect;
  end;

implementation

{ TTLS_SSLServerMethod }

procedure TTLS_SSLServerMethod.CreateHandle;
begin
  Handle := TLS_method();
end;

{ TBIOStreamSSL }

constructor TBIOStreamSSL.Create(ACTX: TCTX);
begin
  inherited Create;
  CTX := ACTX;
  Handle := BIO_new_ssl_connect(CTX);
end;

procedure TBIOStreamSSL.SetHostName(AHostName: string);
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

constructor TBIOStreamFile.Create(AFileName: string; Mode: string);
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
  Result := TSSL.Create(ssl);
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

constructor TSSL.Create(ACTX: TCTX);
begin
  //inherited Create;
  CTX := ACTX;
  Handle := SSL_new(CTX.Handle);
end;

constructor TSSL.Create(ASSL: PSSL);
begin
  //inherited Create;
  Handle := ASSL;
end;

procedure TSSL.SetSocket(ASocket: Integer);
begin
  SSL_set_fd(Handle, ASocket);
end;

procedure TSSL.Connect;
begin
  if SSL_connect(Handle) = -1 then
    raise EmnOpenSSLException.Create('Connect: ' + ERR_error_string(ERR_get_error(), nil));
end;

procedure TSSL.Accept;
begin
  if SSL_accept(Handle) = -1 then
    ;
    //raise EmnOpenSSLException.Create('Accept: ' + ERR_error_string(ERR_get_error(), nil));
end;

function TSSL.Read(var Buf; Size: Integer): Integer;
begin
  Result := SSL_read(Handle, Buf, Size);
end;

function TSSL.Write(const Buf; Size: Integer): Integer;
begin
  Result := SSL_write(Handle, Buf, Size);
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

{ TCTX }

constructor TCTX.Create(AMethod: TSSLMethod);
begin
  inherited Create;
  FMethod := AMethod;
  Handle := SSL_CTX_new(AMethod.Handle);
  if Handle = nil then
    EmnOpenSSLException.Create('Can not create CTX handle');
  SSL_CTX_set_options(Handle, SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3 or SSL_OP_NO_COMPRESSION);
end;

constructor TCTX.Create(AMethodClass: TSSLMethodClass);
begin
  Create(AMethodClass.Create);
  FOwnMethod := True;
end;

destructor TCTX.Destroy;
begin
  if FOwnMethod then
    FMethod.Free;
  if Handle <> nil then
    SSL_CTX_free(Handle);
  inherited Destroy;
end;

procedure TCTX.LoadCertFile(FileName: string);
begin
  if SSL_CTX_use_certificate_file(Handle, PUTF8Char(FileName), SSL_FILETYPE_PEM) <=0 then
    raise EmnOpenSSLException.Create('fail to load certificate');
end;

procedure TCTX.LoadPrivateKeyFile(FileName: string);
begin
  if SSL_CTX_use_PrivateKey_file(Handle, PUTF8Char(FileName), SSL_FILETYPE_PEM) <=0 then
    raise EmnOpenSSLException.Create('fail to load private key');
end;

procedure TCTX.CheckPrivateKey;
begin
  if SSL_CTX_check_private_key(Handle) = 0 then
    raise EmnOpenSSLException.Create('Private key does not match the public certificate');
end;

end.
