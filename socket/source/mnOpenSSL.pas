unit mnOpenSSL;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
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
  mnSockets;

type
  EmnOpenSSLException = EmnSocketException;

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
    function Read(var Buf; Size: Integer): Integer;
    function Write(const Buf; Size: Integer): Integer;
  end;

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

procedure InitOpenSSL;

implementation

procedure InitOpenSSL;
begin
  if OpenSSLLib.Load then
    OPENSSL_init_ssl(0, nil);
  //ERR_load_SSL_strings();//IDK
  if CryptoLib.Load then
    OPENSSL_init_crypto(0, nil);
  //ERR_load_CRYPTO_strings();//IDK
end;

procedure RaiseSSLError(Message: string);
begin
  raise EmnOpenSSLException.Create(Message);
end;

procedure RaiseLastSSLError;
begin
  RaiseSSLError(ERR_error_string(ERR_get_error(), nil));
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
    raise EmnOpenSSLException.Create(ERR_error_string(ERR_get_error(), nil));
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

end.
