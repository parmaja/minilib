unit mnOpenSSL;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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

  { TSSLMethod }

  TSSLMethod = class abstract(TObject)
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

  TCTX = class(TObject)
  protected
    Handle: PSSL_CTX;
    FMethod: TSSLMethod;
    FOwnMethod: Boolean; //created internally
  public
    constructor Create(AMethod: TSSLMethod); overload;
    constructor Create(AMethodClass: TSSLMethodClass); overload;
    destructor Destroy; override;
  end;

  TSSL = class(TObject)
  protected
    Handle: PSSL;
    CTX: TCTX;
  public
    constructor Create(ACTX: TCTX);
    procedure SetSocket(ASocket: Integer);
    procedure Connect;
    function Read(var Buf; Size: Integer): Integer;
    function Write(const Buf; Size: Integer): Integer;
  end;

implementation

{ TSSL }

constructor TSSL.Create(ACTX: TCTX);
begin
  inherited Create;
  CTX := ACTX;
  Handle := SSL_new(CTX.Handle);
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

initialization
  //Temporary here, we need to make it more smart, nah?
  OpenSSLLib.Init;
  CryptoLib.Init;
  OPENSSL_init_ssl(0, nil);
  OPENSSL_init_crypto(0, nil);

  //ERR_load_CRYPTO_strings();//IDK
  //ERR_load_SSL_strings();//IDK

end.
