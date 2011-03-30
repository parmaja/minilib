unit mnCommandClients;
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
  SysUtils, Classes, mnSockets, mnClients;

type
  TmnCommandClientConnection = class(TmnClientConnection)
  private
  protected
    procedure Process; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
    destructor Destroy; override;
  end;

  TmnCommandCaller = class(TmnCaller)
  private
  protected
    function CreateConnection(Socket: TmnCustomSocket): TmnClientConnection; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TmnOnCreateCaller = procedure(var Caller: TmnCaller) of object;

  TmnCommandClient = class(TmnClient)
  private
    FOnCreateCaller: TmnOnCreateCaller;
  protected
    function CreateCaller: TmnCaller; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnCreateCaller: TmnOnCreateCaller read FOnCreateCaller write FOnCreateCaller;
  end;

implementation

{ TmnCommandClientConnection }

constructor TmnCommandClientConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  KeepAlive := True;
end;

destructor TmnCommandClientConnection.Destroy;
begin
  inherited;
end;

procedure TmnCommandClientConnection.Process;
var
  s:string;
begin
  inherited;
  s:=Stream.ReadLn;
  if Connected then
  begin
    if s='GET' then
      Stream.WriteLn('OK')
    else if s='SET' then
      Stream.WriteLn('TOK');
  end;
end;

{ TmnCommandCaller }

constructor TmnCommandCaller.Create;
begin
  inherited;

end;

function TmnCommandCaller.CreateConnection(Socket: TmnCustomSocket): TmnClientConnection;
begin
  Result := TmnCommandClientConnection.Create(Socket);
end;

destructor TmnCommandCaller.Destroy;
begin
  inherited;
end;

{ TmnHttpClient }

constructor TmnCommandClient.Create(AOwner: TComponent);
begin
  inherited;
end;

function TmnCommandClient.CreateCaller: TmnCaller;
begin
  Result := nil;
  if Assigned(FOnCreateCaller) then
    FOnCreateCaller(Result);
  if Result = nil then
    Result := TmnCommandCaller.Create;
end;

destructor TmnCommandClient.Destroy;
begin
  inherited;
end;

end.

