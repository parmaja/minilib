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
  SysUtils, Classes, mnSockets, mnClients, mnConnections;

type

  { TmnCommandClientConnection }

  TmnCommandClientConnection = class(TmnClientConnection)
  private
  protected
    procedure Process; override;
  public
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
  published
    property OnCreateCaller: TmnOnCreateCaller read FOnCreateCaller write FOnCreateCaller;
  end;

implementation

{ TmnCommandClientConnection }

procedure TmnCommandClientConnection.Process;
var
  s: string;
begin
  inherited;
  s := Stream.ReadLine;
  if Connected then
  begin
    if s='GET' then
      Stream.WriteLine('OK')
    else if s='SET' then
      Stream.WriteLine('TOK');
  end;
end;

{ TmnCommandCaller }

constructor TmnCommandCaller.Create;
begin
  inherited;
end;

function TmnCommandCaller.CreateConnection(Socket: TmnCustomSocket): TmnClientConnection;
begin
  Result := TmnCommandClientConnection.Create(Self, Socket);
end;

destructor TmnCommandCaller.Destroy;
begin
  inherited;
end;

{ TmnHttpClient }

function TmnCommandClient.CreateCaller: TmnCaller;
begin
  Result := nil;
  if Assigned(FOnCreateCaller) then
    FOnCreateCaller(Result);
  if Result = nil then
    Result := TmnCommandCaller.Create;
end;

end.

