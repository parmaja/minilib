unit mnConnections;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  mnSockets,
  mnSocketStreams;

type
  TmnThread = class(TThread)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TmnLockThread = class(TmnThread)
  private
    FLock: TCriticalSection;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

  TmnConnection = class;

  { TmnConnector }

  TmnConnector = class(TmnLockThread)
  protected
    function CreateStream(Socket: TmnCustomSocket): TmnSocketStream; virtual;
  end;

  { TmnConnection }

  TmnConnection = class(TmnThread)
  private
    FConnector: TmnConnector;
    FStream: TmnSocketStream;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  protected
    property Connector: TmnConnector read FConnector;
    procedure Prepare; virtual;
    procedure Process; virtual;
    procedure Execute; override;
    procedure Unprepare; virtual;
  public
    constructor Create(vConnector: TmnConnector; vSocket: TmnCustomSocket); virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    {$ifndef FPC} //already found in FPC 2.4.4
    procedure Start;
    {$endif}
    procedure Stop; virtual;
    property Connected: Boolean read GetConnected write SetConnected;
    property Stream: TmnSocketStream read FStream;
  end;

  TmnConnectionList = class(TList)
  private
    function GetItems(Index: Integer): TmnConnection;
    procedure SetItems(Index: Integer; const Value: TmnConnection);
  protected
  public
    property Items[Index: Integer]: TmnConnection read GetItems write SetItems; default;
  end;

procedure mnCheckError(Value: Integer);

implementation

procedure mnCheckError(Value: Integer);
begin
  if Value > 0 then
    raise EmnException.Create('WinSocket, error #' + IntToStr(Value));
end;

{ TmnConnector }

function TmnConnector.CreateStream(Socket: TmnCustomSocket): TmnSocketStream;
begin
  Result := TmnSocketStream.Create(Socket);
end;

procedure TmnConnection.Execute;
begin
  Prepare;
  while not Terminated and Connected do
  begin
    try
      Process;
    except
      Disconnect; //TODO: Do we need to disconnect when we have exception? maybe we need to add option for it
    end;
  end;
  Unprepare;
end;

constructor TmnLockThread.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor TmnLockThread.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TmnLockThread.Enter;
begin
  FLock.Enter;
end;

procedure TmnLockThread.Leave;
begin
  FLock.Leave;
end;

procedure TmnConnection.Process;
begin
end;

function TmnConnectionList.GetItems(Index: Integer): TmnConnection;
begin
  Result := inherited Items[Index];
end;

procedure TmnConnectionList.SetItems(Index: Integer; const Value: TmnConnection);
begin
  inherited Items[Index] := Value;
end;

{ TmnConnection }

procedure TmnConnection.Close;
begin
  Disconnect;
end;

constructor TmnConnection.Create(vConnector: TmnConnector; vSocket: TmnCustomSocket);
begin
  inherited Create;
  FConnector := vConnector;
  FStream := vConnector.CreateStream(vSocket);
end;

destructor TmnConnection.Destroy;
begin
  Close;
  FreeAndNil(FStream);
  inherited;
end;

procedure TmnConnection.Open;
begin
  Connect;
end;

{$ifndef FPC}
procedure TmnConnection.Start;
begin
  Resume;
end;
{$endif}

procedure TmnConnection.Stop;
begin
  Terminate;
  Disconnect;
end;

function TmnConnection.GetConnected: Boolean;
begin
  Result := FStream.Connected;
end;

procedure TmnConnection.SetConnected(const Value: Boolean);
begin
  if Value then
    Open
  else
    Close;
end;

procedure TmnConnection.Disconnect;
begin
  if FStream.Connected then
    FStream.Disconnect;
end;

procedure TmnConnection.Connect;
begin
end;

procedure TmnConnection.Prepare;
begin
end;

procedure TmnConnection.Unprepare;
begin
end;

{ TmnThread }

constructor TmnThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TmnThread.Destroy;
begin
  inherited;
end;

end.

