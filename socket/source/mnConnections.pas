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
{$IFDEF WIN32}
  mnWin32Sockets,
{$ENDIF}
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

  TmnConnection = class(TmnThread)
  private
    FStream: TmnConnectionStream;
    FKeepAlive: Boolean;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  protected
    procedure Prepare; virtual;
    procedure Process; virtual;
    procedure Execute; override;
    procedure Unprepare; virtual;
    function CreateStream(Socket: TmnCustomSocket): TmnConnectionStream; virtual;
  public
    constructor Create(Socket: TmnCustomSocket); virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Connected: Boolean read GetConnected write SetConnected;
    property Stream: TmnConnectionStream read FStream;
  end;

  TmnConnectionList = class(TList)
  private
    function GetItems(Index: Integer): TmnConnection;
    procedure SetItems(Index: Integer; const Value: TmnConnection);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: Integer]: TmnConnection read GetItems write SetItems; default;
  end;

procedure mnCheckError(Value: Integer);

implementation

procedure mnCheckError(Value: Integer);
begin
  if Value > 0 then
    raise EmnException.Create('WinSocket, error #' + IntToStr(Value));
end;

procedure TmnConnection.Execute;
begin
  Prepare;
  while not Terminated and Connected do
  begin
    try
      Process;
    except
      Disconnect;
    end;
    if not KeepAlive and not Terminated and Connected then
      Disconnect;
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

{ TSockeTmnServerConnection }

constructor TmnConnectionList.Create;
begin
  inherited;
end;

destructor TmnConnectionList.Destroy;
begin
  inherited;
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

constructor TmnConnection.Create(Socket: TmnCustomSocket);
begin
  inherited Create;
  FStream := CreateStream(Socket);
end;

destructor TmnConnection.Destroy;
begin
  Close;
  FreeAndNil(FStream);
  inherited;
end;

procedure TmnConnection.Open;
begin
  Disconnect;
end;

procedure TmnConnection.Start;
begin
  Resume;
end;

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

function TmnConnection.CreateStream(Socket: TmnCustomSocket): TmnConnectionStream;
begin
  Result := TmnConnectionStream.Create(Socket);
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

