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


  TmnConnectionList = class(TList)
  private
    function GetItems(Index: Integer): TmnConnection;
    procedure SetItems(Index: Integer; const Value: TmnConnection);
  protected
  public
    property Items[Index: Integer]: TmnConnection read GetItems write SetItems; default;
  end;

  { TmnConnections }

  TmnConnections = class(TmnLockThread)  //TmnListener and TmnCaller using it
  private
    FList: TmnConnectionList;
  protected
    FPort: string;
    FAddress: string;
    function CreateStream(Socket: TmnCustomSocket): TmnSocketStream; virtual;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop; virtual;
    property Count: Integer read GetCount;
    property List: TmnConnectionList read FList;
  end;

  { TmnConnection }

  TmnConnection = class(TmnThread)
  private
    FConnector: TmnConnections;
    FStream: TmnSocketStream;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  protected
    property Connector: TmnConnections read FConnector;
    procedure Prepare; virtual;
    procedure Process; virtual;
    procedure Execute; override;
    procedure Unprepare; virtual;

    procedure HandleException(E: Exception); virtual;
  public
    constructor Create(vConnector: TmnConnections; vSocket: TmnCustomSocket); virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disconnect
    {$ifndef FPC} //already found in FPC 2.4.4
    procedure Start;
    {$endif}
    procedure Stop; virtual;
    property Connected: Boolean read GetConnected write SetConnected;
    property Stream: TmnSocketStream read FStream;
  end;

procedure mnCheckError(Value: Integer);

implementation

procedure mnCheckError(Value: Integer);
begin
  if Value > 0 then
    raise EmnException.Create('WinSocket, error #' + IntToStr(Value));
end;

{ TmnConnections }

function TmnConnections.CreateStream(Socket: TmnCustomSocket): TmnSocketStream;
begin
  Result := TmnSocketStream.Create(Socket);
end;

constructor TmnConnections.Create;
begin
  inherited;
  FList := TmnConnectionList.Create;
end;

destructor TmnConnections.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TmnConnections.Stop;
begin
end;

function TmnConnections.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TmnConnection.Execute;
begin
  Prepare;
  while not Terminated and Connected do
  begin
    try
      Process;
    except
      on E: Exception do
      begin
        HandleException(E);
        //Disconnect; //TODO: Do we need to disconnect when we have exception? maybe we need to add option for it
      end;
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

constructor TmnConnection.Create(vConnector: TmnConnections; vSocket: TmnCustomSocket);
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

procedure TmnConnection.HandleException(E: Exception);
begin
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

end.

