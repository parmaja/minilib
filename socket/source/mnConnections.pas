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
  mnStreams,
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
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; virtual;
    function CreateConnection(vSocket: TmnCustomSocket): TmnConnection;
    function CreateStream(vSocket: TmnCustomSocket): TmnConnectionStream; virtual; //todo move it to another unit
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
    FOwner: TmnConnections;
    FStream: TmnConnectionStream;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  protected
    property Owner: TmnConnections read FOwner;
    procedure Prepare; virtual;
    procedure Process; virtual;
    procedure Execute; override;
    procedure Unprepare; virtual;

    procedure HandleException(E: Exception); virtual;
  public
    constructor Create(vOwner: TmnConnections; vStream: TmnConnectionStream); virtual; //TODO use TmnBufferStream
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disconnect
    procedure Stop; virtual;
    procedure Release;
    property Connected: Boolean read GetConnected write SetConnected;
    property Stream: TmnConnectionStream read FStream;
  end;

procedure mnCheckError(Value: Integer);

implementation

procedure mnCheckError(Value: Integer);
begin
  if Value > 0 then
    raise EmnException.Create('WinSocket, error #' + IntToStr(Value));
end;

{ TmnConnections }

function TmnConnections.CreateStream(vSocket: TmnCustomSocket): TmnConnectionStream;
begin
  Result := TmnSocketStream.Create(vSocket);
end;

function TmnConnections.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnConnection.Create(Self, vStream);
end;

function TmnConnections.CreateConnection(vSocket: TmnCustomSocket): TmnConnection;
begin
  Result := DoCreateConnection(CreateStream(vSocket));
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

constructor TmnConnection.Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited Create;
  FOwner := vOwner;
  FStream := vStream;
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

procedure TmnConnection.Stop;
begin
  Terminate;
  Disconnect;
end;

procedure TmnConnection.Release;
begin
  if FOwner <> nil then
  begin
    FOwner.List.Extract(Self);
    FOwner := nil;
  end;
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

