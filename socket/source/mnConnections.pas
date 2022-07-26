unit mnConnections;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types, SyncObjs, mnLogs,
  mnStreams, mnSockets;

type

  { TmnThread }

  TmnThread = class(TThread)
  public
    constructor Create;
  end;

  { TmnLockThread }

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


  { TmnConnectionList }

  TmnConnectionList = class(TList)
  private
    function GetItems(Index: Integer): TmnConnection;
    procedure SetItems(Index: Integer; const Value: TmnConnection);
  protected
  public
    property Items[Index: Integer]: TmnConnection read GetItems write SetItems; default;
  end;

  { TmnConnections }

  TmnConnections = class abstract(TmnLockThread)  //TmnListener and TmnCaller using it
  private
    FLastID: Int64;
    FList: TmnConnectionList;
  protected
    FPort: string;
    FAddress: string;
    function GetCount: Integer;
    function NewID: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    //procedure Stop; virtual; deprecated;
    property Count: Integer read GetCount;
    property LastID: Int64 read FLastID;
    property List: TmnConnectionList read FList;
    procedure Add(Connection: TmnConnection); virtual;
    procedure Remove(Connection: TmnConnection); virtual;
  end;

  { TmnConnection }

  TmnConnection = class abstract(TmnThread)
  private
    FID: Integer;
    FOwner: TmnConnections;
  strict protected
    function GetConnected: Boolean; virtual; abstract;
    procedure Created; virtual;
    procedure Prepare; virtual;
    procedure Process; virtual;
    procedure Unprepare; virtual;
    procedure Stopped; virtual; //this run main thread called by Synchronize, so dont use it for hard code
    property Owner: TmnConnections read FOwner;
  protected
    procedure Execute; override;
    //procedure SetStream(AValue: TmnConnectionStream);
    procedure HandleException(E: Exception); virtual;
  public
    constructor Create(vOwner: TmnConnections);
    destructor Destroy; override;

    //procedure Connect; virtual;
    //procedure Disconnect(Safe: Boolean = true); virtual; // don't raise exception, now by default true
    //property Stream: TmnConnectionStream read FStream; //write SetStream; //not now

    property Connected: Boolean read GetConnected;

    //procedure Stop; virtual; deprecated;
    property ID: Integer read FID write FID;
  end;

procedure mnCheckError(Value: Integer);

implementation

var
  FCount: Integer = 0;

procedure mnCheckError(Value: Integer);
begin
  if Value > 0 then
    raise EmnException.Create('WinSocket, error #' + IntToStr(Value));
end;

{ TmnConnections }

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

{procedure TmnConnections.Stop;
begin
end;}

procedure TmnConnections.Add(Connection: TmnConnection);
begin
  Enter;
  try
    List.Add(Connection);
  finally
    Leave;
  end;
end;

procedure TmnConnections.Remove(Connection: TmnConnection);
begin
  Enter;
  try
    //if Connection.FreeOnTerminate then //Zaher: @Zaher and @Belal, idk if wrong :( think more zaher
    List.Remove(Connection);
  finally
    Leave;
  end;
end;

function TmnConnections.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TmnConnections.NewID: Int64;
begin
  Inc(FLastID);
  Result := FLastID;
end;

procedure TmnConnection.Execute;
begin
  {$ifdef FPC}
  ID := InterlockedIncrement(FCount);
  {$else}
  ID := AtomicIncrement(FCount);
  {$endif}
  try
    if Owner <> nil then
      Owner.Add(Self);
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
  finally
    Unprepare;
    Synchronize(Stopped);//Synchronize not queue, to sure all other queue is processed

    if FOwner <> nil then
      if FreeOnTerminate then
        Owner.Remove(Self); //remove from the server list
  end;
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

constructor TmnConnection.Create(vOwner: TmnConnections);
begin
  inherited Create;
  FOwner := vOwner;
  Created;
end;

destructor TmnConnection.Destroy;
begin
  {if Connected then
    Stop;}
  inherited;
end;

{procedure TmnConnection.Stop;
begin
  Terminate;
end;}

procedure TmnConnection.HandleException(E: Exception);
begin
end;

procedure TmnConnection.Created;
begin
end;

procedure TmnConnection.Prepare;
begin
end;

procedure TmnConnection.Unprepare;
begin
end;

procedure TmnConnection.Stopped;
begin
end;

{ TmnThread }

constructor TmnThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
end;

end.

