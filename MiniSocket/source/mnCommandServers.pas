unit mnCommandServers;
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
  SysUtils, Classes, Contnrs, mnSockets, mnStreams, mnServers;

type
  TmnCommandExceotion = class(Exception);
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData);

  TmnCommand = class;
  TmnCommandConnection = class(TmnServerConnection)
  private
    FCommandObject: TmnCommand;
  public
  protected
    procedure Process; override;
    function CreateStream(Socket: TmnCustomSocket): TmnConnectionStream; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
    destructor Destroy; override;
  published
  end;

  TmnCommand = class(TObject)
  private
    FName: string;
    FKeepAlive: Boolean;
    FServer: TmnServer;
  protected
    procedure Execute(Connection: TmnCommandConnection); virtual;
  public
    constructor Create(Connection: TmnCommandConnection; const Params:string); virtual;
    class function GetCommandName:string; virtual; 
    property Server:TmnServer read FServer;
    property Name: string read FName;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
  end;

  TmnCommandClass = class of TmnCommand;

  TmnCommandClassItem = class(TObject)
  private
    FName: string;
    FCommandClass: TmnCommandClass;
  public
    property Name: string read FName;
    property CommandClass: TmnCommandClass read FCommandClass;
  end;

  TmnCommands = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnCommandClassItem;
    procedure SetItem(Index: Integer; Value: TmnCommandClassItem);
  public
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
    function Find(const Name: string): TmnCommandClassItem;
    property Items[Index: Integer]: TmnCommandClassItem read GetItem write SetItem; default;
  end;

  TmnCommandListener = class(TmnListener)
  private
    FAddress: string;
    FPort: Integer;
    FCommands: TmnCommands;
  protected
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Port: Integer read FPort write FPort;
    property Address: string read FAddress write FAddress;
    function GetCommandClass(const Name: string): TmnCommandClass;
  end;

  TmnCommandServer = class(TmnServer)
  private
    FCommands: TmnCommands;
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    function CreateListener: TmnListener; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer; overload;
    function RegisterCommand(CommandClass: TmnCommandClass): Integer; overload;
  published
  end;

implementation

constructor TmnCommandServer.Create(AOwner: TComponent);
begin
  inherited;
  FCommands := TmnCommands.Create(True);
  Port := '81';
end;

destructor TmnCommandServer.Destroy;
begin
  FCommands.Free;
  inherited;
end;

procedure TmnCommandServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

constructor TmnCommandConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  KeepAlive := True;
end;

function TmnCommandConnection.CreateStream(Socket: TmnCustomSocket): TmnConnectionStream;
begin
  Result := inherited CreateStream(Socket);
  Result.Timeout := -1; 
end;

destructor TmnCommandConnection.Destroy;
begin
  FreeAndNil(FCommandObject);
  inherited;
end;

{ TmnCommandConnection }

procedure TmnCommandConnection.Process;
var
  aCMD: string;
  aCommand: string;
  aParams: string;
  p: Integer;
  aClass: TmnCommandClass;
begin
  inherited;
  aCMD := Stream.ReadLn;
  if Connected then
  begin
    p := Pos(' ', aCMD);
    if p > 0 then
    begin
      aCommand := Copy(aCmd, 1, p - 1);
      aParams := Copy(aCmd, p + 1, MaxInt);
    end
    else
    begin
      aCommand := aCmd;
      aParams := '';
    end;
    if Connected then
    begin
      if FCommandObject = nil then
      begin
        Listener.Enter;
        try
          aClass := (Listener as TmnCommandListener).GetCommandClass(aCommand);
          if aClass <> nil then
          begin
            FCommandObject := aClass.Create(Self, aParams);
            FCommandObject.FServer:=Listener.Server;
            FCommandObject.FName := aCommand;
//            Listener.Log(Self, aCommand);
          end;
        finally
          Listener.Leave;
        end;
      end;
      if FCommandObject <> nil then
      begin
        try
          FCommandObject.Execute(Self);
        except
          Stream.Disconnect;
        end;
        if not FCommandObject.KeepAlive then
          FreeAndNil(FCommandObject);
      end
      else
        Stream.Disconnect;
    end;
  end;
end;

{ TGuardSocketServer }

function TmnCommandServer.CreateListener: TmnListener;
begin
  Result := TmnCommandListener.Create;
  (Result as TmnCommandListener).FCommands := FCommands;
end;

procedure EnumDirList(const Path: string; Strings: TStrings);
var
  I: Integer;
  SearchRec: TSearchRec;
begin
  try
    I := FindFirst(Path, faDirectory, SearchRec);
    while I = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name[1] <> '.') then
        Strings.Add(SearchRec.Name);
      I := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  except
  end;
end;

{ TmnCommandListener }

function TmnCommandListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TmnCommandConnection.Create(vSocket);
end;

constructor TmnCommandListener.Create;
begin
  inherited;
  FOptions := FOptions + [soReuseAddr];
end;

destructor TmnCommandListener.Destroy;
begin
  inherited;
end;

function TmnCommandServer.RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer;
begin
  if FCommands.Find(vName) <> nil then
    raise TmnCommandExceotion.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
end;

function TmnCommandListener.GetCommandClass(const Name: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := FCommands.Find(Name);
  if aItem <> nil then
    Result := aItem.CommandClass
  else
    Result := nil;
end;

{ TmnCommand }

constructor TmnCommand.Create(Connection: TmnCommandConnection; const Params:string);
begin
  inherited Create;
end;

procedure TmnCommand.Execute(Connection: TmnCommandConnection);
begin
end;

class function TmnCommand.GetCommandName: string;
begin
  Result := ''; 
end;

{ TmnCommands }

function TmnCommands.Add(const Name: string; CommandClass: TmnCommandClass): Integer;
var
  aItem: TmnCommandClassItem;
begin
  aItem := TmnCommandClassItem.Create;
  aItem.FName := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

function TmnCommands.Find(const Name: string): TmnCommandClassItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnCommands.GetItem(Index: Integer): TmnCommandClassItem;
begin
  Result := inherited Items[Index] as TmnCommandClassItem;
end;

procedure TmnCommands.SetItem(Index: Integer; Value: TmnCommandClassItem);
begin
  inherited Items[Index] := Value;
end;

function TmnCommandServer.RegisterCommand(CommandClass: TmnCommandClass): Integer;
begin
  Result := RegisterCommand(CommandClass.GetCommandName, CommandClass);
end;

end.

