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
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs,
  mnSockets, mnConnections, mnSocketStreams, mnServers;

type
  TmnCommandExceotion = class(Exception);
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData);

  TmnCommand = class;

  { TmnCommandConnection }

  TmnCommandConnection = class(TmnServerConnection)
  private
    FCommandObject: TmnCommand;
  public
  protected
    procedure Process; override;
  public
    constructor Create(vConnector: TmnConnector; Socket: TmnCustomSocket); override;
    destructor Destroy; override;
  published
  end;

  { TmnCommand }

  TmnCommand = class(TObject)
  private
    FName: string;
    FRequest: string;
    FKeepAlive: Boolean;
    FServer: TmnServer;
    FConcur: Boolean;
    FSingle: Boolean;
    FConnection: TmnCommandConnection;
    FLocking: Boolean;
    FRaiseExceptions: Boolean;
  protected
    FWorking: Boolean;
    procedure Execute; virtual;
    function Connected: Boolean;
    procedure Shutdown;
  public
    constructor Create(Connection: TmnCommandConnection); virtual;
    //GetCommandName: make name for command when register it, useful when log the name of it
    class function GetCommandName: string; virtual;
    property Connection: TmnCommandConnection read FConnection;
    property Name: string read FName;
    property Request: string read FRequest; //Full of first line of header
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    //Lock the server listener when execute the command
    property Locking: Boolean read FLocking write FLocking default True;
    //Concur: Synchronize the connection thread, when use it in GUI application
    property Concur: Boolean read FConcur write FConcur default False;
    //KeepAlive keey the command object after disconnect, not completed yet!
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default False;
    //Single command not launch another command if there is a privouse live command, depend on KeepAlive
    property Single: Boolean read FSingle write FSingle;
    //Prepare called after created in lucking mode
    procedure Prepare; virtual;
    property Server: TmnServer read FServer;
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

  TmnCommandClasses = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnCommandClassItem;
    procedure SetItem(Index: Integer; Value: TmnCommandClassItem);
  public
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
    function Find(const Name: string): TmnCommandClassItem;
    property Items[Index: Integer]: TmnCommandClassItem read GetItem write SetItem; default;
  end;

  TmnCommandServer = class;

  { TmnCommandListener }

  TmnCommandListener = class(TmnListener)
  private
    function GetServer: TmnCommandServer;
  protected
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
    function CreateStream(Socket: TmnCustomSocket): TmnSocketStream; override;
    procedure ParseCommand(const Line: string; out Method, Params: string); virtual;
    property Server: TmnCommandServer read GetServer;
  public
    constructor Create;
    destructor Destroy; override;
    //Name here will corrected with registered item name for example Get -> GET
    function GetCommandClass(var Name: string): TmnCommandClass;
  end;

  TmnCommandServer = class(TmnEventServer)
  private
    FCommands: TmnCommandClasses;
  protected
    function CreateListener: TmnListener; override;
    property Commands: TmnCommandClasses read FCommands;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer; overload;
    function RegisterCommand(CommandClass: TmnCommandClass): Integer; overload;
  end;

implementation

constructor TmnCommandServer.Create;
begin
  inherited;
  FCommands := TmnCommandClasses.Create(True);
  Port := '81';
end;

destructor TmnCommandServer.Destroy;
begin
  FCommands.Free;
  inherited;
end;

constructor TmnCommandConnection.Create(vConnector: TmnConnector; Socket: TmnCustomSocket);
begin
  inherited;
  KeepAlive := True;
end;

destructor TmnCommandConnection.Destroy;
begin
  FreeAndNil(FCommandObject);
  inherited;
end;

{ TmnCommandConnection }

procedure TmnCommandConnection.Process;
var
  aRequest: string;
  aCommand: string;
  aParams: string;
  p: Integer;
  aClass: TmnCommandClass;
begin
  inherited;
  if Connected then
  begin
    if FCommandObject = nil then
    begin
      if Connected then
      begin

        aRequest := Stream.ReadLine;
        (Listener as TmnCommandListener).ParseCommand(aRequest, aCommand, aParams);

        Listener.Enter;
        try
          aClass := (Listener as TmnCommandListener).GetCommandClass(aCommand);
          if aClass <> nil then
          begin
            FCommandObject := aClass.Create(Self);
            FCommandObject.FName := aCommand; //Already correct with GetCommandClass
            FCommandObject.FRequest := aRequest;
            FCommandObject.FServer := Listener.Server;
            FCommandObject.Prepare;
          end;
          //TODO make a default command if not found
        finally
          Listener.Leave;
        end;
      end;

      if FCommandObject <> nil then
      begin
        try
          FCommandObject.FWorking := True; //TODO
          if FCommandObject.Locking then
            Listener.Enter;
          try
            if FCommandObject.Concur then
              Synchronize(FCommandObject.Execute)
            else
              FCommandObject.Execute;
          finally
            FCommandObject.FWorking := False;
            if FCommandObject.Locking then
              Listener.Leave;
          end;
        except
          if FCommandObject.RaiseExceptions then
            raise;
        end;
        if FCommandObject.KeepAlive then
        begin
        //TODO
        end
        else
        begin
          if Stream.Connected then
            Stream.Disconnect;
          FreeAndNil(FCommandObject);
        end;
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

function TmnCommandListener.GetServer: TmnCommandServer;
begin
  Result := inherited Server as TmnCommandServer;
end;

function TmnCommandListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TmnCommandConnection.Create(Self, vSocket);
end;

function TmnCommandListener.CreateStream(Socket: TmnCustomSocket): TmnSocketStream;
begin
  Result := inherited CreateStream(Socket);
  Result.Timeout := -1;
end;

procedure TmnCommandListener.ParseCommand(const Line: string; out Method, Params: string);
var
  p: Integer;
begin
  p := Pos(' ', Line);
  if p > 0 then
  begin
    Method := Trim(Copy(Line, 1, p - 1));
    Params := Trim(Copy(Line, p + 1, MaxInt));
  end
  else
  begin
    Method := Trim(Line);
    Params := '';
  end;
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
  if Active then
    raise TmnCommandExceotion.Create('Server is Active');
  if FCommands.Find(vName) <> nil then
    raise TmnCommandExceotion.Create('Command already exists: ' + vName);
  Result := FCommands.Add(UpperCase(vName), CommandClass);
end;

function TmnCommandListener.GetCommandClass(var Name: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := Server.Commands.Find(Name);
  if aItem <> nil then
  begin
    Name := aItem.Name;
    Result := aItem.CommandClass;
  end
  else
    Result := nil;
end;

{ TmnCommand }

constructor TmnCommand.Create(Connection: TmnCommandConnection);
begin
  inherited Create;
  FLocking := True;
  FConnection := Connection;
end;

procedure TmnCommand.Execute;
begin
end;

function TmnCommand.Connected: Boolean;
begin
  Result := (Connection <> nil) and (Connection.Connected);
end;

procedure TmnCommand.Shutdown;
begin
  if Connected and (Connection.Stream <> nil) then
    Connection.Stream.Socket.Shutdown(sdBoth);
end;

class function TmnCommand.GetCommandName: string;
begin
  Result := ClassName;
end;

procedure TmnCommand.Prepare;
begin
end;

{ TmnCommandClasses }

function TmnCommandClasses.Add(const Name: string; CommandClass: TmnCommandClass): Integer;
var
  aItem: TmnCommandClassItem;
begin
  aItem := TmnCommandClassItem.Create;
  aItem.FName := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

function TmnCommandClasses.Find(const Name: string): TmnCommandClassItem;
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

function TmnCommandClasses.GetItem(Index: Integer): TmnCommandClassItem;
begin
  Result := inherited Items[Index] as TmnCommandClassItem;
end;

procedure TmnCommandClasses.SetItem(Index: Integer; Value: TmnCommandClassItem);
begin
  inherited Items[Index] := Value;
end;

function TmnCommandServer.RegisterCommand(CommandClass: TmnCommandClass): Integer;
begin
  Result := RegisterCommand(CommandClass.GetCommandName, CommandClass);
end;

end.

