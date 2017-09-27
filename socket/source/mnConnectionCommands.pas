unit mnConnectionCommands;
{$M+}{$H+}
{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes, Contnrs,
  mnClasses,
  mnStreams,
  mnSockets, mnConnections, mnServers;

type
  TmnCommandExceotion = class(Exception);
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData);

  TmnCommand = class;

  { TmnCommandConnection }

  TmnCommandConnection = class(TmnServerConnection)
  private
    FCommand: TmnCommand;
  public
  protected
    procedure Process; override;
  public
    destructor Destroy; override;
  published
  end;

  { TmnCommandLine }

  TmnRequest = record
    Name: string; //Module Name
    Method: string;
    Path: string;
    Version: string;
    Request: string; //Full of first line of header
  end;


  { TmnCommand }

  TmnCommand = class(TObject)
  private
    FRequest: TmnRequest;
    FServer: TmnServer;
    FConcur: Boolean;
    FConnection: TmnCommandConnection;
    FLocking: Boolean;
    FRaiseExceptions: Boolean;
  protected
    FWorking: Boolean;
    procedure Execute; virtual;
    function Connected: Boolean;
    procedure Shutdown;
    procedure DoPrepare; virtual;
  public
    constructor Create(Connection: TmnCommandConnection); virtual;
    //GetCommandName: make name for command when register it, useful when log the name of it
    class function GetCommandName: string; virtual; deprecated;
    property Connection: TmnCommandConnection read FConnection;
    property Request: TmnRequest read FRequest;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    //Lock the server listener when execute the command
    property Locking: Boolean read FLocking write FLocking default True;
    //Concur: Synchronize the connection thread, when use it in GUI application
    property Concur: Boolean read FConcur write FConcur default False;
    //Prepare called after created in lucking mode
    procedure Prepare;
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

  { TmnCommandClasses }

  TmnCommandClasses = class(GItems<TmnCommandClassItem>)
  private
  public
    function Find(const Name: string): TmnCommandClassItem;
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
  end;

  TmnCommandServer = class;

  { TmnCustomCommandListener }

  TmnCustomCommandListener = class(TmnListener)
  private
    function ParseRequest(const Request: string): TmnRequest; virtual; abstract;
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
  public
    constructor Create;
    //Name here will corrected with registered item name for example Get -> GET
    function GetCommandClass(var CommandName: string): TmnCommandClass; virtual; abstract;
    function CreateCommand(Connection: TmnCommandConnection; var CommandName: string): TmnCommand;
  end;

  { TmnCommandListener }

  TmnCommandListener = class(TmnCustomCommandListener)
  private
    function GetServer: TmnCommandServer;
  protected
    property Server: TmnCommandServer read GetServer;
    function ParseRequest(const Request: string): TmnRequest; override;
  public
    function GetCommandClass(var CommandName: string): TmnCommandClass; override;
  end;

  TmnCommandServer = class(TmnEventServer)
  private
    FCommands: TmnCommandClasses;
  protected
    function DoCreateListener: TmnListener; override;
    property Commands: TmnCommandClasses read FCommands;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer; overload;
  end;

implementation

uses
  mnUtils;

{ TmnCommandListener }

function TmnCommandListener.ParseRequest(const Request: string): TmnRequest;
var
  aRequests: TStringList;
begin
  Finalize(Result);
  aRequests := TStringList.Create;
  try
    StrToStrings(Request, aRequests, [' '], []);
    if aRequests.Count > 0 then
    begin
      Result.Name := aRequests[0];
      Result.Method := Result.Name;
    end;
    if aRequests.Count > 1 then
      Result.Path := aRequests[1];
    if aRequests.Count > 2 then
      Result.Version := aRequests[2];
  finally
    aRequests.Free;
  end;
end;

function TmnCommandListener.GetCommandClass(var CommandName: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := Server.Commands.Find(CommandName);
  if aItem <> nil then
  begin
    CommandName := aItem.Name;
    Result := aItem.CommandClass;
  end
  else
    Result := nil;
end;

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

destructor TmnCommandConnection.Destroy;
begin
  FreeAndNil(FCommand);
  inherited;
end;

{ TmnCommandConnection }

procedure TmnCommandConnection.Process;
var
  aRequestLine: string;
  aRequest: TmnRequest;
begin
  inherited;
  if Connected then
  begin
    if FCommand = nil then
    begin
      if Connected then
      begin
        aRequestLine := Stream.ReadLine;
        aRequest := (Listener as TmnCustomCommandListener).ParseRequest(aRequestLine);

        Listener.Enter;
        try
          FCommand := (Listener as TmnCustomCommandListener).CreateCommand(Self, aRequest.Name);
          FCommand.FRequest := aRequest;
          FCommand.Prepare;
        finally
          Listener.Leave;
        end;
      end;

      if FCommand <> nil then
      begin
        try
          FCommand.FWorking := True; //TODO
          if FCommand.Locking then
            Listener.Enter;
          try
            if FCommand.Concur then
              Synchronize(FCommand.Execute)
            else
              FCommand.Execute;
          finally
            FCommand.FWorking := False;
            if FCommand.Locking then
              Listener.Leave;
          end;
        except
          if FCommand.RaiseExceptions then
            raise;
        end;
        if Stream.Connected then
          Stream.Disconnect;
        FreeAndNil(FCommand);
      end
      else
        Stream.Disconnect;
    end;
  end;
end;

{ TGuardSocketServer }

function TmnCommandServer.DoCreateListener: TmnListener;
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

{ TmnCustomCommandListener }

function TmnCustomCommandListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnCommandConnection.Create(Self, vStream);
end;

function TmnCommandListener.GetServer: TmnCommandServer;
begin
  Result := inherited Server as TmnCommandServer;
end;

procedure TmnCustomCommandListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.Timeout := -1;
end;

constructor TmnCustomCommandListener.Create;
begin
  inherited;
end;

function TmnCommandServer.RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer;
begin
  if Active then
    raise TmnCommandExceotion.Create('Server is Active');
  if FCommands.Find(vName) <> nil then
    raise TmnCommandExceotion.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
end;

function TmnCustomCommandListener.CreateCommand(Connection: TmnCommandConnection; var CommandName: string): TmnCommand;
var
  aClass: TmnCommandClass;
begin
  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Connection);
    Result.FServer := Server;
  end
  else
    Result := nil;
  //TODO make a default command if not found
end;

{ TmnCommand }

constructor TmnCommand.Create(Connection: TmnCommandConnection);
begin
  inherited Create;
  FLocking := True;
  FConnection := Connection;
end;

procedure TmnCommand.DoPrepare;
begin
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
    Connection.Stream.Drop;
end;

class function TmnCommand.GetCommandName: string;
begin
  Result := ClassName;
end;

procedure TmnCommand.Prepare;
begin
  DoPrepare;
end;

{ TmnCommandClasses }

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

function TmnCommandClasses.Add(const Name: string; CommandClass: TmnCommandClass): Integer;
var
  aItem: TmnCommandClassItem;
begin
  aItem := TmnCommandClassItem.Create;
  aItem.FName := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

end.
