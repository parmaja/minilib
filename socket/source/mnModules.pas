unit mnModules;
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
  SysUtils, Classes,
  mnClasses, mnStreams,
  mnSockets, mnConnections, mnServers;

type
  TmnCommandException = class(Exception);
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData); //TODO remove it

  TmnCommand = class;

  TmnRequest = record
    Module: string;
    Command: string;
    Method: string;
    Path: string;
    Version: string;
    Request: string; //Full of first line of header
  end;

  TmnModule = class;

  { TmnCommand }

  TmnCommand = class(TObject)
  private
    FRequest: TmnRequest;
    FModule: TmnModule;
    FLocking: Boolean;
    FRaiseExceptions: Boolean;
  protected
    FWorking: Boolean;
    procedure Execute; virtual;
    function Active: Boolean;
    procedure Shutdown;
    procedure DoPrepare; virtual;
  public
    constructor Create(AModule: TmnModule; InStream: TStream = nil; OutStream: TStream = nil); virtual;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmnModule read FModule;
    property Request: TmnRequest read FRequest;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    //Lock the server listener when execute the command
    property Locking: Boolean read FLocking write FLocking default True;
    //Prepare called after created in lucking mode
    procedure Prepare;
  end;

  TmnCommandClass = class of TmnCommand;

  TmnCommandClassItem = class(TmnNamedObject)
  private
    FCommandClass: TmnCommandClass;
  public
    property CommandClass: TmnCommandClass read FCommandClass;
  end;

  { TmnCommandClasses }

  TmnCommandClasses = class(TmnNamedObjectList<TmnCommandClassItem>)
  private
  public
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
  end;

  TmnModule = class(TmnNamedObject)
  private
    FCommands: TmnCommandClasses;
  protected
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: string): TmnCommandClass; virtual;
    function CreateCommand(var CommandName: string): TmnCommand;
  public
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer; overload;
    property Commands: TmnCommandClasses read FCommands;
    property Active: Boolean read GetActive;
  end;


  TmnModules = class(TmnNamedObjectList<TmnModule>)
  private
  protected
    function CreateCommand(ModuleName, CommandName: string): TmnCommand; virtual;
  public
    function ParseRequest(const Request: string): TmnRequest; virtual;
    function Add(const Name: string; AModule:TmnModule): Integer;
  end;

//--- Server ---

  TmnCommandServer = class;

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

  { TmnCommandListener }

  TmnCommandListener = class(TmnListener)
  private
    function GetServer: TmnCommandServer;
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
    property Server: TmnCommandServer read GetServer;
  public
  end;

  TmnCommandServer = class(TmnEventServer)
  private
    FModules: TmnModules;
  protected
    function DoCreateListener: TmnListener; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Modules: TmnModules read FModules;
  end;

implementation

uses
  mnUtils;

{ TmnCommandListener }

constructor TmnCommandServer.Create;
begin
  inherited;
  FModules := TmnModules.Create;
  Port := '81';
end;

destructor TmnCommandServer.Destroy;
begin
  FreeAndNil(FModules);
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
        aRequest := (Listener.Server as TmnCommandServer).Modules.ParseRequest(aRequestLine);

        Listener.Enter;
        try
          FCommand := (Listener.Server as TmnCommandServer).Modules.CreateCommand(aRequest.Module, aRequest.Command);
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

{ TmnCustomCommandListener }

function TmnCommandListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnCommandConnection.Create(Self, vStream);
end;

procedure TmnCommandListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.Timeout := -1;
end;

function TmnCommandListener.GetServer: TmnCommandServer;
begin
  Result := inherited Server as TmnCommandServer;
end;

{ TmnCommand }

constructor TmnCommand.Create(AModule: TmnModule; InStream, OutStream: TStream);
begin
  inherited Create;
  FLocking := True;
  FModule := Module;
end;

procedure TmnCommand.DoPrepare;
begin
end;

procedure TmnCommand.Execute;
begin
end;

function TmnCommand.Active: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmnCommand.Shutdown;
begin
{  if Connected and (Connection.Stream <> nil) then
    Connection.Stream.Drop;}
end;

procedure TmnCommand.Prepare;
begin
  DoPrepare;
end;

function TmnCommandClasses.Add(const Name: string; CommandClass: TmnCommandClass): Integer;
var
  aItem: TmnCommandClassItem;
begin
  aItem := TmnCommandClassItem.Create;
  aItem.Name := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

{ TmnModule }

function TmnModule.CreateCommand(var CommandName: string): TmnCommand;
var
  aClass: TmnCommandClass;
begin
  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self);
  end
  else
    Result := nil;
  //TODO make a default command if not found
end;

function TmnModule.GetCommandClass(var CommandName: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := Commands.Find(CommandName);
  if aItem <> nil then
  begin
    CommandName := aItem.Name;
    Result := aItem.CommandClass;
  end
  else
    Result := nil;
end;

function TmnModule.GetActive: Boolean;
begin
  Result := True; //todo
end;

function TmnModule.RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer;
begin
  if Active then
    raise TmnCommandException.Create('Server is Active');
  if FCommands.Find(vName) <> nil then
    raise TmnCommandException.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
end;

{ TmnModules }

function TmnModules.Add(const Name: string; AModule:TmnModule): Integer;
begin
  AModule.Name := Name;
  Result := inherited Add(AModule);
end;

function TmnModules.CreateCommand(ModuleName, CommandName: string): TmnCommand;
var
  aModule: TmnModule;
begin
  aModule := Find(ModuleName);
  if aModule = nil then
    aModule := Items[0];
  Result := aModule.CreateCommand(CommandName);
end;

function TmnModules.ParseRequest(const Request: string): TmnRequest;
var
  aRequests: TStringList;
begin
  Finalize(Result);
  aRequests := TStringList.Create;
  try
    StrToStrings(Request, aRequests, [' '], []);
    if aRequests.Count > 0 then
    begin
      Result.Module := aRequests[0];
      Result.Method := Result.Module;
    end;
    if aRequests.Count > 1 then
      Result.Path := aRequests[1];
    if aRequests.Count > 2 then
      Result.Version := aRequests[2];
  finally
    aRequests.Free;
  end;
end;

end.
