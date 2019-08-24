unit mnCommands;
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
  mnConnections;

type
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData); //TODO remove it

  TmnCommand = class;

  { TmnCommandConnection }

  TmnCommandConnection = class(TmnConnection)
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
    FConnection: TmnCommandConnection;
    FRaiseExceptions: Boolean;
  protected
    procedure Enter; virtual;
    procedure Leave; virtual;
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
    //Prepare called after created in lucking mode
    procedure Prepare;
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

  TmnCommandClasses = class(TmnObjectList<TmnCommandClassItem>)
  private
  public
    function Find(const Name: string): TmnCommandClassItem;
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer;
    function GetCommandClass(var CommandName: string): TmnCommandClass; virtual;
    function CreateCommand(Connection: TmnCommandConnection; var CommandName: string): TmnCommand;
  end;

  { TmnCommands }

  TmnCommands = class(TmnObjectList<TmnCommand>)
  private
    FCommandClasses: TmnCommandClasses;
  public
    function ParseRequest(const Request: string): TmnRequest; virtual;
    property CommandClasses: TmnCommandClasses read FCommandClasses;
  end;

implementation

uses
  mnUtils;

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
        //aRequest := ParseRequest(aRequestLine);

        FCommand.Enter;
        try
          //FCommand := CreateCommand(Self, aRequest.Name);
          FCommand.FRequest := aRequest;
          FCommand.Prepare;
        finally
          FCommand.Leave;
        end;
      end;

      if FCommand <> nil then
      begin
        try
          FCommand.Enter;
          try
            FCommand.Execute;
          finally
            FCommand.Leave;
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

{ TmnCommand }

constructor TmnCommand.Create(Connection: TmnCommandConnection);
begin
  inherited Create;
  FConnection := Connection;
end;

procedure TmnCommand.DoPrepare;
begin
end;

procedure TmnCommand.Enter;
begin

end;

procedure TmnCommand.Leave;
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

function TmnCommandClasses.RegisterCommand(vName: string; CommandClass: TmnCommandClass): Integer;
begin
  if Find(vName) <> nil then
    raise Exception.Create('Command already exists: ' + vName);
  Result := Add(vName, CommandClass);
end;

function TmnCommandClasses.GetCommandClass(var CommandName: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := Find(CommandName);
  if aItem <> nil then
  begin
    CommandName := aItem.Name;
    Result := aItem.CommandClass;
  end
  else
    Result := nil;
end;

function TmnCommandClasses.CreateCommand(Connection: TmnCommandConnection; var CommandName: string): TmnCommand;
var
  aClass: TmnCommandClass;
begin
  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
    Result := aClass.Create(Connection)
  else
    Result := nil;
  //TODO make a default command if not found
end;

function TmnCommands.ParseRequest(const Request: string): TmnRequest;
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

end.
