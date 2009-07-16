unit sqlvSessions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, mncSchemes,
  mncSQL, mncConnections, mncSQLite, mncSQLiteSchemes;

type
  TsqlvOnNotifySession = procedure of object;

  { TsqlvSession }

  TsqlvSession = class(TObject)
  private
    FDBConnection: TmncSQLiteConnection;
    FDBSession: TmncSQLiteSession;
    FTables: TmncSchemeItems;
    FGenerators: TmncSchemeItems;
    FProceduers: TmncSchemeItems;
    FViews: TmncSchemeItems;
    FFunctions: TmncSchemeItems;
    FExceptions: TmncSchemeItems;
    FDomains: TmncSchemeItems;
    FFields: TmncSchemeItems;
    FOnDisconnected: TsqlvOnNotifySession;
    FOnConnected: TsqlvOnNotifySession;
    FOnSessionStarted: TsqlvOnNotifySession;
    FOnSessionStoped: TsqlvOnNotifySession;
    procedure RunLoginSQL;
    procedure RunLogoutSQL;
    procedure ConnectionAfterConnect(Sender: TObject);
    procedure ConnectionAfterDisconnect(Sender: TObject);
    procedure SessionStarted(Sender: TObject);
    procedure SessionStopped(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadScheme;
    procedure Open(Name:string);
    procedure Close;
    function IsActive: Boolean;
    procedure Connected;
    procedure Disconnected;
    property OnConnected: TsqlvOnNotifySession read FOnConnected write FOnConnected;
    property OnDisconnected: TsqlvOnNotifySession read FOnDisconnected write FOnDisconnected;
    property OnSessionStarted: TsqlvOnNotifySession read FOnSessionStarted write FOnSessionStarted;
    property OnSessionStoped: TsqlvOnNotifySession read FOnSessionStoped write FOnSessionStoped;
    property DBConnection: TmncSQLiteConnection read FDBConnection;
    property DBSession: TmncSQLiteSession read FDBSession;
    property Tables: TmncSchemeItems read FTables;
    property Proceduers: TmncSchemeItems read FProceduers;
    property Views: TmncSchemeItems read FViews;
    property Generators: TmncSchemeItems read FGenerators;
    property Functions: TmncSchemeItems read FFunctions;
    property Exceptions: TmncSchemeItems read FExceptions;
    property Domains: TmncSchemeItems read FDomains;
    property Fields: TmncSchemeItems read FFields;
  end;

implementation

uses
  sqlvClasses;

{ TsqlvSession }

procedure TsqlvSession.Connected;
begin
  DBSession.Start;
  LoadScheme;
  if Assigned(FOnConnected) then
    FOnConnected;
  RunLoginSQL;
end;

constructor TsqlvSession.Create;
begin
  inherited;
  FDBConnection := TmncSQLiteConnection.Create;
  FDBSession := TmncSQLiteSession.Create(DBConnection);
  FTables := TmncSchemeItems.Create;
  FProceduers := TmncSchemeItems.Create;
  FViews := TmncSchemeItems.Create;
  FGenerators := TmncSchemeItems.Create;
  FExceptions := TmncSchemeItems.Create;
  FFunctions := TmncSchemeItems.Create;
  FDomains := TmncSchemeItems.Create;
  FFields := TmncSchemeItems.Create;
end;

destructor TsqlvSession.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FProceduers);
  FreeAndNil(FViews);
  FreeAndNil(FGenerators);
  FreeAndNil(FExceptions);
  FreeAndNil(FFunctions);
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  FreeAndNil(FDBSession);
  FreeAndNil(FDBConnection);
  inherited;
end;

procedure TsqlvSession.Disconnected;
begin
  RunLogoutSQL;
  if Assigned(FOnDisconnected) then
    FOnDisconnected;
end;

procedure TsqlvSession.LoadScheme;
var
  Scheme: TmncSQLiteScheme;
begin
  Scheme := TmncSQLiteScheme.Create;
  try
    Scheme.Session := DBSession;
    Scheme.EnumObject(Tables, sokTable, '', [ekSystem]);
    Scheme.EnumObject(Views, sokView);
    Scheme.EnumObject(Proceduers, sokProcedure);
    Scheme.EnumObject(Generators, sokGenerator);
    Scheme.EnumObject(Functions, sokFunction);
    Scheme.EnumObject(Exceptions, sokException);
    Scheme.EnumObject(Domains, sokDomain);
    if sqlvEngine.Setting.LoadFieldsToAutoComplete then
      Scheme.EnumObject(Fields, sokFields);
  finally
    Scheme.Free;
  end;
end;

procedure TsqlvSession.Open(Name: string);
begin
  DBConnection.Resource := Name;
  DBConnection.Connect;
  DBSession.Start;
  sqlvEngine.AddRecent(Name);
  sqlvEngine.SaveRecents;
end;

procedure TsqlvSession.Close;
begin
  DBSession.Stop;
  DBConnection.Disconnect;
end;

function TsqlvSession.IsActive: Boolean;
begin
  Result := DBSession.Active;
end;

procedure TsqlvSession.RunLoginSQL;
var
  CMD: TmncSQLiteCommand;
begin
  CMD := TmncSQLiteCommand.Create(nil);
  try
    CMD.Session := DBSession;
    if sqlvEngine.Setting.InternalLoginSQL <> '' then
    begin
      CMD.SQL.Text := sqlvEngine.Setting.InternalLoginSQL;
      CMD.Execute;
      CMD.Commit;
    end;
    if sqlvEngine.Setting.LoginSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := sqlvEngine.Setting.LoginSQL;
      CMD.Execute;
      CMD.Commit;
    end;
  finally
    CMD.Free;
  end;
end;

procedure TsqlvSession.RunLogoutSQL;
var
  CMD: TmncSQLiteCommand;
begin
  CMD := TmncSQLiteCommand.Create(nil);
  try
    CMD.Session := DBSession;
    if sqlvEngine.Setting.InternalLogoutSQL <> '' then
    begin
      CMD.SQL.Text := sqlvEngine.Setting.InternalLogoutSQL;
      CMD.Execute;
      CMD.Commit;
    end;
    if sqlvEngine.Setting.LogoutSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := sqlvEngine.Setting.LogoutSQL;
      CMD.Execute;
      CMD.Commit;
    end;
  finally
    CMD.Free;
  end;
end;

procedure TsqlvSession.ConnectionAfterConnect(Sender: TObject);
begin
  Connected;
end;

procedure TsqlvSession.ConnectionAfterDisconnect(Sender: TObject);
begin
  Disconnected;
end;

procedure TsqlvSession.SessionStarted(Sender: TObject);
begin
  if Assigned(FOnSessionStarted) then
    FOnSessionStarted();
end;

procedure TsqlvSession.SessionStopped(Sender: TObject);
begin
  if Assigned(FOnSessionStoped) then
    FOnSessionStoped();
end;

end.

