unit sqlvSessions;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}


{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, mncSchemas,
  mncSQL, mncConnections, mncSQLite, mncSQLiteSchemas;

type
  TsqlvOnNotifySession = procedure of object;

  { TsqlvSession }

  TsqlvSession = class(TObject)
  private
    FDBConnection: TmncSQLiteConnection;
    FDBSession: TmncSQLiteSession;
    FTables: TmncSchemaItems;
    FSequences: TmncSchemaItems;
    FProceduers: TmncSchemaItems;
    FViews: TmncSchemaItems;
    FFunctions: TmncSchemaItems;
    FExceptions: TmncSchemaItems;
    FDomains: TmncSchemaItems;
    FFields: TmncSchemaItems;
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
    procedure LoadSchema;
    procedure Open(Name:string; vAutoCreate: Boolean);
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
    property Tables: TmncSchemaItems read FTables;
    property Proceduers: TmncSchemaItems read FProceduers;
    property Views: TmncSchemaItems read FViews;
    property Sequences: TmncSchemaItems read FSequences;
    property Functions: TmncSchemaItems read FFunctions;
    property Exceptions: TmncSchemaItems read FExceptions;
    property Domains: TmncSchemaItems read FDomains;
    property Fields: TmncSchemaItems read FFields;
  end;

implementation

uses
  sqlvClasses;

{ TsqlvSession }

procedure TsqlvSession.Connected;
begin
  DBSession.Start;
  LoadSchema;
  RunLoginSQL;
  if Assigned(FOnConnected) then
    FOnConnected;
end;

constructor TsqlvSession.Create;
begin
  inherited;
  FDBConnection := TmncSQLiteConnection.Create;
  FDBSession := TmncSQLiteSession.Create(DBConnection);
  FDBConnection.OnConnected :=  @ConnectionAfterConnect;
  FDBConnection.OnDisconnected :=  @ConnectionAfterDisconnect;

  FTables := TmncSchemaItems.Create;
  FProceduers := TmncSchemaItems.Create;
  FViews := TmncSchemaItems.Create;
  FSequences := TmncSchemaItems.Create;
  FExceptions := TmncSchemaItems.Create;
  FFunctions := TmncSchemaItems.Create;
  FDomains := TmncSchemaItems.Create;
  FFields := TmncSchemaItems.Create;
end;

destructor TsqlvSession.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FProceduers);
  FreeAndNil(FViews);
  FreeAndNil(FSequences);
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

procedure TsqlvSession.LoadSchema;
var
  Schema: TmncSQLiteSchema;
begin
  if sqlvEngine.Setting.CacheSchemas then
  begin
    Schema := TmncSQLiteSchema.Create;
    try
      Schema.Session := DBSession;
      Schema.EnumObject(Tables, sokTable, '', [ekSystem, ekSort]);
      Schema.EnumObject(Views, sokView, '', [ekSort]);
      Schema.EnumObject(Proceduers, sokProcedure, '', [ekSort]);
      Schema.EnumObject(Sequences, sokSequences, '', [ekSort]);
      Schema.EnumObject(Functions, sokFunction, '', [ekSort]);
      Schema.EnumObject(Exceptions, sokException, '', [ekSort]);
      Schema.EnumObject(Domains, sokDomains, '', [ekSort]);
      Schema.EnumObject(Fields, sokFields);
    finally
      Schema.Free;
    end;
  end;
end;

procedure TsqlvSession.Open(Name: string; vAutoCreate: Boolean);
begin
  DBConnection.Resource := Name;
  DBConnection.AutoCreate := vAutoCreate;
  DBConnection.Connect;
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

