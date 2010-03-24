unit mncSQLite;
{**
 *  This file is part of the "Mini Connections"
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
  Classes, SysUtils, Variants, StrUtils,
  {$ifdef FPC}
  sqlite3,
  {$else}
  mncSQLiteHeader,
  {$endif}
  mnUtils, mnStreams, mncConnections, mncSQL;

type

  { TmncSQLiteConnection }

  TmncSQLiteConnection = class(TmncConnection)
  private
    FDBHandle: PSqlite3;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    class function GetMode:TmncTransactionMode; override;
    procedure CheckError(Error: Integer; const ExtraMsg: string = '');
  public
    constructor Create;
    procedure Interrupt;
    procedure Execute(SQL: string);
    property DBHandle: PSqlite3 read FDBHandle;
  end;

  { TmncSQLiteSession }

  TmncSQLiteSession = class(TmncSession)
  private
    FExclusive: Boolean;
    function GetConnection: TmncSQLiteConnection;
    procedure SetConnection(const AValue: TmncSQLiteConnection);
    procedure SetExclusive(const AValue: Boolean);
  protected
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure DoStop; override;
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(SQL: string);
    function GetLastInsertID: Int64;
    function GetRowsChanged: Integer;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Connection: TmncSQLiteConnection read GetConnection write SetConnection;
  end;

  { TmncSQLiteCommand }

  TmncSQLiteCommand = class(TmncSQLCommand)
  private
    FStatment: Psqlite3_stmt;
    FTail: pchar;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetConnection: TmncSQLiteConnection;
    procedure FetchColumns;
    procedure FetchValues;
    procedure ApplyParams;
    function GetSession: TmncSQLiteSession;
    procedure SetSession(const AValue: TmncSQLiteSession);
  protected
    procedure CheckError(Error:longint);
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF:Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    property Connection:TmncSQLiteConnection read GetConnection;
    property Session: TmncSQLiteSession read GetSession write SetSession;
  public
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    function GetLastInsertID: Int64;
    property Statment: Psqlite3_stmt read FStatment;
  end;

implementation

function SQLTypeToType(vType: Integer): TmncColumnType;
begin
  case vType of
    SQLITE_INTEGER: Result := ftInteger;
    SQLITE_FLOAT: Result := ftFloat;
    SQLITE_BLOB: Result := ftBlob;
    SQLITE_NULL: Result := ftNull;
    SQLITE_TEXT: Result := ftString;
    else
      Result := ftUnkown;
  end;
end;


procedure TmncSQLiteConnection.CheckError(Error:longint; const ExtraMsg: string);
var
  s : Utf8String;
begin
  if (Error <> SQLITE_OK) then
  begin
    s := 'sqlite: ' + sqlite3_errmsg(FDBHandle);
    if ExtraMsg <> '' then
      s := s + ' - ' + ExtraMsg;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

{ TmncSQLiteConnection }

constructor TmncSQLiteConnection.Create;
begin
  inherited Create;
end;

procedure TmncSQLiteConnection.Interrupt;
begin
  sqlite3_interrupt(DBHandle);
end;

procedure TmncSQLiteConnection.DoConnect;
begin
  if not AutoCreate and not FileExists(Resource) then
    raise EmncException.Create('Database not exist: "' + Resource + '"');
  CheckError(sqlite3_open(PChar(Resource), @FDBHandle), Resource);
end;

function TmncSQLiteConnection.GetConnected: Boolean;
begin
  Result := FDBHandle <> nil;
end;

procedure TmncSQLiteConnection.DoDisconnect;
begin
  CheckError(sqlite3_close(FDBHandle));
  FDBHandle := nil;
end;

{ TmncSQLiteSession }

destructor TmncSQLiteSession.Destroy;
begin
  inherited;
end;

procedure TmncSQLiteSession.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncSQLiteSession.DoStart;
begin
  Execute('BEGIN');
end;

procedure TmncSQLiteSession.DoCommit;
begin
  Execute('COMMIT');
end;

procedure TmncSQLiteSession.DoRollback;
begin
  Execute('ROLLBACK');
end;

procedure TmncSQLiteSession.DoStop;
begin
  //Nothing to do
end;

procedure TmncSQLiteConnection.Execute(SQL: string);
var
 lMsg  : PChar;
 s : Utf8String;
 r  : integer;
begin
  lMSg := nil;
  s := SQL;
  r := sqlite3_exec(FDBHandle, PChar(s), nil, nil, @lMsg);
  if lMSg <> nil then
  begin
    s := lMsg;
    sqlite3_free(lMSg);
  end;
  CheckError(r, s);
end;

function TmncSQLiteSession.GetLastInsertID: Int64;
begin
  CheckActive;
  Result := sqlite3_last_insert_rowid(Connection.DBHandle);
end;

function TmncSQLiteSession.GetRowsChanged: Integer;
begin
  CheckActive;
  Result := sqlite3_changes(Connection.DBHandle);
end;

function TmncSQLiteSession.GetActive: Boolean;
begin
  Result:= inherited GetActive;
end;

constructor TmncSQLiteSession.Create(vConnection: TmncConnection);
begin
  inherited;
end;

function TmncSQLiteSession.GetConnection: TmncSQLiteConnection;
begin
  Result := inherited Connection as TmncSQLiteConnection;
end;

class function TmncSQLiteConnection.GetMode: TmncTransactionMode;
begin
  Result := smEmulateTransaction;
end;

procedure TmncSQLiteSession.SetConnection(const AValue: TmncSQLiteConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncSQLiteSession.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    FExclusive := AValue;
    if Active then
      raise EmncException.Create('You can not set Exclusive when session active');
  end;
end;

procedure TmncSQLiteSession.DoInit;
begin
  Execute('PRAGMA TEMP_STORE=MEMORY');//for WINCE
  Execute('PRAGMA full_column_names=0');
  Execute('PRAGMA short_column_names=1');
  Execute('PRAGMA encoding = "UTF-8"');
  if Exclusive then
    Execute('PRAGMA locking_mode = EXCLUSIVE')
  else
    Execute('PRAGMA locking_mode = NORMAL');
end;

{ TmncSQLiteCommand }

procedure TmncSQLiteCommand.CheckError(Error: longint);
var
  s : Utf8String;
  ExtraMsg: string;
  r: Integer;
begin
  if (Error <> SQLITE_OK) then
  begin
    s := 'sqlite: ' + sqlite3_errmsg(Connection.DBHandle);
    if Active then
    begin
      r := sqlite3_finalize(FStatment);//without check error prevent the loop
      if (r <> SQLITE_OK) then
        ExtraMsg := sqlite3_errmsg(Connection.DBHandle)
      else
        ExtraMsg := '';
      if ExtraMsg <> '' then
        s := s + ' - ' + ExtraMsg;
      FStatment := nil;
    end;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

function TmncSQLiteCommand.GetSession: TmncSQLiteSession;
begin
  Result := inherited Session as TmncSQLiteSession;
end;

procedure TmncSQLiteCommand.SetSession(const AValue: TmncSQLiteSession);
begin
  inherited Session := AValue;
end;

procedure TmncSQLiteCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

function TmncSQLiteCommand.GetEOF: Boolean;
begin
  Result := (FStatment = nil) or FEOF; 
end;

function TmncSQLiteCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

function TmncSQLiteCommand.GetLastInsertID: Int64;
begin
  Result := Session.GetLastInsertID;
end;

procedure TmncSQLiteCommand.ApplyParams;
var
  s: UTF8String;
  i: Integer;
  d: Double;
  c: Currency;
  t: Integer;
  t64: Int64;
begin
  for i := 0 to ParamList.Count - 1 do
  begin
    ParamList.Items[i].FreeBuffer;
  end;

  for i := 0 to ParamList.Count - 1 do
  begin
    if ParamList.Items[i].IsEmpty then
      CheckError(sqlite3_bind_null(FStatment, i + 1))
    else
    begin
      case VarType(ParamList.Items[i].Value) of
        varDate:
        begin
          d := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        varInteger:
        begin
          t := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, t));
        end;
        varint64:
        begin
          t64 := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_int64(FStatment, i + 1, t64));
        end;
        varCurrency:
        begin
          c := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, c));
        end;
        varDouble:
        begin
          d := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        else
        begin
          if not ParamList.Items[i].BufferAllocated then
          begin
            s := VarToStrDef(ParamList.Items[i].Value, '');
            ParamList.Items[i].AllocBuffer(PChar(s)^, Length(s));
          end;
          CheckError(sqlite3_bind_text(FStatment, i + 1, PChar(ParamList.Items[i].Buffer), ParamList.Items[i].BufferSize, nil));
        end;
      end;
    end;
  end;
end;

procedure TmncSQLiteCommand.DoExecute;
begin
  FBOF := True;
  if FStatment <> nil then
    CheckError(sqlite3_reset(FStatment));
  ApplyParams;
end;

procedure TmncSQLiteCommand.DoNext;
var
  r: Integer;
begin
//  CheckError(sqlite3_step(@FStatment));
  r:=sqlite3_step(FStatment);
  if (r = SQLITE_ROW) then
  begin
    if FBOF then
      FetchColumns;
    FetchValues;
    FEOF := False;
  end
  else if (r = SQLITE_DONE) then
  begin
    FEOF := True;
    CheckError(sqlite3_reset(FStatment));
  end
  else
    CheckError(r);
  FBOF := False;
end;

procedure TmncSQLiteCommand.DoPrepare;
var
  s:string;
  r: Integer;
begin
  FBOF := True;
  s := ParseSQL([]);
//  sqlite3_prepare_v2
  r := sqlite3_prepare(Connection.DBHandle, PChar(s), -1 , @FStatment, @FTail);
  CheckError(r);
end;

procedure TmncSQLiteCommand.DoRollback;
begin
  Session.Rollback;
end;

procedure TmncSQLiteCommand.DoClose;
begin
  CheckError(sqlite3_finalize(FStatment));
//  CheckError(sqlite3_clear_bindings(FStatment));//not found in SQLite3 for WinCE
  FStatment := nil;  
end;

procedure TmncSQLiteCommand.DoCommit;
begin
  Session.Commit;
end;

procedure TmncSQLiteCommand.FetchColumns;
var
  i: Integer;
  c: Integer;
  aName: string;
  aType: Integer;
  //aSize: Integer;
begin
  Columns.Clear;
  c := sqlite3_column_count(FStatment);
  for i := 0 to c -1 do
  begin
//    sqlite3_column_type
    aName :=  DequoteStr(sqlite3_column_name(FStatment, i));
    aType := sqlite3_column_type(FStatment, i);
    Columns.Add(aName, SQLTypeToType(aType));
  end;
end;

procedure TmncSQLiteCommand.FetchValues;
var
  i: Integer;
  c: Integer;
  int:Int64;
{$ifdef fpc}
  str: string;
{$else}
  str: utf8string;
{$endif}
  flt: Double;
  aCurrent: TmncFields;
  aType: Integer;
  //aSize: Integer;
begin
  c := sqlite3_column_count(FStatment);
  if c > 0 then
  begin
    aCurrent := TmncFields.Create(Fields);
    for i := 0 to c - 1 do
    begin
//    TStorageType = (stNone, stInteger, stFloat, stText, stBlob, stNull);
      aType := sqlite3_column_type(FStatment, i);
      //aSize := sqlite3_column_bytes(FStatment, i);
      case aType of
        SQLITE_NULL:
        begin
          aCurrent.Add(i, Null);
        end;
        SQLITE_INTEGER:
        begin
          int := sqlite3_column_int(FStatment, i);
          aCurrent.Add(i, int);
        end;
        SQLITE_FLOAT:
        begin
          flt := sqlite3_column_double(FStatment, i);
          aCurrent.Add(i, flt);
        end;
        else
        begin
          str := sqlite3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end;
      end;
    end;
    Current := aCurrent;
  end;
end;

function TmncSQLiteCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil; 
end;

function TmncSQLiteCommand.GetConnection: TmncSQLiteConnection;
begin
  Result := Session.Connection as TmncSQLiteConnection;
end;

end.

