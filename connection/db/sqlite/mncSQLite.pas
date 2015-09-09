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
  Classes, SysUtils, Variants,
  mncSQLiteHeader, mncCommons, mncSchemas,
  mnUtils, mncConnections, mncSQL;

const
  SQLITE_OPEN_FULLMUTEX        = $00010000;
  SQLITE_OPEN_SHAREDCACHE      = $00020000;
  SQLITE_OPEN_PRIVATECACHE     = $00040000;
  SQLITE_OPEN_WAL              = $00080000;

type
  TmncTempStore = (tmpDefault, tmpFile, tmpMemory);
  TmncJournalMode = (jrmDefault, jrmDelete, jrmTruncate, jrmPersist, jrmMemory, jrmWal, jrmOff);
  TmncSynchronous = (syncDefault, syncOFF, syncNormal,  syncFull);

  { TmncSQLiteConnection }

  TmncSQLiteConnection = class(TmncSQLConnection)
  private
    FDBHandle: PSqlite3;
    FExclusive: Boolean;
    FJournalMode: TmncJournalMode;
    FReadCommited: Boolean;
    FSynchronous: TmncSynchronous;
    FTempStore: TmncTempStore;
    procedure SetExclusive(const AValue: Boolean);
    procedure SetJournalMode(const AValue: TmncJournalMode);
    procedure SetReadCommited(const AValue: Boolean);
    procedure SetTempStore(const AValue: TmncTempStore);
  protected
    procedure InitPragma; virtual;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    procedure CheckError(Error: Integer; const ExtraMsg: string = '');
    procedure DoInit; override;
  public
    constructor Create;
    class function Model: TmncConnectionModel; override;
    function CreateSession: TmncSQLSession; overload; override; 
    procedure Interrupt;
    procedure Vacuum;
    function GetVersion: string;
    procedure Execute(Command: string); override;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property ReadCommited: Boolean read FReadCommited write SetReadCommited;
    property Synchronous: TmncSynchronous read FSynchronous write FSynchronous default syncDefault;
    property JournalMode: TmncJournalMode read FJournalMode write SetJournalMode default jrmDefault;
    property TempStore: TmncTempStore read FTempStore write SetTempStore default tmpDefault;
    {TODO
      ANALYZE
    }
    property DBHandle: PSqlite3 read FDBHandle;
  end;

  { TmncSQLiteSession }

  TmncSQLiteSession = class(TmncSQLSession)
  private
    function GetConnection: TmncSQLiteConnection;
    procedure SetConnection(const AValue: TmncSQLiteConnection);
  protected
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    function CreateCommand: TmncSQLCommand; override;
    function CreateSchema: TmncSchema; override;
    procedure Execute(SQL: string);
    function GetLastInsertID: Int64;
    function GetRowsChanged: Integer;
    property Connection: TmncSQLiteConnection read GetConnection write SetConnection;
  end;

  { TmncSQLiteField }

  TmncSQLiteField = class(TmncField)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  { TmncSQLiteParam }

  TmncSQLiteParam = class(TmncParam)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TmncSQLiteFields }

  TmncSQLiteFields = class(TmncFields)
  protected
    function CreateField(vColumn: TmncColumn): TmncField; override;
  end;

  { TmncSQLiteParams }

  TmncSQLiteParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  end;

  { TmncSQLiteBind }

  TmncSQLiteBind = class(TmncBind)
  private
    FBuffer: Pointer;
    FBufferSize: Integer;
    function GetBufferAllocated: Boolean;
  protected
    procedure AllocBuffer(var P; Size: Integer); virtual;
    procedure FreeBuffer;
    property Buffer: Pointer read FBuffer;
    property BufferSize: Integer read FBufferSize;
    property BufferAllocated: Boolean read GetBufferAllocated;
  public
    destructor Destroy; override;
  end;

  { TmncSQLiteBinds }

  TmncSQLiteBinds = class(TmncBinds)
  private
    function GetItem(Index: Integer): TmncSQLiteBind;
  protected
    function CreateBind: TmncBind; override;
  public
    property Items[Index: Integer]: TmncSQLiteBind read GetItem; default;
  end;

  { TmncSQLiteCommand }

  TmncSQLiteCommand = class(TmncSQLCommand)
  private
    FStatment: Psqlite3_stmt;
    FTail: pchar;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetBinds: TmncSQLiteBinds;
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
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    function CreateBinds: TmncBinds; override;
    property Binds: TmncSQLiteBinds read GetBinds;
  public
    property Connection:TmncSQLiteConnection read GetConnection;
    property Session: TmncSQLiteSession read GetSession write SetSession;
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    function GetLastInsertID: Int64;
    property Statment: Psqlite3_stmt read FStatment;
  end;

function SQLiteJournalModeToStr(JournalMode: TmncJournalMode): string;
function SQLiteTempStoreToStr(TempStore: TmncTempStore): string;
function SQLiteSynchronousToStr(Synchronous: TmncSynchronous): string;

implementation

uses
  mncDB, mncSQLiteSchemas;

function SQLiteJournalModeToStr(JournalMode: TmncJournalMode): string;
begin
  case JournalMode of
    jrmDefault:
    {$ifdef WINCE}
      Result := 'MEMORY'; //or MEMORY
    {$else}
      Result := 'DELETE';
    {$endif}
    jrmDelete: Result := 'DELETE';
    jrmTruncate: Result := 'TRUNCATE';
    jrmPersist: Result := 'PERSIST';
    jrmMemory: Result := 'MEMORY';
    jrmWal: Result := 'WAL';
    jrmOff: Result := 'OFF';
  end;
end;

function SQLiteTempStoreToStr(TempStore: TmncTempStore): string;
begin
  case TempStore of
    tmpDefault:
    {$ifdef WINCE}
      Result := 'MEMORY'; //or MEMORY
    {$else}
      Result := 'DEFAULT';
    {$endif}
    tmpFile: Result := 'FILE';
    tmpMemory: Result := 'MEMORY';
  end;
end;

function SQLiteSynchronousToStr(Synchronous: TmncSynchronous): string;
begin
  case Synchronous of
    syncDefault:
    {$ifdef WINCE}
      Result := 'NORMAL'; //or MEMORY
    {$else}
      Result := 'FULL';
    {$endif}
    syncOFF: Result := 'OFF';
    syncNormal: Result := 'NORMAL';
    syncFull: Result := 'FULL';
  end;

end;

function SQLTypeToType(vType: Integer; const SchemaType: string): TmncDataType;
begin
  case vType of
    SQLITE_INTEGER:
      if SameText(SchemaType, 'date') then
        Result := dtDate
      else//not yet
        Result := dtInteger;
    SQLITE_FLOAT:
    begin
      if SameText(SchemaType, 'date') then
        Result := dtDateTime
      else//not yet
        Result := dtFloat;
    end;
    SQLITE_BLOB: Result := dtBlob;
    SQLITE_NULL: Result := dtUnknown;
    SQLITE_TEXT:
    begin
      if SameText(SchemaType, 'Blob') then
        Result := dtBlob
      else
        Result := dtString;
    end
    else
      Result := dtUnknown;
  end;
end;

{ TmncSQLiteBinds }

function TmncSQLiteBinds.GetItem(Index: Integer): TmncSQLiteBind;
begin
  Result := inherited Items[Index] as TmncSQLiteBind;
end;

function TmncSQLiteBinds.CreateBind: TmncBind;
begin
  Result := TmncSQLiteBind.Create;
end;

function TmncSQLiteParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncSQLiteParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

constructor TmncSQLiteParam.Create;
begin
  inherited;
end;

destructor TmncSQLiteParam.Destroy;
begin
  inherited;
end;

function TmncSQLiteBind.GetBufferAllocated: Boolean;
begin
  Result := Buffer <> nil;
end;

procedure TmncSQLiteBind.AllocBuffer(var P; Size: Integer);
begin
  FreeBuffer;
  FBufferSize := Size;
  if Size > 0 then
  begin
    FBuffer := AllocMem(FBufferSize);
    Move(P, FBuffer^, Size);
  end;
end;

procedure TmncSQLiteBind.FreeBuffer;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
end;

destructor TmncSQLiteBind.Destroy;
begin
  FreeBuffer;
  inherited;
end;

function TmncSQLiteField.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncSQLiteField.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncSQLiteFields }

function TmncSQLiteFields.CreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncSQLiteField.Create(vColumn);
end;

{ TmncSQLiteParams }

function TmncSQLiteParams.CreateParam: TmncParam;
begin
  Result := TmncSQLiteParam.Create;
end;

procedure TmncSQLiteConnection.CheckError(Error: Integer; const ExtraMsg: string);
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

class function TmncSQLiteConnection.Model: TmncConnectionModel;
begin
  Result.Name := 'SQLite';
  Result.Title := 'SQLite Database';
  Result.Capabilities := [ccDB, ccSQL, ccTransaction];
  Result.SchemaClass := TmncSQLiteSchema;
end;

function TmncSQLiteConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncSQLiteSession.Create(Self);
end;

procedure TmncSQLiteConnection.Interrupt;
begin
  sqlite3_interrupt(DBHandle);
end;

procedure TmncSQLiteConnection.Vacuum;
begin
  Execute('vacuum');
end;

function TmncSQLiteConnection.GetVersion: string;
var
  p: PChar;
begin
  p := sqlite3_version();
  if p <> nil then
    Result := p
  else
    Result := '';
end;

procedure TmncSQLiteConnection.DoConnect;
var
  f: Integer;
begin
  f := SQLITE_OPEN_READWRITE or SQLITE_OPEN_SHAREDCACHE;
  if not FileExists(Resource) then
  begin
    if AutoCreate then
    begin
      f := f or SQLITE_OPEN_CREATE;
      SetState(cstCreated);
    end
    else
      raise EmncException.Create('Database not exist: "' + Resource + '"');
  end;
  //  CheckError(sqlite3_enable_shared_cache(1));
  CheckError(sqlite3_open_v2(PChar(Resource), @FDBHandle, f, nil), Resource);
  InitPragma;
end;

function TmncSQLiteConnection.GetConnected: Boolean;
begin
  Result := FDBHandle <> nil;
end;

procedure TmncSQLiteConnection.DoDisconnect;
begin
  CheckError(sqlite3_close(FDBHandle));
  FDBHandle := nil;
  {$ifdef FPC}
  ReleaseSQLite;
  {$endif}
end;

{ TmncSQLiteSession }

destructor TmncSQLiteSession.Destroy;
begin
  inherited;
end;

function TmncSQLiteSession.CreateCommand: TmncSQLCommand;
begin
  Result := TmncSQLiteCommand.Create;
  Result.Session := Self;
end;

function TmncSQLiteSession.CreateSchema: TmncSchema;
begin
  Result := TmncSQLiteSchema.CreateBy(Self);
end;

procedure TmncSQLiteSession.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncSQLiteSession.DoStart;
begin
  Execute('BEGIN');
end;

procedure TmncSQLiteSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
begin
  case How of
    sdaCommit: Execute('COMMIT');
    sdaRollback: Execute('ROLLBACK');
  end;
  if Retaining then
    Execute('BEGIN');
end;

procedure TmncSQLiteConnection.Execute(Command: string);
var
 lMsg  : PChar;
 s : Utf8String;
 r  : integer;
begin
  lMSg := nil;
  s := Command;
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

procedure TmncSQLiteConnection.DoInit;
begin
  if not IsInitializeSqlite then
    InitializeSQLite();
end;

procedure TmncSQLiteSession.SetConnection(const AValue: TmncSQLiteConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncSQLiteConnection.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set Exclusive when session active');
    FExclusive := AValue;
  end;
end;

procedure TmncSQLiteConnection.SetJournalMode(const AValue: TmncJournalMode);
begin
  if FJournalMode <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set JournalMode when session active');
    FJournalMode := AValue;
  end;
end;

procedure TmncSQLiteConnection.SetReadCommited(const AValue: Boolean);
begin
  if FReadCommited <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set ReadCommited when session active');
    FReadCommited := AValue;
  end;
end;

procedure TmncSQLiteConnection.SetTempStore(const AValue: TmncTempStore);
begin
  if FTempStore <> AValue then
  begin
    FTempStore := AValue;
    if Active then
      raise EmncException.Create('You can not set TempStore when session active');
  end;
end;

procedure TmncSQLiteConnection.InitPragma;
begin
  Execute('PRAGMA full_column_names=0');
  Execute('PRAGMA short_column_names=1');
  Execute('PRAGMA encoding="UTF-8"');
  Execute('PRAGMA foreign_keys=ON');

  if Exclusive then
    Execute('PRAGMA locking_mode=EXCLUSIVE')
  else
    Execute('PRAGMA locking_mode=NORMAL');
  if Exclusive then
  Execute('PRAGMA TEMP_STORE=' + SQLiteTempStoreToStr(FTempStore));
  Execute('PRAGMA journal_mode=' + SQLiteJournalModeToStr(FJournalMode));
  Execute('PRAGMA synchronous=' + SQLiteSynchronousToStr(Synchronous));
  {TODO
    secure_delete
    case_sensitive_like //to be compatiple with firebird
    temp_store_directory
    read_uncommitted nop
  }
end;

procedure TmncSQLiteSession.DoInit;
begin
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
  b: boolean;
  i: Integer;
  d: Double;
  c: Currency;
  t: Integer;
  t64: Int64;
  v: Variant;
begin
  for i := 0 to Binds.Count - 1 do
  begin
    Binds[i].FreeBuffer;
  end;

  for i := 0 to Binds.Count - 1 do
  begin
    if Binds[i].Param.IsEmpty then
      CheckError(sqlite3_bind_null(FStatment, i + 1))
    else
    begin
      case VarType(Binds[i].Param.Value) of
        varDate:
        begin
          d := Binds[i].Param.Value;// - UnixDateDelta; todo
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        varBoolean:
        begin
          b := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, ord(b)));
        end;
        varInteger:
        begin
          t := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, t));
        end;
        varint64:
        begin
          t64 := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int64(FStatment, i + 1, t64));
        end;
        varCurrency:
        begin
          c := Binds[i].Param.Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, c));
        end;
        varDouble:
        begin
          d := Binds[i].Param.Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        else //String type
        begin
          if not Binds[i].BufferAllocated then //TODO test after remove this line, i think it is not useful
          begin
            v := Binds[i].Param.Value;
            if TVarData(v).vType = varNull then
              s := ''
            else
              s := v + #0;
            Binds[i].AllocBuffer(PChar(s)^, Length(s));
          end;
          CheckError(sqlite3_bind_text(FStatment, i + 1, PChar(Binds[i].Buffer), -1, nil)); //up to #0
          //CheckError(sqlite3_bind_text(FStatment, i + 1, PChar(Binds[i].Buffer), Binds[i].BufferSize, nil)); not work with empty string not null
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
begin
  FBOF := True;
//  sqlite3_prepare_v2
//TODO: apply value of params if using injection mode
  CheckError(sqlite3_prepare(Connection.DBHandle, PChar(SQLProcessed.SQL), -1 , @FStatment, @FTail));
end;

procedure TmncSQLiteCommand.DoRollback;
begin
  Session.Rollback;
end;

function TmncSQLiteCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncSQLiteFields.Create(vColumns);
end;

function TmncSQLiteCommand.CreateParams: TmncParams;
begin
  Result := TmncSQLiteParams.Create;
end;

function TmncSQLiteCommand.CreateBinds: TmncBinds;
begin
  Result := TmncSQLiteBinds.Create;
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
  FieldType: Integer;
  SchemaType: PChar;
  aColumn: TmncColumn;
  //aSize: Integer;
begin
  Columns.Clear;
  c := sqlite3_column_count(FStatment);
  for i := 0 to c -1 do
  begin
    aName :=  DequoteStr(sqlite3_column_name(FStatment, i));
    FieldType := sqlite3_column_type(FStatment, i);
    SchemaType := sqlite3_column_decltype(FStatment, i);
    aColumn := Columns.Add(aName, SQLTypeToType(FieldType, SchemaType));
    aColumn.SchemaType := SchemaType;
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
  aColumn: TmncColumn;
  //aSize: Integer;
begin
  //belal why not use Columns ????
  //c := Columns.Count;
  c := sqlite3_column_count(FStatment);
  if c > 0 then
  begin
    aCurrent := CreateFields(Columns);
    for i := 0 to c - 1 do
    begin
//    TStorageType = (stNone, stInteger, stFloat, stText, stBlob, stNull);
      //aSize := sqlite3_column_bytes(FStatment, i);
      aColumn := Columns[i];
      aType := sqlite3_column_type(FStatment, i);
      //aType := Columns[i].DataType;
      case aType of
        SQLITE_NULL:
        begin
          aCurrent.Add(i, Null);
        end;
        SQLITE_INTEGER:
        begin
          int := sqlite3_column_int(FStatment, i);
{          if aColumn.DataType = ftDate then //todo
            int := int - 1;}
          aCurrent.Add(i, int);
        end;
        SQLITE_FLOAT:
        begin
          flt := sqlite3_column_double(FStatment, i);
          aCurrent.Add(i, flt);
        end;
        SQLITE_BLOB:
        begin
          int := sqlite3_column_bytes(FStatment, i);
          SetString(str, PChar(sqlite3_column_blob(FStatment, i)), int);
          aCurrent.Add(i, str);
        end;
        SQLITE_TEXT:
        begin
          if SameText(aColumn.SchemaType, 'Blob') then
          begin
            int := sqlite3_column_bytes(FStatment, i);
            SetString(str, PChar(sqlite3_column_blob(FStatment, i)), int);
          end
          else
            str := sqlite3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end
        else
        begin
          str := sqlite3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end;
      end;
    end;
    Fields := aCurrent;
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

function TmncSQLiteCommand.GetBinds: TmncSQLiteBinds;
begin
  Result := inherited Binds as TmncSQLiteBinds;
end;

initialization
  mncDB.Engines.Add(TmncSQLiteConnection);
end.
