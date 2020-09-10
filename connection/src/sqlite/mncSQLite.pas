unit mncSQLite;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DateUtils,
  mncSQLiteHeader, mncCommons,
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
    FAutoCreate: Boolean;
    FCorrectDateTime: Boolean;
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
    constructor Create; override;
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    function CreateSession: TmncSQLSession; overload; override; 
    procedure Interrupt;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean =False); override;
    function IsDatabaseExists(vName: string): Boolean; override;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); override;
    procedure Vacuum; override;
    function GetVersion: string;
    procedure Execute(Command: string); override;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property ReadCommited: Boolean read FReadCommited write SetReadCommited;
    property Synchronous: TmncSynchronous read FSynchronous write FSynchronous default syncDefault;
    property JournalMode: TmncJournalMode read FJournalMode write SetJournalMode default jrmDefault;
    property TempStore: TmncTempStore read FTempStore write SetTempStore default tmpDefault;
    property CorrectDateTime: Boolean read FCorrectDateTime write FCorrectDateTime default True;
    property AutoCreate: Boolean read FAutoCreate write FAutoCreate default True;
    {TODO
      ANALYZE
    }
    property DBHandle: PSqlite3 read FDBHandle;
    function GetExtension: string; override;
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
    function InternalCreateCommand: TmncSQLCommand; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(SQL: string);
    function GetLastRowID: Int64;
    function GetRowsChanged: Integer;
    property Connection: TmncSQLiteConnection read GetConnection write SetConnection;
  end;

  { TmncSQLiteField }

  TmncSQLiteField = class(TmncVariantField)
  end;

  { TmncSQLiteParam }

  TmncSQLiteParam = class(TmncVariantParam)
  end;

  { TmncSQLiteFields }

  TmncSQLiteFields = class(TmncFields)
  protected
    function DoCreateField(vColumn: TmncColumn): TmncField; override;
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
    FTail: PByte;
    FLastStepResult: longint;
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
    function GetDone: Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    function CreateBinds: TmncBinds; override;
    property Binds: TmncSQLiteBinds read GetBinds;
  public
    property Connection: TmncSQLiteConnection read GetConnection;
    property Session: TmncSQLiteSession read GetSession write SetSession;
    procedure Clear; override;
    function GetRowsChanged: Integer; override;
    function GetLastRowID: Int64; override;
    property Statment: Psqlite3_stmt read FStatment;
  end;

function SQLiteJournalModeToStr(JournalMode: TmncJournalMode): string;
function SQLiteTempStoreToStr(TempStore: TmncTempStore): string;
function SQLiteSynchronousToStr(Synchronous: TmncSynchronous): string;

implementation

uses
  mncDB, mncSQLiteORM, mncSQLiteMeta;

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
      Result := '0'; //Default
    {$endif}
    tmpFile: Result := '1'; //FILE
    tmpMemory: Result := '2'; //MEMORY
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

function SQLTypeToType(vType: Integer; const MetaType: string): TmncDataType;
  function isDate: Boolean;
  begin
    Result := SameText(MetaType, 'date') or SameText(MetaType, 'timestamp') or SameText(MetaType, 'datetime');
  end;

  function isCurrency: Boolean;
  var
    s: string;
  begin
    Result := false;
    s := SubStr(MetaType, '(');
    if SameText(s, 'decimal') then
    begin
      s := SubStr(MetaType, ',', 1);
      if s <> '' then
        Result := StrToIntDef(s, 0) < 5; //4 or less
    end;
  end;
begin
  case vType of
    SQLITE_INTEGER:
      if IsDate then
        Result := dtDate
      else//not yet
        Result := dtInteger;
    SQLITE_FLOAT:
    begin
      if isDate then
        Result := dtDateTime
      {else if isCurrency then
        Result := dtCurrency} //not yet
      else//not yet
        Result := dtFloat;
    end;
    SQLITE_BLOB: Result := dtBlob;
    SQLITE_NULL: Result := dtUnknown;
    SQLITE_TEXT:
    begin
      if SameText(MetaType, 'Blob') then
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

{ TmncSQLiteFields }

function TmncSQLiteFields.DoCreateField(vColumn: TmncColumn): TmncField;
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
  FCorrectDateTime := True;
  FAutoCreate := True;
end;

class function TmncSQLiteConnection.Capabilities: TmncCapabilities;
begin
  Result := [ccDB, ccPath, ccSQL, ccCreate, ccDrop, ccTransaction];
end;

class function TmncSQLiteConnection.EngineName: string;
begin
  Result := 'SQLite';
end;

function TmncSQLiteConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncSQLiteSession.Create(Self);
end;

procedure TmncSQLiteConnection.Interrupt;
begin
  sqlite3_interrupt(DBHandle);
end;

procedure TmncSQLiteConnection.CreateDatabase(const vName: string; CheckExists: Boolean);
begin
  //TODO
end;

function TmncSQLiteConnection.IsDatabaseExists(vName: string): Boolean;
begin
  Result := FileExists(vName);
end;

procedure TmncSQLiteConnection.Vacuum;
begin
  if Connected then
    Execute('vacuum');
end;

procedure TmncSQLiteConnection.DropDatabase(const vName: string; CheckExists: Boolean);
begin
  if not CheckExists or FileExists(vName) then
    DeleteFile(vName);
end;

function TmncSQLiteConnection.GetVersion: string;
var
  p: PUTF8Char;
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
  CheckError(sqlite3_open_v2(PUtf8Char(UTF8Encode(Resource)), @FDBHandle, f, nil), Resource);
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
  SQLiteLib.Release;
  {$endif}
end;

{ TmncSQLiteSession }

destructor TmncSQLiteSession.Destroy;
begin
  inherited;
end;

function TmncSQLiteSession.InternalCreateCommand: TmncSQLCommand;
begin
  Result := TmncSQLiteCommand.Create;
end;

procedure TmncSQLiteSession.Execute(SQL: string);
begin
  Connection.Execute(UTF8Encode(SQL));
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
 lMsg  : PUtf8Char;
 s : Utf8String;
 r  : integer;
begin
  lMSg := nil;
  s := Command;
  r := sqlite3_exec(FDBHandle, PUtf8Char(s), nil, nil, @lMsg);
  if lMSg <> nil then
  begin
    s := lMsg;
    sqlite3_free(lMSg);
  end;
  CheckError(r, s);
end;

function TmncSQLiteConnection.GetExtension: string;
begin
  Result := '.sqlite';
end;

function TmncSQLiteSession.GetLastRowID: Int64;
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
  inherited;
  SQLiteLib.Load;
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
  //Execute('PRAGMA full_column_names=0'); //deprecated
  //Execute('PRAGMA short_column_names=1'); //deprecated
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
end;

function TmncSQLiteCommand.GetDone: Boolean;
begin
  Result := (FStatment = nil) or inherited GetDone;
end;

function TmncSQLiteCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

function TmncSQLiteCommand.GetLastRowID: Int64;
begin
  Result := Session.GetLastRowID;
end;

procedure TmncSQLiteCommand.ApplyParams;
var
  buf: record case byte of
    0: (i: Integer);
    1: (f: Double);
    2: (b: boolean);
    4: (g: int64);
    5: (c: Currency);
  end;
  s: UTF8String;
  i: Integer;
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
          buf.f := Binds[i].Param.Value;
          if Session.Connection.CorrectDateTime then
            buf.f := buf.f - JulianEpoch;
          CheckError(sqlite3_bind_double(FStatment, i + 1, buf.f));
        end;
        varBoolean:
        begin
          buf.b := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, ord(buf.b)));
        end;
        varInteger:
        begin
          buf.i := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, buf.i));
        end;
        varint64:
        begin
          buf.g := Binds[i].Param.Value;
          CheckError(sqlite3_bind_int64(FStatment, i + 1, buf.g));
        end;
        varCurrency:
        begin
          buf.c := Binds[i].Param.Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, buf.c));
        end;
        varDouble:
        begin
          buf.f := Binds[i].Param.Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, buf.f));
        end;
        else //String type
        begin
          if not Binds[i].BufferAllocated then //TODO test after remove this line, i think it is not useful
          begin
            s := VarToStr(Binds[i].Param.Value) + #0;
            Binds[i].AllocBuffer(PUtf8Char(s)^, Length(s));
          end;
          CheckError(sqlite3_bind_text(FStatment, i + 1, PAnsiChar(Binds[i].Buffer), -1, nil)); //up to #0
          //CheckError(sqlite3_bind_text(FStatment, i + 1, PChar(Binds[i].Buffer), Binds[i].BufferSize, nil)); not work with empty string not null
        end;
      end;
    end;
  end;
end;

procedure TmncSQLiteCommand.DoExecute;
begin
  if FStatment <> nil then
    CheckError(sqlite3_reset(FStatment));
  ApplyParams;
  FLastStepResult := sqlite3_step(FStatment);
  if (FLastStepResult = SQLITE_DONE) then
  begin
    HitDone;
    CheckError(sqlite3_reset(FStatment));
  end;
end;

procedure TmncSQLiteCommand.DoNext;
begin
  if not Ready then
    FLastStepResult := sqlite3_step(FStatment); //already steped in DoExecute
  if (FLastStepResult = SQLITE_ROW) then
  begin
    if Ready then
      FetchColumns;
    FetchValues;
  end
  else if (FLastStepResult = SQLITE_DONE) then
  begin
    if not Done then
    begin
      HitDone;
      CheckError(sqlite3_reset(FStatment));
    end;
  end
  else
    CheckError(FLastStepResult);
  HitUnready;
end;

procedure TmncSQLiteCommand.DoPrepare;
begin
  FLastStepResult := 0;
//  sqlite3_prepare_v2
//TODO: apply value of params if using injection mode
  CheckError(sqlite3_prepare(Connection.DBHandle, PAnsiChar(ProcessedSQL.SQL), -1 , @FStatment, @FTail));
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
  MetaType: PAnsiChar;
  aColumn: TmncColumn;
begin
  Columns.Clear;
  c := sqlite3_column_count(FStatment);
  for i := 0 to c -1 do
  begin
    aName :=  DequoteStr(sqlite3_column_name(FStatment, i));
    FieldType := sqlite3_column_type(FStatment, i);
    MetaType := sqlite3_column_decltype(FStatment, i);
    aColumn := Columns.Add(aName, SQLTypeToType(FieldType, MetaType));
    aColumn.MetaType := MetaType;
  end;
end;

procedure TmncSQLiteCommand.FetchValues;
var
  v: record case byte of
    0: (i: Integer);
    1: (f: Double);
    2: (d: TDateTime);
  end;
  i: Integer;
  c: Integer;
{$ifdef fpc}
  str: utf8string;
{$else}
  str: utf8string;
{$endif}
  aCurrent: TmncFields;
  aType: Integer;
  aColumn: TmncColumn;
  //aSize: Integer;
begin
  //belal why not use Columns ????
  //c := Columns.Count;
  str := '';
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
          v.i := sqlite3_column_int(FStatment, i);
          if aColumn.DataType in [dtDate, dtTime, dtDateTime] then
          begin
            if Session.Connection.CorrectDateTime then
              v.i := trunc(v.i + JulianEpoch);
            v.d := v.i;
            aCurrent.Add(i, v.d);
          end
          else
            aCurrent.Add(i, v.i);
        end;
        SQLITE_FLOAT:
        begin
          v.f := sqlite3_column_double(FStatment, i);
          if aColumn.DataType in [dtDate, dtTime, dtDateTime] then
          begin
            if Session.Connection.CorrectDateTime then
              v.f := v.f + JulianEpoch;
            v.d := v.f;
            aCurrent.Add(i, v.d);
          end
          else
            aCurrent.Add(i, v.f);
        end;
        SQLITE_BLOB:
        begin
          v.i := sqlite3_column_bytes(FStatment, i);
          SetLength(str, v.i);
          Move(PByte(sqlite3_column_blob(FStatment, i))^, PByte(str)^, v.i);
          aCurrent.Add(i, str);
        end;
        SQLITE_TEXT:
        begin
          if SameText(aColumn.MetaType, 'Blob') then
          begin
            v.i := sqlite3_column_bytes(FStatment, i);
            SetLength(str, v.i);
            Move(PByte(sqlite3_column_blob(FStatment, i))^, PByte(str)^, v.i);
          end
          else if aColumn.DataType in [dtDate, dtTime, dtDateTime] then
          begin
            str := sqlite3_column_text(FStatment, i);
            v.d := ISOStrToDate(str);
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
  Engines.RegisterConnection(TmncSQLiteConnection.EngineName, 'SQLite Database', TmncSQLiteConnection);
end.
