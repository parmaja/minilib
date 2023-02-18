unit mncMySQL;
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
  {$ifndef FPC} Windows, {$endif}
  mnLibraries,
  mncCommons, mncMySQLHeader,
  mncConnections, mncSQL;

type
  { TmncMySQLConnection }

  TmncMySQLConnection = class(TmncSQLConnection)
  private
    FDBHandle: PMYSQL;
    FExclusive: Boolean;
    FMultiCursors: Boolean;
    FReadCommited: Boolean;
    procedure SetExclusive(const AValue: Boolean);
    procedure SetMultiCursors(AValue: Boolean);
    procedure SetReadCommited(const AValue: Boolean);
  protected
    FDatabase: string;
    procedure InitPragma; virtual;
    procedure InternalConnect(out vHandle: PMYSQL; vResource: string);
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    procedure RaiseError(Error: Integer; const ExtraMsg: string = '');
    procedure CheckError(Error: Integer; const ExtraMsg: string = ''); overload;
    procedure CheckError(vMySQL: PMYSQL); overload;
    procedure DoInit; override;
  public
    constructor Create; override;
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    function CreateTransaction: TmncSQLTransaction; overload; override; 
    procedure Interrupt;
    procedure SetCharsetName(Charset: string);
    procedure SetStorageEngine(vName: string);
    procedure SetAutoCommit(AMode: Boolean);
    function SelectDatabase(vName: string; RaiseException: Boolean = true): Boolean;
    function IsDatabaseExists(const vName: string): Boolean; override;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); override;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); override;
    procedure Vacuum; override;

    function GetVersion: string;
    procedure Execute(const vSQL: string); override;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property ReadCommited: Boolean read FReadCommited write SetReadCommited;
    property DBHandle: PMYSQL read FDBHandle;
    property MultiCursors: Boolean read FMultiCursors write SetMultiCursors;
  end;

  { TmncMySQLTransaction }

  TmncMySQLTransaction = class(TmncSQLTransaction)
  private
    function GetConnection: TmncMySQLConnection;
    procedure SetConnection(const AValue: TmncMySQLConnection);
  protected
    //TODO Check Setting Connection when no database selected
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoStop(How: TmncTransactionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
    function DoCreateCommand: TmncSQLCommand; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(SQL: string);
    property Connection: TmncMySQLConnection read GetConnection write SetConnection;
  end;

  { TmncMySQLField }

  TmncMySQLField = class(TmncVariantField)
  end;

  { TmncMySQLFields }

  TmncMySQLFields = class(TmncFields)
  protected
    function DoCreateField(vColumn: TmncColumn): TmncField; override;
  end;

  { TmncMySQLParam }

  TmncMySQLParam = class(TmncVariantParam)
  end;

  { TmncMySQLParams }

  TmncMySQLParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  end;

  TMySQLBinds = array of MYSQL_BIND;

  { TmncMySQLBind }

  TmncMySQLBind = class(TmncBind)
  private
    FBuffer: Pointer;
    FBufferSize: cardinal;
    function GetBufferAllocated: Boolean;
  protected
    len: culong;
    is_null: my_bool;
    function AllocBuffer(Size: cardinal; Realloc: Boolean = False): Pointer; overload; virtual;
    procedure CopyBuffer(var P; Size: cardinal);
    function AllocBuffer(var P; Size: cardinal; Realloc: Boolean = False): Pointer; overload; virtual;
    procedure FreeBuffer;
    property Buffer: Pointer read FBuffer;
    property BufferSize: cardinal read FBufferSize;
    property BufferAllocated: Boolean read GetBufferAllocated;
  public
    destructor Destroy; override;
  end;

  { TmncMySQLBinds }

  TmncMySQLBinds = class(TmncBinds)
  private
    function GetItem(Index: Integer): TmncMySQLBind;
  protected
    FValues: TMySQLBinds;
    function CreateBind: TmncBind; override;
  public
    procedure Clear; override;
    property Items[Index: Integer]: TmncMySQLBind read GetItem; default;
  end;

  { TmncMySQLColumn }

  TmncMySQLColumn = class(TmncColumn)
  public
    FieldType: enum_field_types;
    constructor Create(vName: string; vType: TmncDataType);
  end;

  { TmncMySQLColumns }

  TmncMySQLColumns = class(TmncColumns)
  private
    function GetItem(Index: Integer): TmncMySQLColumn;
  protected
  public
    property Items[Index: Integer]: TmncMySQLColumn read GetItem; default;
  end;

  { TmncMySQLResults }

  TmncMySQLResults = class(TObject)
  public
    Binds: array of MYSQL_BIND;

    Buffers : array of record
      buf: record case byte of
        0: (AsRaw: array[0..15] of byte);
        1: (AsInteger: Integer);
        2: (AsBig: int64);
        3: (AsFloat: single);
        4: (AsDouble: double);
        5: (AsDateTime: MYSQL_TIME);
        6: (AsString: array[0..15] of AnsiChar);
      end;
      length: culong;
      is_null: my_bool;
      error: my_bool;
    end;
    constructor Create(vLength: Integer);
    destructor Destroy; override;
  end;

  { TmncMySQLCommand }

  TmncMySQLCommand = class(TmncSQLCommand)
  private
    FReadOnly: Boolean;
    FStatment: PMYSQL_STMT;
    FResults: TmncMySQLResults;
    function GetBinds: TmncMySQLBinds;
    function GetColumns: TmncMySQLColumns;
    function GetConnection: TmncMySQLConnection;
    function FetchColumns: Boolean; //return false if no data
    procedure FetchValues;
    procedure ApplyParams;
    function GetTransaction: TmncMySQLTransaction;
    procedure SetReadOnly(AValue: Boolean);
    procedure SetTransaction(const AValue: TmncMySQLTransaction);
  protected
    procedure CheckError(Error:longint);
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    procedure DoUnPrepare; override;
    function GetDone: Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    function CreateBinds: TmncBinds; override;
    function CreateColumns: TmncColumns; override;
    property Binds: TmncMySQLBinds read GetBinds;

  public
    property Connection: TmncMySQLConnection read GetConnection;
    property Transaction: TmncMySQLTransaction read GetTransaction write SetTransaction;
    procedure Clear; override;
    property Statment: PMYSQL_STMT read FStatment;
    property Columns: TmncMySQLColumns read GetColumns;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    function GetLastRowID: Int64; override;
    function GetRowsChanged: Integer; override;
  end;

function MySQLTypeToType(vType: enum_field_types): TmncDataType;
function MySQLTypeToString(vType: enum_field_types): String;

implementation

uses
  mncDB, mncMySQLORM;

const
  cMaxString = 255;

const
  MySQL_OK = 0;

{ TmncMySQLResults }

constructor TmncMySQLResults.Create(vLength: Integer);
begin
  inherited Create;
  SetLength(Binds, vLength);
  SetLength(Buffers, vLength);
end;

destructor TmncMySQLResults.Destroy;
begin
  Binds := nil;
  Buffers := nil;
  inherited;
end;

{ TmncMySQLColumn }

constructor TmncMySQLColumn.Create(vName: string; vType: TmncDataType);
begin
  inherited Create;
  Name := vName;
  SetType(vType);
end;

{ TmncMySQLColumns }

function TmncMySQLColumns.GetItem(Index: Integer): TmncMySQLColumn;
begin
  Result := inherited Items[Index] as TmncMySQLColumn;
end;

{ TmncMySQLBind }

function TmncMySQLBind.GetBufferAllocated: Boolean;
begin
  Result := Buffer <> nil;
end;

function TmncMySQLBind.AllocBuffer(Size: cardinal; Realloc: Boolean): Pointer;
begin
  if not Realloc then
    FreeBuffer;
  FBufferSize := Size;
  if FBufferSize > 0 then
  begin
    if Realloc then
      ReallocMem(FBuffer, FBufferSize)
    else
      FBuffer := AllocMem(FBufferSize);
  end;
  Result := FBuffer;
end;

function TmncMySQLBind.AllocBuffer(var P; Size: cardinal; Realloc: Boolean): Pointer;
begin
  Result := AllocBuffer(Size, Realloc);
  CopyBuffer(P, Size);
end;

procedure TmncMySQLBind.CopyBuffer(var P; Size: cardinal);
begin
  if Size > FBufferSize then
    Size := FBufferSize;
  if Size > 0 then
    Move(P, FBuffer^, Size);
end;

procedure TmncMySQLBind.FreeBuffer;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
end;

destructor TmncMySQLBind.Destroy;
begin
  FreeBuffer;
  inherited;
end;

function MySQLTypeToType(vType: enum_field_types): TmncDataType;
begin
  case vType of
    MYSQL_TYPE_DECIMAL: Result := dtCurrency;
    MYSQL_TYPE_TINY: Result := dtInteger;
    MYSQL_TYPE_SHORT: Result := dtInteger;
    MYSQL_TYPE_LONG: Result := dtInteger;
    MYSQL_TYPE_FLOAT: Result := dtFloat;
    MYSQL_TYPE_DOUBLE: Result := dtFloat;
    MYSQL_TYPE_NULL: Result := dtUnknown;
    MYSQL_TYPE_TIMESTAMP: Result := dtDateTime;
    MYSQL_TYPE_LONGLONG: Result := dtBig;
    MYSQL_TYPE_INT24: Result := dtBig;
    MYSQL_TYPE_DATE: Result := dtDate;
    MYSQL_TYPE_TIME: Result := dtTime;
    MYSQL_TYPE_DATETIME: Result := dtDateTime;
    MYSQL_TYPE_YEAR: Result := dtDate;
    MYSQL_TYPE_NEWDATE: Result := dtDate;
    MYSQL_TYPE_VARCHAR: Result := dtString;
    MYSQL_TYPE_BIT: Result := dtBoolean;
    MYSQL_TYPE_TIMESTAMP2: Result := dtDateTime;
    MYSQL_TYPE_DATETIME2: Result := dtDateTime;
    MYSQL_TYPE_TIME2: Result := dtTime;
    MYSQL_TYPE_NEWDECIMAL: Result := dtCurrency;
    MYSQL_TYPE_ENUM: Result := dtInteger; //TODO dtEnum
    MYSQL_TYPE_SET: Result := dtBig;
    MYSQL_TYPE_TINY_BLOB: Result := dtBlob;
    MYSQL_TYPE_MEDIUM_BLOB: Result := dtBlob;
    MYSQL_TYPE_LONG_BLOB: Result := dtBlob;
    MYSQL_TYPE_BLOB: Result := dtBlob;
    MYSQL_TYPE_VAR_STRING: Result := dtString;
    MYSQL_TYPE_STRING: Result := dtString;
    MYSQL_TYPE_GEOMETRY: Result := dtBig; //TODO what is that!!!
  end;
end;

function MySQLTypeToString(vType: enum_field_types): String;
begin
  case vType of
    MYSQL_TYPE_DECIMAL: Result := 'DECIMAL';
    MYSQL_TYPE_TINY: Result := 'TINY';
    MYSQL_TYPE_SHORT: Result := 'SHORT';
    MYSQL_TYPE_LONG: Result := 'LONG';
    MYSQL_TYPE_FLOAT: Result := 'FLOAT';
    MYSQL_TYPE_DOUBLE: Result := 'DOUBLE';
    MYSQL_TYPE_NULL: Result := 'NULL';
    MYSQL_TYPE_TIMESTAMP: Result := 'TIMESTAMP';
    MYSQL_TYPE_LONGLONG: Result := 'LONGLONG';
    MYSQL_TYPE_INT24: Result := 'INT24';
    MYSQL_TYPE_DATE: Result := 'DATE';
    MYSQL_TYPE_TIME: Result := 'TIME';
    MYSQL_TYPE_DATETIME: Result := 'DATETIME';
    MYSQL_TYPE_YEAR: Result := 'YEAR';
    MYSQL_TYPE_NEWDATE: Result := 'NEWDATE';
    MYSQL_TYPE_VARCHAR: Result := 'VARCHAR';
    MYSQL_TYPE_BIT: Result := 'BIT';
    MYSQL_TYPE_TIMESTAMP2: Result := 'TIMESTAMP2';
    MYSQL_TYPE_DATETIME2: Result := 'DATETIME2';
    MYSQL_TYPE_TIME2: Result := 'TIME2';
    MYSQL_TYPE_NEWDECIMAL: Result := 'NEWDECIMAL';
    MYSQL_TYPE_ENUM: Result := 'ENUM';
    MYSQL_TYPE_SET: Result := 'SET';
    MYSQL_TYPE_TINY_BLOB: Result := 'TINY_BLOB';
    MYSQL_TYPE_MEDIUM_BLOB: Result := 'MEDIUM_BLOB';
    MYSQL_TYPE_LONG_BLOB: Result := 'LONG_BLOB';
    MYSQL_TYPE_BLOB: Result := 'BLOB';
    MYSQL_TYPE_VAR_STRING: Result := 'VAR_STRING';
    MYSQL_TYPE_STRING: Result := 'STRING';
    MYSQL_TYPE_GEOMETRY: Result := 'GEOMETRY';
  end;
end;

{ TmncMySQLBinds }

function TmncMySQLBinds.GetItem(Index: Integer): TmncMySQLBind;
begin
  Result := inherited Items[Index] as TmncMySQLBind;
end;

function TmncMySQLBinds.CreateBind: TmncBind;
begin
  Result := TmncMySQLBind.Create;
end;

procedure TmncMySQLBinds.Clear;
begin
  inherited Clear;
  FValues := nil;
end;

{ TmncMySQLFields }

function TmncMySQLFields.DoCreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncMySQLField.Create(vColumn);
end;

{ TmncMySQLParams }

function TmncMySQLParams.CreateParam: TmncParam;
begin
  Result := TmncMySQLParam.Create;
end;

procedure TmncMySQLConnection.CheckError(Error: Integer; const ExtraMsg: string);
var
  s : Utf8String;
begin
  if (Error <> MySQL_OK) then
  begin
    s := 'MySQL: ' + IntToStr(Error) + ', ' + mysql_error(FDBHandle);
    if ExtraMsg <> '' then
      s := s + ' - ' + ExtraMsg;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

procedure TmncMySQLConnection.CheckError(vMySQL: PMYSQL);
begin
  if vMySQL = nil then
    RaiseError(mysql_errno(FDBHandle), mysql_error(FDBHandle));
end;

{ TmncMySQLConnection }

constructor TmncMySQLConnection.Create;
begin
  inherited Create;
  FMultiCursors := True;
end;

class function TmncMySQLConnection.Capabilities: TmncCapabilities;
begin
  Result := [ccDB, ccSQL, ccCreate, ccDrop, ccTransaction];
end;

class function TmncMySQLConnection.EngineName: string;
begin
  Result := 'MySQL';
end;

function TmncMySQLConnection.CreateTransaction: TmncSQLTransaction;
begin
  Result := TmncMySQLTransaction.Create(Self);
end;

procedure TmncMySQLConnection.Interrupt;
begin
  CheckError(mysql_kill(DBHandle, 0)); //TODO
  //https://dev.mysql.com/doc/refman/5.0/en/mysql-kill.html
end;

procedure TmncMySQLConnection.SetCharsetName(Charset: string);
begin
  CheckError(mysql_options(FDBHandle, MYSQL_SET_CHARSET_NAME, PAnsiChar(Charset)));
end;

procedure TmncMySQLConnection.SetStorageEngine(vName: string);
begin
  Execute('SET default_storage_engine =' + vName);
end;

procedure TmncMySQLConnection.SetAutoCommit(AMode: Boolean);
begin
  mysql_autocommit(FDBHandle, ord(AMode));
end;

function TmncMySQLConnection.SelectDatabase(vName: string; RaiseException: Boolean): Boolean;
var
  r: Integer;
begin
  CheckActive;
  if Transactions.IsAnyActive then
    RaiseError(-1, 'You cant select database if you have opened Transactions');
  r :=  mysql_select_db(FDBHandle, PAnsiChar(vName));
  Result := r = 0;
  if not Result then
  begin
    if RaiseException then
      CheckError(r);
  end
  else
    FDatabase := vName;
end;

function TmncMySQLConnection.IsDatabaseExists(const vName: string): Boolean;
var
  aConn: TmncSQLConnection;
  aTransaction: TmncSQLTransaction;
  aCMD: TmncSQLCommand;
begin
  aConn := Clone('mysql', False);
  try
    aConn.Connect;
    aTransaction := aConn.CreateTransaction;
    aTransaction.Start;
    aCMD := aTransaction.CreateCommand;
    try
      aCMD.SQL.Text := 'select Count(*) as Result from information_schema.schemata where schema_name = '''+ vName + '''';
      if aCMD.Execute then
      begin
        Result := aCMD.Field['Result'].AsInteger > 0;
      end;
    finally
      aCMD.Free;
      aTransaction.Free;
    end;
  finally
    aConn.Free;
  end;
end;

procedure TmncMySQLConnection.CreateDatabase(const vName: string; CheckExists: Boolean);
var
  s: string;
begin
  //CheckActive;
  s := 'Create Database ';
  if CheckExists then
    s := s + 'if not exists ';
  s := s + vName + ';';
  CloneExecute('mysql', s);
end;

procedure TmncMySQLConnection.DropDatabase(const vName: string; CheckExists: Boolean);
var
  s: string;
begin
  //CheckActive;
  s := 'drop database ';
  if CheckExists then
    s := s + 'if exists ';
  s := s + vName + ';';
  CloneExecute('mysql', s);
end;

procedure TmncMySQLConnection.Vacuum;
begin
  //TODO
end;

function TmncMySQLConnection.GetVersion: string;
var
  p: integer;
begin
  p := mysql_get_server_version(FDBHandle);
  Result := IntToStr(p);
end;

procedure TmncMySQLConnection.DoConnect;
begin
  InternalConnect(FDBHandle, Resource);
  if MultiCursors then
    CheckError(mysql_set_server_option(FDBHandle, MYSQL_OPTION_MULTI_STATEMENTS_ON))
  else
    CheckError(mysql_set_server_option(FDBHandle, MYSQL_OPTION_MULTI_STATEMENTS_OFF));
  CheckError(mysql_options(FDBHandle, MYSQL_SET_CHARSET_NAME, PAnsiChar('utf8')));
  //CheckError(mysql_options(vHandle, MYSQL_REPORT_DATA_TRUNCATION, @b));
  if Resource <> '' then
    SelectDatabase(Resource);
  Execute('SET sql_mode=NO_AUTO_VALUE_ON_ZERO'); //eh idk if it wrong
  SetAutoCommit(false);
  InitPragma;
end;

function TmncMySQLConnection.GetConnected: Boolean;
begin
  Result := FDBHandle <> nil;
end;

procedure TmncMySQLConnection.RaiseError(Error: Integer; const ExtraMsg: string = '');
var
  s : string;
begin
  if (Error <> MySQL_OK) then
  begin
    s := 'MySQL: ' + IntToStr(Error) + ', ' + mysql_error(FDBHandle);
    if ExtraMsg <> '' then
      s := s + ' - ' + ExtraMsg;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

procedure TmncMySQLConnection.DoDisconnect;
begin
  mysql_close(FDBHandle); //TODO check error
  FDBHandle := nil;
end;

{ TmncMySQLTransaction }

destructor TmncMySQLTransaction.Destroy;
begin
  inherited;
end;

function TmncMySQLTransaction.DoCreateCommand: TmncSQLCommand;
begin
  Result := TmncMySQLCommand.Create;
end;

procedure TmncMySQLTransaction.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncMySQLTransaction.DoStart;
begin
  //Execute('SET TRANSACTION ISOLATION LEVEL READ COMMITTED');
  Execute('BEGIN');
end;

procedure TmncMySQLTransaction.DoStop(How: TmncTransactionAction; Retaining: Boolean);
begin
  case How of
    sdaCommit: Execute('COMMIT');
    sdaRollback: Execute('ROLLBACK');
  end;
  if Retaining then
    Execute('BEGIN');
end;

procedure TmncMySQLConnection.Execute(const vSQL: string);
begin
  CheckError(mysql_query(FDBHandle, PAnsiChar(vSQL)));
end;

function TmncMySQLTransaction.GetActive: Boolean;
begin
  Result:= inherited GetActive;
end;

constructor TmncMySQLTransaction.Create(vConnection: TmncConnection);
begin
  inherited;
end;

function TmncMySQLTransaction.GetConnection: TmncMySQLConnection;
begin
  Result := inherited Connection as TmncMySQLConnection;
end;

procedure TmncMySQLConnection.DoInit;
begin
  inherited;
  MySQLLib.Load;
end;

procedure TmncMySQLTransaction.SetConnection(const AValue: TmncMySQLConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncMySQLConnection.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set Exclusive when Transaction active');
    FExclusive := AValue;
  end;
end;

procedure TmncMySQLConnection.SetMultiCursors(AValue: Boolean);
begin
  if FMultiCursors <> AValue then
  begin
    FMultiCursors :=AValue;
    if Active then
    begin
      if MultiCursors then
        CheckError(mysql_set_server_option(FDBHandle, MYSQL_OPTION_MULTI_STATEMENTS_ON))
      else
        CheckError(mysql_set_server_option(FDBHandle, MYSQL_OPTION_MULTI_STATEMENTS_OFF))
    end;
  end;
end;

procedure TmncMySQLConnection.SetReadCommited(const AValue: Boolean);
begin
  if FReadCommited <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set ReadCommited when Transaction active');
    FReadCommited := AValue;
  end;
end;

procedure TmncMySQLConnection.InitPragma;
begin
end;

procedure TmncMySQLConnection.InternalConnect(out vHandle: PMYSQL; vResource: string);
var
  aHost: AnsiString;
  aPort: Integer;
  aResource, aUser, aPassword: AnsiString;
  b: my_bool;
  timout: cuint;
  protocol: mysql_protocol_type;
begin
  b := 0;
  //Initialize(vHandle);
  //* ref: https://dev.mysql.com/doc/refman/5.0/en/mysql-real-connect.html
  vHandle := mysql_init(nil);
  try
    if Host = '' then
      aHost := '127.0.0.1'
    else
      aHost := Host;
    if Port = '' then
      aPort := 3306
    else
      aPort := StrToInt(Port);

    if vResource <> '' then
      aResource := vResource
    else
      aResource := Resource;
    aUser := UserName;
    aPassword := Password;

    //mysql_options(&mysql,MYSQL_READ_DEFAULT_GROUP,"your_prog_name");
{   Shared memory:
    you need to setup server to use it
    [mysqld]
    shared_memory = ON
    shared-memory-base-name=MYSQL
}
{
    protocol := MYSQL_PROTOCOL_MEMORY;
    CheckError(mysql_options(vHandle, MYSQL_OPT_PROTOCOL, @protocol));
    CheckError(mysql_options(vHandle, MYSQL_SHARED_MEMORY_BASE_NAME, PAnsiChar('MYSQL')));
}
    {timout := 1;
    CheckError(mysql_options(vHandle, MYSQL_OPT_CONNECT_TIMEOUT, @timout));}

    CheckError(mysql_real_connect(vHandle, PAnsiChar(aHost), PAnsiChar(aUser), PAnsiChar(aPassword), nil, aPort, nil, CLIENT_MULTI_RESULTS)); //CLIENT_MULTI_STATEMENTS CLIENT_INTERACTIVE
  except
    on E:Exception do
    begin
      if FDBHandle <> nil then
        mysql_close(vHandle);
      vHandle := nil;
      raise;
    end;
  end;
end;

procedure TmncMySQLTransaction.DoInit;
begin
end;

{ TmncMySQLCommand }

procedure TmncMySQLCommand.CheckError(Error: longint);
var
  s : Utf8String;
begin
  if (Error <> MySQL_OK) then
  begin
    s := 'MySQL: ' + IntToStr(Error) + ', ' + mysql_stmt_error(FStatment) ;
    if Active then
    begin
      DoClose;
    end;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

function TmncMySQLCommand.GetTransaction: TmncMySQLTransaction;
begin
  Result := inherited Transaction as TmncMySQLTransaction;
end;

procedure TmncMySQLCommand.SetReadOnly(AValue: Boolean);
begin
  CheckInactive;
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
  end;
end;

procedure TmncMySQLCommand.SetTransaction(const AValue: TmncMySQLTransaction);
begin
  inherited Transaction := AValue;
end;

procedure TmncMySQLCommand.Clear;
begin
  inherited;
end;

function TmncMySQLCommand.GetDone: Boolean;
begin
  Result := (FStatment = nil) or inherited GetDone;
end;

function TmncMySQLCommand.GetRowsChanged: Integer;
begin
  CheckActive;
  Result := mysql_stmt_affected_rows(FStatment);
end;

function TmncMySQLCommand.GetLastRowID: Int64;
begin
  CheckActive;
  Result := mysql_stmt_insert_id(FStatment);
end;

//Ported from FPC ty FPC team
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;
begin
  if Date < 0 then
    Result := trunc(Date) - Abs(frac(Time))
  else
    Result := trunc(Date) + Abs(frac(Time));
end;

function MySQLDateTimeToDateTime(ATime: MYSQL_TIME): TDateTime;
var
  t, d: TDateTime;
begin
  if not TryEncodeDate(ATime.year, ATime.month, ATime.day, d) then
    d := 0;
  if not TryEncodeTime(ATime.hour, ATime.minute, ATime.second, ATime.second_part, t) then
    t := 0;
  Result := ComposeDateTime(d, t);
end;

procedure DateTimeToMySQLDateTime(DateTime: TDateTime; out ATime: MYSQL_TIME);
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(DateTime, st);
  {$ifdef FPC}
  ATime.Year := st.Year;
  ATime.Month := st.Month;
  ATime.Day := st.Day;
  ATime.Hour := st.Hour;
  ATime.Minute := st.Minute;
  ATime.Second := st.Second;
  ATime.second_part := st.MilliSecond;
  {$else}
  ATime.Year := st.wYear;
  ATime.Month := st.wMonth;
  ATime.Day := st.wDay;
  ATime.Hour := st.wHour;
  ATime.Minute := st.wMinute;
  ATime.Second := st.wSecond;
  ATime.second_part := st.wMilliseconds;
  {$endif}
  ATime.neg := 0;
  ATime.time_type := MYSQL_TIMESTAMP_DATETIME;
end;

procedure DateTimeToMySQLTime(DateTime: TDateTime; out ATime: MYSQL_TIME);
begin
  DateTimeToMySQLDateTime(DateTime, ATime);
  ATime.time_type := MYSQL_TIMESTAMP_TIME;
end;

procedure DateTimeToMySQLDate(DateTime: TDateTime; out ATime: MYSQL_TIME);
begin
  DateTimeToMySQLDateTime(DateTime, ATime);
  ATime.time_type := MYSQL_TIMESTAMP_DATE;
end;

procedure TmncMySQLCommand.ApplyParams;
var
  s: UTF8String;
  dt: MYSQL_TIME;
  tiny: smallint;

  i: Integer;
  d: Double;
  n: Integer;
  t64: Int64;
begin
  //* ref: https://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-bind-param.html
  //* ref: https://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-execute.html
  //* https://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statement-type-codes.html
  //* http://www.2uzhan.com/calling-mysql_stmt_bind_param-and-setting-mysql_bind-members/

  if (Binds.Count > 0) then
  begin
    if (Binds.FValues = nil) then //not binded yet
    begin
      SetLength(Binds.FValues, Binds.Count);
      for i := 0 to Binds.Count - 1 do
      begin
        Binds.FValues[i].is_null := @Binds[i].is_null;
        case VarType(Binds[i].Param.Value) of
          varDate:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(dt));
            Binds.FValues[i].buffer_length := SizeOf(dt);
            Binds.FValues[i].buffer_type := MYSQL_TYPE_DATETIME;
          end;
          varBoolean:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(tiny));
            Binds.FValues[i].buffer_length := SizeOf(tiny);
            Binds.FValues[i].buffer_type := MYSQL_TYPE_TINY;
          end;
          varInteger:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(n));
            Binds.FValues[i].buffer_length := 0;
            Binds.FValues[i].buffer_type := MYSQL_TYPE_LONG;
          end;
          varint64:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(t64));
            Binds.FValues[i].buffer_length := 0;
            Binds.FValues[i].buffer_type := MYSQL_TYPE_LONGLONG;
          end;
          varCurrency:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(d)); //TODO MYSQL_TYPE_NEWDECIMAL
            Binds.FValues[i].buffer_length := SizeOf(d);
            Binds.FValues[i].buffer_type := MYSQL_TYPE_DOUBLE;
          end;
          varDouble:
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(SizeOf(d));
            Binds.FValues[i].buffer_length := 0;
            Binds.FValues[i].buffer_type := MYSQL_TYPE_DOUBLE;
          end;
          else //String type
          begin
            Binds.FValues[i].buffer := Binds[i].AllocBuffer(cMaxString); //Will set in setting values, if i set it to 0 it will crash :(
            Binds.FValues[i].length := @Binds[i].len;
            Binds.FValues[i].buffer_length := 0;
            Binds.FValues[i].buffer_type := MYSQL_TYPE_VAR_STRING;
          end;
        end;
      end;
      CheckError(mysql_stmt_bind_param(FStatment, @Binds.FValues[0]));
    end;

    for i := 0 to Binds.Count - 1 do
    begin
      if Binds[i].Param.IsEmpty then
      begin
        Binds[i].is_null := 1;
      end
      else
      begin
        case VarType(Binds[i].Param.Value) of
          varDate:
          begin
            DateTimeToMySQLDateTime(Binds[i].Param.Value, dt);
            Binds[i].CopyBuffer(dt, SizeOf(dt));
          end;
          varBoolean:
          begin
            tiny := Ord(Boolean(Binds[i].Param.Value));
            Binds[i].CopyBuffer(tiny, SizeOf(tiny));
          end;
          varInteger:
          begin
            n := Integer(Binds[i].Param.Value);
            Binds[i].CopyBuffer(n, SizeOf(n));
          end;
          varint64:
          begin
            t64 := Binds[i].Param.Value;
            Binds[i].CopyBuffer(t64, SizeOf(t64));
          end;
          varCurrency:
          begin
            d := Binds[i].Param.Value;
            Binds[i].CopyBuffer(d, SizeOf(d));
          end;
          varDouble:
          begin
            d := Binds[i].Param.Value;
            Binds[i].CopyBuffer(d, SizeOf(d));
          end;
          else //String type
          begin
            s := VarToStrDef(Binds[i].Param.Value, '');
            Binds[i].CopyBuffer(PAnsiChar(s)^, Length(s));
            Binds[i].len := Length(s);
          end;
        end;
      end;
    end;
  end;
end;

procedure TmncMySQLCommand.DoExecute;
begin
  ApplyParams;
  CheckError(mysql_stmt_execute(FStatment));
end;

procedure TmncMySQLCommand.DoNext;
var
  b: Boolean;
  state: integer;
begin
  if Ready then
    if not FetchColumns then
      HitDone;

  if not Done then
  begin
    state := mysql_stmt_fetch(FStatment);
    b := state in [0, MYSQL_DATA_TRUNCATED];
    if (b) then
    begin
      FetchValues;
    end
    else
      HitDone;
  end;
  HitUnready;
end;

procedure TmncMySQLCommand.DoPrepare;
var
  aType: enum_cursor_type;
  aSQL: string;
begin
  //* ref: https://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-prepare.html
  if FStatment <> nil then
    CheckError(mysql_stmt_reset(FStatment))
  else
    FStatment := mysql_stmt_init(Connection.DBHandle);
  try
    if ReadOnly then
    begin
      aType := CURSOR_TYPE_READ_ONLY;
      CheckError(mysql_stmt_attr_set(FStatment, STMT_ATTR_CURSOR_TYPE, @aType));
    end;
    aSQL := GetProcessedSQL;
    CheckError(mysql_stmt_prepare(FStatment, PAnsiChar(aSQL), Length(aSQL)));
  except
    on E: Exception do
    begin
      //FStatment := nil;
      raise;
    end;
  end;
end;

procedure TmncMySQLCommand.DoUnPrepare;
begin
  inherited;
end;

procedure TmncMySQLCommand.DoRollback;
begin
  Transaction.Rollback;
end;

function TmncMySQLCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncMySQLFields.Create(vColumns);
end;

function TmncMySQLCommand.CreateParams: TmncParams;
begin
  Result := TmncMySQLParams.Create;
end;

function TmncMySQLCommand.CreateBinds: TmncBinds;
begin
  Result := TmncMySQLBinds.Create;
end;

function TmncMySQLCommand.CreateColumns: TmncColumns;
begin
  Result := TmncMySQLColumns.Create();
end;

procedure TmncMySQLCommand.DoClose;
begin
  FreeAndNil(FResults);
  CheckError(mysql_stmt_free_result(FStatment));
  CheckError(mysql_stmt_close(FStatment));
  FStatment := nil;
end;

procedure TmncMySQLCommand.DoCommit;
begin
  Transaction.Commit;
end;

function TmncMySQLCommand.FetchColumns: Boolean;
var
  i: Integer;
  c: Integer;
  aName: string;
  FieldType: enum_field_types;
  aColumn: TmncMySQLColumn;
  Res : PMYSQL_RES;
  Field: PMYSQL_FIELD;
  function IsBlob(n : longint) : boolean;
  begin
    Result :=(n and BLOB_FLAG)<>0;
  end;

  function IsBinary(n : longint) : boolean;
  begin
    Result :=(n and BINARY_FLAG)<>0;
  end;

begin
  Columns.Clear;

  Res := mysql_stmt_result_metadata(FStatment); // Fetch result set meta information
  Result := Res <> nil;
  if Result then
  try
    c := mysql_stmt_field_count(FStatment);
    if c > 0 then
    begin
      Field := mysql_fetch_fields(Res);

      FResults := TmncMySQLResults.Create(c);

      for i := 0 to c -1 do
      begin
        aName :=  Field.name;
        FieldType := Field.ftype;

        aColumn := TmncMySQLColumn.Create(aName, MySQLTypeToType(FieldType));

        Columns.Add(aColumn);

        aColumn.MetaType := MySQLTypeToString(FieldType);
        aColumn.Size := Field.length;
        aColumn.Decimals := Field.decimals;
        //aColumn.IsBlob := IsBlob(Field.flags);

        if (FieldType in [MYSQL_TYPE_DECIMAL, MYSQL_TYPE_NEWDECIMAL]) then
        begin
          if (Field.decimals > 4) then
            FieldType := MYSQL_TYPE_DOUBLE
          else
            FieldType := MYSQL_TYPE_DOUBLE; //we should take it as integer/bigint then divide it but no way
        end;

        aColumn.FieldType := FieldType;

        FillChar(FResults.Binds[i], SizeOf(FResults.Binds[i]), #0);

        FResults.Binds[i].buffer_type := FieldType;


        FResults.Binds[i].buffer := @FResults.Buffers[i].buf;
        FResults.Binds[i].buffer_length := SizeOf(FResults.Buffers[i].buf);

        FResults.Binds[i].length := @FResults.Buffers[i].length;
        FResults.Binds[i].is_null := @FResults.Buffers[i].is_null;
        FResults.Binds[i].error := @FResults.Buffers[i].error;

        Inc(Field);
      end;
      CheckError(mysql_stmt_bind_result(FStatment, @FResults.Binds[0]));
    end;
  finally
    mysql_free_result(Res);
  end;
end;

procedure TmncMySQLCommand.FetchValues;
var
  i, c: Integer;
{$ifdef fpc}
  s: string;
{$else}
  s: utf8string;
{$endif}
  aCurrent: TmncFields;
  aType: enum_field_types;
  aColumn: TmncMySQLColumn;
  real_length: culong;
  bind: MYSQL_BIND;
  cr: Currency;
  procedure FetchString;
  begin
    real_length := FResults.Buffers[i].length;

    if real_length <= SizeOf(FResults.Buffers[i].buf.AsRaw) then
      s := FResults.Buffers[i].buf.AsString
    else
    begin
      SetLength(s, real_length);
      FillChar(bind, sizeof(bind), #0);

      bind.buffer := @s[1];
      bind.buffer_length := real_length;
      CheckError(mysql_stmt_fetch_column(FStatment, @bind, i, 0));
    end;
  end;
begin
  if Columns.Count > 0 then
  begin
    aCurrent := CreateFields(Columns);
    c := Columns.Count;
    for i := 0 to c - 1 do
    begin
      aColumn := Columns[i];
      aType := aColumn.FieldType;

      case aType of
        MYSQL_TYPE_NULL: aCurrent.Add(i, NULL);
        MYSQL_TYPE_BIT: aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger <> 0);
        MYSQL_TYPE_TINY: aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);
        MYSQL_TYPE_SHORT: aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);
        MYSQL_TYPE_LONG:
            aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);
        MYSQL_TYPE_INT24:
          aCurrent.Add(i, FResults.Buffers[i].buf.AsBig);
        MYSQL_TYPE_LONGLONG:
          aCurrent.Add(i, FResults.Buffers[i].buf.AsBig);
        MYSQL_TYPE_FLOAT:
          aCurrent.Add(i, FResults.Buffers[i].buf.AsFloat); //float is bad //TODO need cfloat to single
        MYSQL_TYPE_DOUBLE:
          aCurrent.Add(i, FResults.Buffers[i].buf.AsDouble);
        {MYSQL_TYPE_DECIMAL, MYSQL_TYPE_NEWDECIMAL:
        //* ref: https://dev.mysql.com/doc/refman/5.0/en/fixed-point-types.html
        //* ref: http://stackoverflow.com/questions/6831217/double-vs-decimal-in-mysql
          aCurrent.Add(i, FResults.Buffers[i].buf.AsDouble);}
        MYSQL_TYPE_YEAR : aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);
        MYSQL_TYPE_TIMESTAMP, MYSQL_TYPE_DATETIME, MYSQL_TYPE_NEWDATE,  MYSQL_TYPE_DATE,
        MYSQL_TYPE_TIME, MYSQL_TYPE_TIMESTAMP2, MYSQL_TYPE_DATETIME2, MYSQL_TYPE_TIME2:
          aCurrent.Add(i, MySQLDateTimeToDateTime(FResults.Buffers[i].buf.AsDateTime));
        MYSQL_TYPE_DECIMAL, MYSQL_TYPE_NEWDECIMAL, //yes it is a string
        MYSQL_TYPE_VARCHAR,MYSQL_TYPE_VAR_STRING, MYSQL_TYPE_STRING:
        begin
          FetchString;

          if aType in [MYSQL_TYPE_DECIMAL, MYSQL_TYPE_NEWDECIMAL] then
          begin
            cr := StrToCurrDef(s, 0);
            aCurrent.Add(i, cr);
          end
          else
            aCurrent.Add(i, s);
        end;

        MYSQL_TYPE_ENUM: aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);
        MYSQL_TYPE_SET: aCurrent.Add(i, FResults.Buffers[i].buf.AsInteger);

        MYSQL_TYPE_TINY_BLOB, MYSQL_TYPE_MEDIUM_BLOB, MYSQL_TYPE_LONG_BLOB, MYSQL_TYPE_BLOB:
        begin
          if FetchBlobs then
          begin
            FetchString;
            aCurrent.Add(i, s);
          end
          else
            aCurrent.Add(i, Unassigned);
        end;

        MYSQL_TYPE_GEOMETRY:
        begin
          aCurrent.Add(i, Unassigned);
        end;
      end;
    end;
    Fields := aCurrent;
  end;
end;

function TmncMySQLCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil; 
end;

function TmncMySQLCommand.GetConnection: TmncMySQLConnection;
begin
  Result := Transaction.Connection as TmncMySQLConnection;
end;

function TmncMySQLCommand.GetBinds: TmncMySQLBinds;
begin
  Result := inherited Binds as TmncMySQLBinds;
end;

function TmncMySQLCommand.GetColumns: TmncMySQLColumns;
begin
  Result := inherited Columns as TmncMySQLColumns;
end;

initialization
  Engines.RegisterConnection('MySQL', 'MySQL Database', TmncMySQLConnection);
end.
