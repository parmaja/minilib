unit mncMySQL;
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
  mncCommons, mncSchemas, mncMySql56dyn,
  mnUtils, mncConnections, mncSQL;

type
  { TmncMySQLConnection }

  TmncMySQLConnection = class(TmncSQLConnection)
  private
    FDBHandle: PMYSQL;
    FExclusive: Boolean;
    FReadCommited: Boolean;
    procedure SetExclusive(const AValue: Boolean);
    procedure SetReadCommited(const AValue: Boolean);
  protected
    procedure InitPragma; virtual;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    procedure RaiseError(Error: Integer; const Msg: string; const ExtraMsg: string = '');
    procedure CheckError(Error: Integer; const ExtraMsg: string = ''); overload;
    procedure CheckError(vMySQL: PMYSQL); overload;
    procedure DoInit; override;
  public
    constructor Create;
    class function Model: TmncConnectionModel; override;
    function CreateSession: TmncSQLSession; overload; override; 
    procedure Interrupt;
    function GetVersion: string;
    procedure Execute(Command: string); override;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property ReadCommited: Boolean read FReadCommited write SetReadCommited;
    property DBHandle: PMYSQL read FDBHandle;
  end;

  { TmncMySQLSession }

  TmncMySQLSession = class(TmncSQLSession)
  private
    function GetConnection: TmncMySQLConnection;
    procedure SetConnection(const AValue: TmncMySQLConnection);
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
    property Connection: TmncMySQLConnection read GetConnection write SetConnection;
  end;

  { TmncMySQLField }

  TmncMySQLField = class(TmncField)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  { TmncMySQLParam }

  TmncMySQLParam = class(TmncParam)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TmncMySQLFields }

  TmncMySQLFields = class(TmncFields)
  protected
    function CreateField(vColumn: TmncColumn): TmncField; override;
  end;

  { TmncMySQLParams }

  TmncMySQLParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  end;

  { TmncMySQLBind }

  TmncMySQLBind = class(TmncBind)
  private
    FBuffer: Pointer; //MYSQL_BIND
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

  { TmncMySQLBinds }

  TmncMySQLBinds = class(TmncBinds)
  private
    function GetItem(Index: Integer): TmncMySQLBind;
  protected
    function CreateBind: TmncBind; override;
  public
    property Items[Index: Integer]: TmncMySQLBind read GetItem; default;
  end;

  { TmncMySQLCommand }

  TmncMySQLCommand = class(TmncSQLCommand)
  private
    FStatment: PMYSQL_STMT;
    FTail: pchar;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetBinds: TmncMySQLBinds;
    function GetConnection: TmncMySQLConnection;
    procedure FetchColumns;
    procedure FetchValues;
    procedure ApplyParams;
    function GetSession: TmncMySQLSession;
    procedure SetSession(const AValue: TmncMySQLSession);
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
    property Binds: TmncMySQLBinds read GetBinds;
  public
    property Connection:TmncMySQLConnection read GetConnection;
    property Session: TmncMySQLSession read GetSession write SetSession;
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    function GetLastInsertID: Int64;
    property Statment: PMYSQL_STMT read FStatment;
  end;

implementation

uses
  mncDB, mncMySQLSchemas;

const
  MySQL_OK = 0;

var
  IsInitializeMySQL: Boolean = False;
{
function SQLTypeToType(vType: Integer; const SchemaType: string): TmncDataType;
begin
  case vType of
    MySQL_INTEGER:
      if SameText(SchemaType, 'date') then
        Result := dtDate
      else//not yet
        Result := dtInteger;
    MySQL_FLOAT:
    begin
      if SameText(SchemaType, 'date') then
        Result := dtDateTime
      else//not yet
        Result := dtFloat;
    end;
    MySQL_BLOB: Result := dtBlob;
    MySQL_NULL: Result := dtUnknown;
    MySQL_TEXT:
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
}
{ TmncMySQLBinds }

function TmncMySQLBinds.GetItem(Index: Integer): TmncMySQLBind;
begin
  Result := inherited Items[Index] as TmncMySQLBind;
end;

function TmncMySQLBinds.CreateBind: TmncBind;
begin
  Result := TmncMySQLBind.Create;
end;

function TmncMySQLParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncMySQLParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

function TmncMySQLBind.GetBufferAllocated: Boolean;
begin
  Result := Buffer <> nil;
end;

procedure TmncMySQLBind.AllocBuffer(var P; Size: Integer);
begin
  FreeBuffer;
  FBufferSize := Size;
  if Size > 0 then
  begin
    FBuffer := AllocMem(FBufferSize);
    Move(P, FBuffer^, Size);
  end;
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

constructor TmncMySQLParam.Create;
begin
  inherited;
end;

destructor TmncMySQLParam.Destroy;
begin
  inherited;
end;

function TmncMySQLField.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncMySQLField.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncMySQLFields }

function TmncMySQLFields.CreateField(vColumn: TmncColumn): TmncField;
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
end;

class function TmncMySQLConnection.Model: TmncConnectionModel;
begin
  Result.Name := 'MySQL';
  Result.Title := 'MySQL Database';
  Result.Capabilities := [ccDB, ccSQL, ccTransaction];
  //Result.SchemaClass := TmncMySQLSchema;//TOdo
end;

function TmncMySQLConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncMySQLSession.Create(Self);
end;

procedure TmncMySQLConnection.Interrupt;
begin
  mysql_kill(DBHandle, 0); //TODO
  //https://dev.mysql.com/doc/refman/5.0/en/mysql-kill.html
end;

function TmncMySQLConnection.GetVersion: string;
var
  p: integer;
begin
  p := mysql_get_server_version(FDBHandle);
  Result := IntToStr(p);
end;

procedure TmncMySQLConnection.DoConnect;
var
  f: Integer;
  r: PMYSQL;
  aHost: string;
begin
  //TODO AutoCreate
  //* ref: https://dev.mysql.com/doc/refman/5.0/en/mysql-real-connect.html
  FDBHandle := mysql_init(FDBHandle);
  try
    //mysql_options(&mysql,MYSQL_READ_DEFAULT_GROUP,"your_prog_name");
    CheckError(mysql_real_connect(FDBHandle, PAnsiChar(Host), PChar(UserName), PChar(Password), nil, 0, nil, CLIENT_MULTI_RESULTS));
    CheckError(mysql_select_db(FDBHandle, PAnsiChar(Resource)));
  except
    on E:Exception do
    begin
      if FDBHandle <> nil then
        mysql_close(FDBHandle);
      FDBHandle := nil;
      raise;
    end;
  end;
  InitPragma;
end;

function TmncMySQLConnection.GetConnected: Boolean;
begin
  Result := FDBHandle <> nil;
end;

procedure TmncMySQLConnection.RaiseError(Error: Integer; const Msg: string; const ExtraMsg: string = '');
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
  {$ifdef FPC}
  ReleaseMySQL;
  {$endif}
end;

{ TmncMySQLSession }

destructor TmncMySQLSession.Destroy;
begin
  inherited;
end;

function TmncMySQLSession.CreateCommand: TmncSQLCommand;
begin
  Result := TmncMySQLCommand.Create;
  Result.Session := Self;
end;

function TmncMySQLSession.CreateSchema: TmncSchema;
begin
  //Result := TmncMySQLSchema.CreateBy(Self);
  Result := nil;
end;

procedure TmncMySQLSession.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncMySQLSession.DoStart;
begin
  Execute('BEGIN');
end;

procedure TmncMySQLSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
begin
  case How of
    sdaCommit: Execute('COMMIT');
    sdaRollback: Execute('ROLLBACK');
  end;
  if Retaining then
    Execute('BEGIN');
end;

procedure TmncMySQLConnection.Execute(Command: string);
var
 lMsg  : PChar;
 s : Utf8String;
 r  : integer;
begin
  if mysql_query(FDBHandle, PAnsiChar(Command)) <> 0 then
    RaiseError(-1, 'Query failed');
{  lMSg := nil;
  s := Command;
  r := MySQL3_exec(FDBHandle, PChar(s), nil, nil, @lMsg);
  if lMSg <> nil then
  begin
    s := lMsg;
    MySQL3_free(lMSg);
  end;
  CheckError(r, s);}
end;

function TmncMySQLSession.GetLastInsertID: Int64;
begin
  CheckActive;
  //Result := MySQL3_last_insert_rowid(Connection.DBHandle);
end;

function TmncMySQLSession.GetRowsChanged: Integer;
begin
  CheckActive;
  //Result := MySQL3_changes(Connection.DBHandle);
end;

function TmncMySQLSession.GetActive: Boolean;
begin
  Result:= inherited GetActive;
end;

constructor TmncMySQLSession.Create(vConnection: TmncConnection);
begin
  inherited;
end;

function TmncMySQLSession.GetConnection: TmncMySQLConnection;
begin
  Result := inherited Connection as TmncMySQLConnection;
end;

procedure TmncMySQLConnection.DoInit;
begin
  if not IsInitializeMySQL then
  begin
    InitialiseMysql(mysqllib);
    IsInitializeMySQL := True;
  end;
end;

procedure TmncMySQLSession.SetConnection(const AValue: TmncMySQLConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncMySQLConnection.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set Exclusive when session active');
    FExclusive := AValue;
  end;
end;

procedure TmncMySQLConnection.SetReadCommited(const AValue: Boolean);
begin
  if FReadCommited <> AValue then
  begin
    if Active then
      raise EmncException.Create('You can not set ReadCommited when session active');
    FReadCommited := AValue;
  end;
end;

procedure TmncMySQLConnection.InitPragma;
begin
end;

procedure TmncMySQLSession.DoInit;
begin
end;

{ TmncMySQLCommand }

procedure TmncMySQLCommand.CheckError(Error: longint);
var
  s : Utf8String;
  ExtraMsg: string;
  r: Integer;
begin
(*  if (Error <> MySQL_OK) then
  begin
    s := 'MySQL: ' + MySQL3_errmsg(Connection.DBHandle);
    if Active then
    begin
      r := MySQL3_finalize(FStatment);//without check error prevent the loop
      if (r <> MySQL_OK) then
        ExtraMsg := MySQL3_errmsg(Connection.DBHandle)
      else
        ExtraMsg := '';
      if ExtraMsg <> '' then
        s := s + ' - ' + ExtraMsg;
      FStatment := nil;
    end;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;*)
end;

function TmncMySQLCommand.GetSession: TmncMySQLSession;
begin
  Result := inherited Session as TmncMySQLSession;
end;

procedure TmncMySQLCommand.SetSession(const AValue: TmncMySQLSession);
begin
  inherited Session := AValue;
end;

procedure TmncMySQLCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

function TmncMySQLCommand.GetEOF: Boolean;
begin
  Result := (FStatment = nil) or FEOF; 
end;

function TmncMySQLCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

function TmncMySQLCommand.GetLastInsertID: Int64;
begin
  Result := Session.GetLastInsertID;
end;

procedure TmncMySQLCommand.ApplyParams;
var
  s: UTF8String;
  b: boolean;
  i: Integer;
  d: Double;
  c: Currency;
  t: Integer;
  t64: Int64;
begin
{  for i := 0 to Binds.Count - 1 do
  begin
    Binds[i].FreeBuffer;
  end;

  for i := 0 to Binds.Count - 1 do
  begin
    if Binds[i].Param.IsEmpty then
      CheckError(MySQL3_bind_null(FStatment, i + 1))
    else
    begin
      case VarType(Binds[i].Param.Value) of
        varDate:
        begin
          d := Binds[i].Param.Value;// - UnixDateDelta; todo
          CheckError(MySQL3_bind_double(FStatment, i + 1, d));
        end;
        varBoolean:
        begin
          b := Binds[i].Param.Value;
          CheckError(MySQL3_bind_int(FStatment, i + 1, ord(b)));
        end;
        varInteger:
        begin
          t := Binds[i].Param.Value;
          CheckError(MySQL3_bind_int(FStatment, i + 1, t));
        end;
        varint64:
        begin
          t64 := Binds[i].Param.Value;
          CheckError(MySQL3_bind_int64(FStatment, i + 1, t64));
        end;
        varCurrency:
        begin
          c := Binds[i].Param.Value;
          CheckError(MySQL3_bind_double(FStatment, i + 1, c));
        end;
        varDouble:
        begin
          d := Binds[i].Param.Value;
          CheckError(MySQL3_bind_double(FStatment, i + 1, d));
        end;
        else //String type
        begin
          if not Binds[i].BufferAllocated then //TODO test after  remove this line, i think it is not useful
          begin
            s := VarToStrDef(Binds[i].Param.Value, '');
            Binds[i].AllocBuffer(PChar(s)^, Length(s));
          end;
          CheckError(MySQL3_bind_text(FStatment, i + 1, PChar(Binds[i].Buffer), Binds[i].BufferSize, nil));
        end;
      end;
    end;
  end;}
end;

procedure TmncMySQLCommand.DoExecute;
begin
  FBOF := True;
  if FStatment <> nil then
    CheckError(mysql_stmt_execute(FStatment));
  ApplyParams;
end;

procedure TmncMySQLCommand.DoNext;
var
  r: Integer;
  b: Boolean;
begin
//  CheckError(MySQL3_step(@FStatment));
  b := mysql_stmt_fetch(FStatment) in [0, MYSQL_DATA_TRUNCATED];
  //r := mysql_fetch_row(FStatment);
  if (b) then
  begin
    if FBOF then
      FetchColumns;
    FetchValues;
    FEOF := False;
  end
  else //if (r = MySQL_DONE) then
  begin
    FEOF := True;
    //CheckError(MySQL3_reset(FStatment));
  end;
//  else if error
//    CheckError(r);
  FBOF := False;
end;

procedure TmncMySQLCommand.DoPrepare;
var
  r: Integer;
begin
  FBOF := True;
//  MySQL3_prepare_v2
//TODO: apply value of params if using injection mode
  FStatment := mysql_stmt_init(Connection.DBHandle);
  mysql_stmt_prepare(FStatment, PChar(SQLProcessed.SQL), Length(SQLProcessed.SQL));
  //CheckError(FStatment);
end;

procedure TmncMySQLCommand.DoRollback;
begin
  Session.Rollback;
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

procedure TmncMySQLCommand.DoClose;
begin
  mysql_stmt_free_result(FStatment);
  mysql_stmt_close(FStatment);
//  CheckError(MySQL3_clear_bindings(FStatment));//not found in MySQL3 for WinCE
  FStatment := nil;  
end;

procedure TmncMySQLCommand.DoCommit;
begin
  Session.Commit;
end;

procedure TmncMySQLCommand.FetchColumns;
var
  i: Integer;
  c: Integer;
  aName: string;
  aType: Integer;
  pType: PChar;
  aColumn: TmncColumn;
  //aSize: Integer;
begin
  Columns.Clear;
  c := mysql_stmt_field_count(FStatment);
  {for i := 0 to c -1 do
  begin
    aName :=  DequoteStr(MySQL3_column_name(FStatment, i));
    aType := MySQL3_column_type(FStatment, i);
    pType := MySQL3_column_decltype(FStatment, i);
    aColumn := Columns.Add(aName, SQLTypeToType(aType, pType));
    aColumn.SchemaType := pType;
  end;}
end;

procedure TmncMySQLCommand.FetchValues;
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
(*  c := MySQL3_column_count(FStatment);
  if c > 0 then
  begin
    aCurrent := CreateFields(Columns);
    for i := 0 to c - 1 do
    begin
//    TStorageType = (stNone, stInteger, stFloat, stText, stBlob, stNull);
      //aSize := MySQL3_column_bytes(FStatment, i);
      aColumn := Columns[i];
      aType := MySQL3_column_type(FStatment, i);
      //aType := Columns[i].DataType;
      case aType of
        MySQL_NULL:
        begin
          aCurrent.Add(i, Null);
        end;
        MySQL_INTEGER:
        begin
          int := MySQL3_column_int(FStatment, i);
{          if aColumn.DataType = ftDate then //todo
            int := int - 1;}
          aCurrent.Add(i, int);
        end;
        MySQL_FLOAT:
        begin
          flt := MySQL3_column_double(FStatment, i);
          aCurrent.Add(i, flt);
        end;
        MySQL_BLOB:
        begin
          int := MySQL3_column_bytes(FStatment, i);
          SetString(str, PChar(MySQL3_column_blob(FStatment, i)), int);
          aCurrent.Add(i, str);
        end;
        MySQL_TEXT:
        begin
          if SameText(aColumn.SchemaType, 'Blob') then
          begin
            int := MySQL3_column_bytes(FStatment, i);
            SetString(str, PChar(MySQL3_column_blob(FStatment, i)), int);
          end
          else
            str := MySQL3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end
        else
        begin
          str := MySQL3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end;
      end;
    end;
    Fields := aCurrent;
  end;*)
end;

function TmncMySQLCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil; 
end;

function TmncMySQLCommand.GetConnection: TmncMySQLConnection;
begin
  Result := Session.Connection as TmncMySQLConnection;
end;

function TmncMySQLCommand.GetBinds: TmncMySQLBinds;
begin
  Result := inherited Binds as TmncMySQLBinds;
end;

initialization
  mncDB.Engines.Add(TmncMySQLConnection);
end.
