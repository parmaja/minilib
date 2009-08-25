unit mncPostgre;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @comment   Only for postgre 8.x or later
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, StrUtils, syncobjs,
  {$ifdef FPC}
  //postgres3,
  postgres3dyn,
  {$else}
  mncPGHeader,
  {$endif}
  mnUtils, mnStreams, mncConnections, mncSQL;

type

  { TmncPGConnection }

  TmncPGConnection = class(TmncConnection)
  private
    FHandle: PPGconn;
  protected
    procedure InternalConnect(var vHandle: PPGconn);
    procedure InternalDisconnect(var vHandle: PPGconn);
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    class function GetMode:TmncTransactionMode; override;
  protected
    procedure RaiseError(Error: Boolean; const ExtraMsg: string = '');
  public
    constructor Create;
    procedure Interrupt;
    //TODO: Reconnect  use PQReset
    property Handle: PPGconn read FHandle;
  end;

  { TmncPGSession }

  TmncPGSession = class(TmncSession)
  private
    FTokenID: Cardinal;
    FDBHandle: PPGconn;
    FExclusive: Boolean;
    function GetConnection: TmncPGConnection;
    procedure SetConnection(const AValue: TmncPGConnection);
    procedure SetExclusive(const AValue: Boolean);
  protected
    function NewToken: string;//used for new command name
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
    property Connection: TmncPGConnection read GetConnection write SetConnection;
    property DBHandle: PPGconn read FDBHandle;
  end;

  TArrayOfPChar = array of PChar;

  { TmncPGCommand }

  TmncPGCommand = class(TmncSQLCommand)
  private
    FHandle: string;
    FStatment: PPGresult;
    FTuple: Integer;
    FBOF: Boolean;
    FEOF: Boolean;
    FStatus: TExecStatusType;
    function GetConnection: TmncPGConnection;
    procedure FetchFields;
    procedure FetchValues;
    function GetSession: TmncPGSession;
    procedure SetSession(const AValue: TmncPGSession);
  protected
    procedure InternalClose;
    procedure RaiseError(PGResult: PPGresult);
    procedure CreateParamValues(var Result: TArrayOfPChar);
    procedure FreeParamValues(var Result: TArrayOfPChar);
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    property Connection: TmncPGConnection read GetConnection;
    property Session: TmncPGSession read GetSession write SetSession;
  public
    constructor Create(vSession:TmncSession);
    destructor Destroy; override;
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    function GetLastInsertID: Int64;
    property Statment: PPGresult read FStatment;//opened by PQexecPrepared
    property Status: TExecStatusType read FStatus;
    property Handle: string read FHandle;//used for name in PQprepare
  end;

implementation

var
  IsPostgresInitialised: Boolean = False;

procedure TmncPGConnection.RaiseError(Error: Boolean; const ExtraMsg: string);
var
  s : Utf8String;
begin
  if (Error) then
  begin
    s := 'Postgre connection failed' + #13 + PQerrorMessage(FHandle);
    if ExtraMsg <> '' then
      s := s + ' - ' + ExtraMsg;
    raise EmncException.Create(s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
  end;
end;

{ TmncPGConnection }

constructor TmncPGConnection.Create;
begin
  inherited Create;
end;

procedure TmncPGConnection.Interrupt;
begin
  //PG3_interrupt(DBHandle);
end;

procedure TmncPGConnection.InternalConnect(var vHandle: PPGconn);
var
  PGOptions, PGtty: Pchar;
  aPort: string;
begin
  if not IsPostgresInitialised then
  begin
    InitialisePostgres3;
    IsPostgresInitialised := True;
    if not Assigned(PQsetdbLogin) then
      raise EmncException.Create('PQsetdbLogin not found in libpq');
    if not Assigned(PQprepare) then
      raise EmncException.Create('PQprepare not found in libpq');
  end;
  PGOptions := nil;
  PGtty := nil;
  if Port <> '' then
    aPort := Port
  else
    aPort := '5432';
  vHandle := PQsetdbLogin(PChar(Host), PChar(aPort), PChar(PGOptions), PChar(PGtty), PChar(Resource), PChar(UserName), PChar(Password));
  try
    RaiseError(PQstatus(vHandle) = CONNECTION_BAD);
  except
    PQfinish(vHandle);
    vHandle := nil;
    raise;
  end;
end;

procedure TmncPGConnection.InternalDisconnect(var vHandle: PPGconn);
begin
  PQfinish(vHandle);
  FHandle := nil;
end;

procedure TmncPGConnection.DoConnect;
begin
  InternalConnect(FHandle);
end;

function TmncPGConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TmncPGConnection.DoDisconnect;
begin
  InternalDisconnect(FHandle);
end;

{ TmncPGSession }

destructor TmncPGSession.Destroy;
begin
  inherited;
end;

procedure TmncPGSession.DoStart;
begin
  if FDBHandle = nil then
    Connection.InternalConnect(FDBHandle);
  Execute('BEGIN');
end;

procedure TmncPGSession.DoCommit;
begin
  Execute('COMMIT');
end;

procedure TmncPGSession.DoRollback;
begin
  Execute('ROLLBACK');
end;

procedure TmncPGSession.DoStop;
begin
  if FDBHandle <> nil then
    Connection.InternalDisconnect(FDBHandle);
end;

function TmncPGSession.NewToken: string;
begin
  ConnectionLock.Enter;
  try
    Inc(FTokenID);
    Result := 'minilib_' + IntToStr(FTokenID);
  finally
    ConnectionLock.Leave;
  end;
end;

procedure TmncPGSession.Execute(SQL: string);
var
 s : Utf8String;
 r  : PPGresult;
begin
  s := SQL;
  r := PQexec(FDBHandle, PChar(s));
//  Connection.RaisePQError(r, s);
  PQclear(r);
end;

function TmncPGSession.GetLastInsertID: Int64;
begin
  CheckActive;
  //Result := PG3_last_insert_rowid(Connection.DBHandle);
end;

function TmncPGSession.GetRowsChanged: Integer;
begin
  CheckActive;
  //Result := PG3_changes(Connection.DBHandle);
end;

function TmncPGSession.GetActive: Boolean;
begin
  Result:= inherited GetActive;
end;

constructor TmncPGSession.Create(vConnection: TmncConnection);
begin
  inherited;
end;

function TmncPGSession.GetConnection: TmncPGConnection;
begin
  Result := inherited Connection as TmncPGConnection;
end;

class function TmncPGConnection.GetMode: TmncTransactionMode;
begin
  Result := smMultiTransactions;
end;

procedure TmncPGSession.SetConnection(const AValue: TmncPGConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncPGSession.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    FExclusive := AValue;
    if Active then
      raise EmncException.Create('You can not set Exclusive when session active');
  end;
end;

{ TmncPGCommand }

procedure TmncPGCommand.RaiseError(PGResult: PPGresult);
var
  s : Utf8String;
  ExtraMsg: string;
  r: Integer;
begin
  case PQresultStatus(PGResult) of
    PGRES_BAD_RESPONSE,PGRES_NONFATAL_ERROR, PGRES_FATAL_ERROR:
    begin
      s := PQresultErrorMessage(PGResult);
      raise EmncException.Create('Postgre command: ' + s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
    end;
  end;
end;

function TmncPGCommand.GetSession: TmncPGSession;
begin
  Result := inherited Session as TmncPGSession;
end;

procedure TmncPGCommand.SetSession(const AValue: TmncPGSession);
begin
  inherited Session := AValue;
end;

procedure TmncPGCommand.InternalClose;
begin
  PQclear(FStatment);
  FStatment := nil;
  FStatus := PGRES_EMPTY_QUERY;
  FTuple := 0;
end;

procedure TmncPGCommand.CreateParamValues(var Result: TArrayOfPChar);
var
  i: Integer;
  s: string;
begin
  FreeParamValues(Result);
  SetLength(Result, Params.Count);
  for i := 0 to Params.Count -1 do
  begin
    if Params.Items[i].IsNull then
      FreeMem(Result[i])
    else
    begin
      case VarType(Params.Items[i].Value) of
        VarDate:
          s := FormatDateTime('yyyy-mm-dd hh:nn:ss', Params.Items[i].Value);
        else
          s := Params.Items[i].Value;
      end;
      GetMem(Result[i], Length(s) + 1);
      StrMove(PChar(Result[i]), Pchar(s), Length(s) + 1);
    end;
  end;
end;

procedure TmncPGCommand.FreeParamValues(var Result: TArrayOfPChar);
var
  i: Integer;
  s: string;
begin
  for i := 0 to Length(Result) - 1 do
    FreeMem(Result[i]);
  SetLength(Result, 0);
end;

procedure TmncPGCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

constructor TmncPGCommand.Create(vSession:TmncSession);
begin
  inherited Create(vSession);
  FHandle := Session.NewToken;
end;

destructor TmncPGCommand.Destroy;
begin
  inherited;
end;

function TmncPGCommand.GetEOF: Boolean;
begin
  Result := (FStatment = nil) or FEOF; 
end;

function TmncPGCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

function TmncPGCommand.GetLastInsertID: Int64;
begin
  Result := Session.GetLastInsertID;
end;

procedure TmncPGCommand.DoExecute;
var
  Values: TArrayOfPChar;
  p: pointer;
begin
  if FStatment <> nil then
    PQclear(FStatment);
  try
    if Params.Count > 0 then
    begin
      CreateParamValues(Values);
      p := @Values[0];
    end
    else
      p := nil;
    FStatment := PQexecPrepared(Session.DBHandle, PChar(FHandle), Params.Count, p, nil, nil, 1);
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(FStatment);
  FBOF := True;
  FEOF := Status <> PGRES_TUPLES_OK;
  try
    RaiseError(FStatment);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGCommand.DoNext;
var
  r: Integer;
begin
  if (Status = PGRES_TUPLES_OK) then
  begin
    if FBOF then
      FetchFields
    else
      inc(FTuple);
    FEOF := FTuple >= PQntuples(FStatment);
    if not FEOF then
      FetchValues;
  end
  else
    FEOF := True;
  FBOF := False;
end;

procedure TmncPGCommand.DoPrepare;
var
  s:string;
  r: PPGresult;
  c: PPGconn;
begin
  FBOF := True;
  s := ParseSQL([psoAddParamsID], '$');
  c := Session.DBHandle;
  r := PQprepare(c, PChar(FHandle), PChar(s), 0 , nil);
  try
    RaiseError(r);
  finally
    PQclear(r);
  end;
end;

procedure TmncPGCommand.DoRollback;
begin
  Session.Rollback;
end;

procedure TmncPGCommand.DoClose;
begin
  InternalClose;
end;

procedure TmncPGCommand.DoCommit;
begin
  Session.Commit;
end;

procedure TmncPGCommand.FetchFields;
var
  i: Integer;
  c: Integer;
  aName: string;
  r: PPGresult;
begin
  Fields.Clear;
  c := PQnfields(FStatment);
  for i := 0 to c - 1 do
  begin
    aName :=  DequoteStr(PQfname(FStatment, i));
    Fields.Add(aName);
  end;
end;

procedure TmncPGCommand.FetchValues;
var
  i: Integer;
  c: Integer;
  int:Int64;
  str: Utf8String;
  flt: Double;
  aCurrent: TmncRecord;
  aType: Integer;
  v: Variant;
  //aSize: Integer;
begin
  c := PQnfields(FStatment);
  if c > 0 then
  begin
    aCurrent := TmncRecord.Create(Fields);
    for i := 0 to c - 1 do
    begin
      if PQgetisnull(Statment, FTuple, i) <> 0 then
        aCurrent.Add(i, NULL)
      else
      begin
        str := PQgetvalue(FStatment, FTuple, i);
        aCurrent.Add(i, str);
      end;
    end;
{
      aType := PG3_column_type(FStatment, i);
      case aType of
        PG_NULL:
        begin
          aCurrent.Add(i, Null);
        end;
        PG_INTEGER:
        begin
          int := PG3_column_int(FStatment, i);
          aCurrent.Add(i, int);
        end;
        PG_FLOAT:
        begin
          flt := PG3_column_double(FStatment, i);
          aCurrent.Add(i, flt);
        end;
        else
        begin
          str := PG3_column_text(FStatment, i);
          aCurrent.Add(i, str);
        end;
      end;}
    Current := aCurrent;
  end;
end;

function TmncPGCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil; 
end;

function TmncPGCommand.GetConnection: TmncPGConnection;
begin
  Result := Session.Connection as TmncPGConnection;
end;

end.

