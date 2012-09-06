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
{$warning Postgre unit not stable yet}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, StrUtils,
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
    function GetDBHandle: PPGconn;
  protected
    function NewToken: string;//used for new command name
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(SQL: string);
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Connection: TmncPGConnection read GetConnection write SetConnection;
    property DBHandle: PPGconn read GetDBHandle;
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
    constructor Create(vSession:TmncPGSession);
    destructor Destroy; override;
    procedure Clear; override;
    function GetRowsChanged: Integer;
    function GetLastInsertID: Int64;
    property Statment: PPGresult read FStatment;//opened by PQexecPrepared
    property Status: TExecStatusType read FStatus;
    property Handle: string read FHandle;//used for name in PQprepare
  end;

implementation


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
  if not Assigned(PQsetdbLogin) then
    raise EmncException.Create('PQsetdbLogin not assigned');
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

procedure TmncPGSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
begin
  //Execute('COMMIT');
  //Execute('ROLLBACK');
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

function TmncPGSession.GetDBHandle: PPGconn;
begin
  if Connection<>nil then
    Result := Connection.Handle
  else
    Result := nil;
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

constructor TmncPGCommand.Create(vSession:TmncPGSession);
begin
  inherited Create;
  Session := vSession;
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

function TmncPGCommand.GetLastInsertID: Int64;
begin
  Result := 0;
end;

procedure TmncPGCommand.DoExecute;
var
  Values: TArrayOfPChar;
  P: pointer;
  FF: Longint;//Result Field format
begin
  if FStatment <> nil then
    PQclear(FStatment);
  try
    if Params.Count > 0 then
    begin
      CreateParamValues(Values);
      P := @Values[0];
    end
    else
      p := nil;
    FF :=   1;//format as binnary.
    FStatment := PQexecPrepared(Session.DBHandle, PChar(FHandle), Params.Count, P, nil, nil, FF);
    //FStatment := PQexec(Session.DBHandle, PChar(SQL.Text));
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
  //Fields.Clear;
  c := PQnfields(FStatment);
  for i := 0 to c - 1 do
  begin
    aName :=  DequoteStr(PQfname(FStatment, i));
    Columns.Add(aName, dtUnknown);
  end;
end;

function BEtoN(Val: Integer): Integer;
begin
  Result := Val;
end;

procedure TmncPGCommand.FetchValues;
var
  str: Utf8String;
  t: Int64;
  d: Double;
  aType: Integer;
  v: Variant;
  aFieldSize: Integer;
  p: PChar;
  i: Integer;
  c: Integer;
  aCurrent: TmncFields;
begin
  c := PQnfields(FStatment);
  if c > 0 then
  begin
    aCurrent := TmncFields.Create(Columns);
    for i := 0 to c - 1 do
    begin
      if PQgetisnull(Statment, FTuple, i) <> 0 then
        aCurrent.Add(i, NULL)
      else
      begin
        aFieldSize := PQfsize(Statment, i);
        p := PQgetvalue(FStatment, FTuple, i);
        case PQftype(Statment, i) of
          Oid_varchar, Oid_bpchar, Oid_name:
            v := string(p);
          Oid_oid,
          Oid_int4:
            v := BEtoN(PInteger(p)^);
          Oid_int2:
            v  := BEtoN(PShortInt(p)^);
          Oid_int8:
            v := BEtoN(PInt64(p)^);
          Oid_Float4, Oid_Float8:
          begin
          end;
          Oid_Date:
          begin
            d := BEtoN(plongint(p)^) + 36526;
            v := TDateTime(d);
          end;
          Oid_Time,
          Oid_TimeStamp:
          begin
            t := BEtoN(pint64(p)^);
            //v := TDateTime(t);//todo
            v := t;//todo
          end;
          Oid_Bool:
             v := (p[0] <> #0);
          Oid_Money:;
          Oid_Unknown:;
        end;

        aCurrent.Add(i, v);
      end;
    end;
    Fields := aCurrent;
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

function TmncPGCommand.GetRowsChanged: Integer;
begin
  CheckActive;
  Result := StrToIntDef(PQcmdTuples(FStatment), 0);
end;

end.

