unit mncPostgre;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @author    Belal Hamed <belalhamed at gmail dot com>
 * @comment   Only for postgre 8.x or later
 *}

{*TODO
  - Retrieving Query Results Row-By-Row
      http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html
}
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{.$warning Postgre unit not stable yet}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, StrUtils,
  mncPGHeader,
  mnUtils, mnStreams, mncConnections, mncSQL, mncCommons, mncMetas;

type

  TmpgResultFormat = (mrfText, mrfBinary);
  TmncPGConnection = class;
  TmncCustomPGCommand = class;
  TmncPostgreFields = class;
  
  TPGListenThread = class(TThread)
  private
    FConnection: TmncPGConnection;
    FHandle: PPGconn;
    FChannel: string;
    FEvent: PPGnotify;
  protected
    procedure PostEvent;
    procedure Execute; override;
  public
    constructor Create(vConn: TmncPGConnection; const vChannel: string);
    destructor Destroy; override;
    property Connection: TmncPGConnection read FConnection;
    property Channel: string read FChannel;
  end;

  { TmncPGConnection }

  TmncPGConnection = class(TmncSQLConnection)
  private
    FHandle: PPGconn;
    FChannel: string;
    FEventListener: TPGListenThread;
    procedure SetChannel(const Value: string);
  protected
    function CreateConnection: PPGconn; overload;
    function CreateConnection(const vDatabase: string): PPGconn; overload;

    procedure InternalConnect(var vHandle: PPGconn);
    procedure InternalDisconnect(var vHandle: PPGconn);
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;

  protected
    procedure RaiseError(Error: Boolean; const ExtraMsg: string = ''); overload;
    procedure RaiseError(PGResult: PPGresult); overload;
    procedure DoNotify(vPID: Integer; const vName, vData: string); virtual;
    procedure Notify(vPID: Integer; const vName, vData: string);
    procedure Listen(const vChannel: string);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Interrupt;
    procedure Vacuum(Full: Boolean = False);
    function CreateSession: TmncSQLSession; override;
    class function Model: TmncConnectionModel; override;
    //TODO: Reconnect  use PQReset
    property Handle: PPGconn read FHandle;
    procedure CreateDatabase; overload;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); overload;
    procedure DropDatabase; overload;
    procedure DropDatabase(const vName: string); overload;
    procedure RenameDatabase(const vName, vToName: string); overload;
    function IsDatabaseExists(const vName: string): Boolean;

    procedure Execute(const vResource: string; const vSQL: string; vArgs: array of const); overload;
    procedure Execute(const vResource: string; const vSQL: string); overload;

    function Execute(const vSQL: string; vClearResult: Boolean = True): PPGresult; overload;
    function Execute(vHandle: PPGconn; const vSQL: string; vArgs: array of const; vClearResult: Boolean = True): PPGresult; overload;
    function Execute(vHandle: PPGconn; const vSQL: string; vClearResult: Boolean = True): PPGresult; overload;

    property Channel: string read FChannel write SetChannel;

    function loImport(const vFileName: string): OID; overload;
    function loImport(const vStream: TStream): OID; overload;

    function loExport(vOID: OID; const vFileName: string): Integer;
    function loUnlink(vOID: OID): Integer;
    function loCopy(vSrc: TmncPGConnection; vOID: OID): OID;
  end;

  { TmncPGSession }

  TmncPGSession = class(TmncSQLSession) //note now each session has it's connection 
  private
    FTokenID: Cardinal;
    FDBHandle: PPGconn;
    FExclusive: Boolean;
    FIsolated: Boolean;
    function GetConnection: TmncPGConnection;
    procedure SetConnection(const AValue: TmncPGConnection);
    procedure SetExclusive(const AValue: Boolean);
    function GetDBHandle: PPGconn;
  protected
    function NewToken: string;//used for new command name
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;

  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    function Execute(vSQL: string; vClearResult: Boolean=True): PPGresult;
    function CreateCommand: TmncSQLCommand; override;
    function CreateMeta: TmncMeta; override;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Connection: TmncPGConnection read GetConnection write SetConnection;
    property DBHandle: PPGconn read GetDBHandle;
    property Isolated: Boolean read FIsolated write FIsolated default True;
  end;

  TArrayOfPChar = array of PChar;

  TmncPostgreParam = class(TmncParam)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TmncPostgreParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  end;

  TmncPostgreField = class(TmncField)
  private
    FValue: Variant;
    FFields: TmncPostgreFields;
    function GetCommand: TmncCustomPGCommand;

  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    function GetAsDateTime: TDateTime; override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    property Fields: TmncPostgreFields read FFields;
    property Command: TmncCustomPGCommand read GetCommand;
  end;

  TmncPostgreFields = class(TmncFields)
  private
    FCommand: TmncCustomPGCommand;
  protected
    function CreateField(vColumn: TmncColumn): TmncField; override;
    property Command: TmncCustomPGCommand read FCommand;
  public
    function IsNull: Boolean;
  end;

  TPGColumn = class(TmncColumn)
  private
    FPGType: Integer;
    FFieldSize: Integer;
  public
    property PGType: Integer read FPGType write FPGType;
    property FieldSize: Integer read FFieldSize write FFieldSize;
  end;

  TPGColumns = class(TmncColumns)
  private
    function GetItem(Index: Integer): TPGColumn;
  protected
  public
    function Add(vName: string; vPGType, vSize: Integer): TmncColumn; overload;
    property Items[Index: Integer]: TPGColumn read GetItem; default;
  end;

  { TmncPGCommand }

  TmncCustomPGCommand = class(TmncSQLCommand)
  private
    function GetConnection: TmncPGConnection;
    function GetSession: TmncPGSession;
    procedure SetSession(const Value: TmncPGSession);
  protected
    FHandle: string;
    FResultFormat: TmpgResultFormat;
    FStatus: TExecStatusType;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetColumns: TPGColumns;
    procedure SetColumns(const Value: TPGColumns);

    function GetParamChar: string; override;
    property Connection: TmncPGConnection read GetConnection;
    property Session: TmncPGSession read GetSession write SetSession;
    function CreateColumns: TmncColumns; override;
    function CreateParams: TmncParams; override;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;

    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure FetchFields(vRes: PPGresult);
    procedure FetchValues(vRes: PPGresult; vTuple: Integer);
    function FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer): Variant; overload;
    procedure FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer; var Value: Variant); overload;
    procedure RaiseError(PGResult: PPGresult);
    procedure CreateParamValues(var Result: TArrayOfPChar);
    procedure FreeParamValues(var Result: TArrayOfPChar);
  public
    constructor CreateBy(vSession:TmncPGSession);
    destructor Destroy; override;
    procedure Clear; override;
    property Status: TExecStatusType read FStatus;
    property Handle: string read FHandle;//used for name in PQprepare
    property ResultFormat: TmpgResultFormat read FResultFormat write FResultFormat default mrfText;
    property Columns: TPGColumns read GetColumns write SetColumns;
  end;

  TmncPGCommand = class(TmncCustomPGCommand)
  private
    FStatment: PPGresult;
    FTuple: Integer;
    FTuples: Integer;
    function GetRecordCount: Integer;
  protected
    function IsSingleRowMode: Boolean; virtual;
    procedure InternalClose; virtual;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
  public
    function GetRowsChanged: Integer;
    function GetLastInsertID: Int64;
    property Statment: PPGresult read FStatment;//opened by PQexecPrepared
    property RecordCount: Integer read GetRecordCount;
  end;

  TmncPGCursorCommand = class(TmncCustomPGCommand)
  protected
    procedure InternalClose; virtual;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    function FetchSQL: string;
    function CloseSQL: string;
  end;

  TmncPGDDLCommand = class(TmncPGCommand)
  protected
    procedure DoPrepare; override;
    procedure DoExecute; override;
  end;

function EncodeBytea(const vStr: string): string; overload;
function EncodeBytea(vStr: PByte; vLen: Cardinal): string; overload;

function DecodeBytea(const vStr: string): string; overload;
function DecodeBytea(vStr: PByte; vLen: Cardinal): string; overload;
function PGDateToDateTime(const vStr: string): TDateTime;

implementation

uses
  Math;


function PGDateToDateTime(const vStr: string): TDateTime;
var
  T: String;
  Y, M, D, H, N, S: Word;
begin
  try
    T := SubStr(vStr, ' ', 0);

    Y := StrToIntDef(SubStr(T, '-', 0), 0);
    M := StrToIntDef(SubStr(T, '-', 1), 0);
    D := StrToIntDef(SubStr(T, '-', 2), 0);

    T := SubStr(vStr, ' ', 1);
    H := StrToIntDef(SubStr(T, ':', 0), 0);
    N := StrToIntDef(SubStr(T, ':', 1), 0);
    S := StrToIntDef(SubStr(T, ':', 2), 0);
    Result := EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0);
  except
    Result := 0;
  end;
end;

function BEtoN(Val: Integer): Integer;
begin
  Result := Val;
end;

function EncodeBytea(const vStr: string): string;
begin
  Result := EncodeBytea(PByte(vStr), Length(vStr) * SizeOf(Char));
end;

function EncodeBytea(vStr: PByte; vLen: Cardinal): string; overload;
var
  e: PByte;
  aLen: Longword;
begin
  if vLen=0 then
    Result := ''
  else
  begin
    e := PQescapeBytea(vStr, vLen, @aLen);
    try
      SetLength(Result,aLen+1);
      Move(e^, Result[2], aLen-1);
      Result[1] := '''';
      Result[aLen+1] := '''';
      //StrCopy(PChar(Result), e);
    finally
      PQFreemem(e);
    end;
  end;
end;

function DecodeBytea(const vStr: string): string;
begin
  Result := DecodeBytea(PByte(vStr), Length(vStr) * SizeOf(Char));
end;

function DecodeBytea(vStr: PByte; vLen: Cardinal): string; overload;
var
  e: PByte;
  aLen: Longword;
begin
  e := PQunescapeBytea(vStr, @aLen);
  try
    SetLength(Result, aLen);
    if aLen<>0 then
      Move(e^, Result[1], aLen);
  finally
    PQFreemem(e);
  end;
end;

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

procedure TmncPGConnection.RaiseError(PGResult: PPGresult);
var
  s : Utf8String;
  //ExtraMsg: string;
begin
  case PQresultStatus(PGResult) of
    PGRES_BAD_RESPONSE,PGRES_NONFATAL_ERROR, PGRES_FATAL_ERROR:
    begin
      s := PQresultErrorMessage(PGResult);
      raise EmncException.Create('Postgre command: ' + s) {$ifdef fpc} at get_caller_frame(get_frame) {$endif};
    end;
  end;
end;

procedure TmncPGConnection.RenameDatabase(const vName, vToName: string);
begin
  Execute('postgres', 'alter database %s rename to %s', [vName, vToName]);
end;

function TmncPGConnection.IsDatabaseExists(const vName: string): Boolean;
begin
  //Execute('postgres', 'select if exists %s;', [vName]);
  //SELECT datname FROM pg_catalog.pg_database WHERE lower(datname) = lower('dbname');
end;

procedure TmncPGConnection.SetChannel(const Value: string);
begin
  if FChannel <> Value then
  begin
    FChannel := Value;
    if Connected then
      Listen(FChannel);
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

procedure TmncPGConnection.Vacuum(Full: Boolean);
begin
  if Full then
    Execute('Vacuum Full')
  else
    Execute('Vacuum');
end;

procedure TmncPGConnection.Listen(const vChannel: string);
begin
  if FEventListener<>nil then
  begin
    FEventListener.Terminate;
    FreeAndNil(FEventListener);
  end;

  if vChannel<>'' then
  begin
    TPGListenThread.Create(Self, vChannel);
  end;
end;
                           
function TmncPGConnection.loCopy(vSrc: TmncPGConnection; vOID: OID): OID;
const
  cBufferSize = 512;

var
  c, fdd, fds: Integer;
  s: string;
begin
  Result := lo_creat(Handle, INV_READ or INV_WRITE);
  if Result<>0 then
  begin
    fdd := lo_open(Handle, Result, INV_WRITE or INV_READ);
    if (fdd<>-1) then
    begin
      try
        fds := lo_open(vSrc.Handle, vOID, INV_WRITE or INV_READ);
        if (fds<>-1) then
        begin
          SetLength(s, cBufferSize);
          try
            while True do
            begin
              c := lo_read(vSrc.Handle, fds, PChar(s), cBufferSize);
              if c<>0 then
                lo_write(Handle, fdd, PChar(s), c);
              if c<cBufferSize then Break;
            end;
          finally
            SetLength(s, 0);
            //lo_close(Handle, fds);
          end;
        end;
      finally
        //lo_close(Handle, fdd); make transaction abord
      end;
    end;
  end;
end;

function TmncPGConnection.loExport(vOID: OID; const vFileName: string): Integer;
begin
  Result := lo_export(Handle, vOID, PChar(vFileName));
end;

function TmncPGConnection.loImport(const vStream: TStream): OID;
const
  cBufferSize = 512;

var
  c, fdd: Integer;
  s: string;
begin
  Result := lo_creat(Handle, INV_READ or INV_WRITE);
  if Result<>0 then
  begin
    fdd := lo_open(Handle, Result, INV_WRITE or INV_READ);
    if (fdd<>-1) then
    begin
      try
        SetLength(s, cBufferSize);
        try
          while True do
          begin
            c := vStream.Read(s[1], cBufferSize);
            if c<>0 then
              lo_write(Handle, fdd, PChar(s), c);
            if c<cBufferSize then Break;
          end;
        finally
          SetLength(s, 0);
        end;
      finally
        //lo_close(Handle, fdd); make transaction abord
      end;
    end;
  end;
end;

function TmncPGConnection.loImport(const vFileName: string): OID;
begin
  Result := lo_import(Handle, PChar(vFileName));
end;

function TmncPGConnection.loUnlink(vOID: OID): Integer;
begin
  Result := lo_unlink(Handle, vOID);
end;

class function TmncPGConnection.Model: TmncConnectionModel;
begin
  Result.Name := 'PostgreSQL';
  Result.Title := 'Postgre Database';
  Result.Capabilities := [ccDB, ccSQL, ccNetwork, ccTransaction];
  //Result.Mode := smConnection;
end;

procedure TmncPGConnection.Notify(vPID: Integer; const vName, vData: string);
begin
  DoNotify(vPID, vName, vData);
end;

procedure TmncPGConnection.InternalConnect(var vHandle: PPGconn);
begin
  if AutoCreate then
  	CreateDatabase(Resource);
  vHandle := CreateConnection;
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
  try
    PQfinish(vHandle);
  finally
    vHandle := nil;
  end;
end;

function TmncPGConnection.CreateConnection: PPGconn;
begin
  Result := CreateConnection(Resource);
end;

function TmncPGConnection.CreateConnection(const vDatabase: string): PPGconn;
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
  Result := PQsetdbLogin(PChar(Host), PChar(aPort), PChar(PGOptions), PChar(PGtty), PChar(LowerCase(vDatabase)), PChar(UserName), PChar(Password));
end;

procedure TmncPGConnection.CreateDatabase;
begin
  CreateDatabase(Resource);
end;

procedure TmncPGConnection.CreateDatabase(const vName: string; CheckExists: Boolean = False);
var
  s: string;
begin
  s := 'Create Database ';
  if CheckExists then
    s := s + 'if not exists ';
  s := s + vName + ';';
  Execute('postgres', s);
end;

function TmncPGConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncPGSession.Create(Self);
end;

destructor TmncPGConnection.Destroy;
begin
  Listen('');
  inherited;
end;

procedure TmncPGConnection.DoConnect;
begin
  InternalConnect(FHandle);
  Listen(Channel);
end;

function TmncPGConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TmncPGConnection.DoDisconnect;
begin
  InternalDisconnect(FHandle);
end;

procedure TmncPGConnection.DoNotify(vPID: Integer; const vName, vData: string);
begin
end;

procedure TmncPGConnection.DropDatabase;
begin
  DropDatabase(Resource);
end;

procedure TmncPGConnection.DropDatabase(const vName: string);
begin
  Execute('postgres', 'drop database if exists %s;', [vName]);
end;

function TmncPGConnection.Execute(vHandle: PPGconn; const vSQL: string; vArgs: array of const; vClearResult: Boolean): PPGresult;
begin
  Result := Execute(vHandle, Format(vSQL, vArgs), vClearResult);
end;

function TmncPGConnection.Execute(vHandle: PPGconn; const vSQL: string; vClearResult: Boolean): PPGresult;
begin
  try
    Result := PQexec(vHandle, PChar(vSQL));
    try
      RaiseError(Result);
    except
      PQclear(Result);
      raise;
    end;
    if vClearResult then
    begin
      PQclear(Result);
      Result := nil;
    end;
  except
    raise;
  end;
end;

procedure TmncPGConnection.Execute(const vResource, vSQL: string; vArgs: array of const);
begin
  Execute(vResource, Format(vSQL, vArgs));
end;

procedure TmncPGConnection.Execute(const vResource, vSQL: string);
var
  aHandle: PPGconn;
begin
  aHandle := CreateConnection(vResource);
  try
    if PQstatus(aHandle) = CONNECTION_OK then
    begin
      Execute(aHandle, vSQL);
    end;
  finally
    PQfinish(aHandle);
  end;
end;

function TmncPGConnection.Execute(const vSQL: string; vClearResult: Boolean): PPGresult;
begin
  if PQstatus(FHandle) = CONNECTION_OK then
    Result := Execute(FHandle, vSQL, vClearResult)
  else
    Result := nil;
end;

{ TmncPGSession }

function TmncPGSession.CreateCommand: TmncSQLCommand;
begin
  Result := TmncPGCommand.CreateBy(Self);
end;

function TmncPGSession.CreateMeta: TmncMeta;
begin
  Result := nil;
end;

destructor TmncPGSession.Destroy;
begin
  inherited;
end;

procedure TmncPGSession.DoStart;
begin
	//TODO: Use session params to pass it here
  Execute('begin isolation level read committed;');
end;

procedure TmncPGSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
begin
  case How of
    sdaCommit: Execute('COMMIT');
    sdaRollback: Execute('ROLLBACK');
  end;

  if Retaining then
    DoStart
  else
    if FDBHandle <> nil then
      Connection.InternalDisconnect(FDBHandle);
end;

function TmncPGSession.NewToken: string;
var
  aID: Cardinal;
begin
  ConnectionLock.Enter;
  try
    Inc(FTokenID);
    aID := FTokenID;
  finally
    ConnectionLock.Leave;
  end;
  Result := 'minilib_' + IntToStr(aID);
end;

function TmncPGSession.Execute(vSQL: string; vClearResult: Boolean): PPGresult;
begin
  Result := Connection.Execute(DBHandle, vSQL, vClearResult);
end;

constructor TmncPGSession.Create(vConnection: TmncConnection);
begin
  inherited;
  FIsolated := True;
end;

function TmncPGSession.GetConnection: TmncPGConnection;
begin
  Result := inherited Connection as TmncPGConnection;
end;

function TmncPGSession.GetDBHandle: PPGconn;
begin
  {if Connection<>nil then
    Result := Connection.Handle
  else
    Result := nil;}
  ConnectionLock.Enter;
  try
    if Isolated then
      Result := Connection.Handle
    else
    begin
      if FDBHandle = nil then
        Connection.InternalConnect(FDBHandle);
      Result := FDBHandle;
    end;
  finally
    ConnectionLock.Leave;
  end;
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

procedure TmncPGCommand.InternalClose;
begin
  PQclear(FStatment);
  FStatment := nil;
  FStatus := PGRES_EMPTY_QUERY;
  FTuple := 0;
  //Connection.Execute();
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
  f: Integer;//Result Field format
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
    case ResultFormat of
      mrfBinary: f := 1;
      else
        f := 0;
    end;
    FStatment := PQexecPrepared(Session.DBHandle, PChar(FHandle), Binds.Count, P, nil, nil, f);
    //FStatment := PQexec(Session.DBHandle, PChar(SQL.Text));
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(FStatment);
  FTuples := PQntuples(FStatment);
  FBOF := True;
  FTuple := 0;
  FEOF := not (FStatus in [PGRES_TUPLES_OK]);
  try
    RaiseError(FStatment);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGCommand.DoNext;
begin
  if (Status in [PGRES_TUPLES_OK]) then
  begin
    if FBOF then
    begin
      FetchFields(FStatment);
      FBOF := False;
    end
    else
      inc(FTuple);
    FEOF := FTuple >= FTuples;
    if not FEOF then
      FetchValues(FStatment, FTuple);
  end
  else
    FEOF := True;
end;

procedure TmncPGCommand.DoPrepare;
var
  c: PPGconn;
  r: PPGresult;
  f: Integer;
  s: UTF8String;
begin
  FBOF := True;
  FEOF := False;
  FHandle := Session.NewToken;
  ParseSQL([psoAddParamsID], '$');
  c := Session.DBHandle;

  if IsSingleRowMode then
  begin
    //belal check for result ok
{    PQconsumeInput(c);} //TODO
    //f := PQsendQuery(c, PChar(SQLProcessed.SQL));
    {f := PQsendPrepare(c, PAnsiChar(FHandle), PAnsiChar(SQLProcessed.SQL), 0 , nil);
    r := PQgetResult(c);}
  end
  else
  begin
    s := UTF8Encode(SQLProcessed.SQL);
    r := PQprepare(c, PAnsiChar(FHandle), PAnsiChar(s), 0 , nil);
    try
      RaiseError(r);
    finally
      PQclear(r);
    end;
  end;
end;

procedure TmncPGCommand.DoClose;
begin
  InternalClose;
end;

function TmncPGCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil;
end;

function TmncPGCommand.GetRecordCount: Integer;
begin
  Result := FTuples;
end;

function TmncPGCommand.IsSingleRowMode: Boolean;
begin
  Result := False;
end;

function TmncPGCommand.GetRowsChanged: Integer;
begin
  CheckActive;
  Result := StrToIntDef(PQcmdTuples(FStatment), 0);
end;

{ TmncPostgreParam }

constructor TmncPostgreParam.Create;
begin
  inherited;
end;

destructor TmncPostgreParam.Destroy;
begin
  inherited;
end;

function TmncPostgreParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncPostgreParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncPostgreParams }

function TmncPostgreParams.CreateParam: TmncParam;
begin
  Result := TmncPostgreParam.Create;
end;

{ TmncPostgreField }

function StrToBoolEx(const vStr: string): Boolean;
begin
  Result := (vStr<>'') and (vStr[1] in ['1', 't', 'T', 'y', 'Y']);
end;

function TmncPostgreField.GetAsBoolean: Boolean;
begin
  Result := StrToBoolEx(AsString);
end;

function TmncPostgreField.GetAsDateTime: TDateTime;
begin
  Result := PGDateToDateTime(AsString);
  //Result := inherited GetAsDateTime;
end;

function TmncPostgreField.GetCommand: TmncCustomPGCommand;
begin
  Result := Fields.Command; //note must not be nil at any way
end;

function TmncPostgreField.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncPostgreField.SetAsBoolean(const AValue: Boolean);
begin
  Value := AValue;
end;

procedure TmncPostgreField.SetValue(const AValue: Variant);
begin
  FValue := AValue; 
end;

{ TmncPostgreFields }

function TmncPostgreFields.CreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncPostgreField.Create(vColumn);
  with TmncPostgreField(Result) do
    FFields := Self;
end;

function TmncPostgreFields.IsNull: Boolean;
var
  i: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if not Items[i].IsNull then
    begin
      Result := False;
      Break;
    end;
end;

{ TPGColumns }

function TPGColumns.Add(vName: string; vPGType, vSize: Integer): TmncColumn;
begin
  Result := inherited Add(Count, vName, dtUnknown, TPGColumn);
  TPGColumn(Result).PGType := vPGType;
  TPGColumn(Result).FieldSize := vSize;
end;

function TPGColumns.GetItem(Index: Integer): TPGColumn;
begin
  Result := TPGColumn(inherited GetItem(Index));
end;

{ TmncPGDDLCommand }

procedure TmncPGDDLCommand.DoExecute;
var
  Values: TArrayOfPChar;
begin
  if FStatment <> nil then
    PQclear(FStatment);
  try
    FStatment := PQexec(Session.DBHandle, PChar(SQL.Text));
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(FStatment);
  FTuples := PQntuples(FStatment);
  FBOF := True;
  FEOF := FStatus <> PGRES_TUPLES_OK;
  try
    RaiseError(FStatment);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGDDLCommand.DoPrepare;
begin
  //no need prepare
end;

{ TPGListenThread }

constructor TPGListenThread.Create(vConn: TmncPGConnection; const vChannel: string);
begin
  inherited Create(True);
  FConnection := vConn;
  FChannel := vChannel;
  FConnection.InternalConnect(FHandle);
  FConnection.Execute(FHandle, 'LISTEN "%s";', [vChannel]);
  FConnection.FEventListener := Self;
  Resume;
end;

destructor TPGListenThread.Destroy;
begin
  try
    PQfinish(FHandle);
  except
    //no exception needed
  end;
  inherited;
end;

procedure TPGListenThread.Execute;
begin
  while not Terminated do
  begin
    PQconsumeInput(FHandle);
    FEvent := PQnotifies(FHandle);
    if FEvent<>nil then
    begin
      Synchronize(PostEvent);
      PQFreemem(FEvent);
    end;
    Sleep(10); //belal: cpu usage .....
  end;  
end;

procedure TPGListenThread.PostEvent;
begin
  FConnection.Notify(FEvent^.be_pid, FEvent^.relname, FEvent^.extra);
end;

{ TmncCustomPGCommand }

procedure TmncCustomPGCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

constructor TmncCustomPGCommand.CreateBy(vSession: TmncPGSession);
begin
  inherited CreateBy(vSession);
  FResultFormat := mrfText;
end;

function TmncCustomPGCommand.CreateColumns: TmncColumns;
begin
  Result := TPGColumns.Create;
end;

function TmncCustomPGCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncPostgreFields.Create(vColumns);
  with TmncPostgreFields(Result) do
    FCommand := Self;
end;

function TmncCustomPGCommand.CreateParams: TmncParams;
begin
  Result := TmncPostgreParams.Create;
end;

procedure TmncCustomPGCommand.CreateParamValues(var Result: TArrayOfPChar);
var
  i: Integer;
  s: UTF8String;
  p: TmncParam;
begin
  FreeParamValues(Result);
  SetLength(Result, Binds.Count);

  for i := 0 to Binds.Count -1 do
  begin
    p := Binds.Items[i].Param;

    if (p = nil) or (p.IsNull) then
      Result[i] := nil
    else
    begin
      case p.Value of
        VarDate:
          s := UTF8Encode(FormatDateTime('yyyy-mm-dd hh:nn:ss', p.Value));
        else
          s := UTF8Encode(p.Value);
      end;
      GetMem(Result[i], Length(s) + 1);
      StrMove(PAnsiChar(Result[i]), PAnsiChar(s), Length(s) + 1);
    end;
  end;
end;

destructor TmncCustomPGCommand.Destroy;
begin
  inherited;
end;

procedure TmncCustomPGCommand.DoCommit;
begin
  Session.Commit;
end;

procedure TmncCustomPGCommand.DoRollback;
begin
  Session.Rollback;
end;

procedure TmncCustomPGCommand.FetchFields(vRes: PPGresult);
var
  i: Integer;
  aName: string;
  c: Integer;
begin
  Columns.Clear;
  c := PQnfields(vRes);
  for i := 0 to c - 1 do
  begin
    aName :=  DequoteStr(PQfname(vRes, i));
    Columns.Add(aName, PQftype(vRes, i), PQfsize(vRes, i));
  end;
end;

function TmncCustomPGCommand.FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer): Variant;
begin
  FetchValue(vRes, vTuple, vIndex, Result);
end;

procedure TmncCustomPGCommand.FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer; var Value: Variant);
    function _BRead(vSrc: PChar; vCount: Longint): Integer;
    var
      t: PChar;
      i: Integer;
    begin
      Result := 0;
      t := vSrc;
      Inc(t, vCount-1);
      for I := 0 to vCount - 1 do
      begin
        Result := Result + Ord(t^) * (1 shl (i*8));
        Dec(t);
      end;
    end;

    function _DRead(vSrc: PChar; vCount: Longint): Int64;
    var
      t: PChar;
      c, i: Integer;
    begin
      Result := 0;
      t := vSrc;
      Inc(t, vCount);
      c := vCount div 2;
      for I := 0 to c - 1 do
      begin
        Dec(t, 2);
        Result := Result + _BRead(t, 2) * Trunc(Power(10000, i));
      end;
    end;
var
  t: Int64;
  d: Double;
  //aType: Integer;
  aFieldSize: Integer;
  p: PChar;
begin
  if PQgetisnull(vRes, vTuple, vIndex) <> 0 then
    Value := NULL
  else
  begin
    p := PQgetvalue(vRes, vTuple, vIndex);
    if ResultFormat=mrfText then
      Value := string(p)
    else
    begin
      aFieldSize :=PQgetlength(vRes, vTuple, vIndex);
      case Columns[vIndex].PGType of
        Oid_Bool: Value := (p^ <> #0);
        Oid_varchar, Oid_bpchar, Oid_name: Value := string(p);
        Oid_oid, Oid_int2: Value := _BRead(p, 2);
        Oid_int4: Value := _BRead(p, 4);
        Oid_int8: Value := _BRead(p, 8);
        Oid_Money: Value := _BRead(p, 8) / 100;
        Oid_Float4, Oid_Float8:
        begin
        end;
        Oid_Date:
        begin
          //d := BEtoN(plongint(p)^) + 36526;
          d := _BRead(p, aFieldSize) + 36526; //36526 = days between 31/12/1899 and 01/01/2000  = delphi, Postgre (0) date
          Value := TDateTime(d);
        end;
        Oid_Time,
        Oid_TimeStamp:
        begin
          t := BEtoN(pint64(p)^);
          //Value := TDateTime(t);//todo
          Value := t;//todo
        end;
        OID_NUMERIC:
        begin
           t := _BRead(p, 2);
           d := Power(10, 2 * t);


          inc(p, 8);
          t := _DRead(p, aFieldSize - 8);
          Value := t / d;
        end;
        Oid_Unknown:;
      end;
    end;
  end;
end;

procedure TmncCustomPGCommand.FetchValues(vRes: PPGresult; vTuple: Integer);
var
  i: Integer;
  c: Integer;
  aCurrent: TmncFields;
begin
  c := Columns.Count;
  if c > 0 then
  begin
    aCurrent := CreateFields(Columns);
    for i := 0 to c - 1 do
      aCurrent.Add(i, FetchValue(vRes, vTuple, i));
    Fields := aCurrent;
  end;
end;

procedure TmncCustomPGCommand.FreeParamValues(var Result: TArrayOfPChar);
var
  i: Integer;
begin
  for i := 0 to Length(Result) - 1 do
    FreeMem(Result[i]);
  SetLength(Result, 0);
end;

function TmncCustomPGCommand.GetColumns: TPGColumns;
begin
  Result := inherited Columns as TPGColumns
end;

function TmncCustomPGCommand.GetConnection: TmncPGConnection;
begin
  Result := Session.Connection as TmncPGConnection;
end;

function TmncCustomPGCommand.GetParamChar: string;
begin
  Result := '$';
end;

function TmncCustomPGCommand.GetSession: TmncPGSession;
begin
  Result := inherited Session as TmncPGSession;
end;

procedure TmncCustomPGCommand.RaiseError(PGResult: PPGresult);
begin
  Connection.RaiseError(PGResult);
end;

procedure TmncCustomPGCommand.SetColumns(const Value: TPGColumns);
begin
  inherited Columns := Value;
end;

procedure TmncCustomPGCommand.SetSession(const Value: TmncPGSession);
begin
  inherited Session := Value;
end;

{ TmncPGCursorCommand }

function TmncPGCursorCommand.CloseSQL: string;
begin
  Result := Format('close %s', [Handle]);
end;

procedure TmncPGCursorCommand.DoClose;
begin
  inherited;

end;

procedure TmncPGCursorCommand.DoExecute;
var
  Values: TArrayOfPChar;
  P: pointer;
  aStatment: PPGresult;
begin
  try
    if Params.Count > 0 then
    begin
      CreateParamValues(Values);
      P := @Values[0];
    end
    else
      p := nil;
    aStatment := PQexecPrepared(Session.DBHandle, PChar(FHandle), Binds.Count, P, nil, nil, 0);
    //FStatment := PQexec(Session.DBHandle, PChar(SQL.Text));
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(aStatment);
  FBOF := True;
  FEOF := not (FStatus in [PGRES_TUPLES_OK]);
  try
    RaiseError(aStatment);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGCursorCommand.DoNext;
var
  aStatment: PPGresult;
begin
  aStatment := Session.Execute(FetchSQL, False);
  if aStatment<>nil then
  begin
    FStatus := PQresultStatus(aStatment);
    if (Status in [PGRES_TUPLES_OK]) then
    begin
      if FBOF then
      begin
        FetchFields(aStatment);
        FBOF := False;
      end;
      FetchValues(aStatment, 0);
      if TmncPostgreFields(Fields).IsNull then
        FEOF := True
      else
        FEOF := False;
    end
    else
      FEOF := True;
    PQclear(aStatment);
  end;
end;

procedure TmncPGCursorCommand.DoPrepare;
var
  s, b:string;
  r: PPGresult;
  c: PPGconn;
begin
  FBOF := True;
  FHandle := Session.NewToken;
  ParseSQL([psoAddParamsID], '$');
  c := Session.DBHandle;
  if ResultFormat=mrfBinary then
    b := 'binary'
  else
    b := '';

  s := Format('declare %s %s cursor for %s', [Handle, b, SQLProcessed.SQL]);
  r := PQprepare(c, PChar(FHandle), PChar(s), 0 , nil);
  try
    RaiseError(r);
  finally
    PQclear(r);
  end;
end;

function TmncPGCursorCommand.FetchSQL: string;
begin
  Result := Format('fetch in %s', [Handle]);
end;

function TmncPGCursorCommand.GetActive: Boolean;
begin
  Result := not FBOF;
end;

function TmncPGCursorCommand.GetEOF: Boolean;
begin
  Result := FEOF;
end;

procedure TmncPGCursorCommand.InternalClose;
begin
  Session.Execute(CloseSQL);
  FStatus := PGRES_EMPTY_QUERY;
  FBOF := True;
end;

end.
