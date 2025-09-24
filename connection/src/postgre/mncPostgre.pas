unit mncPostgre;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Hamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @comment   Only for postgre 8.x or later
 *}

{*TODO
  - Retrieving Query Results Row-By-Row
      http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html

}

{.$define ThreadedPGClear}

interface

uses
  Classes, SysUtils, Variants, StrUtils, Contnrs, SyncObjs, DateUtils,
  mnTypes, mnUtils,
  mncCommons, mncConnections, mncSQL, mnClasses, mncDB, mncPGHeader;

const
  cBufferSize          = 2048;

type

  TmpgResultFormat = (mrfText, mrfBinary);
  TmncPGConnection = class;
  TmncCustomPGCommand = class;
  TmncPostgreFields = class;

  TPGClearThread = class(TThread)
  private
    FStatement: PPGresult;
  protected
    procedure Execute; override;
  public
    constructor Create(vStatement: PPGresult);
    destructor Destroy; override;

    property Statement: PPGresult read FStatement;
  end;

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
    FClientEncoding: string;
    FByteaOutput: string;
    FDateStyle: string;
    FAppName: string;
    FSimpleConnection: Boolean;
    FUseSSL: Boolean;
    procedure SetChannel(const Value: string);
  protected
    procedure InternalConnect(out vHandle: PPGconn; vResource: string = '');
    procedure InternalDisconnect(var vHandle: PPGconn);
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoConnected; override;
    function GetConnected:Boolean; override;

    procedure DoResetConnection(PGResult: PPGresult; var vResume: Boolean); virtual; //zaher
    function ResetConnection(PGResult: PPGresult): Boolean; //zaher

  protected
    procedure InternalRaiseError(vHandle: PPGconn; const ExtraMsg: string = ''); overload;
    procedure RaiseError(Error: Boolean; const ExtraMsg: string = ''); overload; deprecated;
    procedure RaiseResultError(PGResult: PPGresult); overload;
    procedure DoNotify(vPID: Integer; const vName, vData: string); virtual;
    procedure Notify(vPID: Integer; const vName, vData: string);
    procedure Listen(const vChannel: string);
    procedure DoInit; override;

    function DoGetNextIDSQL(const vName: string; vStep: Integer): string;//?
    procedure DoClone(vConn: TmncSQLConnection); override;

    function loImport(const vFileName: string): Integer; overload;
    function loImport(vStream: TStream; vOID: Integer): Integer; overload;

    function loExport(vOID: Integer; const vFileName: string): Integer; overload;
    function loExport(vOID: Integer; const vStream: TStream): Integer; overload;

    function loUnlink(vOID: Integer): Integer;
    function loCopy(vOID: Integer): Integer; overload;
    property Channel: string read FChannel write SetChannel; //TODO make new classes for it
    procedure DoExecute(const vSQL: string); overload; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CreateTransaction: TmncSQLTransaction; override;
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    property Handle: PPGconn read FHandle;

    procedure Interrupt;
    //TODO: Reconnect  use PQReset
    procedure CreateDatabase(CheckExists: Boolean = False); overload;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); overload; override;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); overload; override;
    procedure RenameDatabase(const vName, vToName: string); overload;
    function IsDatabaseExists(const vName: string): Boolean; override;
    function EnumDatabases: TStrings; override;
    procedure TerminateConnections(const vResource: string);
    function GetParamChar: string; override;

    //function UniqueDBName(const vBase: string): string; override; //zaher, do not copy
    function loCopy(vSrc: TmncPGConnection; vOID: Integer): Integer; overload;

    property ClientEncoding: string read FClientEncoding write FClientEncoding;
    property ByteaOutput: string read FByteaOutput write FByteaOutput;
    property DateStyle: string read FDateStyle write FDateStyle;
    property AppName: string read FAppName write FAppName;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property SimpleConnection: Boolean read FSimpleConnection write FSimpleConnection;
  end;

  { TmncPGTransaction }

  TmncPGTransaction = class(TmncSQLTransaction) //note now each Transaction has it's connection
  private
    //FTokenID: Cardinal;
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
    procedure DoStop(How: TmncTransactionAction; Retaining: Boolean); override;
    function DoCreateCommand: TmncSQLCommand; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(const vSQL: string);
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Connection: TmncPGConnection read GetConnection write SetConnection;
    property DBHandle: PPGconn read GetDBHandle;
    property Isolated: Boolean read FIsolated write FIsolated default True;
  end;

  TArrayOfPChar = array of PByte;

  { TmncPostgreParam }

  TmncPostgreParam = class(TmncVariantParam)
  private
    FBuffer: string;
    FFieldSize: Integer;
  protected
    procedure SetAsString(const AValue: string); override;
    procedure SetAsDate(const AValue: TDateTime); override;
    procedure SetAsDateTime(const AValue: TDateTime); override;
  public
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Buffer: string read FBuffer write FBuffer;
  end;

  { TmncPostgreParams }

  TmncPostgreParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  end;

  { TmncPostgreField }

  TmncPostgreField = class(TmncField)
  private
    FData: PByte;
    FDataLen: Integer;

    FValue: string;
    FIsNull: Boolean;
    function GetData: string;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;


    function GetAsText: string; override;
    function GetAsString: string; override;
    function GetAsInteger: Integer; override;
    function GetAsInt64: Int64; override;
    function GetAsDouble: Double; override;
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDate: TDateTime; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsTime: TDateTime; override;
    function GetIsNull: Boolean; override;

    procedure SetAsText(const AValue: string); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsDouble(const AValue: Double); override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsCurrency(const AValue: Currency); override;
    procedure SetAsDate(const AValue: TDateTime); override;
    procedure SetAsDateTime(const AValue: TDateTime); override;
    procedure SetAsTime(const AValue: TDateTime); override;
    procedure SetIsNull(const AValue: Boolean); override;
  end;

  { TmncPostgreFields }

  TmncPostgreFields = class(TmncFields)
  private
    FCommand: TmncCustomPGCommand;
  protected
    function DoCreateField(vColumn: TmncColumn): TmncField; override;
    property Command: TmncCustomPGCommand read FCommand;

  public
    constructor Create(vColumns: TmncColumns); override;
    destructor Destroy; override;

    function IsNull: Boolean;
  end;

  { TmncPGBind }

  TmncPGBind = class(TmncBind)
  private
  protected
  public
  end;

  { TmncPGBinds }

  TmncPGBinds = class(TmncBinds)
  private
    function GetItem(Index: Integer): TmncPGBind;
  protected
    function CreateBind: TmncBind; override;
  public
    procedure Clear; override;
    property Items[Index: Integer]: TmncPGBind read GetItem; default;
  end;

  { TmncPGColumn }

  TmncPGColumn = class(TmncColumn)
  private
    FPGType: Integer;
    FFieldSize: Integer;
    procedure SetPGType(const Value: Integer);
  public
    property PGType: Integer read FPGType write SetPGType;
    property FieldSize: Integer read FFieldSize write FFieldSize;
  end;

  { TmncPGColumns }

  TmncPGColumns = class(TmncColumns)
  private
    function GetItem(Index: Integer): TmncPGColumn;
  protected
  public
    function Add(vName: string; vPGType, vSize: Integer): TmncColumn; overload;
    property Items[Index: Integer]: TmncPGColumn read GetItem; default;
  end;

  { TmncCustomPGCommand }

  TmncCustomPGCommand = class(TmncSQLCommand)
  private
    function GetConnection: TmncPGConnection;
    function GetTransaction: TmncPGTransaction;
    procedure SetTransaction(const Value: TmncPGTransaction);
    function GetParams: TmncPostgreParams;
    function GetBinds: TmncPGBinds;
  protected
    FHandle: UTF8String;
    FResultFormat: TmpgResultFormat;
    FStatus: TExecStatusType;
    function GetColumns: TmncPGColumns;

    property Connection: TmncPGConnection read GetConnection;
    property Transaction: TmncPGTransaction read GetTransaction write SetTransaction;
    function CreateColumns: TmncColumns; override;
    function CreateParams: TmncParams; override;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;

    procedure FetchFields(vRes: PPGresult);
    procedure FetchValues(vRes: PPGresult; vTuple: Integer);
    function FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer): string; overload;
    function FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer; out Value: string): Boolean; overload;
    procedure RaiseResultError(PGResult: PPGresult);
    procedure CreateParamValues(var Result: TArrayOfPChar);
    procedure FreeParamValues(var Result: TArrayOfPChar);
    function CreateBinds: TmncBinds; override;
    property Binds: TmncPGBinds read GetBinds;
    property Params: TmncPostgreParams read GetParams;

  public
    constructor CreateBy(vTransaction:TmncPGTransaction);
    destructor Destroy; override;
    procedure Clear; override;
    property Status: TExecStatusType read FStatus;
    property Handle: UTF8String read FHandle;//used for name in PQprepare
    property ResultFormat: TmpgResultFormat read FResultFormat write FResultFormat default mrfText;
    property Columns: TmncPGColumns read GetColumns;
  end;

  TmncPGCommand = class(TmncCustomPGCommand)
  private
    FStatement: PPGresult;
    FTuple: Integer;
    FTuples: Integer;
    FSingleRowMode: Boolean;
    function GetRecordCount: Integer;
  protected
    procedure DeallocateStatement;
    function FetchStatement: Boolean;
    procedure InternalClose; virtual;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    procedure ClearStatement; virtual;
    function GetParseOptions: TmncParseSQLOptions; override;
  public
    function GetRowsChanged: Integer; override;
    function GetLastInsertID: Int64;
    property Statement: PPGresult read FStatement;//opened by PQexecPrepared
    property RecordCount: Integer read GetRecordCount;
    //https://www.postgresql.org/docs/9.2/libpq-single-row-mode.html
    property SingleRowMode: Boolean read FSingleRowMode write FSingleRowMode;
  end;

  { TmncPGDDLCommand }

  TmncPGDDLCommand = class(TmncPGCommand)
  protected
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure ClearStatement; override;
    procedure DoParse; override;
    procedure DoNext; override;
  end;

  TmncPGCursorCommand = class(TmncCustomPGCommand)
  private
    FStatement: PPGresult;
  protected
    procedure InternalClose; virtual;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    function FetchSQL: UTF8String;
    function CloseSQL: UTF8String;
  end;

function PQLibVersion: Integer;

implementation

uses
  Math;

var
  fTokenID: Cardinal = 0;

function BEtoN(Val: Integer): Integer;
begin
  Result := Val;
end;

function PQLibVersion: Integer;
begin
  PGLib.Load;
  Result := mncPGHeader.PQlibVersion();
end;

{ TmncPGBinds }

function TmncPGBinds.GetItem(Index: Integer): TmncPGBind;
begin
  Result := inherited Items[Index] as TmncPGBind;
end;

function TmncPGBinds.CreateBind: TmncBind;
begin
  Result := TmncPGBind.Create;
end;

procedure TmncPGBinds.Clear;
begin
  inherited Clear;
end;

procedure TmncPGConnection.InternalRaiseError(vHandle: PPGconn; const ExtraMsg: string);
var
  s : string;
begin
  //s := 'Postgre connection failed' + #13 + UTF8ToString(PQerrorMessage(FHandle));
  s := 'Postgre connection failed' + #13 + PAnsiChar(PQerrorMessage(vHandle));
  if ExtraMsg <> '' then
    s := s + ' - ' + ExtraMsg;
  raise EmncException.Create(s);
end;

procedure TmncPGConnection.RaiseError(Error: Boolean; const ExtraMsg: string);
begin
  if Error then
    InternalRaiseError(FHandle, ExtraMsg);
end;

procedure TmncPGConnection.RaiseResultError(PGResult: PPGresult);
var
  s : AnsiString;
  t: TExecStatusType;
begin
  t := PQresultStatus(PGResult);
  case t of
    PGRES_BAD_RESPONSE,
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR:
    begin
      if (PQstatus(FHandle) = CONNECTION_BAD) and not ResetConnection(PGResult) then
      begin
        s := PAnsiChar(PQresultErrorMessage(PGResult));
        raise EmncException.Create('Postgre lost connection with: ' + s);
      end
      else
      begin
        s := PAnsiChar(PQresultErrorMessage(PGResult));
        raise EmncException.Create('Postgre command: ' + s);
      end;
    end;
  end;
end;

function TmncPGConnection.ResetConnection(PGResult: PPGresult): Boolean;
begin
  Result := False;
  DoResetConnection(PGResult, Result);
end;

procedure TmncPGConnection.DropDatabase(const vName: string; CheckExists: Boolean = False);
begin
  //TODO check
  CloneExecute('postgres', 'drop database if exists %s;', [vName]);
end;

procedure TmncPGConnection.RenameDatabase(const vName, vToName: string);
begin
  CloneExecute('postgres', 'alter database %s rename to %s', [vName, vToName]);
end;

function TmncPGConnection.EnumDatabases: TStrings;
var
  aTr: TmncSQLTransaction;
  aCMD: TmncSQLCommand;
begin
  Result := TStringList.Create;
  try
    aTr := CreateTransaction;
    try
      aCMD := aTr.CreateCommand;
      try
        aCMD.SQL.Text := 'select datname from pg_database where datistemplate = false';
        aCMD.Execute;
        while not aCMD.Done do
        begin
          Result.Add(aCMD.Field['datname'].AsString);
          aCMD.Next;
        end;
      finally
        aCMD.Free;
      end;
    finally
      aTr.Free;
    end;
  except
    FreeAndNil(Result);
  end;
end;

procedure TmncPGConnection.TerminateConnections(const vResource: string);
begin
  CloneExecute('postgres', 'select pg_terminate_backend(pid) from pg_stat_activity where datname = ''%s''', [vResource]);
end;

function TmncPGConnection.GetParamChar: string;
begin
  Result := '$';
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

procedure TmncPGConnection.Listen(const vChannel: string);
begin
  if FEventListener<>nil then
  begin
    FEventListener.Terminate;
    FreeAndNil(FEventListener);
  end;

  if vChannel <> '' then
  begin
    TPGListenThread.Create(Self, vChannel);
  end;
end;

function TmncPGConnection.loCopy(vSrc: TmncPGConnection; vOID: Integer): Integer;

var
  c, fdd, fds: Integer;
  s: ansistring;
begin
  Result := lo_creat(Handle, INV_READ or INV_WRITE);
  if Result <> 0 then
  begin
    fdd := lo_open(Handle, Result, INV_WRITE or INV_READ);
    if (fdd <> -1) then
    begin
      try
        fds := lo_open(vSrc.Handle, vOID, INV_READ);
        if (fds <> -1) then
        begin
          SetLength(s, cBufferSize);
          try
            while True do
            begin
              c := lo_read(vSrc.Handle, fds, PByte(s), cBufferSize);
              if c > 0 then
                lo_write(Handle, fdd, PByte(s), c);
              if c < cBufferSize then Break;
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

function TmncPGConnection.loCopy(vOID: Integer): Integer;
begin
  Result := loCopy(Self, vOID);
end;

function TmncPGConnection.loExport(vOID: Integer; const vStream: TStream): Integer;
var
  c, fds: Integer;
  s: ansistring;
begin
  if vOID<>0 then
  begin
    fds := lo_open(Handle, vOID, INV_READ);
    if (fds<>-1) then
    begin
      Result := 0;
      SetLength(s, cBufferSize);
      try
        while True do
        begin
          c := lo_read(Handle, fds, PByte(s), cBufferSize);
          Inc(Result, c);
          if c>0 then
            vStream.Write(PByte(s)^, c);
          if c<cBufferSize then Break;
        end;
      finally
        SetLength(s, 0);
        //lo_close(Handle, fds);
      end;
    end
    else
      Result := -1;
  end
  else
    Result := 0;
end;

function TmncPGConnection.loExport(vOID: Integer; const vFileName: string): Integer;
var
  f: TFileStream;
begin
  //if vUseStream then
  begin
    if FileExists(vFileName) then //zaher, belal why?
    begin
      f := TFileStream.Create(vFileName, fmOpenWrite or fmShareDenyNone);
      f.Size := 0;
    end
    else
      f := TFileStream.Create(vFileName, fmCreate or fmShareDenyNone);
    try
      Result := loExport(vOID, f);
    finally
      f.Free;
    end;
  end
  //else
    //Result := lo_export(Handle, vOID, PByte(AnsiString(vFileName)));
end;

function TmncPGConnection.loImport(vStream: TStream; vOID: Integer): Integer;
const
  cBufferSize = 512;

var
  c, fdd: Integer;
  s: ansistring;
begin
  if vOID=0 then
    Result := lo_creat(Handle, INV_READ or INV_WRITE)
  else
  begin
    Execute('select lo_create(%d)', [vOID]);
    Result := vOID;
  end;

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
            if c>0 then
              lo_write(Handle, fdd, PByte(s), c);
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

function TmncPGConnection.loImport(const vFileName: string): Integer;
var
  f: TFileStream;
begin
  if FileExists(vFileName) then
  begin
    //if vUseStream then
    begin
      f := TFileStream.Create(vFileName, fmOpenRead or fmShareDenyNone);
      try
        Result := loimport(f, 0);
      finally
        f.Free;
      end;
    end
    //else
      //Result := lo_import(Handle, PByte(AnsiString(vFileName)));
  end
  else
    Result := 0;
end;

function TmncPGConnection.loUnlink(vOID: Integer): Integer;
begin
  Result := lo_unlink(Handle, vOID);
end;

class function TmncPGConnection.Capabilities: TmncCapabilities;
begin
  Result:= [ccDB, ccSQL, ccCreate, ccDrop, ccNetwork, ccTransaction];
end;

class function TmncPGConnection.EngineName: string;
begin
  Result := 'PostgreSQL';
end;

procedure TmncPGConnection.Notify(vPID: Integer; const vName, vData: string);
begin
  DoNotify(vPID, vName, vData);
end;

procedure TmncPGConnection.InternalConnect(out vHandle: PPGconn; vResource: string);
var
  aHost, aPort: UTF8String;
  aResource, aUser, aPassword: UTF8String;
  aSsl, aSslComp: UTF8String;
  aUrl: UTF8String;
begin
  if not Assigned(PQsetdbLogin) then
  	raise EmncException.Create('Postgre not assigned');

  if Host = '' then
    aHost := '127.0.0.1'
  else
    aHost := Host;

  if Port = '' then
    aPort := '5432'
  else
    aPort := Port;

  if vResource <> '' then
    aResource := vResource
  else
    aResource := Resource;

  {if UserName = '' then
    aUser := 'postgres' maybe windows authentication
  else}
  aUser := UserName;

  aPassword := Password;
  if UseSSL then
  begin
    aSsl := 'prefer';
    aSslComp := '1';
  end
  else
  begin
    aSsl := 'disable';
    aSslComp := '0';
  end;

  //Result := PQsetdbLogin(PByte(aHost), PByte(aPort), nil, nil, PByte(aDB), PByte(aUser), PByte(aPass));
  //aUrl := Format('postgresql://%s:%s@%s:%s/%s?sslmode=%s&sslcompression=%s&application_name=%s', [aUser, aPass, aHost, aPort, aDB, aSsl, aSslComp, AppName]);
  if SimpleConnection then
    vHandle := PQsetdbLogin(PByte(aHost), PByte(aPort), nil, nil, PByte(aResource), PByte(aUser), PByte(aPassword))
  else
  begin
    if (aUser='') and (aPassword='') then
      aUrl := Format('host=%s port=%s dbname=''%s'' sslmode=%s sslcompression=%s application_name=''%s''', [aHost, aPort, aResource, aSsl, aSslComp, AppName])
    else
      aUrl := Format('user=%s password=''%s'' host=%s port=%s dbname=''%s'' sslmode=%s sslcompression=%s application_name=''%s''', [aUser, aPassword, aHost, aPort, aResource, aSsl, aSslComp, AppName]);
    //aUrl := Format('postgresql://%s:%s@%s:%s/%s?sslmode=%s&sslcompression=%s&application_name=%s', [aUser, aPass, aHost, aPort, aDB, aSsl, aSslComp, AppName]);
    vHandle := PQconnectdb(PByte(aUrl));
  end;

  try
    if PQstatus(vHandle) = CONNECTION_BAD then
      InternalRaiseError(vHandle);
  except
    PQfinish(vHandle);
    vHandle := nil; //<-this mean something wrong in the code
    raise;
  end;
end;

procedure TmncPGConnection.InternalDisconnect(var vHandle: PPGconn);
begin
  try
    PQfinish(vHandle);
    vHandle := nil;
  except
    vHandle := nil;
    //TODO Why there is no raise
  end;
end;

procedure TmncPGConnection.CreateDatabase(CheckExists: Boolean);
begin
  CreateDatabase(Resource, CheckExists);
end;

procedure TmncPGConnection.CreateDatabase(const vName: string; CheckExists: Boolean);
begin
  //TODO CheckExists
  CloneExecute('postgres', 'Create Database %s;', [vName]);
end;

function TmncPGConnection.CreateTransaction: TmncSQLTransaction;
begin
  Result := TmncPGTransaction.Create(Self);
end;

function TmncPGConnection.IsDatabaseExists(const vName: string): Boolean;
var
  s: UTF8String;
  r: PPGresult;
  conn: PPGconn;
begin
  InternalConnect(conn, 'postgres');
  try
    s := Format('select 1 from pg_database where datname=''%s''', [LowerCase(vName)]);
    r := PQexec(conn, PByte(s));
    try
      Result := (r <> nil) and (PQntuples(r) = 1);
      RaiseResultError(r);
    finally
      PQclear(r);
    end;
  finally
    PQfinish(conn);
  end;
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

procedure TmncPGConnection.DoConnected;
begin
  inherited;
  if ClientEncoding <> '' then
    Execute('set client_encoding to ''%s'';', [ClientEncoding]);
  if ByteaOutput <> '' then
    Execute('set bytea_output = ''%s'';', [ByteaOutput]);
  if DateStyle <> '' then
    Execute('set datestyle to ''%s'';', [DateStyle]);
end;

procedure TmncPGConnection.DoClone(vConn: TmncSQLConnection);
begin
  inherited;
  TmncPGConnection(vConn).UseSSL := UseSSL;
  TmncPGConnection(vConn).SimpleConnection := SimpleConnection;
end;

function TmncPGConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TmncPGConnection.DoDisconnect;
begin
  try
    InternalDisconnect(FHandle);
  except
    beep; //belal need review some time access violation
  end;
end;

function TmncPGConnection.DoGetNextIDSQL(const vName: string; vStep: Integer): string;
var
  n: string;
begin
  n := Format('%s', [vName]);
  case vStep of
    0: Result := Format('select currval(''%s'')', [n]);
    1: Result := Format('select nextval(''%s'')', [n]);
    else
      Result := Format('select setval(''%s'', nextval(''%s'')+%d-1)', [n, n, vStep]);
  end;
end;

procedure TmncPGConnection.DoInit;
begin
  inherited;
  PGLib.Load;
end;

procedure TmncPGConnection.DoNotify(vPID: Integer; const vName, vData: string);
begin

end;

procedure TmncPGConnection.DoResetConnection(PGResult: PPGresult; var vResume: Boolean);
begin
end;

procedure TmncPGConnection.DoExecute(const vSQL: string);
var
  s: utf8string;
  r: PPGresult;
begin
  s := vSQL;
  r := PQexec(FHandle, PByte(s));
  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;
end;

{ TmncPGTransaction }

function TmncPGTransaction.DoCreateCommand: TmncSQLCommand;
begin
  Result := TmncPGCommand.CreateBy(Self);
end;

procedure TmncPGTransaction.DoStart;
begin
	//TODO: Use Transaction params to pass it here
  Execute('begin isolation level read committed;');
  //Execute('begin isolation level serializable;');
end;

procedure TmncPGTransaction.DoStop(How: TmncTransactionAction; Retaining: Boolean);
begin
  case How of
    sdaCommit: Execute('COMMIT');
    sdaRollback: Execute('ROLLBACK');
  end;
  if FDBHandle <> nil then
  begin
    Connection.InternalDisconnect(FDBHandle);
  end;
end;

function TmncPGTransaction.NewToken: string;
begin
  {$ifdef FPC}
  InterlockedIncrement(fTokenID);
  {$else}
  AtomicIncrement(fTokenID);
  {$endif}
  Result := 'minilib_' + IntToStr(fTokenID);
end;

procedure TmncPGTransaction.Execute(const vSQL: string);
begin
  Connection.Execute(vSQL);
end;

constructor TmncPGTransaction.Create(vConnection: TmncConnection);
begin
  inherited;
  FIsolated := True;
end;

destructor TmncPGTransaction.Destroy;
begin
  inherited;
end;

function TmncPGTransaction.GetConnection: TmncPGConnection;
begin
  Result := inherited Connection as TmncPGConnection;
end;

function TmncPGTransaction.GetDBHandle: PPGconn;
begin
  {if Connection<>nil then
    Result := Connection.Handle
  else
    Result := nil;}
  //ConnectionLock.Enter;
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
    //ConnectionLock.Leave;
  end;
end;

procedure TmncPGTransaction.SetConnection(const AValue: TmncPGConnection);
begin
  inherited Connection := AValue;
end;

procedure TmncPGTransaction.SetExclusive(const AValue: Boolean);
begin
  if FExclusive <> AValue then
  begin
    FExclusive := AValue;
    if Active then
      raise EmncException.Create('You can not set Exclusive when Transaction active');
  end;
end;

{ TmncPGCommand }

procedure TmncPGCommand.InternalClose;
begin
  ClearStatement;
  FStatement := nil;
  FStatus := PGRES_EMPTY_QUERY;
  FTuple := 0;
  //Connection.Execute();
end;

function TmncPGCommand.GetLastInsertID: Int64;
begin
  Result := 0;
end;

function TmncPGCommand.GetParseOptions: TmncParseSQLOptions;
begin
  Result := [psoAddParamsID];
end;

procedure TmncPGCommand.DoExecute;
var
  Values: TArrayOfPChar;
  P: pointer;
  f: Integer; //Result Field format
begin
  Values := nil;
  if FStatement <> nil then
    PQclear(FStatement);
  try
    if Binds.Count > 0 then
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

    if SingleRowMode then
    begin
      PQsendQueryPrepared(Transaction.DBHandle, PByte(FHandle), Binds.Count, P, nil, nil, f);
      PQsetSingleRowMode(Transaction.DBHandle);
      //f := PQsetSingleRowMode(Transaction.DBHandle);
      //FStatement := PQgetResult(Transaction.DBHandle);
      FetchStatement;
    end
    else
    begin
      FStatement := PQexecPrepared(Transaction.DBHandle, PByte(FHandle), Binds.Count, P, nil, nil, f);
    end;

  finally
    FreeParamValues(Values);
  end;


  FStatus := PQresultStatus(FStatement);

  if SingleRowMode then
    HitDone((FStatement=nil) or not (FStatus in [PGRES_SINGLE_TUPLE]))
  else
    HitDone((FStatement=nil) or not (FStatus in [PGRES_TUPLES_OK]));

  FTuple := 0;
  FTuples := PQntuples(FStatement);

  if FStatement<>nil then
  begin
    try
      RaiseResultError(FStatement);
    except
      InternalClose;
      raise;
    end;
  end;
end;

procedure TmncPGCommand.DoNext;
begin
  if not Done then
  begin
    if Ready then
    begin
      FetchFields(FStatement);
      FTuple := 0;
    end
    else
      inc(FTuple);

    if SingleRowMode then
    begin
      FetchValues(FStatement, FTuple);
      PQclear(FStatement);
      FStatement := PQgetResult(Transaction.DBHandle);
      HitDone(Statement = nil);
    end
    else
    begin
      HitDone(FTuple >= FTuples);
      if not Done then
        FetchValues(FStatement, FTuple);
    end;
  end;
end;

procedure TmncPGCommand.DoPrepare;
var
  c: PPGconn;
  r: PPGresult;
  s: UTF8String;
//  i: Integer;
//  z: Integer;
begin
  FHandle := Transaction.NewToken;
  c := Transaction.DBHandle;
  s := UTF8Encode(GetProcessedSQL);

  r := PQprepare(c, PByte(FHandle), PByte(s), Params.Count, nil);
  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;

  {r := PQdescribePrepared(Transaction.DBHandle, PByte(FHandle)); //TODO: To indecate the size of param, no chance to get it
  try
    z := PQnparams(r);

    for i := 0 to Params.Count - 1 do
    begin
      //z := PQparamtype(r, i);
      z := PQfsize(r, i); //not works with params
      (Params.Items[i] as TmncPostgreParam).FieldSize := z;
    end;

    RaiseResultError(r);
  finally
    PQclear(r);
  end;}
end;

function TmncPGCommand.FetchStatement: Boolean;
begin
  //PQconsumeInput(Transaction.DBHandle);
  {while PQisBusy(Transaction.DBHandle)<>0 do
  begin
    Sleep(1);

    //if PQconsumeInput(Transaction.DBHandle)=0 then
      //Break;
  end;}

  FStatement := PQgetResult(Transaction.DBHandle);
  Result := True;

end;

procedure TmncPGCommand.ClearStatement;
begin
  {$ifdef ThreadedPGClear}
  TPGClearThread.Create(FStatement); //tooo slow in ddl command  :(
  {$else}
  PQclear(FStatement);
  {$endif}
end;

procedure TmncPGCommand.DeallocateStatement;
var
  s: UTF8String;
  r: PPGresult;
begin
  if Prepared then
  begin
    try
      s := UTF8Encode('deallocate ' + FHandle);
      r := PQexec(Transaction.DBHandle, PByte(s));
      PQclear(r);
    except
    end;
  end;
end;

procedure TmncPGCommand.DoClose;
begin
  DeallocateStatement;
  InternalClose;
end;

function TmncPGCommand.GetActive: Boolean;
begin
  Result := FStatement <> nil;
end;

function TmncPGCommand.GetRecordCount: Integer;
begin
  Result := FTuples;
end;

function TmncPGCommand.GetRowsChanged: Integer;
begin
  CheckActive;
  Result := StrToIntDef(PAnsiChar(PQcmdTuples(FStatement)), 0);
end;

{ TmncPostgreParam }

procedure TmncPostgreParam.SetAsDate(const AValue: TDateTime);
begin
  if AValue = 0 then
    Clear
  else
    inherited;
end;

procedure TmncPostgreParam.SetAsDateTime(const AValue: TDateTime);
begin
  if AValue = 0 then
    Clear
  else
    inherited;
end;

procedure TmncPostgreParam.SetAsString(const AValue: string);
begin
  if AValue='' then
    Clear
  else
    inherited;
end;

{ TmncPostgreParams }

function TmncPostgreParams.CreateParam: TmncParam;
begin
  Result := TmncPostgreParam.Create;
end;

{ TmncPostgreField }

function StrToBoolEx(const vStr: string): Boolean;
begin
  Result := (vStr<>'') and CharInSet(vStr[1], ['1', 't', 'T', 'y', 'Y']);
end;

function TmncPostgreField.GetAsBoolean: Boolean;
begin
  Result := StrToBoolEx(GetData);
end;

function TmncPostgreField.GetAsCurrency: Currency;
begin
  //Result := StrToCurrNumber(AsString);
  Result := StrToCurrDef(AsString, 0);
end;

function TmncPostgreField.GetAsDate: TDateTime;
begin
  Result := Trunc(AsDateTime);
end;

function TmncPostgreField.GetAsDateTime: TDateTime;
begin
  Result := ISOStrToDate(AsString);
  //Result := inherited GetAsDateTime;
end;

function TmncPostgreField.GetAsDouble: Double;
begin
  //Result := StrToDoubleNumber(GetData); //zaher check maybe needit
  Result := StrToFloatDef(GetData, 0);
end;

function TmncPostgreField.GetAsInt64: Int64;
begin
  Result := StrToInt64Def(GetData, 0);
end;

function TmncPostgreField.GetAsInteger: Integer;
begin
  Result := StrToIntDef(GetData, 0);
end;

function TmncPostgreField.GetAsString: string;
begin
  Result := GetData;
end;

function TmncPostgreField.GetAsText: string;
begin
  Result := GetData;
end;

function TmncPostgreField.GetAsTime: TDateTime;
begin
  Result := Frac(AsDateTime);
end;

function TmncPostgreField.GetData: string;
var
  s: AnsiString;
begin
  if FData<>nil then
  begin
    if FDataLen<>0 then
    begin
      SetString(s, PAnsiChar(FData), FDataLen);
      {$ifdef FPC}
      Result := s;
      {$else}
      Result := UTF8ToString(s);
      {$endif}
    end
    else
      Result := '';
  end
  else
    Result := FValue;
end;

function TmncPostgreField.GetIsNull: Boolean;
begin
  Result := FIsNull;
end;

function TmncPostgreField.GetValue: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := GetData;
end;

procedure TmncPostgreField.SetAsBoolean(const AValue: Boolean);
begin
  FValue := BoolToStr(AValue);
end;

procedure TmncPostgreField.SetAsCurrency(const AValue: Currency);
begin
  FValue := CurrToStr(AValue);
end;

procedure TmncPostgreField.SetAsDate(const AValue: TDateTime);
begin
  FValue := DateToStr(AValue);
end;

procedure TmncPostgreField.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := DateTimeToStr(AValue);
end;

procedure TmncPostgreField.SetAsDouble(const AValue: Double);
begin
  FValue := FloatToStr(AValue);
end;

procedure TmncPostgreField.SetAsInt64(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

procedure TmncPostgreField.SetAsInteger(const AValue: Integer);
begin
  FValue := IntToStr(AValue);
end;

procedure TmncPostgreField.SetAsString(const AValue: string);
begin
  FValue := AValue;
end;

procedure TmncPostgreField.SetAsText(const AValue: string);
begin
  FValue := AValue;
end;

procedure TmncPostgreField.SetAsTime(const AValue: TDateTime);
begin
  FValue := TimeToStr(AValue);
end;

procedure TmncPostgreField.SetIsNull(const AValue: Boolean);
begin
  FIsNull := True;
end;

procedure TmncPostgreField.SetValue(const AValue: Variant);
begin
  if AValue=varNull then
    FValue := ''
  else
    FValue := AValue;
end;

{ TmncPostgreFields }

constructor TmncPostgreFields.Create(vColumns: TmncColumns);
begin
  inherited;
end;

destructor TmncPostgreFields.Destroy;
begin
  inherited;
end;

function TmncPostgreFields.DoCreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncPostgreField.Create(vColumn);
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

{ TmncPGColumns }

function TmncPGColumns.Add(vName: string; vPGType, vSize: Integer): TmncColumn;
begin
  Result := inherited Add(Count, vName, dtUnknown, TmncPGColumn);
  TmncPGColumn(Result).PGType := vPGType;
  TmncPGColumn(Result).FieldSize := vSize;
end;

function TmncPGColumns.GetItem(Index: Integer): TmncPGColumn;
begin
  Result := TmncPGColumn(inherited Items[Index]);
end;

{ TmncPGDDLCommand }

procedure TmncPGDDLCommand.ClearStatement;
begin
  //inherited;
  //PQclear(FStatement);
end;

procedure TmncPGDDLCommand.DoExecute;
var
  s: UTF8String;
  r: PPGresult;
begin
  HitDone;
  s := UTF8Encode(SQL.Text);
  r := PQexec(Transaction.DBHandle, PByte(s));
  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;
end;

{var
  s: UTF8String;
begin
  if FStatement <> nil then PQclear(FStatement);
  try
    s := UTF8Encode(SQL.Text);
    FStatement := PQexec(Transaction.DBHandle, PByte(s));
  finally
  end;
  FStatus := PQresultStatus(FStatement);
  FTuples := PQntuples(FStatement);
  FBOF := True;
  FEOF := FStatus <> PGRES_COMMAND_OK;
  try
    RaiseError(FStatement);
  except
    InternalClose;
    raise;
  end;
end;}

procedure TmncPGDDLCommand.DoParse;
begin
  //inherited;
  //no paras needed
end;

procedure TmncPGDDLCommand.DoPrepare;
begin
  //no need prepare
  //NextOnExecute := False;
end;

procedure TmncPGDDLCommand.DoNext;
begin
  //
end;

{ TPGListenThread }

constructor TPGListenThread.Create(vConn: TmncPGConnection; const vChannel: string);
var
  r: PPGresult;
begin
  inherited Create(False);
  FConnection := vConn;
  FChannel := vChannel;
  FConnection.InternalConnect(FHandle); //TODO should be outside of connection class
  r := PQexec(FHandle, PByte('LISTEN "' + vChannel + '";'));
  try
    FConnection.RaiseResultError(r);
  finally
    PQclear(r);
  end;
  FConnection.FEventListener := Self;
  //Resume;
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
    if FEvent <> nil then
    begin
      Synchronize(PostEvent);
      PQFreemem(FEvent);
    end;
    Sleep(10); //belal: cpu usage ..... //zaher: wrong
  end;
end;

procedure TPGListenThread.PostEvent;
begin
  //FConnection.Notify(FEvent^.be_pid, FEvent^.relname, FEvent^.extra);
end;

{ TmncCustomPGCommand }

procedure TmncCustomPGCommand.Clear;
begin
  inherited;
  //Reset;
end;

constructor TmncCustomPGCommand.CreateBy(vTransaction: TmncPGTransaction);
begin
  inherited CreateBy(vTransaction);
  FResultFormat := mrfText;
end;

function TmncCustomPGCommand.CreateColumns: TmncColumns;
begin
  Result := TmncPGColumns.Create;
end;

function TmncCustomPGCommand.CreateFields(vColumns: TmncColumns): TmncFields;
var
  i: Integer;
begin
  Result := TmncPostgreFields.Create(vColumns);

  TmncPostgreFields(Result).FCommand := Self;

  for i := 0 to vColumns.Count - 1 do
    Result.Add(Result.CreateField(i));
end;

function TmncCustomPGCommand.CreateParams: TmncParams;
begin
  Result := TmncPostgreParams.Create;
end;

procedure TmncCustomPGCommand.CreateParamValues(var Result: TArrayOfPChar);
var
  i: Integer;
  s: UTF8String;
  bind: TmncPGBind;
begin
  FreeParamValues(Result);
  SetLength(Result, Binds.Count);

  for i := 0 to Binds.Count -1 do
  begin
    bind := Binds.Items[i];

    if (bind.Param.IsNull) then
      Result[i] := nil
    else
    begin
      case VarType(bind.Param.Value) of
        VarDate: s := UTF8Encode(FormatDateTime('yyyy-mm-dd hh:nn:ss', bind.Param.Value));
        varCurrency: s := CurrToStr(bind.Param.Value);
        varDouble: s := FloatToStr(bind.Param.Value);
        else
          begin
            s := UTF8Encode(bind.Param.Value);
            if (cmoTruncate in Options) and ((bind.Param as TmncPostgreParam).FieldSize > 0) then
              s := MidStr(s, 1, (bind.Param as TmncPostgreParam).FieldSize)
            else
          end;
      end;
      GetMem(Result[i], Length(s) + 1);
      //StrMove(PByte(Result[i]), PByte(s), Length(s) + 1);
      Move(PByte(s)^, PByte(Result[i])^, Length(s) + 1);
    end;
  end;
end;

destructor TmncCustomPGCommand.Destroy;
begin

  inherited;
end;

procedure TmncCustomPGCommand.FetchFields(vRes: PPGresult);
var
  i: Integer;
  aName, t: string;
  c, n: Integer;
begin
  Columns.Clear;
  c := PQnfields(vRes);
  for i := 0 to c - 1 do
  begin
    aName :=  DequoteStr(PAnsiChar(PQfname(vRes, i)));
    t := aName;
    n := 1;
    while Columns.Find(aName)<>nil do
    begin
      aName := t + '_' + IntToStr(n);
      Inc(n);
    end;

    Columns.Add(aName, PQftype(vRes, i), PQfsize(vRes, i));
  end;
  Fields := CreateFields(Columns);
end;

function TmncCustomPGCommand.FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer): string;
begin
  FetchValue(vRes, vTuple, vIndex, Result);
end;

function TmncCustomPGCommand.FetchValue(vRes: PPGresult; vTuple: Integer; vIndex: Integer; out Value: string): Boolean;

    function _BRead(vSrc: PByte; vCount: Longint): Integer;
    var
      t: PByte;
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

    function _DRead(vSrc: PByte; vCount: Longint): Int64;
    var
      t: PByte;
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
  //t: Int64;
  //d: Double;
  //aType: Integer;
  //aFieldSize: Integer;
  p: PByte;
begin
  Result := PQgetisnull(vRes, vTuple, vIndex) = 0;
  if Result then
  begin
    p := PQgetvalue(vRes, vTuple, vIndex);
    {$ifdef FPC}
    Value := PAnsiChar(p);
    {$else}
    Value := UTF8ToString(PAnsiChar(p));
    {$endif}
    {if ResultFormat=mrfText then
      Value := UTF8ToString(p)
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
    end;}
  end
  else
    Value := ''
end;

procedure TmncCustomPGCommand.FetchValues(vRes: PPGresult; vTuple: Integer);
var
  i, c: Integer;
  f: TmncPostgreField;
begin
  c := Columns.Count;
  if c > 0 then
  begin
    for i := 0 to c - 1 do
    begin
      f := TmncPostgreField(Fields.Items[i]);
      if PQgetisnull(vRes, vTuple, i)=0 then
      begin
        f.FData := PQgetvalue(vRes, vTuple, i);
        f.FDataLen := PQgetlength(vRes, vTuple, i);
        //f.FDataLen := NullPos(f.FData)-1;
        //f.FDataLen := 0;
        f.FIsNull := False;
      end
      else
      begin
        f.FIsNull := True;
      end;
    end;
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

function TmncCustomPGCommand.CreateBinds: TmncBinds;
begin
  Result := TmncPGBinds.Create;
end;

function TmncCustomPGCommand.GetColumns: TmncPGColumns;
begin
  Result := inherited Columns as TmncPGColumns
end;

function TmncCustomPGCommand.GetConnection: TmncPGConnection;
begin
  Result := Transaction.Connection as TmncPGConnection;
end;

function TmncCustomPGCommand.GetParams: TmncPostgreParams;
begin
  Result := inherited Params as TmncPostgreParams;
end;

function TmncCustomPGCommand.GetTransaction: TmncPGTransaction;
begin
  Result := inherited Transaction as TmncPGTransaction;
end;

procedure TmncCustomPGCommand.RaiseResultError(PGResult: PPGresult);
begin
  Connection.RaiseResultError(PGResult);
end;

procedure TmncCustomPGCommand.SetTransaction(const Value: TmncPGTransaction);
begin
  inherited Transaction := Value;
end;

function TmncCustomPGCommand.GetBinds: TmncPGBinds;
begin
  Result := inherited Binds as TmncPGBinds;
end;

{ TmncPGCursorCommand }

function TmncPGCursorCommand.CloseSQL: UTF8String;
begin
  Result := Format('close %s', [Handle]);
end;

procedure TmncPGCursorCommand.DoClose;
begin
end;

procedure TmncPGCursorCommand.DoExecute;
var
  Values: TArrayOfPChar;
  P: pointer;
  aStatement: PPGresult;
begin
  try
    Values := nil;
    if Params.Count > 0 then
    begin
      CreateParamValues(Values);
      P := @Values[0];
    end
    else
      p := nil;
    aStatement := PQexecPrepared(Transaction.DBHandle, PByte(FHandle), Binds.Count, P, nil, nil, 0);
    //FStatement := PQexec(Transaction.DBHandle, PChar(SQL.Text));
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(aStatement);
  HitDone(not (FStatus in [PGRES_COMMAND_OK]));
  try
    RaiseResultError(aStatement);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGCursorCommand.DoNext;
begin
  if FStatement<>nil then
    PQclear(FStatement);

  FStatement := PQexec(Transaction.DBHandle, PByte(FetchSQL));
  if FStatement <> nil then
  begin
    FStatus := PQresultStatus(FStatement);
    if (Status in [PGRES_TUPLES_OK]) then
    begin
      if Ready then
      begin
        FetchFields(FStatement);
      end;
      FetchValues(FStatement, 0);
      if TmncPostgreFields(Fields).IsNull then
        HitDone;
    end
    else
      HitDone;
  end;
end;

procedure TmncPGCursorCommand.DoPrepare;
var
  s, b: ansistring;
  r: PPGresult;
  c: PPGconn;
  i: Integer;
begin
  FHandle := Transaction.NewToken;
  ParseSQL([psoAddParamsID]);
  c := Transaction.DBHandle;

  if ResultFormat = mrfBinary then
    b := 'binary'
  else
    b := '';

  s := Format('declare %s %s cursor for %s', [Handle, b, GetProcessedSQL]);
  r := PQprepare(c, PByte(FHandle), PByte(s), 0 , nil);

  for i := 0 to Params.Count - 1 do
  begin
    //Binds[i].FFieldSize := PQfsize(r, i); //TODO
  end;

  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;
end;

function TmncPGCursorCommand.FetchSQL: UTF8String;
begin
  Result := Format('fetch in %s', [Handle]);
end;

function TmncPGCursorCommand.GetActive: Boolean;
begin
  Result := FStatement <> nil;
end;

procedure TmncPGCursorCommand.InternalClose;
begin
  PQexec(Transaction.DBHandle, PByte(CloseSQL));
  FStatus := PGRES_EMPTY_QUERY;
end;

{ TmncPGColumn }

procedure TmncPGColumn.SetPGType(const Value: Integer);
begin
  FPGType := Value;
  case Value of
    Oid_Bool: DataType := dtBoolean;
    Oid_oid, Oid_int2, Oid_int4, Oid_int8: DataType := dtInteger;
    Oid_Money: DataType := dtCurrency;
    Oid_Float4, Oid_Float8: DataType := dtCurrency;
    Oid_Date: DataType := dtDate;
    Oid_Time, Oid_TimeStamp: DataType := dtDateTime;
    OID_NUMERIC: DataType := dtCurrency;
  end;
end;

{ TPGClearThread }

constructor TPGClearThread.Create(vStatement: PPGresult);
begin
  inherited Create(False);
  FStatement := vStatement;
  FreeOnTerminate := True;
  //Start;
end;

destructor TPGClearThread.Destroy;
begin
  inherited;
end;

procedure TPGClearThread.Execute;
{var
  block: PPGresult_data;
  c: Integer;}
begin
  {c := 0;
  block := FStatement^.curBlock;
  while block <> nil do
  begin
    FStatement^.curBlock := block^.Next;
    PQFreemem(block);
    block :=FStatement^.curBlock;
    inc(c);
    if (c mod 10) = 0 then
      Sleep(1);
  end;}
  PQclear(FStatement);
end;

initialization
  Engines.RegisterConnection(TmncPGConnection.EngineName, 'PostgreSQL Database', TmncPGConnection);
end.
