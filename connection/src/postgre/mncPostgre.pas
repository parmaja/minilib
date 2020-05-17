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
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @comment   Only for postgre 8.x or later
 *}

{*TODO
  - Retrieving Query Results Row-By-Row
      http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html

}

interface

uses
  Classes, SysUtils, Variants, StrUtils, Contnrs, SyncObjs, DateUtils,
  mnUtils,
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
  public
    constructor Create; override;
    destructor Destroy; override;

    function CreateSession: TmncSQLSession; override;
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    property Handle: PPGconn read FHandle;
    procedure Execute(vSQL: string); overload; override;

    procedure Interrupt;
    //TODO: Reconnect  use PQReset
    procedure CreateDatabase(CheckExists: Boolean = False); overload;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); overload; override;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); overload; override;
    procedure RenameDatabase(const vName, vToName: string); overload;
    function IsDatabaseExists(vName: string): Boolean; override;
    function EnumDatabases: TStrings;
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

  { TmncPGSession }

  TmncPGSession = class(TmncSQLSession) //note now each session has it's connection
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
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
    function InternalCreateCommand: TmncSQLCommand; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(const vSQL: string);
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Connection: TmncPGConnection read GetConnection write SetConnection;
    property DBHandle: PPGconn read GetDBHandle;
    property Isolated: Boolean read FIsolated write FIsolated default True;
  end;

  TArrayOfPChar = array of PAnsiChar;

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
    FData: PAnsiChar;
    FDataLen: Integer;

    FValue: string;
    FIsNull: Boolean;
    FIsEmpty: Boolean;
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
    function GetIsEmpty: Boolean; override;


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
    function Find(const vName: string): TmncItem; override;
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
    function GetSession: TmncPGSession;
    procedure SetSession(const Value: TmncPGSession);
    function GetParams: TmncPostgreParams;
    function GetBinds: TmncPGBinds;
  protected
    FHandle: ansistring;
    FResultFormat: TmpgResultFormat;
    FStatus: TExecStatusType;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetColumns: TmncPGColumns;

    property Connection: TmncPGConnection read GetConnection;
    property Session: TmncPGSession read GetSession write SetSession;
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
    constructor CreateBy(vSession:TmncPGSession);
    destructor Destroy; override;
    procedure Clear; override;
    property Status: TExecStatusType read FStatus;
    property Handle: ansistring read FHandle;//used for name in PQprepare
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
    function GetDone: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    function IsSingleRowMode: Boolean;
    procedure ClearStatement; virtual;
  public
    function GetRowsChanged: Integer; override;
    function GetLastInsertID: Int64;
    property Statement: PPGresult read FStatement;//opened by PQexecPrepared
    property RecordCount: Integer read GetRecordCount;
    property SingleRowMode: Boolean read FSingleRowMode write FSingleRowMode;
  end;

  TmncPGDDLCommand = class(TmncPGCommand)
  protected
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure ClearStatement; override;
    procedure DoParse; override;
  end;

  TmncPGCursorCommand = class(TmncCustomPGCommand)
  private
    FStatement: PPGresult;
  protected
    procedure InternalClose; virtual;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetDone: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    function FetchSQL: AnsiString;
    function CloseSQL: AnsiString;
  end;

function EncodeBytea(const vStr: string): string; overload;
function EncodeBytea(vStr: PByte; vLen: Cardinal): string; overload;

function DecodeBytea(const vStr: string): string; overload;
function DecodeBytea(vStr: PByte; vLen: Cardinal): string; overload;

implementation

uses
  Math;

var
  fTokenID: Cardinal = 0;

function BEtoN(Val: Integer): Integer;
begin
  Result := Val;
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
      SetLength(Result, aLen + 1);
      Move(e^, Result[2], aLen - 1);
      Result[1] := '''';//todo, what is that?
      Result[aLen+1] := '''';
      //StrCopy(PChar(Result), e);
    finally
      PQFreemem(e);
    end;
  end;
end;

function EncodeBytea(const vStr: string): string;
begin
  Result := EncodeBytea(PByte(vStr), ByteLength(vStr));
end;

function DecodeBytea(vStr: PByte; vLen: Cardinal): string; overload;
var
  e: PByte;
  aLen: Longword;
begin
  e := PQunescapeBytea(PByte(vStr), @aLen);
  try
    SetLength(Result, aLen);
    if aLen<>0 then
      Move(e^, Result[1], aLen);
  finally
    PQFreemem(e);
  end;
end;

function DecodeBytea(const vStr: string): string;
begin
  Result := DecodeBytea(PByte(vStr), ByteLength(vStr));
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
  s := 'Postgre connection failed' + #13 + PQerrorMessage(vHandle);
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
        s := PQresultErrorMessage(PGResult);
        raise EmncException.Create('Postgre lost connection with: ' + s);
      end
      else
      begin
        s := PQresultErrorMessage(PGResult);
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
  aTr: TmncSQLSession;
  aCMD: TmncSQLCommand;
begin
  Result := TStringList.Create;
  try
    aTr := CreateSession;
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
              c := lo_read(vSrc.Handle, fds, PAnsiChar(s), cBufferSize);
              if c > 0 then
                lo_write(Handle, fdd, PAnsiChar(s), c);
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
          c := lo_read(Handle, fds, PAnsiChar(s), cBufferSize);
          Inc(Result, c);
          if c>0 then
            vStream.Write(PAnsiChar(s)^, c);
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
    //Result := lo_export(Handle, vOID, PAnsiChar(AnsiString(vFileName)));
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
              lo_write(Handle, fdd, PAnsiChar(s), c);
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
      //Result := lo_import(Handle, PAnsiChar(AnsiString(vFileName)));
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
  aHost, aPort: AnsiString;
  aResource, aUser, aPassword: AnsiString;
  aSsl, aSslComp: AnsiString;
  aUrl: AnsiString;
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

  if UserName = '' then
    aUser := 'postgres'
  else
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

  //Result := PQsetdbLogin(PAnsiChar(aHost), PAnsiChar(aPort), nil, nil, PAnsiChar(aDB), PAnsiChar(aUser), PAnsiChar(aPass));
  //aUrl := Format('postgresql://%s:%s@%s:%s/%s?sslmode=%s&sslcompression=%s&application_name=%s', [aUser, aPass, aHost, aPort, aDB, aSsl, aSslComp, AppName]);
  if SimpleConnection then
    vHandle := PQsetdbLogin(PAnsiChar(aHost), PAnsiChar(aPort), nil, nil, PAnsiChar(aResource), PAnsiChar(aUser), PAnsiChar(aPassword))
  else
  begin
    aUrl := Format('user=%s password=''%s'' host=%s port=%s dbname=''%s'' sslmode=%s sslcompression=%s application_name=''%s''', [aUser, aPassword, aHost, aPort, aResource, aSsl, aSslComp, AppName]);
    //aUrl := Format('postgresql://%s:%s@%s:%s/%s?sslmode=%s&sslcompression=%s&application_name=%s', [aUser, aPass, aHost, aPort, aDB, aSsl, aSslComp, AppName]);
    vHandle := PQconnectdb(PAnsiChar(aUrl));
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

function TmncPGConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncPGSession.Create(Self);
end;

function TmncPGConnection.IsDatabaseExists(vName: string): Boolean;
var
  s: AnsiString;
  r: PPGresult;
  conn: PPGconn;
begin
  InternalConnect(conn, 'postgres');
  try
    s := Format('select 1 from pg_database where datname=''%s''', [LowerCase(vName)]);
    r := PQexec(conn, PAnsiChar(s));
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

procedure TmncPGConnection.Execute(vSQL: string);
var
  s: utf8string;
  r: PPGresult;
begin
  s := vSQL;
  r := PQexec(FHandle, PAnsiChar(s));
  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;
end;

{ TmncPGSession }

function TmncPGSession.InternalCreateCommand: TmncSQLCommand;
begin
  Result := TmncPGCommand.CreateBy(Self);
end;

procedure TmncPGSession.DoStart;
begin
	//TODO: Use session params to pass it here
  Execute('begin isolation level read committed;');
  //Execute('begin isolation level serializable;');
end;

procedure TmncPGSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
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

function TmncPGSession.NewToken: string;
begin
  ConnectionLock.Enter;
  try
    Inc(fTokenID);
    Result := 'minilib_' + IntToStr(fTokenID);
  finally
    ConnectionLock.Leave;
  end;
end;

procedure TmncPGSession.Execute(const vSQL: string);
begin
  Connection.Execute(vSQL);
end;

constructor TmncPGSession.Create(vConnection: TmncConnection);
begin
  inherited;
  FIsolated := True;
end;

destructor TmncPGSession.Destroy;
begin
  inherited;
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
  ClearStatement;
  FStatement := nil;
  FStatus := PGRES_EMPTY_QUERY;
  FTuple := 0;
  //Connection.Execute();
end;

function TmncPGCommand.IsSingleRowMode: Boolean;
begin
  Result := SingleRowMode and Assigned(PQsetSingleRowMode);
end;

function TmncPGCommand.GetDone: Boolean;
begin
  Result := (FStatement = nil) or FEOF;
end;

function TmncPGCommand.GetLastInsertID: Int64;
begin
  Result := 0;
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

    if IsSingleRowMode then
    begin
      PQsendQueryPrepared(Session.DBHandle, PAnsiChar(FHandle), Binds.Count, P, nil, nil, f);
      PQsetSingleRowMode(Session.DBHandle);
      //f := PQsetSingleRowMode(Session.DBHandle);
      //FStatement := PQgetResult(Session.DBHandle);
      FetchStatement;
    end
    else
    begin
      FStatement := PQexecPrepared(Session.DBHandle, PAnsiChar(FHandle), Binds.Count, P, nil, nil, f);
    end;

  finally
    FreeParamValues(Values);
  end;


  FStatus := PQresultStatus(FStatement);
  FBOF := True;
  if IsSingleRowMode then
    FEOF := (FStatement=nil) or not (FStatus in [PGRES_SINGLE_TUPLE])
  else
    FEOF := (FStatement=nil) or not (FStatus in [PGRES_TUPLES_OK]);

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
  if not FEOF then
  begin
    if FBOF then
    begin
      FetchFields(FStatement);
      FBOF := False;
      FTuple := 0;
    end
    else
      inc(FTuple);

    if IsSingleRowMode then
    begin
      FetchValues(FStatement, FTuple);
      PQclear(FStatement);
      FStatement := PQgetResult(Session.DBHandle);
      FEOF := (Statement = nil);
    end
    else
    begin
      FEOF := FTuple >= FTuples;
      if not FEOF then
        FetchValues(FStatement, FTuple);
    end;
  end;
end;

procedure TmncPGCommand.DoPrepare;
var
  c: PPGconn;
  r: PPGresult;
  s: UTF8String;
  i: Integer;
  z: Integer;
begin
  FBOF := True;
  FHandle := Session.NewToken;
  ParseSQL([psoAddParamsID]);
  c := Session.DBHandle;
  s := UTF8Encode(ProcessedSQL.SQL);

  r := PQprepare(c, PAnsiChar(FHandle), PAnsiChar(s), Params.Count, nil);
  try
    RaiseResultError(r);
  finally
    PQclear(r);
  end;

  {r := PQdescribePrepared(Session.DBHandle, PAnsiChar(FHandle)); //TODO: To indecate the size of param, no chance to get it
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
  //PQconsumeInput(Session.DBHandle);
  {while PQisBusy(Session.DBHandle)<>0 do
  begin
    Sleep(1);

    //if PQconsumeInput(Session.DBHandle)=0 then
      //Break;
  end;}

  FStatement := PQgetResult(Session.DBHandle);
  Result := True;

end;

procedure TmncPGCommand.ClearStatement;
begin
  PQclear(FStatement);
  //TPGClearThread.Create(FStatement); //tooo slow in ddl command  :(
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
      r := PQexec(Session.DBHandle, PAnsiChar(s));
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
  Result := StrToIntDef(PQcmdTuples(FStatement), 0);
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
      SetString(s, FData, FDataLen);
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

function TmncPostgreField.GetIsEmpty: Boolean;
begin
  Result := FIsEmpty;
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

function TmncPostgreFields.Find(const vName: string): TmncItem;
begin
  Result := inherited Find(vName);
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
  FBOF := True;
  FEOF := True;
  s := UTF8Encode(SQL.Text);
  r := PQexec(Session.DBHandle, PAnsiChar(s));
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
    FStatement := PQexec(Session.DBHandle, PAnsiChar(s));
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
  NextOnExecute := False;
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
  r := PQexec(FHandle, PAnsiChar('LISTEN "' + vChannel + '";'));
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
    if FEvent<>nil then
    begin
      Synchronize(PostEvent);
      PQFreemem(FEvent);
    end;
    Sleep(10); //belal: cpu usage ..... //zaher: wrong
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
      //StrMove(PAnsiChar(Result[i]), PAnsiChar(s), Length(s) + 1);
      Move(PAnsiChar(s)^, PAnsiChar(Result[i])^, Length(s) + 1);
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
    aName :=  DequoteStr(PQfname(vRes, i));
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

    function _BRead(vSrc: PAnsiChar; vCount: Longint): Integer;
    var
      t: PAnsiChar;
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

    function _DRead(vSrc: PAnsiChar; vCount: Longint): Int64;
    var
      t: PAnsiChar;
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
  p: PAnsiChar;
begin
  Result := PQgetisnull(vRes, vTuple, vIndex) = 0;
  if Result then
  begin
    p := PQgetvalue(vRes, vTuple, vIndex);
    {$ifdef FPC}
    Value := p;
    {$else}
    Value := UTF8ToString(p);
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
        f.FIsEmpty := f.FDataLen = 0;
        f.FIsNull := False;
      end
      else
      begin
        f.FIsNull := True;
        f.FIsEmpty := True;
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
  Result := Session.Connection as TmncPGConnection;
end;

function TmncCustomPGCommand.GetParams: TmncPostgreParams;
begin
  Result := inherited Params as TmncPostgreParams;
end;

function TmncCustomPGCommand.GetSession: TmncPGSession;
begin
  Result := inherited Session as TmncPGSession;
end;

procedure TmncCustomPGCommand.RaiseResultError(PGResult: PPGresult);
begin
  Connection.RaiseResultError(PGResult);
end;

procedure TmncCustomPGCommand.SetSession(const Value: TmncPGSession);
begin
  inherited Session := Value;
end;

function TmncCustomPGCommand.GetBinds: TmncPGBinds;
begin
  Result := inherited Binds as TmncPGBinds;
end;

{ TmncPGCursorCommand }

function TmncPGCursorCommand.CloseSQL: AnsiString;
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
    aStatement := PQexecPrepared(Session.DBHandle, PAnsiChar(FHandle), Binds.Count, P, nil, nil, 0);
    //FStatement := PQexec(Session.DBHandle, PChar(SQL.Text));
  finally
    FreeParamValues(Values);
  end;
  FStatus := PQresultStatus(aStatement);
  FBOF := True;
  FEOF := not (FStatus in [PGRES_COMMAND_OK]);
  try
    RaiseResultError(aStatement);
  except
    InternalClose;
    raise;
  end;
end;

procedure TmncPGCursorCommand.DoNext;
begin
  if FStatement<>nil then PQclear(FStatement);

  FStatement := PQexec(Session.DBHandle, PAnsiChar(FetchSQL));
  if FStatement<>nil then
  begin
    FStatus := PQresultStatus(FStatement);
    if (Status in [PGRES_TUPLES_OK]) then
    begin
      if FBOF then
      begin
        FetchFields(FStatement);
        FBOF := False;
      end;
      FetchValues(FStatement, 0);
      if TmncPostgreFields(Fields).IsNull then
        FEOF := True
      else
        FEOF := False;
    end
    else
      FEOF := True;
  end;
end;

procedure TmncPGCursorCommand.DoPrepare;
var
  s, b: ansistring;
  r: PPGresult;
  c: PPGconn;
  i: Integer;
begin
  FBOF := True;
  FHandle := Session.NewToken;
  ParseSQL([psoAddParamsID]);
  c := Session.DBHandle;

  if ResultFormat = mrfBinary then
    b := 'binary'
  else
    b := '';

  s := Format('declare %s %s cursor for %s', [Handle, b, ProcessedSQL.SQL]);
  r := PQprepare(c, PAnsiChar(FHandle), PAnsiChar(s), 0 , nil);

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

function TmncPGCursorCommand.FetchSQL: AnsiString;
begin
  Result := Format('fetch in %s', [Handle]);
end;

function TmncPGCursorCommand.GetActive: Boolean;
begin
  Result := not FBOF;
end;

function TmncPGCursorCommand.GetDone: Boolean;
begin
  Result := FEOF;
end;

procedure TmncPGCursorCommand.InternalClose;
begin
  PQexec(Session.DBHandle, PAnsiChar(CloseSQL));
  FStatus := PGRES_EMPTY_QUERY;
  FBOF := True;
end;

{ TmncPGColumn }

procedure TmncPGColumn.SetPGType(const Value: Integer);
begin
  FPGType := Value;
  case Value of
    Oid_Bool: FDataType := dtBoolean;
    Oid_oid, Oid_int2, Oid_int4, Oid_int8: FDataType := dtInteger;
    Oid_Money: FDataType := dtCurrency;
    Oid_Float4, Oid_Float8: FDataType := dtCurrency;
    Oid_Date: FDataType := dtDate;
    Oid_Time, Oid_TimeStamp: FDataType := dtDateTime;
    OID_NUMERIC: FDataType := dtCurrency;
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
begin
  PQclear(FStatement);
end;

initialization
  Engines.RegisterConnection(TmncPGConnection.EngineName, 'PostgreSQL Database', TmncPGConnection);
end.
