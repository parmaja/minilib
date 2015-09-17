unit mncFirebird;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Only FirebirdSQL 2.5 Dialect 3
}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants,
  mncConnections, mncSQL,
  mncFBTypes, mncFBHeader, mncFBErrors, mncFBUtils, mncFBClient, mncMetas,
  mncSQLDA;

const
  FB_DIALECT = 3;

type

  { TmncFBConnection }

  TmncFBConnection = class(TmncSQLConnection)
  private
    FHandle: TISC_DB_HANDLE;
    FCharacterSet: string;
    FRole: string;
    FDPB: PChar;
    FDPBLength: Short;
    function GetIsReadOnly: Boolean;
  protected
    function GetBaseLevel: Long;
    function GetDBSQLDialect: Integer;
    function GetLongDatabaseInfo(Command: Integer): Integer;

    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure DoInit; override;
  public
    constructor Create;
    class function Model: TmncConnectionModel; override;
    function CreateSession: TmncSQLSession; override;

    function GetVersion: string;
    procedure Execute(SQL: string); override;
    property Role: string read FRole write FRole;
    property CharacterSet: string read FCharacterSet write FCharacterSet;
    property Handle: TISC_DB_HANDLE read FHandle;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  { TmncFBSession }

  TmncFBSession = class(TmncSQLSession)
  private
    FHandle: TISC_TR_HANDLE;
    FTPB: PChar;
    FTPBLength: Short;
    function GetConnection: TmncFBConnection;
    procedure SetConnection(const AValue: TmncFBConnection);
  protected
    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    function CreateCommand: TmncSQLCommand; override;
    function CreateMeta: TmncMeta; override;
    procedure Execute(SQL: string);
    property Handle: TISC_TR_HANDLE read FHandle;
    property TPB: PChar read FTPB;
    property TPBLength: Short read FTPBLength;
    property Connection: TmncFBConnection read GetConnection write SetConnection;
  end;

  TmncFBTransaction = TmncFBSession;

  { TmncFBField }

  TmncFBField = class(TmncField)
  private
    FSQLVAR: TmncSQLVAR;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;

    function GetAsText: string; override;
    procedure SetAsText(const AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(const AValue: Integer); override;
    function GetAsInt64: Int64; override;
    procedure SetAsInt64(const AValue: Int64); override;
    function GetAsDouble: Double; override;
    procedure SetAsDouble(const AValue: Double); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    function GetAsCurrency: Currency; override;
    procedure SetAsCurrency(const AValue: Currency); override;
    function GetAsDate: TDateTime; override;
    procedure SetAsDate(const AValue: TDateTime); override;
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(const AValue: TDateTime); override;
    function GetAsTime: TDateTime; override;
    procedure SetAsTime(const AValue: TDateTime); override;

    function GetIsNull: Boolean; override;
    procedure SetIsNull(const AValue: Boolean); override;
    function GetIsEmpty: Boolean; override;
  public
    constructor Create(vColumn: TmncColumn); override;
    destructor Destroy; override;
    property SQLVAR: TmncSQLVAR read FSQLVAR write FSQLVAR;
  end;

  { TFBSQLParam }

  TmncFBParam = class(TmncParam)
  private
    FSQLVAR: TmncSQLVAR;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;

    function GetAsText: string; override;
    procedure SetAsText(const AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(const AValue: Integer); override;
    function GetAsInt64: Int64; override;
    procedure SetAsInt64(const AValue: Int64); override;
    function GetAsDouble: Double; override;
    procedure SetAsDouble(const AValue: Double); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    function GetAsCurrency: Currency; override;
    procedure SetAsCurrency(const AValue: Currency); override;
    function GetAsDate: TDateTime; override;
    procedure SetAsDate(const AValue: TDateTime); override;
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(const AValue: TDateTime); override;
    function GetAsTime: TDateTime; override;
    procedure SetAsTime(const AValue: TDateTime); override;

    function GetIsNull: Boolean; override;
    procedure SetIsNull(const AValue: Boolean); override;
    function GetIsEmpty: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property SQLVAR: TmncSQLVAR read FSQLVAR write FSQLVAR;
  end;

  { TmncFBFields }

  TmncFBFields = class(TmncFields)
  private
    FSQLDA: PXSQLDA;
    function GetItem(Index: Integer): TmncFBField;
  protected
    function GetModified: Boolean;
    function CreateField(vColumn: TmncColumn): TmncField; override;
    procedure Detach; override;
  public
    constructor Create(vColumns: TmncColumns); override;
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TmncFBField read GetItem;
    property SQLDA: PXSQLDA read FSQLDA;
  end;

  { TmncFBParams }

  TmncFBParams = class(TmncParams)
  private
    FSQLDA: PXSQLDA;
    function GetItem(Index: Integer): TmncFBParam;
  protected
    function GetModified: Boolean;
    function CreateParam: TmncParam; override;
    procedure Detach; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TmncFBParam read GetItem;
    property SQLDA: PXSQLDA read FSQLDA;
  end;

  { TmncFBCommand }

  TmncFBCommand = class(TmncSQLCommand)
  private
    FHandle: TISC_STMT_HANDLE;
    FActive: Boolean;
    FBOF: Boolean;
    FEOF: Boolean;
    FCursor: string;
    FSQLType: TFBDSQLTypes;
    function GetConnection: TmncFBConnection;
    function GetSession: TmncFBSession;
    procedure SetCursor(AValue: string);
    procedure SetSession(const AValue: TmncFBSession);
    procedure FreeHandle;
    //Very dangrouse functions, be sure not free it with SQLVAR sqldata and sqlind, becuase it is shared with params sqldata
    procedure AllocateBinds(var XSQLDA: PXSQLDA);
    procedure DeallocateBinds(var XSQLDA: PXSQLDA);
  protected
    function Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure CheckHandle;//TODO remove it
    procedure DoParse; override;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    procedure DoUnprepare; override;
    function GetEOF: Boolean; override;
    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    //function GetPlan: string;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    property Connection: TmncFBConnection read GetConnection;
    property Session: TmncFBSession read GetSession write SetSession;
  public
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    property SQLType: TFBDSQLTypes read FSQLType;
    property Handle: TISC_STMT_HANDLE read FHandle;
    { Cursor name
      Optional you can use it

      UPDATE ... WHERE CURRENT OF MyCursor;
      DELETE FROM ... WHERE CURRENT OF MyCursor;

      http://tech.groups.yahoo.com/group/firebird-support/messages/65692?threaded=1&m=e&var=1&tidx=1
    }
    property Cursor: string read FCursor write SetCursor;
  end;

implementation

uses
  mncFBMetas, mncDB;

{ TmncFBConnection }

constructor TmncFBConnection.Create;
begin
  inherited Create;
end;

class function TmncFBConnection.Model: TmncConnectionModel;
begin
  Result.Name := 'FirebirdSQL';
  Result.Title := 'Firebird SQL Database';
  Result.Capabilities := [ccDB, ccSQL, ccStrict, ccTransaction, ccMultiTransaction, ccNetwork];
end;

function TmncFBConnection.CreateSession: TmncSQLSession;
begin
  Result := TmncFBSession.Create(Self);
end;

function TmncFBConnection.GetVersion: string;
var
  local_buffer: array[0..FBBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  StatusVector:TStatusVector;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        FBBigLocalBufferLength, local_buffer), StatusVector, True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  Result := String(PChar(@local_buffer[5]));
end;

function TmncFBConnection.GetLongDatabaseInfo(Command: Integer): Integer;
var
  l: Integer;
  c: Char;
  StatusVector: TStatusVector;
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
begin
  c := Char(Command);
  Call(FBClient.isc_database_info(@StatusVector, @Handle, 1, @c, FBLocalBufferLength, local_buffer), StatusVector, True);
  l := FBClient.isc_vax_integer(@local_buffer[1], 2);
  Result := FBClient.isc_vax_integer(@local_buffer[3], l);
end;

function TmncFBConnection.GetIsReadOnly: Boolean;
begin
  Result := GetLongDatabaseInfo(isc_info_db_read_only) <> 0;
end;

function TmncFBConnection.GetBaseLevel: Long;
var
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  StatusVector: TStatusVector;
begin
  //CheckInactive;
  DatabaseInfoCommand := Char(isc_info_base_level);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
    FBLocalBufferLength, local_buffer), StatusVector, True);
  Result := FBClient.isc_vax_integer(@local_buffer[4], 1);
end;

function TmncFBConnection.GetDBSQLDialect: Integer;
var
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
  StatusVector: TStatusVector;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
    FBLocalBufferLength, local_buffer), StatusVector, True);
  if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
    Result := 1
  else
  begin
    length := FBClient.isc_vax_integer(@local_buffer[1], 2);
    Result := FBClient.isc_vax_integer(@local_buffer[3], length);
  end;
end;

function TmncFBConnection.Call(ErrCode: ISC_STATUS;  const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if RaiseError and (CheckStatusVector(StatusVector, [isc_lost_db_connection]) or
    CheckStatusVector(StatusVector, [isc_net_read_err]) or
    CheckStatusVector(StatusVector, [isc_net_write_err])) then
  begin
    try
      FBRaiseError(StatusVector);
    finally
      Close;
    end;
  end;
  if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBConnection.DoConnect;
var
  DPB: string;
  //i: Integer;
  aDatabaseName: AnsiString;
  aParams: TStringList;
  StatusVector: TStatusVector;
begin
  aParams := TStringList.Create;
  try
    aParams.Assign(Params);
    FBDatabaseInfo(UserName, Password, Role, CharacterSet, aParams);
    GenerateDPB(aParams, DPB, FDPBLength);
  finally
    aParams.Free;
  end;
  FBAlloc(FDPB, 0, FDPBLength);
  Move(DPB[1], FDPB[0], FDPBLength);
  aDatabaseName := FBComposeConnectionString(Resource, Host, Port, FBClient.IsEmbed);
  if Call(FBClient.isc_attach_database(@StatusVector, Length(aDatabaseName),
    PChar(aDatabaseName), @FHandle, FDPBLength, FDPB), StatusVector, False) > 0 then
  begin
    FHandle := nil;
    FBRaiseError(StatusVector);
  end;
  if (GetDBSQLDialect < FB_DIALECT) then
    raise EFBError.Create(-1, 'This database not dialect 3, other dialects not supported')

{    for i := 0 to FEventNotifiers.Count - 1 do
      if IFBEventNotifier(FEventNotifiers[i]).GetAutoRegister then
        IFBEventNotifier(FEventNotifiers[i]).RegisterEvents;}
end;

function TmncFBConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TmncFBConnection.DoDisconnect;
var
//  i: Integer;
  StatusVector: TStatusVector;
begin
{  for i := 0 to FEventNotifiers.Count - 1 do
    IFBEventNotifier(FEventNotifiers[i]).UnRegisterEvents;}
  if (Call(FBClient.isc_detach_database(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector)
  else
    FHandle := nil;
end;

{ TmncFBSession }

destructor TmncFBSession.Destroy;
begin
  FreeMem(FTPB);
  FTPB := nil;
  inherited;
end;

function TmncFBSession.CreateCommand: TmncSQLCommand;
begin
  Result := TmncFBCommand.CreateBy(Self);
end;

function TmncFBSession.CreateMeta: TmncMeta;
begin
  Result := TmncFBMeta.CreateBy(Self)
end;

procedure TmncFBSession.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncFBSession.DoStart;
var
  pteb: PISC_TEB_ARRAY;
  TPB: string;
  StatusVector: TStatusVector;
begin
  if ParamsChanged then
  begin
    ParamsChanged := False;
    GenerateTPB(Params, TPB, FTPBLength);
    if FTPBLength > 0 then
    begin
      FBAlloc(FTPB, 0, FTPBLength);
      Move(TPB[1], FTPB[0], FTPBLength);
    end;
  end;

  pteb := nil;
  FBAlloc(pteb, 0, SizeOf(TISC_TEB));
  try
    pteb^[0].db_handle := @(Connection.Handle);
    pteb^[0].tpb_length := FTPBLength;
    pteb^[0].tpb_address := FTPB;
    if Call(FBClient.isc_start_multiple(@StatusVector, @FHandle, 1, PISC_TEB(pteb)), StatusVector, False) > 0 then
    begin
      FHandle := nil;
      FBRaiseError(StatusVector);
    end;
  finally
    FreeMem(pteb);
  end;
end;

procedure TmncFBSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
var
  StatusVector: TStatusVector;
begin
  case How of
    sdaCommit:
    begin
      if not Retaining then
        Call(FBClient.isc_commit_transaction(@StatusVector, @FHandle), StatusVector, False)
      else
        Call(FBClient.isc_commit_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
    sdaRollback:
    begin
      if not Retaining then
        Call(FBClient.isc_rollback_transaction(@StatusVector, @FHandle), StatusVector, True)
      else
        Call(FBClient.isc_rollback_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
  end;
end;

procedure TmncFBConnection.Execute(SQL: string);
var
  tr_handle: TISC_TR_HANDLE;
  StatusVector: TStatusVector;
begin
  tr_handle := nil;
  try
    Call(FBClient.isc_dsql_execute_immediate(@StatusVector, @FHandle, @tr_handle, 0, PChar(SQL), FB_DIALECT, nil), StatusVector, True);
  finally
  end;
end;

function TmncFBSession.GetActive: Boolean;
begin
  Result:= FHandle <> nil;
end;

constructor TmncFBSession.Create(vConnection: TmncConnection);
begin
  inherited;
  FHandle := nil;
  FTPB := nil;
  FTPBLength := 0;
end;

function TmncFBSession.GetConnection: TmncFBConnection;
begin
  Result := inherited Connection as TmncFBConnection;
end;

procedure TmncFBConnection.DoInit;
begin
end;

procedure TmncFBSession.SetConnection(const AValue: TmncFBConnection);
begin
  inherited Connection := AValue;
end;

function TmncFBSession.Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if CheckStatusVector(StatusVector, [isc_lost_db_connection]) then
    Connection.Call(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (Result > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBSession.DoInit;
begin
end;

{ TmncFBParam }

constructor TmncFBParam.Create;
begin
  inherited;
  FSQLVAR := TmncSQLVAR.Create;
end;

destructor TmncFBParam.Destroy;
begin
  FreeAndNil(FSQLVAR);
  inherited;
end;

{ TmncFBField }

function TmncFBField.GetValue: Variant;
begin
  Result := FSQLVAR.AsVariant;
end;

procedure TmncFBField.SetValue(const AValue: Variant);
begin
  FSQLVAR.AsVariant := AValue;
end;

function TmncFBField.GetAsText: string;
begin
  Result := FSQLVAR.AsText;
end;

procedure TmncFBField.SetAsText(const AValue: string);
begin
  FSQLVAR.AsText := AValue;
end;

function TmncFBField.GetAsString: string;
begin
  Result := FSQLVAR.AsString;
end;

procedure TmncFBField.SetAsString(const AValue: string);
begin
  FSQLVAR.AsString := AValue;
end;

function TmncFBField.GetAsInteger: Integer;
begin
  Result := FSQLVAR.AsInteger;
end;

procedure TmncFBField.SetAsInteger(const AValue: Integer);
begin
  FSQLVAR.AsInteger := AValue;
end;

function TmncFBField.GetAsInt64: Int64;
begin
  Result := FSQLVAR.AsInt64
end;

procedure TmncFBField.SetAsInt64(const AValue: Int64);
begin
  FSQLVAR.AsInt64 := AValue;
end;

function TmncFBField.GetAsDouble: Double;
begin
  Result := FSQLVAR.AsDouble;
end;

procedure TmncFBField.SetAsDouble(const AValue: Double);
begin
  FSQLVAR.AsDouble := AValue;
end;

function TmncFBField.GetAsBoolean: Boolean;
begin
  Result := FSQLVAR.AsBoolean;
end;

procedure TmncFBField.SetAsBoolean(const AValue: Boolean);
begin
  FSQLVAR.AsBoolean := AValue;
end;

function TmncFBField.GetAsCurrency: Currency;
begin
  Result := FSQLVAR.AsCurrency;
end;

procedure TmncFBField.SetAsCurrency(const AValue: Currency);
begin
  FSQLVAR.AsCurrency := AValue;
end;

function TmncFBField.GetAsDate: TDateTime;
begin
  Result := FSQLVAR.AsDate;
end;

procedure TmncFBField.SetAsDate(const AValue: TDateTime);
begin
  FSQLVAR.AsDate := AValue;
end;

function TmncFBField.GetAsDateTime: TDateTime;
begin
  Result := FSQLVAR.AsDateTime;
end;

procedure TmncFBField.SetAsDateTime(const AValue: TDateTime);
begin
  FSQLVAR.AsDateTime := AValue;
end;

function TmncFBField.GetAsTime: TDateTime;
begin
  Result := FSQLVAR.AsTime;
end;

procedure TmncFBField.SetAsTime(const AValue: TDateTime);
begin
  FSQLVAR.AsTime := AValue;
end;

function TmncFBField.GetIsNull: Boolean;
begin
  Result := FSQLVAR.IsNull;
end;

procedure TmncFBField.SetIsNull(const AValue: Boolean);
begin
  FSQLVAR.IsNull := AValue;
end;

function TmncFBField.GetIsEmpty: Boolean;
begin
  Result := FSQLVAR.IsNull;
end;

constructor TmncFBField.Create(vColumn: TmncColumn);
begin
  inherited;
  FSQLVAR := TmncSQLVAR.Create;
end;

destructor TmncFBField.Destroy;
begin
  FreeAndNil(FSQLVAR);
  inherited;
end;

{ TmncFBParams }

function TmncFBParams.GetItem(Index: Integer): TmncFBParam;
begin
  Result := (inherited Items[Index]) as TmncFBParam;
end;

function TmncFBParams.GetModified: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Items[i].SQLVAR.Modified then
    begin
      Result := True;
      break;
    end;
end;

function TmncFBParams.CreateParam: TmncParam;
begin
  Result := TmncFBParam.Create;
end;

procedure TmncFBParams.Detach;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count -1 do
  begin
    Items[i].SQLVAR.Detach;
  end;
end;

constructor TmncFBParams.Create;
begin
  inherited;
  InitSQLDA(FSQLDA, 0);
end;

destructor TmncFBParams.Destroy;
begin
  Clear;
  //FreeSQLDA(FSQLDA); already did in Clear;
  inherited;
end;

procedure TmncFBParams.Clear;
begin
  inherited Clear;
  InitSQLDA(FSQLDA, 0);
end;

{ TmncFBParam }

function TmncFBParam.GetValue: Variant;
begin
  Result := FSQLVAR.AsVariant;
end;

procedure TmncFBParam.SetValue(const AValue: Variant);
begin
  FSQLVAR.AsVariant := AValue;
end;

function TmncFBParam.GetAsText: string;
begin
  Result := FSQLVAR.AsText;
end;

procedure TmncFBParam.SetAsText(const AValue: string);
begin
  FSQLVAR.AsText := AValue;
end;

function TmncFBParam.GetAsString: string;
begin
  Result := FSQLVAR.AsString;
end;

procedure TmncFBParam.SetAsString(const AValue: string);
begin
  FSQLVAR.AsString := AValue;
end;

function TmncFBParam.GetAsInteger: Integer;
begin
  Result := FSQLVAR.AsInteger;
end;

procedure TmncFBParam.SetAsInteger(const AValue: Integer);
begin
  FSQLVAR.AsInteger := AValue;
end;

function TmncFBParam.GetAsInt64: Int64;
begin
  Result := FSQLVAR.AsInt64
end;

procedure TmncFBParam.SetAsInt64(const AValue: Int64);
begin
  FSQLVAR.AsInt64 := AValue;
end;

function TmncFBParam.GetAsDouble: Double;
begin
  Result := FSQLVAR.AsDouble;
end;

procedure TmncFBParam.SetAsDouble(const AValue: Double);
begin
  FSQLVAR.AsDouble := AValue;
end;

function TmncFBParam.GetAsBoolean: Boolean;
begin
  Result := FSQLVAR.AsBoolean;
end;

procedure TmncFBParam.SetAsBoolean(const AValue: Boolean);
begin
  FSQLVAR.AsBoolean := AValue;
end;

function TmncFBParam.GetAsCurrency: Currency;
begin
  Result := FSQLVAR.AsCurrency;
end;

procedure TmncFBParam.SetAsCurrency(const AValue: Currency);
begin
  FSQLVAR.AsCurrency := AValue;
end;

function TmncFBParam.GetAsDate: TDateTime;
begin
  Result := FSQLVAR.AsDate;
end;

procedure TmncFBParam.SetAsDate(const AValue: TDateTime);
begin
  FSQLVAR.AsDate := AValue;
end;

function TmncFBParam.GetAsDateTime: TDateTime;
begin
  Result := FSQLVAR.AsDateTime;
end;

procedure TmncFBParam.SetAsDateTime(const AValue: TDateTime);
begin
  FSQLVAR.AsDateTime := AValue;
end;

function TmncFBParam.GetAsTime: TDateTime;
begin
  Result := FSQLVAR.AsTime;
end;

procedure TmncFBParam.SetAsTime(const AValue: TDateTime);
begin
  FSQLVAR.AsTime := AValue;
end;

function TmncFBParam.GetIsNull: Boolean;
begin
  Result := FSQLVAR.IsNull;
end;

procedure TmncFBParam.SetIsNull(const AValue: Boolean);
begin
  FSQLVAR.IsNull := AValue;
end;

function TmncFBParam.GetIsEmpty: Boolean;
begin
  Result := FSQLVAR.IsNull;
end;

{ TmncFBFields }

function TmncFBFields.GetItem(Index: Integer): TmncFBField;
begin
  Result := (inherited Items[Index]) as TmncFBField;
end;

function TmncFBFields.GetModified: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Items[i].SQLVAR.Modified then
    begin
      Result := True;
      break;
    end;
end;

function TmncFBFields.CreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncFBField.Create(vColumn);
end;

procedure TmncFBFields.Detach;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count -1 do
  begin
    Items[i].SQLVAR.Detach;
  end;
end;

constructor TmncFBFields.Create(vColumns: TmncColumns);
begin
  inherited;
  InitSQLDA(FSQLDA, 0);
end;

destructor TmncFBFields.Destroy;
begin
  Clear;
  //FreeSQLDA(FSQLDA); already did in Clear;
  inherited;
end;

procedure TmncFBFields.Clear;
begin
  inherited Clear;
  InitSQLDA(FSQLDA, 0);
end;

{ TmncFBCommand }

function TmncFBCommand.GetSession: TmncFBSession;
begin
  Result := inherited Session as TmncFBSession;
end;

procedure TmncFBCommand.SetCursor(AValue: string);
begin
  if FCursor =AValue then Exit;
  CheckInactive;
  FCursor :=AValue;
end;

procedure TmncFBCommand.SetSession(const AValue: TmncFBSession);
begin
  inherited Session := AValue;
end;

procedure TmncFBCommand.FreeHandle;
var
  isc_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  try
    if (FHandle <> nil) then
    begin
      //if Connection.Connected then
      begin
        isc_res := Call(FBClient.isc_dsql_free_statement(@StatusVector, @FHandle, DSQL_drop), StatusVector, False);
        if (StatusVector[0] = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) and (isc_res <> isc_lost_db_connection) then
          FBRaiseError(StatusVector);
      end;
    end;
  finally
    FHandle := nil;
  end;
end;

procedure TmncFBCommand.Clear;
begin
  inherited;
end;

function TmncFBCommand.GetEOF: Boolean;
begin
  Result := FEOF or not FActive;
end;

function TmncFBCommand.GetRowsChanged: Integer;
var
  result_buffer: array[0..1048] of Char;
  info_request: Char;
  StatusVector: TStatusVector;
begin
  if not Prepared then
    Result := -1
  else
  begin
    info_request := Char(isc_info_sql_records);
    if FBClient.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @info_request,
      SizeOf(result_buffer), result_buffer) > 0 then
      FBRaiseError(StatusVector);
    if (result_buffer[0] <> Char(isc_info_sql_records)) then
      Result := -1
    else
      case SQLType of
        SQLSelect: Result := GetInfoReqRecord(result_buffer, isc_info_req_select_count);
        SQLUpdate: Result := GetInfoReqRecord(result_buffer, isc_info_req_update_count);
        SQLDelete: Result := GetInfoReqRecord(result_buffer, isc_info_req_delete_count);
        SQLInsert: Result := GetInfoReqRecord(result_buffer, isc_info_req_insert_count);
      else
        Result := -1;
      end;
  end;
end;

procedure TmncFBCommand.DoExecute;
var
  StatusVector: TStatusVector;
  BindsData: PXSQLDA;
begin
  CheckHandle;
  BindsData := nil;
  AllocateBinds(BindsData);
  try
    case FSQLType of
      SQLSelect:
        begin
          Call(FBClient.isc_dsql_execute2(@StatusVector,  @Session.Handle, @FHandle,
            FB_DIALECT, BindsData, nil), StatusVector, True);

          if FCursor <> '' then
            Call(FBClient.isc_dsql_set_cursor_name(@StatusVector, @FHandle, PAnsiChar(FCursor), 0), StatusVector, True);
          FActive := True;
          FBOF := True;
          FEOF := False;
        end;
      SQLExecProcedure:
        begin
          Call(FBClient.isc_dsql_execute2(@StatusVector,
            @Session.Handle, @FHandle, FB_DIALECT,
            BindsData, (Params as TmncFBParams).SQLDA), StatusVector, True);
          FBOF := False;
          FEOF := False;
        end
    else
      Call(FBClient.isc_dsql_execute(@StatusVector,
        @Session.Handle, @FHandle, FB_DIALECT,
        BindsData), StatusVector, True);
      FBOF := False;
      FEOF := False;
    end;
  finally
    DeallocateBinds(BindsData);
  end;
end;

procedure TmncFBCommand.DoNext;
var
  fetch_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  if not EOF then
  begin
    fetch_res := Call(FBClient.isc_dsql_fetch(@StatusVector, @FHandle, FB_DIALECT, (Fields as TmncFBFields).FSQLDA), StatusVector, False);
    if (fetch_res = 100) or (CheckStatusVector(StatusVector, [isc_dsql_cursor_err])) then
    begin
      FEOF := True;
      Fields.Clean;
    end
    else if (fetch_res > 0) then
      FBRaiseError(StatusVector)
    else
      FBOF := False;
  end;
end;

procedure TmncFBCommand.DoRollback;
begin
  Session.Rollback;
end;

function TmncFBCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncFBFields.Create(vColumns);
end;

function TmncFBCommand.CreateParams: TmncParams;
begin
  Result := TmncFBParams.Create;
end;

procedure TmncFBCommand.DoClose;
var
  isc_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  try
    if (FHandle <> nil) and (SQLType in [SQLSelect, SQLSelectForUpdate, SQLExecProcedure]) {and FActive //zaher} then
    begin
      isc_res := Call(FBClient.isc_dsql_free_statement(@StatusVector, @FHandle, DSQL_close), StatusVector, False);
      if (StatusVector[0] = 1) and (isc_res > 0) and
        not CheckStatusVector(StatusVector, [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        if isc_res = isc_lost_db_connection then
          Call(isc_res, StatusVector, True)
        else
          FBRaiseError(StatusVector);
    end;
  finally
    FEOF := False;
    FBOF := False;
    FActive := False;
  end;
end;

procedure TmncFBCommand.DoCommit;
begin
  Session.Commit;
end;

function TmncFBCommand.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TmncFBCommand.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if not Value then
      Close
    else
      Execute;
  end;
end;

function TmncFBCommand.GetConnection: TmncFBConnection;
begin
  Result := Session.Connection as TmncFBConnection;
end;

procedure TmncFBCommand.CheckHandle;
begin
  //CheckTransaction;
  if (FHandle = nil) then
    FBRaiseError(fbceInvalidStatementHandle, [nil]);
end;

procedure TmncFBCommand.AllocateBinds(var XSQLDA: PXSQLDA);
var
  i: Integer;
  p: PXSQLVAR;
  x: PXSQLDA;
begin
  XSQLDA := nil;
  if (Binds.Count > 0) then
  begin
    x := (Params as TmncFBParams).SQLDA;

    InitSQLDA(XSQLDA, Binds.Count);
    move(x^, XSQLDA^, sizeof(TXSQLDA) - Sizeof(TXSQLVAR)); //minus first value because it is included with the size of TXSQLDA
    p := @XSQLDA^.sqlvar[0];
    for i :=0 to Binds.Count -1 do
    begin
      move((Binds[i].Param as TmncFBParam).FSQLVAR.XSQLVar^, p^, sizeof(TXSQLVAR));
      p := Pointer(PAnsiChar(p) + XSQLVar_Size);
    end;
  end;
end;

procedure TmncFBCommand.DeallocateBinds(var XSQLDA: PXSQLDA);
begin
  FreeSQLDA(XSQLDA, False);
end;

procedure TmncFBCommand.DoUnprepare;
begin
  inherited;
  FreeHandle;
end;

procedure TmncFBCommand.DoParse;
begin
  inherited DoParse;
end;

function TmncFBCommand.Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := 0;
  if (ErrCode > 0) and (Session <> nil) then
    Result := Session.Call(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBCommand.DoPrepare;
var
  res_buffer: array[0..7] of Char;
  type_item: Char;
  sql_type:Integer;
  StatusVector: TStatusVector;
  aData: PXSQLDA;
  p: PXSQLVAR;
  c: Integer;
  i: Integer;
  function Fields: TmncFBFields;
  begin
    Result := (Self.Fields as TmncFBFields);
  end;

  function Params: TmncFBParams;
  begin
    Result := (Self.Params as TmncFBParams);
  end;
var
  aColumn: TmncColumn;
  aField: TmncFBField;
  aParam: TmncFBParam;
begin
  FEOF := True;
  FBOF := False;
  if not Prepared then//TODO remove this line
  begin
    try
      Call(FBClient.isc_dsql_alloc_statement2(@StatusVector, @Connection.Handle, @FHandle), StatusVector, True);
      Call(FBClient.isc_dsql_prepare(@StatusVector, @Session.Handle, @FHandle, 0, PAnsiChar(SQLProcessed.SQL), FB_DIALECT, nil), StatusVector, True);
      { type of the statement }
      type_item := Char(isc_info_sql_stmt_type);
      Call(FBClient.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @type_item, SizeOf(res_buffer), res_buffer), StatusVector, True);
      if not GetInfoReqInteger(res_buffer, isc_info_sql_stmt_type, sql_type) then
        FBRaiseError(fbceUnknownError, [nil]);
      FSQLType := TFBDSQLTypes(sql_type);

      case FSQLType of
        SQLGetSegment, SQLPutSegment, SQLStartTransaction:
          begin
            FreeHandle;
            FBRaiseError(fbceNotPermitted, [nil]);
          end;
        SQLCommit, SQLRollback,
          SQLDDL, SQLSetSequence,
          SQLSelect, SQLInsert, SQLUpdate, SQLDelete, SQLSelectForUpdate,
          SQLExecProcedure:
          begin
            //Params is already created and have the items
            InitSQLDA(Params.FSQLDA, Params.Count);

            Call(FBClient.isc_dsql_describe_bind(@StatusVector, @FHandle, FB_DIALECT, Params.FSQLDA), StatusVector, True);

            p := @Params.FSQLDA^.sqlvar[0];
            for i := 0 to Params.Count - 1 do
            begin
              aParam := (Params.Items[i] as TmncFBParam);

              aParam.SQLVAR.XSQLVar := p;
              aParam.SQLVAR.Attach(@Connection.Handle, @Session.Handle);
              aParam.SQLVAR.Prepare;

              p := Pointer(PAnsiChar(p) + XSQLVar_Size);
            end;

            //if (FSQLType in [SQLSelect, SQLSelectForUpdate, SQLExecProcedure]) then
            begin
              //Check if there is a result data
              aData := nil;
              InitSQLDA(aData, 0);
              try
                Call(FBClient.isc_dsql_describe(@StatusVector, @FHandle, FB_DIALECT, aData), StatusVector, True);
                c := aData^.sqld;
              finally
                FreeSQLDA(aData);
              end;

              if c = 0 then
              begin
                Columns.Clear;
                if Fields <> nil then
                  Fields.Clear;
              end
              else
              begin
                //Now we load a columns for it
                if Self.Fields = nil then
                  Self.Fields := CreateFields(Columns) //need to create Fields because it have SQLDA buffer
                else
                  Fields.Clear;

                InitSQLDA(Fields.FSQLDA, c);

                Call(FBClient.isc_dsql_describe(@StatusVector, @FHandle, FB_DIALECT, Fields.SQLDA), StatusVector, True);
                p := @Fields.SQLDA^.sqlvar[0];
                for i := 0 to Fields.SQLDA^.sqld - 1 do
                begin
                  aColumn := Columns.Add(p^.aliasname, SQLTypeToDataType(p^.sqltype));
                  //aColumn.Name := FBDequoteName(aColumn.Name);
                  aField := Fields.Add(aColumn) as TmncFBField;
                  aField.SQLVAR.XSQLVar := p;
                  aField.SQLVAR.Attach(@Connection.Handle, @Session.Handle);
                  aField.SQLVAR.Prepare;

                  p := Pointer(PAnsiChar(p) + XSQLVar_Size);
                end;
              end;
            end;
          end;
       end;
    except
      on E: Exception do
      begin
        if (FHandle <> nil) then
          FreeHandle;
        raise;
      end;
    end;
  end;
end;

initialization
  mncDB.Engines.Add(TmncFBConnection);
end.
