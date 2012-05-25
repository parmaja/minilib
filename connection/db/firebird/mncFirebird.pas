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
  mncFBTypes, mncFBHeader, mncFBErrors, mncFBUtils, mncFBClient, mncFBStrings,
  mncSQLDA;

const
  FB_DIALECT = 3;

type

  { TmncFBConnection }

  TmncFBConnection = class(TmncConnection)
  private
    FHandle: TISC_DB_HANDLE;
    FDialect: Integer;
    FCachedPasswords: Boolean;
    FCharacterSet: string;
    FProtocol: TFBProtocol;
    FRole: string;
    FDPB: PChar;
    FDPBLength: Short;
    function GetIsReadOnly: Boolean;
    procedure SetProtocol(const AValue: TFBProtocol);
  protected
    function GetBaseLevel: Long;
    function GetDBSQLDialect: Integer;
    function GetLongDatabaseInfo(Command: Integer): Integer;

    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    class function GetMode: TmncSessionMode; override;
    procedure DoInit; override;
  public
    constructor Create;
    function GetVersion: string;
    procedure Execute(SQL: string);
    property CachedPasswords:Boolean read FCachedPasswords write FCachedPasswords default False;
    property Role: string read FRole write FRole;
    property CharacterSet: string read FCharacterSet write FCharacterSet;
    property Protocol: TFBProtocol read FProtocol write SetProtocol default dpTCP;
    property Handle: TISC_DB_HANDLE read FHandle;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  { TmncFBSession }

  TmncFBSession = class(TmncSession)
  private
    FHandle: TISC_TR_HANDLE;
    FStreamedActive: Boolean;
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
    procedure Execute(SQL: string);
    function GetRowsChanged: Integer;
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
    FData: PXSQLDA;
    function GetItem(Index: Integer): TmncFBField;
  protected
    function GetModified: Boolean;
    function GetNames: string;
    procedure SetData(FData: PXSQLDA);
  public
    procedure Prepare(NewCount: integer);
    constructor Create(vColumns: TmncColumns); override;
    destructor Destroy; override;
    property Items[Index: Integer]: TmncFBField read GetItem;
    property Data: PXSQLDA read FData;
  end;

  { TmncFBParams }

  TmncFBParams = class(TmncParams)
  private
    FData: PXSQLDA;
    function GetItem(Index: Integer): TmncFBParam;
  protected
    function GetModified: Boolean;
    function GetNames: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Items[Index: Integer]: TmncFBParam read GetItem;
    property Data: PXSQLDA read FData;
  end;

  { TmncFBCommand }

  TmncFBCommand = class(TmncSQLCommand)
  private
    FHandle: TISC_STMT_HANDLE;
    FActive: Boolean;
    FBOF: Boolean;
    FEOF: Boolean;
    FCursor: string; { Cursor name }
    FSQLType: TFBDSQLTypes;
    FGenerateParamNames: Boolean;
    FRecordCount: Integer;
    FParsedSQL: string;
    function GetConnection: TmncFBConnection;
    function GetSession: TmncFBSession;
    procedure SetSession(const AValue: TmncFBSession);
    procedure FreeHandle;
    procedure InternalPrepare;
  protected
    function Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure ValidStatement;
    procedure DoUnprepare; override;
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
    property Connection: TmncFBConnection read GetConnection;
    property Session: TmncFBSession read GetSession write SetSession;
  public
    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    property SQLType: TFBDSQLTypes read FSQLType;
    property Handle: TISC_STMT_HANDLE read FHandle;
  end;

implementation

procedure ChangeFields(Fields: TmncCustomFields; NewCount:short); forward;

procedure InitSQLDA(var Data: PXSQLDA; New: Integer);
var
  old: Integer;
begin
  if Data = nil then
    old := 0
  else
    old := Data^.sqln;
  FBAlloc(Data, XSQLDA_LENGTH(old), XSQLDA_LENGTH(new));
  Data^.version := SQLDA_VERSION1;
  Data^.sqln := New;
end;

procedure FreeSQLDA(var Data: PXSQLDA);
begin
  if Data <> nil then
    FreeMem(Data);
  Data := nil;
end;

{ TmncFBConnection }

constructor TmncFBConnection.Create;
begin
  inherited Create;
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

procedure TmncFBConnection.SetProtocol(const AValue: TFBProtocol);
begin
  if FProtocol =AValue then exit;
  FProtocol :=AValue;
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
  //CheckActive;
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
  FDialect := FB_DIALECT;

  aParams := TStringList.Create;
  try
    aParams.Assign(Params);
    FBHostInfo(Host, UserName, Password, Role, CharacterSet, aParams, CachedPasswords);
    GenerateDPB(aParams, DPB, FDPBLength);
  finally
    aParams.Free;
  end;
  FBAlloc(FDPB, 0, FDPBLength);
  Move(DPB[1], FDPB[0], FDPBLength);
  aDatabaseName := FBComposeConnectionString(Resource, Host, Port, FBClient.IsEmbed, Protocol);
  if Call(FBClient.isc_attach_database(@StatusVector, Length(aDatabaseName),
    PChar(aDatabaseName), @FHandle, FDPBLength, FDPB), StatusVector, False) > 0 then
  begin
    FHandle := nil;
    FBRaiseError(StatusVector);
  end;
  FDialect := GetDBSQLDialect;
  if (FDialect < FB_DIALECT) then
    raise EFBError.Create(-1, 'This database not dialect ' + IntToStr(FB_DIALECT))

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
  FDialect := FB_DIALECT;
end;

{ TmncFBSession }

destructor TmncFBSession.Destroy;
begin
  FreeMem(FTPB);
  FTPB := nil;
  inherited;
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
  status: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  if not Retaining then
  try
    //EndSQLObjects;
  finally
  end;

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
{  if ((Force) and (status > 0)) then
    status := Call(FBClient.isc_rollback_transaction(@StatusVector, @FHandle), StatusVector, True);
    if not Retaining and Force then
      FHandle := nil
    if (status > 0) then
    FBRaiseError(StatusVector);
}
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


function TmncFBSession.GetRowsChanged: Integer;
begin
  CheckActive;
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

class function TmncFBConnection.GetMode: TmncSessionMode;
begin
  Result := smMultiple;
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

{ TFieldHelper }


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

function TmncFBParams.GetNames: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result <> '' then
      Result := Result + sLineFeed;
    Result := Result + Items[i].SQLVAR.Name;
  end;
end;

constructor TmncFBParams.Create;
begin
  inherited;
  FBAlloc(FData, 0, XSQLDA_LENGTH(0));
  FData.version := SQLDA_VERSION1;
end;

destructor TmncFBParams.Destroy;
begin
  Clear;
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited;
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

function TmncFBFields.GetNames: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result <> '' then
      Result := Result + sLineFeed;
    Result := Result + Items[i].SQLVAR.Name;
  end;
end;

procedure TmncFBFields.SetData(FData: PXSQLDA);
begin
  FreeSQLDA(FData);
  FData := FData;
end;

procedure TmncFBFields.Prepare(NewCount: integer);
var
  i: Integer;
  aVarSize: Integer;
  p: Pointer;
  OldCount: Integer;
  aColumn: TmncColumn;
  aField: TmncFBField;
begin
  OldCount := Count;

  if NewCount <> Count then
  begin
    if NewCount < Count then
    begin
      for i := NewCount to Count - 1 do
      begin
        Items[i].Free;
      end;
    end;
    FBAlloc(FData, XSQLDA_LENGTH(Count), XSQLDA_LENGTH(NewCount));
  end;

  Count := NewCount;

  Data.version := SQLDA_VERSION1;
  aVarSize := sizeof(TXSQLVAR);

  p := @Data^.sqlvar[0];
  for i := 0 to Count - 1 do
  begin
    if i >= OldCount then
    begin
          //aColumn := Columns.Add('')
          //aField := Add(aColumn);

//            Items[i].FSQLVAR.XSqlVar := p;
    end;

    Items[i].SQLVAR := p;
    //            Items[i].Clear;

    p := Pointer(PAnsiChar(p) + aVarSize);
  end;

  if Count > 0 then
  begin
    Data^.sqln := NewCount;
    Data^.sqld := NewCount;
  end;
end;

constructor TmncFBFields.Create(vColumns: TmncColumns);
begin
  inherited;
  FBAlloc(FData, 0, XSQLDA_LENGTH(0));
  FData.version := SQLDA_VERSION1;
end;

destructor TmncFBFields.Destroy;
begin
  Clear;
  FreeSQLDA(FData);
  inherited;
end;

{ TmncFBCommand }

function TmncFBCommand.GetSession: TmncFBSession;
begin
  Result := inherited Session as TmncFBSession;
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
end;

function TmncFBCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

procedure TmncFBCommand.DoExecute;
var
  i: Integer;
  StatusVector: TStatusVector;
begin
  ValidStatement;
  case FSQLType of
    SQLSelect:
      begin
        Call(FBClient.isc_dsql_execute2(@StatusVector,
          @Session.Handle, @FHandle,
          FB_DIALECT,
          (Params as TmncFBParams).Data, nil), StatusVector, True);

        Call(FBClient.isc_dsql_set_cursor_name(@StatusVector, @FHandle, PAnsiChar(FCursor), 0), StatusVector, True);
        FActive := True;
        FBOF := True;
        FEOF := False;
        FRecordCount := 0;
        if NextOnExecute then
          Next;
      end;
    SQLExecProcedure:
      begin
 {       Call(FBClient.isc_dsql_execute2(@StatusVector,
          @Session.Handle, @FHandle, FB_DIALECT,
          FSQLParams.Data, FSQLCurrent.Data), StatusVector, True);}//todo
      end
  else
    Call(FBClient.isc_dsql_execute(@StatusVector,
      @Session.Handle,
      @FHandle,
      FB_DIALECT,
      (Params as TmncFBParams).Data), StatusVector, True)
  end;
end;

procedure TmncFBCommand.DoNext;
begin
end;

procedure TmncFBCommand.DoPrepare;
begin
  InternalPrepare;
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
begin
end;

procedure TmncFBCommand.DoCommit;
begin
  Session.Commit;
end;

function TmncFBCommand.GetActive: Boolean;
begin
end;

function TmncFBCommand.GetConnection: TmncFBConnection;
begin
  Result := Session.Connection as TmncFBConnection;
end;

procedure TmncFBCommand.ValidStatement;
begin
  //CheckTransaction;
  if (FHandle = nil) then
    FBRaiseError(fbceInvalidStatementHandle, [nil]);
end;

procedure TmncFBCommand.DoUnprepare;
begin
  inherited;
  FreeHandle;
end;

function TmncFBCommand.Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := 0;
  if (ErrCode > 0) and (Session <> nil) then
    Result := Session.Call(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBCommand.InternalPrepare;
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
    Result := (Fields as TmncFBFields);
  end;

  function Params: TmncFBParams;
  begin
    Result := (Params as TmncFBParams);
  end;
var
  aColumn: TmncColumn;
  aField: TmncFBField;
  aParam: TmncFBParam;
begin
  if not Prepared then
  begin
    if (SQL.Text = '') then
      FBRaiseError(fbceEmptyQuery, [nil]);
    FParsedSQL := ParseSQL([]);
    if (FParsedSQL = '') then
      FBRaiseError(fbceEmptyQuery, [nil]);
    try
      Call(FBClient.isc_dsql_alloc_statement2(@StatusVector, @Connection.Handle, @FHandle), StatusVector, True);
      Call(FBClient.isc_dsql_prepare(@StatusVector, @Session.Handle, @FHandle, 0, PAnsiChar(FParsedSQL), FB_DIALECT, nil), StatusVector, True);
      { After preparing the statement, query the stmt type and possibly
        create a FSQLCurrent "holder" }
      { Get the type of the statement }
      type_item := Char(isc_info_sql_stmt_type);
      Call(FBClient.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @type_item, SizeOf(res_buffer), res_buffer), StatusVector, True);
      if not GetInfoReqInteger(res_buffer, isc_info_sql_stmt_type, sql_type) then
        FBRaiseError(fbceUnknownError, [nil]);
      FSQLType := TFBDSQLTypes(sql_type);

      { Done getting the type }
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
            { here we must save the old values of params }
            { Fetch params info int SQLDA}
            {if (FSQLParams.Data <> nil) and
              (Call(FBClient.isc_dsql_describe_bind(@StatusVector, @FHandle, FB_DIALECT, FSQLParams.Data), StatusVector, True) > 0) then
              FBRaiseError(StatusVector);
            FSQLParams.Initialize;}

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
                  Self.Fields := CreateFields(Columns); //need to create Fields becuase it have SQLDA buffer
                Fields.Count := c;

                InitSQLDA(Fields.FData, c);

              { Get count of columns }
                Call(FBClient.isc_dsql_describe(@StatusVector, @FHandle, FB_DIALECT, Fields.Data), StatusVector, True);
                p := @Fields.Data^.sqlvar[0];
                for i := 0 to Fields.Data^.sqld - 1 do
                begin
                  aColumn := Columns.Add(p^.aliasname, SQLTypeToDataType(p^.sqltype));
                  aField := Fields.Add(aColumn) as TmncFBField;
                  //aField.SQLVAR.SqlVar := p;
                  p := Pointer(PAnsiChar(p) + XSQLVar_Size);
                end;
                //FSQLCurrent.Initialize;
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

procedure ChangeFields(Fields: TmncCustomFields; NewCount:short);
var
  i: Integer;
  p: Pointer;
  OldCount: Integer;
  Data: PXSQLDA;
  aColumn: TmncColumn;
  aField: TmncFBField;
  aParam: TmncFBParam;
begin
  if Fields is TmncFBFields then
    Data := (Fields as TmncFBFields).Data
  else if Fields is TmncFBParams then
    Data := (Fields as TmncFBParams).Data
  else
    EFBExceptionError.Create('Fields is not Firebird SQL Fields');

  OldCount := Fields.Count;
  if NewCount <> 0 then
  begin
    if NewCount <> Fields.Count then
    begin
      if NewCount < Fields.Count then
      begin
        for i := NewCount to Fields.Count - 1 do
        begin
          Fields.Items[i].Free;
        end;
      end;
    end;

    Fields.Count := NewCount;
    InitSQLDA(Data, NewCount);

    p := @Data^.sqlvar[0];
    for i := 0 to Fields.Count - 1 do
    begin
      if i >= OldCount then
      begin
        if Fields is TmncFBFields then
        begin
          with Fields as TmncFBFields do
          begin
            //aColumn := Columns.Add('')
            //aField := Add(aColumn);

//            Items[i].FSQLVAR.XSqlVar := p;
          end;
        end
        else
        begin
          with Fields as TmncFBParams do
          begin
          end;
        end;
      end;

      if Fields is TmncFBFields then
        (Fields as TmncFBFields).Items[i].SQLVAR.XSQLVar := p
      else
        (Fields as TmncFBParams).Items[i].SQLVAR.XSQLVar := p;
      //            Items[i].Clear;

      p := Pointer(PAnsiChar(p) + XSQLVar_Size);
    end;
    if Fields.Count > 0 then
    begin
      Data^.sqln := NewCount;
      Data^.sqld := NewCount;
    end;
  end;
end;

end.

procedure InitializeSQLDA(Fields);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    with Items[i].Data do
    begin
      if Items[i].Name = '' then
      begin
        if AliasName = '' then
          AliasName := 'F_' + IntToStr(i);
        Items[i].Name := FBDequoteName(aliasname);
      end;

      if (SqlDef = SQL_TEXT) or
        (SqlDef = SQL_VARYING) then
        Items[i].FMaxLen := sqllen
      else
        Items[i].FMaxLen := 0;

      if FXSQLVAR^.sqldata = nil then
        case SqlDef of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
            SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
            SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN:
            begin
              if (sqllen = 0) then
              { Make sure you get a valid pointer anyway
               select '' from foo }
                SetDataSize(0, 1)
              else
                SetDataSize(0, sqllen)
            end;
          SQL_VARYING:
            begin
              SetDataSize(0, sqllen + 2);
            end;
        else
          FBError(fbceUnknownSQLDataType, [SqlDef])
        end;
      if (sqltype and 1 = 1) then
        SetIndSize(0, SizeOf(Short))
      else if (sqlind <> nil) then
        SetIndSize(0, 0);
    end;
  end;
end;

end.
