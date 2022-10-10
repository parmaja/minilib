unit mncFirebird;
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
 * @comment   for Firebird 2.5
 *}

interface

uses
  Classes, SysUtils, Variants,
  mncConnections, mncSQL,
  mncFBHeader, mncFBClasses, mncFBUtils, mncDB;

const
  FB_DIALECT = 3;

type

  TFBErrorHandle = (
    eonExecute, //Not yet
    eonDisconnect //Do not raise error when disconnect
  );

  TFBErrorHandles = set of TFBErrorHandle;

  { TmncFBConnection }

  TmncFBConnection = class(TmncSQLConnection)
  private
    FErrorHandles: TFBErrorHandles;
    FHandle: TISC_DB_HANDLE;
    FCharacterSet: string;
    FRole: string;
    function GetIsReadOnly: Boolean;
  protected
    function ExecDatabaseInfo(vCommand: Tdb_info_types): TFBLocalBufferArray;

    function GetBaseLevel: Long;
    function GetDBSQLDialect: Integer;

    function CheckErr(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure DoInit; override;
  public
    constructor Create; override;
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    function CreateTransaction: TmncSQLTransaction; override;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); override;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); override;
    function IsDatabaseExists(vName: string): Boolean; override;
    procedure Vacuum; override;
    function GetVersion: string;
    function GetExtension: string; override;
    procedure Execute(vCommand: string); override;
    property Role: string read FRole write FRole;
    property CharacterSet: string read FCharacterSet write FCharacterSet; //ex: WIN1252 for Lazarus use UTF8
    //todo 'character set WIN1252 collate WIN_PTBR';
    property Handle: TISC_DB_HANDLE read FHandle;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property ErrorHandles: TFBErrorHandles read FErrorHandles write FErrorHandles;
    procedure SetVariable(const vName, vData: string);
  end;

  { TmncFBTransaction }

  TmncFBTransaction = class(TmncSQLTransaction)
  private
    FHandle: TISC_TR_HANDLE;
    FTPB: TBytes;
    function GetConnection: TmncFBConnection;
    procedure SetConnection(const AValue: TmncFBConnection);
  protected
    function CheckErr(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoStop(How: TmncTransactionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
    function InternalCreateCommand: TmncSQLCommand; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(vSQL: string);
    property Handle: TISC_TR_HANDLE read FHandle;
    property TPB: TBytes read FTPB;
    property Connection: TmncFBConnection read GetConnection write SetConnection;
  end;

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

  { TmncFBFields }

  TmncFBFields = class(TmncFields)
  private
    FSQLDA: PXSQLDA;
    function GetItem(Index: Integer): TmncFBField;
  protected
    function GetModified: Boolean;
    function DoCreateField(vColumn: TmncColumn): TmncField; override;
    procedure Detach; override;
  public
    constructor Create(vColumns: TmncColumns); override;
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TmncFBField read GetItem;
    property SQLDA: PXSQLDA read FSQLDA;
  end;

  { TFBSQLParam }

  TmncFBParam = class(TmncParam)
  private
    XSQLVAR: TXSQLVAR;
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

  { TmncFBParams }

  TmncFBParams = class(TmncParams)
  private
    function GetItem(Index: Integer): TmncFBParam;
  protected
    function GetModified: Boolean;
    function CreateParam: TmncParam; override;
    procedure Detach; override;
  public
    property Items[Index: Integer]: TmncFBParam read GetItem;
  end;

  { TmncFBBinds }

  TmncFBBinds = class(TmncBinds)
  private
    FSQLDA: PXSQLDA;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    property SQLDA: PXSQLDA read FSQLDA;
  end;

  { TmncCustomFBCommand }

  TmncCustomFBCommand = class(TmncSQLCommand)
  private
    function GetConnection: TmncFBConnection;
    function GetTransaction: TmncFBTransaction;
    procedure SetTransaction(const Value: TmncFBTransaction);
    function GetParams: TmncFBParams;
    function GetBinds: TmncFBBinds;
  protected
    function GetParseOptions: TmncParseSQLOptions; override;
    function CheckErr(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    function CreateParams: TmncParams; override;
    property Params: TmncFBParams read GetParams;
    property Binds: TmncFBBinds read GetBinds;
  public
    property Connection: TmncFBConnection read GetConnection;
    property Transaction: TmncFBTransaction read GetTransaction write SetTransaction;
  end;

  TmncFBCommand = class(TmncCustomFBCommand)
  private
    FHandle: TISC_STMT_HANDLE;
    FActive: Boolean;
    FCursor: string;
    FSQLType: TFBDSQLTypes;
    FBOF: Boolean;
    FEOF: Boolean;
    procedure SetCursor(AValue: string);
    procedure FreeHandle;
    //Very dangrouse functions, be sure not free it with SQLVAR sqldata and sqlind, becuase it is shared with params sqldata
    //AllocateBinds apply param values into Binds SQLDA
    procedure AllocateBinds(out XSQLDA: PXSQLDA);
    procedure DeallocateBinds(var XSQLDA: PXSQLDA);
    function GetFields: TmncFBFields;
    procedure SetFields(Value: TmncFBFields);
  protected
    procedure CheckHandle;//TODO remove it
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    procedure DoUnprepare; override;
    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    //function GetPlan: string;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    function CreateBinds: TmncBinds; override;
    function GetDone: Boolean; override;
  public
    procedure Clear; override;
    function GetRowsChanged: Integer; override;
    property SQLType: TFBDSQLTypes read FSQLType;
    property Handle: TISC_STMT_HANDLE read FHandle;
    procedure RunSQL(const vSQL: string);

    property Fields: TmncFBFields read GetFields write SetFields;
    { Cursor name
      Optional you can use it

      UPDATE ... WHERE CURRENT OF MyCursor;
      DELETE FROM ... WHERE CURRENT OF MyCursor;

      http://tech.groups.yahoo.com/group/firebird-support/messages/65692?threaded=1&m=e&var=1&tidx=1
    }
    property Cursor: string read FCursor write SetCursor;
  end;

  TmncFBDDLCommand = class(TmncCustomFBCommand)
  private
  protected
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoParse; override;
    procedure DoNext; override;
    function GetDone: Boolean; override;
    function CreateColumns: TmncColumns; override;
  end;

implementation

function SQLTypeToDataType(SQLType: Integer):TmncDataType;
begin
  case SQLType of
    SQL_TEXT: Result := dtString;
    SQL_DOUBLE: Result := dtFloat;
    SQL_FLOAT: Result := dtFloat;
    SQL_LONG: Result := dtInteger;
    SQL_SHORT: Result := dtInteger;
    SQL_TIMESTAMP: Result := dtDateTime;
    SQL_BLOB: Result := dtBlob;
    SQL_D_FLOAT: Result := dtFloat;
    SQL_ARRAY: Result := dtUnknown;
    SQL_QUAD: Result := dtBlob;
    SQL_TYPE_TIME: Result := dtTime;
    SQL_TYPE_DATE: Result := dtDate;
    SQL_INT64: Result := dtInteger;
    SQL_NULL: Result := dtUnknown;
    //SQL_DATE: Result := dtDateTime;
    SQL_BOOLEAN: Result := dtBoolean;
    else
      Result := dtUnknown;
  end;
end;

{ TmncFBConnection }

constructor TmncFBConnection.Create;
begin
  inherited Create;
  FErrorHandles := [eonExecute, eonDisconnect];
  {$ifdef FPC}
  FCharacterSet := 'UTF8';
  {$else}
  FCharacterSet := 'UTF8';
  {$endif}
end;

class function TmncFBConnection.Capabilities: TmncCapabilities;
begin
  Result:= [ccDB, ccAlias, ccPath, ccCreate, {ccDrop, }ccSQL, ccNetwork, ccTransaction, ccMultiTransaction];
end;

class function TmncFBConnection.EngineName: string;
begin
  Result := 'FirebirdSQL';
end;

function TmncFBConnection.GetExtension: string;
begin
  Result := '.fdb';
end;

function TmncFBConnection.CreateTransaction: TmncSQLTransaction;
begin
  Result := TmncFBTransaction.Create(Self);
end;

procedure TmncFBConnection.CreateDatabase(const vName: string; CheckExists: Boolean);
var
  aUserName, aPassword: string;
  aParams: TStringList;
  ConnectString: UTF8String;
  tr_handle: TISC_TR_HANDLE;
  StatusVector: TStatusVector;
  aHandle: TISC_DB_HANDLE;
begin
  aParams := TStringList.Create;
  try
    aUserName := UserName;
    aPassword := Password;

    if aUserName <> '' then
      aParams.Add(Format('USER ''%s''', [aUserName]));
    if aPassword <> '' then
      aParams.Add(Format('PASSWORD ''%s''', [aPassword]));
    if CharacterSet <> '' then
      aParams.Add(Format('DEFAULT CHARACTER SET %s', [UpperCase(CharacterSet)]));

    ConnectString := 'CREATE DATABASE ''' + FBComposeConnectionString(vName, Host, Port) +  ''' ' + aParams.Text;
  finally
    aParams.Free;
  end;

  try
    tr_handle := 0;
    aHandle := 0;
    CheckErr(FBLib.isc_dsql_execute_immediate(@StatusVector, @aHandle, @tr_handle, Length(ConnectString), PByte(ConnectString), FB_DIALECT, nil), StatusVector, True);
    CheckErr(FBLib.isc_detach_database(@StatusVector, @aHandle), StatusVector, False);
  finally
  end;
end;

procedure TmncFBConnection.DropDatabase(const vName: string; CheckExists: Boolean);
var
  aUserName, aPassword: string;
  aParams: TStringList;
  ConnectString: UTF8String;
  tr_handle: TISC_TR_HANDLE;
  StatusVector: TStatusVector;
  aHandle: TISC_DB_HANDLE;
begin
  aParams := TStringList.Create;
  try
    aUserName := UserName;
    aPassword := Password;

    if aUserName <> '' then
      aParams.Add(Format('USER ''%s''', [aUserName]));
    if aPassword <> '' then
      aParams.Add(Format('PASSWORD ''%s''', [aPassword]));
    if CharacterSet <> '' then
      aParams.Add(Format('DEFAULT CHARACTER SET %s', [UpperCase(CharacterSet)]));

    ConnectString := 'DROP DATABASE ''' + FBComposeConnectionString(vName, Host, Port) +  ''' ' + aParams.Text;
  finally
    aParams.Free;
  end;

  try
    tr_handle := 0;
    aHandle := 0;
    CheckErr(FBLib.isc_dsql_execute_immediate(@StatusVector, @aHandle, @tr_handle, Length(ConnectString), PByte(ConnectString), FB_DIALECT, nil), StatusVector, True);
  finally
  end;
end;

function TmncFBConnection.IsDatabaseExists(vName: string): Boolean;
begin
  //TODO
end;

procedure TmncFBConnection.SetVariable(const vName, vData: string);
var
  aTR: TmncFBTransaction;
  s: string;
begin
  aTR := TmncFBTransaction.Create(Self);
  try
    aTR.Start;
    s := Format('select RDB$SET_CONTEXT(''USER_Transaction'', ''%s'', ''%s'') from rdb$database', [vName, vData]);
    aTR.Execute(s);
    aTR.Commit;
  finally
    aTR.Free;
  end;
end;

function TmncFBConnection.ExecDatabaseInfo(vCommand: Tdb_info_types): TFBLocalBufferArray;
var
  b: Byte;
  StatusVector:TStatusVector;
  aStatus: ISC_STATUS;
begin
  b := ord(vCommand);
  aStatus := FBLib.isc_database_info(@StatusVector, @FHandle, 1, @b, FBLocalBufferLength, @Result[0]);
  CheckErr(aStatus, StatusVector, True);
end;

procedure TmncFBConnection.Vacuum;
begin
  //TODO
end;

function TmncFBConnection.GetVersion: string;
var
  aBuf: TFBLocalBufferArray;
begin
  aBuf := ExecDatabaseInfo(isc_info_version);
  Result := FBGetString(aBuf, 5, Integer(aBuf[4]));
end;

function TmncFBConnection.GetIsReadOnly: Boolean;
var
  aBuf: TFBLocalBufferArray;
  aLen: Integer;
begin
  aBuf := ExecDatabaseInfo(isc_info_db_read_only);
  aLen := FBLib.isc_vax_integer(@aBuf[1], 2);
  Result := FBLib.isc_vax_integer(@aBuf[3], aLen)<>0;
end;

function TmncFBConnection.GetBaseLevel: Long;
var
  aBuf: TFBLocalBufferArray;
begin
  aBuf := ExecDatabaseInfo(isc_info_base_level);
  Result := FBLib.isc_vax_integer(@aBuf[4], 1);
end;

function TmncFBConnection.GetDBSQLDialect: Integer;
var
  aBuf: TFBLocalBufferArray;
  aLen: Integer;
begin
  aBuf := ExecDatabaseInfo(isc_info_db_SQL_Dialect);
  if (aBuf[0] <> ord(isc_info_db_SQL_dialect)) then
    Result := 1
  else
  begin
    aLen := FBLib.isc_vax_integer(@aBuf[1], 2);
    Result := FBLib.isc_vax_integer(@aBuf[3], aLen);
  end;
end;

function TmncFBConnection.CheckErr(ErrCode: ISC_STATUS;  const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
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
  aDPBLength: SmallInt;
  //i: Integer;
  aDatabaseName: UTF8String;
  aParams: TStringList;
  StatusVector: TStatusVector;
  aDPBs: TBytes;
  aDPB: PByte;
begin
  aParams := TStringList.Create;
  try
    aParams.Assign(Params);
    FBDatabaseInfo(UserName, Password, Role, CharacterSet, aParams);
    aDPBs := GenerateDPBEx(aParams);
    aDPBLength := Length(aDPBs);
  finally
    aParams.Free;
  end;

  aDPB := PByte(@aDPBs[0]);
  try
    aDatabaseName := FBComposeConnectionString(Resource, Host, Port);
    if CheckErr(FBLib.isc_attach_database(@StatusVector, Length(aDatabaseName), PByte(aDatabaseName), @FHandle, aDPBLength, aDPB), StatusVector, False) > 0 then
    begin
      FHandle := 0;
      FBRaiseError(StatusVector);
    end;
    if (GetDBSQLDialect < FB_DIALECT) then
      raise EFBError.Create(-1, 'This database not dialect 3, other dialects not supported')
  finally
  end;

{    for i := 0 to FEventNotifiers.Count - 1 do
      if IFBEventNotifier(FEventNotifiers[i]).GetAutoRegister then
        IFBEventNotifier(FEventNotifiers[i]).RegisterEvents;}
end;

function TmncFBConnection.GetConnected: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TmncFBConnection.DoDisconnect;
var
  StatusVector: TStatusVector;
begin
  if (eonDisconnect in ErrorHandles) and (CheckErr(FBLib.isc_detach_database(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector)
  else
    FHandle := 0;
end;

{ TmncFBTransaction }

destructor TmncFBTransaction.Destroy;
begin

  inherited;
end;

function TmncFBTransaction.InternalCreateCommand: TmncSQLCommand;
begin
  Result := TmncFBCommand.CreateBy(Self);
end;

procedure TmncFBTransaction.Execute(vSQL: string);
var
  StatusVector: TStatusVector;
  s: UTF8String;
begin
  s := UTF8Encode(vSQL);
  CheckErr(FBLib.isc_dsql_execute_immediate(@StatusVector, @Connection.Handle, @FHandle, Length(s), PByte(s), FB_DIALECT, nil), StatusVector, True);
end;

procedure TmncFBTransaction.DoStart;
var
  pteb: PISC_TEB_ARRAY;
  aTPB: PByte;
  StatusVector: TStatusVector;
begin
  if ParamsChanged then
  begin
    ParamsChanged := False;
    FTPB := GenerateTPBEx(Params);
  end;

  if Length(FTPB)=0 then
    aTPB := nil
  else
    aTPB := PByte(@FTPB[0]);

  pteb := nil;
  FBAlloc(pteb, 0, SizeOf(TISC_TEB), False);
  try
    pteb^[0].db_handle := @(Connection.Handle);
    pteb^[0].tpb_length := Length(FTPB);
    pteb^[0].tpb_address := aTPB;
    if CheckErr(FBLib.isc_start_multiple(@StatusVector, @FHandle, 1, PISC_TEB(pteb)), StatusVector, False) > 0 then
    begin
      FHandle := 0;
      FBRaiseError(StatusVector);
    end;
  finally
    FreeMem(pteb);
  end;
end;

procedure TmncFBTransaction.DoStop(How: TmncTransactionAction; Retaining: Boolean);
var
  StatusVector: TStatusVector;
begin
  case How of
    sdaCommit:
    begin
      if not Retaining then
        CheckErr(FBLib.isc_commit_transaction(@StatusVector, @FHandle), StatusVector, True)
      else
        CheckErr(FBLib.isc_commit_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
    sdaRollback:
    begin
      if not Retaining then
        CheckErr(FBLib.isc_rollback_transaction(@StatusVector, @FHandle), StatusVector, True)
      else
        CheckErr(FBLib.isc_rollback_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
  end;
end;

procedure TmncFBConnection.Execute(vCommand: string);
var
  tr_handle: TISC_TR_HANDLE;
  StatusVector: TStatusVector;
  s: UTF8String;
begin
  tr_handle := 0;
  try
    s := UTF8Encode(vCommand);
    CheckErr(FBLib.isc_dsql_execute_immediate(@StatusVector, @FHandle, @tr_handle, 0, PByte(s), FB_DIALECT, nil), StatusVector, True);
  finally
  end;
end;

function TmncFBTransaction.GetActive: Boolean;
begin
  Result:= FHandle <> 0;
end;

constructor TmncFBTransaction.Create(vConnection: TmncConnection);
begin
  inherited;
  FHandle := 0;
  FTPB := nil;
end;

function TmncFBTransaction.GetConnection: TmncFBConnection;
begin
  Result := inherited Connection as TmncFBConnection;
end;

procedure TmncFBConnection.DoInit;
begin
  inherited;
  FBLib.Load;
end;

procedure TmncFBTransaction.SetConnection(const AValue: TmncFBConnection);
begin
  inherited Connection := AValue;
end;

function TmncFBTransaction.CheckErr(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if CheckStatusVector(StatusVector, [isc_lost_db_connection]) then
    Connection.CheckErr(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (Result > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBTransaction.DoInit;
begin
end;

{ TmncFBParam }

constructor TmncFBParam.Create;
begin
  inherited;
  FSQLVAR := TmncSQLVAR.Create;
  FSQLVAR.XSQLVar := @XSQLVAR;
end;

destructor TmncFBParam.Destroy;
begin
  XSQLVAR.Clean;
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
  if AValue='' then
    Clear
  else
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
  if AValue='' then
    Clear
  else
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

function TmncFBFields.DoCreateField(vColumn: TmncColumn): TmncField;
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
  //InitSQLDA(FSQLDA, 0);
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
  FreeSQLDA(FSQLDA);
end;

{ TmncFBBinds }

procedure TmncFBBinds.Clear;
begin
  inherited Clear;
  FreeSQLDA(FSQLDA, False);
end;

constructor TmncFBBinds.Create;
begin
  inherited;
  //InitSQLDA(FSQLDA, 0, False);//no need
end;

destructor TmncFBBinds.Destroy;
begin
  Clear;
  inherited;
end;

{ TmncFBCommand }

procedure TmncFBCommand.RunSQL(const vSQL: string);
begin
  Transaction.Execute(vSQL);
end;

procedure TmncFBCommand.SetCursor(AValue: string);
begin
  if FCursor <> AValue then
  begin
    CheckInactive;
    FCursor :=AValue;
  end;
end;

procedure TmncFBCommand.SetFields(Value: TmncFBFields);
begin
  inherited Fields := Value;
end;

procedure TmncFBCommand.FreeHandle;
var
  isc_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  try
    if (FHandle <> 0) then
    begin
      //if Connection.Connected then
      begin
        isc_res := CheckErr(FBLib.isc_dsql_free_statement(@StatusVector, @FHandle, DSQL_drop), StatusVector, False);
        if (StatusVector[0] = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) and (isc_res <> isc_lost_db_connection) then
          FBRaiseError(StatusVector);
      end;
    end;
  finally
    FHandle := 0;
  end;
end;

procedure TmncFBCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

function TmncFBCommand.GetDone: Boolean;
begin
  Result := FEOF;
end;

function TmncFBCommand.GetFields: TmncFBFields;
begin
  Result := inherited Fields as TmncFBFields;
end;

function TmncFBCommand.GetRowsChanged: Integer;
var
  result_buffer: array[0..1048] of Byte;
  info_request: Char;
  StatusVector: TStatusVector;
begin
  if not Prepared then
    Result := -1
  else
  begin
    info_request := Char(isc_info_sql_records);
    if FBLib.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @info_request,
      SizeOf(result_buffer), @result_buffer[0]) > 0 then
      FBRaiseError(StatusVector);
    if (result_buffer[0] <> isc_info_sql_records) then
      Result := -1
    else
      case SQLType of
        SQLSelect: Result := GetInfoReqRecord(@result_buffer[0], isc_info_req_select_count);
        SQLUpdate: Result := GetInfoReqRecord(@result_buffer[0], isc_info_req_update_count);
        SQLDelete: Result := GetInfoReqRecord(@result_buffer[0], isc_info_req_delete_count);
        SQLInsert: Result := GetInfoReqRecord(@result_buffer[0], isc_info_req_insert_count);
      else
        Result := -1;
      end;
  end;
end;

procedure TmncFBCommand.DoExecute;
var
  StatusVector: TStatusVector;
  aData: PXSQLDA;
begin
  if Active then DoClose;
  FBOF := True;
  FEOF := False;
  CheckHandle;
  AllocateBinds(aData);
  try
    case FSQLType of
      SQLSelect:
      begin
        CheckErr(FBLib.isc_dsql_execute2(@StatusVector,  @Transaction.Handle, @FHandle, FB_DIALECT, aData, nil), StatusVector, True);
        if FCursor <> '' then
          CheckErr(FBLib.isc_dsql_set_cursor_name(@StatusVector, @FHandle, PByte(FCursor), 0), StatusVector, True);
        FActive := True;
        FBOF := False;
      end;
      SQLExecProcedure:
      begin
        if Fields <> nil then
        begin
          CheckErr(FBLib.isc_dsql_execute2(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData, Fields.SQLDA), StatusVector, True);
          FActive := True;
          FBOF := True;
        end
        else
        begin
          //CheckErr(FBLib.isc_dsql_execute2(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData, aData), StatusVector, True);
          CheckErr(FBLib.isc_dsql_execute2(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData, nil), StatusVector, True);
          FEOF := True;
        end;
      end;
      SQLCommit:
      begin
        CheckErr(FBLib.isc_dsql_execute(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData), StatusVector, True);
        FEOF := True;
      end;
      SQLStartTransaction:
      begin
        CheckErr(FBLib.isc_dsql_execute(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData), StatusVector, True);
        FEOF := True;
      end;
      else
      begin
        CheckErr(FBLib.isc_dsql_execute(@StatusVector, @Transaction.Handle, @FHandle, FB_DIALECT, aData), StatusVector, True);
        FActive := True;
        FEOF := True;
      end;
    end;
  finally
    DeallocateBinds(aData);
  end;
end;

procedure TmncFBCommand.DoNext;
var
  fetch_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  if FBOF then
  begin
    FBOF := False;
  end
  else
  begin
    if (FSQLType = SQLExecProcedure) then
    begin
      FEOF := True;
      Fields.Clean;
    end;
  end;

  if not FEOF  and (FSQLType <> SQLExecProcedure) then
  begin
      fetch_res := CheckErr(FBLib.isc_dsql_fetch(@StatusVector, @FHandle, FB_DIALECT, (Fields as TmncFBFields).FSQLDA), StatusVector, False);

      if (fetch_res = 100) or (CheckStatusVector(StatusVector, [isc_dsql_cursor_err])) then
      begin
        FEOF := True;
        Fields.Clean;
      end
      else if (fetch_res > 0) then
        FBRaiseError(StatusVector)
  end;
end;

procedure TmncFBCommand.DoRollback;
begin
  Transaction.Rollback;
end;

function TmncFBCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncFBFields.Create(vColumns);
end;

function TmncFBCommand.CreateBinds: TmncBinds;
begin
  Result := TmncFBBinds.Create;
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
    if (FHandle <> 0) and (SQLType in [SQLSelect, SQLSelectForUpdate, SQLExecProcedure]) {and FActive //zaher} then
    begin
      isc_res := CheckErr(FBLib.isc_dsql_free_statement(@StatusVector, @FHandle, DSQL_close), StatusVector, False);
      if (StatusVector[0] = 1) and (isc_res > 0) and
        not CheckStatusVector(StatusVector, [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        if isc_res = isc_lost_db_connection then
          CheckErr(isc_res, StatusVector, True)
        else
          FBRaiseError(StatusVector);
    end;
  finally
    FEOF := True;
    FActive := False;
  end;
end;

procedure TmncFBCommand.DoCommit;
begin
  Transaction.Commit;
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

procedure TmncFBCommand.CheckHandle;
begin
  //CheckTransaction;
  if (FHandle = 0) then
    FBRaiseError(fbceInvalidStatementHandle, [nil]);
end;

procedure TmncFBCommand.AllocateBinds(out XSQLDA: PXSQLDA);
var
  i: Integer;
  p: PXSQLVAR;
  x: PXSQLDA;
  aParam: TmncFBParam;
begin
  XSQLDA := nil;
  if (Binds.Count > 0) then
  begin
    x := Binds.SQLDA;

    InitSQLDA(XSQLDA, Binds.Count, False);
    move(x^, XSQLDA^, sizeof(TXSQLDA) - Sizeof(TXSQLVAR)); //minus first value because it is included with the size of TXSQLDA
    p := @XSQLDA^.sqlvar[0];
    for i :=0 to Binds.Count -1 do
    begin
      aParam := Binds[i].Param as TmncFBParam;
      move(aParam.SQLVAR.XSQLVar^, p^, sizeof(TXSQLVAR));
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

procedure TmncFBCommand.DoPrepare;
var
  res_buffer: array[0..7] of Byte;
  type_item: Byte;
  sql_type:Integer;
  StatusVector: TStatusVector;
  aData: PXSQLDA;
  p: PXSQLVAR;
  c: Integer;
  i: Integer;

var
  aColumn: TmncColumn;
  aField: TmncFBField;
  aParam: TmncFBParam;
  aSQL: UTF8String;

begin
  aSQL := UTF8Encode(GetProcessedSQL);
  try
    CheckErr(FBLib.isc_dsql_alloc_statement2(@StatusVector, @Connection.Handle, @FHandle), StatusVector, True);
    CheckErr(FBLib.isc_dsql_prepare(@StatusVector, @Transaction.Handle, @FHandle, 0, PByte(aSQL), FB_DIALECT, nil), StatusVector, True);
    { type of the statement }
    type_item := isc_info_sql_stmt_type;
    CheckErr(FBLib.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @type_item, SizeOf(res_buffer), @res_buffer[0]), StatusVector, True);
    if not GetInfoReqInteger(@res_buffer[0], isc_info_sql_stmt_type, sql_type) then
      FBRaiseError(fbceUnknownError, [nil]);
    FSQLType := TFBDSQLTypes(sql_type);

    case FSQLType of
      SQLGetSegment, SQLPutSegment:
      begin
        FreeHandle;
        FBRaiseError(fbceNotPermitted, [nil]);
      end;
      SQLCommit, SQLRollback, SQLStartTransaction,
      SQLDDL, SQLSetSequence,
      SQLSelect, SQLInsert, SQLUpdate, SQLDelete, SQLSelectForUpdate,
      SQLExecProcedure:
      begin
        //Params is already created and have the items
        InitSQLDA(Binds.FSQLDA, Binds.Count, False); //Binds > Params
        CheckErr(FBLib.isc_dsql_describe_bind(@StatusVector, @FHandle, FB_DIALECT, Binds.FSQLDA), StatusVector, True);

        p := @Binds.FSQLDA^.sqlvar[0];
        for i := 0 to Binds.Count - 1 do
        begin
          aParam := (Binds.Items[i].Param as TmncFBParam);
          if aParam.SQLVAR.XSQLVar.sqldata = nil then //we need to clean sqldata or not reassign it again for memory leak in case of duplicate params
          begin
            aParam.XSQLVAR := p^;
            aParam.SQLVAR.Attach(@Connection.Handle, @Transaction.Handle);
            aParam.SQLVAR.Prepare;
          end;
          p := Pointer(PAnsiChar(p) + XSQLVar_Size);
        end;

        //if (FSQLType in [SQLSelect, SQLSelectForUpdate, SQLExecProcedure]) then
        begin
          //Check if there is a result data
          aData := nil;
          InitSQLDA(aData, 0);
          try
            var aErrCode: ISC_STATUS;
            aErrCode := FBLib.isc_dsql_describe(@StatusVector, @FHandle, FB_DIALECT, aData);
            CheckErr(aErrCode, StatusVector, True);
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
            if Fields = nil then
              Fields := CreateFields(Columns) as TmncFBFields //need to create Fields because it have SQLDA buffer
            else
              Fields.Clear;

            InitSQLDA(Fields.FSQLDA, c);
            aData := Fields.FSQLDA;

            CheckErr(FBLib.isc_dsql_describe(@StatusVector, @FHandle, FB_DIALECT, aData), StatusVector, True);
            p := @aData^.sqlvar[0];
            for i := 0 to aData^.sqld - 1 do
            begin
              aColumn := Columns.Add(p^.GetAliasName, SQLTypeToDataType(p^.sqltype));
              aColumn.Scale := p^.sqlscale;
              //aColumn.Name := FBDequoteName(aColumn.Name);
              aField := Fields.Add(aColumn) as TmncFBField;
              aField.SQLVAR.XSQLVar := p;
              aField.SQLVAR.Attach(@Connection.Handle, @Transaction.Handle);
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
      if (FHandle <> 0) then
        FreeHandle;
      raise;
    end;
  end;
end;

{ TmncCustomFBCommand }

function TmncCustomFBCommand.CheckErr(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := 0;
  if (ErrCode > 0) and (Transaction <> nil) then
    Result := Transaction.CheckErr(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

function TmncCustomFBCommand.CreateParams: TmncParams;
begin
  Result := nil;
end;

function TmncCustomFBCommand.GetConnection: TmncFBConnection;
begin
  Result := Transaction.Connection as TmncFBConnection;
end;

function TmncCustomFBCommand.GetParams: TmncFBParams;
begin
  Result := inherited Params as TmncFBParams;
end;

function TmncCustomFBCommand.GetBinds: TmncFBBinds;
begin
  Result := inherited Binds as TmncFBBinds;
end;

function TmncCustomFBCommand.GetParseOptions: TmncParseSQLOptions;
begin
  Result := [];
end;

function TmncCustomFBCommand.GetTransaction: TmncFBTransaction;
begin
  Result := inherited Transaction as TmncFBTransaction;
end;

procedure TmncCustomFBCommand.SetTransaction(const Value: TmncFBTransaction);
begin
  inherited Transaction := Value;
end;

{ TmncFBDDLCommand }

function TmncFBDDLCommand.CreateColumns: TmncColumns;
begin
  Result := nil;
end;

procedure TmncFBDDLCommand.DoExecute;
begin
  Transaction.Execute(SQL.Text);
end;

procedure TmncFBDDLCommand.DoNext;
begin

end;

procedure TmncFBDDLCommand.DoParse;
begin

end;

procedure TmncFBDDLCommand.DoPrepare;
begin

end;

function TmncFBDDLCommand.GetDone: Boolean;
begin
  Result := True;
end;

initialization
  Engines.RegisterConnection(TmncFBConnection.EngineName, 'FirebirdSQL Database', TmncFBConnection);
end.
