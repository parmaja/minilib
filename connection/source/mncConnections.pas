unit mncConnections;
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
{$MODE delphi}
{$ENDIF}

{
  We Columns Fields and Params
  Columns has info about Fields, like Name, Size, Type etc...,
  while Fields have only the values, the idea, is you can use one Columns with multiple records,
  record here is the Fields

  Params not need that technique.
}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs, SyncObjs,
  mnFields, mncCommons, mncSchemas;

type

  TmncSession = class;
  TmncCommand = class;

  { TmncLinkSession }

  TmncLinkSession = class(TmncLinkObject)
  private
    function GetSession: TmncSession;
    procedure SetSession(AValue: TmncSession);
  protected
  public
    constructor CreateBy(vSession: TmncSession);
    property Session: TmncSession read GetSession write SetSession;//alias for FLink in base class
  end;

  { TmncSessions }

  TmncSessions = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncSession;
    procedure SetItem(Index: Integer; const Value: TmncSession);
  protected
    procedure Stop;
  public
    property Items[Index: Integer]: TmncSession read GetItem write SetItem; default;
  end;

  TmncCodePageConvert = (cpcNone, cpcAnsi, cpcUTF8, cpcUnicode);

  TmncCapability = (
    ccDB, //It is Database engine not just transfer data object
    ccSQL, //It is SQL engine, you know CSV or Paradox is not, we have no plan to support Paradox
    ccNetwork, //Can connect over network
    ccStrict, //Without it, it no need to call Start and Stop (Commit/Rollback) this DB automatically do it (pg/SQLite allow it)
    ccTransaction, //Support transaction, most of them Like Firebird, SQLite, PG
    ccMultiTransaction //Like Firebird can have multiple transaction for one connection, while PG and SQLite has not, see also sbhMultiple
{  nop i hate it
    ccMultiConnection //Some time we need to make for every session a new connection
        //(PG and SQLite), it slowing app, i will not use it. see sbhIndependent
        }
  );
  //if you put ccTransaction and not put ccMultiTransaction it will emulate it.

  TmncCapabilities = set of TmncCapability;

  TmncConnectionModel = record
    Name: string;
    Title: string;
    SchemaClass: TmncSchemaClass;
    Capabilities: TmncCapabilities;
  end;

  { TmncConnection }

  TmncConnection = class(TmncObject)
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FParams: TStrings;
    FParamsChanged: Boolean;
    FPassword: string;
    FPort: string;
    FResource: string;
    FHost: string;
    FUserName: string;
    FAutoStart: Boolean;
    FAutoCreate: Boolean;
    FSessions: TmncSessions;
    FStartCount: Integer;
    FIsInit: Boolean;
    procedure SetConnected(const Value: Boolean);
    procedure SetParams(const AValue: TStrings);
    procedure ParamsChanging(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
  protected
    procedure CheckActive;
    procedure CheckInactive;
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    procedure DoInit; virtual;
    procedure Init;
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
  public
    constructor Create;
    destructor Destroy; override;
    class function Model: TmncConnectionModel; virtual; abstract;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disonnect;

    property Sessions: TmncSessions read FSessions;
    property AutoStart: Boolean read FAutoStart write FAutoStart; //AutoStart the Session when created
    property Connected: Boolean read GetConnected write SetConnected;
    property Active: Boolean read GetConnected write SetConnected;
    property AutoCreate: Boolean read FAutoCreate write FAutoCreate default False;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Resource: string read FResource write FResource; //can be a Database name or Alias or service name etc...
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Params: TStrings read FParams write SetParams;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

  TmncConnectionClass = class of TmncConnection;

  {
    sbhStrict: Without it, it no need to call Start and Stop (Commit/Rollback) this DB automatically do it (pg/SQLite allow it)
    sbhMultiple: Multiple Transactions works simultaneously, Every session have transacion like as Firebird
    sbhEmulate:  Single transaction(main connection) but last session commited make the real commit, maybe PG/SQLite

    sbhIndependent: This session have independent resources, maybe open new connection separated from the main one, or other database file, not sure
  }
  TmncSessionBehavior = (sbhStrict, sbhIndependent, sbhMultiple, sbhEmulate);
  TmncSessionBehaviors = set of TmncSessionBehavior;

  //Session it is branch/clone of Connection but usefull for take a special params, it is like Transactions.
  TmncSessionAction = (sdaCommit, sdaRollback);

  { TmncSession }

  TmncSession = class(TmncLinksObject)
  private
    FParams: TStrings;
    FConnection: TmncConnection;
    FStartCount: Integer;
    FAction: TmncSessionAction;
    FIsInit: Boolean;
    FParamsChanged: Boolean;
    procedure SetParams(const Value: TStrings);
    procedure SetConnection(const Value: TmncConnection);
    procedure SetActive(const Value: Boolean);
    procedure ParamsChanging(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
  protected
    FBehaviors: TmncSessionBehaviors;
    function GetActive: Boolean; virtual;
    procedure CheckActive;
    procedure CheckInactive;
    procedure DoInit; virtual;
    procedure DoStart; virtual; abstract;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); virtual; abstract;
    procedure Init;
    procedure InternalStop(How: TmncSessionAction; Retaining: Boolean = False);
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
    property StartCount: Integer read FStartCount;
  public
    constructor Create(vConnection: TmncConnection); virtual; overload;
    constructor Create(vConnection: TmncConnection; vBehaviors: TmncSessionBehaviors); virtual; overload;
    destructor Destroy; override;

    procedure Start;
    //* Retaining: keep it active
    procedure Commit(Retaining: Boolean = False); virtual;
    procedure Rollback(Retaining: Boolean = False); virtual;
    procedure Stop;
    property Behaviors: TmncSessionBehaviors read FBehaviors;
    property Action: TmncSessionAction read FAction write FAction;
    property Connection: TmncConnection read FConnection write SetConnection;
    property Active: Boolean read GetActive write SetActive;
    property Params: TStrings read FParams write SetParams;
  end;

  TmncDataType = (dtUnknown, dtString, dtBoolean, dtInteger, dtCurrency, dtFloat, dtDate, dtTime, dtDateTime, dtMemo, dtBlob);
  TmncBlobType = (blobBinary, blobText);

{
  TmncItem base class for Column and Field and Param
  TmnColumn have Name but have no Value
  TmnField have no Name but have Value and reference to Column for name and type
  TmnParam have Name and have Value
}

  { TmncCustomColumn }

  TmncItem = class(TmnCustomField)
  private
    FDataType: TmncDataType;
    FBlobType: TmncBlobType;
    FIsBlob: Boolean;
  protected
    function GetAsText: string; override;
    procedure SetAsText(const AValue: string); override;
    property IsBlob: Boolean read FIsBlob write FIsBlob default false;
    property BlobType: TmncBlobType read FBlobType write FBlobType default blobBinary;
    property DataType: TmncDataType read FDataType default dtUnknown;
  public
  published
  end;

  TmncCustomColumnClass = class of TmncItem;

  { TmncItems }

  TmncItems = class(TmnCustomFields)
  private
    function GetItem(Index: Integer): TmncItem; overload;
  protected
    function Find(vName: string): TmncItem; virtual; abstract;
  public
    function Add(AColumn: TmncItem): Integer; overload;
    function ItemByName(vName: string): TmncItem;
    function IsExists(vName: string): Boolean;
    procedure Clean; virtual;
    property Items[Index: Integer]: TmncItem read GetItem;
  end;

  TmnDataOption = (doRequired, doNullable);

  TmnDataOptions = set of TmnDataOption;

  { TmncColumn }

  TmncColumn = class(TmncItem)
  private
    FName: string;
    FIndex: Integer;
    FMaxSize: Integer;
    FOptions: TmnDataOptions;
    FSchemaType: string;
    FSize: Int64;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure SetSize(AValue: Int64); virtual;
  published
    property Index: Integer read FIndex write FIndex;
    property Name: string read FName write FName;
    property DataType;
    property SchemaType: string read FSchemaType write FSchemaType;
    //Size, in sqlite every value have own length not depends on the Schema
    property Size: Int64 read FSize write SetSize; //TODO: I am thinking to move it to TmncItem
    property Options: TmnDataOptions read FOptions write FOptions default [];
    property MaxSize: Integer read FMaxSize write FMaxSize;
  end;

  TmncColumnClass = class of TmncColumn;

  TmncColumns = class(TmncItems)
  private
    function GetItem(Index: Integer): TmncColumn;
  protected
    function Find(vName: string): TmncItem; override;
  public
    function Add(vIndex: Integer; vName: string; vType: TmncDataType; FieldClass: TmncColumnClass = nil): TmncColumn; overload;
    function Add(vName: string; vType: TmncDataType): TmncColumn; overload;
    property Items[Index: Integer]: TmncColumn read GetItem; default;
  end;

  { TmncCustomField }

  TmncCustomField = class(TmncItem)
  private
  protected
  public
  published
    //property IsBlob;
    //property BlobType;
    property IsEmpty;
    property IsNull;
    property Value;
    property AsVariant;
    property AsString;
    property AsAnsiString;
    property AsTrimString;
    property AsNullString;
    property AsInteger;
    property AsInt64;
    property AsBoolean;
    property AsCurrency;
    property AsDate;
    property AsTime;
    property AsDateTime;
    property AsText; //binary text blob convert to hex
    property AsHex;
    property AsDouble;
  end;

  { TmncCustomFields }

  TmncCustomFields = class(TmncItems)
  private
    function GetItem(Index: Integer): TmncCustomField;
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
  protected
    //Called before release it, good to deattach the handles
    procedure Detach; virtual;
  public
    property Items[Index: Integer]: TmncCustomField read GetItem;
    property Values[Index: string]: Variant read GetValue write SetValue;
  end;

  TmncRecord = class(TmncCustomFields)
  private
    function GetItemByName(Index: string): TmncCustomField;
  public
    property Item[Index: string]: TmncCustomField read GetItemByName; default;
  end;

  { TmncField }

  TmncField = class(TmncCustomField)
  private
    FColumn: TmncColumn;
  protected
  public
    constructor Create(vColumn: TmncColumn); virtual;
    function GetName: string;
  published
    property Column: TmncColumn read FColumn write FColumn;
  end;

  { TmncFields }

  TmncFields = class(TmncRecord)
  private
    FColumns: TmncColumns;
    FRowID: Integer;
    function GetItem(Index: Integer): TmncField;
    function GetField(Index: string): TmncField;
  protected
    function Find(vName: string): TmncItem; override;
    function CreateField(vColumn: TmncColumn): TmncField; virtual; abstract;
  public
    constructor Create(vColumns: TmncColumns); virtual;
    function FindField(vName: string): TmncField;
    function FieldByName(vName: string): TmncField;
    function Add(Column: TmncColumn; Value: Variant): TmncField; overload;
    function Add(Column: TmncColumn): TmncField; overload;
    function Add(Index: Integer; Value: Variant): TmncField; overload;
    property Columns: TmncColumns read FColumns;
    property Field[Index: string]: TmncField read GetField; default;
    property Items[Index: Integer]: TmncField read GetItem;
    property RowID: Integer read FRowID write FRowID default 0; //most of SQL engines have this value
  end;

  { TmncParam }

  TmncParam = class(TmncCustomField)
  private
    FName: string;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: string read FName write FName;
  end;

  { TmncCustomParams }

  TmncCustomParams = class(TmncRecord)
  private
  protected
    function GetParam(Index: string): TmncParam;
    function GetItem(Index: Integer): TmncParam;
    function Find(vName: string): TmncItem; override;
  public
    procedure Clear; override;
    function FindParam(vName: string): TmncParam;
    function ParamByName(vName: string): TmncParam;
    property Items[Index: Integer]: TmncParam read GetItem;
    property Param[Index: string]: TmncParam read GetParam; default;
  end;

  { TmncParams }

  TmncParams = class(TmncCustomParams)
  private
  protected
    function CreateParam: TmncParam; virtual; abstract;
  public
    constructor Create; virtual;
    function Add(Name: string): TmncParam;
    //Add it if not exists
    function Found(Name: string): TmncParam;
  end;

  { TmncBind }

  TmncBind = class(TObject)
  private
    FParam: TmncParam;
  public
    property Param: TmncParam read FParam write FParam;
  end;

  { TmncBinds }

  TmncBinds = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncBind;
  protected
    function CreateBind: TmncBind; virtual;
  public
    constructor Create; virtual;
    function Add(ABind: TmncBind): Integer; overload;
    function Add(AParam: TmncParam): Integer; overload;
    property Items[Index: Integer]: TmncBind read GetItem; default;
  end;

  {
    Look at
      Columns: Fields header, have Name, Size and Type of the field
      Fields: Only contain value of field, and refrenced to Column

      Params: Have Name, Size and Type and also the Value
      Binds: Referenced to Params but can be dublicate the param more than one
      Why Binds
      e.g:

      select * from Employee
      where EMP_NO = ?EMP_NO or EMP_NO = ?EMP_NO

      here in example EMP_NO found 2 times in Binds but on in Param,
      when you change the value of Param['EMP_NO'].AsInteger:=10,
      it will send as 2 of params by Binds
  }
  { TmncCommand }

  TmncCommand = class(TmncLinkSession)
  private
    FColumns: TmncColumns;
    FFields: TmncFields;
    FParams: TmncParams;
    FBinds: TmncBinds;
    FParsed: Boolean;
    FPrepared: Boolean;
    FNextOnExecute: Boolean;
    procedure SetRequest(const Value: TStrings);
    procedure SetColumns(const Value: TmncColumns);
    procedure SetFields(const Value: TmncFields);
    procedure SetParams(const Value: TmncParams);
    function GetField(Index: string): TmncField;
    function GetParam(Index: string): TmncParam;
  protected
    FRequest: TStrings;
    procedure SetActive(const Value: Boolean); override;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckStarted; //Check the session is started
    function GetEOF: Boolean; virtual; abstract;
    procedure DoParse; virtual; abstract;
    procedure DoUnparse; virtual;
    procedure DoPrepare; virtual; abstract;
    procedure DoUnprepare; virtual;
    procedure DoExecute; virtual; abstract; //Here apply the Binds and execute the sql
    procedure DoNext; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoCommit; virtual; //some time we need make commit with command or session
    procedure DoRollback; virtual;
    procedure DoRequestChanged(Sender: TObject); virtual;
    function CreateFields(vColumns: TmncColumns): TmncFields; virtual; abstract;
    function CreateColumns: TmncColumns; virtual;
    function CreateParams: TmncParams; virtual; abstract;
    function CreateBinds: TmncBinds; virtual;
    property Request: TStrings read FRequest write SetRequest;
    property Binds: TmncBinds read FBinds; //for Dublicated names when pass the params when execute
  public
    constructor Create; override;
    destructor Destroy; override;
    {
      Parse: is not an api call to sql engine, it just inside fpc parsing the sql,
      for collecting params and other things prepareing the sql string.
      Do not use api call here
      NOTICE: You can not use params after Parse, params used after prepare.
    }
    procedure Parse;
    {
      Prepare: call the sql engine api, it call parse automaticly if it not called
    }
    procedure Prepare;
    function Execute: Boolean;
    procedure Close;
    function Next: Boolean;
    function EOF: Boolean;
    procedure Clear; virtual;
    procedure Commit;
    procedure Rollback;
    //Detach make Fields or Params unrelated to DB handles, you can use them in salfty in arrays
    function DetachFields: TmncFields;
    function DetachParams: TmncParams;
    function FieldIsExist(Name: string): Boolean;
    property NextOnExecute: Boolean read FNextOnExecute write FNextOnExecute default True;
    property Parsed: Boolean read FParsed;
    property Prepared: Boolean read FPrepared;
    property Columns: TmncColumns read FColumns write SetColumns;
    property Fields: TmncFields read FFields write SetFields; //Current record loaded in memory
    property Field[Index: string]: TmncField read GetField;
    property Params: TmncParams read FParams write SetParams;
    property Param[Index: string]: TmncParam read GetParam;
  end;

{ Simple classes usful for rapid implmetation }

  { TmncVariantField }

  TmncVariantField = class(TmncField)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
  end;

  { TmncVariantParam }

  TmncVariantParam = class(TmncParam)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
  end;


  { TmncVariantFields }

  TmncVariantFields = class(TmncFields)
  protected
    function CreateField(vColumn: TmncColumn): TmncField; override;
  public
  end;

  { TmncVariantParams }

  TmncVariantParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  public
  end;

function ConnectionLock: TCriticalSection;

implementation

var
  FConnectionLock: TCriticalSection = nil;

function ConnectionLock: TCriticalSection;
begin
  if FConnectionLock = nil then
    FConnectionLock := TCriticalSection.Create;
  Result := FConnectionLock;
end;

{ TmncVariantParams }

function TmncVariantParams.CreateParam: TmncParam;
begin
  Result := TmncVariantParam.Create;
end;

{ TmncVariantFields }

function TmncVariantFields.CreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncVariantField.Create(vColumn);
end;

{ TmncVariantParam }

function TmncVariantParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncVariantParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncVarianField }

function TmncVariantField.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncVariantField.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncConnection }

procedure TmncConnection.Close;
begin
  Disconnect;
end;

procedure TmncConnection.Connect;
begin
  CheckActive;
  DoConnect;
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

constructor TmncConnection.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FSessions := TmncSessions.Create(False);
end;

destructor TmncConnection.Destroy;
begin
  FSessions.Stop;
  while FSessions.Count > 0 do
    FSessions[0].Connection := nil;
  if Connected then
    Disconnect;
  FreeAndNil(FSessions);
  FreeAndNil(FParams);
  inherited;
end;

procedure TmncConnection.Disconnect;
begin
  CheckInactive;
  DoDisconnect;
  if Assigned(OnDisconnected) then
    OnDisconnected(Self);
end;

procedure TmncConnection.Open;
begin
  Connect;
end;

procedure TmncConnection.SetConnected(const Value: Boolean);
begin
  if Connected <> Value then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

procedure TmncConnection.SetParams(const AValue: TStrings);
begin
  FParams.Assign(AValue);
end;

procedure TmncConnection.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TmncConnection.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TmncConnection.CheckActive;
begin
  if Connected then
    raise EmncException.Create('Connection already connected');
end;

procedure TmncConnection.CheckInactive;
begin
  if not Connected then
    raise EmncException.Create('Connection not connected');
end;

procedure TmncConnection.DoInit;
begin
end;

procedure TmncConnection.Init;
begin
  if not FIsInit then
  begin
    DoInit;
    FIsInit := True;
  end;
end;

{ TmncCommand }

procedure TmncCommand.Clear;
begin
  DoUnparse;
  FPrepared := False;
  FRequest.Clear;
  if FParams <> nil then
    FParams.Clear;
  FBinds.Clear;
end;

destructor TmncCommand.Destroy;
begin
  Active := False;
  Session := nil;//already in Linked but must be sure before free other objects
  FreeAndNil(FRequest);
  FreeAndNil(FFields);
  FreeAndNil(FBinds);
  FreeAndNil(FParams);
  FreeAndNil(FColumns);
  inherited;
end;

procedure TmncCommand.Parse;
begin
  DoParse;
  FParsed := True;
end;

procedure TmncCommand.DoRequestChanged(Sender: TObject);
begin
  if Active then
    Close;
end;

function TmncCommand.CreateColumns: TmncColumns;
begin
  Result := TmncColumns.Create;
end;

function TmncCommand.CreateBinds: TmncBinds;
begin
  Result := TmncBinds.Create;
end;

constructor TmncCommand.Create;
begin
  inherited;
  FRequest := TStringList.Create;
  (FRequest as TStringList).OnChange := DoRequestChanged;

  FColumns := CreateColumns;
  FParams := CreateParams;
  FBinds := CreateBinds;
  FNextOnExecute := True;
end;

function TmncCommand.EOF: Boolean;
begin
  Result := GetEOF;
end;

function TmncCommand.Execute: Boolean;
begin
  if not FPrepared then
    Prepare;
  DoExecute;
  if FNextOnExecute then
    Result := Next
  else
    Result := not EOF;
end;

function TmncCommand.FieldIsExist(Name: string): Boolean;
begin
  Result := Fields.Find(Name) <> nil;
end;

function TmncCommand.GetField(Index: string): TmncField;
begin
  if not Prepared then
    Prepare;
  if Fields <> nil then
    Result := Fields.Field[Index]
  else
    raise EmncException.Create('Current record not found');
end;

function TmncCommand.GetParam(Index: string): TmncParam;
begin
  if not Prepared then
    Prepare;
  if Params <> nil then
    Result := Params.Param[Index]
  else
    raise EmncException.Create('Params is nil');
end;

procedure TmncCommand.CheckActive;
begin
  if not Active then
    raise EmncException.Create('Command is not active/opened');
end;

procedure TmncCommand.CheckInactive;
begin
  if Active then
    raise EmncException.Create('Command is active/opened');
end;

procedure TmncCommand.CheckStarted;
begin
  if (Session = nil) then
    raise EmncException.Create('Session not assigned');
  if (sbhStrict in Session.Behaviors) and not Session.Active then
    raise EmncException.Create('Session is not active/started');
end;

procedure TmncCommand.DoUnparse;
begin
  FParsed := False;
end;

procedure TmncCommand.DoUnprepare;
begin
end;

procedure TmncCommand.DoCommit;
begin
end;

procedure TmncCommand.DoRollback;
begin
end;

function TmncCommand.Next: Boolean;
begin
  DoNext;
  Result := not EOF;
end;

procedure TmncCommand.Prepare;
begin
  if not FParsed then
    Parse;
  CheckStarted;
  DoPrepare;
  FPrepared := True;
end;

function TmncCommand.DetachFields: TmncFields;
begin
  FFields.Detach;
  Result := FFields;
  FFields := nil;
end;

function TmncCommand.DetachParams: TmncParams;
begin
  Result := FParams;
  FParams := nil;
end;

procedure TmncCommand.Rollback;
begin
  if Active then
    Close;
  DoRollback;
end;

procedure TmncCommand.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
      Execute
    else
      Close;
  end;
end;

procedure TmncCommand.SetFields(const Value: TmncFields);
begin
  if FFields <> Value then
  begin
    FreeAndNil(FFields);
    FFields := Value;
  end;
end;

procedure TmncCommand.SetColumns(const Value: TmncColumns);
begin
  FColumns.Assign(Value);
end;

procedure TmncCommand.SetParams(const Value: TmncParams);
begin
  if FParams <> Value then
  begin
    FreeAndNil(FParams);
    FParams := Value;
  end;
end;

procedure TmncCommand.SetRequest(const Value: TStrings);
begin
  FRequest.Assign(Value);
  //TODO OnRequestChanged;
end;

procedure TmncLinkSession.SetSession(AValue: TmncSession);
begin
  inherited Link := AValue;
end;

function TmncLinkSession.GetSession: TmncSession;
begin
  Result := Link as TmncSession;
end;

constructor TmncLinkSession.CreateBy(vSession: TmncSession);
begin
  Create;
  Session := vSession;//Session not FSession
end;

procedure TmncCommand.Commit;
begin
  if Active then
    Close;
  DoCommit;
end;

procedure TmncCommand.Close;
begin
  if not Active then
    raise EmncException.Create('Command already Closed');
  DoUnprepare;
  DoClose;
  DoUnparse;
  FPrepared := False;
end;

{ TmncSession }

procedure TmncSession.Commit(Retaining: Boolean = False);
begin
  InternalStop(sdaCommit, Retaining);
end;

constructor TmncSession.Create(vConnection: TmncConnection);
var
  aBehaviors: TmncSessionBehaviors;
begin
  aBehaviors := [];
  if ccStrict in vConnection.Model.Capabilities then
    aBehaviors := aBehaviors + [sbhStrict];
  if ccTransaction in vConnection.Model.Capabilities then
  begin
{    if ccMultiConnection in vConnection.Model.Capabilities then //deprecated
      aBehaviors := aBehaviors + [sbhMultiple, sbhIndependent]
    else }if ccMultiTransaction in vConnection.Model.Capabilities then
      aBehaviors := aBehaviors + [sbhMultiple]
    else
      aBehaviors := aBehaviors + [sbhEmulate]
  end;
  Create(vConnection, aBehaviors);
end;

constructor TmncSession.Create(vConnection: TmncConnection; vBehaviors: TmncSessionBehaviors);
begin
  inherited Create;
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  Connection := vConnection;
  FBehaviors := vBehaviors;
  if Connection.AutoStart then
    Start;
end;

destructor TmncSession.Destroy;
begin
  Stop;
  Links.Unlink;
  Connection := nil;
  FreeAndNil(FParams);
  inherited;
end;

function TmncSession.GetActive: Boolean;
begin
  Result := FStartCount > 0;
end;

procedure TmncSession.CheckActive;
begin
  if not Active then
    raise EmncException.Create('Session is not active/opened');
end;

procedure TmncSession.CheckInactive;
begin
  if Active then
    raise EmncException.Create('Session is active/opened');
end;

procedure TmncSession.DoInit;
begin
end;

procedure TmncSession.Init;
begin
  if not FIsInit then
  begin
    DoInit;
    FIsInit := True;
  end;
end;

procedure TmncSession.InternalStop(How: TmncSessionAction; Retaining: Boolean);
begin
  if not Active then
    raise EmncException.Create('Oops you have not started yet!');

  if sbhEmulate in Behaviors then
  begin
    if not Retaining then //Nothing to do if Retaingig
    begin
      if Connection.FStartCount = 0 then
        raise EmncException.Create('Connection not started yet!');
      Dec(Connection.FStartCount);
      if Connection.FStartCount = 0 then
        DoStop(How, Retaining);
    end;
  end
  else if sbhMultiple in Behaviors then
      DoStop(How, Retaining);
  Dec(FStartCount);
end;

procedure TmncSession.Rollback(Retaining: Boolean);
begin
  InternalStop(sdaRollback, Retaining);
end;

procedure TmncSession.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TmncSession.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TmncSession.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TmncSession.SetConnection(const Value: TmncConnection);
begin
  if FConnection <> Value then
  begin
    if FConnection <> nil then
      FConnection.Sessions.Remove(Self);
    FConnection := Value;
    if FConnection <> nil then
      FConnection.Sessions.Add(Self);
  end;
end;

procedure TmncSession.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TmncSession.Start;
begin
  if Active then
    raise EmncException.Create('Session is already active.');
  Connection.Init;
  Init;
  if sbhEmulate in Behaviors then
  begin
     if Connection.FStartCount = 0 then
       DoStart;
      Inc(Connection.FStartCount);
  end
  else if sbhMultiple in Behaviors then
    DoStart;
  Inc(FStartCount);
end;

procedure TmncSession.Stop;
begin
  Links.Close;
  if Active then
  begin
    if Action = sdaCommit then
      Commit
    else
      Rollback;
  end;
end;

function TmncFields.Add(Column: TmncColumn; Value: Variant): TmncField;
begin
  Result := CreateField(Column) as TmncField;
  Result.Value := Value;
  Result.Column := Column;
  inherited Add(Result);
end;

function TmncFields.Add(Index: Integer; Value: Variant): TmncField;
begin
  Result := Add(Columns[Index], Value);
end;

function TmncFields.FieldByName(vName: string): TmncField;
begin
  Result := FindField(vName);
  if Result = nil then
    raise EmncException.Create('Field "' + vName + '" not found');
end;

function TmncFields.Add(Column: TmncColumn): TmncField;
begin
  Result := CreateField(Column) as TmncField;
  Result.Column:= Column;
  inherited Add(Result);
end;

constructor TmncFields.Create(vColumns: TmncColumns);
begin
  inherited Create;
  FColumns := vColumns;
  if FColumns = nil then
    raise EmncException.Create('vColumns must be not nil');
end;

function TmncFields.FindField(vName: string): TmncField;
begin
  Result := Find(vName) as TmncField;
end;

function TmncFields.GetField(Index: string): TmncField;
begin
  Result := FieldByName(Index);
end;

function TmncFields.GetItem(Index: Integer): TmncField;
begin
  Result := (inherited Items[Index]) as TmncField;
end;

function TmncFields.Find(vName: string): TmncItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Column = nil then
      raise EmncException.Create('Field item not have Column');
    if SameText(vName, Items[i].Column.Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

{ TmncColumns }

function TmncColumns.Add(vIndex: Integer; vName: string; vType: TmncDataType; FieldClass: TmncColumnClass): TmncColumn;
begin
  if FieldClass = nil then
    FieldClass := TmncColumn;
  Result := FieldClass.Create;
  Result.Index := vIndex;
  Result.Name := vName;
  Result.FDataType := vType;
  inherited Add(Result);
end;

function TmncColumns.Add(vName: string; vType: TmncDataType): TmncColumn;
begin
  Result := Add(Count, vName, vType);
end;

function TmncColumns.Find(vName: string): TmncItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncColumns.GetItem(Index: Integer): TmncColumn;
begin
  Result := inherited Items[Index] as TmncColumn;
end;

{ TmncField }

constructor TmncField.Create(vColumn: TmncColumn);
begin
  inherited Create;
  FColumn := vColumn;
end;

function TmncField.GetName: string;
begin
  if Column = nil then
    Result := ''
  else
    Result := Column.Name;
end;

constructor TmncParams.Create;
begin
  inherited;
end;

function TmncParams.Add(Name: string): TmncParam;
begin
  Result := CreateParam as TmncParam;
  Result.Name := Name;
  inherited Add(Result);
end;

function TmncParams.Found(Name: string): TmncParam;
begin
  Result := FindParam(Name) as TmncParam;
  if Result = nil then
    Result := Add(Name);
end;

procedure TmncCustomParams.Clear;
begin
  inherited;
end;

function TmncCustomParams.FindParam(vName: string): TmncParam;
begin
  Result := Find(vName) as TmncParam;
end;

function TmncCustomParams.ParamByName(vName: string): TmncParam;
begin
  Result := FindParam(vName);
  if Result = nil then
    raise EmncException.Create('Param ' + vName + ' not found');
end;

function TmncCustomParams.Find(vName: string): TmncItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i] as TmncParam;
      break;
    end;
  end;
end;

function TmncCustomParams.GetParam(Index: string): TmncParam;
begin
  Result := ParamByName(Index);
end;

function TmncCustomParams.GetItem(Index: Integer): TmncParam;
begin
  Result := inherited Items[Index] as TmncParam;
end;

{ TmncBinds }

function TmncBinds.GetItem(Index: Integer): TmncBind;
begin
  Result := (inherited Items[Index]) as TmncBind;
end;

function TmncBinds.CreateBind: TmncBind;
begin
  Result := TmncBind.Create;
end;

constructor TmncBinds.Create;
begin
  inherited Create(True);
end;

function TmncBinds.Add(ABind: TmncBind): Integer;
begin
  Result := inherited Add(ABind);
end;

function TmncBinds.Add(AParam: TmncParam): Integer;
var
  aItem: TmncBind;
begin
  aItem := CreateBind;
  aItem.Param := AParam;
  Result := Add(aItem);
end;

{ TCustomField }

{ TmncColumn }

procedure TmncColumn.SetSize(AValue: Int64);
begin
  if FSize =AValue then Exit;
  FSize :=AValue;
end;

function TmncColumn.GetValue: Variant;
begin
  Result := null;
  raise EmncException.Create('Field have no value, You must not use it, try use Fields!') {$ifdef fpc}at get_caller_addr(get_frame){$endif};
end;

procedure TmncColumn.SetValue(const AValue: Variant);
begin
  raise EmncException.Create('Field have no value, You must not use it, try use Fields!');
end;

{ TmncParam }

destructor TmncParam.Destroy;
begin
  inherited;
end;

constructor TmncParam.Create;
begin
  inherited;
end;

{ TmncSessions }

procedure TmncSessions.Stop;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Stop;
  end;
end;

function TmncSessions.GetItem(Index: Integer): TmncSession;
begin
  Result := inherited Items[Index] as TmncSession;
end;

procedure TmncSessions.SetItem(Index: Integer; const Value: TmncSession);
begin
  inherited Items[Index] := Value;
end;

{ TmncCustomFields }

function TmncCustomFields.GetItem(Index: Integer): TmncCustomField;
begin
  Result := (inherited Items[Index]) as TmncCustomField;
end;

function TmncRecord.GetItemByName(Index: string): TmncCustomField;
begin
  Result := ItemByName(Index) as TmncCustomField;
end;

function TmncCustomFields.GetValue(Index: string): Variant;
begin
  Result := ItemByName(Index).Value;
end;

procedure TmncCustomFields.SetValue(Index: string; const Value: Variant);
begin
  ItemByName(Index).Value := Value;
end;

procedure TmncCustomFields.Detach;
begin
end;

{ TmncItem }

function TmncItem.GetAsText: string;
begin
  if (IsBlob) and (BlobType = blobText) then
    Result := AsHex
  else
    Result := inherited GetAsText;
end;

procedure TmncItem.SetAsText(const AValue: string);
begin
  if (IsBlob) and (BlobType = blobText) then
    AsHex := AValue
  else
    inherited SetAsText(AValue);
end;

{ TmncItems }

function TmncItems.GetItem(Index: Integer): TmncItem;
begin
  Result := (inherited Items[Index]) as TmncItem;
end;

function TmncItems.Add(AColumn: TmncItem): Integer;
begin
  Result := inherited Add(AColumn);
end;

function TmncItems.ItemByName(vName: string): TmncItem;
begin
  Result := Find(vName);
  if Result = nil then
    raise Exception.Create('Field "' + vName + '" not found');
end;

function TmncItems.IsExists(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

procedure TmncItems.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Clear;
  end;
end;

initialization
finalization
  FreeAndNil(FConnectionLock);
end.
