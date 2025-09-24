unit mncConnections;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamad <belal, hamad>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

{
  Columns Fields and Params
  Columns has info about Fields, like Name, Size, Type etc...,
  while Fields have only the values, the idea, is you can use one Columns with multiple records,
  record here is the Fields with more info, maybe include columns inside

  Params not need that technique.

  Column have metadata, name, datatype
  Fields have only Values, no name, no datatype
  Param have metadata and values

  TODO:
    use Dictonary hash list for Fields
    remove Retain
    Can we assign params before prepare?
    --detach to record (clone)--
    Nested Fields (for REST)
      CMD.Fields['Name'].AsString
      CMD.Fields['Data', 'Name'].AsString
      CMD.Fields['Data'].AsFields['Name'].AsString
}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs, SyncObjs, Types,
  Generics.Collections,
  mnTypes, mnFields, mncCommons;

type
  TmncTransaction = class;
  TmncCommand = class;

  { TmncLinkTransaction }

  TmncLinkTransaction = class(TmncLinkObject)
  private
    function GetTransaction: TmncTransaction;
    procedure SetTransaction(AValue: TmncTransaction);
  protected
  public
    constructor CreateBy(vTransaction: TmncTransaction);
    property Transaction: TmncTransaction read GetTransaction write SetTransaction;//alias for FLink in base class
  end;

  { TmncTransactions }

  TmncTransactions = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncTransaction;
    procedure SetItem(Index: Integer; const Value: TmncTransaction);
  protected
    procedure Stop;
  public
    function IsAnyActive: Boolean;
    property Items[Index: Integer]: TmncTransaction read GetItem write SetItem; default;
  end;

  TmncCapability = (
    ccDB, //It is Database engine not just transfer data object
    ccAlias, //Allow alias names in resrouce like Firebird
    ccPath, //Allow path as file name in resrouce, like Sqlite and Firebird
    ccCreate, //Allow create database
    ccDrop, //Allow drop database
    ccSQL, //It is SQL engine, you know CSV or Paradox is not, we have no plan to support Paradox
    ccNetwork, //Can connect over network, like firebird, postgre, mysql but not sqlite
    ccBrowse, //Server can browse databases, mysql and postgre
    ccStrict, //Without it: it is no need to call Start and Stop (Commit/Rollback) this DB automatically do it (pg/SQLite allow it)
              //But that not mean you can call Start more than one or Stop without started it.
    ccTransaction, //Support transaction, most of them Like Firebird, SQLite, PG
    ccMultiTransaction //Like Firebird can have multiple transaction for one connection, while PG and SQLite has not, see also sbhMultiple
{  nop i hate it
    ccMultiConnection //Some time we need to make for every Transaction a new connection
        //(PG and SQLite), it slowing app, i will not use it. see sbhIndependent
        }
  );
  //if you put ccTransaction and not put ccMultiTransaction it will emulate it.

  TmncCapabilities = set of TmncCapability;

  TmncState = (
      cstCreated  //When use autocreate and it is created
    );

  TmncMode = (
      mcmStrict,
      mcmAutoStart
    );

  TmncModes = set of TmncMode;

  TmncStates = set of TmncState;

  TmncServerInfo = record
    Host: string;
    Port: string;
    UserName: string;
    Password: string;
    Role: string;
  end;

  { TmncConnection }

  TmncConnection = class(TmncObject)
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FParamsChanged: Boolean;
    FServerInfo: TmncServerInfo;
    FParams: TStrings;
    FAutoStart: Boolean;
    FResource: string;
    FTransactions: TmncTransactions;
    FStartCount: Integer;
    FStates: TmncStates;
    FMode: TmncModes;
    procedure SetConnected(const Value: Boolean);
    procedure SetParams(const AValue: TStrings);
    procedure ParamsChanging(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
    procedure SetServerInfo(AValue: TmncServerInfo);
  protected
    procedure CheckActive;
    procedure CheckInactive;
    procedure DoConnect; virtual; abstract;
    procedure DoConnected; virtual;
    procedure DoDisconnect; virtual; abstract;
    procedure DoDisconnected; virtual;
    function GetConnected: Boolean; virtual; abstract;
    procedure DoInit; virtual;
    procedure Prepare; virtual;
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
    function _AddRef : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    constructor Create; virtual;
    destructor Destroy; override;
//    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    class function Capabilities: TmncCapabilities; virtual; abstract;
    class function EngineName: string; virtual; abstract;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disonnect;

    procedure SetState(aState: TmncState);
    property Transactions: TmncTransactions read FTransactions;
    property AutoStart: Boolean read FAutoStart write FAutoStart; //AutoStart the Transaction when created
    property Mode: TmncModes read FMode write FMode;
    function IsStrict: Boolean; virtual;
    property Connected: Boolean read GetConnected write SetConnected;
    property Active: Boolean read GetConnected write SetConnected;
    property States: TmncStates read FStates;

    property Resource: string read FResource write FResource; //can be a Database name or Alias or service name etc...
    property Host: string read FServerInfo.Host write FServerInfo.Host;
    property Port: string read FServerInfo.Port write FServerInfo.Port;
    property UserName: string read FServerInfo.UserName write FServerInfo.UserName;
    property Password: string read FServerInfo.Password write FServerInfo.Password;
    property Role: string read FServerInfo.Role write FServerInfo.Role;
    property ServerInfo: TmncServerInfo read FServerInfo write SetServerInfo;
    property StartCount: Integer read FStartCount write FStartCount;

    property Params: TStrings read FParams write SetParams;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

  TmncConnectionClass = class of TmncConnection;

  TmncTransactionBehavior = (
    sbhStrict, //Without it, it no need to call Start and Stop (Commit/Rollback) this DB automatically do it (pg/SQLite allow it)
    sbhMultiple, //Multiple Transactions works simultaneously, Every Transaction have transacion like as Firebird
    sbhEmulate //or Virtual, a Single transaction(main connection) many start last stop, maybe PG/SQLite
  );
  TmncTransactionBehaviors = set of TmncTransactionBehavior;

  //Transaction it is branch/clone of Connection but usefull for take a special params, it is like Transactions.
  TmncTransactionAction = (
    sdaCommit,
    sdaRollback
  );

  { TmncTransaction }

  TmncTransaction = class(TmncLinksObject)
  private
    FParams: TStrings;
    FConnection: TmncConnection;
    FStartCount: Integer;
    FAction: TmncTransactionAction;
    FParamsChanged: Boolean;
    FIsInit: Boolean;
    FCurrentAction: TmncTransactionAction;
    procedure SetParams(const Value: TStrings);
    procedure SetConnection(const Value: TmncConnection);
    procedure SetActive(const Value: Boolean);
    procedure ParamsChanging(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
  protected
    FBehaviors: TmncTransactionBehaviors;
    function GetActive: Boolean; virtual;
    procedure CheckActive;
    procedure CheckInactive;
    procedure DoInit; virtual;
    procedure DoStart; virtual; abstract;
    procedure DoStop(How: TmncTransactionAction; Retaining: Boolean); virtual; abstract;
    procedure Init;
    procedure InternalStart; virtual;
    procedure InternalStop(How: TmncTransactionAction; Retaining: Boolean = False); virtual;
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
    property StartCount: Integer read FStartCount;
  public
    constructor Create(vConnection: TmncConnection); overload; virtual;
    constructor Create(vConnection: TmncConnection; vBehaviors: TmncTransactionBehaviors); overload; virtual;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    //* Retaining: keep it active
    procedure Commit(Retaining: Boolean = False); virtual;
    procedure Rollback(Retaining: Boolean = False); virtual;
    procedure CommitRetaining;
    procedure RollbackRetaining;
    property Behaviors: TmncTransactionBehaviors read FBehaviors;
    property Action: TmncTransactionAction read FAction write FAction; //todo zaher i dont like the name
    property Connection: TmncConnection read FConnection write SetConnection;
    property Active: Boolean read GetActive write SetActive;
    property Params: TStrings read FParams write SetParams;
  end;

{
  TmncItem base class for Column and Field and Param
  TmnColumn have Name but have no Value
  TmnField have no Name but have Value and reference to Column for name and type
  TmnParam have Name and have Value
}

  { TmncItem }

  TmncItem = class abstract(TmnCustomField)
  private
    FDataType: TmnDataType;
    FBlobType: TmnBlobType;
  protected
    function GetAsText: string; override;
    procedure SetAsText(const AValue: string); override;
    property BlobType: TmnBlobType read FBlobType write FBlobType default blobBinary;
    property DataType: TmnDataType read FDataType write FDataType default dtUnknown;
    procedure Created; override;
  public
    function IsNumber: Boolean;
    function GetName: string; virtual; abstract;

  published
  end;

  { TmncItems }

  TmncItems<T: TmncItem> = class abstract(TmnCustomFields<T>)
  private
    FDic: TDictionary<string, T>;
  protected
    function Find(const vName: string): T;
    function ItemByName(const vName: string): T;
    {$ifdef FPC}
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    {$else}
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
    {$endif}
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TmnDataOption = (doRequired, doNullable);
  TmnDataOptions = set of TmnDataOption;

  { TmncColumn }

  TmncColumn = class(TmncItem)
  private
    FDecimals: Integer;
    FFullName: string;
    FName: string;
    FIndex: Integer;
    FMaxSize: Integer;
    FOptions: TmnDataOptions;
    FMetaType: string;
    FScale: SmallInt;
    FSize: Int64;
    FSubType: TmnSubType;
  protected
    procedure SetIsNull(const AValue: Boolean); override;
    function GetIsNull: Boolean; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure SetSize(AValue: Int64); virtual;
    procedure SetType(vType: TmnDataType);
    procedure SetDecimals(AValue: Integer);
   public
    function GetName: string; override;
  published
    property Index: Integer read FIndex write FIndex;
    property FullName: string read FFullName write FFullName; //TODO
    property Name: string read FName write FName;
    property DataType;
    property MetaType: string read FMetaType write FMetaType;
    //Size, in sqlite every value have own length not depends on the Meta
    property Size: Int64 read FSize write SetSize; //TODO: I am thinking to move it to TmncItem
    property Scale: SmallInt read FScale write FScale;
    property Decimals: Integer read FDecimals write SetDecimals;
    property Options: TmnDataOptions read FOptions write FOptions default [];
    //property IsBlob;
    property SubType: TmnSubType read FSubType write FSubType;
    property MaxSize: Integer read FMaxSize write FMaxSize;
  end;

  TmncColumnClass = class of TmncColumn;

  { TmncColumns }

  TmncColumns = class(TmncItems<TmncColumn>)
  private
  protected
  public
    procedure Assign(Source: TmncColumns); virtual;
    function Add(vIndex: Integer; vName: string; vType: TmnDataType; FieldClass: TmncColumnClass = nil): TmncColumn; overload;
    function Add(vName: string; vType: TmnDataType): TmncColumn; overload;
    function Add(vColumn: TmncColumn): Integer; overload;
    procedure Clone(FromColumns: TmncColumns; AsDataType: TmnDataType); overload;
    procedure Clone(FromColumns: TmncColumns); overload;
  end;

  { TmncCustomField }

  TmncCustomField = class abstract(TmncItem) //Param and Field
  private
  protected
  public

    property AsUtf8String;
  published
    //property IsBlob;
    //property BlobType;
    property IsNull;
    property Value;
    property AsVariant;
    property AsString;
    {$ifndef NEXTGEN}
    property AsAnsiString;
    {$endif}
    property AsTrimString;
    property AsNullString;
    property AsInteger;
    property AsInt64;
    property AsForeign;
    property AsBoolean;
    property AsCurrency;
    property AsDate;
    property AsTime;
    property AsDateTime;
    property AsText; //binary text blob convert to hex
    property AsHex;
    property AsDouble;
    property AsBytes;
  end;

  { TmncCustomFields }

  TmncCustomFields<T: TmncCustomField> = class abstract(TmncItems<T>)
  private
  protected
    //Called before release it, good to deattach the handles
  public
  end;

  { TmncField }

  TmncField = class(TmncCustomField)
  private
    FColumn: TmncColumn;
  protected
  public
    constructor Create(vColumn: TmncColumn); virtual;
    function GetName: string; override;
  published
    property Column: TmncColumn read FColumn write FColumn;
  end;

  { TmncFields }

  TmncFields = class(TmncCustomFields<TmncField>)
  private
    FColumns: TmncColumns;
    FRowID: Int64;
    function GetItem(Index: Integer): TmncField;
    function GetField(const Index: string): TmncField;
    function GetValues(const Index: string): Variant;
    procedure SetValues(const Index: string; const AValue: Variant);
    //function SetValue(const Index: string; const AValue: Variant): TmncField;
  protected
    function DoCreateField(vColumn: TmncColumn): TmncField; virtual; abstract;
  public
    constructor Create(vColumns: TmncColumns); virtual;
    function CreateField(vColumn: TmncColumn): TmncField; reintroduce; overload;
    function CreateField(vIndex: Integer): TmncField; reintroduce; overload;
    function IsExist(const vName: string): Boolean;
    function FieldByName(const vName: string): TmncField;
    function Add(Column: TmncColumn): TmncField; overload;
    function Add(Column: TmncColumn; Value: Variant): TmncField; overload;
    function Add(Index: Integer; Value: Variant): TmncField; overload;
    property Columns: TmncColumns read FColumns;
    property Field[const Index: string]: TmncField read GetField; default;
    {$ifndef FPC}
    property Field[Index: Integer]: TmncField read GetItem; default;
    {$endif}
    property Items[Index: Integer]: TmncField read GetItem;
    property Values[const Index: string]: Variant read GetValues write SetValues;
    property RowID: Int64 read FRowID write FRowID default 0; //most of SQL engines have this value
  end;

  TmncRecord = class(TmncFields) //*TODO: use it insteadof TmncFields
  private
  public
  end;

  { TmncParam }

  TmncParam = class(TmncCustomField)
  private
    FName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetName: string; override;
    property Name: string read FName write FName;

    property DataType;
  end;

  { TmncCustomParams }

  TmncCustomParams = class(TmncCustomFields<TmncParam>)
  protected
    function GetParam(const Index: string): TmncParam;
    function GetItem(Index: Integer): TmncParam;
  public
    procedure Clear; override;
    function ParamByName(const vName: string): TmncParam;
    property Items[Index: Integer]: TmncParam read GetItem;
    property Param[const Index: string]: TmncParam read GetParam; default;
  end;

  { TmncParams }

  TmncParams = class(TmncCustomParams)
  private
  protected
    function CreateParam: TmncParam; virtual; abstract;
  public
    function Add(Name: string): TmncParam;
    //Add it if not exists
    function Require(const Name: string): TmncParam;
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

  //TODO not yet
  TmncCommandOption = (
    cmdReplaceParams, //replace param in sql text instead pass it to SQL engine
    cmoTruncate, //Truncate the string to fit into field size, not recomended if you are strict, you will lose data
    cmoCorrectDate //Correct DateTime fields to be compatiple with SQL and Pascal
  );

  TmncCommandOptions = set of TmncCommandOption;

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

  TmncCommand = class(TmncLinkTransaction)
  private
    FColumns: TmncColumns;
    FFields: TmncFields;
    FOptions: TmncCommandOptions;
    FParams: TmncParams;
    FBinds: TmncBinds;
    FParsed: Boolean;
    FPrepared: Boolean;
    FIndex: Int64;
    FID: Int64;
    FDone: Boolean;
    FReady: Boolean;
    FExecuted: Boolean;
    function GetValues(const Index: string): Variant;
    procedure SetRequest(const Value: TStrings);
    procedure SetColumns(const Value: TmncColumns);
    procedure SetFields(const Value: TmncFields);
    procedure SetParams(const Value: TmncParams);
    function GetField(const Index: string): TmncField;
    function GetParam(const Index: string): TmncParam;
    function GetIndex: Int64;
  protected
    type

      { TmncCommandEnumerator }

      TmncCommandEnumerator = class(TObject)
      private
        FCommand: TmncCommand;
      public
        constructor Create(ACommand: TmncCommand);
        function GetCurrent: TmncFields; inline;
        function MoveNext: Boolean; inline;
        property Current: TmncFields read GetCurrent;
      end;

  protected
    FRequest: TStrings;
    procedure SetActive(const Value: Boolean); override;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckTransaction; // Check the Transaction is started if need transaction
    procedure DoParse; virtual; abstract;
    procedure DoPrepare; virtual; abstract;
    procedure DoUnprepare; virtual;
    procedure DoExecute; virtual; abstract; //Here apply the Binds and execute the sql
    procedure DoNext; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoRequestChanged(Sender: TObject); virtual;
    function CreateFields(vColumns: TmncColumns): TmncFields; virtual; abstract;
    function CreateColumns: TmncColumns; virtual;
    function CreateParams: TmncParams; virtual; abstract;
    function CreateBinds: TmncBinds; virtual;
    property Request: TStrings read FRequest write SetRequest;
    function InternalExecute(vNext: Boolean): Boolean; virtual;
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
    procedure Prepare; overload;
    //* Return true of there is data to read
    function Execute: Boolean; overload;
    function Execute(vNext: Boolean): Boolean; overload;
    function Next: Boolean;
    //function Step: Boolean; deprecated 'Use Fetch';
    function Fetch: Boolean; //Execute and Next for while loop
    procedure Close;
    procedure Clear; virtual;
    //procedure Reset; virtual; //Reset and reset stamemnt like Done or Ready called in Execute before DoExecute and after Prepare
    property Done: Boolean read FDone; //EOF :)
    property Ready: Boolean read FReady; //BOF :)

    function GetEnumerator: TmncCommandEnumerator; inline;

    procedure HitDone; overload;   //Make it FDone True
    procedure HitDone(vCondition: Boolean); overload;
    procedure HitUnready; //Make it FReady False

    property Parsed: Boolean read FParsed;
    property Prepared: Boolean read FPrepared;
    property Columns: TmncColumns read FColumns write SetColumns;
    property Fields: TmncFields read FFields write SetFields; //Current record loaded in memory, it is nil sometime if no data, do not access it if no data exists
    property Binds: TmncBinds read FBinds; //for Dublicated names when pass the params when execute select * from t1 where f1 = ?p1 and f2 = ?p1 and f3=p2
    property Field[const Index: string]: TmncField read GetField;
    property Params: TmncParams read FParams write SetParams;
    property Param[const Index: string]: TmncParam read GetParam;
    property Values[const Index: string]: Variant read GetValues;
    property Options: TmncCommandOptions read FOptions write FOptions;
    property Index: Int64 read GetIndex; //* RowIndex
    property ID: Int64 read FID write FID; //*TODO: deprecated;
    property Tag: Int64 read FID write FID; //*TODO: deprecated;
  end;

{ Simple classes usful for rapid implmetation }

  { TmncVariantField }

  TmncVariantField = class(TmncField)
  private
    FValue: Variant;
  protected
    procedure SetIsNull(const AValue: Boolean); override;
    function GetIsNull: Boolean; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
  end;

  { TmncVariantParam }

  TmncVariantParam = class(TmncParam)
  private
    FValue: Variant;
  protected
    procedure SetIsNull(const AValue: Boolean); override;
    function GetIsNull: Boolean; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
  end;


  { TmncVariantFields }

  TmncVariantFields = class(TmncFields)
  protected
    function DoCreateField(vColumn: TmncColumn): TmncField; override;
  public
  end;

  { TmncVariantParams }

  TmncVariantParams = class(TmncParams)
  protected
    function CreateParam: TmncParam; override;
  public
  end;

implementation


{ TmncVariantParams }

function TmncVariantParams.CreateParam: TmncParam;
begin
  Result := TmncVariantParam.Create;
end;

{ TmncVariantFields }

function TmncVariantFields.DoCreateField(vColumn: TmncColumn): TmncField;
begin
  Result := TmncVariantField.Create(vColumn);
end;

{ TmncVariantParam }

procedure TmncVariantParam.SetIsNull(const AValue: Boolean);
begin
  Value := Null;
end;

function TmncVariantParam.GetIsNull: Boolean;
begin
  Result := VarIsNull(Value);
end;

function TmncVariantParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncVariantParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TmncVarianField }

procedure TmncVariantField.SetIsNull(const AValue: Boolean);
begin
  Value := Null;
end;

function TmncVariantField.GetIsNull: Boolean;
begin
  Result := VarIsNull(Value);
end;

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

procedure TmncConnection.SetState(aState: TmncState);
begin
  FStates := FStates + [aState];
end;

procedure TmncConnection.Connect;
begin
  Prepare;
  CheckInactive;
  DoConnect;
  if Connected then
  begin
    DoConnected;
    if Assigned(OnConnected) then
      OnConnected(Self);
  end;
end;

constructor TmncConnection.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FTransactions := TmncTransactions.Create(False);
  DoInit;
end;

destructor TmncConnection.Destroy;
begin
  FTransactions.Stop;
  while FTransactions.Count > 0 do
    FTransactions[0].Connection := nil;
  if Connected then
    Disconnect;
  FreeAndNil(FTransactions);
  FreeAndNil(FParams);
  inherited;
end;

procedure TmncConnection.Disconnect;
begin
  CheckActive;
  DoDisconnect;
  if not Connected then
  begin
    DoDisconnected;
    if Assigned(OnDisconnected) then
      OnDisconnected(Self);
  end;
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

procedure TmncConnection.SetServerInfo(AValue: TmncServerInfo);
begin
  CheckInactive;
  FServerInfo :=AValue;
end;

procedure TmncConnection.CheckActive;
begin
  if not Connected then
    raise EmncException.Create('Connection is not connected');
end;

procedure TmncConnection.CheckInactive;
begin
  if Connected then
    raise EmncException.Create('Connection is connected');
end;

procedure TmncConnection.DoConnected;
begin
end;

procedure TmncConnection.DoDisconnected;
begin
end;

procedure TmncConnection.DoInit;
begin
end;

function TmncConnection.IsStrict: Boolean;
begin
  Result := (mcmStrict in Mode) or (ccStrict in Capabilities);
end;

procedure TmncConnection.Prepare;
begin
end;
(*
function TmncConnection.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj): longint;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;
*)
function TmncConnection._AddRef: longint;
begin
  Result := 0;
end;

function TmncConnection._Release: longint;
begin
  Result := 0;
end;

{ TmncCommand }

procedure TmncCommand.Clear;
begin
  FPrepared := False;
  FRequest.Clear;
  if FParams <> nil then
    FParams.Clear;
  FBinds.Clear;
end;

function TmncCommand.GetEnumerator: TmncCommandEnumerator;
begin
  Result := TmncCommandEnumerator.Create(Self);
end;

destructor TmncCommand.Destroy;
begin
  Active := False;
  Transaction := nil; //already in Linked but must be sure before free other objects
  FreeAndNil(FRequest);
  FreeAndNil(FParams);
  FreeAndNil(FBinds); //If we r freeing binds before Params it crash in FB when freeing command
  FreeAndNil(FFields);
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
  FParsed := False;
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
  inherited Create;
  FRequest := TStringList.Create;
  (FRequest as TStringList).OnChange := DoRequestChanged;

  FColumns := CreateColumns;
  FParams := CreateParams;
  FBinds := CreateBinds;
  FReady := True;
  FDone := True;
  FExecuted := False;
end;

function TmncCommand.InternalExecute(vNext: Boolean): Boolean;
begin
  if not FPrepared then
    Prepare;
  FReady := True;
  FDone := False;
  FExecuted := True;
  DoExecute;
  if vNext and not Done then //TODO Check it do we need not Done
  begin
    FIndex := -1;
    Result := Next;
  end
  else
  begin
    FIndex := 0;
    Result := not Done;
  end;
end;

{function TmncCommand.Step: Boolean;
begin
  Result := Fetch;
end;}

function TmncCommand.Execute: Boolean;
begin
  Result := Execute(True);
end;

function TmncCommand.GetField(const Index: string): TmncField;
begin
  {if not Prepared then
    Prepare;}
  if Fields <> nil then
    Result := Fields.Field[Index]
  else
    Result := nil;
end;

function TmncCommand.GetIndex: Int64;
begin
  Result := FIndex;
end;

function TmncCommand.GetParam(const Index: string): TmncParam;
begin
  if not Prepared then
    Prepare;
  if Params <> nil then
    Result := Params.Param[Index]
  else
    Result := nil;
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

procedure TmncCommand.CheckTransaction;
begin
  if (Transaction = nil) then
    raise EmncException.Create('Transaction not assigned');
  //Raise an exception if not active when it is strict
  {
  if (sbhStrict in Transaction.Behaviors) and not Transaction.Active then
  //we do not need to check if active, some command not need active transaction, like 'set transaction' in firebird
    raise EmncException.Create('Transaction is not active/started');
  }
end;

procedure TmncCommand.DoUnprepare;
begin
end;

function TmncCommand.Execute(vNext: Boolean): Boolean;
begin
  Result := InternalExecute(vNext);
end;

function TmncCommand.Fetch: Boolean;
begin
  {if FExecuted then in case we need skip first next after execute
  begin
    if FSteped or Ready then
      Result := Next
    else
      Result := not Done;

    FSteped := True;
  end
  else
  begin
    if Ready then
      Result := InternalExecute(True)
    else
      Result := Next;
  end;}


  if not FExecuted then
    Result := InternalExecute(True)
  else
    Result := Next
end;

function TmncCommand.Next: Boolean;
begin
  DoNext;
  HitUnready;
  Result := not Done;
  if Result then
    Inc(FIndex);
end;

procedure TmncCommand.Prepare;
begin
  if FPrepared then
    raise EmncException.Create('Command is already prepared!');
  CheckTransaction;
  if not FParsed then
    Parse;
  { TODO : Clear Columns and Fields }
  DoPrepare;
  FPrepared := True;
  if Params <> nil then
    Params.Clean;
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

procedure TmncCommand.Close;
begin
{  if not Active then
    raise EmncException.Create('Command already Closed');}

  if Prepared then
    DoUnprepare;

  FReady := True;
  FDone := True;
  FExecuted := False;
  DoClose;
  FPrepared := False;
end;

function TmncCommand.GetValues(const Index: string): Variant;
begin
  if not Prepared then
    Prepare;
  if Fields <> nil then
    Result := Fields.Values[Index]
  else
    raise EmncException.Create('Current record not found');
end;

procedure TmncCommand.HitDone;
begin
  FDone := True;
end;

procedure TmncCommand.HitDone(vCondition: Boolean);
begin
  if vCondition then
    HitDone;
end;

procedure TmncCommand.HitUnready;
begin
  FReady := False;
end;

{ TmncCommand.TmncCommandEnumerator }

constructor TmncCommand.TmncCommandEnumerator.Create(ACommand: TmncCommand);
begin
  FCommand := ACommand;
  //FCommand.Execute(False);
end;

function TmncCommand.TmncCommandEnumerator.GetCurrent: TmncFields;
begin
  Result := FCommand.Fields;
end;

function TmncCommand.TmncCommandEnumerator.MoveNext: Boolean;
begin
  Result := FCommand.Fetch;
  if not Result then
    FCommand.Close;
end;

{ TmncLinkTransaction }

procedure TmncLinkTransaction.SetTransaction(AValue: TmncTransaction);
begin
  inherited Link := AValue;
end;

function TmncLinkTransaction.GetTransaction: TmncTransaction;
begin
  Result := Link as TmncTransaction;
end;

constructor TmncLinkTransaction.CreateBy(vTransaction: TmncTransaction);
begin
  Create;
  Transaction := vTransaction;//Transaction not FTransaction
end;

{ TmncTransaction }

procedure TmncTransaction.Commit(Retaining: Boolean = False);
begin
  InternalStop(sdaCommit, Retaining);
end;

constructor TmncTransaction.Create(vConnection: TmncConnection; vBehaviors: TmncTransactionBehaviors);
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

constructor TmncTransaction.Create(vConnection: TmncConnection);
var
  aBehaviors: TmncTransactionBehaviors;
begin
  aBehaviors := [];
  if ccStrict in vConnection.Capabilities then
    aBehaviors := aBehaviors + [sbhStrict];
  if ccTransaction in vConnection.Capabilities then
  begin
    if ccMultiTransaction in vConnection.Capabilities then
      aBehaviors := aBehaviors + [sbhMultiple]
    else
      aBehaviors := aBehaviors + [sbhEmulate]
  end;
  Create(vConnection, aBehaviors);
end;

destructor TmncTransaction.Destroy;
begin
  if Active then
    Stop;
  Links.Unlink;
  Connection := nil;
  FreeAndNil(FParams);
  inherited;
end;

function TmncTransaction.GetActive: Boolean;
begin
  Result := FStartCount > 0;
end;

procedure TmncTransaction.CheckActive;
begin
  if not Active then
    raise EmncException.Create('Transaction is not active/opened');
end;

procedure TmncTransaction.CheckInactive;
begin
  if Active then
    raise EmncException.Create('Transaction is active/opened');
end;

procedure TmncTransaction.DoInit;
begin
end;

procedure TmncTransaction.Init;
begin
  if not FIsInit then
  begin
    DoInit;
    FIsInit := True;
  end;
end;

procedure TmncTransaction.InternalStart;
begin
  if Connection.IsStrict then
  begin
    if Active then //Even if not strict, you cant start the Transaction more than one?
      raise EmncException.Create('Transaction is already active.');
    Connection.CheckActive; //* TODO move out of strict
  end;
  Init;
  if sbhEmulate in Behaviors then
  begin
    if Connection.FStartCount = 0 then
    begin
      FCurrentAction := sdaCommit;
      DoStart;
    end;
    Inc(Connection.FStartCount);
  end
  else if sbhMultiple in Behaviors then
    DoStart;
  Inc(FStartCount);
end;

procedure TmncTransaction.InternalStop(How: TmncTransactionAction; Retaining: Boolean);
begin
  if Connection.IsStrict and (not Active) then //Even if not strict we check if active, because you cant stop Transaction if you not started it!
    raise EmncException.Create('Oops you have not started it yet!');

  //Links.Close; belal: conceder this case cmd1  begin trans cmd2 end trans => force cmd1 close

  if sbhEmulate in Behaviors then
  begin
    if Retaining then //Nothing to do if Retaingig
    begin
      DoStop(How, Retaining);
    end
    else
    begin
      if Connection.FStartCount = 0 then
        raise EmncException.Create('Transaction not started yet!');

      if (FCurrentAction > How) then
      begin
        How := FCurrentAction;
        //raise EmncException.Create('there is older action rollbacked!'); //in emulate mode you can't do rollback, then commit if we have 2 Transaction started
      end
      else
        FCurrentAction := How;

      if Connection.FStartCount = 1 then //* Last transaction
      begin
        DoStop(How, Retaining);
        Dec(Connection.FStartCount); //* Dec here after stop, if Stop raise exception no dec for startcount
      end
      else if Connection.FStartCount > 0 then
        Dec(Connection.FStartCount);
    end;
  end
  else if sbhMultiple in Behaviors then
      DoStop(How, Retaining);

  if not Retaining then
    Dec(FStartCount);
end;

procedure TmncTransaction.Rollback(Retaining: Boolean);
begin
  InternalStop(sdaRollback, Retaining);
end;

procedure TmncTransaction.CommitRetaining;
begin
  Commit(True);
end;

procedure TmncTransaction.RollbackRetaining;
begin
  Rollback(True);
end;

procedure TmncTransaction.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TmncTransaction.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TmncTransaction.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TmncTransaction.SetConnection(const Value: TmncConnection);
begin
  if FConnection <> Value then
  begin
    if FConnection <> nil then
      FConnection.Transactions.Remove(Self);
    FConnection := Value;
    if FConnection <> nil then
      FConnection.Transactions.Add(Self);
  end;
end;

procedure TmncTransaction.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TmncTransaction.Start;
begin
  InternalStart;
end;

procedure TmncTransaction.Stop;
begin
  InternalStop(Action);
end;

function TmncFields.Add(Column: TmncColumn; Value: Variant): TmncField;
begin
  Result := Add(Column);
  Result.Value := Value;
end;

function TmncFields.Add(Index: Integer; Value: Variant): TmncField;
var
  aColumn: TmncColumn;
begin
  if Index < Columns.Count then
    aColumn := Columns[Index]
  else
    aColumn := nil; //TODO bad idea, need more check
  Result := Add(aColumn, Value);
end;

function TmncFields.FieldByName(const vName: string): TmncField;
begin
  Result := Find(vName) as TmncField;
  if Result = nil then
    raise EmncException.Create('Field "' + vName + '" not found');
end;

function TmncFields.Add(Column: TmncColumn): TmncField;
begin
  Result := CreateField(Column) as TmncField;
  Result.Column := Column;
  inherited Add(Result);
end;

constructor TmncFields.Create(vColumns: TmncColumns);
begin
  inherited Create;
  FColumns := vColumns;
  if FColumns = nil then
    raise EmncException.Create('vColumns must be not nil');
end;

function TmncFields.CreateField(vColumn: TmncColumn): TmncField;
begin
  Result := DoCreateField(vColumn);
end;

function TmncFields.CreateField(vIndex: Integer): TmncField;
begin
  Result := CreateField(Columns[vIndex]);
end;

function TmncFields.GetField(const Index: string): TmncField;
begin
  Result := Find(Index);
end;

function TmncFields.GetItem(Index: Integer): TmncField;
begin
  if Self = nil then
    raise EmncException.Create('Fields is nil');
  Result := (inherited Items[Index]) as TmncField;
end;

function TmncFields.GetValues(const Index: string): Variant;
var
  F: TmncField;
begin
  F := Find(Index);
  if F <> nil then
    Result := F.Value
  else
    Result := Unassigned;
end;

function TmncFields.IsExist(const vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{function TmncFields.SetValue(const Index: string; const AValue: Variant): TmncField;
begin
  Result := FieldByName(Index);
  Result.Value := AValue;
end;}

procedure TmncFields.SetValues(const Index: string; const AValue: Variant);
var
  Field: TmncField;
begin
  Field := FieldByName(Index);
  if Field<>nil then
    Field.Value := AValue;
end;

{ TmncColumns }

function TmncColumns.Add(vIndex: Integer; vName: string; vType: TmnDataType; FieldClass: TmncColumnClass): TmncColumn;
begin
  if FieldClass = nil then
    FieldClass := TmncColumn;
  Result := FieldClass.Create;
  Result.Index := vIndex;
  Result.Name := vName;
  Result.FullName := vName;
  Result.FDataType := vType;
  inherited Add(Result);
end;

function TmncColumns.Add(vName: string; vType: TmnDataType): TmncColumn;
begin
  Result := Add(Count, vName, vType);
end;

function TmncColumns.Add(vColumn: TmncColumn): Integer;
begin
  Result := inherited Add(vColumn);
end;

procedure TmncColumns.Assign(Source: TmncColumns);
begin

end;

procedure TmncColumns.Clone(FromColumns: TmncColumns; AsDataType: TmnDataType);
var
  i: Integer;
begin
  for i := 0 to FromColumns.Count -1 do
    Add(FromColumns[i].Name, AsDataType);
end;

procedure TmncColumns.Clone(FromColumns: TmncColumns);
var
  i: Integer;
begin
  for i := 0 to FromColumns.Count -1 do
    Add(FromColumns[i].Name, FromColumns[i].DataType);
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

function TmncParams.Add(Name: string): TmncParam;
begin
  Result := CreateParam as TmncParam;
  Result.Name := Name;
  inherited Add(Result);
end;

function TmncParams.Require(const Name: string): TmncParam;
begin
  Result := Find(Name);
  if Result = nil then
    Result := Add(Name);
end;

procedure TmncCustomParams.Clear;
begin
  inherited;
end;

function TmncCustomParams.ParamByName(const vName: string): TmncParam;
begin
  Result := Find(vName);
  if Result = nil then
    raise EmncException.Create('Param ' + vName + ' not found');
end;

function TmncCustomParams.GetParam(const Index: string): TmncParam;
begin
  Result := Find(Index);
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

{ TmncColumn }

procedure TmncColumn.SetSize(AValue: Int64);
begin
  if FSize =AValue then Exit;
  FSize :=AValue;
end;

procedure TmncColumn.SetType(vType: TmnDataType);
begin
  FDataType := vType;
end;

procedure TmncColumn.SetDecimals(AValue: Integer);
begin
  if FDecimals =AValue then Exit;
  FDecimals :=AValue;
end;

procedure TmncColumn.SetIsNull(const AValue: Boolean);
begin
  raise EmncException.Create('Field have no value, You must not use it, try use Fields!') {$ifdef fpc}at get_caller_addr(get_frame){$else}at ReturnAddress{$endif};
end;

function TmncColumn.GetIsNull: Boolean;
begin
    Result := False;
  //raise EmncException.Create('Field have no value, You must not use it, try use Fields!');
end;

function TmncColumn.GetName: string;
begin
  Result := Name;
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

function TmncParam.GetName: string;
begin
  Result := Name;
end;

constructor TmncParam.Create;
begin
  inherited;
end;

{ TmncTransactions }

procedure TmncTransactions.Stop;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Active then
      Items[i].Stop;
  end;
end;

function TmncTransactions.IsAnyActive: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Active then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TmncTransactions.GetItem(Index: Integer): TmncTransaction;
begin
  Result := inherited Items[Index] as TmncTransaction;
end;

procedure TmncTransactions.SetItem(Index: Integer; const Value: TmncTransaction);
begin
  inherited Items[Index] := Value;
end;

{ TmncItem }

procedure TmncItem.Created;
begin
  inherited;
  FBlobType := blobBinary;
  FDataType := dtUnknown;
end;

function TmncItem.GetAsText: string;
begin
  //if (IsBlob) and (BlobType = blobText) then
  if (DataType in [dtBlob]) and (BlobType = blobBinary) then
    Result := AsHex
  else
    Result := inherited GetAsText;
end;

procedure TmncItem.SetAsText(const AValue: string);
begin
  //if (IsBlob) and (BlobType = blobText) then
  if (DataType in [dtBlob]) and (BlobType = blobBinary) then
    AsHex := AValue
  else
    inherited SetAsText(AValue);
end;

function TmncItem.IsNumber: Boolean;
begin
  Result := DataType in [dtInteger, dtCurrency, dtFloat];
end;

{ TmncItems<T> }

constructor TmncItems<T>.Create;
begin
  inherited Create;
  FDic := TDictionary<string, T>.Create(256);
end;

destructor TmncItems<T>.Destroy;
begin
  FreeAndNil(FDic);
  inherited;
end;

function TmncItems<T>.Find(const vName: string): T;
var
  i: integer;
begin
  if FDic <> nil then
    FDic.TryGetValue(vName.ToLower, Result)
  else
  begin
    Result := nil;
    if vName <> '' then
      for i := 0 to Count - 1 do
      begin
        if SameText(Items[i].GetName, vName) then
        begin
          Result := Items[i];
          break;
        end;
      end;
  end;
end;

function TmncItems<T>.ItemByName(const vName: string): T;
begin
  Result := Find(vName);
  if Result =nil then
    raise EmncException.Create('Can not find ' + vName);
end;

{$ifdef FPC}
procedure TmncItems<T>.Notify(Ptr: Pointer; Action: TListNotification);
{$else}
procedure TmncItems<T>.Notify(const Value: T; Action: TCollectionNotification);
{$endif}
begin
  if FDic <> nil then
  begin
    {$ifdef FPC}
    if Action in [lnExtracted, lnDeleted] then //Need it in FPC https://forum.lazarus.freepascal.org/index.php/topic,60984.0.html
      FDic.Remove(T(Ptr).GetName.ToLower);//bug in fpc
    {$else}
    if Action in [cnExtracting, cnRemoved, cnDeleting] then
      FDic.Remove(Value.GetName.ToLower);
    {$endif}
    inherited;
    {$ifdef FPC}
    if Action = lnAdded then
      FDic.AddOrSetValue(T(Ptr).GetName.ToLower, T(Ptr));
    {$else}
    if Action = cnAdded then
      FDic.AddOrSetValue(Value.GetName.ToLower, Value);
    {$endif}
  end
  else
    inherited;
end;

initialization
finalization
end.
