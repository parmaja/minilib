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

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs, SyncObjs,
  mnFields, mnParams;

type
  EmncException = class(Exception)
  end;

  TmncObject = class(TObject)
  end;

  TmncSession = class;
  TmncLinkObject = class;

  TmncSessions = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncSession;
    procedure SetItem(Index: Integer; const Value: TmncSession);
  protected
    procedure Stop;
  public
    property Items[Index: Integer]: TmncSession read GetItem write SetItem; default;
  end;

  TmncLinks = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncLinkObject;
    procedure SetItem(Index: Integer; const Value: TmncLinkObject);
  protected
    procedure Close;
  public
    property Items[Index: Integer]: TmncLinkObject read GetItem write SetItem; default;
  end;

  TmncCodepageConvert = (cpcNone, cpcAnsi, cpcUTF8, cpcUnicode);

//Connection as like connect by FTP server to send commands or a Database to send SQL
{
  smNone:     No transactions support
  smSingle:   All sessions have the same transacion like PG and SQLite
                         but we can make in PG for every session a have new connection to database so PG is smMultiTransactions
  smEmulate:  Single transaction but last session commited make the real commit
  smMultiple: Every session have transacion like as Firebird
  smConnection: Every session have new connection good for SQLite and PG
}
  TmncSessionMode = (smNone, smSingle, smEmulate, smMultiple, smConnection);

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
    property Sessions: TmncSessions read FSessions;
    class function GetMode: TmncSessionMode; virtual;
    procedure DoInit; virtual;
    procedure Init;
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disonnect;
    property Mode: TmncSessionMode read GetMode;
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

  //Session it is branch/clone of Connection but usefull for take a special params, it is like Transactions.
  TmncSessionAction = (sdaCommit, sdaRollback);

  { TmncSession }

  TmncSession = class(TmncObject)
  private
    FParams: TStrings;
    FConnection: TmncConnection;
    FCommands: TmncLinks;
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
    function GetActive: Boolean; virtual;
    procedure CheckActive;
    procedure CheckInactive;
    procedure DoInit; virtual;
    procedure DoStart; virtual; abstract;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); virtual; abstract;
    procedure Init;
    property Commands: TmncLinks read FCommands;
    procedure InternalStop(How: TmncSessionAction; Retaining: Boolean = False);
    property ParamsChanged: Boolean read FParamsChanged write FParamsChanged;
  public
    constructor Create(vConnection: TmncConnection); virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Commit;
    procedure Rollback;
    procedure Stop;
    property Action: TmncSessionAction read FAction write FAction;
    property Connection: TmncConnection read FConnection write SetConnection;
    property Active: Boolean read GetActive write SetActive;
    property Params: TStrings read FParams write SetParams;
  end;

  TmncDataType = (ftUnkown, ftString, ftInteger, ftCurrency, ftFloat, ftDate, ftTime, ftDateTime, ftMemo, ftBlob);
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
    property DataType: TmncDataType read FDataType default ftUnkown;
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

  { TmncColumn }

  TmncColumn = class(TmncItem)
  private
    FIndex: Integer;
    FName: string;
    FSchemaType: string;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  published
    property Index: Integer read FIndex write FIndex;
    property DataType;
    property SchemaType: string read FSchemaType write FSchemaType;
    property Name: string read FName write FName;
    //property Size: Integer read FSize write FSize;//todo not yet in sqlite every value have own length not depends on the Schema
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
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
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
  end;

  { TmncCustomFields }

  TmncCustomFields = class(TmncItems)
  private
    function GetItem(Index: Integer): TmncCustomField;
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
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
    constructor Create(vColumn: TmncColumn);
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
  public
    constructor Create(vColumns: TmncColumns);
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
    FBuffer: Pointer;
    FBufferSize: Integer;
    FName: string;
    function GetBufferAllocated: Boolean;
  protected
  public
    destructor Destroy; override;
    procedure AllocBuffer(var P; Size: Integer);
    procedure FreeBuffer;
    property Buffer: Pointer read FBuffer;
    property BufferSize: Integer read FBufferSize;
    property BufferAllocated: Boolean read GetBufferAllocated;
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

  TmncParams = class(TmncCustomParams)
  private
  published
  public
    function Add(Name: string): TmncParam;
    function AddExists(Name: string): TmncParam;
  end;

  TmncParamList = class(TmncCustomParams)
  private
  public
    constructor Create;
  end;

  { TmncLinkObject }

  TmncLinkObject = class(TmncObject)
  private
    FSession: TmncSession;
    procedure SetSession(const Value: TmncSession);
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
  public
    constructor Create; virtual;
    constructor CreateBy(vSession: TmncSession);
    destructor Destroy; override;
    property Session: TmncSession read FSession write SetSession;
    property Active: Boolean read GetActive write SetActive;
  end;

  { TmncCommand }

  TmncCommand = class(TmncLinkObject)
  private
    FColumns: TmncColumns;
    FCurrent: TmncFields;
    FParams: TmncParams;
    FPrepared: Boolean;
    FNextOnExecute: Boolean;
    FParamList: TmncParamList;
    procedure SetRequest(const Value: TStrings);
    procedure SetColumns(const Value: TmncColumns);
    procedure SetCurrent(const Value: TmncFields);
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
    procedure DoPrepare; virtual; abstract;
    procedure DoExecute; virtual; abstract; //Here apply the ParamList and execute the sql
    procedure DoNext; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoCommit; virtual; //some time we need make commit with command or session
    procedure DoRollback; virtual;
    procedure DoRequestChanged(Sender: TObject); virtual;
    property Request: TStrings read FRequest write SetRequest;
    property ParamList: TmncParamList read FParamList; //for Dublicated names when pass the params when execute
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare;
    function Execute: Boolean;
    procedure Close;
    function Next: Boolean;
    function EOF: Boolean;
    procedure Clear; virtual;
    procedure Commit;
    procedure Rollback;
    function ReleaseCurrent: TmncFields;
    function ReleaseParams: TmncParams;
    function FieldIsExist(Name: string): Boolean;
    property NextOnExecute: Boolean read FNextOnExecute write FNextOnExecute default True;
    property Prepared: Boolean read FPrepared;
    property Columns: TmncColumns read FColumns write SetColumns;
    property Fields: TmncColumns read FColumns write SetColumns; //TODO Fields > Columns backward compatibility
    property Current: TmncFields read FCurrent write SetCurrent; //TODO Current > Fields
    property Params: TmncParams read FParams write SetParams;
    property Param[Index: string]: TmncParam read GetParam;
    property Field[Index: string]: TmncField read GetField;
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
  FParams := TStrings.Create;
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

class function TmncConnection.GetMode: TmncSessionMode;
begin
  Result := smNone;
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
  FPrepared := False;
  FRequest.Clear;
  if FParams <> nil then
    FParams.Clear;
  FParamList.Clear;
end;

destructor TmncCommand.Destroy;
begin
  Active := False;
  Session := nil;//already in Linked but must be sure before free other objects
  FreeAndNil(FRequest);
  FreeAndNil(FCurrent);
  FreeAndNil(FParamList);
  FreeAndNil(FParams);
  FreeAndNil(FColumns);
  inherited;
end;

procedure TmncCommand.DoRequestChanged(Sender: TObject);
begin
  if Active then
    Close;
end;

constructor TmncCommand.Create;
begin
  inherited;
  FRequest := TStringList.Create;
  (FRequest as TStringList).OnChange := DoRequestChanged;

  FColumns := TmncColumns.Create;
  FParams := TmncParams.Create;
  FParamList := TmncParamList.Create;
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
  if Current <> nil then
    Result := Current.Field[Index]
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
  if not Active then
    raise EmncException.Create('Command is active/opened');
end;

procedure TmncCommand.CheckStarted;
begin
  if (Session = nil) then
    raise EmncException.Create('Session not assigned');
  if not Session.Active then
    raise EmncException.Create('Session is not active/started');
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
  CheckStarted;
  DoPrepare;
  FPrepared := True;
end;

function TmncCommand.ReleaseCurrent: TmncFields;
begin
  Result := FCurrent;
  FCurrent := nil;
end;

function TmncCommand.ReleaseParams: TmncParams;
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

procedure TmncCommand.SetCurrent(const Value: TmncFields);
begin
  if FCurrent <> Value then
  begin
    FreeAndNil(FCurrent);
    FCurrent := Value;
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

procedure TmncLinkObject.SetSession(const Value: TmncSession);
begin
  if FSession <> Value then
  begin
    if FSession <> nil then
      FSession.Commands.Remove(Self);
    FSession := Value;
    if FSession <> nil then
      FSession.Commands.Add(Self);
  end;
end;

function TmncLinkObject.GetActive: Boolean;
begin
  Result := False;
end;

procedure TmncLinkObject.SetActive(const AValue: Boolean);
begin
end;

constructor TmncLinkObject.Create;
begin
  inherited Create;
end;

constructor TmncLinkObject.CreateBy(vSession: TmncSession);
begin
  Create;
  Session := vSession;//Session not FSession
end;

destructor TmncLinkObject.Destroy;
begin
  Session := nil;
  inherited Destroy;
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
  DoClose;
  FPrepared := False;
end;

{ TmncSession }

procedure TmncSession.Commit;
begin
  InternalStop(sdaCommit, False);
end;

constructor TmncSession.Create(vConnection: TmncConnection);
begin
  inherited Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  Connection := vConnection;
  FCommands := TmncLinks.Create(False);
  if Connection.AutoStart then
    Start;
end;

destructor TmncSession.Destroy;
begin
  Stop;
  while FCommands.Count > 0 do
    FCommands[0].Session := nil;
  Connection := nil;
  FreeAndNil(FCommands);
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
  Dec(FStartCount);
  case Connection.Mode of
    smMultiple:
      DoStop(How, Retaining);
    smSingle:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        if (Connection.FStartCount > 0) then
          DoStop(How, Retaining);
      end;
    smEmulate:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        DoStop(How, Retaining);
        if (Connection.FStartCount > 0) then
          DoStart;
      end;
    smConnection:{TODO};
  end;
end;

procedure TmncSession.Rollback;
begin
  InternalStop(sdaCommit, False);
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
  if (Connection.Mode <> smNone) and (Active) then
    raise EmncException.Create('Session is already active.');
  Connection.Init;
  Init;
  case Connection.Mode of
    smMultiple: DoStart;
    smSingle,
    smEmulate:
      begin
        if Connection.FStartCount = 0 then
          DoStart;
        Inc(Connection.FStartCount);
      end;
    smConnection:{TODO};
  end;
  Inc(FStartCount);
end;

procedure TmncSession.Stop;
begin
  FCommands.Close;
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
  Result := TmncField.Create(Column);
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
    raise EmncException.Create('Field ' + vName + ' not found');
end;

function TmncFields.Add(Column: TmncColumn): TmncField;
begin
  Result := TmncField.Create(Column);
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

function TmncParams.Add(Name: string): TmncParam;
begin
  Result := TmncParam.Create;
  Result.Name := Name;
  inherited Add(Result);
end;

function TmncParams.AddExists(Name: string): TmncParam;
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

{ TmncParamList }

constructor TmncParamList.Create;
begin
  inherited Create(False);
end;

{ TCustomField }

{ TmncColumn }

function TmncColumn.GetValue: Variant;
begin
  Result := null;
  raise EmncException.Create('Field have no value, You must not use it, try use Current!') {$ifdef fpc}at get_caller_addr(get_frame){$endif};
end;

procedure TmncColumn.SetValue(const AValue: Variant);
begin
  raise EmncException.Create('Field have no value, You must not use it, try use Current!');
end;

{ TmncParam }

procedure TmncParam.AllocBuffer(var P; Size: Integer);
begin
  FreeBuffer;
  FBufferSize := Size;
  if Size > 0 then
  begin
    FBuffer := AllocMem(FBufferSize);
    Move(P, FBuffer^, Size);
  end;
end;

destructor TmncParam.Destroy;
begin
  FreeBuffer;
  inherited;
end;

procedure TmncParam.FreeBuffer;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
end;

function TmncParam.GetBufferAllocated: Boolean;
begin
  Result := Buffer <> nil;
end;

{ TmncLinks }

procedure TmncLinks.Close;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Active := False;
  end;
end;

function TmncLinks.GetItem(Index: Integer): TmncLinkObject;
begin
  Result := inherited Items[Index] as TmncLinkObject;
end;

procedure TmncLinks.SetItem(Index: Integer; const Value: TmncLinkObject);
begin
  inherited Items[Index] := Value;
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

{ TmncCustomField }

function TmncCustomField.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TmncCustomField.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

initialization
finalization
  FreeAndNil(FConnectionLock);
end.
