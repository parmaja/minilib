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
  Classes, SysUtils, DateUtils, Variants, Contnrs, syncobjs;

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
  smNone:                No transactions support
  smSingleTransaction:  All sessions have the same transacion like PG and SQLite
                         but we make in PG for every session a new connection to database so PG is smMultiTransactions
  smEmulateTransaction: Single transaction but last session commited make the real commit
  smMultiTransaction:   Every session have transacion like as Firebird
}
  TmncTransactionMode = (smNone, smSingleTransaction, smEmulateTransaction, smMultiTransaction);

  { TmncConnection }

  TmncConnection = class(TmncObject)
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FPassword: string;
    FPort: string;
    FResource: string;
    FHost: string;
    FUserName: string;
    FAutoStart: Boolean;
    FAutoCreate: Boolean;
    FSessions: TmncSessions;
    FStartCount: Integer;
    procedure SetConnected(const Value: Boolean);
  protected
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    property Sessions: TmncSessions read FSessions;
    class function GetMode: TmncTransactionMode; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disonnect;
    property Mode: TmncTransactionMode read GetMode;
    property AutoStart: Boolean read FAutoStart write FAutoStart; //AutoStart the Session when created
    property Connected: Boolean read GetConnected write SetConnected;
    property Active: Boolean read GetConnected write SetConnected;
    property AutoCreate: Boolean read FAutoCreate write FAutoCreate default False;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Resource: string read FResource write FResource; //can be a Database name or Alias or service name etc...
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

  //Session it branch/clone of Connection but usefull for take a special params, it like Transactions.
  TmncSessionAction = (sdaCommit, sdaRollback);

  { TmncSession }

  TmncSession = class(TmncObject)
  private
    FParams: TStrings;
    FConnection: TmncConnection;
    FCommands: TmncLinks;
    FStartCount: Integer;
    FAction: TmncSessionAction;
    procedure SetParams(const Value: TStrings);
    procedure SetConnection(const Value: TmncConnection);
    procedure SetActive(const Value: Boolean);
  protected
    function GetActive: Boolean; virtual;
    procedure CheckActive;
    procedure DoInit; virtual;
    procedure DoStart; virtual; abstract;
    procedure DoCommit; virtual; abstract;
    procedure DoRollback; virtual; abstract;
    procedure DoStop; virtual;
    property Commands: TmncLinks read FCommands;
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

  { TmncCustomColumn }

  TmncCustomColumn = class(TObject)
  private
    FDataType: TmncDataType;
    FBlobType: TmncBlobType;
    FSchemaType: string;
    FIsBlob: Boolean;
    FName: string;
    function GetAsHex: string;
    procedure SetAsHex(const AValue: string);
    function GetAsText: string;
    procedure SetAsText(const AValue: string);


    procedure SetAsNullString(const Value: string);

    function GetAsAnsiString: ansistring;
    procedure SetAsAnsiString(const Value: ansistring);

    function GetAsWideString: widestring;
    procedure SetAsWideString(const Value: widestring);

    function GetAsUtf8String: UTF8String;
    procedure SetAsUtf8String(const Value: UTF8String);
  protected
    function GetVariant: Variant; virtual; abstract;
    procedure SetVariant(const Value: Variant); virtual; abstract;

    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;

    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const Value: Integer); virtual;

    function GetAsInt64: Integer; virtual;
    procedure SetAsInt64(const Value: Integer); virtual;

    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(const Value: Boolean); virtual;

    function GetAsCurrency: Currency; virtual;
    procedure SetAsCurrency(const Value: Currency); virtual;

    function GetAsDate: TDateTime; virtual; //zaher must use trunc
    procedure SetAsDate(const Value: TDateTime); virtual;

    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const Value: TDateTime); virtual;

    function GetAsTime: TDateTime; virtual;
    procedure SetAsTime(const Value: TDateTime); virtual;


    function GetAsTrimString: string;
    procedure SetAsTrimString(const Value: string);

    property Value: Variant read GetVariant write SetVariant;
    property AsVariant: Variant read GetVariant write SetVariant;
    //AsAnsiString: Convert strign to utf8 it is special for Lazarus
    property AsAnsiString: ansistring read GetAsAnsiString write SetAsAnsiString;

    property AsWideString: widestring read GetAsWideString write SetAsWideString;
    property AsUtf8String: Utf8String read GetAsUtf8String write SetAsUtf8String;
    property AsString: string read GetAsString write SetAsString;
    property AsTrimString: string read GetAsTrimString write SetAsTrimString;
    property AsNullString: string read GetAsString write SetAsNullString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Integer read GetAsInt64 write SetAsInt64;
    property AsID: Integer read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsHex: string read GetAsHex write SetAsHex;
    property AsText: string read GetAsText write SetAsText; //binary text blob convert to hex

    function GetIsNull: Boolean;
    function GetIsEmpty: Boolean; virtual;
    function GetText: string; virtual;
    property Text: string read GetText;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNull: Boolean read GetIsNull;
    property IsBlob: Boolean read FIsBlob write FIsBlob default false;
    property BlobType: TmncBlobType read FBlobType write FBlobType default blobBinary;
    property DataType: TmncDataType read FDataType default ftUnkown;
    property SchemaType: string read FSchemaType write FSchemaType;
    {    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToIStream(Stream: IStreamPersist);}
  public
    procedure Clear;//make value null
  published
    property Name: string read FName write FName;
  end;

  TmncCustomColumnClass = class of TmncCustomColumn;

  { TmncCustomColumns }

  TmncCustomColumns = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncCustomColumn;
  protected
    function Find(vName: string): TmncCustomColumn; virtual; abstract;
  public
    function Add(AColumn: TmncCustomColumn): Integer; overload;
    function ByName(vName: string): TmncCustomColumn;
    function IsExists(vName: string): Boolean;
    procedure Clean; virtual;
    property Items[Index: Integer]: TmncCustomColumn read GetItem;
  end;

  { TmncColumn }

  TmncColumn = class(TmncCustomColumn)
  private
    FIndex: Integer;
  protected
    function GetVariant: Variant; override;
    procedure SetVariant(const Value: Variant); override;
  published
    property Index: Integer read FIndex write FIndex;
    property DataType;
    property SchemaType;
    //property Size: Integer read FSize write FSize;//todo not yet
  end;

  TmncColumnClass = class of TmncColumn;

  TmncColumns = class(TmncCustomColumns)
  private
    function GetItem(Index: Integer): TmncColumn;
  protected
    function Find(vName: string): TmncCustomColumn; override;
  public
    function Add(vIndex: Integer; vName: string; vType: TmncDataType; FieldClass: TmncColumnClass = nil): TmncColumn; overload;
    function Add(vName: string; vType: TmncDataType): TmncColumn; overload;
    property Items[Index: Integer]: TmncColumn read GetItem; default;
  end;

{
  Values
}

  TmncCustomField = class(TmncCustomColumn)
  private
  protected
  public
  published
    property IsEmpty;
    property IsNull;
    property IsBlob;
    property BlobType;

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

  TmncCustomFields = class(TmncCustomColumns)
  private
    function GetItem(Index: Integer): TmncCustomField;
    function GetField(Index: string): TmncCustomField;
  public
    property Items[Index: Integer]: TmncCustomField read GetItem;
    property Field[Index: string]: TmncCustomField read GetField; default;
  end;

{
  Record
}

  TmncField = class(TmncCustomField)
  private
    FValue: Variant;
    FColumn: TmncColumn;
  protected
    function GetVariant: Variant; override;
    procedure SetVariant(const Value: Variant); override;
  public
    constructor Create(vColumn: TmncColumn);
  published
    property Column: TmncColumn read FColumn write FColumn;
  end;

  { TmncFields }

  TmncFields = class(TmncCustomFields)
  private
    FColumns: TmncColumns;
    FRowID: Integer;
    function GetItem(Index: Integer): TmncField;
    function GetField(Index: string): TmncField;
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
  protected
    function Find(vName: string): TmncCustomColumn; override;
  public
    constructor Create(vColumns: TmncColumns);
    function FindField(vName: string): TmncField;
    function FieldByName(vName: string): TmncField;
    function Add(Column: TmncColumn; Value: Variant): TmncField; overload;
    function Add(Column: TmncColumn): TmncField; overload;
    function Add(Index: Integer; Value: Variant): TmncField; overload;
    property Fields: TmncColumns read FColumns;
    property Value[Index: string]: Variant read GetValue write SetValue;
    property Field[Index: string]: TmncField read GetField; default;
    property Items[Index: Integer]: TmncField read GetItem;
    property RowID: Integer read FRowID write FRowID default 0; //most of SQL engines have this value
  end;

{
  Params
}

  TmncParam = class(TmncCustomField)
  private
    FValue: Variant;
    FBuffer: Pointer;
    FBufferSize: Integer;
    function GetBufferAllocated: Boolean;
  protected
    function GetVariant: Variant; override;
    procedure SetVariant(const Value: Variant); override;
  public
    destructor Destroy; override;
    procedure AllocBuffer(var P; Size: Integer);
    procedure FreeBuffer;
    property Buffer: Pointer read FBuffer;
    property BufferSize: Integer read FBufferSize;
    property BufferAllocated: Boolean read GetBufferAllocated;
  end;

  { TmncCustomParams }

  TmncCustomParams = class(TmncCustomFields)
  private
  protected
    function GetParam(Index: string): TmncParam;
    function GetItem(Index: Integer): TmncParam;
    function Find(vName: string): TmncCustomColumn; override;
  public
    procedure Clear; override;
    function FindParam(vName: string): TmncParam;
    function ParamByName(vName: string): TmncParam;
    property Items[Index: Integer]: TmncParam read GetItem;
    property Param[Index: string]: TmncParam read GetParam; default;
  end;

  TmncParams = class(TmncCustomParams)
  private
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
  published
  public
    function Add(Name: string): TmncParam;
    function AddExists(Name: string): TmncParam;
    property Value[Index: string]: Variant read GetValue write SetValue; default;
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
    procedure SetActive(const Value: Boolean); virtual;
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
  DoConnect;
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

constructor TmncConnection.Create;
begin
  inherited Create;
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
  inherited;
end;

procedure TmncConnection.Disconnect;
begin
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

class function TmncConnection.GetMode: TmncTransactionMode;
begin
  Result := smNone;
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

procedure TmncCommand.CheckStarted;
begin
  if (Session = nil) or not Session.Active then
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

procedure TmncLinkObject.SetActive(const Value: Boolean);
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
  if not Active then
    raise EmncException.Create('Oops you have not started yet!');
  Dec(FStartCount);
  case Connection.Mode of
    smMultiTransaction: DoCommit;
    smSingleTransaction:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        if (Connection.FStartCount > 0) then
          DoCommit;
      end;
    smEmulateTransaction:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        DoCommit;
        if (Connection.FStartCount > 0) then
          DoStart;
      end;
  end;
end;

constructor TmncSession.Create(vConnection: TmncConnection);
begin
  inherited Create;
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

procedure TmncSession.DoInit;
begin
end;

procedure TmncSession.DoStop;
begin
end;

procedure TmncSession.Rollback;
begin
  if not Active then
    raise EmncException.Create('Oops you have not started yet!');
  Dec(FStartCount);
  case Connection.Mode of
    smMultiTransaction: DoRollback;
    smSingleTransaction:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        if (Connection.FStartCount > 0) then
          DoRollback;
      end;
    smEmulateTransaction:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        DoRollback;
        if (Connection.FStartCount > 0) then
          DoStart;
      end;
  end;
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
  DoInit;
  case Connection.Mode of
    smMultiTransaction: DoStart;
    smSingleTransaction,
    smEmulateTransaction:
      begin
        if Connection.FStartCount = 0 then
          DoStart;
        Inc(Connection.FStartCount);
      end;
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
  DoStop;
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
  Result := Add(Fields[Index], Value);
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

function TmncCustomColumns.ByName(vName: string): TmncCustomColumn;
begin
  Result := Find(vName);
  if Result = nil then
    raise EmncException.Create('Column "' + vName + '" not found');
end;

function TmncCustomColumns.IsExists(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

procedure TmncCustomColumns.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Clear;
  end;
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

function TmncFields.GetValue(Index: string): Variant;
begin
  Result := Field[Index].Value;
end;

procedure TmncFields.SetValue(Index: string; const Value: Variant);
begin
  Field[Index].Value := Value;
end;

function TmncFields.Find(vName: string): TmncCustomColumn;
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

function TmncColumns.Find(vName: string): TmncCustomColumn;
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

function TmncParams.GetValue(Index: string): Variant;
begin
  Result := Param[Index].Value;
end;

procedure TmncParams.SetValue(Index: string; const Value: Variant);
begin
  Param[Index].Value := Value;
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

function TmncCustomParams.Find(vName: string): TmncCustomColumn;
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

function TmncField.GetVariant: Variant;
begin
  Result := FValue;
end;

procedure TmncField.SetVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmncParamList }

constructor TmncParamList.Create;
begin
  inherited Create(False);
end;

{ TCustomField }

{ TmncColumn }

function TmncColumn.GetVariant: Variant;
begin
  Result := null;
  raise EmncException.Create('Field have no value, You must not use it, try use Current!') {$ifdef fpc}at get_caller_addr(get_frame){$endif};
end;

procedure TmncColumn.SetVariant(const Value: Variant);
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

function TmncParam.GetVariant: Variant;
begin
  Result := FValue;
end;

procedure TmncParam.SetVariant(const Value: Variant);
begin
  FValue := Value;
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

{ TmncCustomColumn }

procedure TmncCustomColumn.Clear;
begin
  Value := Null;
end;

function TmncCustomColumn.GetAsBoolean: Boolean;
begin
  if IsEmpty then
    Result := False
  else
    Result := AsInteger <> 0;
end;

function TmncCustomColumn.GetAsCurrency: Currency;
begin
  if IsEmpty or not VarIsNumeric(Value) then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomColumn.GetAsDate: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := DateOf(Result);
  end;
end;

function TmncCustomColumn.GetAsDateTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomColumn.GetAsInt64: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomColumn.GetAsInteger: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomColumn.GetAsString: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

function TmncCustomColumn.GetAsTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := TimeOf(Result);
  end;
end;

function TmncCustomColumn.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TmncCustomColumn.GetIsEmpty: Boolean;
begin
  Result := VarType(Value) in [varEmpty, varNull, varUnknown];
end;

function TmncCustomColumn.GetIsNull: Boolean;
begin
  Result := IsEmpty;
end;

function TmncCustomColumn.GetText: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

procedure TmncCustomColumn.SetAsNullString(const Value: string);
begin
  if Value = '' then
    Clear
  else
    AsString := Value;
end;

function TmncCustomColumn.GetAsHex: string;
var
  s: string;
begin
  s := GetAsString;
  SetLength(Result, Length(s) * 2);
  BinToHex(PChar(s), @Result[1], Length(s));
end;

function TmncCustomColumn.GetAsText: string;
begin
  if (IsBlob) and (BlobType = blobText) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TmncCustomColumn.SetAsHex(const AValue: string);
var
  s: string;
begin
  SetLength(s, Length(Value) div 2);
  HexToBin(PChar(AValue), @s[1], Length(s));
  AsString := s;
end;

procedure TmncCustomColumn.SetAsBoolean(const Value: Boolean);
begin
  AsInteger := Ord(Value);
end;

procedure TmncCustomColumn.SetAsCurrency(const Value: Currency);
begin
  Self.Value := Value;
end;

procedure TmncCustomColumn.SetAsDate(const Value: TDateTime);
begin
  Self.Value := DateOf(Value);
end;

procedure TmncCustomColumn.SetAsDateTime(const Value: TDateTime);
begin
  Self.Value := Value;
end;

procedure TmncCustomColumn.SetAsInt64(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmncCustomColumn.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmncCustomColumn.SetAsString(const Value: string);
begin
  Self.Value := Value;
end;

function TmncCustomColumn.GetAsAnsiString: ansistring;
begin
  Result := Utf8ToAnsi(GetAsString);
end;

procedure TmncCustomColumn.SetAsAnsiString(const Value: ansistring);
begin
  //fpc not auto convert because string type it same with ansistring
  SetAsString(AnsiToUtf8(Value));
end;

function TmncCustomColumn.GetAsWideString: widestring;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmncCustomColumn.SetAsText(const AValue: string);
begin
  if (IsBlob) and (BlobType = blobText) then
    AsHex := AValue
  else
    AsString := AValue;
end;

procedure TmncCustomColumn.SetAsWideString(const Value: widestring);
begin
  SetAsString(Value);
end;

function TmncCustomColumn.GetAsUtf8String: UTF8String;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmncCustomColumn.SetAsUtf8String(const Value: UTF8String);
begin
  SetAsString(Value);
end;

procedure TmncCustomColumn.SetAsTime(const Value: TDateTime);
begin
  Self.Value := TimeOf(Value);
end;

procedure TmncCustomColumn.SetAsTrimString(const Value: string);
begin
  AsString := Trim(Value);
end;

{ TmncCustomColumns }

function TmncCustomColumns.GetItem(Index: Integer): TmncCustomColumn;
begin
  Result := (inherited Items[Index]) as TmncCustomColumn;
end;

function TmncCustomColumns.Add(AColumn: TmncCustomColumn): Integer;
begin
  Result := inherited Add(AColumn);
end;

{ TmncCustomFields }

function TmncCustomFields.GetItem(Index: Integer): TmncCustomField;
begin
  Result := (inherited Items[Index]) as TmncCustomField;
end;

function TmncCustomFields.GetField(Index: string): TmncCustomField;
begin
  Result := ByName(Index) as TmncCustomField;
end;

initialization
finalization
  FreeAndNil(FConnectionLock);
end.
