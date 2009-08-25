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
  smNone: No transactions support
  smTransactions: all session have the same transacion like PG and SQLite
  smMultiTransactions: every session have transacion like as Firebird
}
  TmncTransactionMode = (smNone, smSingleTransactions, smMultiTransactions);

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
    procedure DoStart; virtual; abstract;
    procedure DoCommit; virtual; abstract;
    procedure DoRollback; virtual; abstract;
    procedure DoStop; virtual; abstract;
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

  { TmncCustomField }

  TmncCustomField = class(TObject)
  private
    FName: string;
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
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;

    function GetIsNull: Boolean;
    function GetIsEmpty: Boolean; virtual;
    function GetText: string; virtual;
    property Text: string read GetText;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNull: Boolean read GetIsNull;
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

  TmncCustomFieldClass = class of TmncCustomField;

  { TmncCustomFields }

  TmncCustomFields = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncCustomField;
  protected
    function FindField(vName: string): TmncCustomField; virtual; abstract;
  public
    function Add(AField: TmncCustomField): Integer; overload;
    function FieldByName(vName: string): TmncCustomField;
    property Items[Index: Integer]: TmncCustomField read GetItem;
  end;

  { TmncField }

  TmncField = class(TmncCustomField)
  private
    FIndex: Integer;
//    FSize: Integer;
  protected
    function GetVariant: Variant; override;
    procedure SetVariant(const Value: Variant); override;
  published
    property Index: Integer read FIndex write FIndex;
    //property Size: Integer read FSize write FSize;//todo not yet
  end;

  TmncFieldClass = class of TmncField;

  TmncFields = class(TmncCustomFields)
  private
    function GetItem(Index: Integer): TmncField;
  protected
    function FindField(vName: string): TmncCustomField; override;
  public
    function Add(Index: Integer; Name: string; FieldClass: TmncFieldClass = nil): TmncField; overload;
    function Add(Name: string): TmncField; overload;
    property Items[Index: Integer]: TmncField read GetItem; default;
  end;

{
  Values
}

  TmncRecordField = class(TmncCustomField)
  private
  protected
  public
  published
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
  end;

  { TmncCustomRecord }

  TmncCustomRecord = class(TmncCustomFields)
  private
    function GetItem(Index: Integer): TmncRecordField;
    function GetField(Index: string): TmncRecordField;
  public
    property Items[Index: Integer]: TmncRecordField read GetItem;
    property Field[Index: string]: TmncRecordField read GetField; default;
  end;

{
  Record
}

  TmncRecordItem = class(TmncRecordField)
  private
    FValue: Variant;
    FField: TmncField;
  protected
    function GetVariant: Variant; override;
    procedure SetVariant(const Value: Variant); override;
  public
    constructor Create(vField: TmncField);
  published
    property Field: TmncField read FField write FField;
  end;

  { TmncRecord }

  TmncRecord = class(TmncCustomRecord)
  private
    FFields: TmncFields;
    FRowID: Integer;
    function GetItem(Index: Integer): TmncRecordItem;
    function GetField(Index: string): TmncRecordItem;
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
  protected
  public
    constructor Create(vFields: TmncFields);
    function FindField(vName: string): TmncCustomField; override;
    function FieldByName(vName: string): TmncRecordItem;
    function Add(Field: TmncField; Value: Variant): TmncRecordItem; overload;
    function Add(Field: TmncField): TmncRecordItem; overload;
    function Add(FieldIndex: Integer; Value: Variant): TmncRecordItem; overload;
    property Fields: TmncFields read FFields;
    property Items[Index: Integer]: TmncRecordItem read GetItem;
    property Value[Index: string]: Variant read GetValue write SetValue;
    property Field[Index: string]: TmncRecordItem read GetField; default;
    property RowID: Integer read FRowID write FRowID default 0; //most of SQL engines have this value
  end;

{
  Params
}

  TmncParamItem = class(TmncRecordField)
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

  TmncCustomParams = class(TmncCustomRecord)
  private
  protected
    function GetParam(Index: string): TmncParamItem;
    function GetItem(Index: Integer): TmncParamItem;
  public
    procedure Clear; override;
    function FindField(vName: string): TmncCustomField; override;
    function ParamByName(vName: string): TmncParamItem;
    property Items[Index: Integer]: TmncParamItem read GetItem;
    property Param[Index: string]: TmncParamItem read GetParam; default;
  end;

  TmncParams = class(TmncCustomParams)
  private
    function GetValue(Index: string): Variant;
    procedure SetValue(Index: string; const Value: Variant);
  published
  public
    function Add(Name: string): TmncParamItem;
    function AddExists(Name: string): TmncParamItem;
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
    destructor Destroy; override;
    property Session: TmncSession read FSession write SetSession;
    property Active: Boolean read GetActive write SetActive;
  end;

//  TmncCommandStatus = (csSQLEmpty, csSQLCommand, csSQLRows);

  { TmncCommand }

  TmncCommand = class(TmncLinkObject)
  private
    FCurrent: TmncRecord;
    FParams: TmncParams;
    FPrepared: Boolean;
    FNextOnExecute: Boolean;
    FFields: TmncFields;
    FParamList: TmncParamList;
    procedure SetRequest(const Value: TStrings);
    procedure SetFields(const Value: TmncFields);
    procedure SetCurrent(const Value: TmncRecord);
    procedure SetParams(const Value: TmncParams);
    function GetField(Index: string): TmncRecordItem;
    function GetParam(Index: string): TmncParamItem;
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
    constructor Create(vSession: TmncSession);
    destructor Destroy; override;
    procedure Prepare;
    function Execute: Boolean;
    procedure Close;
    function Next: Boolean;
    function EOF: Boolean;
    procedure Clear; virtual;
    procedure Commit;
    procedure Rollback;
    function ReleaseCurrent: TmncRecord;
    function ReleaseParams: TmncParams;
    function FieldIsExist(Name: string): Boolean;
    property NextOnExecute: Boolean read FNextOnExecute write FNextOnExecute default True;
    property Prepared: Boolean read FPrepared;
    property Params: TmncParams read FParams write SetParams;
    property Fields: TmncFields read FFields write SetFields;
    property Current: TmncRecord read FCurrent write SetCurrent;
    property Param[Index: string]: TmncParamItem read GetParam;
    property Field[Index: string]: TmncRecordItem read GetField;
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

constructor TmncCommand.Create(vSession: TmncSession);
begin
  inherited Create;
  Session := vSession;
  FRequest := TStringList.Create;
  (FRequest as TStringList).OnChange := DoRequestChanged;

  FFields := TmncFields.Create;
  FParams := TmncParams.Create;
  FParamList := TmncParamList.Create;
  FNextOnExecute := True;
end;

destructor TmncCommand.Destroy;
begin
  Active := False;
  Session := nil;//already in Linked but must be sure before free other objects
  FreeAndNil(FRequest);
  FreeAndNil(FCurrent);
  FreeAndNil(FParamList);
  FreeAndNil(FParams);
  FreeAndNil(FFields);
  inherited;
end;

procedure TmncCommand.DoRequestChanged(Sender: TObject);
begin
  if Active then
    Close;
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
  Result := Fields.FindField(Name) <> nil;
end;

function TmncCommand.GetField(Index: string): TmncRecordItem;
begin
  if not Prepared then
    Prepare;
  if Current <> nil then
    Result := Current.Field[Index]
  else
    raise EmncException.Create('Current record not found');
end;

function TmncCommand.GetParam(Index: string): TmncParamItem;
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

function TmncCommand.ReleaseCurrent: TmncRecord;
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
  DoCommit;
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

procedure TmncCommand.SetCurrent(const Value: TmncRecord);
begin
  if FCurrent <> Value then
  begin
    FreeAndNil(FCurrent);
    FCurrent := Value;
  end;
end;

procedure TmncCommand.SetFields(const Value: TmncFields);
begin
  FFields.Assign(Value);
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
    smMultiTransactions: DoCommit;
    smSingleTransactions:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        if Connection.FStartCount = 0 then
          DoCommit;
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

procedure TmncSession.Rollback;
begin
  if not Active then
    raise EmncException.Create('Oops you have not started yet!');
  Dec(FStartCount);
  case Connection.Mode of
    smMultiTransactions: DoRollback;
    smSingleTransactions:
      begin
        if Connection.FStartCount = 0 then
          raise EmncException.Create('Connection not started yet!');
        Dec(Connection.FStartCount);
        if Connection.FStartCount = 0 then
          DoRollback;
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
  case Connection.Mode of
    smMultiTransactions: DoStart;
    smSingleTransactions:
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

function TmncRecord.Add(Field: TmncField; Value: Variant): TmncRecordItem;
begin
  Result := TmncRecordItem.Create(Field);
  Result.Value := Value;
  Result.Field := Field;
  inherited Add(Result);
end;

function TmncRecord.Add(FieldIndex: Integer; Value: Variant): TmncRecordItem;
begin
  Result := Add(Fields[FieldIndex], Value);
end;

function TmncRecord.FieldByName(vName: string): TmncRecordItem;
begin
  Result := inherited FieldByName(vName) as TmncRecordItem;
end;

function TmncRecord.Add(Field: TmncField): TmncRecordItem;
begin
  Result := TmncRecordItem.Create(Field);
  Result.Field := Field;
  inherited Add(Result);
end;

constructor TmncRecord.Create(vFields: TmncFields);
begin
  inherited Create;
  FFields := vFields;
  if FFields = nil then
    raise EmncException.Create('vFields must be not nil');
end;

function TmncCustomFields.FieldByName(vName: string): TmncCustomField;
begin
  Result := FindField(vName);
  if Result = nil then
    raise EmncException.Create('Field "' + vName + '" not found');
end;

function TmncRecord.FindField(vName: string): TmncCustomField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Field.Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncRecord.GetField(Index: string): TmncRecordItem;
begin
  Result := FieldByName(Index);
end;

function TmncRecord.GetItem(Index: Integer): TmncRecordItem;
begin
  Result := (inherited Items[Index]) as TmncRecordItem;
end;

function TmncRecord.GetValue(Index: string): Variant;
begin
  Result := Field[Index].Value;
end;

procedure TmncRecord.SetValue(Index: string; const Value: Variant);
begin
  Field[Index].Value := Value;
end;

{ TmncFields }

function TmncFields.Add(Index: Integer; Name: string; FieldClass: TmncFieldClass): TmncField;
begin
  if FieldClass = nil then
    FieldClass := TmncField;
  Result := FieldClass.Create;
  Result.Index := Index;
  Result.Name := Name;
  inherited Add(Result);
end;

function TmncFields.Add(Name: string): TmncField;
begin
  Result := Add(Count, Name);
end;

function TmncFields.FindField(vName: string): TmncCustomField;
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

function TmncFields.GetItem(Index: Integer): TmncField;
begin
  Result := inherited Items[Index] as TmncField;
end;

{ TmncRecordItem }

constructor TmncRecordItem.Create(vField: TmncField);
begin
  inherited Create;
  FField := vField;
end;

function TmncParams.Add(Name: string): TmncParamItem;
begin
  Result := TmncParamItem.Create;
  Result.Name := Name;
  inherited Add(Result);
end;

function TmncParams.AddExists(Name: string): TmncParamItem;
begin
  Result := FindField(Name) as TmncParamItem;
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

function TmncCustomParams.ParamByName(vName: string): TmncParamItem;
begin
  Result := FindField(vName) as TmncParamItem;
  if Result = nil then
    raise EmncException.Create('Param ' + vName + ' not found');
end;

function TmncCustomParams.FindField(vName: string): TmncCustomField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i] as TmncParamItem;
      break;
    end;
  end;
end;

function TmncCustomParams.GetParam(Index: string): TmncParamItem;
begin
  Result := ParamByName(Index);
end;

function TmncCustomParams.GetItem(Index: Integer): TmncParamItem;
begin
  Result := inherited Items[Index] as TmncParamItem;
end;

function TmncRecordItem.GetVariant: Variant;
begin
  Result := FValue;
end;

procedure TmncRecordItem.SetVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmncParamList }

constructor TmncParamList.Create;
begin
  inherited Create(False);
end;

{ TCustomField }

{ TmncField }

function TmncField.GetVariant: Variant;
begin
  Result := null;
  raise EmncException.Create('Field have no value, You must not use it, try use Current!') {$ifdef fpc}at get_caller_addr(get_frame){$endif};
end;

procedure TmncField.SetVariant(const Value: Variant);
begin
  raise EmncException.Create('Field have no value, You must not use it, try use Current!');
end;

{ TmncParamItem }

procedure TmncParamItem.AllocBuffer(var P; Size: Integer);
begin
  FreeBuffer;
  FBufferSize := Size;
  if Size > 0 then
  begin
    FBuffer := AllocMem(FBufferSize);
    Move(P, FBuffer^, Size);
  end;
end;

destructor TmncParamItem.Destroy;
begin
  FreeBuffer;
  inherited;
end;

procedure TmncParamItem.FreeBuffer;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
end;

function TmncParamItem.GetBufferAllocated: Boolean;
begin
  Result := Buffer <> nil;
end;

function TmncParamItem.GetVariant: Variant;
begin
  Result := FValue;
end;

procedure TmncParamItem.SetVariant(const Value: Variant);
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

{ TmncCustomField }

procedure TmncCustomField.Clear;
begin
  Value := Null;
end;

function TmncCustomField.GetAsBoolean: Boolean;
begin
  if IsEmpty then
    Result := False
  else
    Result := AsInteger <> 0;
end;

function TmncCustomField.GetAsCurrency: Currency;
begin
  if IsEmpty or not VarIsNumeric(Value) then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomField.GetAsDate: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := DateOf(Result);
  end;
end;

function TmncCustomField.GetAsDateTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomField.GetAsInt64: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomField.GetAsInteger: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmncCustomField.GetAsString: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

function TmncCustomField.GetAsTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := TimeOf(Result);
  end;
end;

function TmncCustomField.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TmncCustomField.GetIsEmpty: Boolean;
begin
  Result := VarType(Value) in [varEmpty, varNull, varUnknown];
end;

function TmncCustomField.GetIsNull: Boolean;
begin
  Result := IsEmpty;
end;

function TmncCustomField.GetText: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

procedure TmncCustomField.SetAsNullString(const Value: string);
begin
  if Value = '' then
    Clear
  else
    AsString := Value;
end;

procedure TmncCustomField.SetAsBoolean(const Value: Boolean);
begin
  AsInteger := Ord(Value);
end;

procedure TmncCustomField.SetAsCurrency(const Value: Currency);
begin
  Self.Value := Value;
end;

procedure TmncCustomField.SetAsDate(const Value: TDateTime);
begin
  Self.Value := DateOf(Value);
end;

procedure TmncCustomField.SetAsDateTime(const Value: TDateTime);
begin
  Self.Value := Value;
end;

procedure TmncCustomField.SetAsInt64(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmncCustomField.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmncCustomField.SetAsString(const Value: string);
begin
  Self.Value := Value;
end;

function TmncCustomField.GetAsAnsiString: ansistring;
begin
  Result := Utf8ToAnsi(GetAsString);
end;

procedure TmncCustomField.SetAsAnsiString(const Value: ansistring);
begin
  //fpc not auto convert because string type it same with ansistring
  SetAsString(AnsiToUtf8(Value));
end;

function TmncCustomField.GetAsWideString: widestring;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmncCustomField.SetAsWideString(const Value: widestring);
begin
  SetAsString(Value);
end;

function TmncCustomField.GetAsUtf8String: UTF8String;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmncCustomField.SetAsUtf8String(const Value: UTF8String);
begin
  SetAsString(Value);
end;

procedure TmncCustomField.SetAsTime(const Value: TDateTime);
begin
  Self.Value := TimeOf(Value);
end;

procedure TmncCustomField.SetAsTrimString(const Value: string);
begin
  AsString := Trim(Value);
end;

{ TmncCustomFields }

function TmncCustomFields.GetItem(Index: Integer): TmncCustomField;
begin
  Result := (inherited Items[Index]) as TmncCustomField;
end;

function TmncCustomFields.Add(AField: TmncCustomField): Integer;
begin
  Result := inherited Add(AField);
end;

{ TmncCustomRecord }

function TmncCustomRecord.GetItem(Index: Integer): TmncRecordField;
begin
  Result := (inherited Items[Index]) as TmncRecordField;
end;

function TmncCustomRecord.GetField(Index: string): TmncRecordField;
begin
  Result := FieldByName(Index) as TmncRecordField;
end;

finalization
  FreeAndNil(FConnectionLock);
end.

