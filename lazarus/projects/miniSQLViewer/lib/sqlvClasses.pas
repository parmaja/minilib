unit sqlvClasses;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Classes, Controls, Dialogs, Contnrs,
  mnXMLRttiProfile, mncCSVExchanges,
  mncSchemas, mnUtils, mnParams,
  sqlvConsts, sqlvSessions, ImgList;

const
  IMG_UNKOWN = 0;
  IMG_DATABASE = 1;
  IMG_DOMAIN = 2;
  IMG_GENERATOR = 3;
  IMG_EXCEPTION = 4;
  IMG_TABLE = 5;
  IMG_VIEW = 6;
  IMG_PROCEDURE = 7;
  IMG_FUNCTION = 8;
  IMG_TRIGGER = 9;
  IMG_INDEX = 10;
  IMG_FIELD = 11;
  IMG_DATA = 12;
  IMG_COMMAND = 13;

type
  EsqlvException = class(Exception);

  TSchemaInfo = record
    Name: string;
    Value: string;
  end;

  { TsqlvSetting }

  TsqlvSetting = class(TmnXMLProfile)
  private
    FCSVANSIContents: Boolean;
    FCSVDelimiterChar: Char;
    FCSVHeader: TCSVIEHeader;
    FCSVQuoteChar: Char;
    FOpenSaveDialogFilters: string;
    FCacheSchemas: Boolean;
    FLogoutSQL: string;
    FLoginSQL: string;
    FInternalLogoutSQL: string;
    FInternalLoginSQL: string;
  public
    constructor Create;
    property InternalLoginSQL:string read FInternalLoginSQL write FInternalLoginSQL;
    property InternalLogoutSQL:string read FInternalLogoutSQL write FInternalLogoutSQL;
  published
    property CacheSchemas: Boolean read FCacheSchemas write FCacheSchemas default False;
    property OpenSaveDialogFilters:string read FOpenSaveDialogFilters write FOpenSaveDialogFilters;
    property LoginSQL: string read FLoginSQL write FLoginSQL;
    property LogoutSQL: string read FLogoutSQL write FLogoutSQL;
    property CSVQuoteChar: Char read FCSVQuoteChar write FCSVQuoteChar default '"';
    property CSVDelimiterChar: Char read FCSVDelimiterChar write FCSVDelimiterChar default ';';
    property CSVHeader: TCSVIEHeader read FCSVHeader write FCSVHeader default hdrNone;
    property CSVANSIContents: Boolean read FCSVANSIContents write FCSVANSIContents default False;
  end;

  TsqlvNode = class;

  { TsqlvStackItem }

  TsqlvStackItem = class(TObject)
  private
    FGroup: string;
    FName: string;
    FValue: string;
    FSelect: string;
    FNode: TsqlvNode;
  public
    property Group: string read FGroup write FGroup;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Select: string read FSelect write FSelect;

    property Node: TsqlvNode read FNode write FNode;
  end;

  { TsqlvStack }

  TsqlvStack = class(TObjectList)
  private
    function GetCurrent: TsqlvStackItem;
    function GetItem(Index: Integer): TsqlvStackItem;
    function GetValues(Index: string): string;
    procedure SetItem(Index: Integer; const Value: TsqlvStackItem);
  public
    constructor Create;
    function Find(const Name: string): TsqlvStackItem;
    function Add(Param: TsqlvStackItem): Integer; overload;
    function Push(Group, Name: string; Value, Select: string): TsqlvStackItem;
    procedure Pop;
    procedure Top; //pop all to top
    procedure Trim(ToCount: Integer); //Similar to SetCount
    property Current: TsqlvStackItem read GetCurrent;
    property Items[Index: Integer]: TsqlvStackItem read GetItem write SetItem;
    property Values[Index: string]: string read GetValues; default;
  end;

  TsqlvNodes = class;

{
  nsDefault: Default node for execute parent when parent can not parent
  nsCommand: It is command not SQL member
  nsEditor: Show a script in SQL editor like triggers or stored prpocedures
  nsButton: Make it visible as button or menu in gui form
  nsNeedSession: Enum only when session is active
}

  TsqlvNodeStyle = set of (nsDefault, nsGroup, nsCommand, nsEditor, nsButton, nsNeedSession);

  { TsqlvNode }

  TsqlvNode = class(TObject)
  private
    FItem: string;
    FGroup: string;
    FName: string;
    FStyle: TsqlvNodeStyle;
    FTitle: string;
    FKind: TschmKind;
    FImageIndex: TImageIndex;
  protected
    function GetCanExecute: Boolean; virtual;
    procedure DoExecute(const Value: string; Params: TsqlvStack); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowProperty; virtual;
    procedure Execute(const Value: string; vStack: TsqlvStack; FallDefault: Boolean = False);
    procedure Execute(const Value: string);
    procedure Enum(Nodes: TsqlvNodes);
    procedure EnumDefaults(Nodes: TsqlvNodes);
    procedure EnumHeader(Header: TStringList); virtual;
    procedure EnumSchema(vItems: TmncSchemaItems; const MemberName: string = ''); virtual; abstract;
    property CanExecute: Boolean read GetCanExecute;
    property Group: string read FGroup write FGroup; //Group is parent node like Tabkes.Group = 'Database'
    property Name: string read FName write FName; //Name = 'Tables'
    property Item: string read FItem write FItem; //Item name eg  Tables.Item = 'Table'
    property Kind: TschmKind read FKind write FKind default sokNone;
    property Style: TsqlvNodeStyle read FStyle write FStyle;
    property Title: string read FTitle write FTitle;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  TsqlvNodeClass = class of TsqlvNode;

  { TsqlvCustomNodes }

  TsqlvCustomNodes = class(TObjectList)
  private
    function GetItem(Index: Integer): TsqlvNode;
    procedure SetItem(Index: Integer; const Value: TsqlvNode);
  public
    procedure Enum(GroupName: string; Nodes: TsqlvNodes; SessionActive: Boolean; OnlyDefaults: Boolean = False); overload;
    function Find(const Name: string): TsqlvNode; deprecated;
    function Find(const Group, Name: string): TsqlvNode;
    property Items[Index: Integer]: TsqlvNode read GetItem write SetItem; default;
  end;

  { TsqlvNodes }

  TsqlvNodes = class(TsqlvCustomNodes)
  public
    constructor Create; virtual;
    function Add(vNode:TsqlvNode): Integer;
  end;

  { TsqlvCustomHistoryItem }

  TsqlvCustomHistoryItem = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TsqlvHistory }

  TsqlvCustomHistory = class(TObjectList)
  private
    FIndex: Integer;
    FMaxCount: Integer;
    FOnChanged: TNotifyEvent;
    function GetCurrent: TsqlvCustomHistoryItem;
    function GetItem(Index: Integer): TsqlvCustomHistoryItem;
  protected
    function CreateItem: TsqlvCustomHistoryItem; virtual; abstract;
  public
    constructor Create;
    function Add(History: TsqlvCustomHistoryItem): Integer;
    function HaveBackward: Boolean;
    function Backward: Boolean;
    function HaveForward: Boolean;
    function Forward: Boolean;
    procedure Changed; virtual;
    property Current: TsqlvCustomHistoryItem read GetCurrent;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Items[Index: Integer]: TsqlvCustomHistoryItem read GetItem; default;
    property Index: Integer read FIndex write FIndex;
  end;

  TsqlvSQLHistoryItem = class(TsqlvCustomHistoryItem)
  private
    FStrings: TStringList;
    function GetText: string;
    procedure SetText(AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Text: string read GetText write SetText;
    property Strings: TStringList read FStrings;
  end;

  { TsqlvSQLHistory }

  TsqlvSQLHistory = class(TsqlvCustomHistory)
  private
    function GetCurrent: TsqlvSQLHistoryItem;
  protected
    function CreateItem: TsqlvCustomHistoryItem; override;
  public
    procedure Add(const Text: string; Silent: Boolean);
    property Current: TsqlvSQLHistoryItem read GetCurrent;
  end;

  { TsqlvNodeHistoryItem }

  TsqlvNodeHistoryItem = class(TsqlvCustomHistoryItem)
  private
    FNode: TsqlvNode;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Node: TsqlvNode read FNode write FNode;
  end;

  TsqlvNodeHistory = class(TsqlvCustomHistory)
  private
    function GetCurrent: TsqlvNodeHistoryItem;
  protected
    function CreateItem: TsqlvCustomHistoryItem; override;
  public
    procedure Add(const Node: TsqlvNode; Silent: Boolean);
    property Current: TsqlvNodeHistoryItem read GetCurrent;
  end;

  { TsqlvEngine }

  TsqlvEngine = class(TsqlvCustomNodes)
  private
    FSession: TsqlvSession;
    FSetting: TsqlvSetting;
    FRecents: TStringList;
    FStack: TsqlvStack;
    FWorkPath: string;
    FHistory: TsqlvNodeHistory;
    FSQLHistory: TsqlvSQLHistory;
    procedure SetWorkPath(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    procedure LoadRecents;
    procedure SaveRecents;
    procedure Run(vStack: TsqlvStack);
    procedure Run;
    procedure Run(vGroup, vName, vValue: string; vSelect: string = '');
    procedure RegisterFilter(Filter: string);
    procedure RegisterViewer(Classes: array of TsqlvNodeClass);
    procedure AddRecent(Name:string);
    procedure LoadFile(FileName:string; Strings:TStrings);
    procedure SaveFile(FileName:string; Strings:TStrings);
    function GetAllSupportedFiles: string;
    property Setting: TsqlvSetting read FSetting;
    property Recents: TStringList read FRecents;
    property Session: TsqlvSession read FSession;
    property WorkPath :string read FWorkPath write SetWorkPath;
    property History: TsqlvNodeHistory read FHistory;
    property Stack: TsqlvStack read FStack;
    property SQLHistory: TsqlvSQLHistory read FSQLHistory;
  end;

var
  AddOpenSaveDialogFilters: string = '';

function sqlvEngine: TsqlvEngine;

implementation

var
  FsqlvEngine: TsqlvEngine = nil;

function sqlvEngine: TsqlvEngine;
begin
  if FsqlvEngine = nil then
    FsqlvEngine := TsqlvEngine.Create;
  Result := FsqlvEngine;
end;

{ TsqlvStack }

function TsqlvStack.GetCurrent: TsqlvStackItem;
begin
  Result := Last as TsqlvStackItem;
end;

function TsqlvStack.GetItem(Index: Integer): TsqlvStackItem;
begin
  Result := inherited Items[Index] as TsqlvStackItem;
end;

function TsqlvStack.GetValues(Index: string): string;
var
  aItem: TsqlvStackItem;
begin
  if Self = nil then
    Result := ''
  else
  begin
    aItem := Find(Index);
    if aItem = nil then
      Result := ''
    else
      Result := aItem.Value;
  end;
end;

procedure TsqlvStack.SetItem(Index: Integer; const Value: TsqlvStackItem);
begin
  inherited Items[Index] := Value;
end;

constructor TsqlvStack.Create;
begin
  inherited Create(True);
end;

function TsqlvStack.Find(const Name: string): TsqlvStackItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsqlvStack.Add(Param: TsqlvStackItem): Integer;
begin
  Result := inherited Add(Param);
end;

function TsqlvStack.Push(Group, Name: string; Value, Select: string): TsqlvStackItem;
begin
  Result := TsqlvStackItem.Create;
  Result.Group := Group;
  Result.Name := Name;
  Result.Value := Value;
  Result.Select := Select;
  Add(Result);
end;

procedure TsqlvStack.Trim(ToCount: Integer);
begin
  SetCount(ToCount);
end;

procedure TsqlvStack.Pop;
begin
  if Count > 0 then
    Delete(Count - 1);
end;

procedure TsqlvStack.Top;
begin
  if Count > 0  then
    SetCount(1);
end;

{ TsqlvNodeHistory }

function TsqlvNodeHistory.GetCurrent: TsqlvNodeHistoryItem;
begin
  REsult := inherited GetCurrent as TsqlvNodeHistoryItem;
end;

function TsqlvNodeHistory.CreateItem: TsqlvCustomHistoryItem;
begin
  Result := TsqlvNodeHistoryItem.Create;
end;

procedure TsqlvNodeHistory.Add(const Node: TsqlvNode; Silent: Boolean);
var
  i: Integer;
  aHistory: TsqlvNodeHistoryItem;
begin
  aHistory := TsqlvNodeHistoryItem.Create;
  aHistory.Node := Node;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

{ TsqlvNodeHistoryItem }

constructor TsqlvNodeHistoryItem.Create;
begin
  inherited Create;
end;

destructor TsqlvNodeHistoryItem.Destroy;
begin
  inherited;
end;

{ TsqlvCustomHistoryItem }

constructor TsqlvCustomHistoryItem.Create;
begin
  inherited Create;
end;

destructor TsqlvCustomHistoryItem.Destroy;
begin
  inherited;
end;

{ TsqlvSQLHistory }

function TsqlvSQLHistory.CreateItem: TsqlvCustomHistoryItem;
begin
  Result := TsqlvSQLHistoryItem.Create;
end;

{ TsqlvCustomHistoryItem }

function TsqlvSQLHistoryItem.GetText: string;
begin
  Result := FStrings.Text;
end;

procedure TsqlvSQLHistoryItem.SetText(AValue: string);
begin
  FStrings.Text := AValue;
end;

constructor TsqlvSQLHistoryItem.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TsqlvSQLHistoryItem.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TsqlvEngine.RegisterViewer(Classes: array of TsqlvNodeClass);
var
  i: Integer;
begin
  for i := 0 to Length(Classes) - 1 do
    Add(Classes[i].Create);
end;

procedure TsqlvEngine.AddRecent(Name: string);
var
  i: Integer;
begin
  i := Recents.IndexOf(Name);
  if i > 0 then
    Recents.Move(i, 0)
  else if i < 0 then
    Recents.Insert(0, Name);
  if Recents.Count > 10 then
    Recents.Capacity := 10;
end;

procedure TsqlvEngine.LoadFile(FileName: string; Strings: TStrings);
begin
  if FileExists(WorkPath + FileName) then
    Strings.LoadFromFile(WorkPath + FileName);
end;

procedure TsqlvEngine.SaveFile(FileName: string; Strings: TStrings);
begin
  Strings.SaveToFile(WorkPath + FileName);
end;

procedure TsqlvEngine.SaveSetting;
begin
  FSetting.SaveToFile(WorkPath + 'sqlviewer.config');
end;

procedure TsqlvEngine.LoadRecents;
begin
  if FileExists(WorkPath + 'recents.ini') then
    FRecents.LoadFromFile(WorkPath + 'recents.ini');
end;

procedure TsqlvEngine.SaveRecents;
begin
  FRecents.SaveToFile(WorkPath + 'recents.ini');
end;

procedure TsqlvEngine.Run(vStack: TsqlvStack);
var
  aNodes: TsqlvNodes;
begin
  if (vStack = nil) and (vStack.Count = 0) then
    raise Exception.Create('Stack is empty');
  with vStack.Current do
  begin
    if Node = nil then
    begin
      Node := Find(Group, Name);
      if (Node = nil) then
      begin
        aNodes := TsqlvNodes.Create;
        try
          Enum(Name, aNodes, True);
          if aNodes.Count > 0 then
            Node := aNodes[0];
        finally
          aNodes.Free;
        end;
      end;
    end;
    if Node <> nil then
      Node.Execute(Value, vStack, True);
  end;
end;

procedure TsqlvEngine.Run;
begin
  Run(Stack);
end;

procedure TsqlvEngine.Run(vGroup, vName, vValue: string; vSelect: string);
begin
  Stack.Push(vGroup, vName, vValue, vSelect);
  Run(Stack);
end;

procedure TsqlvEngine.SetWorkPath(const Value: string);
begin
  if FWorkPath <> Value then
  begin
    FWorkPath := Value;
    LoadSetting;
    LoadRecents;
  end;
end;

{ TsqlvNodes }

procedure TsqlvCustomNodes.Enum(GroupName: string; Nodes: TsqlvNodes; SessionActive: Boolean; OnlyDefaults:Boolean = False);
var
  i: Integer;
  aDefault: Integer;
  c: Integer;
begin
  inherited;
  aDefault := -1;
  c := 0;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Group, GroupName) and (not OnlyDefaults or (nsDefault in Items[i].Style)) and (SessionActive or not (nsNeedSession in Items[i].Style)) then
    begin
      if (aDefault < 0) and (nsDefault in Items[i].Style) then
        aDefault := c;
      Nodes.Add(Items[i]);
      Inc(c);
    end;
  end;
  if aDefault >= 0 then
    Nodes.Move(aDefault, 0);
end;

function TsqlvCustomNodes.Find(const Name: string): TsqlvNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsqlvCustomNodes.Find(const Group, Name: string): TsqlvNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Group, Items[i].Group) and SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsqlvCustomNodes.GetItem(Index: Integer): TsqlvNode;
begin
  Result := inherited Items[Index] as TsqlvNode;
end;

procedure TsqlvCustomNodes.SetItem(Index: Integer; const Value: TsqlvNode);
begin
  inherited Items[Index] := Value;
end;

{ TsqlvNodes }

constructor TsqlvNodes.Create;
begin
  inherited Create(False);
end;

function TsqlvNodes.Add(vNode: TsqlvNode): Integer;
begin
  Result := inherited Add(vNode);
end;

{ TsqlvNode }

constructor TsqlvNode.Create;
begin
  inherited;
  FImageIndex := -1;
end;

destructor TsqlvNode.Destroy;
begin
  inherited Destroy;
end;

{procedure TsqlvNode.Enum(Session: TsqlvSession; Strings: TStrings);
var
  i: Integer;
  aDefault: Integer;
  c:Integer;
begin
  inherited;
  aDefault := -1;
  c := 0;
  for i := 0 to sqlvClasses.Count - 1 do
  begin
    if SameText(sqlvClasses[i].Group, Name) then
    begin
      if (aDefault < 0) and sqlvClasses[i].IsDefault then
        aDefault := c;
      Strings.AddObject(sqlvClasses[i].Title, sqlvClasses[i]);
      Inc(c);
    end;
  end;
  if aDefault >= 0 then
    Strings.Move(aDefault, 0);
end;}

procedure TsqlvNode.Enum(Nodes: TsqlvNodes);
begin
  sqlvEngine.Enum(Name, Nodes, sqlvEngine.Session.IsActive);
end;

procedure TsqlvNode.EnumDefaults(Nodes: TsqlvNodes);
begin
  sqlvEngine.Enum(Name, Nodes, sqlvEngine.Session.IsActive, True);
end;

procedure TsqlvNode.EnumHeader(Header: TStringList);
begin
  Header.Clear;
  if Item = '' then
    Header.Add(Title)
  else
    Header.Add(Item);
end;

procedure TsqlvNode.Execute(const Value: string; vStack: TsqlvStack; FallDefault: Boolean = False);
var
  aNodes: TsqlvNodes;
begin
  if CanExecute then
    DoExecute(Value, vStack)
  else if FallDefault then
  begin
    //if Node.CanRunDefault then //TODO
    aNodes := TsqlvNodes.Create; //It only contain live nodes, not freed when free this list
    try
      EnumDefaults(aNodes);
{      if vValue = '' then
        vValue := Node.Name;}//TODO
      if aNodes.Count > 0 then
        aNodes[0].Execute(Value, vStack);
    finally
      aNodes.Free;
    end;
  end;
end;

procedure TsqlvNode.Execute(const Value: string);
var
  aParams: TsqlvStack;
begin
  aParams := TsqlvStack.Create;
  try
    DoExecute(Value, aParams);
  finally
    FreeAndNil(aParams);
  end;
end;

function TsqlvNode.GetCanExecute: Boolean;
begin
  Result := True;
end;

procedure TsqlvNode.DoExecute(const Value: string; Params: TsqlvStack);
begin
end;

procedure TsqlvNode.ShowProperty;
begin
end;

{ TsqlvClass }

constructor TsqlvEngine.Create;
begin
  inherited Create(True);
  FHistory := TsqlvNodeHistory.Create;
  FStack := TsqlvStack.Create;
  FSQLHistory := TsqlvSQLHistory.Create;
  FSetting := TsqlvSetting.Create;
  FRecents := TStringList.Create;
  FSession := TsqlvSession.Create;
end;


destructor TsqlvEngine.Destroy;
begin
  FreeAndNil(FSession);
  FreeAndNil(FSetting);
  FreeAndNil(FRecents);
  FreeAndNil(FHistory);
  FreeAndNil(FSQLHistory);
  inherited;
end;

procedure TsqlvEngine.RegisterFilter(Filter: string);
begin
  if AnsiPos('|', Filter) = 0 then
    raise EsqlvException.Create('Invalid sqlviewer filter');
  if AnsiPos(Filter, Setting.OpenSaveDialogFilters) = 0 then
  begin
    if Setting.OpenSaveDialogFilters <> '' then
      Setting.OpenSaveDialogFilters := Setting.OpenSaveDialogFilters + ';';
    Setting.OpenSaveDialogFilters := Setting.OpenSaveDialogFilters + Filter;
  end;
end;

function TsqlvEngine.GetAllSupportedFiles: string;
var
  aStrings: TStringList;
  s: string;
  i: Integer;
begin
  s := AddOpenSaveDialogFilters;
  if s <> '' then
    s := s + '|';
  s := s + Setting.OpenSaveDialogFilters;
  if s <> '' then
  begin
    Result := 'All Supported|*.sqlite';
    aStrings := TStringList.Create;
    StrToStrings(s, aStrings, ['|'], [' ', #13, #10]);
    for i := 0 to aStrings.Count - 1 do
    begin
      if Odd(i) then
        Result := Result + ';' + aStrings[i];
    end;
    aStrings.Free;
    Result := Result + '|' + sSqliteFilter + '|' + s + '|' + sAllFilesFilter;
  end
  else
    Result := Result + sSqliteFilter + '|' + sAllFilesFilter;
end;

procedure TsqlvEngine.LoadSetting;
begin
  FSetting.SafeLoadFromFile(WorkPath + 'sqlviewer.config');
end;

{ TsqlvCustomHistory }

function TsqlvCustomHistory.GetItem(Index: Integer): TsqlvCustomHistoryItem;
begin
  Result := inherited Items[Index] as TsqlvCustomHistoryItem;
end;

procedure TsqlvCustomHistory.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TsqlvCustomHistory.GetCurrent: TsqlvCustomHistoryItem;
begin
  if (Index < Count) and (FIndex >=0) then
    Result := Items[Index]
  else
    Result := nil;
end;

constructor TsqlvCustomHistory.Create;
begin
  inherited Create(True);
  Index := 0;
  MaxCount := 50;
end;

function TsqlvCustomHistory.Add(History: TsqlvCustomHistoryItem): Integer;
begin
  if (Count > MaxCount) and (Count > 0) then
  begin
    Delete(0);
    FIndex := FIndex - 1
  end;
  Result := inherited Add(History);
end;

function TsqlvSQLHistory.GetCurrent: TsqlvSQLHistoryItem;
begin
  Result := inherited GetCurrent as TsqlvSQLHistoryItem;
end;

procedure TsqlvSQLHistory.Add(const Text: string; Silent: Boolean);
var
  i: Integer;
  aHistory: TsqlvSQLHistoryItem;
begin
  if (Count > 0) then
  begin
    aHistory := Items[Count - 1] as TsqlvSQLHistoryItem;
    if (aHistory.Text = Text) then
      exit;//do not duplicate the last one
    if (Index < Count) and (Index >= 0) then
    begin
      aHistory := Items[Index] as TsqlvSQLHistoryItem;
      if (aHistory.Text = Text) then
        exit;//do not duplicate the current one
    end;
  end;
  aHistory := TsqlvSQLHistoryItem.Create;
  aHistory.Text := Text;
  //aHistory.Node := ANode;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

function TsqlvCustomHistory.HaveForward: Boolean;
begin
  Result := (Count > 0) and (Index < Count - 1);
end;

function TsqlvCustomHistory.HaveBackward: Boolean;
begin
  Result := (Count> 0) and (FIndex > 0);
end;

function TsqlvCustomHistory.Forward: Boolean;
begin
  Result := HaveForward;
  if Result then
  begin
    Index := FIndex + 1;
    Changed;
  end;
end;

function TsqlvCustomHistory.Backward: Boolean;
begin
  Result := HaveBackward;
  if Result then
  begin
    Index := FIndex - 1;
    Changed;
  end;
end;

{ TsqlvSetting }

constructor TsqlvSetting.Create;
begin
  inherited Create;
  FCSVQuoteChar := '"';
  FCSVDelimiterChar := ';';
end;

initialization
finalization
  FreeAndNil(FsqlvEngine);
end.

