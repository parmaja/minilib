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
  SysUtils, Variants, Classes, Controls,
  mnXMLRttiProfile, mnXMLStreams,
  Dialogs, Contnrs,
  mncSchemas, mnUtils,
  sqlvConsts, sqlvSessions,
  Menus, Buttons, ImgList;

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

  TsqlvSetting = class(TmnXMLProfile)
  private
    FOpenSaveDialogFilters: string;
    FCacheSchemas: Boolean;
    FLogoutSQL: string;
    FLoginSQL: string;
    FInternalLogoutSQL: string;
    FInternalLoginSQL: string;
  public
    property InternalLoginSQL:string read FInternalLoginSQL write FInternalLoginSQL;
    property InternalLogoutSQL:string read FInternalLogoutSQL write FInternalLogoutSQL;
  published
    property CacheSchemas:Boolean read FCacheSchemas write FCacheSchemas default False;
    property OpenSaveDialogFilters:string read FOpenSaveDialogFilters write FOpenSaveDialogFilters;
    property LoginSQL:string read FLoginSQL write FLoginSQL;
    property LogoutSQL:string read FLogoutSQL write FLogoutSQL;
  end;

  TsqlvNodes = class;

{
  nsDefault: Default node for execute parent when parent can not parent
  nsCommand: It is command not SQL member
  nsEditor: Show a script in SQL editor like triggers or stored prpocedures
  nsButton: Make it visible as button or menu in gui form
}

  TsqlvNodeStyle = set of (nsDefault, nsCommand, nsEditor, nsButton);

  { TsqlvNode }

  TsqlvNode = class(TObject)
  private
    FAttributes: TStringList;
    FGroup: string;
    FName: string;
    FStyle: TsqlvNodeStyle;
    FTitle: string;
    FKind: TschmKind;
    FImageIndex: TImageIndex;
  protected
    function GetCanExecute: Boolean; virtual;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowProperty; virtual;
    procedure Execute(const Value: string; Params: TmncParams = nil);
    procedure Enum(Nodes: TsqlvNodes);
    procedure EnumDefaults(Nodes: TsqlvNodes);
    procedure EnumHeader(Header: TStringList); virtual;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string = ''); virtual;
    property CanExecute: Boolean read GetCanExecute;
    property Group: string read FGroup write FGroup;
    property Name: string read FName write FName;
    property Attributes: TStringList read FAttributes write FAttributes;
    property Title: string read FTitle write FTitle;
    property Kind: TschmKind read FKind write FKind default sokNone;
    property Style:TsqlvNodeStyle read FStyle write FStyle;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  TsqlvNodeClass = class of TsqlvNode;

  { TsqlvCustomNodes }

  TsqlvCustomNodes = class(TObjectList)
  private
    function GetItem(Index: Integer): TsqlvNode;
    procedure SetItem(Index: Integer; const Value: TsqlvNode);
  public
    procedure Enum(Name: string; Nodes: TsqlvNodes; OnlyDefaults:Boolean = False); overload;
    function Find(const Name: string): TsqlvNode; overload;
    function Find(const Group, Name: string): TsqlvNode; overload;
    property Items[Index: Integer]: TsqlvNode read GetItem write SetItem; default;
  end;

  { TsqlvNodes }

  TsqlvNodes = class(TsqlvCustomNodes)
  public
    constructor Create; virtual;
    function Add(vNode:TsqlvNode): Integer;
  end;

  TsqlvHistoryItem = class(TObject)
    Text: string;
  end;

  { TsqlvHistory }

  TsqlvHistory = class(TObjectList)
  private
    FIndex: Integer;
    FMaxCount: Integer;
    FOnChanged: TNotifyEvent;
    function GetCurrent: TsqlvHistoryItem;
    function GetItem(Index: Integer): TsqlvHistoryItem;
  protected
  public
    constructor Create;
    function Add(History: TsqlvHistoryItem): Integer;
    procedure Add(const Text: string; Silent: Boolean);
    function HaveBackward: Boolean;
    function Backward: Boolean;
    function HaveForward: Boolean;
    function Forward: Boolean;
    procedure Changed; virtual;
    property Items[Index: Integer]: TsqlvHistoryItem read GetItem; default;
    property Index: Integer read FIndex write FIndex;
    property Current: TsqlvHistoryItem read GetCurrent;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  end;

  { TsqlvEngine }

  TsqlvEngine = class(TsqlvCustomNodes)
  private
    FSession: TsqlvSession;
    FSetting: TsqlvSetting;
    FRecents: TStringList;
    FWorkPath: string;
    FHistory: TsqlvHistory;
    FSQLHistory: TsqlvHistory;
    procedure SetWorkPath(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    procedure LoadRecents;
    procedure SaveRecents;
    //vSilent: without add to history
    procedure Launch(Name: string; Group: string; MemberName: string; vSilent:Boolean = False);
    procedure Launch(Node: TsqlvNode; MemberName: string; vSilent:Boolean = False);
    procedure LaunchGroup(SchemaName: string; MemberName: string =''; vSilent:Boolean = False);
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
    property History: TsqlvHistory read FHistory;
    property SQLHistory: TsqlvHistory read FSQLHistory;
  end;

  { TsqlvMenuItem }

  TsqlvMenuItem = class(TMenuItem)
  private
    FNode: TsqlvNode;
    FMemberName: string;
  public
    procedure Click; override;
    property Node: TsqlvNode read FNode write FNode;
    property MemberName: string read FMemberName write FMemberName;
  end;

  { TsqlvButton }

  TsqlvButton = class(TButton)
  private
    FNode: TsqlvNode;
  protected
    function GetMemberName: string; virtual;
  public
    GroupInfo: TSchemaInfo;//Table,Accounts
    SchemaName: string;//Field
    procedure Click; override;
    property Node: TsqlvNode read FNode write FNode;
  end;

var
  AddOpenSaveDialogFilters: string = '';

function sqlvEngine: TsqlvEngine;

procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Node: TsqlvNode; MemberName: string); overload;
procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Group: string; MemberName: string); overload;

implementation

var
  FsqlvEngine: TsqlvEngine = nil;

function sqlvEngine: TsqlvEngine;
begin
  if FsqlvEngine = nil then
    FsqlvEngine := TsqlvEngine.Create;
  Result := FsqlvEngine;
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

procedure TsqlvCustomNodes.Enum(Name: string; Nodes: TsqlvNodes; OnlyDefaults:Boolean = False);
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
    if SameText(Items[i].Group, Name) and (not OnlyDefaults or (nsDefault in Items[i].Style)) then
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
  FAttributes := TStringList.Create;
end;

destructor TsqlvNode.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

procedure TsqlvNode.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
begin
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
  sqlvEngine.Enum(Name, Nodes);
end;

procedure TsqlvNode.EnumDefaults(Nodes: TsqlvNodes);
begin
  sqlvEngine.Enum(Name, Nodes, True);
end;

procedure TsqlvNode.EnumHeader(Header: TStringList);
begin
  Header.Clear;
  Header.Add(Title);
end;

procedure TsqlvNode.Execute(const Value: string; Params: TmncParams = nil);
begin
  DoExecute(Value, Params);
  FreeAndNil(Params);
end;

function TsqlvNode.GetCanExecute: Boolean;
begin
  Result := True;
end;

procedure TsqlvNode.DoExecute(const Value: string; Params: TmncParams);
begin
end;

procedure TsqlvNode.ShowProperty;
begin
end;

{ TsqlvMenuItem }

procedure TsqlvMenuItem.Click;
begin
  inherited;
  //Node.Execute(MemberName);
end;

procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Node: TsqlvNode; MemberName: string);
var
  aNodes: TsqlvNodes;
  aMenuItem: TsqlvMenuItem;
  i: Integer;
begin
  if Node <> nil then
  begin
    aNodes := TsqlvNodes.Create;
    try
      if Node.CanExecute then
      begin
        aMenuItem := TsqlvMenuItem.Create(PopupMenu);
        aMenuItem.Caption := 'Open';
        aMenuItem.Node := Node;
        aMenuItem.Default := True;
        aMenuItem.MemberName := Node.Name;
        aMenuItem.ImageIndex := Node.ImageIndex;
        PopupMenu.Items.Add(aMenuItem);
      end;

      Node.Enum(aNodes);
      for i := 0 to aNodes.Count - 1 do
      begin
        aMenuItem := TsqlvMenuItem.Create(PopupMenu);
        aMenuItem.Caption := aNodes[i].Title;
        aMenuItem.Node := aNodes[i];
        aMenuItem.Default := (nsDefault in aMenuItem.Node.Style);
        aMenuItem.MemberName := MemberName;
        aMenuItem.ImageIndex := aMenuItem.Node.ImageIndex;
        PopupMenu.Items.Add(aMenuItem);
      end;
    finally
      aNodes.Free;
    end;
  end;
end;

procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Group: string; MemberName: string);
var
  aNodes: TsqlvNodes;
  aMenuItem: TsqlvMenuItem;
  i: Integer;
begin
  aNodes := TsqlvNodes.Create;
  try
    sqlvEngine.Enum(Group, aNodes);
    for i := 0 to aNodes.Count - 1 do
    begin
      aMenuItem := TsqlvMenuItem.Create(PopupMenu);
      aMenuItem.Caption := aNodes[i].Title;
      aMenuItem.Node := aNodes[i];
      aMenuItem.Default := nsDefault in aMenuItem.Node.Style;
      aMenuItem.MemberName := MemberName;
      aMenuItem.ImageIndex := aMenuItem.Node.ImageIndex;
      PopupMenu.Items.Add(aMenuItem);
    end;
  finally
    aNodes.Free;
  end;
end;

procedure TsqlvEngine.Launch(Name: string; Group, MemberName: string; vSilent:Boolean); overload;
var
  aNode: TsqlvNode;
  s: string;
begin
  try
    if Group = '' then
      aNode := Find(Name)
    else
      aNode := Find(Group, Name);
    if aNode = nil then
    begin
      s := Group;
      if s <> '' then
        s := s + '\';
      raise Exception.Create(s + Name + ' node not found');
    end;
    Launch(aNode, MemberName, vSilent);
  finally
  end;
end;

procedure TsqlvEngine.Launch(Node: TsqlvNode; MemberName: string; vSilent:Boolean);
var
  aNodes: TsqlvNodes;
begin
  if Node <> nil then
  begin
    if Node.CanExecute then
    begin
      Node.Execute(MemberName);
    end
    else
    begin
      aNodes := TsqlvNodes.Create;
      try
        Node.EnumDefaults(aNodes);
        if MemberName = '' then
          MemberName := Node.Name;
        if aNodes.Count > 0 then
        begin
          aNodes[0].Execute(MemberName);
        end;
      finally
        aNodes.Free;
      end;
    end;
  end;
end;

procedure TsqlvEngine.LaunchGroup(SchemaName: string; MemberName: string; vSilent:Boolean); overload;
var
  aNodes: TsqlvNodes;
  aNode: TsqlvNode;
begin
  aNodes := TsqlvNodes.Create;
  try
    aNode := Find(SchemaName);
    if (aNode <> nil) and (aNode.CanExecute) then
       Launch(aNode, MemberName, vSilent)
    else
    begin
      Enum(SchemaName, aNodes, True);
      if aNodes.Count > 0 then
        Launch(aNodes[0], MemberName, vSilent);
    end;
  finally
    aNodes.Free;
  end;
end;

{ TsqlvClass }

constructor TsqlvEngine.Create;
begin
  inherited Create(True);
  FHistory := TsqlvHistory.create;
  FSQLHistory := TsqlvHistory.create;
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

{ TsqlvHistory }

function TsqlvHistory.GetItem(Index: Integer): TsqlvHistoryItem;
begin
  Result := inherited Items[Index] as TsqlvHistoryItem;
end;

procedure TsqlvHistory.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TsqlvHistory.GetCurrent: TsqlvHistoryItem;
begin
  if (Index < Count) and (FIndex >=0) then
    Result := Items[Index]
  else
    Result := nil;
end;

constructor TsqlvHistory.Create;
begin
  inherited Create(True);
  Index := 0;
  MaxCount := 50;
end;

function TsqlvHistory.Add(History: TsqlvHistoryItem): Integer;
begin
  if (Count > MaxCount) and (Count > 0) then
  begin
    Delete(0);
    FIndex := FIndex - 1
  end;
  Result := inherited Add(History);
end;

procedure TsqlvHistory.Add(const Text: string; Silent: Boolean);
var
  i: Integer;
  aHistory: TsqlvHistoryItem;
begin
  if (Count > 0) then
  begin
    aHistory := Items[Count - 1];
    if (aHistory.Text = Text) then
      exit;//do not duplicate the last one
    if (Index < Count) and (Index >= 0) then
    begin
      aHistory := Items[Index];
      if (aHistory.Text = Text) then
        exit;//do not duplicate the current one
    end;
  end;
  aHistory := TsqlvHistoryItem.Create;
  aHistory.Text := Text;
  i := Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

function TsqlvHistory.HaveForward: Boolean;
begin
  Result := (Count > 0) and (Index < Count - 1);
end;

function TsqlvHistory.HaveBackward: Boolean;
begin
  Result := (Count> 0) and (FIndex > 0);
end;

function TsqlvHistory.Forward: Boolean;
begin
  Result := HaveForward;
  if Result then
  begin
    Index := FIndex + 1;
    Changed;
  end;
end;

function TsqlvHistory.Backward: Boolean;
begin
  Result := HaveBackward;
  if Result then
  begin
    Index := FIndex - 1;
    Changed;
  end;
end;

{ TsqlvButton }

function TsqlvButton.GetMemberName: string;
begin
  Result := '';
end;

procedure TsqlvButton.Click;
begin
  inherited Click;
  Node.Execute(GetMemberName, TmncParams.Create([GroupInfo.Name], [GroupInfo.Value]));
end;

initialization
finalization
  FreeAndNil(FsqlvEngine);
end.

