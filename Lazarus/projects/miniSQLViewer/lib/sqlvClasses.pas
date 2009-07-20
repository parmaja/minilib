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
  sqlvConsts, sqlvSessions, Menus, ImgList;

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

  TsqlvSetting = class(TmnXMLProfile)
  private
    FOpenSaveDialogFilters: string;
    FLoadFieldsToAutoComplete: Boolean;
    FLogoutSQL: string;
    FLoginSQL: string;
    FInternalLogoutSQL: string;
    FInternalLoginSQL: string;
  public
    property InternalLoginSQL:string read FInternalLoginSQL write FInternalLoginSQL;
    property InternalLogoutSQL:string read FInternalLogoutSQL write FInternalLogoutSQL;
  published
    property LoadFieldsToAutoComplete:Boolean read FLoadFieldsToAutoComplete write FLoadFieldsToAutoComplete default False;
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
    FComment: string;
    FGroup: string;
    FName: string;
    FStyle: TsqlvNodeStyle;
    FTitle: string;
    FKind: TschmKind;
    FImageIndex: TImageIndex;
  protected
    function GetCanExecute: Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowProperty; virtual;
    procedure Execute(const MemberName: string); virtual;
    procedure Enum(Nodes: TsqlvNodes);
    procedure EnumHeader(Header: TStringList); virtual;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string = ''); virtual;
    property CanExecute: Boolean read GetCanExecute;
    property Group: string read FGroup write FGroup;
    property Name: string read FName write FName;
    property Comment: string read FComment write FComment;
    property Attributes: TStringList read FAttributes write FAttributes;
    property Title: string read FTitle write FTitle;
    property Kind: TschmKind read FKind write FKind default sokNone;
    property Style:TsqlvNodeStyle read FStyle write FStyle;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  TsqlvNodeClass = class of TsqlvNode;

  TsqlvCustomNodes = class(TObjectList)
  private
    function GetItem(Index: Integer): TsqlvNode;
    procedure SetItem(Index: Integer; const Value: TsqlvNode);
  public
    procedure Enum(Name: string; Nodes: TsqlvNodes); overload;
    function Find(const Name: string): TsqlvNode;
    property Items[Index: Integer]: TsqlvNode read GetItem write SetItem; default;
  end;

  { TsqlvNodes }

  TsqlvNodes = class(TsqlvCustomNodes)
  public
    constructor Create; virtual;
    function Add(vNode:TsqlvNode): Integer;
  end;

  TsqlvHistoryItem = class(TObject)
    Name: string;
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
    procedure Add(const Name, Text: string);
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
    procedure Launch(Name: string; MemberName: string =''; vSilent:Boolean = False);
    procedure Launch(Node: TsqlvNode; MemberName: string; vSilent:Boolean = False);
    procedure LaunchGroup(SchemaName: string; MemberName: string =''; vSilent:Boolean = False);
    procedure RegisterFilter(Filter: string);
    procedure RegisterViewer(Classes: array of TsqlvNodeClass);
    procedure AddRecent(Name:string);
    procedure LoadFile(FileName:string; Strings:TStrings);
    procedure SaveFile(FileName:string; Strings:TStrings);
    procedure Backward;
    procedure Forward;
    function GetAllSupportedFiles: string;
    property Setting: TsqlvSetting read FSetting;
    property Recents: TStringList read FRecents;
    property Session: TsqlvSession read FSession;
    property WorkPath :string read FWorkPath write SetWorkPath;
    property History: TsqlvHistory read FHistory;
    property SQLHistory: TsqlvHistory read FSQLHistory;
  end;

  TsqlvMenuItem = class(TMenuItem)
  private
    FNode: TsqlvNode;
    FMemberName: string;
  public
    procedure Click; override;
    property Node: TsqlvNode read FNode write FNode;
    property MemberName: string read FMemberName write FMemberName;
  end;

var
  LoadFieldsToAutoComplete: Boolean = False;
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

procedure TsqlvEngine.Backward;
begin
  if History.Current <> nil then
  begin
    Launch(History.Current.Name, History.Current.Text, True);
    History.Backward;
  end;
end;

procedure TsqlvEngine.Forward;
begin
  if History.Current <> nil then
  begin
    Launch(History.Current.Name, History.Current.Text, True);
    History.Forward;
  end;
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

procedure TsqlvCustomNodes.Enum(Name: string; Nodes: TsqlvNodes);
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
    if SameText(Items[i].Group, Name) then
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

procedure TsqlvNode.EnumHeader(Header: TStringList);
begin
  Header.Clear;
  Header.Add(Title);
end;

procedure TsqlvNode.Execute(const MemberName: string);
begin
end;

function TsqlvNode.GetCanExecute: Boolean;
begin
  Result := True;
end;

procedure TsqlvNode.ShowProperty;
begin
end;

{ TsqlvMenuItem }

procedure TsqlvMenuItem.Click;
begin
  inherited;
  Node.Execute(MemberName);
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

procedure TsqlvEngine.Launch(Name: string; MemberName: string; vSilent:Boolean); overload;
var
  aNode: TsqlvNode;
begin
  try
    aNode := Find(Name);
    if aNode = nil then
      raise Exception.Create(Name +' Node not found');
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
      if not vSilent then
        History.Add(Node.Name, MemberName);
    end
    else
    begin
      aNodes := TsqlvNodes.Create;
      try
        Node.Enum(aNodes);
        if MemberName = '' then
          MemberName := Node.Name;
        if aNodes.Count > 0 then
        begin
          aNodes[0].Execute(MemberName);
          if not vSilent then
            History.Add(aNodes[0].Name, MemberName);
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
      Enum(SchemaName, aNodes);
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
  if (Index < Count) and (Index >=0) then
    Result := Items[Index]
  else
    Result := nil;
end;

constructor TsqlvHistory.create;
begin
  inherited Create(True);
  Index := 0;
  MaxCount := 50;
end;

function TsqlvHistory.Add(History: TsqlvHistoryItem): Integer;
begin
  if (Count > 0) and (Index >=0) then
    Count := Index + 1;//cut to index
  Result := inherited Add(History);
  Index := Result;
  if (Count > MaxCount) and (Count > 0) then
    Delete(0);
end;

procedure TsqlvHistory.Add(const Name, Text: string);
var
  aHistory: TsqlvHistoryItem;
begin
  if (Count > 0) then
  begin
    aHistory := Items[Count - 1];
    if (aHistory.Name = Name) and (aHistory.Text = Text) then
      exit;//do not duplicate the last one
  end;
  aHistory := TsqlvHistoryItem.Create;
  aHistory.Name := Name;
  aHistory.Text := Text;
  Add(aHistory);
  Changed;
end;

function TsqlvHistory.HaveForward: Boolean;
begin
  Result := Index < Count - 1;
end;

function TsqlvHistory.HaveBackward: Boolean;
begin
  Result := Index > 0;
end;

function TsqlvHistory.Forward: Boolean;
begin
  Result := HaveForward;
  if Result then
  begin
    Index := Index + 1;
    Changed;
  end;
end;

function TsqlvHistory.Backward: Boolean;
begin
  Result := HaveBackward;
  if Result then
  begin
    Index := Index - 1;
    Changed;
  end;
end;

initialization
finalization
  FreeAndNil(FsqlvEngine);
end.

