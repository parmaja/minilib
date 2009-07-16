unit sqlvClasses;

interface

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Classes, Controls,
  mnXMLRttiProfile, mnXMLStreams,
  Dialogs, Contnrs,
  mncSchemes, mnUtils,
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
    FRelated: string;
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
    procedure EnumRelated(Nodes: TsqlvNodes); overload;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string = ''); virtual;
    property CanExecute: Boolean read GetCanExecute;
    property Related: string read FRelated write FRelated;
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
    procedure EnumRelated(Name: string; Nodes: TsqlvNodes); overload;
    function Find(const Name: string): TsqlvNode;
    property Items[Index: Integer]: TsqlvNode read GetItem write SetItem; default;
  end;

  { TsqlvNodes }

  TsqlvNodes = class(TsqlvCustomNodes)
  public
    constructor Create; virtual;
    function Add(vNode:TsqlvNode): Integer;
  end;

  { TsqlvEngine }

  TsqlvEngine = class(TsqlvCustomNodes)
  private
    FSession: TsqlvSession;
    FSetting: TsqlvSetting;
    FRecents: TStringList;
    FWorkPath: string;
    procedure SetWorkPath(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    procedure LoadRecents;
    procedure SaveRecents;
    procedure RegisterFilter(Filter: string);
    procedure RegisterViewer(Classes: array of TsqlvNodeClass);
    procedure AddRecent(Name:string);
    function GetAllSupportedFiles: string;
    property Setting: TsqlvSetting read FSetting;
    property Recents: TStringList read FRecents;
    property Session: TsqlvSession read FSession;
    property WorkPath :string read FWorkPath write SetWorkPath;
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
procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Related: string; MemberName: string); overload;
procedure Launch(Name: string; MemberName: string = ''); overload;
procedure Launch(Node: TsqlvNode; MemberName: string = ''); overload;
procedure LaunchRelated(SchemeName: string; MemberName: string = ''); overload;
procedure BackForm(vRelated: string);


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

procedure TsqlvCustomNodes.EnumRelated(Name: string; Nodes: TsqlvNodes);
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
    if SameText(Items[i].Related, Name) then
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

procedure TsqlvNode.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
begin
end;

{procedure TsqlvNode.EnumRelated(Session: TsqlvSession; Strings: TStrings);
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
    if SameText(sqlvClasses[i].Related, Name) then
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

procedure TsqlvNode.EnumRelated(Nodes: TsqlvNodes);
begin
  inherited;
  sqlvEngine.EnumRelated(Name, Nodes);
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

      Node.EnumRelated(aNodes);
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

procedure FillPopupMenu(PopupMenu: TPopupMenu; Session: TsqlvSession; Related: string; MemberName: string);
var
  aNodes: TsqlvNodes;
  aMenuItem: TsqlvMenuItem;
  i: Integer;
begin
  aNodes := TsqlvNodes.Create;
  try
    sqlvEngine.EnumRelated(Related, aNodes);
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

procedure Launch(Name: string; MemberName: string = ''); overload;
var
  aNode: TsqlvNode;
begin
  try
    aNode := sqlvEngine.Find(Name);
    if aNode = nil then
      raise Exception.Create(Name +' Node not found');
    Launch(aNode);
  finally
  end;
end;

procedure Launch(Node: TsqlvNode; MemberName: string);
var
  aNodes: TsqlvNodes;
begin
  if Node <> nil then
  begin
    if Node.CanExecute then
      Node.Execute(MemberName)
    else
    begin
      aNodes := TsqlvNodes.Create;
      try
        Node.EnumRelated(aNodes);
        if MemberName = '' then
          MemberName := Node.Name;
        if aNodes.Count > 0 then
          aNodes[0].Execute(MemberName);
      finally
        aNodes.Free;
      end;
    end;
  end;
end;

procedure LaunchRelated(SchemeName: string; MemberName: string = ''); overload;
var
  aNodes: TsqlvNodes;
  aNode: TsqlvNode;
begin
  aNodes := TsqlvNodes.Create;
  try
    aNode := sqlvEngine.Find(SchemeName);
    if (aNode <> nil) and (aNode.CanExecute) then
      aNode.Execute(MemberName)
    else
    begin
      sqlvEngine.EnumRelated(SchemeName, aNodes);
      if aNodes.Count > 0 then
        aNodes[0].Execute(MemberName);
    end;
  finally
    aNodes.Free;
  end;
end;

procedure BackForm(vRelated: string);
{var
  i: Integer;}
begin
{  for i := 0 to Application.MainForm.MDIChildCount - 1 do
  begin
    if (Application.MainForm.MDIChildren[i] is TsqlvForm) and ((Application.MainForm.MDIChildren[i] as TsqlvForm).Related = vRelated) then
    begin
      Application.MainForm.MDIChildren[i].Show;
      break;
    end;
  end;}
end;

{ TsqlvClass }

constructor TsqlvEngine.Create;
begin
  inherited Create(True);
  FSetting := TsqlvSetting.Create;
  FRecents := TStringList.Create;
  FSession:=TsqlvSession.Create;
end;


destructor TsqlvEngine.Destroy;
begin
  FreeAndNil(FSession);
  FreeAndNil(FSetting);
  FreeAndNil(FRecents);
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
    Result := 'All Supported|*.fdb';
    aStrings := TStringList.Create;
    StrToStrings(s, aStrings, ['|'], [' ', #13, #10]);
    for i := 0 to aStrings.Count - 1 do
    begin
      if Odd(i) then
        Result := Result + ';' + aStrings[i];
    end;
    aStrings.Free;
    Result := Result + '|' + sFBSQLFilter + '|' + s + '|' + sAllFilesFilter;
  end
  else
    Result := Result + sFBSQLFilter + '|' + sAllFilesFilter;
end;

procedure TsqlvEngine.LoadSetting;
begin
  FSetting.SafeLoadFromFile(WorkPath + 'sqlviewer.config');
end;

initialization
finalization
  FreeAndNil(FsqlvEngine);
end.

