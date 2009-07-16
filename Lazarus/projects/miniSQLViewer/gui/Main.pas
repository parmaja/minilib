unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  contnrs, ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus,
  SynHighlighterSqlite, sqlvSessions,
  mnUtils, mncSQLite, mncSchemes, mncSqliteSchemes, sqlvClasses, sqlvStdClasses;

type
  TsqlState = (sqlsRoot, sqlsSQL, sqlsResults, sqlsMembers, sqlsObject);

  TRelatedObject = class(TObject)
  public
    Control: TControl;
    UseActive: Boolean;
  end;

  { TRelatedObjects }

  TRelatedObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TRelatedObject;
  public
    function Add(Related: TRelatedObject): Integer;
    property Items[Index: Integer]: TRelatedObject read GetItem; default;
  end;

  TPanelObject = class(TObject)
  private
    FRelated: TRelatedObjects;
  public
    Control: TControl;
    constructor Create;
    destructor Destroy; override;
    property Related: TRelatedObjects read FRelated;
    procedure Show(Active: Boolean);
    procedure Hide(Active: Boolean);
  end;

  TPanelsList = class(TObjectList)
  private
    function GetItem(Index: Integer): TPanelObject;
  public
    constructor Create;
    function Find(AControl: TControl): TPanelObject;
    procedure Add(AControl: TControl; ARelatedControl: TControl = nil; UseActive: Boolean = False);
    procedure Show(AControl: TControl; Active: Boolean);
    procedure HideAll;
    property Items[Index: Integer]: TPanelObject read GetItem; default;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ConnectBtn: TButton;
    DatabasesCbo: TComboBox;
    ExclusiveChk1: TCheckBox;
    GroupsList: TComboBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    AboutMnu: TMenuItem;
    OptionsMnu: TMenuItem;
    ToolsMnu: TMenuItem;
    TitleLbl: TLabel;
    DataPathCbo: TComboBox;
    DisconnectBtn: TButton;
    ExclusiveChk: TCheckBox;
    ObjectsBtn: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    MembersList: TListBox;
    ObjectsPanel: TPanel;
    ClientPanel: TPanel;
    ExecuteBtn: TSpeedButton;
    SQLBtn: TSpeedButton;
    SQLBtn1: TSpeedButton;
    OpenBtn: TSpeedButton;
    SpeedButton5: TSpeedButton;
    TopPanel: TPanel;
    ResultPanel: TPanel;
    RootPanel: TPanel;
    SQLEdit: TSynEdit;
    SQLPanel: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesCboDropDown(Sender: TObject);
    procedure DataPathCboDropDown(Sender: TObject);
    procedure DataPathCboExit(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure GroupsListClick(Sender: TObject);
    procedure GroupsListSelect(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ObjectsBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure SQLBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    FLockEnum: Boolean;
    FSqliteSyn: TSynSqliteSyn;
    FDataPath: string;
    procedure FileFoundEvent(FileIterator: TFileIterator);
    procedure DirectoryFoundEvent(FileIterator: TFileIterator);
    procedure EnumDatabases;
    procedure SetDataPath(const AValue: string);
    procedure Connected;
    procedure Disconnected;
    procedure SessionStarted;
    procedure SessionStoped;
  private
    FState: TsqlState;
    PanelsList: TPanelsList;
    procedure SetState(const AValue: TsqlState);
    procedure StateChanged;
    function GetDatabaseName: string;
    function GetRealDataPath: string;
    procedure SetRealDataPath(FileName: string);
    property DataPath: string read FDataPath write SetDataPath;
  public
    Node: TsqlvNode;
    SchemeName: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenMember;
    procedure OpenGroup;
    property State: TsqlState read FState write SetState;
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  sqlvEngine.Session.Open(GetDatabaseName);
  //State := sqlsMembers;
  Launch('Database', DatabasesCbo.Text);
end;

procedure TMainForm.DatabasesCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin

end;

procedure TMainForm.DataPathCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.DataPathCboExit(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
begin

end;

procedure TMainForm.GroupsListClick(Sender: TObject);
begin

end;

procedure TMainForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ObjectsBtnClick(Sender: TObject);
begin
  State := sqlsMembers;
end;

procedure TMainForm.MembersListDblClick(Sender: TObject);
begin
  OpenMember;
end;

procedure TMainForm.MembersListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    OpenMember;
    Key := #0;
  end;
end;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
  OpenMember;
end;

procedure TMainForm.SQLBtnClick(Sender: TObject);
begin
  State := sqlsSQL;
end;

procedure TMainForm.SpeedButton5Click(Sender: TObject);
begin
  State := sqlsRoot;
end;

procedure TMainForm.FileFoundEvent(FileIterator: TFileIterator);
begin
  DatabasesCbo.Items.Add(FileIterator.FileInfo.Name);
  ExclusiveChk.Caption := FileIterator.FileInfo.Name;
end;

procedure TMainForm.DirectoryFoundEvent(FileIterator: TFileIterator);
begin
  DataPathCbo.Items.Add(FileIterator.FileName);
end;

procedure TMainForm.EnumDatabases;
var
  aFiles: TStringList;
  aFileSearcher: TFileSearcher;
  s: string;
begin
  if not FLockEnum then
  begin
    FLockEnum := True;
    try
      aFileSearcher := TFileSearcher.Create;
      aFiles := TStringList.Create;
      DataPathCbo.Items.BeginUpdate;
      DatabasesCbo.Items.BeginUpdate;
      try
        ExclusiveChk.Caption := '';
        DataPathCbo.Items.Clear;
        DataPathCbo.Items.Add(FDataPath);
        DatabasesCbo.Clear;
        aFileSearcher.OnDirectoryFound := @DirectoryFoundEvent;
        aFileSearcher.OnFileFound := @FileFoundEvent;
        aFileSearcher.Search(GetRealDataPath, '*.sqlite;*.db', False);
        DataPathCbo.Text := FDataPath;
      finally
        DataPathCbo.Items.EndUpdate;
        DatabasesCbo.Items.EndUpdate;
        aFileSearcher.Free;
        aFiles.Free;
      end;
    finally
      FLockEnum := False;
    end;
  end;
end;

procedure TMainForm.SetDataPath(const AValue: string);
begin
  if FDataPath <> AValue then
  begin
    FDataPath := AValue;
    EnumDatabases;
  end;
end;

procedure TMainForm.Connected;
begin

end;

procedure TMainForm.Disconnected;
begin

end;

procedure TMainForm.SessionStarted;
begin

end;

procedure TMainForm.SessionStoped;
begin

end;

procedure TMainForm.SetState(const AValue: TsqlState);
begin
  if FState <> AValue then
  begin
    FState :=AValue;
    StateChanged;
  end;
end;

procedure TMainForm.StateChanged;
begin
  case FState of
    sqlsRoot: PanelsList.Show(RootPanel, sqlvEngine.Session.IsActive);
    sqlsSQL: PanelsList.Show(SQLPanel, sqlvEngine.Session.IsActive);
    sqlsResults: PanelsList.Show(ResultPanel, sqlvEngine.Session.IsActive);
    sqlsMembers: PanelsList.Show(ObjectsPanel, sqlvEngine.Session.IsActive);
  end;
end;

function TMainForm.GetDatabaseName: string;
begin
  Result := GetRealDataPath + DatabasesCbo.Text;
end;

function TMainForm.GetRealDataPath: string;
begin
  Result := ExpandToPath('', FDataPath, Application.Location);
end;

procedure TMainForm.SetRealDataPath(FileName: string);
var
  aPath: string;
  i: Integer;
begin
  aPath := ExtractFilePath(FileName);
  DataPathCbo.Text := aPath;
  DataPath := aPath;
  FileName := ExtractFileName(FileName);
  i:= DatabasesCbo.Items.IndexOf(FileName);
  if i >= 0 then
    DatabasesCbo.ItemIndex := i;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //SQLEdit.CharWidth := 8;
  sqlvEngine.WorkPath := Application.Location;
  sqlvEngine.Session.OnConnected := @Connected;
  sqlvEngine.Session.OnDisconnected := @Disconnected;
  sqlvEngine.Session.OnSessionStarted := @SessionStarted;
  sqlvEngine.Session.OnSessionStoped := @SessionStoped;
  FSqliteSyn := TSynSqliteSyn.Create(Self);
  SQLEdit.Highlighter := FSqliteSyn;
  PanelsList := TPanelsList.Create;
  PanelsList.Add(RootPanel);
  PanelsList.Add(RootPanel, SQLBtn, True);
  PanelsList.Add(RootPanel, ObjectsBtn, True);
  PanelsList.Add(SQLPanel);
  PanelsList.Add(SQLPanel, ExecuteBtn, True);
  PanelsList.Add(SQLPanel, ObjectsBtn, True);
  PanelsList.Add(ResultPanel);
  PanelsList.Add(ResultPanel, SQLBtn, True);
  PanelsList.Add(ObjectsPanel);
  PanelsList.Add(ObjectsPanel, OpenBtn, True);
  PanelsList.Add(ObjectsPanel, SQLBtn, True);
  if sqlvEngine.Recents.Count > 0 then
    SetRealDataPath(sqlvEngine.Recents[0]);
  StateChanged;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(PanelsList);
  inherited Destroy;
end;

{ TPanelsList }

constructor TPanelsList.Create;
begin
  inherited Create(True);
end;

function TPanelsList.Find(AControl: TControl): TPanelObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if (Items[i].Control = AControl) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TPanelsList.GetItem(Index: Integer): TPanelObject;
begin
  Result := inherited items[Index] as TPanelObject;
end;

procedure TPanelsList.HideAll;
begin
  Show(nil, False);
end;

procedure TPanelsList.Show(AControl: TControl; Active: Boolean);
var
  i: Integer;
  aPanel : TPanelObject;
begin
  aPanel := nil;
  for i := 0 to Count -1 do
  begin
    if (Items[i].Control = AControl) then
      aPanel := Items[i]
    else
    begin
      Items[i].Hide(Active);
    end;
  end;
  if aPanel <> nil then
  begin
    aPanel.Show(Active);
  end;
end;

procedure TPanelsList.Add(AControl: TControl; ARelatedControl: TControl; UseActive: Boolean);
var
  aPanel: TPanelObject;
  aRelated: TRelatedObject;
begin
  if ARelatedControl <> nil then
  begin
    aPanel := Find(AControl);
    if aPanel = nil then
      raise Exception.Create('Control not found in the list');
    aRelated := TRelatedObject.Create;
    aRelated.UseActive := UseActive;
    aRelated.Control := ARelatedControl;
    aPanel.Related.Add(aRelated);
  end
  else
  begin
    aPanel := TPanelObject.Create;
    aPanel.Control := AControl;
    inherited Add(aPanel);
  end;
end;

{ TPanelObject }

constructor TPanelObject.Create;
begin
  FRelated := TRelatedObjects.Create(True);
end;

destructor TPanelObject.Destroy;
begin
  FRelated.Free;
  inherited Destroy;
end;

procedure TPanelObject.Show(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := True;
  Control.Align := alClient;
  for i := 0 to Related.Count -1 do
    if not Related[i].UseActive or Active then
      Related[i].Control.Visible := True;
end;

procedure TPanelObject.Hide(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := False;
  for i := 0 to Related.Count -1 do
    Related[i].Control.Visible := False;
end;

{ TRelatedObjects }

function TRelatedObjects.GetItem(Index: Integer): TRelatedObject;
begin
  Result := inherited items[Index] as TRelatedObject;
end;

function TRelatedObjects.Add(Related: TRelatedObject): Integer;
begin
  Result := inherited Add(Related);
end;

procedure TMainForm.OpenMember;
begin
  if (MembersList.Items.Count > 0) and (MembersList.ItemIndex >=0) then
  begin
    LaunchRelated(SchemeName, MembersList.Items[MembersList.ItemIndex]);
  end;
end;

procedure TMainForm.OpenGroup;
begin
  if (GroupsList.Items.Count > 0) and (GroupsList.ItemIndex >=0) then
    Launch(GroupsList.Items[GroupsList.ItemIndex]);
end;

initialization
  {$I Main.lrs}
end.

