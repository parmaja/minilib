unit Main;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}


{$mode objfpc}{$H+}

{todo: Save/Load sql scripts: DONE}
{todo: Auto complete: DONE}

{todo: search for members: Done}
{todo: More short cuts: Return in members}
{todo: Assoiate with *.sqlite}
{todo: Export/Import As CSV}
{todo: Extract the schema of whale database}
{todo: Find and Replace}


interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  dateutils, LCLType,
  contnrs, ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus,
  SynHighlighterSqlite, sqlvSessions,
  SynCompletion, SynEditAutoComplete, SynHighlighterHashEntries,
  mnUtils, mncSQLite, mncSchemes, mncSqliteSchemes, sqlvClasses, sqlvStdClasses, LMessages;

type
  TsqlState = (sqlsRoot, sqlsSQL, sqlsResults, sqlsInfo, sqlsMembers);

  TControlObject = class(TObject)
  public
    Control: TControl;
    UseActive: Boolean;
    Reverse: Boolean;
  end;

  { TControlObjects }

  TControlObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TControlObject;
  public
    function Add(ControlObject: TControlObject): Integer;
    property Items[Index: Integer]: TControlObject read GetItem; default;
  end;

  TPanelObject = class(TObject)
  private
    FList: TControlObjects;
  public
    Control: TControl;
    constructor Create;
    destructor Destroy; override;
    property List: TControlObjects read FList;
    procedure Show(Active: Boolean);
    procedure Hide(Active: Boolean);
  end;

  TPanelsList = class(TObjectList)
  private
    function GetItem(Index: Integer): TPanelObject;
  public
    constructor Create;
    function Find(AControl: TControl): TPanelObject;
    procedure Add(AControl: TControl; ALinkControl: TControl = nil; UseActive: Boolean = False; Reverse: Boolean = False);
    procedure Show(AControl: TControl; Active: Boolean);
    procedure HideAll;
    property Items[Index: Integer]: TPanelObject read GetItem; default;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SQLLoadBtn: TButton;
    FirstBtn: TSpeedButton;
    ResultsBtn: TSpeedButton;
    InfoPanel: TPanel;
    InfoLbl: TLabel;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    ResultEdit: TMemo;
    InfoBtn: TSpeedButton;
    SQLBackwardBtn: TSpeedButton;
    ConnectBtn: TButton;
    DatabasesCbo: TComboBox;
    DisconnectBtn: TButton;
    AutoCreateChk: TCheckBox;
    SQLForwardBtn: TSpeedButton;
    GroupsList: TComboBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    AboutMnu: TMenuItem;
    ForwardBtn: TSpeedButton;
    MembersGrid: TStringGrid;
    SQLSaveBtn: TButton;
    ToolsMnu: TMenuItem;
    TitleLbl: TLabel;
    DataPathCbo: TComboBox;
    ObjectsBtn: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
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
    DataGrid: TStringGrid;
    BackwordBtn: TSpeedButton;
    procedure AboutMnuClick(Sender: TObject);
    procedure BackwordBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesCboDropDown(Sender: TObject);
    procedure DataPathCboDropDown(Sender: TObject);
    procedure DataPathCboExit(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure GroupsListClick(Sender: TObject);
    procedure GroupsListSelect(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure MembersGridClick(Sender: TObject);
    procedure MembersGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MembersGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure MembersGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MenuItem1Click(Sender: TObject);
    procedure ObjectsBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure ResultsBtnClick(Sender: TObject);
    procedure SQLBackwardBtnClick(Sender: TObject);
    procedure SQLBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure SQLForwardBtnClick(Sender: TObject);
    procedure SQLLoadBtnClick(Sender: TObject);
    procedure SQLSaveBtnClick(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    LastSQLFileName: string;
    FLockEnum: Boolean;
    FSqliteSyn: TSynSqliteSyn;
    FDataPath: string;
    procedure SearchFor(S: string);
    procedure Connect;
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
    FCancel: Boolean;
    Completion: TSynCompletion;
    procedure AddSql;
    procedure ClearGrid;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure Execute(SQLCMD: TmncSQLiteCommand; SQL:TStringList; ShowGrid:Boolean);
    procedure ExecuteScript;
    procedure FillGrid(SQLCMD: TmncSQLiteCommand);
    procedure LoadCompletion;
    function LogTime(Start: TDateTime): string;
    procedure RefreshSQLHistory(Sender: TObject);
    procedure RefreshHistory(Sender: TObject);
    procedure SetState(const AValue: TsqlState);
    procedure StateChanged;
    function GetDatabaseName: string;
    function GetRealDataPath: string;
    procedure SetRealDataPath(FileName: string);
    property DataPath: string read FDataPath write SetDataPath;
  public
    GroupsNames: TStringList;
    SchemaName: string;
    MebmerName: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenMember;
    procedure OpenGroup;
    property State: TsqlState read FState write SetState;
  end;

var
  MainForm: TMainForm;

implementation

uses
  AboutForm;

{ TMainForm }

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.DatabasesCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
end;

procedure TMainForm.BackwordBtnClick(Sender: TObject);
begin
  sqlvEngine.Backward;
end;

procedure TMainForm.AboutMnuClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.DataPathCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.DataPathCboExit(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.DisconnectBtnClick(Sender: TObject);
begin
  sqlvEngine.Session.Close;
  StateChanged;
end;

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
begin
  ExecuteScript;
end;

procedure TMainForm.FirstBtnClick(Sender: TObject);
begin
  sqlvEngine.Launch('Database', DatabasesCbo.Text);
end;

procedure TMainForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  if Msg.CharCode = VK_F9 then
  begin
    if (State = sqlsSQL) and sqlvEngine.Session.IsActive then
    begin
      ExecuteScript;
      Handled := True;
    end;
  end;
end;

procedure TMainForm.GroupsListClick(Sender: TObject);
begin

end;

procedure TMainForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup;
end;

procedure TMainForm.InfoBtnClick(Sender: TObject);
begin
  State := sqlsInfo;
end;

procedure TMainForm.MembersGridClick(Sender: TObject);
begin
  if not FSearching then
    FSearch := '';
end;

procedure TMainForm.MembersGridColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin

end;

procedure TMainForm.MembersGridDblClick(Sender: TObject);
begin
  OpenMember;
end;

procedure TMainForm.MembersGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      OpenMember;
    end;
  end;
end;

procedure TMainForm.MembersGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin

end;

procedure TMainForm.MembersGridSelection(Sender: TObject; aCol, aRow: Integer);
begin

end;

procedure TMainForm.MembersGridUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (FSearch = '') or (SecondsBetween(Now, FSearchTime) > 3) then
  begin
    FFirstSearch := True;
    FSearch := '';
  end;
  FSearch := FSearch + UTF8Key;
  SearchFor(FSearch);
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

procedure TMainForm.ResultsBtnClick(Sender: TObject);
begin
  State := sqlsResults;
end;

procedure TMainForm.SQLBackwardBtnClick(Sender: TObject);
begin
  if (sqlvEngine.SQLHistory.HaveBackward) then
  begin
    sqlvEngine.SQLHistory.Backward;
    if (sqlvEngine.SQLHistory.Current <> nil) then
      SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
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

procedure TMainForm.ForwardBtnClick(Sender: TObject);
begin
  sqlvEngine.Forward;
end;

procedure TMainForm.SQLForwardBtnClick(Sender: TObject);
begin
  if (sqlvEngine.SQLHistory.HaveForward) then
  begin
    sqlvEngine.SQLHistory.Forward;
    if sqlvEngine.SQLHistory.Current <> nil then
      SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
  end;
end;

procedure TMainForm.SQLLoadBtnClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '*.sql';
  OpenDialog.Filter := '*.sql';
  OpenDialog.InitialDir := Application.Location;
  if OpenDialog.Execute then
  begin
    LastSQLFileName := OpenDialog.FileName;
    SQLEdit.Lines.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.SQLSaveBtnClick(Sender: TObject);
begin
  SaveDialog.FileName := LastSQLFileName;
  SaveDialog.DefaultExt := '*.sql';
  SAveDialog.Filter := '*.sql';
  SaveDialog.InitialDir := Application.Location;
  if SaveDialog.Execute then
    SQLEdit.Lines.LoadFromFile(SaveDialog.FileName);
end;

procedure TMainForm.SearchFor(S: string);
var
  i: Integer;
  t: string;
  f: Integer;
begin
  f := ord(not FFirstSearch);
  for i := MembersGrid.Row + f to MembersGrid.RowCount -1 do
  begin
    t := LeftStr(MembersGrid.Cells[0, i], Length(S));
    if SameText(S, t) then
    begin
      FSearching := True;
      try
        MembersGrid.Row := i;
      finally
        FSearching := False;
      end;
      break;
    end;
  end;
  FSearchTime := Now;
end;

procedure TMainForm.Connect;
begin
  sqlvEngine.Session.Open(GetDatabaseName, AutoCreateChk.Checked);
  sqlvEngine.Launch('Database', DatabasesCbo.Text);
end;

procedure TMainForm.FileFoundEvent(FileIterator: TFileIterator);
begin
  DatabasesCbo.Items.Add(FileIterator.FileInfo.Name);
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
  FreeAndNil(Completion);
  Completion := TSynCompletion.Create(nil);
  LoadCompletion;
end;

procedure TMainForm.Disconnected;
begin
  FreeAndNil(Completion);
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
    sqlsInfo: PanelsList.Show(InfoPanel, sqlvEngine.Session.IsActive);
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
    DatabasesCbo.ItemIndex := i
  else
    DatabasesCbo.Text := FileName;
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  aFile: string;
begin
  inherited Create(TheOwner);
  {$ifdef WINCE}
  SQLEdit.Font.Size := 8;
  {$endif}
  SQLBackwardBtn.Glyph.Assign(BackwordBtn.Glyph);
  SQLForwardBtn.Glyph.Assign(ForwardBtn.Glyph);
  GroupsNames := TStringList.Create;
  sqlvEngine.WorkPath := Application.Location;
  sqlvEngine.Session.OnConnected := @Connected;
  sqlvEngine.Session.OnDisconnected := @Disconnected;
  sqlvEngine.Session.OnSessionStarted := @SessionStarted;
  sqlvEngine.Session.OnSessionStoped := @SessionStoped;
  FSqliteSyn := TSynSqliteSyn.Create(Self);
  SQLEdit.Highlighter := FSqliteSyn;
  PanelsList := TPanelsList.Create;
  PanelsList.Add(RootPanel);
  PanelsList.Add(RootPanel, DisconnectBtn, True);
  PanelsList.Add(RootPanel, ConnectBtn, True, True);
  PanelsList.Add(RootPanel, SQLBtn, True);
  PanelsList.Add(RootPanel, ObjectsBtn, True);
  PanelsList.Add(SQLPanel);
  PanelsList.Add(SQLPanel, ExecuteBtn, True);
  PanelsList.Add(SQLPanel, ResultsBtn, True);
  PanelsList.Add(SQLPanel, ObjectsBtn, True);
  PanelsList.Add(SQLPanel, InfoBtn, True);
  PanelsList.Add(ResultPanel);
  PanelsList.Add(ResultPanel, SQLBtn, True);
  PanelsList.Add(ResultPanel, InfoBtn, True);
  PanelsList.Add(ObjectsPanel);
  PanelsList.Add(ObjectsPanel, OpenBtn, True);
  PanelsList.Add(ObjectsPanel, SQLBtn, True);
  PanelsList.Add(InfoPanel);
  PanelsList.Add(InfoPanel, ResultsBtn, True);
  PanelsList.Add(InfoPanel, SQLBtn, True);

  sqlvEngine.History.OnChanged := @RefreshHistory;
  sqlvEngine.SQLHistory.OnChanged := @RefreshSQLHistory;
  sqlvEngine.LoadFile('recent.sql', SQLEdit.Lines);
  AddSql;
  sqlvEngine.History.Changed;
  sqlvEngine.SQLHistory.Changed;
  if Paramcount > 0 then
  begin
    aFile := ParamStrUTF8(1);
    if FileExistsUTF8(aFile) then
    begin
      SetRealDataPath(aFile);
      Connect;
    end;
  end
  else
  begin
    if sqlvEngine.Recents.Count > 0 then
      SetRealDataPath(sqlvEngine.Recents[0]);
  end;
  StateChanged;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(PanelsList);
  FreeAndNil(GroupsNames);
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

procedure TPanelsList.Add(AControl: TControl; ALinkControl: TControl; UseActive: Boolean; Reverse: Boolean = False);
var
  aPanel: TPanelObject;
  aLink: TControlObject;
begin
  if ALinkControl <> nil then
  begin
    aPanel := Find(AControl);
    if aPanel = nil then
      raise Exception.Create('Control not found in the list');
    aLink := TControlObject.Create;
    aLink.UseActive := UseActive;
    aLink.Reverse := Reverse;
    aLink.Control := ALinkControl;
    aPanel.List.Add(aLink);
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
  FList := TControlObjects.Create(True);
end;

destructor TPanelObject.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TPanelObject.Show(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := True;
  Control.Align := alClient;
  for i := 0 to List.Count -1 do
    List[i].Control.Visible := not List[i].UseActive or ((Active and not List[i].Reverse) or (not Active and List[i].Reverse));
end;

procedure TPanelObject.Hide(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := False;
  for i := 0 to List.Count -1 do
    List[i].Control.Visible := False;
end;

{ TControlObjects }

function TControlObjects.GetItem(Index: Integer): TControlObject;
begin
  Result := inherited items[Index] as TControlObject;
end;

function TControlObjects.Add(ControlObject: TControlObject): Integer;
begin
  Result := inherited Add(ControlObject);
end;

procedure TMainForm.OpenMember;
begin
  if (MembersGrid.RowCount > 1) and (MembersGrid.Row >= 1) then
    sqlvEngine.LaunchGroup(SchemaName, MembersGrid.Cells[0, MembersGrid.Row]);
end;

procedure TMainForm.OpenGroup;
begin
  if (GroupsList.Items.Count > 0) and (GroupsList.ItemIndex >=0) then
    sqlvEngine.Launch(GroupsNames[GroupsList.ItemIndex], MebmerName, True);
end;

procedure TMainForm.RefreshSQLHistory(Sender: TObject);
begin
  SQLForwardBtn.Enabled := sqlvEngine.SQLHistory.HaveForward;
  SQLBackwardBtn.Enabled := sqlvEngine.SQLHistory.HaveBackward;
end;

procedure TMainForm.RefreshHistory(Sender: TObject);
begin
  ForwardBtn.Enabled := sqlvEngine.History.HaveForward;
  BackwordBtn.Enabled := sqlvEngine.History.HaveBackward;
end;

procedure TMainForm.AddSql;
begin
  sqlvEngine.SQLHistory.Add('', SQLEdit.Text);
end;

procedure TMainForm.Execute(SQLCMD: TmncSQLiteCommand; SQL:TStringList; ShowGrid:Boolean);
var
  t: TDateTime;
begin
  try
    ResultEdit.Lines.Add('========= Execute ==========');
    SQLCMD.SQL.Assign(SQL);
    t := NOW;
    SQLCMD.Prepare;
{          if not ShowCMDParams(SQLCMD) then//that todo
    begin
      ResultEdit.Lines.Add('Canceled by user');
      Abort;
    end;}
    try
      ResultEdit.Lines.Add('Prepare time: ' + LogTime(t));
//            SQLCMD.NextOnExecute := False;
      t := NOW;
      SQLCMD.Execute;
      ResultEdit.Lines.Add('Execute time: ' + LogTime(t));
      if ShowGrid then
      begin
        if not SQLCMD.EOF then
        begin
          State := sqlsResults;
          DataGrid.SetFocus;
          t := NOW;
          if SQLCMD.Eof then
            ClearGrid
          else
            FillGrid(SQLCMD);
          ResultEdit.Lines.Add('Fetch time: ' + LogTime(t));
        end
        else
        begin
          ClearGrid;
          State := sqlsInfo;
        end;
      end;
      ResultEdit.Lines.Add('Last Row ID: ' + IntToStr(SQLCMD.GetLastInsertID));
      ResultEdit.Lines.Add('Rows affected: ' + IntToStr(SQLCMD.GetRowsChanged));
    except
      if ShowGrid then
        ClearGrid;
      raise;
    end;
  finally
  end;
end;

procedure TMainForm.ExecuteScript;
var
  aStrings: TStringList;
  i: Integer;
  t: TDateTime;
  SQLSession: TmncSQLiteSession;
  SQLCMD: TmncSQLiteCommand;
begin
  sqlvEngine.SaveFile('recent.sql', SQLEdit.Lines);
  SQLSession := TmncSQLiteSession.Create(sqlvEngine.Session.DBConnection);
  SQLCMD := TmncSQLiteCommand.Create(SQLSession);
  aStrings := TStringList.Create;
  try
    SqlBtn.Enabled := False;
    Screen.Cursor := crHourGlass;
    try
      SQLSession.Start;
      ResultEdit.Clear;
      AddSQL;
      for i := 0 to SQLEdit.Lines.Count - 1 do
      begin
        if Trim(SQLEdit.Lines[i]) = '^' then
        begin
          Execute(SQLCMD, aStrings, False);
          aStrings.Clear;
        end
        else
          aStrings.Add(SQLEdit.Lines[i]);
      end;
      if aStrings.Count > 0 then
        Execute(SQLCMD, aStrings, True);
      SQLSession.Commit;
    except
      on E: Exception do
      begin
        SQLSession.Rollback;
        ResultEdit.Lines.Add(E.Message);
        raise;
      end
    else
      raise;
    end;
  finally
    ResultEdit.Lines.Add('');
    Screen.Cursor := crDefault;
    SqlBtn.Enabled := True;
    aStrings.Free;
    SQLCMD.Free;
    SQLSession.Free;
  end;
end;

procedure TMainForm.ClearGrid;
var
  FixedCols: Integer;
begin
  DataGrid.FixedCols := 1;
  DataGrid.FixedRows := 1;
  DataGrid.ColCount := 2;
  DataGrid.RowCount := 2;
  DataGrid.ColWidths[0] := 20;
  DataGrid.Cells[0, 1] := '';
  DataGrid.Cells[1, 0] := '';
  DataGrid.Cells[1, 1] := '';
end;

procedure TMainForm.FillGrid(SQLCMD: TmncSQLiteCommand);

  function GetTextWidth(Text: string): Integer;
  begin
    DataGrid.Canvas.Font := DataGrid.Font;
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;
var
  i, z, c, cw, tw, w: Integer;
  s: string;
begin
  FCancel := False;
  DataGrid.ColCount := SQLCMD.Fields.Count + 1;
  DataGrid.FixedCols := 1;
  DataGrid.FixedRows := 1;
  DataGrid.ColWidths[0] := 24;
  DataGrid.Row := 1;
  DataGrid.Col := 1;
  DataGrid.Cells[0, 0] := '';
  cw := GetCharWidth; //must calc from canvas
  for i := 1 to DataGrid.ColCount - 1 do
  begin
    s := SQLCMD.Fields[i - 1].Name;
    z := 10;//SQLCMD.Fields[i - 1].Size;
    if z < 4 then
      z := 4
    else if z > 20 then
      z := 20;
    w := z * cw;
    tw := GetTextWidth(s) + 12;
    if tw > w then
      w := tw;
    tw := GetTextWidth(SQLCMD.Current.Items[i - 1].AsString) + 12;
    if tw > w then
      w := tw;
    DataGrid.ColWidths[i] := w;
    DataGrid.Cells[i, 0] := s;
  end;

  c := 1;
  while not SQLCMD.EOF do
  begin
    DataGrid.RowCount := c + 1;
    DataGrid.Cells[0, c] := IntToStr(c);
    for i := 1 to DataGrid.ColCount - 1 do
    begin
      DataGrid.Cells[i, c] := SQLCMD.Current.Items[i - 1].AsString;
    end;
    Inc(c);
    if Frac(c / 100) = 0 then
    begin
      FetchCountLbl.Caption := IntToStr(c);
      Application.ProcessMessages;
    end;
    if FCancel then
      break;
    SQLCMD.Next;
  end;
  w := GetTextWidth(IntToStr(c)) + 12;
  if w < 24 then
    w := 24;
  DataGrid.ColWidths[0] := w;
  FetchCountLbl.Caption := IntToStr(c);
end;

function TMainForm.LogTime(Start: TDateTime): string;
var
  ms, m, s: Cardinal;
begin
  ms := MilliSecondsBetween(Now, Start);
  s := (ms div 1000);
  ms := (ms mod 1000);
  m := (s div 60);
  s := (s mod 60);
  Result := Format('%d:%d:%d', [m, s, ms]);
end;

procedure TMainForm.DoAddKeyword(AKeyword: string; AKind: integer);
begin
  AKeyword := LowerCase(AKeyword);
  Completion.ItemList.Add(AKeyword);
end;

procedure TMainForm.LoadCompletion;
  procedure FillNow(Name: string; SchemaItems: TmncSchemaItems);
  var
    i: Integer;
  begin
    for i := 0 to SchemaItems.Count - 1 do
      Completion.ItemList.Add(SchemaItems[i].Name);
  end;
begin
  Completion.AddEditor(SQLEdit);
  Completion.Editor := SQLEdit;
  Completion.EndOfTokenChr := Completion.EndOfTokenChr + ' ';
  Completion.CaseSensitive := True;
  EnumerateKeywords(Ord(tkDatatype), SqliteTypes, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), SqliteFunctions, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), SqliteKeywords, SQLEdit.IdentChars, @DoAddKeyword);
  FillNow('Table', sqlvEngine.Session.Tables);
  FillNow('Fields', sqlvEngine.Session.Fields);
  FillNow('View', sqlvEngine.Session.Views);
  //FillNow('Procedure', (Owner as TfbvSession).Proceduers);
  //FillNow('Triggers', (Owner as TfbvSession).Triggers);
  //FillNow('Functions', (Owner as TfbvSession).Functions);
  //FillNow('Exceptions', (Owner as TfbvSession).Exceptions);
end;

initialization
  {$I Main.lrs}
end.

