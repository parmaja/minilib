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
{todo: More short cuts: Done}
{todo: Export/Import As CSV: DONE}
{todo: Ask for param when have params in normal execute sql script: DONE}

{todo: Assoiate with *.sqlite}
{todo: Extract the schema of whale database}
{todo: Find and Replace}
{todo: Blob access as PNG or JPG}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  dateutils, LCLType, LCLIntf, Types, mncConnections,
  contnrs, ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus,
  SynHighlighterSQLite, sqlvSessions, mncCSV,
  SynCompletion, SynEditAutoComplete, SynHighlighterHashEntries,
  mnUtils, mncSQLite, mncSchemas, mncSQLiteSchemas, mncCSVExchanges,
  sqlvClasses, sqlvStdClasses, LMessages;

type
  TExecuteType = (execNormal, execExport, execImport);
  TsqlState = (sqlsRoot, sqlsSQL, sqlsResults, sqlsInfo, sqlsMembers);

  TControlObject = class(TObject)
  public
    Control: TControl;
    UseActive: Boolean;
    Reverse: Boolean;
  end;

  TActionInfo = record
    Name: string;
    Value: string;
    Node: TsqlvNode;
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
    ActionsPanel: TPanel;
    CacheSchemaChk1: TCheckBox;
    ExclusiveChk: TCheckBox;
    ExecuteBtn: TButton;
    FileMnu: TMenuItem;
    ExitMnu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    HelpMnu: TMenuItem;
    SaveMnu: TMenuItem;
    SaveAsMnu: TMenuItem;
    OpenMnu: TMenuItem;
    StopBtn: TButton;
    VacuumChk: TCheckBox;
    FirstBtn: TSpeedButton;
    GroupsList: TComboBox;
    ActionsList: TComboBox;
    InfoBtn: TButton;
    Label4: TLabel;
    FileNameLbl: TLabel;
    Label5: TLabel;
    OpenBtn: TButton;
    GroupsPanel: TPanel;
    RefreshBtn: TButton;
    CacheSchemaChk: TCheckBox;
    RecentsCbo: TComboBox;
    Label3: TLabel;
    OpenDialog: TOpenDialog;
    ResultsBtn: TButton;
    SaveDialog: TSaveDialog;
    SchemaBtn: TButton;
    SQLSaveAsBtn: TButton;
    SQLNewBtn: TButton;
    StartBtn: TButton;
    SQLBtn: TButton;
    SQLLoadBtn: TButton;
    InfoPanel: TPanel;
    InfoLbl: TLabel;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    ResultEdit: TMemo;
    SQLBackwardBtn: TSpeedButton;
    ConnectBtn: TButton;
    DatabasesCbo: TComboBox;
    DisconnectBtn: TButton;
    AutoCreateChk: TCheckBox;
    SQLForwardBtn: TSpeedButton;
    MainMenu: TMainMenu;
    AboutMnu: TMenuItem;
    MembersGrid: TStringGrid;
    SQLSaveBtn: TButton;
    BrowseBtn: TButton;
    RemoveBtn: TButton;
    TitleLbl: TLabel;
    ToolsMnu: TMenuItem;
    DataPathCbo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    GroupPanel: TPanel;
    ClientPanel: TPanel;
    TopPanel: TPanel;
    ResultPanel: TPanel;
    RootPanel: TPanel;
    SQLEdit: TSynEdit;
    SQLPanel: TPanel;
    DataGrid: TStringGrid;
    procedure AboutMnuClick(Sender: TObject);
    procedure ActionsListSelect(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure ClientPanelClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesCboDropDown(Sender: TObject);
    procedure DataPathCboDropDown(Sender: TObject);
    procedure DataPathCboExit(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure GroupsListKeyPress(Sender: TObject; var Key: char);
    procedure GroupsListSelect(Sender: TObject);
    procedure HelpMnuClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure MembersGridClick(Sender: TObject);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MembersGridKeyPress(Sender: TObject; var Key: char);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ExitMnuClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure OpenMnuClick(Sender: TObject);
    procedure SaveMnuClick(Sender: TObject);
    procedure SaveAsMnuClick(Sender: TObject);
    procedure RecentsCboSelect(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SchemaBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure ResultsBtnClick(Sender: TObject);
    procedure SQLBackwardBtnClick(Sender: TObject);
    procedure SQLBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SQLNewBtnClick(Sender: TObject);
    procedure SQLPanelClick(Sender: TObject);
    procedure SQLSaveAsBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure SQLForwardBtnClick(Sender: TObject);
    procedure SQLLoadBtnClick(Sender: TObject);
    procedure SQLSaveBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    FLastSQLFile: string;
    FLockEnum: Boolean;
    FSqliteSyn: TSynSqliteSyn;
    Completion: TSynCompletion;
    FDataPath: string;
    FActions: array of TActionInfo;
    procedure LoadSQLFile;
    procedure SaveAsSQLFile;
    procedure SaveLastSQLFile;
    procedure CheckSearch;
    procedure SearchFor(S: string);
    procedure Connect;
    procedure Disconnect;
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
    procedure ClearGrid;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure Execute(ExecuteType: TExecuteType; SQLCMD: TmncSQLiteCommand; SQL:TStringList; ShowGrid:Boolean);
    procedure FillGrid(SQLCMD: TmncSQLiteCommand);
    procedure LoadCompletion;
    function LogTime(Start: TDateTime): string;
    procedure RefreshSQLHistory(Sender: TObject);
    procedure SetLastSQLFile(const AValue: string);
    procedure SetState(const AValue: TsqlState);
    procedure StateChanged;
    function GetDatabaseName: string;
    function GetRealDataPath: string;
    procedure SetRealDataPath(FileName: string);
    property DataPath: string read FDataPath write SetDataPath;
  public
    GroupsNames: TStringList;//Fields, Indexes
    GroupInfo: TSchemaInfo;//Table,Accounts
    SchemaName: string;//Field
    FActionsSchemaName: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRecentSQL(Silent: Boolean = False);
    procedure ExecuteScript(ExecuteType: TExecuteType);
    procedure OpenMember;
    procedure OpenGroup;
    procedure UpdateToolActions(vSchemaName: string);
    property State: TsqlState read FState write SetState;
    property LastSQLFile: string read FLastSQLFile write SetLastSQLFile;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  AboutForm, CSVIEForms, ParamsForms, SynEditMiscProcs;

{ TMainForm }

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.DatabasesCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TMainForm.BrowseBtnClick(Sender: TObject);
begin
  OpenDialog.FileName := '*.sqlite';
  OpenDialog.DefaultExt := 'sqlite';
  OpenDialog.Filter := '*.sqlite';
  OpenDialog.InitialDir := DataPathCbo.Text;
  if OpenDialog.Execute then
  begin
    SetRealDataPath(OpenDialog.FileName);
  end;
end;

procedure TMainForm.ClientPanelClick(Sender: TObject);
begin

end;

procedure TMainForm.AboutMnuClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ActionsListSelect(Sender: TObject);
var
  aMemberName: string;
  aInfo: TActionInfo;
begin
  if ActionsList.ItemIndex >= 0 then
  begin
    try
      aInfo := FActions[ActionsList.ItemIndex];
      if State = sqlsMembers then
        aMemberName := MembersGrid.Cells[0, MembersGrid.Row]
      else
        aMemberName := '';
      aInfo.Node.Execute(aMemberName, TmncParams.Create([aInfo.Name], [aInfo.Value]));
    finally
      ActionsList.ItemIndex := -1;
    end;
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
  Disconnect;
end;

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
begin
  ExecuteScript(execNormal);
end;

procedure TMainForm.FirstBtnClick(Sender: TObject);
begin
  sqlvEngine.Launch('Database', 'Databases', DatabasesCbo.Text);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  sqlvEngine.SaveFile('recent.sql', SQLEdit.Lines);
  sqlvEngine.Session.Close;
  Application.Terminate;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  w: Integer;
begin
  if SQLEdit.Modified and (LastSQLFile <> '') then
  begin
     w := MessageDlg('Save', 'SQL not saved, do you want to save', mtWarning, mbYesNoCancel, '');
     if w = mrYes then
       SaveAsSQLFile
     else if w = mrCancel then
       CanClose := False;
  end;
end;

procedure TMainForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Msg.KeyData);
  case Msg.CharCode of
    VK_F7:
    begin
      State := sqlsInfo;
      Handled := True;
    end;
    VK_F8:
    begin
      if State = sqlsSQL then
      begin
        State := sqlsMembers;
        MembersGrid.SetFocus;
      end
      else
      begin
        State := sqlsSQL;
        SQLEdit.SetFocus;
      end;
      Handled := True;
    end;
    else
    begin
      case State of
        sqlsRoot:
          case Msg.CharCode of
            VK_F9:
            begin
              if ssShift in ShiftState then
                Disconnect
              else
                Connect;
              Handled := True;
            end;
          end;
        sqlsMembers:
          case Msg.CharCode of
            VK_F6:
            begin
              if MembersGrid.Focused then
                GroupsList.SetFocus
              else
                MembersGrid.SetFocus;
              Handled := True;
            end;
          end;
        sqlsResults:
          case Msg.CharCode of
            VK_F6:
            begin
              State := sqlsSQL;
              SQLEdit.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.Session.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        sqlsInfo:
          case Msg.CharCode of
            VK_F6:
            begin
              State := sqlsSQL;
              SQLEdit.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.Session.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        sqlsSQL:
          case Msg.CharCode of
            Ord('S'):
            begin
              if ssCtrl in ShiftState then
                SaveLastSQLFile;
            end;
            VK_F6:
            begin
              State := sqlsResults;
              DataGrid.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.Session.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TMainForm.GroupsListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    MembersGrid.SetFocus;
end;

procedure TMainForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup;
end;

procedure TMainForm.HelpMnuClick(Sender: TObject);
begin
  ResultEdit.Clear;
  ResultEdit.Lines.Add('===Help===');
  ResultEdit.Lines.Add('[Keys]');
  ResultEdit.Lines.Add('F5 Refresh schema');
  ResultEdit.Lines.Add('F6 Switch between Result and SQL editor');
  ResultEdit.Lines.Add('F7 Switch to Info');
  ResultEdit.Lines.Add('F8 Switch to SQL editor');
  ResultEdit.Lines.Add('F9 to excecute sql sctipt');
  State := sqlsInfo;
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

procedure TMainForm.MembersGridDblClick(Sender: TObject);
begin
  OpenMember;
end;

procedure TMainForm.MembersGridKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TMainForm.MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F3:
    begin
      if FSearch <> '' then
        SearchFor(FSearch);
    end;
    VK_RETURN:
    begin
      OpenMember;
    end;
  end;
end;

procedure TMainForm.MembersGridUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  CheckSearch;
  FSearch := FSearch + UTF8Key;
  SearchFor(FSearch);
end;

procedure TMainForm.ExitMnuClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  State := sqlsRoot;
end;

procedure TMainForm.OpenMnuClick(Sender: TObject);
begin
  LoadSQLFile;
end;

procedure TMainForm.SaveMnuClick(Sender: TObject);
begin
  SaveLastSQLFile;
end;

procedure TMainForm.SaveAsMnuClick(Sender: TObject);
begin
  SaveAsSQLFile;
end;

procedure TMainForm.RecentsCboSelect(Sender: TObject);
begin
  if RecentsCbo.ItemIndex >= 0 then
    SetRealDataPath(RecentsCbo.Text);
end;

procedure TMainForm.RefreshBtnClick(Sender: TObject);
begin
  EnumDatabases;
end;

procedure TMainForm.RemoveBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := sqlvEngine.Recents.IndexOf(RecentsCbo.Text);
  if i >= 0 then
  begin
    sqlvEngine.Recents.Delete(i);
    sqlvEngine.SaveRecents;
    RecentsCbo.Items.Assign(sqlvEngine.Recents);
  end;
end;

procedure TMainForm.SchemaBtnClick(Sender: TObject);
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

procedure TMainForm.SQLForwardBtnClick(Sender: TObject);
begin
  sqlvEngine.SQLHistory.Forward;
  if sqlvEngine.SQLHistory.Current <> nil then
    SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
end;

procedure TMainForm.SQLBackwardBtnClick(Sender: TObject);
begin
  sqlvEngine.SQLHistory.Backward;
  if (sqlvEngine.SQLHistory.Current <> nil) then
    SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
end;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
  OpenMember;
end;

procedure TMainForm.SQLNewBtnClick(Sender: TObject);
begin
  AddRecentSQL;
  LastSQLFile := '';
  SQLEdit.Lines.Clear;
  SQLEdit.Modified := False;
  SQLEdit.SetFocus;
end;

procedure TMainForm.SQLPanelClick(Sender: TObject);
begin

end;

procedure TMainForm.SQLSaveAsBtnClick(Sender: TObject);
begin
  SaveAsSQLFile;
end;

procedure TMainForm.SQLBtnClick(Sender: TObject);
begin
  State := sqlsSQL;
end;

procedure TMainForm.StartBtnClick(Sender: TObject);
begin
  State := sqlsRoot;
end;

procedure TMainForm.SQLLoadBtnClick(Sender: TObject);
begin
  LoadSQLFile;
end;

procedure TMainForm.SQLSaveBtnClick(Sender: TObject);
begin
  SaveLastSQLFile;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TMainForm.LoadSQLFile;
begin
  State := sqlsSQL;
  OpenDialog.FileName := '*.sql';
  OpenDialog.DefaultExt := 'sql';
  OpenDialog.Filter := '*.sql';
  if OpenDialog.FileName = '' then
    OpenDialog.InitialDir := Application.Location;
  if OpenDialog.Execute then
  begin
    AddRecentSQL;
    LastSQLFile := OpenDialog.FileName;
    SQLEdit.Lines.LoadFromFile(LastSQLFile);
    SQLEdit.Modified := False;
  end;
end;

procedure TMainForm.SaveAsSQLFile;
begin
  State := sqlsSQL;
  SaveDialog.FileName := LastSQLFile;
  SaveDialog.DefaultExt := 'sql';
  SAveDialog.Filter := '*.sql';
  SaveDialog.InitialDir := Application.Location;
  if SaveDialog.Execute then
  begin
    LastSQLFile := SaveDialog.FileName;
    SaveLastSQLFile;
  end;
end;

procedure TMainForm.SaveLastSQLFile;
begin
  State := sqlsSQL;
  if LastSQLFile = '' then
  begin
    SaveAsSQLFile;
  end
  else if LastSQLFile <> '' then
  begin
    SQLEdit.Lines.SaveToFile(LastSQLFile);
    SQLEdit.Modified := False;
  end;
end;

procedure TMainForm.CheckSearch;
begin
  if (FSearch = '') or (SecondsBetween(Now, FSearchTime) > 3) then
  begin
    FFirstSearch := True;
    FSearch := '';
  end;
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
    //t := MembersGrid.Cells[0, i];
    //if Pos(s, t) > 0 then
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
  if not sqlvEngine.Session.IsActive then
  begin
    sqlvEngine.Setting.CacheSchemas := CacheSchemaChk.Checked;
    sqlvEngine.Session.Open(GetDatabaseName, AutoCreateChk.Checked, ExclusiveChk.Checked, VacuumChk.Checked);
    sqlvEngine.Launch('Database', 'Databases', DatabasesCbo.Text);
  end;
end;

procedure TMainForm.Disconnect;
begin
  if sqlvEngine.Session.IsActive then
  begin
    sqlvEngine.Session.Close;
    StateChanged;
  end;
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
  OldFile: string;
  aUpPath: string;
begin
  if not FLockEnum then
  begin
    FLockEnum := True;
    try
      aFileSearcher := TFileSearcher.Create;
      aFiles := TStringList.Create;
      OldFile := DatabasesCbo.Text;
      DataPathCbo.Items.BeginUpdate;
      DatabasesCbo.Items.BeginUpdate;
      try
        DatabasesCbo.Items.Clear;
        DataPathCbo.Items.Clear;
        aUpPath := ExpandFileNameUTF8(IncludeTrailingPathDelimiter(FDataPath) + '..\');
        if aUpPath <> FDataPath then
          DataPathCbo.Items.Add(aUpPath);
        DataPathCbo.Items.Add(FDataPath);
        aFileSearcher.OnDirectoryFound := @DirectoryFoundEvent;
        aFileSearcher.OnFileFound := @FileFoundEvent;
        aFileSearcher.Search(GetRealDataPath, '*.sqlite;*.db', False);
        DataPathCbo.Text := FDataPath;
        if DatabasesCbo.Items.IndexOf(OldFile) >=0 then
          DatabasesCbo.Text := OldFile;
      finally
        aFileSearcher.Free;
        aFiles.Free;
        DataPathCbo.Items.EndUpdate;
        DatabasesCbo.Items.EndUpdate;
        DataPathCbo.Refresh;
        DatabasesCbo.Refresh;
      end;
    finally
      FLockEnum := False;
    end;
  end;
end;

procedure TMainForm.SetDataPath(const AValue: string);
begin
  if not FLockEnum then
    if FDataPath <> AValue then
    begin
      FDataPath := AValue;
      EnumDatabases;
    end;
end;

procedure TMainForm.Connected;
begin
  LoadCompletion;
  RecentsCbo.Items.Assign(sqlvEngine.Recents);
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
    sqlsRoot:
    begin
      PanelsList.Show(RootPanel, sqlvEngine.Session.IsActive);
      UpdateToolActions('GUI.Root');
    end;
    sqlsSQL:
    begin
      PanelsList.Show(SQLPanel, sqlvEngine.Session.IsActive);
      UpdateToolActions('GUI.SQL');
    end;
    sqlsResults:
    begin
      PanelsList.Show(ResultPanel, sqlvEngine.Session.IsActive);
      {if DataGrid.CanFocus then
        DataGrid.SetFocus;}
      UpdateToolActions('GUI.Results');
    end;
    sqlsInfo:
    begin
      PanelsList.Show(InfoPanel, sqlvEngine.Session.IsActive);
      UpdateToolActions('GUI.Info');
    end;
    sqlsMembers:
    begin
      PanelsList.Show(GroupPanel, sqlvEngine.Session.IsActive);
      UpdateToolActions(SchemaName);
      {if MembersGrid.CanFocus then
        MembersGrid.SetFocus;}
    end;
  end;
end;

procedure TMainForm.UpdateToolActions(vSchemaName: string);
var
  i, c: Integer;
  aNodes: TsqlvNodes;
begin
  if FActionsSchemaName <> vSchemaName then
  begin
    FActionsSchemaName := vSchemaName;
    ActionsList.Items.Clear;
    FActions := nil;
    if vSchemaName <> '' then
    begin
      aNodes := TsqlvNodes.Create;
      try
        sqlvEngine.Enum(vSchemaName, aNodes, sqlvEngine.Session.IsActive);
        c := 0;
        for i := 0 to aNodes.Count - 1 do
        if nsCommand in aNodes[i].Style then
        begin
          SetLength(FActions, c + 1);
          ActionsList.Items.Add(aNodes[i].Title);
          FActions[c].Name := GroupInfo.Name;
          FActions[c].Value := GroupInfo.Value;
          FActions[c].Node := aNodes[i];
          Inc(c);
        end;
      finally
        aNodes.Free;
      end;
    end;
    ActionsPanel.Visible := ActionsList.Items.Count > 0;
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
  //ScaleBy(PixelsPerInch, 96);
  {$endif}
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
  PanelsList.Add(RootPanel, SQLBtn, False);
  PanelsList.Add(RootPanel, SchemaBtn, True);
  PanelsList.Add(SQLPanel);
  PanelsList.Add(SQLPanel, ResultsBtn, True);
  PanelsList.Add(SQLPanel, SchemaBtn, True);
  PanelsList.Add(SQLPanel, InfoBtn, True);
  PanelsList.Add(ResultPanel);
  PanelsList.Add(ResultPanel, SQLBtn, False);
  PanelsList.Add(ResultPanel, InfoBtn, True);
  PanelsList.Add(ResultPanel, SchemaBtn, True);
  PanelsList.Add(GroupPanel);
  PanelsList.Add(GroupPanel, OpenBtn, True);
  PanelsList.Add(GroupPanel, SQLBtn, False);
  PanelsList.Add(InfoPanel);
  PanelsList.Add(InfoPanel, ResultsBtn, True);
  PanelsList.Add(InfoPanel, SQLBtn, False);
  PanelsList.Add(InfoPanel, SchemaBtn, True);

  sqlvEngine.SQLHistory.OnChanged := @RefreshSQLHistory;
  sqlvEngine.LoadFile('recent.sql', SQLEdit.Lines);
  sqlvEngine.History.Changed;
  sqlvEngine.SQLHistory.Changed;
  if ParamCount > 0 then
  begin
    aFile := ParamStrUTF8(1);
    if FileExistsUTF8(aFile) then
    begin
      if SameText(ExtractFileExt(aFile), '.sql') then
      begin
        LastSQLFile := aFile;
        SQLEdit.Lines.LoadFromFile(aFile);
        State := sqlsSQL;
      end
      else if SameText(ExtractFileExt(aFile), '.sqlite') then
      begin
        SetRealDataPath(aFile);
        Connect;
      end;
    end;
  end
  else
  begin
    if sqlvEngine.Recents.Count > 0 then
      SetRealDataPath(sqlvEngine.Recents[0]);
  end;
  RecentsCbo.Items.Assign(sqlvEngine.Recents);
  StateChanged;
  if not sqlvEngine.Session.IsActive then //already loaded in connect
    LoadCompletion;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(GroupsNames);
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
    sqlvEngine.Launch(GroupsNames[GroupsList.ItemIndex], GroupInfo.Name, GroupInfo.Value, True);
end;

procedure TMainForm.RefreshSQLHistory(Sender: TObject);
begin
  SQLForwardBtn.Enabled := sqlvEngine.SQLHistory.HaveForward;
  SQLBackwardBtn.Enabled := sqlvEngine.SQLHistory.HaveBackward;
end;

procedure TMainForm.SetLastSQLFile(const AValue: string);
begin
  FLastSQLFile := AValue;
  FileNameLbl.Caption := AValue;
end;

procedure TMainForm.AddRecentSQL(Silent: Boolean);
begin
  sqlvEngine.SQLHistory.Add(SQLEdit.Text, Silent);
end;

procedure TMainForm.Execute(ExecuteType: TExecuteType; SQLCMD: TmncSQLiteCommand; SQL:TStringList; ShowGrid:Boolean);
var
  t: TDateTime;
  aExport: TmncCSVExport;
  aImport: TmncCSVImport;
  aStream : TFileStream;
begin
  try
    ResultEdit.Lines.Add('========= Execute ==========');
    SQLCMD.SQL.Assign(SQL);
    try
      case ExecuteType of
        execNormal:
        begin
          t := NOW;
          SQLCMD.Prepare;
          ResultEdit.Lines.Add('Prepare time: ' + LogTime(t));

          if (SQLCMD.Params.Count > 0) then
            if not ShowSQLParams(SQLCMD) then
            begin
              SQLCMD.Close;
              ResultEdit.Lines.Add('Canceled by user');
              exit;
            end;

          t := NOW;
          Screen.Cursor := crHourGlass;
          SQLCMD.Execute;
          Screen.Cursor := crDefault;
          ResultEdit.Lines.Add('Execute time: ' + LogTime(t));
          if ShowGrid then
          begin
            if not SQLCMD.EOF then
            begin
              State := sqlsResults;
              Application.ProcessMessages;
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
        end;
        execExport:
        begin
          aExport := TmncCSVExport.Create;
          try
            SaveDialog.FileName := '*.csv';
            SaveDialog.DefaultExt := 'csv';
            SaveDialog.Filter := '*.csv';
            if SaveDialog.Execute and ShowCSVIEOptions('Export CSV', aExport) then
            begin
              aStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
              try
                aExport.Command := SQLCMD;
                aExport.Stream := aStream;
                Screen.Cursor := crHourGlass;
                t := NOW;
                aExport.Execute;
                ResultEdit.Lines.Add('Export time: ' + LogTime(t));
                ResultEdit.Lines.Add('Export count: ' + IntToStr(aExport.Count));
                Screen.Cursor := crDefault;
                State := sqlsInfo;
                ShowMessage('Export count: '+IntToStr(aExport.Count));
              finally
                aStream.Free;
              end;
            end;
          finally
            aExport.Free;
          end;
        end;
        execImport:
        begin
          OpenDialog.FileName := '*.csv';
          OpenDialog.DefaultExt := 'csv';
          OpenDialog.Filter := '*.csv';
          if OpenDialog.Execute then
          begin
            SQLCMD.Prepare;
            if (SQLCMD.Params.Count = 0) then
            begin
              ShowMessage('SQL statment must have params for import');
              exit;
            end;
            aImport := TmncCSVImport.Create;
            try
              if ShowCSVIEOptions('Import CSV', aImport) then
              begin
                aStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
                try
                  aImport.Command := SQLCMD;
                  aImport.Stream := aStream;
                  Screen.Cursor := crHourGlass;
                  t := NOW;
                  aImport.Execute;
                  ResultEdit.Lines.Add('Import time: ' + LogTime(t));
                  ResultEdit.Lines.Add('Import count: ' + IntToStr(aImport.Count));
                  Screen.Cursor := crDefault;
                  State := sqlsInfo;
                  ShowMessage('Import count: '+ IntToStr(aImport.Count));
                finally
                  aStream.Free;
                end;
              end;
            finally
              aImport.Free;
            end;
          end;
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

procedure TMainForm.ExecuteScript(ExecuteType: TExecuteType);
var
  aStrings: TStringList;
  i: Integer;
  SQLCMD: TmncSQLiteCommand;
  aStart: Integer;
begin
  sqlvEngine.SaveFile('recent.sql', SQLEdit.Lines);
  SQLCMD := TmncSQLiteCommand.CreateBy(sqlvEngine.Session.DBSession);
  aStrings := TStringList.Create;
  try
    SqlBtn.Enabled := False;
    StopBtn.Visible := True;
    try
      ResultEdit.Clear;
      AddRecentSQL;
      aStart := 0;
      for i := 0 to SQLEdit.Lines.Count - 1 do
      begin
        if Trim(SQLEdit.Lines[i]) = '^' then
        begin
          Execute(ExecuteType, SQLCMD, aStrings, False);
          aStrings.Clear;
          aStart := i + 1;//+1 to skip terminator
        end
        else
          aStrings.Add(SQLEdit.Lines[i]);
      end;
      if aStrings.Count > 0 then
        Execute(ExecuteType, SQLCMD, aStrings, True);
      SQLCMD.Commit;
    except
      on E: Exception do
      begin
        SQLCMD.Rollback;
        ResultEdit.Lines.Add(E.Message);
        SQLEdit.CaretX := 0;
        SQLEdit.CaretY := aStart + 1;//CaretY start from 1
        raise;
      end
    else
      raise;
    end;
  finally
    ResultEdit.Lines.Add('');
    Screen.Cursor := crDefault;
    SqlBtn.Enabled := True;
    StopBtn.Visible := False;
    aStrings.Free;
    SQLCMD.Free;
    sqlvEngine.Session.DBSession.Active := True;
  end;
end;

procedure TMainForm.ClearGrid;
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
  str: utf8string;
begin
  FCancel := False;
  //DataGrid.BeginUpdate;
  try
    DataGrid.ColCount := SQLCMD.Fields.Count + 1;
    DataGrid.FixedCols := 1;
    DataGrid.FixedRows := 1;
    DataGrid.RowCount := 1;
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
    Application.ProcessMessages;

    c := 1;
    while not SQLCMD.EOF do
    begin
      DataGrid.RowCount := c + 1;
      DataGrid.Cells[0, c] := IntToStr(c);
      for i := 1 to DataGrid.ColCount - 1 do
      begin
        str := SQLCMD.Current.Items[i - 1].AsString;
        DataGrid.Cells[i, c] := str;
      end;
      Inc(c);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (c < 100) or (Frac(c / 100) = 0) then
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
  finally
    Application.ProcessMessages;
    //DataGrid.EndUpdate;
  end;
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
{  FreeAndNil(Completion);
  Completion := TSynCompletion.Create(nil);} //there is bug in TSynCompletion when remove editor
  if Completion = nil then
  begin
    Completion := TSynCompletion.Create(nil);
    Completion.AddEditor(SQLEdit);
    Completion.TheForm.Font.Assign(SQLEdit.Font);
    Completion.CaseSensitive := False;
  end
  else
    Completion.ItemList.Clear;
  EnumerateKeywords(Ord(tkDatatype), SqliteTypes, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), SqliteFunctions, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), SqliteKeywords, SQLEdit.IdentChars, @DoAddKeyword);
  if sqlvEngine.Session.IsActive then
  begin
    FillNow('Table', sqlvEngine.Session.Tables);
    FillNow('Fields', sqlvEngine.Session.Fields);
    FillNow('View', sqlvEngine.Session.Views);
  end;
  //FillNow('Procedure', (Owner as TfbvSession).Proceduers);
  //FillNow('Triggers', (Owner as TfbvSession).Triggers);
  //FillNow('Functions', (Owner as TfbvSession).Functions);
  //FillNow('Exceptions', (Owner as TfbvSession).Exceptions);
end;

end.

