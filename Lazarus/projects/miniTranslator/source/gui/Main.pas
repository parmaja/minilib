unit Main;
{**
 * This file is part of the "Mini Translator" http://www.sourceforge.net/projects/minilib
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, Classes, SysUtils, StrUtils,
  LResources, LCLIntf, LangUtils,
  Registry, Contnrs, FileCtrl, Dialogs, Grids, LangClasses, Menus,
  ExtCtrls, trsProjects, trsClasses,
  Setups, PO_Languages,
  mnXMLUtils, mnXMLRtti, mnXMLRttiProfile;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    EditCommentBtn: TButton;
    HintLbl: TLabel;
    MainMenu: TMainMenu;
    ExitMnu: TMenuItem;
    MenuItem1: TMenuItem;
    ReopenFilesMnu: TMenuItem;
    WorkPathMnu: TMenuItem;
    OptionsMnu: TMenuItem;
    ProjectOptionsMnu: TMenuItem;
    SaveMnu: TMenuItem;
    OpenMnu: TMenuItem;
    NewMnu: TMenuItem;
    _MenuItem: TMenuItem;
    ProjectsMnu: TMenuItem;
    HelpMnu: TMenuItem;
    AboutMnu: TMenuItem;
    FileMnu: TMenuItem;
    OpenProjectMnu: TMenuItem;
    FooterPanel: TPanel;
    SaveAsProjectMnu: TMenuItem;
    EditMnu: TMenuItem;
    NextMnu: TMenuItem;
    FindMnu: TMenuItem;
    FindNonTransMnu: TMenuItem;
    FindNextMnu: TMenuItem;
    SaveAllMnu: TMenuItem;
    FindInLocalMnu: TMenuItem;
    FindinOriginalMnu: TMenuItem;
    N2: TMenuItem;
    OpenDialog: TOpenDialog;
    FindFirstMnu: TMenuItem;
    CleanMnu: TMenuItem;
    NewProjectMnu: TMenuItem;
    ViewMnu: TMenuItem;
    LeftLayoutMnu: TMenuItem;
    TopLayoutMnu: TMenuItem;
    N5: TMenuItem;
    ReopenProjectMnu: TMenuItem;
    SaveProjectMnu: TMenuItem;
    SaveDialog: TSaveDialog;
    Close1: TMenuItem;
    UpgradeMnu: TMenuItem;
    Panel2: TPanel;
    OriginalPanel: TPanel;
    Label5: TLabel;
    OriginalValueEdit: TMemo;
    LocalPanel: TPanel;
    Label6: TLabel;
    LeftSplitter: TSplitter;
    LeftLayoutPanel: TPanel;
    TopLayoutPanel: TPanel;
    Label1: TLabel;
    AllIdentList: TListBox;
    Add1: TMenuItem;
    LocalValueEdit: TMemo;
    SearchMnu: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
    Bookmark1: TMenuItem;
    MergeMnu: TMenuItem;
    _N7: TMenuItem;
    N8: TMenuItem;
    ExportMnu: TMenuItem;
    CommentEdit: TEdit;
    KeyLbl: TEdit;
    Splitter1: TSplitter;
    FilterNotTranslatedMnu: TMenuItem;
    Progressreport1: TMenuItem;
    Logreport1: TMenuItem;
    PriorMnu: TMenuItem;
    N9: TMenuItem;
    FontMnu: TMenuItem;
    FontDialog: TFontDialog;
    Panel1: TPanel;
    FindEdit: TEdit;
    Label4: TLabel;
    Button1: TButton;
    Label7: TLabel;
    FindKindCbo: TComboBox;
    Panel3: TPanel;
    Label2: TLabel;
    SectionList: TListBox;
    Panel4: TPanel;
    Label3: TLabel;
    IdentList: TListBox;
    Splitter2: TSplitter;
    UpSplitter: TSplitter;
    Label8: TLabel;
    Label9: TLabel;
    ToolsMnu: TMenuItem;
    Removedublicated1: TMenuItem;
    Removeduplicated1: TMenuItem;
    Retranslaterepeated1: TMenuItem;
    N3: TMenuItem;
    N10: TMenuItem;
    Reportretranslate1: TMenuItem;
    procedure ApplicationPropertiesShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure EditCommentBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IdentListClick(Sender: TObject);
    procedure IdentListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OpenMnuClick(Sender: TObject);
    procedure ProjectOptionsMnuClick(Sender: TObject);
    procedure SaveAsProjectMnuClick(Sender: TObject);
    procedure SaveMnuClick(Sender: TObject);
    procedure SectionListClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ExitMnuClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AboutMnuClick(Sender: TObject);
    procedure NextMnuClick(Sender: TObject);
    procedure FindMnuClick(Sender: TObject);
    procedure FindNextMnuClick(Sender: TObject);
    procedure SaveAllMnuClick(Sender: TObject);
    procedure FindFirstMnuClick(Sender: TObject);
    procedure CleanMnuClick(Sender: TObject);
    procedure LeftLayoutMnuClick(Sender: TObject);
    procedure TopLayoutMnuClick(Sender: TObject);
    procedure NewProjectMnuClick(Sender: TObject);
    procedure OptionsMnuClick(Sender: TObject);
    procedure SaveProjectMnuClick(Sender: TObject);
    procedure OpenProjectMnuClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure UpgradeMnuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LocalValueEditChange(Sender: TObject);
    procedure Logreport1Click(Sender: TObject);
    procedure AllIdentListClick(Sender: TObject);
    procedure PriorMnuClick(Sender: TObject);
    procedure FontMnuClick(Sender: TObject);
    procedure FindKindCboClick(Sender: TObject);
    procedure FindEditKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure FindInLocalMnuClick(Sender: TObject);
    procedure FindinOriginalMnuClick(Sender: TObject);
    procedure Removedublicated1Click(Sender: TObject);
    procedure Removeduplicated1Click(Sender: TObject);
    procedure Progressreport1Click(Sender: TObject);
    procedure Retranslaterepeated1Click(Sender: TObject);
    procedure Reportretranslate1Click(Sender: TObject);
    procedure FilterNotTranslatedMnuClick(Sender: TObject);
    procedure WorkPathMnuClick(Sender: TObject);
  private
    Showed: Boolean;
    FProject: TtrsProject;
    procedure UpdateFonts;
    function GetProject: TtrsProject;
    procedure SetProject(const AValue: TtrsProject);
    function CreateDefaultProject(vExtension: string): TtrsProject;
    procedure UpdateView;
    procedure FindByEdit;
    procedure ReopenProjectClick(Sender: TObject);
    procedure ReopenFileClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure ProjectLoaded;
    procedure LoadProject(FileName: string; Upgrade: Boolean);
    procedure SaveProject(FileName: string; Force: Boolean);
    procedure LoadFile(FileName: string);
    procedure SaveFile(FileName: string);
    procedure SaveAsProject;
    procedure JumpTo(Word: TLangItem);
    procedure ClearAll;
    procedure CloseProject;
    function AskCloseProject: Boolean;
    procedure ProcessRecentProjects(FileName: string);
    procedure EnumRecentProjects;
    procedure ProcessRecentFiles(FileName: string);
    procedure EnumRecentFiles;
    procedure EnumContents;
    procedure EnumIdents(Silent: Boolean = False);
    procedure EnumAll;
    procedure Enumerate;
    procedure UpdateIdentList;
    procedure UpdateAllIdentList;
    procedure LoadValueFromWord(Word: TLangItem);
    procedure LoadValueFromList;
    procedure LoadValueFromAllList;
    procedure FindFirst;
    procedure EnumExports;
    procedure RefreshProject;
    function FindNext: Boolean;
    procedure EnumTools(vCaption: string; vOnClick: TNotifyEvent; vAutoInSave:Boolean);
    procedure Loaded; override;
  public
    FindIndex: Integer;
    procedure ResetFind;
    property Project: TtrsProject read GetProject write SetProject;
    procedure UpdateSetting;
  end;

var
  MainForm: TMainForm;

implementation

uses
  AboutForms, NewProjectForms, OptionForms,
  LogForms;


procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Project <> nil then
  begin
    CloseProject;
  end;
  trsEngine.Options.TopLayout := TopLayoutMnu.Checked;
  trsEngine.Options.FormWidth := Width;
  trsEngine.Options.FormHeight := Height;
  trsEngine.SaveOptions;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
end;

procedure TMainForm.ApplicationPropertiesShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintLbl.Caption := HintInfo.HintStr;
end;

procedure TMainForm.EditCommentBtnClick(Sender: TObject);
begin

end;

procedure TMainForm.EnumContents;
var
  i, j: Integer;
  aContents: TLangContents;
begin
  SectionList.Items.BeginUpdate;
  try
    SectionList.Clear;
    for i := 0 to Project.Dictionary.Local.Count - 1 do
    begin
      aContents := Project.Dictionary.Local[i];
      if aContents.Visible then
        SectionList.Items.AddObject(aContents.Name, aContents);
    end;
    if SectionList.Items.Count > 0 then
      SectionList.ItemIndex := 0;
  finally
    SectionList.Items.EndUpdate;
  end;
end;

procedure TMainForm.EnumIdents(Silent: Boolean);
var
  i: Integer;
  aContents: TLangContents;
  Filtered:Boolean;
begin
  Filtered := FilterNotTranslatedMnu.Checked;
  IdentList.Items.BeginUpdate;
  try
    IdentList.Clear;
    if SectionList.ItemIndex >= 0 then
    begin
      aContents := Project.Dictionary.Local.Find(SectionList.Items[SectionList.ItemIndex]);
      for i := 0 to aContents.Count - 1 do
      begin
         if aContents[i].Visible and (not Filtered or (aContents.Items[i].Text='')) then
          IdentList.Items.AddObject(aContents.Items[i].ID, aContents.Items[i]);
      end;
    end;
    if not Silent and (IdentList.Items.Count > 0) then
    begin
      IdentList.ItemIndex := 0;
      LoadValueFromList;
    end;
  finally
    IdentList.Items.EndUpdate;
  end;
end;

procedure TMainForm.IdentListClick(Sender: TObject);
begin
  LoadValueFromList;
end;

procedure TMainForm.IdentListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  //ARect.Right := ARect.Left + 3;
end;

procedure TMainForm.OpenMnuClick(Sender: TObject);
var
  e: string;
begin
  OpenDialog.DefaultExt := 'po';
  OpenDialog.Filter := 'PO gettext|*.po|Plan files|*.ini|All Files|*.*';
  OpenDialog.FileName := '*.po;*.ini';
  if OpenDialog.Execute then
  begin
    LoadFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.ProjectOptionsMnuClick(Sender: TObject);
begin
  if Project <> nil then
  begin
    try
      ShowOptions(Project, False);
      ProjectLoaded;
      Enumerate;
    except
      on E: Exception do
      begin
        Project.Log.Add('Error: ' + E.Message);
        ClearAll;
        raise;
      end;
    end;
  end;
end;

procedure TMainForm.SaveAsProjectMnuClick(Sender: TObject);
begin

end;

procedure TMainForm.SaveMnuClick(Sender: TObject);
begin
  SaveFile('');
end;

procedure TMainForm.LoadValueFromList;
begin
  if (IdentList.ItemIndex >= 0) and (SectionList.ItemIndex >= 0) then
    LoadValueFromWord(TLangItem(IdentList.Items.Objects[IdentList.ItemIndex]))
  else
    LoadValueFromWord(nil);
  UpdateAllIdentList;
end;

procedure TMainForm.SectionListClick(Sender: TObject);
begin
  if SectionList.ItemIndex >= 0 then
  begin
    EnumIdents;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := AskCloseProject;
end;

procedure TMainForm.ExitMnuClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Enumerate;
begin
  Project.Current := nil;
  if (Project.Dictionary.Local <> nil) then
  begin
    LocalValueEdit.Text := '';
    OriginalValueEdit.Text := '';
    if Project.Dictionary.Local.IsRightToLeft then
      LocalValueEdit.BidiMode := bdRightToLeft
    else
      LocalValueEdit.BidiMode := bdLeftToRight;

    if lffMultiple in Project.FilerClass.GetFlags then
      LocalValueEdit.WantReturns := False
    else
      LocalValueEdit.WantReturns := True;

    OriginalPanel.Visible := (Project.Dictionary.Original <> nil) or (lffAlone in Project.FilerClass.GetFlags);
    if Project.Dictionary.Original <> nil then
    begin
      if Project.Dictionary.Original.IsRightToLeft then
        OriginalValueEdit.BidiMode := bdRightToLeft
      else
        OriginalValueEdit.BidiMode := bdLeftToRight;

      if lffMultiple in Project.FilerClass.GetFlags then
        OriginalValueEdit.WantReturns := False
      else
        OriginalValueEdit.WantReturns := True;
    end;
    EnumContents;
    EnumIdents;
    EnumAll;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not Showed then
  begin
    Showed := True;
//    ShowWizard;
  end;
end;

procedure TMainForm.AboutMnuClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.NextMnuClick(Sender: TObject);
begin
  if AllIdentList.ItemIndex < AllIdentList.Items.Count - 1 then
  begin
    AllIdentList.ItemIndex := AllIdentList.ItemIndex + 1;
    LoadValueFromAllList;
  end;
end;

procedure TMainForm.FindMnuClick(Sender: TObject);
var
  s: string;
  aWordItem: TLangItem;
  Dictionary: TtrsLanguage;
begin
  if FindInLocalMnu.Checked then
    Dictionary := Project.Dictionary.Local
  else
    Dictionary := Project.Dictionary.Original;
  if (Dictionary <> nil) and InputQuery('Find', 'Enter Key name', s) then
  begin
    aWordItem := Dictionary.FindID(s);
    if aWordItem <> nil then
    begin
      JumpTo(aWordItem);
    end;
  end;
end;

procedure TMainForm.FindFirst;
var
  aItem: TLangItem;
  Dictionary: TtrsLanguage;
begin
  if FindInLocalMnu.Checked then
    Dictionary := Project.Dictionary.Local
  else
    Dictionary := Project.Dictionary.Original;
  if (Project.Dictionary.Local <> nil) and InputQuery('Find Value', 'Enter any text to find', Project.FindWordStr) then
  begin
    ResetFind;
    //aItem := Dictionary.FindID;
    if aItem <> nil then
    begin
      JumpTo(aItem);
    end
    else
      ShowMessage('Not found');
  end;
end;

procedure TMainForm.FindNextMnuClick(Sender: TObject);
begin
  FindNext;
end;

function TMainForm.FindNext: Boolean;
var
  aItem: TLangItem;
  Dictionary: TtrsLanguage;
begin
  Result := False;
  if FindInLocalMnu.Checked then
    Dictionary := Project.Dictionary.Local
  else
    Dictionary := Project.Dictionary.Original;
  if (Project.Dictionary.Local <> nil) then
  begin
    if (Project.FindWordStr = '') then
      FindFirst
    else
    begin
      //aItem := Dictionary.FindWordNext(Project.FindWordStr);
      if aItem <> nil then
      begin
        Result := True;
        JumpTo(aItem);
      end
      else
      begin
        ShowMessage('End of search');
        ResetFind;
      end;
    end;
  end;
end;

procedure TMainForm.SaveAllMnuClick(Sender: TObject);
begin
  SaveProject(Project.FileName, True);
end;

procedure TMainForm.FindFirstMnuClick(Sender: TObject);
begin
  FindFirst;
end;

procedure TMainForm.CleanMnuClick(Sender: TObject);
begin
  Project.Dictionary.Local.CheckWith(Project.Dictionary.Original);
end;

procedure TMainForm.UpdateSetting;
begin
  if Project <> nil then
    Caption := Project.Name + ' - miniTranslator'
  else
    Caption := 'miniTranslator'
end;

procedure TMainForm.LeftLayoutMnuClick(Sender: TObject);
begin
  UpdateView;
end;

procedure TMainForm.TopLayoutMnuClick(Sender: TObject);
begin
  UpdateView;
end;

procedure TMainForm.NewProjectMnuClick(Sender: TObject);
var
  aProject: TtrsProject;
begin
  if AskCloseProject then
  begin
    aProject := ShowNewProject;
    if aProject <> nil then
    begin
      try
        if ShowOptions(aProject, True) then
        begin
          Project := aProject;
          ProjectLoaded;
          RefreshProject;//zaher: maybe moved to SetProject
        end;
      except
        on E: Exception do
        begin
          Project.Log.Add('Error: ' + E.Message);
          ClearAll;
          raise;
        end;
      end;
    end;
  end;
end;

function TMainForm.AskCloseProject: Boolean;
var
  r: Integer;
begin
  Result := True;
  if (Project <> nil) then
  begin
    if (Project.Dictionary.Local <> nil) and Project.Dictionary.Local.Modified then
    begin
      r := MessageDlg('Save', 'Files was changed, save before exit?', mtConfirmation, mbYesNoCancel, '');
      case r of
        mrYes:
          begin
            SaveProject(Project.FileName, False);
          end;
        mrCancel:
          Result := False;
      end;
    end;
    if Result then
      CloseProject;
  end;
end;

procedure TMainForm.OptionsMnuClick(Sender: TObject);
begin
end;

procedure TMainForm.EnumRecentProjects;
var
  i: Integer;
  aStrings: TStringList;
  aMenuItem: TMenuItem;
begin
  aStrings := TStringList.Create;
  try
    if FileExists(trsEngine.WorkPath + 'recents_projects.cfg') then
    aStrings.LoadFromFile(trsEngine.WorkPath + 'recents_projects.cfg');
    ReopenProjectMnu.Clear;
    for i := 0 to aStrings.Count - 1 do
    begin
      aMenuItem := TMenuItem.Create(Self);
      aMenuItem.Caption := aStrings[i];
      aMenuItem.OnClick := @ReopenProjectClick;
      ReopenProjectMnu.Add(aMenuItem);
    end;
  finally
    aStrings.Free;
  end;
end;

procedure TMainForm.ReopenProjectClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    LoadProject(aFile, False);
  end;
end;

procedure TMainForm.ReopenFileClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    LoadFile(aFile);
  end;
end;

procedure TMainForm.LoadProject(FileName: string; Upgrade: Boolean);
begin
  if AskCloseProject then
  begin
    Project := TtrsProject.Create;
    try
      Project.LoadFromFile(FileName);
      Project.FileName := FileName;
      try
{        if Upgrade or (ftAutoUpgrade in Project.TranslatorClass.GetFlags) then
          Project.UpgradeLanguage
        else}
          Project.LoadLanguage;
      except
        on E: Exception do
        begin
          Project.Log.Add('Fail to load languages: ' + E.Message);
          Project.HaveWarring := True;
        end;
      end;
      ProjectLoaded;
      RefreshProject;
      ProcessRecentProjects(FileName);
      if Project.HaveWarring then
        ShowLogForm(Project.Log);
    except
      CloseProject;
      raise;
    end;
  end;
end;

procedure TMainForm.SaveProject(FileName: string; Force: Boolean);
var
  i:Integer;
begin
  Project.FileName := FileName;
  Project.SaveToFile(FileName);
  Project.SaveLanguage(Force);
  for i := 0 to Project.ToolsList.Count-1 do
  begin
    if (Project.ToolsList[i] is TMenuItem) then
      if (Project.ToolsList[i] as TMenuItem).Tag = 1 then
        (Project.ToolsList[i] as TMenuItem).Click;
  end;
  ProcessRecentProjects(FileName);
  UpdateSetting;
end;

procedure TMainForm.LoadFile(FileName: string);
var
  e: string;
begin
  if AskCloseProject then
  begin
    e := ExtractFileName(FileName);
    Project := CreateDefaultProject(e);
    Project.LoadDictionary(FileName, Project.Dictionary.Local);
    ProcessRecentFiles(FileName);
    ProjectLoaded;
    RefreshProject;
  end;
end;

procedure TMainForm.SaveFile(FileName: string);
var
  e: string;
begin
  if (Project.Dictionary <> nil) and (Project.Dictionary.Local <> nil) then
  begin
    if FileName = '' then
      FileName := Project.Dictionary.Local.Source;
    Project.SaveDictionary(FileName, Project.Dictionary.Local, False);
    ProcessRecentFiles(FileName);
    ProjectLoaded;
    RefreshProject;
  end;
end;

procedure TMainForm.ProcessRecentProjects(FileName: string);
var
  i: Integer;
  aStrings: TStringList;
begin
  aStrings := TStringList.Create;
  try
    if FileExists(trsEngine.WorkPath + 'recents_projects.cfg') then
      aStrings.LoadFromFile(trsEngine.WorkPath + 'recents_projects.cfg');
    i := aStrings.IndexOf(FileName);
    if i >= 0 then
      aStrings.Move(i, 0)
    else
      aStrings.Insert(0, FileName);
    while aStrings.Count > 10 do
      aStrings.Delete(10);
    aStrings.SaveToFile(trsEngine.WorkPath + 'recents_projects.cfg');
  finally
    aStrings.Free;
  end;
  EnumRecentProjects;
end;

procedure TMainForm.ProcessRecentFiles(FileName: string);
var
  i: Integer;
  aStrings: TStringList;
begin
  aStrings := TStringList.Create;
  try
    if FileExists(trsEngine.WorkPath + 'recents_files.cfg') then
      aStrings.LoadFromFile(trsEngine.WorkPath + 'recents_files.cfg');
    i := aStrings.IndexOf(FileName);
    if i >= 0 then
      aStrings.Move(i, 0)
    else
      aStrings.Insert(0, FileName);
    while aStrings.Count > 10 do
      aStrings.Delete(10);
    aStrings.SaveToFile(trsEngine.WorkPath + 'recents_files.cfg');
  finally
    aStrings.Free;
  end;
  EnumRecentFiles;
end;

procedure TMainForm.EnumRecentFiles;
var
  i: Integer;
  aStrings: TStringList;
  aMenuItem: TMenuItem;
begin
  aStrings := TStringList.Create;
  try
    if FileExists(trsEngine.WorkPath + 'recents_files.cfg') then
    aStrings.LoadFromFile(trsEngine.WorkPath + 'recents_files.cfg');
    //ReopenFilesMnu.BeginUpdate;
    ReopenFilesMnu.Clear;
    for i := 0 to aStrings.Count - 1 do
    begin
      aMenuItem := TMenuItem.Create(Self);
      aMenuItem.Caption := aStrings[i];
      aMenuItem.OnClick := @ReopenFileClick;
      ReopenFilesMnu.Add(aMenuItem);
    end;
  finally
    aStrings.Free;
  end;
end;

procedure TMainForm.RefreshProject;
begin
  Enumerate;
  UpdateSetting;
end;

procedure TMainForm.SaveProjectMnuClick(Sender: TObject);
begin
  if Project.FileName <> '' then
  begin
    SaveProject(Project.FileName, False);
  end
  else
    SaveAsProject;
end;

procedure TMainForm.SaveAsProject;
begin
  SaveDialog.FileName := Project.Name;
  if SaveDialog.Execute then
  begin
    SaveProject(SaveDialog.FileName, False);
    Project.FileName := SaveDialog.FileName;
  end;
end;

procedure TMainForm.OpenProjectMnuClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := 'translator';
  OpenDialog.Filter := 'miniTranslator|*.translator|All Files|*.*';
  OpenDialog.FileName := '*.translator';
  if OpenDialog.Execute then
  begin
    LoadProject(OpenDialog.FileName, False);
  end;
end;

procedure TMainForm.Close1Click(Sender: TObject);
begin
  AskCloseProject;
end;

procedure TMainForm.UpgradeMnuClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    LoadProject(OpenDialog.FileName, True);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  aReg: TRegistry;
begin
  HintLbl.Caption := '';
  if trsEngine.Options.FormWidth > 100 then
    Width := trsEngine.Options.FormWidth;
  if trsEngine.Options.FormHeight > 100 then
    Height := trsEngine.Options.FormHeight;
  TopLayoutMnu.Checked := trsEngine.Options.TopLayout;
  UpdateFonts;
  UpdateView;
  EnumRecentProjects;
  EnumRecentFiles;
  EnumExports;
end;

procedure TMainForm.ClearAll;
begin
  AllIdentList.Clear;
  SectionList.Clear;
  IdentList.Clear;
  OriginalValueEdit.Clear;
  LocalValueEdit.Clear;
  KeyLbl.Clear;
end;

procedure TMainForm.LocalValueEditChange(Sender: TObject);
begin
  if (Project <> nil) and (Project.Current <> nil) then
    Project.Current.DisplayText := LocalValueEdit.Text;
end;

procedure TMainForm.CloseProject;
begin
  FreeAndNil(FProject);
  ClearAll;
end;

procedure TMainForm.Logreport1Click(Sender: TObject);
begin
  if Project <> nil then
    ShowLogForm(Project.Log);
end;

procedure TMainForm.EnumAll;
var
  i, k: Integer;
  aContents: TLangContents;
  Filtered:Boolean;
begin
  Filtered := FilterNotTranslatedMnu.Checked;
  AllIdentList.Items.BeginUpdate;
  try
    AllIdentList.Clear;
    for i := 0 to Project.Dictionary.Local.Count - 1 do
    begin
      aContents := Project.Dictionary.Local[i];
      if aContents.Visible then
      begin
        for k := 0 to aContents.Count - 1 do
        begin
          if aContents.Items[k].Visible and (not Filtered or (aContents.Items[k].Text = '')) then
            AllIdentList.Items.AddObject(aContents.Items[k].ID, aContents.Items[k]);
        end;
      end;
    end;
  finally
    AllIdentList.Items.EndUpdate;
  end;
end;

procedure TMainForm.LoadValueFromAllList;
begin
  if (AllIdentList.ItemIndex >= 0) then
    LoadValueFromWord(TLangItem(AllIdentList.Items.Objects[AllIdentList.ItemIndex]))
  else
    LoadValueFromWord(nil);
  UpdateIdentList;
end;

procedure TMainForm.AllIdentListClick(Sender: TObject);
begin
  LoadValueFromAllList;
end;

procedure TMainForm.UpdateAllIdentList;
begin
  AllIdentList.ItemIndex := AllIdentList.Items.IndexOfObject(Project.Current);
end;

procedure TMainForm.UpdateIdentList;
var
  aContents: TLangContents;
  i: Integer;
begin
  i := SectionList.Items.IndexOfObject(Project.Current.Contents);
  if SectionList.ItemIndex >= 0 then
    aContents := TLangContents(SectionList.Items.Objects[SectionList.ItemIndex])
  else
    aContents := nil;
  SectionList.ItemIndex := i;
  if SectionList.ItemIndex >= 0 then
  begin
    if aContents <> TLangContents(SectionList.Items.Objects[SectionList.ItemIndex]) then
      EnumIdents(True);
    IdentList.ItemIndex := IdentList.Items.IndexOfObject(Project.Current);
  end;
end;

procedure TMainForm.LoadValueFromWord(Word: TLangItem);
begin
  if Word <> nil then
  begin
    Project.Current := nil; //locking ValueEdit.Changed
    LocalValueEdit.Text := Word.DisplayText;
    if Project.Dictionary.Original <> nil then
      OriginalValueEdit.Text := Project.Dictionary.Original.Values[Word.Contents.Name].FindID(Word.ID).DisplayText//error
    else
      OriginalValueEdit.Text := Word.DisplayID;
    KeyLbl.Text := Word.Contents.Name + '\' + Word.ID;
    CommentEdit.Text := Word.Comment;
    Project.Current := Word;
  end
  else
  begin
    Project.Current := nil;
    LocalValueEdit.Text := '';
    OriginalValueEdit.Text := '';
  end;
end;

procedure TMainForm.PriorMnuClick(Sender: TObject);
begin
  if AllIdentList.ItemIndex > 0 then
  begin
    AllIdentList.ItemIndex := AllIdentList.ItemIndex - 1;
    LoadValueFromAllList;
  end;
end;

procedure TMainForm.EnumExports;
var
  i: Integer;
  aMenuItem: TMenuItem;
begin
  ExportMnu.Clear;
{  for i := 0 to Translators.Count - 1 do
    if (tfGenerateContent in Translators[i].GetFlags) then
    begin
      aMenuItem := TMenuItem.Create(Self);
      aMenuItem.Caption := Translators[i].GetTitle;
      aMenuItem.OnClick := ExportClick;
      aMenuItem.Tag := i;
      ExportMnu.Add(aMenuItem);
    end;}
end;

procedure TMainForm.ExportClick(Sender: TObject);
var
  i: Integer;
begin
  if Sender is TMenuItem then
  begin
    i := (Sender as TMenuItem).Tag;
    //Project.ExportLanguage(Translators[i]);
  end;
end;

procedure TMainForm.FontMnuClick(Sender: TObject);
begin
  FontDialog.Font := trsEngine.Options.Font;
  if FontDialog.Execute then
  begin
    trsEngine.Options.Font.Assign(FontDialog.Font);
    trsEngine.SaveOptions;
    UpdateFonts;
  end;
end;

procedure TMainForm.FindKindCboClick(Sender: TObject);
begin
  if FindKindCbo.ItemIndex = 0 then
    FindinLocalMnu.Checked := True
  else
    FindinOriginalMnu.Checked := True;
end;

procedure TMainForm.FindEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    FindByEdit;
end;

procedure TMainForm.FindByEdit;
begin
  Project.FindWordStr := FindEdit.Text;
  FindNext;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FindByEdit;
end;

procedure TMainForm.UpdateView;
begin
  if TopLayoutMnu.Checked then
  begin
    UpSplitter.Visible := True;
    TopLayoutPanel.Visible := True;
    LeftSplitter.Visible := False;
    LeftLayoutPanel.Visible := False;
  end
  else
  begin
    UpSplitter.Visible := False;
    TopLayoutPanel.Visible := False;
    LeftSplitter.Visible := True;
    LeftLayoutPanel.Visible := True;
  end
end;

procedure TMainForm.JumpTo(Word: TLangItem);
begin
  Project.Current := nil;
  Word := Project.Dictionary.Local.FindID(Word.Contents.Name, Word.ID);
  if Word <> nil then
  begin
    Project.Current := Word;
    UpdateIdentList;
    UpdateAllIdentList;
    LoadValueFromList;
  end;
end;

procedure TMainForm.FindInLocalMnuClick(Sender: TObject);
begin
  FindKindCbo.ItemIndex := 0;
end;

procedure TMainForm.FindinOriginalMnuClick(Sender: TObject);
begin
  FindKindCbo.ItemIndex := 1;
end;

procedure TMainForm.Removedublicated1Click(Sender: TObject);
var
  aLog: TStringList;
begin
  if Project.Dictionary.Original <> nil then
    raise Exception.Create('You must open orginal language as single file');
  aLog := Project.Dictionary.Local.RemoveDublicated(False);
  if aLog.Count > 0 then
    ShowLogForm(aLog)
  else
    ShowMessage('There is no any dublicated!');
  aLog.Free;
end;

procedure TMainForm.Removeduplicated1Click(Sender: TObject);
var
  aLog: TStringList;
  aDictionary: TtrsLanguage;
begin
  if Project.Dictionary.Original <> nil then
  begin
    aDictionary := Project.Dictionary.Original;
    ShowMessage('You must open orginal language as single file');
  end
  else
    aDictionary := Project.Dictionary.Local;
  aLog := aDictionary.RemoveDublicated(True);
  if aLog.Count > 0 then
  begin
    ShowMessage('There is a dublicated: ' + IntToStr(aLog.Count));
    ShowLogForm(aLog)
  end
  else
    ShowMessage('There is no any dublicated!');
  aLog.Free;
end;

procedure TMainForm.Progressreport1Click(Sender: TObject);
var
  Log: TStringList;
begin
  Log := Project.Dictionary.Local.ProgressReport;
  try
    ShowLogForm(Log);
  finally
    Log.Free;
  end;
end;

procedure TMainForm.EnumTools(vCaption: string; vOnClick: TNotifyEvent; vAutoInSave:Boolean);
var
  aMenuItem: TMenuItem;
begin
  aMenuItem := TMenuItem.Create(Self);
  aMenuItem.Caption := vCaption;
  aMenuItem.OnClick := vOnClick;
  aMenuItem.Tag := Ord(vAutoInSave);
  ToolsMnu.Add(aMenuItem);
  Project.ToolsList.Add(aMenuItem);
end;

procedure TMainForm.Loaded;
begin
  inherited Loaded;
end;

procedure TMainForm.ResetFind;
begin
  FindIndex := 0;
end;

procedure TMainForm.ProjectLoaded;
begin
  Project.ToolsList.Clear;
  //Project.Dictionary.Local.EnumTools(EnumTools);
end;

procedure TMainForm.Retranslaterepeated1Click(Sender: TObject);
var
  aLog:TStringList;
begin
  if (Project <> nil) then
  begin
    aLog := Project.Dictionary.Original.Retranslate(Project.Dictionary.Local, False);
    ShowLogForm(aLog);
    aLog.Free;
  end;
end;

procedure TMainForm.Reportretranslate1Click(Sender: TObject);
var
  aLog:TStringList;
begin
  if (Project <> nil) then
  begin
    aLog := Project.Dictionary.Original.Retranslate(Project.Dictionary.Local, True);
    ShowLogForm(aLog);
    aLog.Free;
  end;
end;

procedure TMainForm.FilterNotTranslatedMnuClick(Sender: TObject);
begin
  Enumerate;
end;

procedure TMainForm.WorkPathMnuClick(Sender: TObject);
begin
  ShowSetup;
end;

procedure TMainForm.UpdateFonts;
begin
  OriginalValueEdit.Font.Assign(trsEngine.Options.Font);
  LocalValueEdit.Font.Assign(trsEngine.Options.Font);
end;

function TMainForm.GetProject: TtrsProject;
begin
  if FProject= nil then
    FProject := CreateDefaultProject('');
  Result := FProject;
end;

procedure TMainForm.SetProject(const AValue: TtrsProject);
begin
  if FProject <> AValue then
  begin
    FreeAndNil(FProject);
    FProject := AValue;
  end;
end;

function TMainForm.CreateDefaultProject(vExtension: string): TtrsProject;
var
  aFilerClass: TLangFilerClass;
begin
  Result := TtrsProject.Create;
  aFilerClass := LangOptions.FindFilerByExt(vExtension);
  if aFilerClass = nil then
    aFilerClass := TPOFileFiler;
  Result.FilerClass := aFilerClass;
  Result.Internal := True;
end;

initialization
  {$i Main.lrs}
end.

