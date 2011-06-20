unit mneProjectForms;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, Dialogs, ComCtrls, StdCtrls, mneClasses;

type
  TManageProjectsForm = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ProjectsList: TListBox;
    OpenBtn: TButton;
    CloseBtn: TButton;
    RProjectsList: TListBox;
    RFilesList: TListBox;
    PopupMnu: TPopupMenu;
    AddtoProjects1: TMenuItem;
    Button1: TButton;
    Button3: TButton;
    MoveDownBtn: TButton;
    MoveUpBtn: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure ProjectsListDblClick(Sender: TObject);
    procedure AddtoProjects1Click(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
  private
    procedure EnumRecentFile;
    procedure EnumRecentProjects;
    procedure EnumProjects;
    procedure NewProject;
    procedure RemoveNow;
    procedure OpenNow;
    procedure ChangeToIndex(ListBox: TlistBox; Index: integer);
  public
    { Public declarations }
  end;

implementation

uses EditorEngine, mneProjectOptions;

{$R *.lfm}

{ TForm1 }

procedure TManageProjectsForm.EnumRecentFile;
var
  i: integer;
begin
  RFilesList.Clear;
  for i := 0 to Engine.Options.RecentFiles.Count - 1 do
  begin
    RFilesList.Items.Add(Engine.Options.RecentFiles[i]);
  end;
end;

procedure TManageProjectsForm.EnumRecentProjects;
var
  i: integer;
begin
  RProjectsList.Clear;
  for i := 0 to Engine.Options.RecentProjects.Count - 1 do
  begin
    RProjectsList.Items.Add(Engine.Options.RecentProjects[i]);
  end;
end;

procedure TManageProjectsForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex := 0;
  EnumRecentFile;
  EnumRecentProjects;
  EnumProjects;
  if ProjectsList.Items.Count > 0 then
    ProjectsList.ItemIndex := 0;
  if RProjectsList.Items.Count > 0 then
    RProjectsList.ItemIndex := 0;
  if RFilesList.Items.Count > 0 then
    RFilesList.ItemIndex := 0;
end;

procedure TManageProjectsForm.NewProject;
var
  aProject: TEditorProject;
begin
  aProject := Engine.Session.New;
  try
    if ShowProjectForm(aProject) then
    begin
      if aProject.Save then
      begin
        Engine.ProcessProject(aProject.FileName);
        Engine.ProcessRecentProject(aProject.FileName);
        EnumProjects;
        EnumRecentProjects;
      end;
    end;
  finally
    aProject.Free;
  end;
end;

procedure TManageProjectsForm.Button3Click(Sender: TObject);
begin
  NewProject;
end;

procedure TManageProjectsForm.EnumProjects;
var
  i: integer;
begin
  ProjectsList.Clear;
  for i := 0 to Engine.Options.Projects.Count - 1 do
  begin
    ProjectsList.Items.Add(Engine.Options.Projects[i]);
  end;
end;

procedure TManageProjectsForm.Button1Click(Sender: TObject);
var
  aOpenDialog: TOpenDialog;
begin
  aOpenDialog := TOpenDialog.Create(nil);
  try
    aOpenDialog.Title := 'Open project';
    aOpenDialog.DefaultExt := Engine.Extenstion;
    aOpenDialog.Filter := 'Project files|*.' + Engine.Extenstion + '|All files|*.*';
    aOpenDialog.FileName := '*.' + aOpenDialog.DefaultExt;
    if aOpenDialog.Execute then
    begin
      Engine.ProcessProject(aOpenDialog.FileName);
      Engine.ProcessRecentProject(aOpenDialog.FileName);
      EnumProjects;
      EnumRecentProjects;
    end;
  finally
    aOpenDialog.Free;
  end;
end;

procedure TManageProjectsForm.Button2Click(Sender: TObject);
begin
  RemoveNow;
end;

procedure TManageProjectsForm.RemoveNow;
var
  Old: integer;
begin
  case PageControl.TabIndex of
    0:
      if ProjectsList.ItemIndex >= 0 then
      begin
        Old := ProjectsList.ItemIndex;
        Engine.RemoveProject(ProjectsList.Items[Old]);
        EnumProjects;
        ChangeToIndex(ProjectsList, Old);
      end;
    1:
      if RProjectsList.ItemIndex >= 0 then
      begin
        Old := RProjectsList.ItemIndex;
        Engine.RemoveRecentProject(RProjectsList.Items[RProjectsList.ItemIndex]);
        EnumRecentProjects;
        ChangeToIndex(RProjectsList, Old);
      end;
    2:
      if RFilesList.ItemIndex >= 0 then
      begin
        Old := RFilesList.ItemIndex;
        Engine.RemoveRecentFile(RFilesList.Items[RFilesList.ItemIndex]);
        EnumRecentFile;
        ChangeToIndex(RFilesList, Old);
      end;
  end;
end;

procedure TManageProjectsForm.OpenNow;
begin
  case PageControl.TabIndex of
    0:
      if ProjectsList.ItemIndex >= 0 then
      begin
        Engine.Session.Load(ProjectsList.Items[ProjectsList.ItemIndex]);
        Close;
      end;
    1:
      if RProjectsList.ItemIndex >= 0 then
      begin
        Engine.Session.Load(RProjectsList.Items[RProjectsList.ItemIndex]);
        Close;
      end;
    2:
      if RFilesList.ItemIndex >= 0 then
      begin
        Engine.Files.OpenFile(RFilesList.Items[RFilesList.ItemIndex]);
        Close;
      end;
  end;
end;

procedure TManageProjectsForm.OpenBtnClick(Sender: TObject);
begin
  OpenNow;
end;

procedure TManageProjectsForm.ProjectsListDblClick(Sender: TObject);
begin
  OpenNow;
end;

procedure TManageProjectsForm.AddtoProjects1Click(Sender: TObject);
var
  aProject: string;
  POld, ROld: integer;
begin
  if RProjectsList.ItemIndex >= 0 then
  begin
    ROld := RProjectsList.ItemIndex;
    POld := ProjectsList.ItemIndex;
    aProject := RProjectsList.Items[RProjectsList.ItemIndex];
    Engine.ProcessProject(aProject);
    Engine.ProcessRecentProject(aProject);
    EnumProjects;
    EnumRecentProjects;
    ChangeToIndex(RProjectsList, ROld);
    ChangeToIndex(ProjectsList, POld);
  end;
end;

procedure TManageProjectsForm.MoveUpBtnClick(Sender: TObject);
var
  i: integer;
begin
  if ProjectsList.ItemIndex > 0 then
  begin
    i := ProjectsList.ItemIndex;
    Engine.Options.Projects.Exchange(i, i - 1);
    EnumProjects;
    ProjectsList.ItemIndex := i - 1;
  end;
end;

procedure TManageProjectsForm.MoveDownBtnClick(Sender: TObject);
var
  i: integer;
begin
  if (ProjectsList.ItemIndex >= 0) and (ProjectsList.ItemIndex < (ProjectsList.Count - 1)) then
  begin
    i := ProjectsList.ItemIndex;
    Engine.Options.Projects.Exchange(i, i + 1);
    EnumProjects;
    ProjectsList.ItemIndex := i + 1;
  end;
end;

procedure TManageProjectsForm.ChangeToIndex(ListBox: TlistBox; Index: integer);
begin
  if ListBox.Items.Count = 0 then
    Index := -1
  else if Index >= ListBox.Items.Count then
    Index := ListBox.Items.Count - 1;
  ListBox.ItemIndex := Index;
end;

end.

