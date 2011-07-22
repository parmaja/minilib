unit mneProjectOptions;
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
  EditorEngine, mneClasses, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    Label7: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    OpenDialog: TOpenDialog;
    NameEdit: TEdit;
    Label3: TLabel;
    DescriptionEdit: TEdit;
    Label4: TLabel;
    PerspectiveCbo: TComboBox;
    SaveDesktopChk: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    RootDirEdit: TEdit;
    Button3: TButton;
    RootUrlEdit: TEdit;
    RunModeCbo: TComboBox;
    Bevel1: TBevel;
    procedure OkBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProject: TEditorProject;
  protected
  public
    procedure Retrive;
    procedure Apply;
  end;

function ShowProjectForm(vProject: TEditorProject): Boolean;

implementation

uses mneResources;

{$R *.lfm}

function ShowProjectForm(vProject: TEditorProject): Boolean;
begin
  with TProjectForm.Create(Application) do
  begin
    FProject := vProject;
    Retrive;
    Result := ShowModal = mrOk;
  end;
end;

procedure TProjectForm.OkBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TProjectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TProjectForm.Apply;
begin
  FProject.Name := NameEdit.Text;
  FProject.Description := DescriptionEdit.Text;
  FProject.RootDir := RootDirEdit.Text;
  FProject.RootUrl := RootUrlEdit.Text;
  FProject.RunMode := TRunMode(RunModeCbo.ItemIndex);
  FProject.SaveDesktop := SaveDesktopChk.Checked;
  FProject.PerspectiveName := Engine.Perspectives[PerspectiveCbo.ItemIndex].Name;
end;

procedure TProjectForm.Retrive;
begin
  NameEdit.Text := FProject.Name;
  DescriptionEdit.Text := FProject.Description;
  RootDirEdit.Text := FProject.RootDir;
  RootUrlEdit.Text := FProject.RootUrl;
  RunModeCbo.ItemIndex := Ord(FProject.RunMode);
  SaveDesktopChk.Checked := FProject.SaveDesktop;
  PerspectiveCbo.ItemIndex := Engine.Perspectives.IndexOf(FProject.PerspectiveName);
end;

procedure TProjectForm.Button2Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
//  OpenDialog.FileName := ProgramEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
//    ProgramEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TProjectForm.Button3Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := RootDirEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.Name);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    RootDirEdit.Text := aFolder;
  end;
end;

procedure TProjectForm.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  RunModeCbo.Items.Add('None');
  RunModeCbo.Items.Add('Console');
  RunModeCbo.Items.Add('HTTP URL');
  PerspectiveCbo.Items.BeginUpdate;
  try
    for i := 0 to Engine.Perspectives.Count -1 do
    begin
      PerspectiveCbo.Items.Add(Engine.Perspectives[i].Title);
    end;
  finally
    PerspectiveCbo.Items.EndUpdate;
  end;
end;

end.

