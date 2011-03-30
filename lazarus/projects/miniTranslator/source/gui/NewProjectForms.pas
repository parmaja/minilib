unit NewProjectForms;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  LangClasses,
  Dialogs, StdCtrls, ExtCtrls, LResources, trsProjects;

type

  { TNewProjectForm }

  TNewProjectForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    FilerCbo: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    NameEdit: TEdit;
    Label1: TLabel;
    NotesEdit: TMemo;
    Label2: TLabel;
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowNewProject: TtrsProject;

implementation

{uses mnXMLRttiProfile, TranslatorClasses;}


function ShowNewProject: TtrsProject;
begin
  with TNewProjectForm.Create(Application) do
  begin
    if ShowModal = mrOk then
    begin
      Result := TtrsProject.Create;
      Result.FilerClass := TLangFilerClass(FilerCbo.Items.Objects[FilerCbo.ItemIndex]);
      Result.Name := NameEdit.Text;
      Result.Notes := NotesEdit.Text;
    end
    else
      Result := nil;
  end;
end;

procedure TNewProjectForm.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  for i := 0 to LangOptions.FilerClasses.Count - 1 do
      FilerCbo.Items.AddObject(LangOptions.FilerClasses[i].GetTitle, TObject(LangOptions.FilerClasses[i]));
  FilerCbo.ItemIndex := 0;
end;

procedure TNewProjectForm.Button4Click(Sender: TObject);
begin

end;

initialization
  {$i NewProjectForms.lrs}
end.

