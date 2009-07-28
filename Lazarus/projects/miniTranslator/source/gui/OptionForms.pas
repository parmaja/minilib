unit OptionForms;
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
  Dialogs, StdCtrls, ExtCtrls, LResources, trsProjects;

type
  TOptionsForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    LocalNameLbl: TLabel;
    OriginalNameLbl: TLabel;
    Label6: TLabel;
    LocalNameEdit: TEdit;
    OriginalNameBtn: TButton;
    OriginalNameEdit: TEdit;
    LocalNameBtn: TButton;
    NameEdit: TEdit;
    OpenDialog: TOpenDialog;
    Label2: TLabel;
    ProjectTypeEdit: TEdit;
    NotesLbl: TLabel;
    NotesEdit: TMemo;
    procedure LocalNameBtnClick(Sender: TObject);
    procedure OriginalNameBtnClick(Sender: TObject);
  private
    FProject: TtrsProject;
    procedure SelectFiles(Edit: TEdit);
  public
  end;

function ShowOptions(Project: TtrsProject; AsNew:Boolean): Boolean;

implementation

uses
  FileCtrl{, TranslatorClasses};


function ShowOptions(Project: TtrsProject; AsNew:Boolean): Boolean;
var
  OldOriginal, OldLocal: string;
begin
  with TOptionsForm.Create(Application) do
  begin
    FProject := Project;
    ProjectTypeEdit.Text := FProject.FilerClass.GetTitle;
    NameEdit.Text := FProject.Name;
    NotesEdit.Text := FProject.Notes;
    OriginalNameEdit.Text := FProject.OriginalName;
    LocalNameEdit.Text := FProject.LocalName;
    OldOriginal := FProject.OriginalName;
    OldLocal := FProject.LocalName;
    if FProject.FilerClass.IsSingle then
    begin
      OriginalNameEdit.Visible := False;
      OriginalNameLbl.Visible := False;
      OriginalNameBtn.Visible := False;
      NotesLbl.Top := OriginalNameLbl.Top;
      NotesEdit.BoundsRect := Rect(NotesEdit.BoundsRect.Left, OriginalNameEdit.Top, NotesEdit.BoundsRect.Right, NotesEdit.BoundsRect.Bottom);
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      FProject.Name := NameEdit.Text;
      FProject.OriginalName := OriginalNameEdit.Text;
      FProject.LocalName := LocalNameEdit.Text;
      FProject.Notes := NotesEdit.Text;
      if AsNew or (((OldOriginal<>'') and (OldOriginal <> FProject.OriginalName)) or ((OldLocal<>'') and (OldLocal <> FProject.LocalName))) then
      begin
        if not AsNew and (MessageDlg('Save', 'The files was changed, save before change to new files?', mtConfirmation, mbYesNoCancel, '') = mrYes) then
          FProject.SaveLanguage(False);
        FreeAndNil(FProject.Dictionary.Local);
        FreeAndNil(FProject.Dictionary.Original);
        {if (ftAutoUpgrade in Project.TranslatorClass.GetFlags) then
          FProject.UpgradeLanguage
        else}
          FProject.LoadLanguage;
      end;
    end;
  end;
end;

procedure TOptionsForm.LocalNameBtnClick(Sender: TObject);
begin
  SelectFiles(LocalNameEdit);
end;

procedure TOptionsForm.OriginalNameBtnClick(Sender: TObject);
begin
  SelectFiles(OriginalNameEdit);
end;

procedure TOptionsForm.SelectFiles(Edit: TEdit);
var
  d: string;
begin
  if FProject <> nil then
  begin
    if FProject.FilerClass.IsMultiFiles then
    begin
      d := OriginalNameEdit.Text;
      if SelectDirectory('Select your lang folder', '', d) then
      begin
        Edit.Text := d;
      end;
    end
    else
    begin
      OpenDialog.Filter := FProject.FilerClass.GetName + ' (*' + FProject.FilerClass.GetExtensions + ')|*' + FProject.FilerClass.GetExtensions + '|All files|*.*';
      if OpenDialog.Execute then
      begin
        Edit.Text := OpenDialog.FileName;
      end;
    end;
  end;
end;

initialization
  {$i OptionForms.lrs}
end.

