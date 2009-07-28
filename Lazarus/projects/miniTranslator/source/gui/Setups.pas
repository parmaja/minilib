unit Setups;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LCLIntf,
  SysUtils, Classes, Graphics, Controls, Forms,
  trsProjects, Dialogs, StdCtrls, LResources;

type
  TSetupForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    WorkPathEdit: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation


uses
  Registry;
  
procedure TSetupForm.Button1Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := WorkPathEdit.Text;
//  if SelectFolder('Select root directory for you project', '', aFolder) then
  begin
    WorkPathEdit.Text := aFolder;
  end;
end;

procedure TSetupForm.OkBtnClick(Sender: TObject);
var
  aReg: TRegistry;
begin
  if WorkPathEdit.Text = '' then
  begin
    MessageDlg('You must enter valid folder', mtWarning, [mbClose], 0);
    WorkPathEdit.SetFocus;
    Abort;
  end
  else
  begin
    ForceDirectories(WorkPathEdit.Text);
    aReg := TRegistry.Create;
    try
  //    aReg.RootKey := HKEY_LOCAL_MACHINE;
      if aReg.OpenKey(sSoftwareRegKey, True) then
        aReg.writeString('WorkPath', IncludeTrailingPathDelimiter(WorkPathEdit.Text));
    finally
      aReg.Free;
    end;
  end;
  ModalResult := mrOK;
end;

procedure TSetupForm.FormCreate(Sender: TObject);
var
  aReg: TRegistry;
begin
  aReg := TRegistry.Create(KEY_READ);
  try
//    aReg.RootKey := HKEY_LOCAL_MACHINE;
    if aReg.OpenKey(sSoftwareRegKey, False) and aReg.ValueExists('WorkPath') then
      WorkPathEdit.Text := aReg.ReadString('WorkPath');
  finally
    aReg.Free;
  end;
end;

initialization
  {$i Setups.lrs}
end.
