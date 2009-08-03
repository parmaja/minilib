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
  FileUtil,
  trsProjects, Dialogs, StdCtrls, LResources;

type

  { TSetupForm }

  TSetupForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    WorkPathEdit: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    OldWorkPath: string;
  public
  end;

function ShowSetup: Boolean;

implementation

function ShowSetup: Boolean;
begin
  with TSetupForm.Create(Application) do
  begin
    Result := ShowModal = mrOK;
    Free;
  end;
end;

procedure TSetupForm.Button1Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := WorkPathEdit.Text;
  if SelectDirectory('Select root directory for you project', '', aFolder) then
  begin
    WorkPathEdit.Text := aFolder;
  end;
end;

procedure TSetupForm.OkBtnClick(Sender: TObject);
var
  aStrings: TStringList;
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
    aStrings := TStringList.Create;
    try
      if FileExists(GetConfigFileName) then
        aStrings.LoadFromFile(GetConfigFileName);
      aStrings.Values['WorkPath'] := IncludeTrailingPathDelimiter(WorkPathEdit.Text);
      aStrings.SaveToFile(GetConfigFileName);
      if OldWorkPath = '' then
        trsEngine.WorkPath := IncludeTrailingPathDelimiter(WorkPathEdit.Text);
    finally
      aStrings.Free;
    end;
  end;
  ModalResult := mrOK;
end;

procedure TSetupForm.FormCreate(Sender: TObject);
var
  s: string;
begin
  OldWorkPath := trsEngine.WorkPath;
  WorkPathEdit.Text := OldWorkPath;
  s := GetEnvironmentVariableUTF8('WORKSPACE');
  if s <> '' then
    WorkPathEdit.Items.Add(s + sMiniTranslator);
  s := GetEnvironmentVariableUTF8('HOME');
  if s <> '' then
    WorkPathEdit.Items.Add(s + sMiniTranslator);
  WorkPathEdit.Items.Add(GetAppConfigDir(False)+sMiniTranslator);
  WorkPathEdit.Items.Add(GetAppConfigDir(True)+sMiniTranslator);
end;

initialization
  {$i Setups.lrs}
end.
