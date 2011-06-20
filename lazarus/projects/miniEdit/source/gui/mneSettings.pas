unit mneSettings;
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
  EditorEngine, Dialogs, StdCtrls, ComCtrls, Grids;

type

  { TEditorSettingForm }

  TEditorSettingForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    PHPFolderEdit: TEdit;
    Button3: TButton;
    Label3: TLabel;
    Label4: TLabel;
    CollectTimeoutEdit: TEdit;
    CollectTimeoutSpn: TUpDown;
    CollectAutoCompleteChk: TCheckBox;
    SendOutputToNewFileChk: TCheckBox;
    AutoStartDebugServerChk: TCheckBox;
    TabSheet3: TTabSheet;
    PHPManualEdit: TEdit;
    Label2: TLabel;
    Button1: TButton;
    HTMLManualEdit: TEdit;
    Label8: TLabel;
    Button5: TButton;
    TabSheet4: TTabSheet;
    Label9: TLabel;
    ExtensionsGrid: TStringGrid;
    Label10: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure INIEditChange(Sender: TObject);
  private
    FEngine: TEditorEngine;
    FExtraExtensions: array of string;
    procedure Retrive;
    procedure Apply;
  public
  end;


function ShowSettingForm(Engine: TEditorEngine): Boolean;

implementation

var
  CurrentPage: Integer;

{$R *.lfm}

function ShowSettingForm(Engine: TEditorEngine): Boolean;
begin
  with TEditorSettingForm.Create(Application) do
  begin
    FEngine := Engine;
    Retrive;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Apply;
      Engine.Options.Save;
    end;
  end;
end;

procedure TEditorSettingForm.Button3Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := IncludeTrailingPathDelimiter(PHPFolderEdit.Text);
  if SelectFolder('PHP path', '', aFolder) then
  begin
    PHPFolderEdit.Text := aFolder;
  end;
end;

procedure TEditorSettingForm.Button1Click(Sender: TObject);
begin
  OpenDialog.Title := 'select PHP Help file "php_manual_en.chm"';
  OpenDialog.Filter := 'Help files|*.chm|All files|*.*';
  OpenDialog.FileName := PHPManualEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(PHPManualEdit.Text);
  if OpenDialog.Execute then
  begin
    PHPManualEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TEditorSettingForm.Button4Click(Sender: TObject);
begin

end;

procedure TEditorSettingForm.Apply;
var
  i, c: Integer;
begin
  FEngine.Options.HelpFiles.Clear;
  FEngine.Options.HelpFiles.Values['php'] := PHPManualEdit.Text;
  FEngine.Options.HelpFiles.Values['html'] := HTMLManualEdit.Text;
  FEngine.Options.CompilerFolder := PHPFolderEdit.Text;
  FEngine.Options.CollectAutoComplete := CollectAutoCompleteChk.Checked;
  FEngine.Options.CollectTimeout := CollectTimeoutSpn.Position;
  FEngine.Options.SendOutputToNewFile := SendOutputToNewFileChk.Checked;
  FEngine.Options.AutoStartDebugServer := AutoStartDebugServerChk.Checked;
  c := 1;
  for i := 0 to FEngine.Groups.Count - 1 do
  begin
    if fgkPublish in FEngine.Groups[i].Kind then
    begin
      FEngine.Options.ExtraExtensions.Values[FExtraExtensions[c - 1]] := ExtensionsGrid.Cells[1, c];
      Inc(c);
    end;
  end;
end;

procedure TEditorSettingForm.Retrive;
var
  i, c: Integer;
begin
  PHPManualEdit.Text := FEngine.Options.HelpFiles.Values['php'];
  HTMLManualEdit.Text := FEngine.Options.HelpFiles.Values['html'];
  PHPFolderEdit.Text := FEngine.Options.CompilerFolder;
  CollectAutoCompleteChk.Checked := FEngine.Options.CollectAutoComplete;
  CollectTimeoutSpn.Position := FEngine.Options.CollectTimeout;
  SendOutputToNewFileChk.Checked := FEngine.Options.SendOutputToNewFile;
  AutoStartDebugServerChk.Checked := FEngine.Options.AutoStartDebugServer;
  ExtensionsGrid.Cells[0, 0] := 'Group';
  ExtensionsGrid.Cells[1, 0] := 'Extensions';
  c := 1;
  for i := 0 to FEngine.Groups.Count - 1 do
  begin
    if fgkPublish in FEngine.Groups[i].Kind then
    begin
      ExtensionsGrid.RowCount := c + 1;
      ExtensionsGrid.Cells[0, c] := FEngine.Groups[i].DisplayName;
      ExtensionsGrid.Cells[1, c] := FEngine.Options.ExtraExtensions.Values[FEngine.Groups[i].Name];
      SetLength(FExtraExtensions, c);
      FExtraExtensions[c - 1] := FEngine.Groups[i].Name;
      Inc(c);
    end;
  end;
end;

procedure TEditorSettingForm.Button5Click(Sender: TObject);
begin
  OpenDialog.Title := 'select HTML Help file';
  OpenDialog.Filter := 'Help files|*.chm|All files|*.*';
  OpenDialog.FileName := HTMLManualEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(HTMLManualEdit.Text);
  if OpenDialog.Execute then
  begin
    HTMLManualEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TEditorSettingForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := CurrentPage;
end;

procedure TEditorSettingForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CurrentPage := PageControl.ActivePageIndex;
end;

procedure TEditorSettingForm.INIEditChange(Sender: TObject);
begin

end;

end.

