unit mnePHPIniForm;
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
  EditorEngine, Dialogs, StdCtrls, ComCtrls, IniFiles, mnUtils, PHPUtils, IAddons;

type

  { TPHPIniForm }

  TPHPIniForm = class(TForm)
    PageControl: TPageControl;
    PHPIniFileEdit: TEdit;
    Button3: TButton;
    Label1: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    OkBtn: TButton;
    CancelBtn: TButton;
    InstallXDebugChk: TCheckBox;
    AutoStartChk: TCheckBox;
    EnablelXDebugChk: TCheckBox;
    DefaultEnableChk: TCheckBox;
    Label2: TLabel;
    XDebugExtensionPathEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    ExtensionDirEdit: TEdit;
    RegisterGlobalsChk: TCheckBox;
    ErrorReportingAllChk: TCheckBox;
    Label5: TLabel;
    MaxExecutionTimeEdit: TEdit;
    DisplayErrorsChk: TCheckBox;
    DisplayStartupErrorsChk: TCheckBox;
    OpenDialog: TOpenDialog;
    Edit1: TEdit;
    Label6: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Retrive;
    procedure Apply;
  end;

procedure ShowPHPIniForm;

implementation

{$R *.lfm}

uses
  mnXMLUtils;

procedure ShowPHPIniForm;
begin
  with TPHPIniForm.Create(Application) do
  begin
    //Retrive;
    if ShowModal = mrOK then
      Apply;
  end;
end;

{ TForm1 }

procedure TPHPIniForm.Apply;
var
  IniFile: TPHPIniFile;
  s: string;
  function GetOnOff(b:Boolean):string;
  begin
    if b then
      Result := 'On'
    else
      Result := 'Off';
  end;
begin
  IniFile := TPHPIniFile.Create(PHPIniFileEdit.Text);
  try
    IniFile.WriteString('PHP', 'register_globals', GetOnOff(RegisterGlobalsChk.Checked));
    if ErrorReportingAllChk.Checked then
      IniFile.WriteString('PHP', 'error_reporting', 'E_ALL')
    else
      IniFile.WriteString('PHP', 'error_reporting', 'E_ALL & ~E_NOTICE');
    IniFile.WriteString('PHP', 'display_errors', GetOnOff(DisplayErrorsChk.Checked));
    IniFile.WriteString('PHP', 'display_startup_errors', GetOnOff(DisplayStartupErrorsChk.Checked));
    IniFile.WriteString('PHP', 'max_execution_time', MaxExecutionTimeEdit.Text);
    IniFile.WriteString('PHP', 'extension_dir', QuoteStr(ExtensionDirEdit.Text));
    
    if InstallXDebugChk.Checked then
    begin
      s := XDebugExtensionPathEdit.Text;
      if s = '' then
      begin
        s := DequoteStr(IniFile.ReadString('PHP', 'extension_dir', ''));
        if s <> '' then
          s := IncludeTrailingPathDelimiter(s);
        s := s + 'php_xdebug.dll';
  //      s := ExpandToPath(s, Engine.Options.CompilerFolder);
        s := StringReplace(s, '\', '/', [rfReplaceAll]);
      end;
      IniFile.AppendString('PHP', 'zend_extension_ts', s);
      IniFile.WriteString('PHP', 'xdebug.remote_handler', 'dbgp');
      IniFile.WriteString('PHP', 'xdebug.remote_mode', 'req');
      IniFile.WriteString('PHP', 'xdebug.remote_port', '9000');
    end;
    IniFile.WriteString('PHP', 'xdebug.remote_enable', GetOnOff(EnablelXDebugChk.Checked));
    IniFile.WriteString('PHP', 'xdebug.remote_autostart', GetOnOff(AutoStartChk.Checked));
    IniFile.WriteString('PHP', 'xdebug.default_enable', GetOnOff(DefaultEnableChk.Checked));
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

procedure TPHPIniForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex := 0;
end;

procedure TPHPIniForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TPHPIniForm.Button3Click(Sender: TObject);
begin
  OpenDialog.Title := 'select PHP INI file';
  OpenDialog.Filter := 'INI files|*.ini|All files|*.*';
  OpenDialog.FileName := PHPIniFileEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(PHPIniFileEdit.Text);
  if OpenDialog.Execute then
  begin
    PHPIniFileEdit.Text := OpenDialog.FileName;
    Retrive;
  end;
end;

procedure TPHPIniForm.Retrive;
var
  IniFile: TPHPIniFile;
  s: string;
  function GetOnOff(b:string):Boolean;
  begin
    if SameText(b , 'On') then
      Result := True
    else if SameText(b , 'Off') then
      Result := False
    else if SameText(b , 'Yes') then
      Result := True
    else
      Result := False;
  end;
begin
  IniFile := TPHPIniFile.Create(PHPIniFileEdit.Text);
  try
    ErrorReportingAllChk.Checked := IniFile.ReadString('PHP', 'error_reporting', '') = 'E_ALL'; 
    RegisterGlobalsChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'register_globals', 'Off'));
    DisplayErrorsChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'display_errors', 'Off'));
    DisplayStartupErrorsChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'display_startup_errors', 'Off'));
    MaxExecutionTimeEdit.Text := IniFile.ReadString('PHP', 'max_execution_time', '30');
    ExtensionDirEdit.Text := DequoteStr(IniFile.ReadString('PHP', 'extension_dir', ''));

    s := IniFile.FindValue('PHP', 'zend_extension_ts', 'php_xdebug.dll');
    XDebugExtensionPathEdit.Text := s;
    InstallXDebugChk.Checked := s <> '';
    EnablelXDebugChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'xdebug.remote_enable', 'Off'));
    AutoStartChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'xdebug.remote_autostart', 'Off'));
    DefaultEnableChk.Checked := GetOnOff(IniFile.ReadString('PHP', 'xdebug.default_enable', 'Off'));
//    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

type
  TOpenPHPConfigAddon = class(TAddon, IClickAddon, IMenuAddon)
  public
    procedure Click(Sender: TObject);
    function GetCaption: string;
  end;

  { TAssociateAddon }

  procedure TOpenPHPConfigAddon.Click(Sender: TObject);
  begin
    ShowPHPIniForm;
  end;

  function TOpenPHPConfigAddon.GetCaption: string;
  begin
    Result := 'PHP Config';
  end;

initialization
  Addons.Add('PHP', 'OpenPHPConfigAddon', TOpenPHPConfigAddon);
end.