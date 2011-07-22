unit mneAssociateForm;
{$mode objfpc}
{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IAddons;

type
  TAssociateForm = class(TForm)
    PHPChk: TCheckBox;
    CSSChk: TCheckBox;
    EditAssociateSupportedChk: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    procedure AssociateNow(Cmd, Ext, FileType, WithApplication, Description, Mime: string; WithDDE: Boolean);
    function GetAssociated(Cmd, FileType, Ext: string): Boolean;
  public
    procedure Apply;
    procedure Retrive;
  end;

implementation

{$R *.lfm}

uses
  {$ifdef Windows}Windows, Registry, ShlObj,{$endif} EditorEngine;

procedure TAssociateForm.Apply;
var
  AExtensions: TStringList;
  i: Integer;
begin
  if PHPChk.Checked then
  begin
    AssociateNow('Open', '.php', 'phpfile', Application.ExeName, 'PHP script file', 'text/plain', False);
    AssociateNow('Edit', '.phpx', 'phpxfile', Application.ExeName, 'PHPX script file', 'text/plain', False);
{      AssociateNow('Open', '.phpx', 'phpxfile', IncludeTrailingPathDelimiter(Engine.Options.CompilerFolder) + 'php.exe', 'PHP executable script file', 'text/plain', False);}
  end;

  AssociateNow('Open', '.mne-project', 'mne-project', Application.ExeName, 'Mini Edit project file', 'application/miniedit', True);

  if CSSChk.Checked then
    AssociateNow('Open', '.css', 'cssfile', Application.ExeName, 'CSS file', 'text/plain', True);

  if EditAssociateSupportedChk.Checked then
  begin
    AExtensions := TStringList.Create;
    try
      Engine.Groups.EnumExtensions(AExtensions);
      for i := 0 to AExtensions.Count - 1 do
        AssociateNow('Edit', '.' + AExtensions[i], AExtensions[i] + 'file', Application.ExeName, AExtensions[i] + ' files', 'text/plain', True);
    finally
      AExtensions.Free;
    end;
  end;

  {$ifdef Windows}
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
//  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, MsgResult);
  {$endif}
end;

procedure TAssociateForm.AssociateNow(Cmd, Ext, FileType, WithApplication, Description, Mime: string; WithDDE: Boolean);
{$ifdef Windows}
var
  aReg: TRegistry;
begin
  aReg := TRegistry.Create;
  try
    aReg.RootKey := HKEY_CLASSES_ROOT;
    aReg.OpenKey(Ext, True);
    aReg.WriteString('', FileType);
    aReg.WriteString('Content Type', Mime);
    aReg.CloseKey;
    aReg.OpenKey(FileType, True);
    aReg.WriteString('', Description);
    aReg.CloseKey;
    aReg.OpenKey(FileType + '\DefaultIcon', True);
    aReg.WriteString('', Application.ExeName + ',0');
    aReg.CloseKey;
    aReg.OpenKey(FileType + '\Shell\', True);
    aReg.WriteString('', 'Open');
    aReg.CloseKey;
    aReg.OpenKey(FileType + '\Shell\' + Cmd + '\Command', True);
    if WithDDE then
      aReg.WriteString('', '"' + WithApplication + '" /DDE "%1"')
    else
      aReg.WriteString('', '"' + WithApplication + '" "%1"');
    aReg.CloseKey;
    if WithDDE then
    begin
      aReg.OpenKey(FileType + '\Shell\' + Cmd + '\ddeexec', True);
      aReg.WriteString('', '[Open "%1"' + '' + ']');
      aReg.CloseKey;
      aReg.OpenKey(FileType + '\Shell\' + Cmd + '\ddeexec\Application', True);
      aReg.WriteString('', 'MiniEdit');
      aReg.CloseKey;
      aReg.OpenKey(FileType + '\Shell\' + Cmd + '\ddeexec\Topic', True);
      aReg.WriteString('', 'DDESystem');
      aReg.CloseKey;
    end;
  finally
    aReg.Free;
  end;
end;
{$else}
begin
end;
{$endif}

function TAssociateForm.GetAssociated(Cmd, FileType, Ext: string): Boolean;
{$ifdef Windows}
var
  aReg: TRegistry;
begin
  aReg := TRegistry.Create;
  try
    aReg.RootKey := HKEY_CLASSES_ROOT;
    aReg.OpenKey(Ext, True);
    Result := aReg.ReadString('') = FileType;
    aReg.CloseKey;
    aReg.OpenKey(FileType + '\Shell\' + Cmd + '\Command', True);
    Result := Result and (aReg.ReadString('') = '"' + Application.ExeName + '" "%1"');
    aReg.CloseKey;
  finally
    aReg.Free;
  end;
end;
{$else}
begin
end;
{$endif}

procedure TAssociateForm.Retrive;
begin
  PHPChk.Checked := GetAssociated('Open', 'phpfile', '.php');
  CSSChk.Checked := GetAssociated('Open', 'cssfile', '.css');
end;

procedure TAssociateForm.FormCreate(Sender: TObject);
begin
  Retrive;
end;

procedure TAssociateForm.OkBtnClick(Sender: TObject);
begin
  Apply;
end;

type
  TAssociateAddon = class(TAddon, IClickAddon, IMenuAddon)
  public
    procedure Click(Sender: TObject);
    function GetCaption: string;
  end;

  { TAssociateAddon }

  procedure TAssociateAddon.Click(Sender: TObject);
  begin
    with TAssociateForm.Create(Application) do
    begin
      ShowModal;// = mrOK;
    end;
  end;

  function TAssociateAddon.GetCaption: string;
  begin
    Result := 'Associate';
  end;

initialization
  Addons.Add('File', 'Associate', TAssociateAddon);
end.

