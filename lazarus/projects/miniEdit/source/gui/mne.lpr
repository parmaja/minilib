program mne;

{$mode objfpc}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @url       http://sourceforge.net/projects/minilib
 *}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Dialogs,
  Controls,
  Forms,
  Registry,
  SysUtils,
  simpleipc,
  Themes, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  mneProjectOptions in 'mneProjectOptions.pas' {ProjectForm},
  SearchForms in '..\editor\SearchForms.pas' {SearchForm},
  EditorOptions in '..\editor\EditorOptions.pas' {EditorOptionsForm},
  EditorProfiles in '..\editor\EditorProfiles.pas',
  mneSetups in 'mneSetups.pas' {EditorSetupForm},
  EditorEngine in '..\editor\EditorEngine.pas',
  mneResources in 'mneResources.pas' {EditorResource: TDataModule},
  SelectFiles in '..\editor\SelectFiles.pas' {SelectFileForm},
  SynHighlighterXHTML in '..\lib\SynHighlighterXHTML.pas',
  mneClasses in 'mneClasses.pas',
  mneConsts in 'mneConsts.pas',
  AboutForms in 'AboutForms.pas' {AboutForm},
  mneProjectForms in 'mneProjectForms.pas' {ManageProjectsForm},
  GotoForms in '..\editor\GotoForms.pas' {GotoLineForm},
  Match in '..\lib\Match.pas',
  HTMLProcessor in '..\lib\HTMLProcessor.pas',
  PHPProcessor in '..\lib\PHPProcessor.pas',
  mneRun in 'mneRun.pas',
  mnePHPIniForm in 'mnePHPIniForm.pas' {PHPIniForm},
  mneBreakpoints in 'mneBreakpoints.pas' {BreakpointsForm},
  SearchInFilesForms in '..\editor\SearchInFilesForms.pas' {SearchInFilesForm},
  SearchProgressForms in '..\editor\SearchProgressForms.pas',
  EditorDebugger, PHP_xDebug, SelectList,
  SynHighlighterApache in '..\lib\SynHighlighterApache.pas',
  IniFiles,
  mneAddons, mneAssociateForm,
  mnePHPClasses,
  mnePASClasses,
  MsgBox,
  GUIMsgBox,
  Classes,
  PHPUtils in '..\lib\PHPUtils.pas', mnePHPConfigForms;

{$R *.res}

function CheckSetup: Boolean;
var
  aIniFile: TIniFile;
  aIni, aPath: string;
begin
  Result := False;
  aIni := ExtractFilePath(Application.ExeName) + 'setting.ini';
  if FileExists(aIni) then
  begin
    aIniFile := TIniFile.Create(aIni);
    try
      aPath := aIniFile.ReadString(SysPlatform, 'Workspace', '');
      Result := DirectoryExists(aPath);
    finally
      aIniFile.Free;
    end;
  end;
  if not Result then
  begin
    with TEditorSetupForm.Create(Application) do
    begin
      Result := ShowModal = mrOK;
      Free;
    end;
  end;
end;

function AnotherInstance: Boolean;
var
  aClient: TSimpleIPCClient;
begin
  if (ParamCount > 0) and not (SameText(ParamStr(1), '/dde')) then
  begin
    aClient := TSimpleIPCClient.Create(nil);
    try
      aClient.ServerID := sApplicationID;
      Result := aClient.ServerRunning;//There is another instance
      if Result then
      begin
        aClient.Connect;
        try
          aClient.SendStringMessage(1, ParamStr(1));
        finally
          aClient.Disconnect;
        end;
      end
    finally
      aClient.Free;
    end;
  end
  else
  begin
    Result := False;
  end
end;

procedure Run;
begin
  Application.CreateForm(TEditorResource, EditorResource);
  Application.CreateForm(TMainForm, MainForm);
end;

begin
  if not AnotherInstance then
  begin
    Application.BidiMode := bdLeftToRight;
    Application.Initialize;
    if CheckSetup then
    begin
      Run;
      Application.Run;
    end;
  end;
end.

