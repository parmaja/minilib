program Translator;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Classes,
  Controls,
  Forms,
  AboutForms,
  OptionForms,
  LogForms,
  Setups,
  Main, NewProjectForms, trsClasses, trsProjects {SetupForm};

function CheckSetup: Boolean;
var
  FileName: string;
  aStrings: TStringList;
  v: string;
begin
  FileName := Application.Location + 'init.cfg';
  if FileExists(FileName) then
  begin
    try
      aStrings := TStringList.Create;
      v := aStrings.Values['WorkPath'];
      Result := v <> '';
    finally
      aStrings.Free;
    end;
  end;
  if not Result then
  begin
    with TSetupForm.Create(Application) do
    begin
      Result := ShowModal = mrOK;
      Free;
    end;
  end;
end;

{$IFDEF WINDOWS}{$R Translator.rc}{$ENDIF}

begin
  Application.Initialize;
  if CheckSetup then
    Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
