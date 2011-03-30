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
  Setups, Main, NewProjectForms, trsClasses, trsProjects;

function CheckSetup: Boolean;
begin
  Result := True;
  if trsEngine.WorkPath = '' then
  begin
    Result := ShowSetup;
  end;
end;

{$IFDEF WINDOWS}{$R Translator.rc}{$ENDIF}

begin
  Application.Initialize;
  trsEngine.Init;
  if CheckSetup then
  begin
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.
