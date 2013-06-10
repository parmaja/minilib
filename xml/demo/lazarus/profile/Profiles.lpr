program Profiles;

{$MODE Delphi}

uses
  Forms, Interfaces, Classes,
  MainForms in 'MainForms.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.BidiMode := bdLeftToRight;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
