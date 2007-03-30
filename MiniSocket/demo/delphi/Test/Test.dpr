program Test;

uses
  Forms,
  MainForms in 'MainForms.pas' {Form1},
  CmdSockets in 'CmdSockets.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
