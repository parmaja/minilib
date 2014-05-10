program testcipher;

uses
  Forms,
  main in 'main.pas' {MainForm};
  //zlibcipher in '..\..\..\zlibcipher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
