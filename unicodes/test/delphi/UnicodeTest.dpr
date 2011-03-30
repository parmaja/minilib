program UnicodeTest;

uses
  Forms,
  MainForms in 'MainForms.pas' {Form1},
  cp1256 in '..\..\cp1256.pas',
  cputils in '..\..\cputils.pas',
  cp1252 in '..\..\cp1252.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
