program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LangClasses in '..\..\..\translator\LangClasses.pas',
  StringHashList in '..\..\..\translator\StringHashList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
