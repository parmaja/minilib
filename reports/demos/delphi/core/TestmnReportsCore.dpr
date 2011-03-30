program TestmnReportsCore;

uses
  Forms,
  main in 'main.pas' {Form1},
  mnrLists in '..\..\..\source\core\mnrLists.pas',
  mnrClasses in '..\..\..\source\core\mnrClasses.pas',
  designer in 'designer.pas' {DesignerForm},
  mnrNodes in '..\..\..\source\core\mnrNodes.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
