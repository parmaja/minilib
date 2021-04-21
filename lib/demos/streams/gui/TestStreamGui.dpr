program TestStreamGui;

uses
  System.StartUpCopy,
  SendAndRecv in '..\SendAndRecv.pas',
  Unit1 in 'Unit1.pas' {Form1},
  FMX.Forms;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
