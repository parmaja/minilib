program IRChat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, mnIRCClients, ScatServers
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource :=True;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

