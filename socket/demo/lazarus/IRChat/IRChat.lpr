program IRChat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, mnIRCClients, ChatRoomFrames;

{$R *.res}

begin
  RequireDerivedFormResource :=True;
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

