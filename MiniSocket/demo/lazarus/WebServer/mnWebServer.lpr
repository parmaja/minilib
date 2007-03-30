program mnWebServer;

uses
  Interfaces,
  Forms,
  LCLIntf,
  SysUtils,
  mnWin32Sockets,
  mnStreams,
  mnClients,
  MainForm in 'MainForm.pas' {Main}, minisockets;

begin
  Application.Initialize;
  if FindCmdLineSwitch('hide',true) then
  begin
    Application.ShowMainForm:=False;
//    Application.Active := False;
//    ShowWindow(Application.Handle,SW_HIDE);
  end;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
