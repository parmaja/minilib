program WebServer;

uses
  Forms,
  Windows,
  SysUtils,
  mnStreams,
  mnClients,
  MainForm in 'MainForm.pas' {Main},
  HomeModules in '..\..\lazarus\WebModule\HomeModules.pas';

{$R *.RES}

begin
  Application.Initialize;
  if FindCmdLineSwitch('hide',true) then
  begin
    Application.ShowMainForm:=false;
    ShowWindow(Application.Handle,SW_HIDE);
  end;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
