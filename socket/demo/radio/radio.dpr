program radio;

{**
 *  Radio demo for "Mini Library"
 *  Plays an IceCast stream using mnIceCasts.pas,
 *  sound output with Windows API (ACM MP3 decoder + waveOut).
 *
 *  Stream: http://solid24.streamupsolutions.com:8026/stream
 *}

uses
  Forms,
  MainForm in 'MainForm.pas' {MainForm},
  RadioPlayer in 'RadioPlayer.pas',
  mnTypes in '..\..\..\lib\mnTypes.pas',
  mnUtils in '..\..\..\lib\mnUtils.pas',
  mnLogs in '..\..\..\lib\mnLogs.pas',
  mnClasses in '..\..\..\lib\mnClasses.pas',
  mnLibraries in '..\..\..\lib\mnLibraries.pas',
  mnStreams in '..\..\..\lib\mnStreams.pas',
  mnSockets in '..\..\source\mnSockets.pas',
  mnWinSockets in '..\..\source\mnWinSockets.pas',
  mnOpenSSL3API in '..\..\source\mnOpenSSL3API.pas',
  mnOpenSSL in '..\..\source\mnOpenSSL.pas',
  mnConnections in '..\..\source\mnConnections.pas',
  mnClients in '..\..\source\mnClients.pas',
  mnIceCasts in '..\..\source\mnIceCasts.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Radio';
  Application.CreateForm(TMainForm, RadioForm);
  Application.Run;
end.
