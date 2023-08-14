program SampleSMTPClient;

{$APPTYPE CONSOLE}

uses
  mnLogs, mnIRCClients, mnHttpClient, mnSMTPClient;

var
  Name, Email: string;
begin
  InstallConsoleLog;
  SMTPMail('localhost:25', 'Admin<smtp@localhost>', 'syspwd', 'Admin<smtp@localhost>', '"Zaher Dirkey"<zaher@localhost>', 'hello', 'as subject');
  Write('Press Enter');
  Readln;
end.
