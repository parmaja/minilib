program SampleSMTPClient;

{$APPTYPE CONSOLE}

uses
  mnIRCClients,mnHttpClient, mnSMTPClient;

begin
  SMTPMail('localhost:25', 'smtp@localhost', 'syspwd', 'smtp@localhost', 'zaher@localhost', 'hello', 'as subject');
  Write('Press Enter');
  Readln;
end.
