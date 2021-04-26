program TestStream;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  SendAndRecv;

begin
  Application := TTestStream.Create;
  Application.Run;
  Application.Free;
end.

