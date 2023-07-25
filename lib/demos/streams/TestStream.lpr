program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  UnitTests,
  SendAndRecv;

begin
  Application := TTestStream.Create;
  Application.Run;
  Application.Free;
end.

