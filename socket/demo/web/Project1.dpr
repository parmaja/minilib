program Project1;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  TestHtml in 'TestHtml.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
