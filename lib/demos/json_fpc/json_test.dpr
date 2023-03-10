program json_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, TestExamples;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
