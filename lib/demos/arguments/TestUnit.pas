unit TestUnit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, CustApp, mnUtils;

procedure RunTest;

implementation

procedure MyArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; IsSwitch:Boolean; var Resume: Boolean);
begin
  Writeln(Name + '=' + Value);
end;

{ TMyArguments }

procedure RunTest;
var
  sText: string;
  list: TStringList;
  files: TStringList;
  s: string;
  i: Integer;

  procedure ParseSpace;
  begin
    WriteLn('Parse: ' + sText);
    ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [pargDeqoute, pargKeepSwitch], [' ', #9], [' ', #9], ['''','"'], [':', '=']);
    WriteLn('--------');
    WriteLn('');
  end;

  procedure ParseColone;
  begin
    WriteLn('Parse: ' + sText);
    ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [pargDeqoute, pargKeepSwitch], [';'], [' ', #9], ['''','"'], [':', '=']);
    WriteLn('--------');
    WriteLn('');
  end;

begin
  sText := '""';
  ParseColone;
  sText := '-sss;b';
  ParseColone;
  sText := 'x=1;;b;';
  ParseColone;
  sText := 'a;;b';
  ParseColone;
  sText := 'a b  ';
  ParseSpace;
  sText := 'a';
  ParseSpace;
  sText := 'a;b;';
  ParseColone;
  sText := '''your name is "belal"''';
  ParseColone;

  //sText := '-t -s -v: value test';
  sText := 'build "c:\projects\project.pro" -t /s -v: " -z -d -r: value" test --value:testin --verbose=true platform=win32 compiler=dccarm -x=-x';
  ParseSpace;
  //sText := '"-v":test'; //bug
  ParseSpace;

  sText := '-w zaher test';
  ParseSpace;

  list := TStringList.Create;
  files := TStringList.Create;
  sText := 'test1 test1 -s --silent -w -v: value1';
  ParseArguments(sText, list, ['-', '/'], [pargKeepSwitch, pargValues]);

  GetArgument(list, files);
  for i := 0 to list.Count-1 do
    WriteLn(list[i]);

  WriteLn('--------');
  for i := 0 to files.Count-1 do
    WriteLn(files[i]);

  GetArgumentValue(list, s, '-v');
  WriteLn(s);
  if GetArgumentSwitch(list, '-s', '--silent') then
    WriteLn('s is exists')
  else
    WriteLn('s is NOT exists');

  list.Free;
  files.Free;
end;

end.

