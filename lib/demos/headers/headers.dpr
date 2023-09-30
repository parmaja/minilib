program arguments;


{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, mnUtils;


type

  { TMyArguments }

  TMyArguments = class(TObject)
  protected
    procedure DoRun;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure MyArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; IsSwitch:Boolean; var Resume: Boolean);
begin
  Writeln(Name + '=' + Value);
end;

{ TMyArguments }

procedure TMyArguments.DoRun;
var
  ErrorMsg: String;
  sText: string;
  list: TStringList;
  files: TStringList;
  s: string;
  i: Integer;
begin

  //sText := '-t -s -v: value test';
  sText := 'build "c:\projects\project.pro" -t /s -v: " -z -d -r: value" test --value:testin --verbose=true platform= win32 compiler=dccarm -x=-x';
  //sText := '-w zaher test';
  //sText := '"-v":test'; //bug
  //file=/data/file.exe /D
  //file=c:/data/file.exe /D

{

name="test;code"data;

name=test;codedata

file=/data/file.exe /D

name="test;code"data"mdhfg;sdf";

name="test;code"data"mdhfg;sdf";


name=test";code"data"mdhfg;sdf";




name=     "test;code";


run.exe /f: c:\file.txt

run.exe --file=c:\file.txt

name= value
name=   value

}

  //sText := 'file=/data/file.exe /D: value'; //bug
  sText := 'name=a";b"c;"m;"';
  sText := 'a "a" a,b a;b "a;b" a="b"';
  sText := 'name=a;;s';

  ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [pargDeqoute, pargKeepSwitch], [';', ' '], [' ', #9], ['''','"'], [':', '=']);
  WriteLn('--------');
  WriteLn('');
  //Exit;



  ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [pargDeqoute, pargKeepSwitch], [' ', #9], [' ', #9], ['''','"'], [':', '=']);
  WriteLn('--------');
  WriteLn('');

  ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [pargKeepSwitch], [' ', #9], [' ', #9], ['''','"'], [':', '=']);
  WriteLn('--------');
  WriteLn('');

  ParseArgumentsCallback(sText, @MyArgumentsCallbackProc, nil, ['-', '/'], [], [' ', #9], [' ', #9], ['''','"'], [':', '=']);
  WriteLn('--------');
  WriteLn('');

  list := TStringList.Create;
  files := TStringList.Create;
  sText := 'test1 test1 -s --silent -w -v: value1';
  ParseArguments(sText, list, ['-', '/'], [pargKeepSwitch]);
  GetArgument(list, files);

  for i := 0 to list.Count-1 do
    WriteLn(list[i]);

  WriteLn('--------');
  for i := 0 to files.Count-1 do
    WriteLn(files[i]);

  GetArgumentValue(list, s, 'v');
  WriteLn(s);
  if GetArgumentSwitch(list, '-s', '--silent') then
    WriteLn('s is exists')
  else
    WriteLn('s is NOT exists');

  list.Free;
  files.Free;

  // stop program loop
end;

constructor TMyArguments.Create;
begin
  inherited Create;

end;

destructor TMyArguments.Destroy;
begin
  inherited Destroy;
end;

procedure TMyArguments.WriteHelp;
begin
  { add your help code here }
end;

var
  r: TMyArguments;
begin
  r := TMyArguments.Create;
  try
    r.DoRun;
    ReadLn;
  finally
    r.Free;
  end;
end.

