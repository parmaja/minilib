unit TestExamples;

//https://github.com/briandfoy/json-acceptance-tests

interface

uses
  SysUtils, Classes, StrUtils,
  mnUtils, mnJSON, mnDON;

procedure Run;

implementation

var
  DON: TDON_Root = nil;
  AppPath: string;

procedure PrintIt;
var
  i: Integer;
  Lines: TStringList;
begin
  Lines := TStringList.Create();
  try
    JsonSerialize(DON, Lines);
    for i := 0 to Lines.Count -1 do
      WriteLn(Lines[i]);
  finally
    Lines.Free;
  end;
end;

procedure Run;
const
  cTestValue: string = 'JSON Parsers';
var
  Parser: TmnJSONParser;
  S, E: string;
  Files: TStringList;
begin
  DON := TDON_Root.Create(nil);
  try
    //E := JsonLintFile('test\fail09.json', [jsoStrict]);
    //E := JsonLintFile('test.json', [jsoStrict]);
    //exit;

    Parser.Init(DON, @JsonParseAcquireCallback, []);
    Parser.Parse('{'#13);
    Parser.Parse('  Books: {'#13);
    Parser.Parse('    Book1: {'#13);
    Parser.Parse('      Title: "'+cTestValue+'"'#13);
    Parser.Parse('    }'#13);
    Parser.Parse('  }'#13);
    Parser.Parse('}'#13);
    Parser.Finish;

    Writeln;
    Writeln('-----------------');
    PrintIt;
    Writeln('-----------------');
    Writeln;

    S := DON['Books']['Book1']['Title'].AsString;
    if S = cTestValue then
      Writeln('1: TestValue Success')
    else
      Writeln('1: Error TestValue');

    Writeln('');

    Files := TStringList.Create;
    try
      EnumFiles(Files, AppPath + 'test\', '*.json', [efFile, efFullPath]);
      for S in Files do
      begin
        E := JsonLintFile(S, [jsoSafe, jsoStrict]);

        if E <> '' then
          Writeln('FAIL: ' + ExtractFileName(S)+' : ' + E)
        else
          Writeln('PASS: ' + ExtractFileName(S));
      end;
    finally
      Files.Free;
    end;

  finally
    DON.Free;
  end;
  WriteLn('Type Enter to exit');
  ReadLn;
end;

initialization
  AppPath := ExtractFilePath(ParamStr(0));
end.
