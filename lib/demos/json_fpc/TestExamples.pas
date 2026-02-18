unit TestExamples;

//https://github.com/briandfoy/json-acceptance-tests

interface

uses
  SysUtils, Classes, StrUtils,
  mnUtils, mnJSON, mnDON, mnConfigs;

procedure Run;

implementation

var
  Config: TConfFile;
  DON: TDON_Root = nil;
  AppPath: string;
  WorkPath: string;

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
  aFile, S, E: string;
  Files: TStringList;
  print_it: Boolean;
  options: TJSONParseOptions;
begin
  try
    options := [jsoStrict];
    //E := JsonLintFile('test\fail09.json', [jsoStrict]);
    //E := JsonLintFile('test.json', [jsoStrict]);
    //exit;

    if jsoStrict in options then
      WriteLn('Strict On')
    else
      WriteLn('Strict Off');
    aFile := Config.ReadString('');

    if aFile = '' then
    begin
      DON := TDON_Root.Create(nil);
      try
        Parser.Init(DON, @JsonParseAcquireCallback, options);
        Parser.Parse('{'#13);
        Parser.Parse('  Books: {'#13);
        Parser.Parse('    Book1: {'#13);
        Parser.Parse('      Title: "' + cTestValue + '"'#13);
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
      finally
        DON.Free;
      end
    end;

    print_it := Config.ReadSwitch('-print');
    Files := TStringList.Create;
    try
      if aFile <> '' then
          Files.Add(ExpandToPath(aFile, WorkPath))
      else
        EnumFiles(Files, WorkPath, '*.json', [efFile, efFullPath]);

      for S in Files do
      begin
        if print_it then
        begin
          DON := JsonParseFile(S);
          try
            JsonConsoleSerialize(DON);
          finally
            DON.Free;
          end;
        end
        else
        begin
          E := JsonLintFile(S, [jsoSafe]);

          if E <> '' then
            Writeln('FAIL: ' + ExtractFileName(S)+' : ' + E)
          else
            Writeln('PASS: ' + ExtractFileName(S));
        end;
      end;
    finally
      Files.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.Message);
    end;
  end;
  WriteLn('Type Enter to exit');
  ReadLn;
end;

initialization
  AppPath := ExtractFilePath(ParamStr(0));
  WorkPath := AppPath + 'test' + PathDelim;
  Config := ParseArguments(['-print']);
end.
