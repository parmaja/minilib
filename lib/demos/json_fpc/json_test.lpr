program json_test;

//https://github.com/briandfoy/json-acceptance-tests

uses
  mnJSON, mnDON;

procedure Run;
var
  DON: TDON_Root;
  Parser: TmnJSONParser;
begin
  DON := TDON_Root.Create(nil);
  try
    Parser.Init(DON, @JsonAcquireCallback, []);
    Parser.Parse('{');
    Parser.Parse('  Books: {');
    Parser.Parse('    Book1: {');
    Parser.Parse('      Title: "JSON Parsers"');
    Parser.Parse('    }');
    Parser.Parse('  }');
    Parser.Parse('}');
    Parser.Finish;

  finally
    DON.Free;
  end;
end;

begin
  Run;
end.
