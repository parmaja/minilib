program filhandler_check;
{**
 *  Tests for mnwFileHandlers (registered file extensions -> schema) and the
 *  markdown MIME registration in mnMIME.
 *
 *  Build (fpc 3.3, win64):
 *    fpc -Mdelphi -Twin64 -FUout -Fu..\..\lib -Fu..\..\socket\source -Fu..\..\web\source filhandler_check.dpr
 *
 *  No server is needed: the registry, the extension matching, the security gates
 *  and the MIME types are all reachable directly.
 *}
{$H+}
{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils, Classes,
  mnUtils, mnClasses, mnMIME, mnModules,
  mnWebElements, mnwFileHandlers, mnwDocuments, mnwMarkdownFile;

var
  Failures: Integer = 0;
  Checks: Integer = 0;

procedure Check(const Condition: Boolean; const What: string);
begin
  Inc(Checks);
  if Condition then
    WriteLn('  ok   ', What)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL ', What);
  end;
end;

function TempFile(const AName, AContent: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) + AName;
  with TStringList.Create do
  try
    Text := AContent;
    SaveToFile(Result);
  finally
    Free;
  end;
end;

procedure TestMIME;
begin
  WriteLn('MIME registration');
  Check(DocumentToContentType('README.md') = 'text/markdown', 'md -> text/markdown');
  Check(DocumentToContentType('notes.markdown') = 'text/markdown', 'markdown -> text/markdown');
  Check(DocumentToContentType('a.mkd') = 'text/markdown', 'mkd -> text/markdown');
  Check(DocumentToContentType('a.mkdn') = 'text/markdown', 'mkdn -> text/markdown');
  Check(DocumentToContentType('a.mdown') = 'text/markdown', 'mdown -> text/markdown');
  //* unchanged behavior for existing types
  Check(DocumentToContentType('app.css') = 'text/css', 'css unchanged');
  Check(DocumentToContentType('app.js') = 'text/javascript', 'js unchanged');
  Check(DocumentToContentType('page.html') = 'text/html', 'html unchanged');
end;

procedure TestRegistry;
var
  Handlers: TmnwFileHandlers;
  H: TmnwFileHandler;
begin
  WriteLn('Registry lookup');
  Handlers := TmnwFileHandlers.Create;
  try
    H := Handlers.Add(TmnwMarkdownFileHandler.Create);
    Check(Handlers.Count = 1, 'one handler registered');

    Check(Handlers.FindByExtension('md') = H, 'find by md');
    Check(Handlers.FindByExtension('.md') = H, 'find by .md');
    Check(Handlers.FindByExtension('MD') = H, 'find is case insensitive');
    Check(Handlers.FindByExtension('markdown') = H, 'find by markdown alias');
    Check(Handlers.Find('folder/sub/README.MD') = H, 'find by file name, upper case');
    Check(Handlers.Find('style.css') = nil, 'css has no handler');
    Check(Handlers.Find('noext') = nil, 'no extension, no handler');
    Check(Handlers.Find('') = nil, 'empty name, no handler');

    Check(H.CanHandle('a.md'), 'CanHandle a.md');
    Check(not H.CanHandle('a.pas'), 'CanHandle rejects a.pas');

    Check(Handlers.Enabled, 'registry enabled by default');
  finally
    Handlers.Free;
  end;
end;

procedure TestSecurityGates;
var
  Handlers: TmnwFileHandlers;
  H: TmnwFileHandler;
begin
  WriteLn('Security gates');
  Handlers := TmnwFileHandlers.Create;
  try
    H := Handlers.Add(TmnwMarkdownFileHandler.Create);
    //* dot files are never rendered, same rule as TmnwElement.ServeFile
    Check(not H.CanHandle('.env'), 'dot file has no extension match');
    Check(H.Visible('/tmp/readme.md'), 'plain file is visible');
    Check(not H.Visible('/tmp/.hidden.md'), 'dot file is not visible');
    Check(not H.Visible(''), 'empty name is not visible');
  finally
    Handlers.Free;
  end;
end;

procedure TestMaxSize;
var
  Handlers: TmnwFileHandlers;
  H: TmnwFileHandler;
begin
  WriteLn('Max size');
  Handlers := TmnwFileHandlers.Create;
  try
    Handlers.MaxSize := 10;
    H := Handlers.Add(TmnwMarkdownFileHandler.Create);
    Check(H.MaxSize = 10, 'handler inherits registry default max size');
    H.MaxSize := 0;
    H.AddExtension('txt');
    Check(H.CanHandle('a.txt'), 'extra extension accepted');
    Check(H.CanHandle('A.TXT'), 'extra extension case insensitive');
    Check(H.Extensions = 'markdown,md,mdown,mkd,mkdn,txt', 'extensions listed sorted: ' + H.Extensions);
  finally
    Handlers.Free;
  end;
end;

begin
  WriteLn('=== mnwFileHandlers check ===');
  try
    TestMIME;
    TestRegistry;
    TestSecurityGates;
    TestMaxSize;
  except
    on E: Exception do
    begin
      Inc(Failures);
      WriteLn('  EXCEPTION: ', E.ClassName, ': ', E.Message);
    end;
  end;

  WriteLn;
  WriteLn(Format('%d checks, %d failures', [Checks, Failures]));
  if Failures > 0 then
    Halt(1);
end.
