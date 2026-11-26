program ext_handle;

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

{**
 *  Test for registered file extension handlers (see web/doc/roadmap_ext_handle-qw.md).

 *  Starts a TmodWebServer with a WebElements module serving the fixtures dir,
 *  then requests files with the same process HTTP client:
 *    - *.md            must be rendered by TmnwMarkdownSchema (HTML page)
 *    - folder /        must resolve index.md and render it
 *    - *.md?raw=1      must send the original bytes
 *    - *.txt           no handler, must send the raw bytes
 *  Exits with 0 when all checks pass, 1 otherwise.
}

uses
  SysUtils, Classes,
  mnUtils, mnStreams, mnModules, mnWebModules, mnWebElements,
  mnBootstraps, mnwMarkdownIt, mnHttpClient;

const
  TestPort = '18097';
  ReadmeContent = '# Test Heading' + sLineBreak + 'This is **bold** text.';
  IndexContent = '# Folder Index' + sLineBreak + 'The **folder** page.';
  TextContent = 'plain text data';

type
  { TFilesSchema }

  TFilesSchema = class(THTML)
  protected
    procedure DoCompose(const Ctx: TmnwContext); override;
  end;

  { TExtModule }

  TExtModule = class(TmnwWebModule)
  protected
    procedure InitItems; override;
  end;

var
  FixtureDir: string;
  FailedCount: Integer;
  DebugBody: Boolean;

procedure WriteFixture(const AName, AText: string);
var
  aList: TStringList;
begin
  aList := TStringList.Create;
  try
    aList.Text := AText;
    aList.SaveToFile(IncludePathDelimiter(FixtureDir) + AName);
  finally
    aList.Free;
  end;
end;

procedure Check(const ATest, ADetail: string; AOk: Boolean);
begin
  if AOk then
    WriteLn('OK   ', ATest)
  else
  begin
    WriteLn('FAIL ', ATest, ' -> ', ADetail);
    Inc(FailedCount);
  end;
end;

{ TFilesSchema }

procedure TFilesSchema.DoCompose(const Ctx: TmnwContext);
begin
  inherited;
  ServeFiles := [serveEnabled, serveSmart, serveDefault, serveIndex];
  PublicPath := FixtureDir;
end;

{ TExtModule }

procedure TExtModule.InitItems;
begin
  inherited;
  Web.RegisterSchema('', TFilesSchema);
  Web.RegisterExtension('md', TmnwMarkdownSchema);
  Web.RegisterExtension('markdown', TmnwMarkdownSchema);
end;

function ReadPage(AClient: TmnHttpClient; const AURL: string; out ABody: string): string;
begin
  try
    AClient.Disconnect; //clean any leftover connection
  except
  end;
  AClient.GetString(Utf8String(AURL), ABody);
  if DebugBody then
    WriteLn('URL: ', AURL, #13#10, '  len=', Length(ABody),
      ' ct=', AClient.Response.ContentType,
      ' answer=', Ord(AClient.Response.Answer), #13#10, Copy(ABody, 1, 300));
  Result := AClient.Response.ContentType;
end;

var
  aServer: TmodWebServer;
  aModule: TExtModule;
  aClient: TmnHttpClient;
  aBody: string;
  aContentType: string;
  aTried: Integer;
begin
  FailedCount := 0;
  DebugBody := FindCmdLineSwitch('debug');
  FixtureDir := IncludePathDelimiter(ExtractFilePath(ParamStr(0))) + 'fixtures';
  ForceDirectories(FixtureDir);
  WriteFixture('readme.md', ReadmeContent);
  WriteFixture('index.md', IndexContent);
  WriteFixture('data.txt', TextContent);

  aServer := TmodWebServer.Create;
  aServer.Port := TestPort;
  aModule := TExtModule.Create(aServer, 'ext', 'ext');
  aModule.PublicPath := FixtureDir;
  aModule.PrivatePath := IncludePathDelimiter(ExtractFilePath(ParamStr(0))) + 'private';
  ForceDirectories(IncludePathDelimiter(aModule.PrivatePath) + 'cache');
  ForceDirectories(IncludePathDelimiter(aModule.PrivatePath) + 'temp');
  aServer.Start;

  if FindCmdLineSwitch('serve') then
  begin
    WriteLn('serving on http://127.0.0.1:', TestPort, '/ext/');
    while True do
      Sleep(200);
  end;

  aClient := TmnHttpClient.Create;
  try
    //wait for the server to listen
    aTried := 0;
    while (aTried < 20) do
    begin
      try
        aClient.GetString(Utf8String('http://127.0.0.1:' + TestPort + '/ext/readme.md'), aBody);
        break;
      except
        on Exception do
        begin
          Inc(aTried);
          Sleep(100);
        end;
      end;
    end;

    //*.md renders as a markdown page
    aContentType := ReadPage(aClient, 'http://127.0.0.1:' + TestPort + '/ext/readme.md', aBody);
    Check('md rendered as html', 'content-type=' + aContentType, Pos('text/html', aContentType) > 0);
    Check('md page has markdown block', Copy(aBody, 1, 120), Pos('markdown-body', aBody) > 0);
    Check('md page keeps the source', '', Pos('Test Heading', aBody) > 0);
    Check('md page has raw link', '', Pos('raw=1', aBody) > 0);

    //folder URL resolves index.md and renders it
    aContentType := ReadPage(aClient, 'http://127.0.0.1:' + TestPort + '/ext/', aBody);
    Check('index.md rendered as html', 'content-type=' + aContentType, Pos('text/html', aContentType) > 0);
    Check('index.md page has markdown block', '', Pos('markdown-body', aBody) > 0);
    Check('index.md page keeps the source', '', Pos('Folder Index', aBody) > 0);

    //?raw=1 sends the original bytes
    aContentType := ReadPage(aClient, 'http://127.0.0.1:' + TestPort + '/ext/readme.md?raw=1', aBody);
    Check('md raw content type', 'content-type=' + aContentType, Pos('text/markdown', aContentType) > 0);
    Check('md raw body', Copy(aBody, 1, 40), Pos(ReadmeContent, aBody) > 0);
    Check('md raw has no page', '', Pos('markdown-body', aBody) = 0);

    //unregistered extension sends the original bytes
    aContentType := ReadPage(aClient, 'http://127.0.0.1:' + TestPort + '/ext/data.txt', aBody);
    Check('txt raw content type', 'content-type=' + aContentType, Pos('text/plain', aContentType) > 0);
    Check('txt raw body', Copy(aBody, 1, 40), Pos(TextContent, aBody) > 0);
  finally
    aClient.Free;
    aServer.Stop;
    aServer.Free;
  end;

  if FailedCount = 0 then
  begin
    WriteLn('All extension handler tests passed.');
    Halt(0);
  end
  else
  begin
    WriteLn(IntToStr(FailedCount), ' test(s) failed.');
    Halt(1);
  end;
end.
