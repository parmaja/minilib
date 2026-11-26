program request_check;
{**
 *  End to end check of the whole request path, in process:
 *    request line -> module match -> TmnwWebCommand -> TmnwWeb -> schema ->
 *    TmnwElement.ServeFile -> TmnwFileHandlers -> TmnwDocumentSchema -> render
 *
 *  It drives the very same command the server calls for a live connection, but
 *  over an in-memory stream, so no TCP socket or background thread is involved.
 *
 *  Asserts:
 *    1. GET /app/doc/README.md       -> 200 text/html, the rendered page
 *    2. GET /app/doc/README.md?raw=1 -> 200 text/markdown, the file itself
 *    3. GET /app/doc/style.css       -> 200 text/css, untouched
 *    4. GET /app/doc/.hidden.md      -> 403, dot files are never served
 *
 *  Build (fpc 3.3, win64):
 *    fpc -Mdelphi -Twin64 -FUout -Fu..\..\lib -Fu..\..\socket\source -Fu..\..\web\source request_check.dpr
 *}
{$H+}
{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils, Classes, StrUtils,
  mnUtils, mnClasses, mnSockets, mnStreams, mnModules, mnWebModules,
  mnWebElements, mnwFileHandlers, mnwDocuments, mnwMarkdownFile;

const
  cModuleName = 'app';
  cModuleAlias = 'doc';
  cSchemaName = 'home';
  cHost = '127.0.0.1:8080';

var
  Failures: Integer = 0;
  Checks: Integer = 0;
  DocDir: string;

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

procedure WriteTextFile(const FileName, Content: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Text := Content;
    L.SaveToFile(FileName);
  finally
    L.Free;
  end;
end;

type
  { TReply }

  TReply = record
    Status: string;
    ContentType: string;
    Body: string;
  end;

  { TDocSchema }

  //* A root schema that serves the documents folder. TmnwSchema turns
  //* on serveRender and schemaStartup, so we get an instance to configure.
  TDocSchema = class(TmnwSchema)
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
    procedure DoInit; override;
  end;

class function TDocSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  //* schemaStatic keeps it alive between requests; without it the web frees
  //* the schema after every response and leaves a dangling entry in its list
  Result := [schemaStartup, schemaStatic];
end;

procedure TDocSchema.DoInit;
begin
  inherited;
  //* set here, not on the instance made by RegisterSchema, so every schema
  //* the web creates for this class serves files
  ServeFiles := [serveEnabled, serveIndex, serveDefault, serveSmart, serveRender];
end;

var
  Server: TmodWebServer;
  Module: TmnwWebModule;

{ Creates a module, its schema and the registered file handlers. }
procedure BuildApp(const ADocDir: string);
var
  aSchema: TmnwSchema;
begin
  Server := TmodWebServer.Create;
  Module := TmnwWebModule(Server.Add(TmnwWebModule, cModuleName, cModuleAlias));
  Module.Web.PublicPath := IncludeTrailingPathDelimiter(ADocDir);

  //* the schema is the document root of the module, so files are requested
  //* as /app/doc/<file>
  aSchema := Module.Web.RegisterSchema(cSchemaName, TDocSchema, True);
  aSchema.ServeFiles := [serveEnabled, serveIndex, serveDefault, serveSmart, serveRender];
  aSchema.PublicPath := IncludeTrailingPathDelimiter(ADocDir);

  //* this is what an app does to enable 'md' rendering
  WebRegisterFileHandler(Module.Web, TmnwMarkdownFileHandler);
  Module.Web.Start;
end;

{ Runs one GET through the same command a live connection would use. }
function Request(const APath: string): TReply;
var
  Mem: TMemoryStream;
  Wrapper: TmnWrapperStream;
  RequestLine, Head: string;
  Line: string;
  aRequest: TmodRequest;
  aModules: TmodWebModules;
  aMatched: TmodModule;
  aCommand: TmnwWebCommand;
  aResult: TmodRespondResult;
  Response: TmnwResponse;
  aBody: RawByteString;
begin
  Result.Status := '';
  Result.ContentType := '';
  Result.Body := '';

  Mem := TMemoryStream.Create;
  Wrapper := TmnWrapperStream.Create(Mem, #13#10, False);
  aRequest := nil;
  aCommand := nil;
  try
    //* a minimal but complete request head, exactly as the server parses it
    RequestLine := 'GET ' + APath + ' HTTP/1.1';
    aModules := Server.Modules as TmodWebModules;
    //* TwebRequest is what the module builds for a live connection
    aRequest := TwebRequest.Create(nil, Wrapper);
    aRequest.Client := '127.0.0.1';
    aRequest.PutHeader('Host', cHost);
    aModules.ParseHead(aRequest, RequestLine);

    //* the server matches the request against the modules before creating a
    //* command; matching fills in the command name and the module params
    aMatched := aModules.Match(aRequest);
    Check(aMatched = Module, 'request matches the web module');

    aCommand := TmnwWebCommand.Create(Module, aRequest);
    aResult.Status := [];
    aResult.Timout := 0;
    aCommand.RespondResult(aResult);

    Response := aCommand.Response;
    if Response <> nil then
    begin
      Result.Status := IntToStr(Response.StatusCode) + ' ' + Response.StatusResult;
      Result.ContentType := Response.ContentType;
    end;

    //* the writer wrote into the body of the response, which is our wrapper
    SetLength(aBody, Mem.Size);
    if Mem.Size > 0 then
    begin
      Mem.Position := 0;
      Mem.ReadBuffer(aBody[1], Mem.Size);
    end;
    Result.Body := string(aBody);
  finally
    aCommand.Free;
    aRequest.Free;
    Wrapper.Free;
    Mem.Free;
  end;
end;

procedure TestRendered;
var
  R: TReply;
begin
  WriteLn('Rendered document');
  R := Request('/' + cModuleAlias + '/' + cSchemaName + '/README.md');
  Check(Pos('200', R.Status) > 0, 'status 200 (got: ' + R.Status + ')');
  Check(Pos('text/html', LowerCase(R.ContentType)) > 0, 'content type text/html (got: ' + R.ContentType + ')');
  Check(Pos('mnw-document', R.Body) > 0, 'body has the document container');
  Check(Pos('mnw_markdown_doc_src', R.Body) > 0, 'markdown source block embedded');
  Check(Pos('?raw=1', R.Body) > 0, 'page has a raw source link');
end;

procedure TestRaw;
var
  R: TReply;
begin
  WriteLn('Raw document');
  R := Request('/' + cModuleAlias + '/' + cSchemaName + '/README.md?raw=1');
  Check(Pos('200', R.Status) > 0, 'status 200 (got: ' + R.Status + ')');
  Check(Pos('text/markdown', LowerCase(R.ContentType)) > 0, 'content type text/markdown (got: ' + R.ContentType + ')');
  Check(Pos('# Hello', R.Body) > 0, 'raw markdown text returned');
  Check(Pos('mnw-document', R.Body) = 0, 'raw is not rendered');
end;

procedure TestOtherFiles;
var
  R: TReply;
begin
  WriteLn('Other static files are untouched');
  R := Request('/' + cModuleAlias + '/' + cSchemaName + '/style.css');
  Check(Pos('200', R.Status) > 0, 'status 200 (got: ' + R.Status + ')');
  Check(Pos('text/css', LowerCase(R.ContentType)) > 0, 'content type text/css (got: ' + R.ContentType + ')');
  Check(Pos('body{color:#333}', R.Body) > 0, 'css content unchanged');
end;

procedure TestSecurity;
var
  R: TReply;
begin
  WriteLn('Security');
  R := Request('/' + cModuleAlias + '/' + cSchemaName + '/.hidden.md');
  Check(Pos('200', R.Status) = 0, 'dot file is not served (got: ' + R.Status + ')');
  Check(Pos('# secret', R.Body) = 0, 'dot file content is not leaked');
end;

begin
  WriteLn('=== web request end to end check ===');

  DocDir := IncludeTrailingPathDelimiter(GetTempDir) + 'mnw_req_' + IntToStr(GetProcessID);
  ForceDirectories(DocDir);
  WriteTextFile(IncludeTrailingPathDelimiter(DocDir) + 'README.md',
    '# Hello' + #13#10 + #13#10 + 'This is **markdown**.');
  WriteTextFile(IncludeTrailingPathDelimiter(DocDir) + 'style.css', 'body{color:#333}');
  WriteTextFile(IncludeTrailingPathDelimiter(DocDir) + '.hidden.md', '# secret');

  try
    BuildApp(DocDir);
    try
      TestRendered;
      TestRaw;
      TestOtherFiles;
      TestSecurity;
    finally
      Module.Web.Stop;
      Server.Free;
    end;
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
