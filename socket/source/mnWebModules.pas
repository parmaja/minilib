unit mnWebModules;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

{**
-------------------
GET http://localhost/index.html HTTP/1.1
Host: localhost
Connection: Close

Post Body
-------------------

-------------------
method URI[path?params] http_version
headers[0]->Host: localhost
headers[1]->Connection: Close
headers[2]
-------------------

*}

{**
  Ref: https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html
*}


interface

uses
  SysUtils, Classes, syncobjs, StrUtils,
  mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams,
  mnModules;

type

  TmodWebModule = class;

  { TmodHttpRespond }

  TmodHttpRespond = class(TmodRespond)
  private
    FKeepAlive: Boolean;
    FContentLength: Integer;
    FCompressClass: TmnCompressStreamProxyClass;
    FCompressProxy: TmnCompressStreamProxy;
    FURIParams: TmnParams;
    FCookies: TmnParams;
    FRoot: string; //Document root folder
    FHost: string;
    procedure SetCompressClass(AValue: TmnCompressStreamProxyClass);
    procedure SetsCompressProxy(AValue: TmnCompressStreamProxy);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendHeader; override;
    property Cookies: TmnParams read FCookies;
    property URIParams: TmnParams read FURIParams;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    //Compress on the fly, now we use deflate
    property ContentLength: Integer read FContentLength write FContentLength;
    property CompressClass: TmnCompressStreamProxyClass read FCompressClass write SetCompressClass;
    property CompressProxy: TmnCompressStreamProxy read FCompressProxy write SetsCompressProxy;
    //Document root folder
    property Root: string read FRoot;
    property Host: string read FHost;
  end;

  { TmodHttpCommand }

  TmodHttpCommand = class abstract(TmodCommand)
  private
    function GetRespond: TmodHttpRespond;
  protected
    procedure Created; override;
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure Unprepare(var Result: TmodRespondResult); override;
    procedure RespondResult(var Result: TmodRespondResult); override;
    function CreateRespond: TmodRespond; override;
  public
    destructor Destroy; override;
    property Respond: TmodHttpRespond read GetRespond;
  end;

  { TmodURICommand }

  TmodURICommand = class(TmodHttpCommand)
  private
    function GetModule: TmodWebModule;
  protected
    function GetDefaultDocument(Root: string): string;
    procedure RespondNotFound;
    procedure RespondResult(var Result: TmodRespondResult); override;
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property Module: TmodWebModule read GetModule;
  end;

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class(TmodModule)
  private
    //FServer: TmodWebServer;
    procedure SetDefaultDocument(AValue: TStringList);
    procedure SetDocumentRoot(AValue: string);
  protected
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
    procedure Created; override;
    procedure DoCreateCommands; override;

    procedure Log(S: string); override;
  public
    destructor Destroy; override;
    property DocumentRoot: string read FDocumentRoot write SetDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  { TmodWebModules }

  TmodWebModules = class(TmodModules)
  protected
  public
    procedure ParseHead(ARequest: TmodRequest; const RequestLine: string); override;
  end;

  { TmodWebServer }

  TmodCustomWebServer = class(TmodModuleServer)
  protected
    function CreateModules: TmodModules; override;
  public
  end;

  TmodWebServer = class(TmodCustomWebServer)
  protected
  public
    constructor Create; override;
  end;

  {**
    Files Commands
  *}

  { TmodHttpGetCommand }

  TmodHttpGetCommand = class(TmodURICommand)
  protected
    procedure RespondDocument(const vDocument: string; var Result: TmodRespondResult); virtual;
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodHttpPostCommand }

  TmodHttpPostCommand = class(TmodHttpGetCommand)
  protected
    Contents: TMemoryStream;
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodPutCommand }

  TmodPutCommand = class(TmodURICommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodServerInfoCommand }

  TmodServerInfoCommand = class(TmodURICommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

  { TmodDirCommand }

  TmodDirCommand = class(TmodURICommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

  { TmodDeleteFileCommand }

  TmodDeleteFileCommand = class(TmodURICommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

var
  modLock: TCriticalSection = nil;

implementation

//TODO slow function needs to improvements
//https://stackoverflow.com/questions/1549213/whats-the-correct-encoding-of-http-get-request-strings

function URIDecode(const S: AnsiString; CodePage: Word = CP_UTF8): string;
var
  c: AnsiChar;
  D: Ansistring;
  i: Integer;
  R: RawByteString;
begin
  Result := '';
  i := Low(S);
  R := '';
  while i <= High(S) do
  begin
    C := S[i];
    {if C = '+' then
    begin
      R := R + ' ';
    end
    else}
    if C = '%' then
    begin
      D := copy(S, i + 1, 2);
      R := R + AnsiChar(StrToInt('$'+D));
      inc(i, 2);
    end
    else
      R := R + c;
    Inc(i);
  end;
  SetCodePage(R, CP_UTF8, False);
  Result := R;
end;

{ TmodHttpRespond }

procedure TmodHttpRespond.SetCompressClass(AValue: TmnCompressStreamProxyClass);
begin
  if FCompressClass <> nil then
    raise TmodModuleException.Create('Compress class is already set!');
  FCompressClass := AValue;
end;

procedure TmodHttpRespond.SetsCompressProxy(AValue: TmnCompressStreamProxy);
begin
  if FCompressProxy <> nil then
    raise TmodModuleException.Create('Compress proxy is already set!');
  FCompressProxy :=AValue;
end;

constructor TmodHttpRespond.Create;
begin
  inherited Create;
  FCookies := TmnParams.Create;
  FURIParams := TmnParams.Create;
end;

destructor TmodHttpRespond.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FURIParams);
  inherited Destroy;
end;

procedure TmodHttpRespond.SendHeader;
begin
  if Cookies.Count > 0 then
    PostHeader('Cookies', Cookies.AsString);

  inherited; //*<----------

  if CompressClass <> nil then
  begin
    if CompressProxy = nil then
    begin
      CompressProxy := CompressClass.Create([cprsWrite], 9);
      Stream.AddProxy(CompressProxy);
    end
    else
      CompressProxy.Enable;
  end;
end;

{ TmodHttpPostCommand }

procedure TmodHttpPostCommand.RespondResult(var Result: TmodRespondResult);
begin
  if SameText(Request.Method, 'POST') and (Request.Header['Content-Type'].Have('application/json')) then
  begin
    Contents := TMemoryStream.Create;
    if Request.Stream <> nil then
      Request.Stream.ReadStream(Contents, Respond.ContentLength);
    Contents.Position := 0; //it is memory btw
    //Contents.SaveToFile('d:\temp\json.json');
  end;
  inherited;
end;

{ TmodWebModule }

procedure TmodWebModule.SetDocumentRoot(AValue: string);
begin
  if FDocumentRoot =AValue then Exit;
  FDocumentRoot :=AValue;
end;

procedure TmodWebModule.SetDefaultDocument(AValue: TStringList);
begin
  FDefaultDocument.Assign(AValue);
end;

procedure TmodWebModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  UseKeepAlive := False;
  UseCompressing := True;

  FDocumentRoot := '';
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
end;

procedure TmodWebModule.DoCreateCommands;
begin
  inherited;
  RegisterCommand('GET', TmodHttpGetCommand, true);
  RegisterCommand('POST', TmodHttpPostCommand, true);
  RegisterCommand('Info', TmodServerInfoCommand);
  {
  RegisterCommand('GET', TmodHttpGetCommand);
  RegisterCommand('PUT', TmodPutCommand);
  RegisterCommand('DIR', TmodDirCommand);
  RegisterCommand('DEL', TmodDeleteFileCommand);
  }
end;

destructor TmodWebModule.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited Destroy;
end;

procedure TmodWebModule.Log(S: string);
begin
  inherited;
  Modules.Log(S);
end;

{ TmodURICommand }

procedure TmodURICommand.RespondNotFound;
var
  Body: string;
begin
  Respond.SendRespond('HTTP/1.1 200 OK');
  Respond.PostHeader('Content-Type', 'text/html');
  Respond.SendHeader;
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  Respond.Stream.WriteString(Body);
  Respond.KeepAlive := False;
end;

function TmodURICommand.GetModule: TmodWebModule;
begin
  Result := (inherited Module) as TmodWebModule;
end;

function TmodURICommand.GetDefaultDocument(Root: string): string;
var
  i: Integer;
  aFile: string;
begin
  //TODO baaad you need to luck before access
  for i := 0 to Module.DefaultDocument.Count - 1 do
  begin
    aFile := Root + Module.DefaultDocument[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
    end;
  end;
end;

procedure TmodURICommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
end;

procedure TmodURICommand.Prepare(var Result: TmodRespondResult);
begin
  inherited;
  Respond.FRoot := Module.DocumentRoot;
  Respond.FHost := Request.Header.ReadString('Host');
end;

procedure TmodURICommand.Created;
begin
  inherited Created;
end;

{ TmodHttpGetCommand }

function DocumentToContentType(FileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Length(Ext) > 1 then
    Ext := Copy(Ext, 2, Length(Ext));
  if (Ext = 'htm') or (Ext = 'html') or (Ext = 'shtml') or (Ext = 'dhtml') then
    Result := 'text/html'
  else if Ext = 'gif' then
    Result := 'image/gif'
  else if Ext = 'bmp' then
    Result := 'image/bmp'
  else if (Ext = 'jpg') or (Ext = 'jpeg') then
    Result := 'image/jpeg'
  else if (Ext = 'png') then
    Result := 'image/png'
  else if Ext = 'txt' then
    Result := 'text/plain'
  else if Ext = 'svg' then
    Result := 'image/svg+xml'
  else if Ext = 'css' then
    Result := 'text/css'
  else if Ext = 'json' then
    Result := 'application/json'
  else if Ext = 'js' then
    Result := 'text/js'
  else
    Result := 'application/binary';
end;

procedure TmodHttpGetCommand.RespondDocument(const vDocument: string; var Result: TmodRespondResult);
var
  DocSize: Int64;
  aDocStream: TFileStream;
begin
  if FileExists(vDocument) then
  begin
    if Active then
    begin
      aDocStream := TFileStream.Create(vDocument, fmOpenRead or fmShareDenyWrite);
      try
        DocSize := aDocStream.Size;
        if Active then
        begin
          Respond.SendRespond('HTTP/1.1 200 OK');
          Respond.PostHeader('Content-Type', DocumentToContentType(vDocument));
          if Respond.KeepAlive then
            Respond.PostHeader('Content-Length', IntToStr(DocSize));
        end;

        Respond.SendHeader;

        if Active then
          Respond.Stream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    RespondNotFound;
end;

procedure TmodHttpGetCommand.RespondResult(var Result: TmodRespondResult);
var
  aDocument: string;
begin
(*

  '/web'               path = ''
  '/web/'              path = '/'
  '/web/dashbord'     path = '/dashbord' is dir
  '/web/dashbord/'    path = '/dashbord' is dir
  '/web/dashbord/index' path = '/dashbord/index' is not dir
  '/web/dashbord/index.html' file

*)

  aDocument := IncludeTrailingPathDelimiter(Respond.Root);
  if Request.Path <> '' then
    aDocument := aDocument + '.' + Request.Path;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux
  if EndsText(PathDelim, aDocument) then //get the default file if it not defined
     aDocument := GetDefaultDocument(aDocument);
  aDocument := ExpandFileName(aDocument);

  if (Request.Path='') or (not EndsText(PathDelim, aDocument) and DirectoryExists(aDocument)) then
  begin
    //https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
    Request.Address := IncludeURLDelimiter(Request.Address);
    //Respond.SendRespond('HTTP/1.1 301 Moved Permanently');
    Respond.SendRespond('HTTP/1.1 302 Found');
    //Respond.SendRespond('HTTP/1.1 307 Temporary Redirect');

    Respond.PostHeader('Location', Request.CollectURI);
    Respond.SendHeader;
  end
  else
  begin

    RespondDocument(aDocument, Result);
  end;
  inherited;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Respond.SendRespond('OK');
  //Respond.Stream.WriteLine('Server is running on port: ' + Module.Server.Port);
  Respond.Stream.WriteLine('the server is: "' + ParamStr(0) + '"');
end;

{ TmodPutCommand }

procedure TmodPutCommand.RespondResult(var Result: TmodRespondResult);
var
  aFile: TFileStream;
  aFileName: string;
begin
  inherited;
  Respond.Stream.WriteCommand('OK');
  aFileName := Respond.URIParams.Values['FileName'];
  aFile := TFileStream.Create(Respond.Root + aFileName, fmCreate);
  try
    Respond.Stream.ReadStream(aFile, Respond.ContentLength);
  finally
    aFile.Free;
  end;
end;

{ TmodDirCommand }

procedure TmodDirCommand.RespondResult(var Result: TmodRespondResult);
var
  i: Integer;
  aStrings: TStringList;
  //aPath: string;
  aFilter: string;
begin
  inherited;
  Respond.Stream.WriteCommand('OK');
  aFilter := Respond.URIParams.Values['Filter'];
  //aPath := IncludeTrailingPathDelimiter(Root);
  if aFilter = '' then
    aFilter := '*.*';
   aStrings := TStringList.Create;
  try
    //EnumFileList(aPath + aFilter, aStrings);
    for i := 0 to aStrings.Count - 1 do
    begin
      Respond.Stream.WriteLine(IntToStr(i) + ': ' + aStrings[i]);
    end;
  finally
    aStrings.Free;
  end;
end;

{ TmodDeleteFileCommand }

procedure TmodDeleteFileCommand.RespondResult(var Result: TmodRespondResult);
var
  aFileName: string;
begin
  inherited;
  aFileName := IncludeTrailingPathDelimiter(Respond.Root) + Request.Path;
  if FileExists(aFileName) then
    DeleteFile(aFileName);
  Respond.Stream.WriteCommand('OK');
end;

{ TmodURICommand }

destructor TmodURICommand.Destroy;
begin
  inherited;
end;

{ TmodWebServer }

constructor TmodWebServer.Create;
begin
  inherited;
  Modules.DefaultModule := TmodWebModule.Create('web', 'doc', ['http/1.1'], Modules);
  Port := '80';
end;

function TmodCustomWebServer.CreateModules: TmodModules;
begin
  Result := TmodWebModules.Create(Self);
end;

{ TmodHttpCommand }

function TmodHttpCommand.GetRespond: TmodHttpRespond;
begin
  Result := inherited Respond as TmodHttpRespond;
end;

procedure TmodHttpCommand.Created;
begin
  inherited;
end;

destructor TmodHttpCommand.Destroy;
begin
  inherited;
end;

procedure TmodHttpCommand.Prepare(var Result: TmodRespondResult);
begin
  inherited;
  ParseParams(Request.Params, Respond.URIParams);

  if Module.UseKeepAlive and SameText(Request.Header.ReadString('Connection'), 'Keep-Alive') then
  begin
    Respond.KeepAlive := True;
    Respond.PostHeader('Connection', 'Keep-Alive');
    Respond.PostHeader('Keep-Alive', 'timout=' + IntToStr(Module.KeepAliveTimeOut div 5000) + ', max=100');
  end;

  if Module.UseCompressing then
  begin
    if Request.Header['Accept-Encoding'].Have('gzip', [',']) then
      Respond.CompressClass := TmnGzipStreamProxy
    else if Request.Header['Accept-Encoding'].Have('deflate', [',']) then
      Respond.CompressClass := TmnDeflateStreamProxy
    else
      Respond.CompressClass := nil;
    if Respond.CompressClass <> nil then
      Respond.PostHeader('Content-Encoding', Respond.CompressClass.GetCompressName);
  end;

  if (Request.Header['Content-Length'].IsExists) then
    Respond.ContentLength := Request.Header['Content-Length'].AsInteger;
end;

procedure TmodHttpCommand.Unprepare(var Result: TmodRespondResult);
var
  aParams: TmnParams;
begin
  inherited;
  if not Respond.Header.Exists['Content-Length'] then
    Respond.KeepAlive := False;
  if Respond.KeepAlive and Module.UseKeepAlive and SameText(Request.Header.ReadString('Connection'), 'Keep-Alive') then
  begin
    Result.Timout := Module.KeepAliveTimeOut;
    if Request.Header.IsExists('Keep-Alive') then //idk if really sent from client
    begin
      aParams := TmnParams.Create;
      try
        //Keep-Alive: timeout=5, max=1000
        aParams.Separator := '=';
        aParams.Delimiter := ',';
        aParams.AsString := Request.Header['Keep-Alive'].AsString;
        Result.Timout := aParams['timeout'].AsInteger;
      finally
        aParams.Free;
      end;
    end;
    Result.Status := Result.Status + [erKeepAlive];
  end;

  if Respond.CompressProxy <> nil then
  begin
    Respond.CompressProxy.Disable;
  end;
end;

procedure TmodHttpCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Log(Request.Client + ': ' + Request.Raw);
end;

function TmodHttpCommand.CreateRespond: TmodRespond;
begin
  Result := TmodHttpRespond.Create;
end;

{ TmodCustomWebModules }

procedure TmodWebModules.ParseHead(ARequest: TmodRequest; const RequestLine: string);
begin
  inherited ParseHead(ARequest, RequestLine);
  ARequest.URI := URIDecode(ARequest.URI);
  //ARequest.ParsePath(ARequest.URI); duplicate in parse head :)
  ARequest.Command := ARequest.Method;
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  FreeAndNil(modLock);
end.
