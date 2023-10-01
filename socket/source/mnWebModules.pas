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
  SysUtils, Classes, syncobjs, StrUtils, //NetEncoding, Hash,
  mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnMultipartData, mnModules;

type

  TmodWebModule = class;
  THttpResult = (
    hrNone,
    hrOK,
    hrError,
    hrMovedTemporarily, //307
    hrFound, //302
    hrNotFound,
    hrSwitchingProtocols,
    hrServiceUnavailable
  );

  THttpResultHelper = record helper for THttpResult
    function ToString: string;
  end;

  { TmodHttpRespond }

  TmodHttpRespond = class(TmodRespond)
  private
    FKeepAlive: Boolean;
    FContentLength: Integer;
    FCompressClass: TmnCompressStreamProxyClass;
    FCompressProxy: TmnCompressStreamProxy;
    FRoot: string; //Document root folder
    FHost: string;
    FHttpResult: THttpResult;
    procedure SetCompressClass(AValue: TmnCompressStreamProxyClass);
    procedure SetsCompressProxy(AValue: TmnCompressStreamProxy);
  protected
    function HeadText: string; override;
    procedure DoSendHeader; override;
    procedure DoHeaderSent; override;
  public
    constructor Create;
    destructor Destroy; override;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    //Compress on the fly, now we use deflate
    property ContentLength: Integer read FContentLength write FContentLength;
    property CompressClass: TmnCompressStreamProxyClass read FCompressClass write SetCompressClass;
    property CompressProxy: TmnCompressStreamProxy read FCompressProxy write SetsCompressProxy;
    property HttpResult: THttpResult read FHttpResult write FHttpResult;
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
    procedure InternalError(ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; var Handled: Boolean); override;
    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;
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

  {$ifndef FPC}
  TmodWebEventProc = reference to procedure(vRequest: TmodRequest; vRespond: TmodHttpRespond; var vResult: TmodRespondResult);

  TmodWebEventModule = class(TmodWebModule)
  protected
    FProc: TmodWebEventProc; //need discuss
    procedure DoCreateCommands; override;
  end;

  TmodWebEventServer = class(TmodCustomWebServer)
  public
    constructor Create(const vPort: string; vProc: TmodWebEventProc);
  end;

  {**
    Files Commands
  *}

  { TmodHttpEventCommand }

  TmodHttpEventCommand = class(TmodURICommand)
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;
  {$endif}

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

function WebExpandToRoot(FileName: string; Root: string): string;

implementation

function WebExpandToRoot(FileName: string; Root: string): string;
begin
  if (FileName <> '') then
  begin
    if StartsStr('../', FileName) or StartsStr('..\', FileName) then
      Result := ExpandFileName(IncludePathDelimiter(Root) + FileName)
    else if StartsStr('./', FileName) or StartsStr('.\', FileName) then
      Result := IncludePathDelimiter(Root) + Copy(FileName, 3, MaxInt)
    else if StartsDelimiter(FileName) then
      Result := IncludePathDelimiter(Root) + Copy(FileName, 2, MaxInt)
    else
      Result := IncludePathDelimiter(Root) + FileName;
  end
  else
    Result := '';
end;

//TODO slow function needs to improvements
//https://stackoverflow.com/questions/1549213/whats-the-correct-encoding-of-http-get-request-strings

{
function HashWebSocketKey(const key: string): string;
var
  b: TBytes;
begin
  b := THashSHA1.GetHashBytes(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
  Result := TNetEncoding.Base64String.EncodeBytesToString(b);
end;
}

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
  FHttpResult := hrNone;
end;

destructor TmodHttpRespond.Destroy;
begin

  inherited Destroy;
end;

procedure TmodHttpRespond.DoHeaderSent;
begin
  inherited;

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

procedure TmodHttpRespond.DoSendHeader;
begin
  inherited;
end;

function TmodHttpRespond.HeadText: string;
begin
  Result := HttpResult.ToString;
end;

{ TmodHttpPostCommand }

procedure TmodHttpPostCommand.RespondResult(var Result: TmodRespondResult);
begin
  if SameText(Request.Method, 'POST') then
  begin
    if (Request.Header.Field['Content-Type'].Have('application/json')) then
    begin
      Contents := TMemoryStream.Create;
      if Request.Stream <> nil then
        Request.Stream.ReadStream(Contents, Request.ContentLength);
      Contents.Position := 0; //it is memory btw
      //Contents.SaveToFile('d:\temp\json.json');
    end
    else
    begin
      Contents := TMemoryStream.Create;
      if Request.Stream <> nil then
        Request.Stream.ReadStream(Contents, Request.ContentLength);
      Contents.Position := 0; //it is memory btw
      Contents.SaveToFile('c:\temp\1.txt');
    end;
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
  UseKeepAlive := klvUndefined;
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
  //use post and get as same command
  RegisterCommand('GET', TmodHttpGetCommand, true);
  //RegisterCommand('GET', TmodHttpPostCommand, true);
  RegisterCommand('POST', TmodHttpPostCommand, true);
  RegisterCommand('Info', TmodServerInfoCommand);
  {
  RegisterCommand('GET', TmodHttpGetCommand);
  RegisterCommand('PUT', TmodPutCommand);
  RegisterCommand('DIR', TmodDirCommand);
  RegisterCommand('DEL', TmodDeleteFileCommand);
  }
end;

procedure TmodWebModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  //inherited;

  ARequest.Command := ARequest.Method;
  ARequest.Path := Copy(ARequest.Address, Length(ARequest.Route[0]) + 1, MaxInt);
end;

procedure TmodWebModule.DoMatch(const ARequest: TmodRequest; var vMatch: Boolean);
begin
  //inherited;

  vMatch := ARequest.Route[0] = AliasName;
end;

procedure TmodWebModule.InternalError(ARequest: TmodRequest; ARequestStream, ARespondStream: TmnBufferStream; var Handled: Boolean);
begin
  inherited;
  ARespondStream.WriteLineUTF8('HTTP/1.1 500 Internal Server Error');
  ARespondStream.WriteLineUTF8('');
  Handled := True;
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
  Respond.HttpResult := hrOK;
  Respond.AddHeader('Content-Type', 'text/html');
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

{function CompressSize(vData: PByte; vLen: Integer): TFileSize;
var
  p: Pointer;
  aLen: Integer;
begin
  if vLen<>0 then
  begin
    ZCompress(Pointer(vData), vLen, p, aLen);
    Result := aLen;
    FreeMem(p);
  end
  else
    Result := 0;
end;}


procedure TmodHttpGetCommand.RespondDocument(const vDocument: string; var Result: TmodRespondResult);
var
  aDocSize: Int64;
  aDocStream: TFileStream;
begin
  if FileExists(vDocument) then
  begin
    if Active then
    begin
      aDocStream := TFileStream.Create(vDocument, fmOpenRead or fmShareDenyWrite);
      try
        {if Respond.KeepAlive then
          aDocSize := CompressSize(PByte(aDocStream.Memory), aDocStream.Size)
        else}
          aDocSize := aDocStream.Size;

        Respond.HttpResult := hrOK;

        if Active then
        begin
          Respond.AddHeader('Content-Type', DocumentToContentType(vDocument));
          if Respond.KeepAlive then
            Respond.AddHeader('Content-Length', IntToStr(aDocSize));
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
  aDocument, aRoot: string;
begin
(*

  '/web'               path = ''
  '/web/'              path = '/'
  '/web/dashbord'     path = '/dashbord' is dir
  '/web/dashbord/'    path = '/dashbord' is dir
  '/web/dashbord/index' path = '/dashbord/index' is not dir
  '/web/dashbord/index.html' file

*)


  aRoot := ExcludePathDelimiter(Respond.Root);


  if (Request.Path = '') or StartsDelimiter(Request.Path) or StartsStr('.', Request.Path) then
    aDocument := aRoot + Request.Path
  else
    aDocument := aRoot + '\' +Request.Path;

  aDocument := ExpandFileName(aDocument);


  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux

  if EndsDelimiter(aDocument) then //get the default file if it not defined
     aDocument := GetDefaultDocument(aDocument);

  if not StartsStr(aRoot, aDocument) then //check if out of root :)
  begin
    Respond.HttpResult := hrError;
  end
  else if ((Request.Path = '') and not FileExists(aDocument)) or (not EndsDelimiter(aDocument) and DirectoryExists(aDocument)) then
  begin
    //http://127.0.0.1:81
    //http://127.0.0.1:81/
    //http://127.0.0.1:81/index.html
    //http://127.0.0.1:81/test/web

    //https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
    Request.Address := IncludeURLDelimiter(Request.Address);
    //Respond.SendHead('HTTP/1.1 301 Moved Permanently');
    Respond.HttpResult := hrFound;
    //Respond.SendHead('HTTP/1.1 307 Temporary Redirect');

    Respond.AddHeader('Location', Request.CollectURI);
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
  Respond.HttpResult := hrOK;
  Respond.SendHeader;
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
  aFileName := Request.Params.Values['FileName'];
  aFile := TFileStream.Create(Respond.Root + aFileName, fmCreate);
  try
    Respond.Stream.ReadStream(aFile, Request.ContentLength);
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
  aFilter := Request.Params.Values['Filter'];
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
var
  Key: string;
begin
  inherited;
  ParseQuery(Request.Query, Request.Params);

  //* https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers
  Key := Request.Header.ReadString('Sec-WebSocket-Key');
  if Key <> '' then
  begin
    //Key := HashWebSocketKey(Key);
    //Respond.AddHeader('Sec-WebSocket-Accept', Key);
  end;

  if SameText(Respond.Header.ReadString('Connection'), 'Keep-Alive') then
  begin
    Respond.KeepAlive := True;
    Respond.AddHeader('Connection', 'Keep-Alive');
    Respond.AddHeader('Keep-Alive', 'timout=' + IntToStr(Module.KeepAliveTimeOut div 5000) + ', max=100');
  end;

  if Module.UseCompressing then
  begin
    if Request.Header.Field['Accept-Encoding'].Have('gzip', [',']) then
      Respond.CompressClass := TmnGzipStreamProxy
    else if Request.Header.Field['Accept-Encoding'].Have('deflate', [',']) then
      Respond.CompressClass := TmnDeflateStreamProxy
    else
      Respond.CompressClass := nil;
    if Respond.CompressClass <> nil then
      Respond.AddHeader('Content-Encoding', Respond.CompressClass.GetCompressName);
  end;

  if (Request.Header.Field['Content-Length'].IsExists) then
    Request.ContentLength := Request.Header.Field['Content-Length'].AsInteger;
end;

procedure TmodHttpCommand.Unprepare(var Result: TmodRespondResult);
var
  aParams: TmnParams;
begin
  inherited;
  if not Respond.Header.Exists['Content-Length'] then
    Respond.KeepAlive := False;
  if Respond.KeepAlive and (Module.UseKeepAlive in [klvUndefined, klvKeepAlive]) and SameText(Request.Header.ReadString('Connection'), 'Keep-Alive') then
  begin
    Result.Timout := Module.KeepAliveTimeOut;
    if Request.Header.IsExists('Keep-Alive') then //idk if really sent from client
    begin
      aParams := TmnParams.Create;
      try
        //Keep-Alive: timeout=5, max=1000
        aParams.Separator := '=';
        aParams.Delimiter := ',';
        aParams.AsString := Request.Header['Keep-Alive'];
        Result.Timout := aParams['timeout'].AsInteger;
      finally
        aParams.Free;
      end;
    end;
    Result.Status := Result.Status + [mrKeepAlive];
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

{ THttpResultHelper }

function THttpResultHelper.ToString: string;
begin
  Result := 'HTTP/1.1 ';
  case Self of
    hrOK: Result := Result + '200 OK';
    hrError: Result := Result + '500 Internal Server Error';
    hrNotFound: Result := Result + '404 NotFound';
    hrMovedTemporarily: Result := Result + '307 Temporary Redirect';
    hrFound: Result := Result + '302 Found';
    hrSwitchingProtocols: Result := Result + '101 Switching Protocols';
    hrServiceUnavailable: Result := Result + '503 Service Unavailable';
  end;
end;

{$ifndef FPC}
{ TmodHttpEventCommand }

procedure TmodHttpEventCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  TmodWebEventModule(Module).FProc(Request, Respond, Result);
end;

{ TmodWebEventServer }

constructor TmodWebEventServer.Create(const vPort: string; vProc: TmodWebEventProc);
var
  aModule: TmodWebEventModule;
begin
  inherited Create;

  aModule := TmodWebEventModule.Create('web', 'doc', ['http/1.1'], Modules);
  aModule.FProc := vProc;

  Modules.DefaultModule := aModule;
  Port := vPort;
end;

{ TmodWebEventModule }

procedure TmodWebEventModule.DoCreateCommands;
begin
  // inherited;
  RegisterCommand('Event', TmodHttpEventCommand, true);
end;
{$endif FPC}

initialization
  modLock := TCriticalSection.Create;

finalization
  FreeAndNil(modLock);
end.
