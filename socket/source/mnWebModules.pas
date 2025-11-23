unit mnWebModules;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}

{
            Userinfo       Host      Port                       URI (always started / )
            ┌──┴───┐ ┌──────┴────────┌┴┐┌────────────────────────┴───────────────────────────────┐
GET https://john.doe@www.example.com:123/username/forum/questions/?tag=networking&order=newest#top
                     └──────┬──────┘    └───────────────┬────────┘└───────────┬─────────────┘ └┬─┘
                       DomainName                    Address                Query           Fragment
                                        └───┬───┘└──┬──┘└──┬─────┘            ┬
                                        Directory Alias   Path              Params
    └────────────────────────┬─────────┘       Module Name
                           HomeURL
}

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

Notes:

  Last module without AliasName is the fallback module

*}

{**
  Ref: https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html
*}
interface

uses
  SysUtils, Classes, syncobjs, StrUtils,
  {$ifdef FPC}
  sha1, base64,
  {$else}
  NetEncoding, Hash,
  {$endif}
  DateUtils, mnLogs, mnBase64,
  mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnClasses, mnMultipartData, mnModules;

type

  TmodWebModule = class;

  TSendFileDisposition = (sdDefault, sdInline, sdAttachment);
  TmodServeFiles = set of (
    serveEnabled,
    serveIndex,
    serveSmart, //
    serveDefault
//    serveRender
  );

  TmodWebFileModule = class;

  { TwebFileCommand }

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class abstract(TmodModule)
  private
    FHomePath: string;
    FOrigins: TStrings;
    FWorkPath: string;

    //FSmartURL: Boolean;
    procedure SetHomePath(AValue: string);
    procedure SetOrigins(AValue: TStrings);
  protected
    procedure Created; override;
    procedure Started; override;
    procedure InitItems; override;

    procedure Log(S: string); override;
    procedure InternalError(ARequest: TmodRequest; var Handled: Boolean); override;
    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;
  public
    destructor Destroy; override;
    //property SmartURL: Boolean read FSmartURL write FSmartURL;
  public
    //protocol://domain:port/alias/directory
    //--------HOST URL------/alias/directory
    //----------HOME URL----------/directory
    Domain: string; //localhost
    Port: string;

    property Origins: TStrings read FOrigins write SetOrigins;
    //Public Path
    property HomePath: string read FHomePath write SetHomePath;
    //Private Path
    property WorkPath: string read FWorkPath write FWorkPath;
  end;
  { TmodWebFileModule }

  TmodWebFileModule = class(TmodWebModule)
  private
    FServeFiles: TmodServeFiles;
  protected
    FDefaultDocuments: TStringList;
    procedure SetDefaultDocuments(AValue: TStringList);
    procedure InitItems; override;
    procedure Created; override;
    procedure Started; override;
  public
    destructor Destroy; override;
    property DefaultDocuments: TStringList read FDefaultDocuments write SetDefaultDocuments;
    property ServeFiles: TmodServeFiles read FServeFiles write FServeFiles;
  end;

  TwebFileCommand = class(TwebCommand)
  private
    function GetModule: TmodWebFileModule;
  protected
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure RespondResult(var Result: TmodRespondResult); override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property Module: TmodWebFileModule read GetModule;
  end;

  TmodNotFoundModule = class(TmodWebModule)
  protected
    procedure InitItems; override;
  public
  end;

  { TmodRedirectModule }

  TmodRedirectModule = class(TmodWebModule)
  protected
    procedure InitItems; override;
  public
    RedirectTo: string;
  end;

  { TmodForwardHttpsModule }

  TmodForwardHttpsModule = class(TmodWebModule)
  protected
    procedure InitItems; override;
  public
  end;

  { TmodWebModules }

  TmodWebModules = class(TmodModules)
  protected
    function CreateRequest(Astream: TmnBufferStream): TmodRequest; override;
  public
    procedure ParseHead(ARequest: TmodRequest; const RequestLine: string); override;
  end;

  { TmodCustomWebServer }

  TmodCustomWebServer = class(TmodModuleServer)
  protected
    function CreateModules: TmodModules; override;
  public
    constructor Create; override;
    procedure AddChallengeAcme(const AHomePath: string);
    procedure AddFileModule(const Alias: string; const AHomePath: string);
    procedure AddRedirectHttps;
    procedure SetFallbackRedirect(ToLocation: string);
    procedure SetNotfound;
  end;

  //*****************************************

  TmodWebServer = class(TmodCustomWebServer)
  protected
    procedure Created; override;
  end;

  {$ifndef FPC}
  TmodWebEventProc = reference to procedure(vRequest: TmodRequest; vResponse: TwebResponse; var vResult: TmodRespondResult);

  TmodWebEventModule = class(TmodWebModule)
  protected
    FProc: TmodWebEventProc; //need discuss
    procedure InitItems; override;
  end;

  TmodWebEventServer = class(TmodCustomWebServer)
  public
    constructor Create(const vPort: string; vProc: TmodWebEventProc); reintroduce;
  end;

  {**
    Files Commands
  *}

  { TmodHttpEventCommand }

  TmodHttpEventCommand = class(TwebFileCommand)
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  {$endif}

  { TmodHttpGetPostCommand }

  TwebGetPostCommand = class(TwebFileCommand)
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  //Handle cors :)

  TwebOptionCommand = class(TwebGetPostCommand)
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TwebPutCommand }

  TwebPutCommand = class(TwebFileCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodRedirectCommand }

  TmodRedirectCommand = class(TwebCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  Tmod404Command = class(TwebCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodForwardHttpsCommand }

  TmodForwardHttpsCommand = class(TwebCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { TmodServerInfoCommand }

  TmodServerInfoCommand = class(TwebFileCommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

  { TmodDirCommand }

  TmodDirCommand = class(TwebFileCommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

  { TmodDeleteFileCommand }

  TmodDeleteFileCommand = class(TwebFileCommand)
  protected
    procedure RespondResult(var Result: TmodRespondResult); override;
  public
  end;

  { TWebServerItem }

  TWebServerItem = class(TmnNamedObject)
  private
    FOwnIt: Boolean;
    FServer: TmodCustomWebServer;
    function GetStarted: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TmodCustomWebServer read FServer;
    property Started: Boolean read GetStarted;
  end;

  { TWebServers }

  TWebServers = class(TmnNamedObjectList<TWebServerItem>)
  private
    FStarted: Boolean;
  public
    function AddServer(AName: string; AServer: TmodCustomWebServer; OwnIt: Boolean = True): Integer;
    procedure Start;
    procedure Stop;
    property Started: Boolean read FStarted;
  end;

var
  modLock: TCriticalSection = nil;

function WebFindDocument(const HomePath, Path: string; out Document:string; Smart: Boolean = False): Boolean;
function WebExpandFile(HomePath, Path: string; out Document: string; Smart: Boolean): Boolean;
function WebExpandToRoot(FileName: string; Root: string): string;
function FindDefaultDocument(Root: string; DefaultDocuments: TStringList): string;
procedure WebServeFolder(Title, Path: string; Response: TwebResponse; Request: TmodRequest);
procedure WebServeFile(Response: TwebResponse; Request: TmodRequest; DefaultDocuments: TStringList; Options: TmodServeFiles);

function WebServers: TWebServers;

implementation

uses
  mnMIME;

var
  FWebServers: TWebServers = nil;

function WebServers: TWebServers;
begin
  if FWebServers = nil then
    FWebServers := TWebServers.Create;
  Result := FWebServers;
end;

function WebFindDocument(const HomePath, Path: string; out Document:string; Smart: Boolean = False): Boolean;
begin
  Document := HomePath;
  // path = '' or '/' or './' or '../'
  if not ((Path = '') or StartsDelimiter(Path) or StartsStr('./', Path) or StartsStr('../', Path)) then //* some file or folder names starts with . like '.well-known/acme-challenge/'
    Document := IncludePathDelimiter(Document);
  Document := ExpandFile(Document + Path);

  Result := FileExists(Document) or DirectoryExists(Document);
  if Smart and not Result and (Path <> '') then
    Result := WebFindDocument(HomePath, TruncPath(Path, -1), Document, Smart);
end;

function WebExpandFile(HomePath, Path: string; out Document: string; Smart: Boolean): Boolean;
begin
  HomePath := ExcludePathDelimiter(ExpandFile(CorrectPath(HomePath)));
  Result := WebFindDocument(HomePath, CorrectPath(Path), Document, Smart);
  Result := Result and StartsStr(HomePath, Document); //check if out of root :)
end;

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

function FindDefaultDocument(Root: string; DefaultDocuments: TStringList): string;
var
  i: Integer;
  aFile: string;
begin
  if DefaultDocuments = nil then
    exit(Root);
  //TODO baaad you need to lock before access
  Root := IncludePathDelimiter(Root);
  for i := 0 to DefaultDocuments.Count - 1 do
  begin
    aFile := Root + DefaultDocuments[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
      Exit;
    end;
  end;

  if DefaultDocuments.Count <> 0 then
    Result := Root + DefaultDocuments[0]
  else
    Result := Root;
end;

{ TmodWebModule }

procedure TmodWebModule.SetHomePath(AValue: string);
begin
  if FHomePath = AValue then
	  exit;
  FHomePath := AValue;
end;

procedure TmodWebModule.SetOrigins(AValue: TStrings);
begin
  if FOrigins=AValue then Exit;
  FOrigins.Assign(AValue);
end;

procedure TmodWebModule.Created;
begin
  inherited;
  UseKeepAlive := ovUndefined;
  UseCompressing := ovNo;
  UseWebSocket := True;
  FHomePath := '';
  FOrigins := TStringList.Create;
end;

procedure TmodWebModule.Started;
begin
  inherited;
end;

procedure TmodWebModule.InitItems;
begin
  inherited;
  Protocols := [sHTTPProtocol_100, sHTTPProtocol_101];
end;

procedure TmodWebModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  //inherited;
  ARequest.Command := ARequest.Method;
  if (AliasName <> '') then
  begin
    ARequest.Path := DeleteSubPath(ARequest.Route[0], ARequest.Path);
  end;
end;

procedure TmodWebModule.DoMatch(const ARequest: TmodRequest; var vMatch: Boolean);
begin
  //inherited;
  vMatch := ARequest.Route[0] = AliasName;
end;

procedure TmodWebModule.InternalError(ARequest: TmodRequest; var Handled: Boolean);
begin
  inherited;
  ARequest.Stream.WriteUTF8Line(sHTTPProtocol_101 + ' 500 Internal Server Error');
  ARequest.Stream.WriteUTF8Line('');
  Handled := True;
end;

destructor TmodWebModule.Destroy;
begin
  FreeAndNil(FOrigins);
  inherited;
end;

procedure TmodWebModule.Log(S: string);
begin
  inherited;
  Modules.Log(S);
end;

{ TmodWebFileModule }

procedure TmodWebFileModule.SetDefaultDocuments(AValue: TStringList);
begin
  FDefaultDocuments.Assign(AValue);
end;

procedure TmodWebFileModule.InitItems;
begin
  inherited;
  RegisterCommand('GET', TwebGetPostCommand, True);
  RegisterCommand('POST', TwebGetPostCommand);
  RegisterCommand('INFO', TmodServerInfoCommand);
end;

procedure TmodWebFileModule.Created;
begin
  inherited;
  FDefaultDocuments := TStringList.Create;
  FDefaultDocuments.Add('index.html');
  FDefaultDocuments.Add('index.htm');
  FDefaultDocuments.Add('default.html');
  FDefaultDocuments.Add('default.htm');
  ServeFiles:= [serveEnabled, serveIndex, serveDefault, serveSmart];
end;

procedure TmodWebFileModule.Started;
begin
  inherited;
  if HomePath = '' then
    raise Exception.Create('Home path not set!');
end;

destructor TmodWebFileModule.Destroy;
begin
  FreeAndNil(FDefaultDocuments);
  inherited;
end;

{ TmodForwardHttpsModule }

procedure TmodForwardHttpsModule.InitItems;
begin
  inherited;
  RegisterCommand('', TmodForwardHttpsCommand, True);
end;

{ TwebFileCommand }

function TwebFileCommand.GetModule: TmodWebFileModule;
begin
  Result := (inherited Module) as TmodWebFileModule;
end;

procedure TwebFileCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
end;

procedure TwebFileCommand.Prepare(var Result: TmodRespondResult);
begin
  inherited;
  Response.HomePath := Module.HomePath;
end;

procedure TwebFileCommand.Created;
begin
  inherited Created;
end;

{ TwebGetPostCommand }

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

procedure WebServeFolder(Title, Path: string; Response: TwebResponse; Request: TmodRequest);
var
  Files: TStringList;
  s: string;
begin
  Response.ContentType := DocumentToContentType('html');
  Files := TStringList.Create;
  try
    Response.Stream.WriteUTF8Line('<!DOCTYPE html>');
    Response.Stream.WriteUTF8Line('<html>');
    Response.Stream.WriteUTF8Line('<head>');
    Response.Stream.WriteUTF8Line('<title> Index of ' + Title + '</title>');
    Response.Stream.WriteUTF8Line('<style> body { font-family: monospace; } </style>');
    Response.Stream.WriteUTF8Line('</head>');
    Response.Stream.WriteUTF8Line('<body>');
    EnumFiles(Files, Path, '*.*', [efDirectory]);
    Response.Stream.WriteUTF8Line('<h1> Index of ' + Title + '</h1>');
    Response.Stream.WriteUTF8Line('<h2>Folders</h2>');
    Response.Stream.WriteUTF8Line('<ul>');
    for s in Files do
    begin
      if not StartsText('.', s) then
      begin
        Response.Stream.WriteUTF8Line('<ui>');
        Response.Stream.WriteUTF8Line('<a href="' + s + '\">' + s + '</a>');
        Response.Stream.WriteUTF8Line('<br/>');
        Response.Stream.WriteUTF8Line('</ui>');
      end;
    end;
    Response.Stream.WriteUTF8Line('</ul>');
    Response.Stream.WriteUTF8Line('<h2>Files</h2>');
    Files.Clear;
    EnumFiles(Files, Path, '*.*', [efFile]);
    Response.Stream.WriteUTF8Line('<ul>');
    for s in Files do
    begin
      if not StartsText('.', s) then
      begin
        Response.Stream.WriteUTF8Line('<ui>');
        Response.Stream.WriteUTF8Line('<a href="' + s + '">'+ s + '</a>');
        Response.Stream.WriteUTF8Line('<br/>');
        Response.Stream.WriteUTF8Line('</ui>');
      end;
    end;
    Response.Stream.WriteUTF8Line('</ul>');
    Response.Stream.WriteUTF8Line('</body>');
    Response.Stream.WriteUTF8Line('</html>');
  finally
    Files.Free;
  end;
end;

procedure WebServeFile(Response: TwebResponse; Request: TmodRequest; DefaultDocuments: TStringList; Options: TmodServeFiles);
var
  aDocument, aFile, aHomePath: string;

  {function FindDocument(Path: string; Smart: Boolean = False): Boolean;
  begin
    if Path = '' then
      exit(False);

    aPath := Path;
    // path = '' or '/' or './' or '../'
    if not ((aPath = '') or StartsDelimiter(aPath) or StartsStr('./', aPath) or StartsStr('../', aPath)) then //* some file or folder names starts with . like '.well-known/acme-challenge/'
      aDocument := IncludePathDelimiter(aHomePath);
    aDocument := ExpandFile(aDocument + aPath);

    Result := FileExists(aDocument) or DirectoryExists(aDocument);
    if Smart and not Result then
      Result := FindDocument(TruncPath(Path, -1), Smart);
  end;}
begin

(*

  '/web'               path = ''
  '/web/'              path = '/'
  '/web/dashbord'     path = '/dashbord' is dir
  '/web/dashbord/'    path = '/dashbord' is dir
  '/web/dashbord/index' path = '/dashbord/index' is not dir
  '/web/dashbord/index.html' file

*)
  aHomePath := ExpandFile(CorrectPath(ExcludePathDelimiter(Response.HomePath)));

  if not WebExpandFile(aHomePath, Request.Path, aDocument, serveSmart in Options) then
    Response.Answer := hrUnauthorized
  else if ((Request.Path = '') and not FileExists(aDocument)) or (not EndsDelimiter(aDocument) and DirectoryExists(aDocument)) then
  //                                                                  http://127.0.0.1:81/web  to   http://127.0.0.1:81/web/
  begin
    //http://127.0.0.1:81
    //http://127.0.0.1:81/
    //http://127.0.0.1:81/index.html
    //http://127.0.0.1:81/test/web

    //https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
    //Request.Address := IncludeURLDelimiter(Request.Address);
    //Response.SendHead(sHTTPProtocol1 + ' 301 Moved Permanently');
    Response.Answer := hrRedirect;
    //Response.SendHead(sHTTPProtocol1 + ' 307 Temporary Redirect');
    Response.Location := IncludeURLDelimiter(Request.Address);
    Response.SendHeader;
  end
  else
  begin
    if (serveDefault in Options) and EndsDelimiter(aDocument) {and DirectoryExists(aDocument)} then
    begin
      aFile := FindDefaultDocument(aDocument, DefaultDocuments);
      if FileExists(aFile) then
        aDocument := aFile;
    end;

    if (serveIndex in Options) and EndsDelimiter(aDocument) then
    begin
      Response.Answer := hrOK;
      Response.ContentType := 'text/html';
      WebServeFolder(Request.Address, aDocument, Response, Request)
    end
    else
    begin
      if StartsText('.', ExtractFileName(aDocument)) then //no files starts with dots, TODO no folders in path
        Response.Answer := hrForbidden
      else if FileExists(aDocument) then
        Response.SendFile(aDocument, Request.Header['If-None-Match'])
      else
        Response.Answer := hrNotFound;
    end;
  end;
end;

procedure TwebGetPostCommand.RespondResult(var Result: TmodRespondResult);
begin
  WebServeFile(Response, Request, Module.DefaultDocuments, Module.ServeFiles);
  inherited;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Response.Answer := hrOK;
  Response.SendHeader;
  //Response.Stream.WriteLine('Server is running on port: ' + Module.Server.Port);
  Response.Stream.WriteLine(Utf8String('the server is: "' + ParamStr(0) + '"'));
end;

{ TwebPutCommand }

procedure TwebPutCommand.RespondResult(var Result: TmodRespondResult);
var
  aFile: TFileStream;
  aFileName: string;
begin
  inherited;
  Response.Stream.WriteCommand('OK');
  aFileName := Request.Params.Values['FileName'];
  aFile := TFileStream.Create(Response.HomePath + aFileName, fmCreate);
  try
    Response.Stream.ReadStream(aFile, Request.ContentLength);
  finally
    aFile.Free;
  end;
end;

{ TmodRedirectCommand }

procedure TmodRedirectCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Response.RedirectTo((Module as TmodRedirectModule).RedirectTo);
end;

{ TmodForwardHttpsCommand }

procedure TmodForwardHttpsCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Response.RedirectTo('https://'+Response.Request.Host + Response.Request.URI);
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
  Response.Stream.WriteCommand('OK');
  aFilter := Request.Params.Values['Filter'];
  //aPath := IncludeTrailingPathDelimiter(Root);
  if aFilter = '' then
    aFilter := '*.*';
   aStrings := TStringList.Create;
  try
    //EnumFileList(aPath + aFilter, aStrings);
    for i := 0 to aStrings.Count - 1 do
    begin
      Response.Stream.WriteLine(IntToStr(i) + ': ' + aStrings[i]);
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
  aFileName := IncludeTrailingPathDelimiter(Response.HomePath) + Request.Path;
  if FileExists(aFileName) then
    DeleteFile(aFileName);
  Response.Stream.WriteCommand('OK');
end;

{ TWebServerItem }

function TWebServerItem.GetStarted: Boolean;
begin
  Result := (Server <> nil) and (Server.Started);
end;

constructor TWebServerItem.Create;
begin
  inherited;
end;

destructor TWebServerItem.Destroy;
begin
  if FOwnIt then
    FreeAndNil(FServer);
  inherited;
end;

procedure TWebServerItem.Start;
begin
  if Server <> nil then
  begin
    Server.Start;
  end;
end;

procedure TWebServerItem.Stop;
begin
  if Server <> nil then
  begin
    Server.Stop;
  end;
end;

{ TWebServers }

function TWebServers.AddServer(AName: string; AServer: TmodCustomWebServer;
  OwnIt: Boolean): Integer;
var
  item: TWebServerItem;
begin
  item := TWebServerItem.Create;;
  item.Name := AName;
  item.FServer := AServer;
  item.FOwnIt := OwnIt;
  Result := Add(item);
end;

procedure TWebServers.Start;
var
  item: TWebServerItem;
begin
  for item in Self do
  begin
    if (item.Server <> nil) and item.Server.Enabled then
      item.Start;
  end;
  FStarted := True;
end;

procedure TWebServers.Stop;
var
  item: TWebServerItem;
begin
  for item in Self do
  begin
    if (item.Server <> nil) and item.Server.Started then
      item.Stop;
  end;
  FStarted := False;
end;

{ TwebFileCommand }

destructor TwebFileCommand.Destroy;
begin
  inherited;
end;

{ TmodRedirectModule }

procedure TmodRedirectModule.InitItems;
begin
  inherited;
  RegisterCommand('', TmodRedirectCommand, True);
end;

{ TmodWebServer }

procedure TmodWebServer.Created;
begin
  inherited;
end;

{ TmodAcmeChallengeServer }

function TmodCustomWebServer.CreateModules: TmodModules;
begin
  Result := TmodWebModules.Create(Self);
end;

constructor TmodCustomWebServer.Create;
begin
  inherited;
  Port := '80';
end;

const
  sAcmeNameFolder = 'well-known';

procedure TmodCustomWebServer.AddChallengeAcme(const AHomePath: string);
begin
  if Modules.Find(sAcmeNameFolder) = nil then
  begin
    //* http://localhost/.well-known/acme-challenge/index.html
    with TmodWebFileModule.Create(Modules, sAcmeNameFolder, '.' + sAcmeNameFolder) do
    begin
      Level := -1;
      HomePath := AHomePath;
    end;
    //* use certbot folder to "Application.Location + 'acme'" because certbot will create folder .well-known
  end;
end;

const
  sForwardHttps = 'ForwardHttps';

procedure TmodCustomWebServer.AddFileModule(const Alias: string; const AHomePath: string);
begin
  if Modules.Find(Alias) = nil then
  begin
    with TmodWebFileModule.Create(Modules, Alias, Alias) do
    begin
      Level := -1;
      HomePath := AHomePath;
    end;
  end;
end;

procedure TmodCustomWebServer.AddRedirectHttps;
begin
  if Modules.Find(sForwardHttps) = nil then
  begin
    with TmodForwardHttpsModule.Create(Modules, sForwardHttps) do
    begin
        //Level := 0;
    end;
  end;
end;

procedure TmodCustomWebServer.SetFallbackRedirect(ToLocation: string);
var
  aModule: TmodRedirectModule;
begin
  aModule := Modules.Find<TmodRedirectModule>;
  if aModule = nil then
    aModule := TmodRedirectModule.Create(Modules, 'redirect');
  aModule.RedirectTo := ToLocation;
end;

procedure TmodCustomWebServer.SetNotfound;
var
  aModule: TmodNotFoundModule;
begin
  aModule := Modules.Find<TmodNotFoundModule>;
  if aModule = nil then
    aModule := TmodNotFoundModule.Create(Modules, 'notfound');
end;

{ TmodCustomWebModules }

procedure TmodWebModules.ParseHead(ARequest: TmodRequest; const RequestLine: string);
begin
  inherited;
  //ARequest.ParsePath(ARequest.URI); duplicate in parse head :)
  ARequest.Command := ARequest.Method;
end;

{$ifndef FPC}

{ TmodHttpEventCommand }

procedure TmodHttpEventCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  TmodWebEventModule(Module).FProc(Request, Response, Result);
end;

{ TmodWebEventServer }

constructor TmodWebEventServer.Create(const vPort: string; vProc: TmodWebEventProc);
var
  aModule: TmodWebEventModule;
begin
  inherited Create;

  aModule := TmodWebEventModule.Create(Modules, 'web', 'doc');
  aModule.FProc := vProc;

  Port := vPort;
end;

{ TmodWebEventModule }

procedure TmodWebEventModule.InitItems;
begin
  // inherited;
  RegisterCommand('Event', TmodHttpEventCommand, true);
end;
{$endif FPC}

{ TwebOptionCommand }

procedure TwebOptionCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;

  Response.Answer := hrOK;
  Response.PutHeader('server', sMiniLibServer);
  Response.PutHeader('Allow', 'GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS');
//  PutHeader('Access-Control-Allow-Headers', 'Origin, Accept, Accept-  Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version, X-Response-Time, X-PINGOTHER, X-CSRF-Token,Authorization');
//  PutHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
  Response.PutHeader('Access-Control-Allow-Method', 'GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS');
  Response.PutHeader('Access-Control-Allow-Headers', 'X-PINGOTHER, Content-Type, Authorization, Accept, Origin');
  if (Module.Origins.Count = 0) then
    Response.PutHeader('Access-Control-Allow-Origin', '*')
  else
    Response.PutHeader('Access-Control-Allow-Origin', Module.Origins.CommaText);
end;

function TmodWebModules.CreateRequest(Astream: TmnBufferStream): TmodRequest;
begin
  Result := TwebRequest.Create(nil, Astream);
end;

{ Tmod404Command }

procedure Tmod404Command.RespondResult(var Result: TmodRespondResult);
begin
  Response.Answer := hrNotFound;
  Response.ContentType := 'text/html';
  Response.SendUTF8String('404 Not Found');
end;

{ TmodNotFoundModule }

procedure TmodNotFoundModule.InitItems;
begin
  inherited;
  RegisterCommand('', Tmod404Command, True);
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  FreeAndNil(modLock);
end.
