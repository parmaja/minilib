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
    procedure CreateItems; override;

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
  protected
    FDefaultDocument: TStringList;
    procedure SetDefaultDocument(AValue: TStringList);
    procedure DoRegisterCommands; override;
    procedure Created; override;
    procedure Started; override;
  public
    destructor Destroy; override;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  TwebFileCommand = class(TwebCommand)
  private
    function GetModule: TmodWebFileModule;
  protected
    function GetDefaultDocument(vRoot: string): string;
    procedure RespondResult(var Result: TmodRespondResult); override;
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property Module: TmodWebFileModule read GetModule;
  end;

  { TmodForwardHttpsModule }

  TmodForwardHttpsModule = class(TmodWebModule)
  protected
    procedure DoRegisterCommands; override;
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
  end;

  //*****************************************

  TmodWebServer = class(TmodCustomWebServer)
  protected
    procedure Created; override;
  end;

  {$ifndef FPC}
  TmodWebEventProc = reference to procedure(vRequest: TmodRequest; vRespond: TwebRespond; var vResult: TmodRespondResult);

  TmodWebEventModule = class(TmodWebModule)
  protected
    FProc: TmodWebEventProc; //need discuss
    procedure DoRegisterCommands; override;
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

function WebExpandFile(HomePath, Path: string; out Document: string): Boolean;
function WebExpandToRoot(FileName: string; Root: string): string;

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

function WebExpandFile(HomePath, Path: string; out Document: string): Boolean;
begin
  HomePath := ExcludePathDelimiter(HomePath);

  if (Path = '') or StartsDelimiter(Path) or StartsStr('./', Path) or StartsStr('../', Path) then //* some file or folder names starts with . like '.well-known/acme-challenge/'
    Document := HomePath + Path
  else
    Document := IncludePathDelimiter(HomePath) + Path;

  HomePath := CorrectPath(HomePath);
  Document := CorrectPath(Document);

  HomePath := ExpandFile(HomePath);
  Document := ExpandFile(Document);

  if not StartsStr(HomePath, Document) then //check if out of root :)
    Result := False
  else if ((Path = '') and not FileExists(Document)) or (not EndsDelimiter(Document) and DirectoryExists(Document)) then
    Result := False
  else
    Result := True;
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

procedure TmodWebModule.CreateItems;
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

procedure TmodWebFileModule.SetDefaultDocument(AValue: TStringList);
begin
  FDefaultDocument.Assign(AValue);
end;

procedure TmodWebFileModule.DoRegisterCommands;
begin
  inherited;
  RegisterCommand('GET', TwebGetPostCommand, True);
  RegisterCommand('POST', TwebGetPostCommand);
  RegisterCommand('INFO', TmodServerInfoCommand);
end;

procedure TmodWebFileModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
end;

procedure TmodWebFileModule.Started;
begin
  inherited;
  if HomePath = '' then
    raise Exception.Create('Home path not set!');
end;

destructor TmodWebFileModule.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

{ TmodForwardHttpsModule }

procedure TmodForwardHttpsModule.DoRegisterCommands;
begin
  inherited;
  RegisterCommand('', TmodForwardHttpsCommand, True);
end;

{ TwebFileCommand }

function TwebFileCommand.GetModule: TmodWebFileModule;
begin
  Result := (inherited Module) as TmodWebFileModule;
end;

function TwebFileCommand.GetDefaultDocument(vRoot: string): string;
var
  i: Integer;
  aFile: string;
begin
  //TODO baaad you need to lock before access
  vRoot := IncludePathDelimiter(vRoot);
  for i := 0 to Module.DefaultDocument.Count - 1 do
  begin
    aFile := vRoot + Module.DefaultDocument[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
      Exit;
    end;
  end;

  if Module.DefaultDocument.Count<>0 then
    Result := vRoot + Module.DefaultDocument[0]
  else
    Result := vRoot;
end;

procedure TwebFileCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
end;

procedure TwebFileCommand.Prepare(var Result: TmodRespondResult);
begin
  inherited;
  Respond.HomePath := Module.HomePath;
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

procedure TwebGetPostCommand.RespondResult(var Result: TmodRespondResult);
var
  aDocument, aHomePath: string;
  {aPath, aFile: string;}
  aDefault: Boolean;
begin

(*

  '/web'               path = ''
  '/web/'              path = '/'
  '/web/dashbord'     path = '/dashbord' is dir
  '/web/dashbord/'    path = '/dashbord' is dir
  '/web/dashbord/index' path = '/dashbord/index' is not dir
  '/web/dashbord/index.html' file

*)

  aHomePath := ExcludePathDelimiter(Respond.HomePath);

  //if (Request.Path = '') or StartsDelimiter(Request.Path) or StartsStr('.', Request.Path) then
  if (Request.Path = '') or StartsDelimiter(Request.Path) or StartsStr('./', Request.Path) or StartsStr('../', Request.Path) then //* some file or folder names starts with . like '.well-known/acme-challenge/'
    aDocument := aHomePath + Request.Path
  else
    aDocument := IncludePathDelimiter(aHomePath) + Request.Path;

  aHomePath := CorrectPath(aHomePath);
  aDocument := CorrectPath(aDocument);

  aHomePath := ExpandFile(aHomePath);
  aDocument := ExpandFile(aDocument);

  if EndsDelimiter(aDocument) then //get the default file if it not defined
  begin
    aDocument := GetDefaultDocument(aDocument);
    aDefault := True;
  end
  else
    aDefault := False;


  if not StartsStr(aHomePath, aDocument) then //check if out of root :)
  begin
    Respond.Answer := hrError;
  end
  else if ((Request.Path = '') and not FileExists(aDocument)) or (not EndsDelimiter(aDocument) and DirectoryExists(aDocument)) then
  begin
    //http://127.0.0.1:81
    //http://127.0.0.1:81/
    //http://127.0.0.1:81/index.html
    //http://127.0.0.1:81/test/web

    //https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
    //Request.Address := IncludeURLDelimiter(Request.Address);
    //Respond.SendHead(sHTTPProtocol1 + ' 301 Moved Permanently');
    Respond.Answer := hrRedirect;
    //Respond.SendHead(sHTTPProtocol1 + ' 307 Temporary Redirect');
    Respond.Location := IncludeURLDelimiter(Request.Address);
    Respond.SendHeader;
  end
  else
  begin
    {if Module.SmartURL then //* Endless loop
    begin

      repeat
        if FileExists(aDocument) then
          Break;

        //aFile := ExtractFileName(aDocument);
        //aPath := ExtractFilePath(aDocument);

        if aDefault then
        begin
          aPath := ExtractFilePath(aDocument);
          aPath := ExtractFilePath(aPath)
        end
        else
        begin
          aPath := ExtractFilePath(aDocument);
          aDefault := True;
        end;

        aDocument := GetDefaultDocument(aPath);

      until (aPath='') or SameText(aPath, aHomePath);
    end;}

    Respond.SendFile(aDocument);
  end;
  inherited;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Respond.Answer := hrOK;
  Respond.SendHeader;
  //Respond.Stream.WriteLine('Server is running on port: ' + Module.Server.Port);
  Respond.Stream.WriteLine(Utf8String('the server is: "' + ParamStr(0) + '"'));
end;

{ TwebPutCommand }

procedure TwebPutCommand.RespondResult(var Result: TmodRespondResult);
var
  aFile: TFileStream;
  aFileName: string;
begin
  inherited;
  Respond.Stream.WriteCommand('OK');
  aFileName := Request.Params.Values['FileName'];
  aFile := TFileStream.Create(Respond.HomePath + aFileName, fmCreate);
  try
    Respond.Stream.ReadStream(aFile, Request.ContentLength);
  finally
    aFile.Free;
  end;
end;

{ TmodForwardHttpsCommand }

procedure TmodForwardHttpsCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Respond.Answer := hrRedirect;
  Respond.Location := 'https://'+Respond.Request.Host + Respond.Request.URI;
  Respond.SendHeader;
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
  aFileName := IncludeTrailingPathDelimiter(Respond.HomePath) + Request.Path;
  if FileExists(aFileName) then
    DeleteFile(aFileName);
  Respond.Stream.WriteCommand('OK');
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
    Server.Start;
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

{ TmodWebServer }

procedure TmodWebServer.Created;
begin
  inherited;
  //TmodWebFileModule.Create('web', 'doc', [sHTTPProtocol1], Modules);
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
    with TmodWebFileModule.Create(sAcmeNameFolder, '.' + sAcmeNameFolder, Modules) do
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
    with TmodWebFileModule.Create(Alias, Alias, Modules) do
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
    with TmodForwardHttpsModule.Create(sForwardHttps, '', Modules) do
    begin
        //Level := 0;
    end;
  end;
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
  TmodWebEventModule(Module).FProc(Request, Respond, Result);
end;

{ TmodWebEventServer }

constructor TmodWebEventServer.Create(const vPort: string; vProc: TmodWebEventProc);
var
  aModule: TmodWebEventModule;
begin
  inherited Create;

  aModule := TmodWebEventModule.Create('web', 'doc', Modules);
  aModule.FProc := vProc;

  Port := vPort;
end;

{ TmodWebEventModule }

procedure TmodWebEventModule.DoRegisterCommands;
begin
  // inherited;
  RegisterCommand('Event', TmodHttpEventCommand, true);
end;
{$endif FPC}

{ TwebOptionCommand }

procedure TwebOptionCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;

  Respond.Answer := hrOK;
  Respond.PutHeader('server', sMiniLibServer);
  Respond.PutHeader('Allow', 'GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS');
//  PutHeader('Access-Control-Allow-Headers', 'Origin, Accept, Accept-  Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version, X-Response-Time, X-PINGOTHER, X-CSRF-Token,Authorization');
//  PutHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
  Respond.PutHeader('Access-Control-Allow-Method', 'GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS');
  Respond.PutHeader('Access-Control-Allow-Headers', 'X-PINGOTHER, Content-Type, Authorization, Accept, Origin');
  if (Module.Origins.Count = 0) then
    Respond.PutHeader('Access-Control-Allow-Origin', '*')
  else
    Respond.PutHeader('Access-Control-Allow-Origin', Module.Origins.CommaText);
end;

function TmodWebModules.CreateRequest(Astream: TmnBufferStream): TmodRequest;
begin
  Result := TwebRequest.Create(nil, Astream);
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  FreeAndNil(modLock);
end.
