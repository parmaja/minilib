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

  { TwebCommand }

  TSendFileDisposition = (sdDefault, sdInline, sdAttachment);

  TwebCommand = class abstract(TmodCommand)
  private
    function GetRespond: TwebRespond;
  protected
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure RespondResult(var Result: TmodRespondResult); override;
    procedure Unprepare(var Result: TmodRespondResult); override;

    function CreateRespond: TmodRespond; override;

    procedure RespondNotFound; virtual;

    //procedure SendFile(const vFile, vName: string; vDisposition: TSendFileDisposition = sdDefault); overload;
    //procedure SendFile(const vFile: string); overload;

  public
    destructor Destroy; override;
    property Respond: TwebRespond read GetRespond;
  end;

  TmodWebFileModule = class;

  { TwebFileCommand }

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class abstract(TmodModule)
  private
    FHomePath: string;
    FWorkPath: string;

    //FSmartURL: Boolean;
    procedure SetHomePath(AValue: string);
  protected
    procedure Created; override;
    procedure Started; override;

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
    function CheckRequest(const ARequest: string): Boolean; override;
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
  protected
    procedure RespondDocument(const vDocument: string; var Result: TmodRespondResult); virtual;
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
function HashWebSocketKey(const key: string): string;

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

//TODO slow function needs to improvements
//https://stackoverflow.com/questions/1549213/whats-the-correct-encoding-of-http-get-request-strings

{$ifdef FPC}
function EncodeBase64(const Buffer; Count: Integer): Utf8String;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  if Count=0 then
    Exit('');
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.Write(Buffer, Count);
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
    end;
end;
{$endif}

function HashWebSocketKey(const key: string): string;
var
{$ifdef FPC}
  b: TSHA1Digest;
{$else}
  b: TBytes;
{$endif}
begin
{$ifdef FPC}
  b := SHA1String(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
  Result := EncodeBase64(b, SizeOf(b));
{$else}
  b := THashSHA1.GetHashBytes(Utf8String(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
  Result := TNetEncoding.Base64String.EncodeBytesToString(b);
{$endif}
end;

{ TmodWebModule }

procedure TmodWebModule.SetHomePath(AValue: string);
begin
  if FHomePath = AValue then
	  exit;
  FHomePath := AValue;
end;

procedure TmodWebModule.Created;
begin
  inherited;
  UseKeepAlive := ovUndefined;
  UseCompressing := ovNo;
  UseWebSocket := True;
  FHomePath := '';
end;

procedure TmodWebModule.Started;
begin
  inherited;
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
  ARequest.Stream.WriteUTF8Line('HTTP/1.1 500 Internal Server Error');
  ARequest.Stream.WriteUTF8Line('');
  Handled := True;
end;

destructor TmodWebModule.Destroy;
begin
  inherited Destroy;
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


procedure TwebGetPostCommand.RespondDocument(const vDocument: string; var Result: TmodRespondResult);
begin
  Respond.SendFile(vDocument);
end;

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

  {$ifdef DEBUG}
  if (Request.Path='') and (Request.URI = '/favicon.ico') then
  begin
    RespondDocument('favicon.ico', Result);
    Exit;
  end;
  {$endif}

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
    //Respond.SendHead('HTTP/1.1 301 Moved Permanently');
    Respond.Answer := hrRedirect;
    //Respond.SendHead('HTTP/1.1 307 Temporary Redirect');
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

    RespondDocument(aDocument, Result);
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
  //TmodWebFileModule.Create('web', 'doc', ['http/1.1'], Modules);
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
    with TmodWebFileModule.Create(sAcmeNameFolder, '.' + sAcmeNameFolder, [], Modules) do
    begin
      Level := -1;
      HomePath := AHomePath;
    end;
    //* use certbot folder to "Application.Location + 'acme'" because certbot will create folder .well-known
  end;
end;

const
  sForwardHttps = 'ForwardHttps';

procedure TmodCustomWebServer.AddRedirectHttps;
begin
  if Modules.Find(sForwardHttps) = nil then
  begin
    with TmodForwardHttpsModule.Create(sForwardHttps, '', ['http/1.1'], Modules) do
    begin
        //Level := 0;
    end;
  end;
end;

{ TwebCommand }

function TwebCommand.GetRespond: TwebRespond;
begin
  Result := inherited Respond as TwebRespond;
end;

destructor TwebCommand.Destroy;
begin
  inherited;
end;

procedure TwebCommand.Prepare(var Result: TmodRespondResult);
var
  aKeepAlive: Boolean;
  WSHash, WSKey: string;
  SendHostHeader: Boolean;
begin
  inherited;

  if Request.Header.Field['Connection'].Have('Upgrade', [',']) then
  begin
    if Request.Use.WebSocket and Request.Header.Field['Upgrade'].Have('WebSocket', [',']) then
    begin
      if Request.Header['Sec-WebSocket-Version'].ToInteger = 13 then
      begin
        WSHash := Request.Header['Sec-WebSocket-Key'];
        SendHostHeader := Request.Header.ReadBool('X-Send-Server-Hostname', True);

        WSKey := HashWebSocketKey(WSHash);
        Respond.Answer := hrSwitchingProtocols;
        //Respond.AddHeader('Cache-Control', 'no-store, no-cache, must-revalidate, private');
        Respond.AddHeader('Connection', 'Upgrade');
        Respond.AddHeader('upgrade', 'websocket');
        Respond.AddHeader('date: ', FormatHTTPDate(Now));
        Respond.AddHeader('Sec-Websocket-Accept', WSKey);
        if Request.Header['Sec-WebSocket-Protocol'] = 'plain' then
          Respond.AddHeader('Sec-WebSocket-Protocol', 'plain');
        Respond.SendHeader;

        Respond.KeepAlive := True;
        Request.ProtcolClass := TmnWebSocket13StreamProxy;
        Request.ProtcolProxy := Request.ProtcolClass.Create;
        Request.ConnectionType := ctWebSocket;
        Result.Status := Result.Status + [mrKeepAlive];
        Request.Stream.AddProxy(Request.ProtcolProxy);

        if SendHostHeader then
          Respond.Stream.WriteUTF8String('Request served by miniWebModule');
        //* https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers
      end;
    end;
  end;

  if not Respond.IsHeaderSent and Request.KeepAlive then //not WebSocket
  begin
    Respond.KeepAlive := True;
    Respond.AddHeader('Connection', 'Keep-Alive');
    Respond.AddHeader('Keep-Alive', 'timout=' + IntToStr(Request.Use.KeepAliveTimeOut div 1000) + ', max=100');
  end;

  if Request.ConnectionType = ctWebSocket then
  begin
    Request.CompressProxy.Disable;
  end
  else
  begin
    if Request.Header.Field['Content-type'].Have('multipart/form-data', [';']) then
    begin
      Request.ConnectionType := ctFormData;
    end;
    {if not Respond.KeepAlive and (Request.Use.Compressing in [ovUndefined, ovYes]) then
    begin
      if Request.CompressProxy <> nil then
        Respond.AddHeader('Content-Encoding', Request.CompressProxy.GetCompressName);
    end;}

    //Compressing
    {if not Respond.KeepAlive and (UseCompressing in [ovUndefined, ovYes]) then
    begin
      if Request.Header.Field['Accept-Encoding'].Have('gzip', [',']) then
        CompressClass := TmnGzipStreamProxy
      else if Request.Header.Field['Accept-Encoding'].Have('deflate', [',']) then
        CompressClass := TmnDeflateStreamProxy
      else
        CompressClass := nil;
      if CompressClass <> nil then
        Respond.AddHeader('Content-Encoding', CompressClass.GetCompressName);
    end;}
  end;
end;

procedure TwebCommand.Unprepare(var Result: TmodRespondResult);
var
  aParams: TmnParams;
begin
  inherited;
  if Request.ConnectionType = ctWebSocket then
  begin
  end
  else
  begin
    // If no content length that mean we cant continue as keep alive, content length is recomended to keep the stream
    if not Respond.Header.Exists['Content-Length'] then //TODO see it Zaher,Belal
      Respond.KeepAlive := False;

    if Respond.KeepAlive then
    begin
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
      end
      else
        Result.Timout := Request.Use.KeepAliveTimeOut;

      Result.Status := Result.Status + [mrKeepAlive];
    end;

    Request.CompressProxy.Disable;
  end;
end;

procedure TwebCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
end;
(*
procedure TwebCommand.SendFile(const vFile, vName: string; vDisposition: TSendFileDisposition);
var
  aDocSize: Int64;
  aDocStream: TFileStream;
  aDate: TDateTime;
  aEtag, aFtag: string;
begin
  //TODO use Respond.SendFile
  if Respond.Stream.Connected then
  begin
    FileAge(vFile, aDate);
    aFtag := DateTimeToUnix(aDate).ToString;
    aEtag := Request.Header['If-None-Match'];
    if (aEtag<>'') and (aEtag = aFtag) then
    begin
      Respond.Answer := hrNotModified;
      Respond.SendHeader;
      //Log(vFile+': not modified');
      Exit;
    end;

    aDocStream := TFileStream.Create(vFile, fmOpenRead or fmShareDenyWrite);
    try
      {if Respond.KeepAlive then
        aDocSize := CompressSize(PByte(aDocStream.Memory), aDocStream.Size)
      else}
        aDocSize := aDocStream.Size;

      Respond.Answer := hrOK;

      //Respond.AddHeader('Cache-Control', 'max-age=600');
      Respond.AddHeader('Cache-Control', 'max-age=600');
      //Respond.AddHeader('Cache-Control', 'public');
      //Respond.AddHeader('Date', Now);
      Respond.AddHeader('Last-Modified', FormatHTTPDate(aDate));
      Respond.AddHeader('ETag', aFtag);
      if Respond.Stream.Connected then
      begin
        Respond.PutHeader('Content-Type', DocumentToContentType(vName));
        case vDisposition of
          sdInline: Respond.PutHeader('Content-Disposition', Format('inline; filename="%s"', [vName]));
          sdAttachment: Respond.PutHeader('Content-Disposition', Format('attachment; filename="%s"', [vName]));
          else;
        end;

        if Respond.KeepAlive then
          Respond.ContentLength := aDocSize;
      end;

      Respond.SendHeader;

      if Respond.Stream.Connected then
        Respond.Stream.WriteStream(aDocStream);
    finally
      aDocStream.Free;
    end;
  end;
end;
*)

procedure TwebCommand.RespondNotFound;
begin
  Respond.Answer := hrNotFound; //hrError
  Respond.ContentType := 'text/plain';
  Respond.Stream.WriteUTF8String('404 Not Found');
  Respond.KeepAlive := False;
end;

function TwebCommand.CreateRespond: TmodRespond;
begin
  Result := TwebRespond.Create(Request);
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

  aModule := TmodWebEventModule.Create('web', 'doc', ['http/1.1'], Modules);
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
  Respond.PutHeader('Allow', 'OPTIONS, GET, HEAD, POST');
  //PutHeader('Access-Control-Allow-Origin', 'origin');
//  PutHeader('Access-Control-Allow-Headers', 'Origin, Accept, Accept-  Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version, X-Response-Time, X-PINGOTHER, X-CSRF-Token,Authorization');
//  PutHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
  Respond.PutHeader('server', 'cserp.web.service/v1');
  Respond.PutHeader('Access-Control-Allow-Origin', '*');
  Respond.PutHeader('Access-Control-Allow-Method', 'POST');
  Respond.PutHeader('Access-Control-Allow-Headers', 'X-PINGOTHER, Content-Type');
end;

{ ThttpModules }

function TmodWebModules.CheckRequest(const ARequest: string): Boolean;
begin
  Result := Server.UseSSL or (ARequest[1]<>#$16);
end;

{ TmodHttpRequest }

function TmodWebModules.CreateRequest(Astream: TmnBufferStream): TmodRequest;
begin
  Result := TwebRequest.Create(nil, Astream);
end;

{ TmodHttpRequest }

initialization
  modLock := TCriticalSection.Create;
finalization
  FreeAndNil(modLock);
end.
