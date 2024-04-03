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
  DateUtils, mnLogs,
  mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnMultipartData, mnModules;

type

  TmodWebModule = class;
  THttpResult = (
    hrNone,
    hrOK,
    hrUnauthorized,
    hrError,
    hrFound, //302
    hrNotModified,
    hrMovedTemporarily, //307
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
    FWebSocket: Boolean;
    FKeepAlive: Boolean;
    FContentLength: Integer;
    FCompressClass: TmnCompressStreamProxyClass;
    FCompressProxy: TmnCompressStreamProxy;
    FProtcolClass: TmnProtcolStreamProxyClass;
    FProtcolProxy: TmnProtcolStreamProxy;
    FHomePath: string; //Document root folder
    FHostURL: string;
    FHttpResult: THttpResult;
    procedure SetCompressClass(AValue: TmnCompressStreamProxyClass);
    procedure SetsCompressProxy(AValue: TmnCompressStreamProxy);
    procedure SetHttpResult(const Value: THttpResult);
    procedure SetProtcolClass(const Value: TmnProtcolStreamProxyClass);
  protected
    function HeadText: string; override;
    procedure DoHeaderSent; override;
  public
    constructor Create;
    property WebSocket: Boolean read FWebSocket write FWebSocket;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    //Compress on the fly, now we use deflate
    property ContentLength: Integer read FContentLength write FContentLength;
    property CompressClass: TmnCompressStreamProxyClass read FCompressClass write SetCompressClass;
    property CompressProxy: TmnCompressStreamProxy read FCompressProxy write SetsCompressProxy;
    //WebSocket
    property ProtcolClass: TmnProtcolStreamProxyClass read FProtcolClass write SetProtcolClass;
    property ProtcolProxy: TmnProtcolStreamProxy read FProtcolProxy;

    property HttpResult: THttpResult read FHttpResult write SetHttpResult;
    //Document root folder
    property HomePath: string read FHomePath;
    property HostURL: string read FHostURL;
  end;

  { TmodHttpCommand }

  TSendFileDisposition = (sdDefault, sdInline, sdAttachment);

  TmodHttpCommand = class abstract(TmodCommand)
  private
    function GetRespond: TmodHttpRespond;
  protected
    procedure Created; override;
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure Unprepare(var Result: TmodRespondResult); override;
    procedure RespondResult(var Result: TmodRespondResult); override;
    function CreateRespond: TmodRespond; override;

    procedure RespondNotFound;
    procedure RespondNotActive;
    procedure SendFile(const vFile: string); overload;
    procedure SendFile(const vFile, vName: string; vDisposition: TSendFileDisposition = sdDefault); overload;
  public
    destructor Destroy; override;
    property Respond: TmodHttpRespond read GetRespond;
  end;

  TmodWebSocketCommand = class(TmodHttpCommand)
  end;

  TmodWebFileModule = class;

  { TmodURICommand }

  TmodURICommand = class(TmodHttpCommand)
  private
    function GetModule: TmodWebFileModule;
  protected
    function GetDefaultDocument(Root: string): string;
    procedure RespondResult(var Result: TmodRespondResult); override;
    procedure Prepare(var Result: TmodRespondResult); override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property Module: TmodWebFileModule read GetModule;
  end;

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class abstract(TmodModule)
  private
    procedure SetHomePath(AValue: string);
  protected
    FHomePath: string;
    procedure Created; override;

    procedure Log(S: string); override;
    procedure InternalError(ARequest: TmodRequest; AStream: TmnBufferStream; var Handled: Boolean); override;
    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;
  public
    HomeURL: string;
    HostURL: string;
    CachePath: string;
    destructor Destroy; override;
    property DocumentRoot: string read FHomePath write SetHomePath; //deprecated;
    property HomePath: string read FHomePath write SetHomePath;
  end;

  { TmodWebFileModule }

  TmodWebFileModule = class(TmodWebModule)
  protected
    FDefaultDocument: TStringList;
    procedure SetDefaultDocument(AValue: TStringList);
    procedure DoRegisterCommands; override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  ThttpModules = class(TmodModules)
  protected
    function CheckRequest(const ARequest: string): Boolean; override;
  end;

  { TmodWebModules }

  TmodWebModules = class(ThttpModules)
  protected
  public
    procedure ParseHead(ARequest: TmodRequest; const RequestLine: string); override;
  end;

  { TmodCustomWebServer }

  TmodCustomWebServer = class(TmodModuleServer)
  protected
    function CreateModules: TmodModules; override;
  public
    procedure AddAcmeChallenge(const AName: string = '.well-known'; const AHomePath: string = '');
  end;

  TmodWebServer = class(TmodCustomWebServer)
  protected
  public
    constructor Create; override;
  end;

  { TmodAcmeChallengeServer }

  TmodAcmeChallengeServer = class(TmodCustomWebServer)
  protected
  public
    constructor Create; override;
  end;

  {$ifndef FPC}
  TmodWebEventProc = reference to procedure(vRequest: TmodRequest; vRespond: TmodHttpRespond; var vResult: TmodRespondResult);

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

  //handle cors :)
  TmodHttpOptionCommand = class(TmodHttpGetCommand)
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


function HashWebSocketKey(const key: string): string;
var
{$ifdef FPC}
  s: string;
{$else}
  b: TBytes;
{$endif}
begin
{$ifdef FPC}
  s := SHA1Print(SHA1String(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
  Result := EncodeStringBase64(s);
{$else}
  b := THashSHA1.GetHashBytes(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
  Result := TNetEncoding.Base64String.EncodeBytesToString(b);
{$endif}
end;


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

procedure TmodHttpRespond.SetHttpResult(const Value: THttpResult);
begin
  if resHeaderSent in States then
    raise TmodModuleException.Create('Header is already sent');
  FHttpResult := Value;
end;

procedure TmodHttpRespond.SetProtcolClass(const Value: TmnProtcolStreamProxyClass);
begin
  FProtcolClass := Value;
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

function TmodHttpRespond.HeadText: string;
begin
  Result := HttpResult.ToString;
end;

{ TmodHttpPostCommand }

procedure TmodHttpPostCommand.RespondResult(var Result: TmodRespondResult);
begin
  {if SameText(Request.Method, 'POST') then
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
  end;}
  inherited;
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
  UseKeepAlive := klvUndefined;
  UseCompressing := True;
  UseWebSocket := True;
  FHomePath := '';
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

procedure TmodWebModule.InternalError(ARequest: TmodRequest; AStream: TmnBufferStream; var Handled: Boolean);
begin
  inherited;
  AStream.WriteLineUTF8('HTTP/1.1 500 Internal Server Error');
  AStream.WriteLineUTF8('');
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

procedure TmodWebFileModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
end;

destructor TmodWebFileModule.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

{ TmodURICommand }

function TmodURICommand.GetModule: TmodWebFileModule;
begin
  Result := (inherited Module) as TmodWebFileModule;
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
  Respond.FHomePath := Module.HomePath;
  Respond.FHostURL := Request.Header.ReadString('Host');
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
begin
  if FileExists(vDocument) then
  begin
    SendFile(vDocument);
  end
  else
    RespondNotFound;
end;

procedure TmodHttpGetCommand.RespondResult(var Result: TmodRespondResult);
var
  aDocument, aHomePath: string;
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
     aDocument := GetDefaultDocument(aDocument);

  if not StartsStr(aHomePath, aDocument) then //check if out of root :)
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
  aFile := TFileStream.Create(Respond.HomePath + aFileName, fmCreate);
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
  aFileName := IncludeTrailingPathDelimiter(Respond.HomePath) + Request.Path;
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
  TmodWebFileModule.Create('web', 'doc', ['http/1.1'], Modules);
  Port := '80';
end;

{ TmodAcmeChallengeServer }

constructor TmodAcmeChallengeServer.Create;
begin
  inherited Create;
  AddAcmeChallenge;
end;

function TmodCustomWebServer.CreateModules: TmodModules;
begin
  Result := TmodWebModules.Create(Self);
end;

procedure TmodCustomWebServer.AddAcmeChallenge(const AName: string; const AHomePath: string);
begin
  //* http://localhost/.well-known/acme-challenge/index.html
  with TmodWebFileModule.Create(AName, '.well-known', ['http/1.1'], Modules) do
  begin
    Level := -1;
    HomePath := AHomePath;
  end;
  //* use certbot folder to "Application.Location + 'cert'" because certbot will create folder .well-known
  Port := '80';
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
  aKeepAlive: Boolean;
  WSHash, WSKey: string;
begin
  inherited;

  if Request.Header.Field['Connection'].Have('Upgrade', [',']) then
  begin
    if Module.UseWebSocket and Request.Header.Field['Upgrade'].Have('WebSocket', [',']) then
    begin
      if Request.Header['Sec-WebSocket-Version'].ToInteger = 13 then
      begin
        WSHash := Request.Header['Sec-WebSocket-Key'];
        WSKey := HashWebSocketKey(WSHash);
        Respond.HttpResult := hrSwitchingProtocols;
        Respond.AddHeader('Connection', 'Upgrade');
        Respond.AddHeader('upgrade', 'websocket');
        Respond.AddHeader('date: ', FormatHTTPDate(Now));
        Respond.AddHeader('sec-websocket-accept', WSKey);
        Respond.AddHeader('Sec-WebSocket-Protocol', 'plain');
        Respond.SendHeader;

{
        Respond.SendHead;
        Respond.Stream.WriteLineUTF8('connection: Upgrade');
        Respond.Stream.WriteLineUTF8('upgrade: websocket');
        Respond.Stream.WriteLineUTF8('sec-websocket-accept: ' + WSKey);

//      Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
        Respond.Stream.WriteLineUTF8('Sec-WebSocket-Protocol: plain');

        Respond.Stream.WriteLineUTF8('');
        Respond.ClearHeader;
}

        Respond.KeepAlive := True;
        Respond.ProtcolClass := TmnWebSocket13StreamProxy;
        Respond.FProtcolProxy := Respond.ProtcolClass.Create;
        Respond.WebSocket := True;
        Respond.Stream.AddProxy(Respond.ProtcolProxy);
        Result.Status := Result.Status + [mrKeepAlive];

      //Respond.AddHeader('Sec-WebSocket-Accept', Key);
    //* https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers
      end;
    end;
  end;

  if not Respond.WebSocket then
  begin
    aKeepAlive := (Module.UseKeepAlive = klvUndefined) and SameText(Request.Header.ReadString('Connection'), 'Keep-Alive');
    aKeepAlive := aKeepAlive or ((Module.UseKeepAlive = klvKeepAlive) and not SameText(Request.Header.ReadString('Connection'), 'close'));

    if aKeepAlive then
    begin
      Respond.KeepAlive := True;
      Respond.AddHeader('Connection', 'Keep-Alive');
      Respond.AddHeader('Keep-Alive', 'timout=' + IntToStr(Module.KeepAliveTimeOut div 1000) + ', max=100');
    end;

    if not aKeepAlive and Module.UseCompressing then
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
end;

procedure TmodHttpCommand.Unprepare(var Result: TmodRespondResult);
var
  aParams: TmnParams;
begin
  inherited;
  if Respond.WebSocket then
  begin
  end
  else
  begin
    if not Respond.Header.Exists['Content-Length'] then
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
        Result.Timout := Module.KeepAliveTimeOut;

      Result.Status := Result.Status + [mrKeepAlive];
    end;

    if Respond.CompressProxy <> nil then
    begin
      Respond.CompressProxy.Disable;
    end;
  end;
end;

procedure TmodHttpCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
end;

procedure TmodHttpCommand.SendFile(const vFile, vName: string; vDisposition: TSendFileDisposition);
var
  aDocSize: Int64;
  aDocStream: TFileStream;
  aDate: TDateTime;
  aEtag, aFtag: string;
begin
  if Active then
  begin
    FileAge(vFile, aDate);
    aFtag := DateTimeToUnix(aDate).ToString;
    aEtag := Request.Header['If-None-Match'];
    if (aEtag<>'') and (aEtag=aFtag) then
    begin
      Respond.HttpResult := hrNotModified;
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

      Respond.HttpResult := hrOK;

      //Respond.AddHeader('Cache-Control', 'max-age=600');
      Respond.AddHeader('Cache-Control', 'max-age=600');
      //Respond.AddHeader('Cache-Control', 'public');
      //Respond.AddHeader('Date', Now);
      Respond.AddHeader('Last-Modified', aDate);
      Respond.AddHeader('ETag', aFtag);
      if Active then
      begin
        Respond.PutHeader('Content-Type', DocumentToContentType(vName));
        case vDisposition of
          sdInline: Respond.PutHeader('Content-Disposition', Format('inline; filename="%s"', [vName]));
          sdAttachment: Respond.PutHeader('Content-Disposition', Format('attachment; filename="%s"', [vName]));
          else;
        end;

        if Respond.KeepAlive then
          Respond.AddHeader('Content-Length', IntToStr(aDocSize));
      end;

      Respond.SendHeader;

      if Active then
        Respond.Stream.WriteStream(aDocStream);
    finally
      aDocStream.Free;
    end;
  end
  else
  begin
    RespondNotActive;
  end;

end;

procedure TmodHttpCommand.RespondNotActive;
var
  Body: string;
begin
  Respond.HttpResult := hrOK;
  Respond.AddHeader('Content-Type', 'text/html');
  Respond.SendHeader;
  Body := '<HTML><HEAD><TITLE>404 Not Active</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  Respond.Stream.WriteUTF8(UTF8Encode(Body));
  Respond.KeepAlive := False;
end;

procedure TmodHttpCommand.RespondNotFound;
var
  Body: string;
begin
  Respond.HttpResult := hrOK;
  Respond.AddHeader('Content-Type', 'text/html');
  Respond.SendHeader;
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  Respond.Stream.WriteUTF8(UTF8Encode(Body));
  Respond.KeepAlive := False;
end;


procedure TmodHttpCommand.SendFile(const vFile: string);
begin
  SendFile(vFile, ExtractFileName(vFile));
end;

function TmodHttpCommand.CreateRespond: TmodRespond;
begin
  Result := TmodHttpRespond.Create;
end;

{ TmodCustomWebModules }

procedure TmodWebModules.ParseHead(ARequest: TmodRequest; const RequestLine: string);
begin
  inherited;
  ARequest.URI := URIDecode(ARequest.URI);
  //ARequest.ParsePath(ARequest.URI); duplicate in parse head :)
  ARequest.Command := ARequest.Method;
end;

{ THttpResultHelper }

function THttpResultHelper.ToString: string;
begin
  Result := 'HTTP/1.1 ';
  case Self of
    hrNone: Result := '';
    hrOK: Result := Result + '200 OK';
    hrError: Result := Result + '500 Internal Server Error';
    hrUnauthorized: Result := Result + '401 Unauthorized';
    hrNotFound: Result := Result + '404 NotFound';
    hrMovedTemporarily: Result := Result + '307 Temporary Redirect';
    hrFound: Result := Result + '302 Found';
    hrNotModified: Result := Result + '304 Not Modified';
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

  Port := vPort;
end;

{ TmodWebEventModule }

procedure TmodWebEventModule.DoRegisterCommands;
begin
  // inherited;
  RegisterCommand('Event', TmodHttpEventCommand, true);
end;
{$endif FPC}

{ TmodHttpOptionCommand }

procedure TmodHttpOptionCommand.RespondResult(var Result: TmodRespondResult);
begin
  inherited;
  Respond.HttpResult := hrOK;
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

function ThttpModules.CheckRequest(const ARequest: string): Boolean;
begin
  Result := Server.UseSSL or (ARequest[1]<>#$16);
end;

initialization
  modLock := TCriticalSection.Create;

finalization
  FreeAndNil(modLock);
end.
