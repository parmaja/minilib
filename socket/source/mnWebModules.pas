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
  SysUtils, Classes, syncobjs,
  mnFields, mnConfigs, mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnModules;

type

  TmodWebModule = class;

  { TmodHttpCommand }

  TmodHttpCommand = class(TmodCommand)
  private
    FCookies: TmnParams;
    FKeepAlive: Boolean;
    FURIParams: TmnParams;
    FCompressIt: Boolean;
    FContentLength: Integer;
    DeflateProxy: TmnDeflateStreamProxy;
  protected
    procedure Created; override;
    procedure Prepare(var Result: TmodExecuteResults); override;
    procedure SendHeader; override;
    procedure Unprepare(var Result: TmodExecuteResults); override;
    procedure Respond(var Result: TmodExecuteResults); override;
  public
    destructor Destroy; override;
    property Cookies: TmnParams read FCookies;
    property URIParams: TmnParams read FURIParams;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    //Compress on the fly, now we use deflate
    property CompressIt: Boolean read FCompressIt write FCompressIt;
    property ContentLength: Integer read FContentLength write FContentLength;
  end;

  { TmodURICommand }

  TmodURICommand = class(TmodHttpCommand)
  private
    function GetModule: TmodWebModule;
  protected
    function GetDefaultDocument(Root: string): string;
    procedure RespondNotFound;
    procedure Respond(var Result: TmodExecuteResults); override;
    procedure Prepare(var Result: TmodExecuteResults); override;
    procedure Created; override;
  public
    Root: string; //Document root folder
    Host: string;
    destructor Destroy; override;
    property Module: TmodWebModule read GetModule;
  end;

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class(TmodModule)
  private
    FServer: TmodWebServer;
    procedure SetDefaultDocument(AValue: TStringList);
    procedure SetDocumentRoot(AValue: string);
  protected
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
    function GetActive: Boolean; override;
    procedure Created; override;
    procedure DoCreateCommands; override;

    function RequestCommand(var ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmodCommand; override;

    function Match(const ARequest: TmodRequest): Boolean; override;
    procedure Log(S: string); override;
  public
    destructor Destroy; override;
    property Server: TmodWebServer read FServer;
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
  private
    FWebModule: TmodWebModule;
  protected
  public
    constructor Create; override;
    property WebModule: TmodWebModule read FWebModule;
  end;

  {**
    Files Commands
  *}

  { TmodHttpGetCommand }

  TmodHttpGetCommand = class(TmodURICommand)
  protected
  public
    procedure Respond(var Result: TmodExecuteResults); override;
  end;

  { TmodHttpPostCommand }

  TmodHttpPostCommand = class(TmodHttpGetCommand)
  protected
    Contents: TMemoryStream;
  public
    procedure Respond(var Result: TmodExecuteResults); override;
  end;

  { TmodPutCommand }

  TmodPutCommand = class(TmodURICommand)
  protected
  public
    procedure Respond(var Result: TmodExecuteResults); override;
  end;

  { TmodServerInfoCommand }

  TmodServerInfoCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmodExecuteResults); override;
  public
  end;

  { TmodDirCommand }

  TmodDirCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmodExecuteResults); override;
  public
  end;

  { TmodDeleteFileCommand }

  TmodDeleteFileCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmodExecuteResults); override;
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

{ TmodHttpPostCommand }

procedure TmodHttpPostCommand.Respond(var Result: TmodExecuteResults);
begin
  if SameText(Request.Method, 'POST') and (RequestHeader['Content-Type'].Have('application/json')) then
  begin
    Contents := TMemoryStream.Create;
    if RequestStream <> nil then
      RequestStream.ReadStream(Contents, ContentLength);
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

function TmodWebModule.GetActive: Boolean;
begin
  Result := Server.Active;
end;

procedure TmodWebModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  //UseKeepAlive := true;
  Compressing := True;
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

function TmodWebModule.RequestCommand(var ARequest: TmodRequest; ARequestStream, ARespondStream: TmnBufferStream): TmodCommand;
begin
  ARequest.ParsePath(ARequest.URI);
  ARequest.Command := ARequest.Method;
  Result := CreateCommand(ARequest.Command, ARequest, ARequestStream, ARespondStream);
  if Result <> nil then
    ParseHeader(Result.RequestHeader, ARequestStream);
end;

function TmodWebModule.Match(const ARequest: TmodRequest): Boolean;
begin
  //ParseRequest(ARequest);
  Result := SameText(Name, ARequest.Module) and SameText(SubStr(ARequest.Protcol, '/', 0),  'http');
end;

procedure TmodWebModule.Log(S: string);
begin
  inherited;
  Server.Listener.Log(S);
end;

{ TmodURICommand }

procedure TmodURICommand.RespondNotFound;
var
  Body: string;
begin
  SendRespond('HTTP/1.1 200 OK');
  PostHeader('Content-Type', 'text/html');
  SendHeader;
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  RespondStream.WriteString(Body);
  KeepAlive := False;
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

procedure TmodURICommand.Respond(var Result: TmodExecuteResults);
begin
  inherited;
end;

procedure TmodURICommand.Prepare(var Result: TmodExecuteResults);
begin
  inherited;
  Root := Module.DocumentRoot;
  Host := RequestHeader.ReadString('Host');
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

procedure TmodHttpGetCommand.Respond(var Result: TmodExecuteResults);
var
  DocSize: Int64;
  aDocStream: TFileStream;
  aDocument: string;
begin
  aDocument := IncludeTrailingPathDelimiter(Root);
  if Request.Path <> '' then
    aDocument := aDocument + '.' + Request.Path;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux
  if aDocument[Length(aDocument)] = PathDelim then //get the default file if it not defined
     aDocument := GetDefaultDocument(aDocument);
  aDocument := ExpandFileName(aDocument);

  if FileExists(aDocument) then
  begin
    if Active then
    begin
      aDocStream := TFileStream.Create(aDocument, fmOpenRead or fmShareDenyWrite);
      try
        DocSize := aDocStream.Size;
        if Active then
        begin
          SendRespond('HTTP/1.1 200 OK');
          PostHeader('Content-Type', DocumentToContentType(aDocument));
          if KeepAlive then
            PostHeader('Content-Length', IntToStr(10));
        end;

        SendHeader;

        if Active then
          RespondStream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    RespondNotFound;
  inherited;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.Respond(var Result: TmodExecuteResults);
begin
  inherited;
  SendRespond('OK');
  RespondStream.WriteLine('Server is running on port: ' + Module.Server.Port);
  RespondStream.WriteLine('the server is: "' + ParamStr(0) + '"');
end;

{ TmodPutCommand }

procedure TmodPutCommand.Respond(var Result: TmodExecuteResults);
var
  aFile: TFileStream;
  aFileName: string;
begin
  inherited;
  RespondStream.WriteCommand('OK');
  aFileName := URIParams.Values['FileName'];
  aFile := TFileStream.Create(Root + aFileName, fmCreate);
  try
    RespondStream.ReadStream(aFile, ContentLength);
  finally
    aFile.Free;
  end;
end;

{ TmodDirCommand }

procedure TmodDirCommand.Respond(var Result: TmodExecuteResults);
var
  i: Integer;
  aStrings: TStringList;
  //aPath: string;
  aFilter: string;
begin
  inherited;
  RespondStream.WriteCommand('OK');
  aFilter := URIParams.Values['Filter'];
  //aPath := IncludeTrailingPathDelimiter(Root);
  if aFilter = '' then
    aFilter := '*.*';
   aStrings := TStringList.Create;
  try
    //EnumFileList(aPath + aFilter, aStrings);
    for i := 0 to aStrings.Count - 1 do
    begin
      RespondStream.WriteLine(IntToStr(i) + ': ' + aStrings[i]);
    end;
  finally
    aStrings.Free;
  end;
end;

{ TmodDeleteFileCommand }

procedure TmodDeleteFileCommand.Respond(var Result: TmodExecuteResults);
var
  aFileName: string;
begin
  inherited;
  aFileName := IncludeTrailingPathDelimiter(Root) + Request.Path;
  if FileExists(aFileName) then
    DeleteFile(aFileName);
  RespondStream.WriteCommand('OK');
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
  FWebModule := TmodWebModule.Create('web', 'http/1.1', Modules);
  FWebModule.FServer := Self;
  Modules.DefaultModule := FWebModule;
  Port := '80';
  with FWebModule do
  begin
    FDocumentRoot := '';
    FDefaultDocument.Add('index.html');
    FDefaultDocument.Add('index.htm');
    FDefaultDocument.Add('default.html');
    FDefaultDocument.Add('default.htm');
  end;
end;

function TmodCustomWebServer.CreateModules: TmodModules;
begin
  Result := TmodWebModules.Create;
end;

{ TmodHttpCommand }

procedure TmodHttpCommand.Created;
begin
  inherited;
  FCookies := TmnParams.Create;
  FURIParams := TmnParams.Create;
end;

destructor TmodHttpCommand.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FURIParams);
  inherited;
end;

procedure TmodHttpCommand.Prepare(var Result: TmodExecuteResults);
begin
  inherited;
  Request.ParsePath(Request.URI, URIParams);

  if Module.UseKeepAlive and SameText(RequestHeader.ReadString('Connection'), 'Keep-Alive') then
  begin
    KeepAlive := True;
    PostHeader('Connection', 'Keep-Alive');
    PostHeader('Keep-Alive', 'timout=' + IntToStr(Module.KeepAliveTimeOut div 5000) + ', max=100');
  end;

  FCompressIt := Module.Compressing and RequestHeader['Accept-Encoding'].Have('deflate', [',']);
  if CompressIt then
  begin
    PostHeader('Content-Encoding', 'deflate');
  end;
  if (RequestHeader['Content-Length'].IsExists) then
    ContentLength := RequestHeader['Content-Length'].AsInteger;
end;

procedure TmodHttpCommand.Unprepare(var Result: TmodExecuteResults);
var
  aParams: TmnParams;
begin
  inherited;
  if not RespondHeader.Exists['Content-Length'] then
    KeepAlive := False;
  if KeepAlive and Module.UseKeepAlive and SameText(RequestHeader.ReadString('Connection'), 'Keep-Alive') then
  begin
    Result.Timout := Module.KeepAliveTimeOut;
    if RequestHeader.IsExists('Keep-Alive') then //idk if really sent from client
    begin
      aParams := TmnParams.Create;
      try
        //Keep-Alive: timeout=5, max=1000
        aParams.Separator := '=';
        aParams.Delimiter := ',';
        aParams.AsString := RequestHeader['Keep-Alive'].AsString;
        Result.Timout := aParams['timeout'].AsInteger;
      finally
        aParams.Free;
      end;
    end;
    Result.Status := Result.Status + [erKeepAlive];
  end;
  if DeflateProxy <> nil then
  begin
    DeflateProxy.Disable;
  end;
end;

procedure TmodHttpCommand.Respond(var Result: TmodExecuteResults);
begin
  inherited Respond(Result);
  Log(Request.Client + ': ' + Request.Raw);
end;

procedure TmodHttpCommand.SendHeader;
begin
  if Cookies.Count > 0 then
    PostHeader('Cookies', Cookies.AsString);
  inherited;

  if CompressIt then
  begin
    if DeflateProxy = nil then
    begin
      DeflateProxy := TmnDeflateStreamProxy.Create([cprsWrite], 9, false);
      RespondStream.AddProxy(DeflateProxy);
    end
    else
      DeflateProxy.Enable;
  end;
end;

{ TmodCustomWebModules }

procedure TmodWebModules.ParseHead(ARequest: TmodRequest; const RequestLine: string);
begin
  inherited ParseHead(ARequest, RequestLine);
  ARequest.URI := URIDecode(ARequest.URI);
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  FreeAndNil(modLock);
end.
