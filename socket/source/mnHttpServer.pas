unit mnHttpServer;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, mnSockets, mnServers, mnStreams, mnConnections;

type
  THttpConnection = class;
  THttpConnectionClass = class of THttpConnection;
  THttpGetFlag = (hgSendDoc, hgSendStream, hgWillSendMySelf, hg404, hgAcceptData);
  THttpSendType = (httpSendHead, httpSendDoc);
  THttpConnectionState = (hcRequest, hcHeader, hcPostedData);

  { TRequestInfo }

  TRequestInfo = class(TObject)
  private
    FMethod: string;
    FVersion: string;
    FRequestHeader: TStringList;
    FAnswerContentType: string;
    FRequestContentLength: Integer;
    FRequestContentType: string;
    FRequestConnection: string;
    FRequestAccept: string;
    FRequestReferer: string;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestHost: string;
    FRequestPort: string;
    FProxyRequest: Boolean;
    FOrginalPath: string;
    FKeepAlive: Boolean;
    FPath: string;
    FParams: string;
    FDocument: string;
    FDocBuf: PChar;
  protected
    procedure ParseRequest;
    procedure ParseHeader;
  public
    constructor Create;
    destructor Destroy; override;
    property Method: string read FMethod;
    property Version: string read FVersion;
    property RequestHeader: TStringList read FRequestHeader;
    property RequestContentLength: Integer read FRequestContentLength;
    property RequestContentType: string read FRequestContentType;
    property RequestAccept: string read FRequestAccept;
    property RequestReferer: string read FRequestReferer;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestUserAgent: string read FRequestUserAgent;
    property RequestHost: string read FRequestHost;
    property RequestPort: string read FRequestPort;
    property RequestConnection: string read FRequestConnection;
    property ProxyRequest: Boolean  read FProxyRequest;
    property AnswerContentType: string read FAnswerContentType;
    property Document: string read FDocument write FDocument;
    property Path: string read FPath write FPath;
    property KeepAlive: boolean read FKeepAlive;
    property Params: string read FParams write FParams;
  end;

  TmnHttpListener = class;

  TConfigInfo = record
    DocumentRoot: string;
    AllowKeepAlive: Boolean;
  end;

  { THttpConnection }

  THttpConnection = class(TmnServerConnection)
  private
    FState: THttpConnectionState;
    function GetDocument(Document: string): string;
  public
    FRequestInfo: TRequestInfo;
    FUsedCount: Integer;
    procedure ReceiveData;
    procedure ProcessRequest;
    procedure ProcessGet;
    procedure ProcessHead;
    procedure ProcessPost;
    procedure AnswerDocument;
    procedure Answer404;
  protected
    function Listener: TmnHttpListener;
    procedure Process; override;
  public
    constructor Create(vConnector: TmnConnector; Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    property RequestInfo: TRequestInfo read FRequestInfo;
  end;

  { TmnHttpListener }

  TmnHttpListener = class(TmnListener)
  private
    FConfig: TConfigInfo;
    FDefaultDocument: TStringList;
    procedure SetDefaultDocument(const Value: TStringList);
  protected
    function GetDocument(Root: string): string;
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Config: TConfigInfo read FConfig write FConfig;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  TOnHttpServerEvent = procedure(Sender: TObject; Socket: THttpConnection) of object;

  { TmnHttpServer }

  TmnHttpServer = class(TmnEventServer) //or TmnServer not sure now
  private
    FConfig: TConfigInfo;
    FDefaultDocument: TStringList;
    procedure SetDefaultDoc(const Value: TStringList);
  protected
    function DoCreateListener: TmnListener; override;
    function CreateListener: TmnListener; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Config: TConfigInfo read FConfig write FConfig;
  published
    property DocumentRoot: string read FConfig.DocumentRoot write FConfig.DocumentRoot;
    property AllowKeepAlive: Boolean read FConfig.AllowKeepAlive write FConfig.AllowKeepAlive;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDoc;
  end;

implementation

{ TRequestInfo }

constructor TRequestInfo.Create;
begin
  inherited;
  FRequestHeader := TStringList.Create;
  FProxyRequest := false;
  FKeepAlive := false;
end;

destructor TRequestInfo.Destroy;
begin
  FreeAndNil(FRequestHeader);
  if Assigned(FDocBuf) then
  begin
    FreeMem(FDocBuf);
    FDocBuf := nil;
  end;
  inherited Destroy;
end;

procedure TRequestInfo.ParseRequest;
var
  I, J: Integer;
  s: string;
begin
  s := FRequestHeader[0];
  I := 1;
  while (I <= Length(s)) and (s[I] <> ' ') do
    Inc(I);
  FMethod := UpperCase(Copy(s, 1, I - 1));
  Inc(I);
  while (I <= Length(s)) and (s[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(s)) and (s[I] <> ' ') do
    Inc(I);
  FOrginalPath := Copy(s, J, I - J);
  FPath := FOrginalPath;
  J := Pos('//', FPath);
  if J > 0 then
  begin
    FProxyRequest := true;
    FPath := Copy(FPath, J + 1, MaxInt);
  end;

  if FPath[1] = '/' then
    Delete(FPath, 1, 1);

  { Find parameters }
  J := Pos('?', FPath);
  if J <= 0 then
    FParams := ''
  else
  begin
    FParams := Copy(FPath, J + 1, Length(FPath));
    FPath := Copy(FPath, 1, J - 1);
  end;
  Inc(I);
  while (I <= Length(s)) and (s[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(s)) and (s[I] <> ' ') do
    Inc(I);
  FVersion := Trim(UpperCase(Copy(s, J, I - J)));
  if FVersion = '' then
    FVersion := 'HTTP/1.0';
  FRequestHeader.Delete(0);
end;

procedure TRequestInfo.ParseHeader;
var
  i, j: Integer;
  s: string;
begin
  for j := 0 to FRequestHeader.Count - 1 do
  begin
    s := FRequestHeader[j];
    i := AnsiPos(':', s);
    if i > 0 then
    begin
      FRequestHeader[j] := (Copy(s, 0, i - 1)) + '=' + TrimLeft(Copy(s, i + 1, MaxInt));
    end;
  end;
  FRequestContentType := FRequestHeader.Values['Content-Type'];
  FRequestConnection := FRequestHeader.Values['Connection'];
  FRequestContentLength := StrToIntDef(FRequestHeader.Values['Content-Length'], 0);
  FRequestAccept := FRequestHeader.Values['Accept'];
  FRequestReferer := FRequestHeader.Values['Referer'];
  FRequestAcceptLanguage := FRequestHeader.Values['Accept-Language'];
  FRequestAcceptEncoding := FRequestHeader.Values['Accept-Encoding'];
  FRequestUserAgent := FRequestHeader.Values['User-Agent'];
  s := FRequestHeader.Values['Host'];
  i := AnsiPos(':', s);
  if i > 0 then
  begin
    FRequestPort := Copy(s, i + 1, MaxInt);
    s := Copy(s, 0, i - 1);
  end
  else
    FRequestPort := '';
  FRequestHost := s;
  FKeepAlive := SameText(FRequestConnection, 'Keep-Alive');
end;

constructor TmnHttpServer.Create;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  Port := '80';
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
end;

destructor TmnHttpServer.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

constructor THttpConnection.Create(vConnector: TmnConnector; Socket: TmnCustomSocket);
begin
  inherited;
  Stream.EndOfLine := sWinEndOfLine;
end;

destructor THttpConnection.Destroy;
begin
  inherited;
end;

function THttpConnection.GetDocument(Document: string): string;
begin
  Listener.Enter;
  try
    Result := (Listener as TmnHttpListener).GetDocument(Document);
  finally
    Listener.Leave;
  end;
end;

{ THttpConnection }

procedure THttpConnection.ReceiveData;
var
  ln: string;
begin
  if FState = hcPostedData then
  begin
  end;

  if FState = hcRequest then
  begin
    FRequestInfo := TRequestInfo.Create;
    ln := Trim(Stream.ReadLine);
    while ln <> '' do
    begin
      RequestInfo.RequestHeader.Add(ln);
      ln := Trim(Stream.ReadLine);
    end;

    if RequestInfo.RequestHeader.Count > 0 then
    begin
      RequestInfo.ParseRequest;
      RequestInfo.ParseHeader;
      if not Listener.Config.AllowKeepAlive then
        RequestInfo.FKeepAlive := False;
      FState := hcPostedData;
      ProcessRequest;
    end;
  end;
end;

procedure THttpConnection.Answer404;
var
  Body: string;
begin
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' + RequestInfo.Document +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>' + sEndOfLine;
  Stream.WriteString(Body);
end;

function THttpConnection.Listener: TmnHttpListener;
begin
  Result := inherited Listener as TmnHttpListener;
end;

procedure THttpConnection.ProcessRequest;
var
  aDocument: string;
begin
  aDocument := ExcludeTrailingPathDelimiter(Listener.Config.DocumentRoot);
  if RequestInfo.ProxyRequest then
    aDocument := IncludeTrailingPathDelimiter(aDocument) + IncludeTrailingPathDelimiter(RequestInfo.RequestHost) + RequestInfo.Path
  else
    aDocument := IncludeTrailingPathDelimiter(aDocument) + RequestInfo.Path;

  RequestInfo.FDocument := StringReplace(RequestInfo.FDocument, '/', PathDelim, [rfReplaceAll]);

  if aDocument[Length(aDocument)] = PathDelim then
    aDocument := GetDocument(aDocument);

  RequestInfo.FDocument := aDocument;

  if RequestInfo.Method = 'GET' then
    ProcessGet
  else if RequestInfo.Method = 'POST' then
    ProcessPost
  else if RequestInfo.Method = 'HEAD' then
    ProcessHead
  else
    Answer404;
  Listener.Log(RequestInfo.Document); //when i put this line first not work in WinCE!!!
end;

procedure THttpConnection.ProcessPost;
begin
  Answer404;
end;

procedure THttpConnection.ProcessHead;
begin
  AnswerDocument;
end;

procedure THttpConnection.ProcessGet;
begin
  AnswerDocument;
end;

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
  else if Ext = 'js' then
    Result := 'text/js'
  else
    Result := 'application/binary';
end;

procedure THttpConnection.AnswerDocument;
var
  DocSize: Int64;
  aDocStream: TFileStream;
begin
  if FileExists(RequestInfo.Document) then
  begin
    if Connected then
    begin
      RequestInfo.FAnswerContentType := DocumentToContentType(RequestInfo.Document);
      aDocStream := TFileStream.Create(RequestInfo.Document, fmOpenRead or fmShareDenyWrite);
      DocSize := aDocStream.Size;
      if Connected then
      begin
          Stream.WriteString(RequestInfo.Version + ' 200 OK' + sEndOfLine);
          Stream.WriteString('Content-Type: ' + RequestInfo.FAnswerContentType + sEndOfLine);
          if not RequestInfo.KeepAlive then
            Stream.WriteString('Connection: close' + sEndOfLine);
          Stream.WriteString('Content-Length: ' + IntToStr(DocSize) + sEndOfLine);
          Stream.WriteString(sEndOfLine);
      end;
      if Connected then
        Stream.WriteStream(aDocStream);
      aDocStream.Free;
    end;
    if Connected and not RequestInfo.KeepAlive then
      Stream.Close;
  end
  else
    Answer404;
end;

procedure THttpConnection.Process;
begin
  if Stream.WaitToRead(Stream.Timeout) then
  begin
    FState := hcRequest;
    FUsedCount := FUsedCount + 1;
    try
      ReceiveData;
    finally
      FreeAndNil(FRequestInfo);
    end;
  end;
end;

{ THttpSocketServer }

procedure TmnHttpServer.SetDefaultDoc(const Value: TStringList);
begin
  FDefaultDocument.Assign(Value);
end;

function TmnHttpServer.DoCreateListener: TmnListener;
begin
  Result := TmnHttpListener.Create;
end;

function TmnHttpServer.CreateListener: TmnListener;
begin
  Result := inherited CreateListener;
  TmnHttpListener(Result).Config := Config;
  TmnHttpListener(Result).DefaultDocument.Assign(FDefaultDocument);
  TmnHttpListener(Result).FConfig.DocumentRoot := ExcludeTrailingPathDelimiter(TmnHttpListener(Result).Config.DocumentRoot);
end;

procedure EnumDirList(const Path: string; Strings: TStrings);
var
  I: Integer;
  SearchRec: TSearchRec;
begin
  try
    I := FindFirst(Path, faDirectory, SearchRec);
    while I = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name[1] <> '.') then
        Strings.Add(SearchRec.Name);
      I := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  except
  end;
end;

function TmnHttpListener.GetDocument(Root: string): string;
var
  i: Integer;
begin
  for i := 0 to DefaultDocument.Count - 1 do
  begin
    if FileExists(Root + DefaultDocument[i]) then
    begin
      Result := Root + DefaultDocument[i];
    end;
  end;
end;

{ TmnHttpListener }

function TmnHttpListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := THttpConnection.Create(Self, vSocket);
end;

procedure TmnHttpListener.SetDefaultDocument(const Value: TStringList);
begin
  FDefaultDocument := Value;
end;

constructor TmnHttpListener.Create;
begin
  inherited;
  FOptions := FOptions + [soReuseAddr];
  FDefaultDocument := TStringList.Create;
end;

destructor TmnHttpListener.Destroy;
begin
  FDefaultDocument.Free;
  inherited;
end;

end.

