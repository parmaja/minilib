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
  SysUtils, Classes, mnSockets, mnServers, mnStreams;

type
  THttpConnection = class;
  THttpConnectionClass = class of THttpConnection;
  THttpGetFlag = (hgSendDoc, hgSendStream, hgWillSendMySelf, hg404, hgAcceptData);
  THttpSendType = (httpSendHead, httpSendDoc);
  THttpConnectionState = (hcRequest, hcHeader, hcPostedData);

  THttpConnection = class(TmnServerConnection)
  private
    function GetDocument(Document: string): string;
  public
    FMethod: string;
    FVersion: string;
    FPath: string;
    FOrginalPath: string;
    FParams: string;
    FRequestHeader: TStringList;
    FState: THttpConnectionState;
    FDocumentRoot: string;
    FDocument: string;
    FDocBuf: PChar;
    FAnswerContentType: string;
    FRequestContentLength: Integer;
    FRequestContentType: string;
    FRequestAccept: string;
    FRequestReferer: string;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestHost: string;
    FRequestConnection: string;
    FAcceptPostedData: Boolean;
    FHeaderStrings: TStringList;
    FRequestPort: string;
    FProxyRequest: Boolean;
    procedure ReceiveData;
    procedure ProcessRequest;
    procedure ProcessGet;
    procedure ProcessHead;
    procedure ProcessPost;
    procedure AnswerDocument;
    procedure Answer404;
  protected
    procedure Process; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
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
  published
    property DocumentRoot: string read FDocumentRoot write FDocumentRoot;
    property Document: string read FDocument write FDocument;
    property Path: string read FPath write FPath;
    property Params: string read FParams write FParams;
  end;

  TmnHttpListener = class(TmnListener)
  private
    FDocumentRoot: string;
    FAddress: string;
    FPort: Integer;
    FDefaultDocument: TStringList;
    procedure SetDefaultDocument(const Value: TStringList);
  protected
    function GetDocument(Root: string): string;
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Port: Integer read FPort write FPort;
    property Address: string read FAddress write FAddress;
    property DocumentRoot: string read FDocumentRoot write FDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  TOnHttpServerEvent = procedure(Sender: TObject; Socket: THttpConnection) of object;

  TmnHttpServer = class(TmnServer)
  private
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
    procedure SetDefaultDoc(const Value: TStringList);
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    function CreateListener: TmnListener; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DocumentRoot: string read FDocumentRoot write FDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDoc;
  end;

implementation

constructor TmnHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  Port := '80';
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
  FDocumentRoot := '';
end;

destructor TmnHttpServer.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

procedure TmnHttpServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

constructor THttpConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  FRequestHeader := TStringList.Create;
  FState := hcRequest;
  FProxyRequest := false;
end;

destructor THttpConnection.Destroy;
begin
  FreeAndNil(FRequestHeader);
  if Assigned(FDocBuf) then
  begin
    FreeMem(FDocBuf);
    FDocBuf := nil;
  end;
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
  procedure ParseRequest;
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

  procedure ParseHeader;
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
    FRequestConnection := FRequestHeader.Values['Connection'];
  end;
var
  ln: string;
begin
  if FState = hcPostedData then
  begin
  end;

  if FState = hcRequest then
  begin
    FRequestContentType := '';
    FRequestContentLength := 0;
    FRequestAccept := '';
    FRequestReferer := '';
    FRequestAcceptLanguage := '';
    FRequestAcceptEncoding := '';
    FRequestUserAgent := '';
    FRequestHost := '';
    FRequestConnection := '';
    ln := Stream.ReadLn(#13);
    while ln <> '' do
    begin
      FRequestHeader.Add(ln);
      ln := Stream.ReadLn;
    end;
    if FRequestHeader.Count > 0 then
    begin
      ParseRequest;
      ParseHeader;
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
    '<BODY><H1>404 Not Found</H1>The requested URL ' + FDocument +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>' + sEndOfLine;
  Stream.WriteString(Body);
end;

procedure THttpConnection.ProcessRequest;
begin
  FDocument := ExcludeTrailingPathDelimiter(FDocumentRoot);
  if FProxyRequest then
    FDocument := IncludeTrailingPathDelimiter(FDocument) + IncludeTrailingPathDelimiter(FRequestHost) + FPath
  else
    FDocument := IncludeTrailingPathDelimiter(FDocument) + FPath;

  FDocument := StringReplace(FDocument, '/', DirectorySeparator, [rfReplaceAll]);

  if FDocument[Length(FDocument)] = DirectorySeparator then
    FDocument := GetDocument(FDocument);

  if FMethod = 'GET' then
    ProcessGet
  else if FMethod = 'POST' then
    ProcessPost
  else if FMethod = 'HEAD' then
    ProcessHead
  else
    Answer404;
  Listener.Log(Self, FDocument); //when i put this line first not work in WinCE!!!
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
  else if Ext = 'txt' then
    Result := 'text/plain'
  else
    Result := 'application/binary';
end;

procedure THttpConnection.AnswerDocument;
var
  DocSize: Int64;
  aDocStream: TFileStream;
begin
  if FileExists(FDocument) then
  begin
    if Connected then
    begin
      FAnswerContentType := DocumentToContentType(FDocument);
      aDocStream := TFileStream.Create(FDocument, fmOpenRead or fmShareDenyWrite);
      DocSize := aDocStream.Size;
      if Connected then
        Stream.WriteString(FVersion + ' 200 OK' + sEndOfLine +
          'Content-Type: ' + FAnswerContentType + sEndOfLine +
          'Content-Length: ' + IntToStr(DocSize) + sEndOfLine +
          sEndOfLine);
      if Connected then
        Stream.WriteStream(aDocStream);
      aDocStream.Free;
    end;
    if Connected then
      Stream.Socket.Shutdown(sdBoth);
  end
  else
    Answer404;
end;

procedure THttpConnection.Process;
begin
  if Stream.WaitToRead(5000) then
    ReceiveData;
end;

{ THttpSocketServer }

procedure TmnHttpServer.SetDefaultDoc(const Value: TStringList);
begin
  FDefaultDocument.Assign(Value);
end;

function TmnHttpServer.CreateListener: TmnListener;
begin
  Result := TmnHttpListener.Create;
  TmnHttpListener(Result).DocumentRoot := ExcludeTrailingPathDelimiter(FDocumentRoot);
  TmnHttpListener(Result).DefaultDocument.Assign(FDefaultDocument);
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
  Result := THttpConnection.Create(vSocket);
  (Result as THttpConnection).DocumentRoot := FDocumentRoot;
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
