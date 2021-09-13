unit mnHttpClient;
{ **
  *  This file is part of the "Mini Library"
  *
  * @license   MIT
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    initial work Jihad Khlaifa <jkhalifa at gmail dot com>
  * @author    Zaher Dirkey zaherdirkey
  * }

{
  https://en.wikipedia.org/wiki/Uniform_Resource_Identifier
}

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

{$define OUT_Console}

interface

uses
  SysUtils, Classes, StrUtils,
 mnUtils, mnClasses, mnLogs, mnFields, mnConfigs, mnModules, mnSockets,
 mnClients, mnStreams, mnStreamUtils;

type

  TmnHttpClient = class;

  { TmnCustomHttpHeader }

  TmnCustomHttpHeader = class(TmnObject)
  private
    FAccept: UTF8String;
    FAcceptCharSet: UTF8String;
    FAcceptLanguage: UTF8String;
    FClient: TmnHttpClient;
    FAcceptEncoding: TStringList;

    FContentLength: Integer;
    FContentType: UTF8String;

    FDate: TDateTime;
    FKeepAlive: Boolean;
    FLastModified: TDateTime;
    FExpires: TDateTime;

    FHeaders: TmnHeader;
    function GetHeader(Index: string): string;
    procedure SetHeader(Index: string; AValue: string);

  protected
  public
    constructor Create(AClient: TmnHttpClient); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Items: TmnHeader read FHeaders write FHeaders;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Accept: UTF8String read FAccept write FAccept;
    property AcceptCharSet: UTF8String read FAcceptCharSet write FAcceptCharSet;
    property AcceptEncoding: TStringList read FAcceptEncoding write FAcceptEncoding;
    property AcceptLanguage: UTF8String read FAcceptLanguage write FAcceptLanguage;
    property ContentType: UTF8String read FContentType write FContentType;
    property ContentLength: Integer read FContentLength write FContentLength;

    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Client: TmnHttpClient read FClient;
    property Header[Index: string]: string read GetHeader write SetHeader; default;
  end;

  { TmnHttpRequest }

  TmnHttpRequest = class(TmnCustomHttpHeader)
  private
    FAccept: UTF8String;
    FReferer: UTF8String;
  protected
    procedure CollectHeaders; virtual;
  public
    procedure Created; override;
    procedure SendGet;
    procedure SendHead;
    procedure SendPost(vData: PByte; vCount: Cardinal);

    property Referer: UTF8String read FReferer write FReferer;
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: UTF8String;
    FServer: UTF8String;
  protected
    procedure ReceiveHeaders; virtual;
  public
    procedure Receive;
    property Location: UTF8String read FLocation write FLocation;
    property Server: UTF8String read FServer write FServer;
  end;

  { TmnCustomHttpStream }

  TmnHttpStream = class(TmnClientSocket)
  private
  protected
  public
    {$ifdef FPC}
    function Seek(Offset: longint; Origin: Word): Integer; override;
    {$else}
      {$if CompilerVersion > 33}
      function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
      {$else}
      function Seek(Offset: longint; Origin: Word): Integer; override;
      {$ifend}
    {$endif}
  end;

  { TmnHttpClient }

  TmnHttpClient = class(TObject)
  private
    FCookies: TmnParams;
    FHost: UTF8String;
    FPort: UTF8String;
    FPath: UTF8String;
    FProtocol: UTF8String;
    FCompressing: Boolean;
    FKeepAlive: Boolean;
    FUserAgent: UTF8String;

    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;

    FStream: TmnHttpStream;
  protected
    CompressClass: TmCompressStreamProxyClass;
    CompressProxy: TmCompressStreamProxy;
    function CreateStream: TmnHttpStream; virtual;
    procedure FreeStream; virtual;
    procedure Receive; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;

    //Use it to open connection and keep it connected
    procedure Connect(const vURL: UTF8String);
    function Open(const vURL: UTF8String; SendAndReceive: Boolean = True): Boolean;
    function Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean;

    procedure ReceiveStream(AStream: TStream); overload;
    procedure ReceiveStream(AStream: TStream; Count: Integer); overload;
    procedure ReceiveMemoryStream(AStream: TStream);
    procedure Disconnect;
    //Some utils
    //This will download the content into a stream and disconnect
    function GetStream(const vURL: UTF8String; OutStream: TStream): TFileSize;
    function GetFile(const vURL: UTF8String; OutFileName: UTF8String): TFileSize;
    function GetFileSize(vURL: string; out FileSize: TFileSize): Boolean;
    //Please add seek to 0 after getting it
    procedure GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
    procedure SendFile(const vURL: UTF8String; AFileName: UTF8String);

    property Request: TmnHttpRequest read FRequest;
    property Response: TmnHttpResponse read FResponse;
    property Cookies: TmnParams read FCookies write FCookies;

    property Host: UTF8String read FHost write FHost;
    property Port: UTF8String read FPort write FPort;
    property Protocol: UTF8String read FProtocol write FProtocol;
    property Path: UTF8String read FPath write FPath;
    property Compressing: Boolean read FCompressing write FCompressing;
    property UserAgent: UTF8String read FUserAgent write FUserAgent;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Stream: TmnHttpStream read FStream;
  end;

function HttpDownloadFile(URL: string; FileName: string): TFileSize;
function HttpGetFileSize(URL: string; out FileSize: TFileSize): Boolean;

implementation

const
  ProtocolVersion = 'HTTP/1.1';
  sUserAgent = 'Mozilla/5.0';

function HttpDownloadFile(URL: string; FileName: string): TFileSize;
var
  aHttpClient: TmnHttpClient;
begin
  aHttpClient := TmnHttpClient.Create;
  try
    Result := aHttpClient.GetFile(URL, FileName);
  finally
    FreeAndNil(aHttpClient);
  end;
end;

function HttpGetFileSize(URL: string; out FileSize: TFileSize): Boolean;
var
  aHttpClient: TmnHttpClient;
begin
  aHttpClient := TmnHttpClient.Create;
  try
    Result := aHttpClient.GetFileSize(URL, FileSize);
  finally
    FreeAndNil(aHttpClient);
  end;
end;

function GetUrlPart(var vPos: PUtf8Char; var vCount: Integer; const vTo: UTF8String; const vStops: TSysCharSet = []): UTF8String;

  function _IsMatch(vSrc, vDst: PUtf8Char; ACount: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to ACount - 1 do
    begin
      if vSrc^ <> vDst^ then
      begin
        Result := False;
        Break;
      end;
      Inc(vSrc);
      Inc(vDst);
    end;
  end;

var
  l: Integer;
  p, e, d: PUtf8Char;
  aFound: Boolean;
begin
  l := Length(vTo);
  d := PUtf8Char(vTo);
  p := vPos;
  e := vPos;
  Inc(e, vCount - l);
  aFound := False;
  while p < e do
  begin
    if (p^ in vStops) then
      Break;
    if (p^ = d^) and _IsMatch(p, d, l) then
    begin
      aFound := True;
      Break;
    end;
    Inc(p);
  end;

  if aFound then
  begin
    SetString(Result, vPos, p - vPos);
    Dec(vCount, l + (p - vPos));
    Inc(vPos, l + (p - vPos));
  end
  else
    Result := '';
end;

procedure SocketDownloadFile(URL: string; FileName: string);
var
  c: TmnHttpClient;
begin
  c := TmnHttpClient.Create;
  try
    //c.Compressing := True;
    //m.SaveToFile('c:\temp\1.json');
    c.GetFile(URL, FileName);
  finally
    FreeAndNil(c);
  end;
end;

procedure ParseURL(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String);
var
  p: PUTF8Char;
  l: Integer;
begin
  vProtocol := '';
  vAddress := '';
  vPort := '';
  vParams := '';

  p := PUtf8Char(vURL);
  l := Length(vURL);
  if l > 0 then
    vProtocol := GetUrlPart(p, l, '://', ['.']);

  if l > 0 then
  begin
    vAddress := GetUrlPart(p, l, ':', ['/']);
    if vAddress <> '' then
    begin
      vPort := GetUrlPart(p, l, '/', []);
      if vPort = '' then
      begin
        SetString(vPort, p, l);
        l := 0;
      end;
    end
    else
    begin
      vAddress := GetUrlPart(p, l, '/', []);
      if vAddress = '' then
      begin
        SetString(vAddress, p, l);
        l := 0;
      end;
    end;
  end;

  if l > 0 then
    SetString(vParams, p, l);

  if LeftStr(vParams, 1) <> '/' then
    vParams := '/' + vParams;

  if vPort = '' then
  begin
    if SameText(vProtocol, 'https') then
      vPort := '443'
    else
      vPort := '80';
  end;
end;

{ TmnCustomHttpHeader }

function TmnCustomHttpHeader.GetHeader(Index: string): string;
var
  F: TmnField;
begin
  F := Items.Field[Index];
  if F <> nil then
    Result := F.AsString
  else
    Result := '';
end;

procedure TmnCustomHttpHeader.SetHeader(Index: string; AValue: string);
begin
  if AValue <> '' then
    Items.Require[Index].Value := AValue
  else
    Items.RemoveByName(Index);
end;

constructor TmnCustomHttpHeader.Create(AClient: TmnHttpClient);
begin
  inherited Create;
  FClient := AClient;
  FHeaders := TmnHeader.Create;
  FAcceptEncoding := TStringList.Create;
  FAcceptEncoding.Delimiter := ',';
end;

destructor TmnCustomHttpHeader.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FAcceptEncoding);
  inherited;
end;

procedure TmnCustomHttpHeader.Clear;
begin
  FHeaders.Clear;
end;

{ TmnHttpRequest }

procedure TmnHttpRequest.CollectHeaders;
begin
  inherited;
  with FHeaders do
  begin
    Header['Host'] := Client.Host;
    Header['User-Agent'] := Client.UserAgent;

    Header['Accept'] := FAccept;
    Header['Accept-CharSet'] := FAcceptCharSet;
    if Client.Compressing then
      Header['Accept-Encoding'] := 'deflate, gzip';
    Header['Accept-Language'] := FAcceptLanguage;
    Header['Referer'] := FReferer;
  end;
end;

procedure TmnHttpRequest.Created;
begin
  inherited;
end;

procedure TmnHttpRequest.SendPost(vData: PByte; vCount: Cardinal);
var
  s: UTF8String;
  f: TmnField;
begin
  CollectHeaders;
  Client.Stream.WriteLineUTF8('POST ' + Client.Path + ' ' + ProtocolVersion);
  for f in Items do
    if f.AsString <> '' then
      Client.Stream.WriteLineUTF8(f.FullString);
  for f in Client.Cookies do
    s := f.Value + ';';
  if s <> '' then
    Client.Stream.WriteLineUTF8('Cookie: ' + s);
  Client.Stream.WriteLineUTF8('Content-Length: ' + IntToStr(vCount));
  Client.Stream.WriteLineUTF8('');
  Client.Stream.Write(vData^, vCount)
end;

procedure TmnHttpRequest.SendGet;
var
  s: UTF8String;
  f: TmnField;
begin
  CollectHeaders;
  Client.Stream.WriteLineUTF8('GET ' + Client.Path + ' ' + ProtocolVersion);
  for f in Items do
    if f.AsString <> '' then
      Client.Stream.WriteLineUTF8(f.FullString);
  for f in Client.Cookies do
    s := f.Value + ';';
  if s <> '' then
    Client.Stream.WriteLineUTF8('Cookie: ' + s);
  Client.Stream.WriteLineUTF8('');
end;

procedure TmnHttpRequest.SendHead;
var
  s: UTF8String;
  f: TmnField;
begin
  CollectHeaders;
  Client.Stream.WriteLineUTF8('HEAD ' + Client.Path + ' ' + ProtocolVersion);
  for f in Items do
    if f.AsString <> '' then
      Client.Stream.WriteLineUTF8(f.FullString);
  for f in Client.Cookies do
    s := f.Value + ';';
  if s <> '' then
    Client.Stream.WriteLineUTF8('Cookie: ' + s);
  Client.Stream.WriteLineUTF8('');
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.ReceiveHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FLocation := Header['Location'];
    FServer := Header['Server'];
    FContentType:= Header['Content-Type'];
    FContentLength := StrToIntDef(Header['Content-Length'], 0);
    FAccept := Header['Accept'];
    FAcceptCharSet := Header['Accept-CharSet'];
    FAcceptLanguage := Header['Accept-Language'];
    FAcceptEncoding.DelimitedText := Header['Content-Encoding'];
  end;
end;

procedure TmnHttpResponse.Receive;
var
  s: UTF8String;
begin
  FHeaders.Clear;
  // if FStream.Connected then
  begin
    Client.Stream.ReadLine(s, True);
    s := Trim(s);
    repeat
      Items.AddItem(s, ':', True);
      Client.Stream.ReadLine(s, True);
      s := Trim(s);
    until { FStream.Connected or } (s = '');
  end;
  ReceiveHeaders;
end;

{ TmnHttpStream }
{$ifdef FPC}
function TmnHttpStream.Seek(Offset: longint; Origin: Word): Integer;
{$else}
{$if CompilerVersion > 33}
function TmnHttpStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$else}
function TmnHttpStream.Seek(Offset: longint; Origin: Word): Integer;
{$ifend}
{$endif}
begin
  Result := 0; // for loading from this stream like Image.loadfrom stream
end;

{ TmnHttpClient }

function TmnHttpClient.Connected: Boolean;
begin
  Result := FStream.Connected;
end;

procedure TmnHttpClient.Connect(const vURL: UTF8String);
begin
  if FStream = nil then
    FStream := CreateStream;

  ParseURL(vURL, FProtocol, FHost, FPort, FPath);
  Stream.Address := Host;
  Stream.Port := Port;

  Stream.Options := Stream.Options + [soNoDelay];
  if SameText(Protocol, 'https') then
    Stream.Options := Stream.Options + [soSSL, soWaitBeforeRead]
  else
    Stream.Options := Stream.Options - [soSSL];

  if KeepAlive then
  begin
    FRequest['Connection'] := 'Keep-Alive';
    //Keep-Alive: timeout=1200
  end
  else
    FRequest['Connection'] := 'close';
  Stream.Connect;
end;

function TmnHttpClient.Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean;
begin
  Connect(vUrl);
  Request.SendPost(vData, vCount);
  //
  Result := Stream.Connected;
  if Result then
    Receive;
end;

function TmnHttpClient.CreateStream: TmnHttpStream;
begin
  Result := TmnHttpStream.Create;
  Result.EndOfLine := sWinEndOfLine;
  Result.ReadTimeout := 5000;
  Result.ConnectTimeout := 5000;
  Result.WriteTimeout := 5000;
  {$if defined(ios) or defined(macos)}
  Result.Options := Result.Options + [soWaitBeforeRead];
  {$endif}
end;

procedure TmnHttpClient.FreeStream;
begin
  FreeAndNil(FStream);
end;

constructor TmnHttpClient.Create;
begin
  inherited;
  FRequest := TmnHttpRequest.Create(Self);
  FResponse := TmnHttpResponse.Create(Self);
  FCookies := TmnParams.Create;
  FUserAgent := sUserAgent;
end;

destructor TmnHttpClient.Destroy;
begin
  FreeStream;
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FCookies);
  inherited;
end;

{ TmnHttpClient }

function TmnHttpClient.Open(const vURL: UTF8String; SendAndReceive: Boolean): Boolean;
begin
  Connect(vUrl);
  if SendAndReceive then
  begin
    Request.SendGet;
    if Stream.Connected then
      Receive;
  end;
  Result := Stream.Connected;
end;

procedure TmnHttpClient.ReceiveStream(AStream: TStream);
begin
  if KeepAlive then //TODO check if response.KeepAlive
    FStream.ReadStream(AStream, Response.ContentLength)
  else
    FStream.ReadStream(AStream);
end;

procedure TmnHttpClient.ReceiveStream(AStream: TStream; Count: Integer);
begin
  FStream.ReadStream(AStream, Count);
end;

procedure TmnHttpClient.Receive;
var
  s: string;
begin
  Response.Receive;

  s := Response.Header['Set-Cookie'];
  Cookies.Delimiter := ';';
  Cookies.AsString := s;


  if Response.Items['Content-Encoding'].Have('gzip', [',']) then
    CompressClass := TmnGzipStreamProxy
  else if Response.Items['Content-Encoding'].Have('deflate', [',']) then
    CompressClass := TmnDeflateStreamProxy;

  if CompressClass <> nil then
  begin
    if CompressProxy <> nil then
      CompressProxy.Enable
    else
    begin
      CompressProxy := CompressClass.Create([cprsRead], 9);
      Stream.AddProxy(CompressProxy);
    end;
  end
  else
  begin
    if CompressProxy <> nil then
      CompressProxy.Disable;
  end;
end;

procedure TmnHttpClient.ReceiveMemoryStream(AStream: TStream);
begin
  ReceiveStream(AStream);
  AStream.Seek(0, soFromBeginning);
end;

procedure TmnHttpClient.Disconnect;
begin
  if FStream <> nil then
    FStream.Disconnect;
  FreeStream;
end;

function TmnHttpClient.GetStream(const vURL: UTF8String; OutStream: TStream): TFileSize;
begin
  if Open(vURL) then
    Result := FStream.ReadStream(OutStream)
  else
    Result := 0;
end;

function TmnHttpClient.GetFile(const vURL: UTF8String; OutFileName: UTF8String): TFileSize;
var
  f: TFileStream;
begin
  f := TFileStream.Create(OutFileName, fmCreate or fmShareDenyWrite);
  try
    Result := GetStream(vURL, f);
  finally
    f.Free;
  end;
end;

function TmnHttpClient.GetFileSize(vURL: string; out FileSize: TFileSize): Boolean;
var
  aSizeStr: string;
begin
  Result := Open(vURL, False);
  try
    Request.SendHead;
    Receive;
    aSizeStr := Response.Header['Content-Length'];
    FileSize := StrToInt64(aSizeStr);
  finally
    Disconnect;
  end;
end;

procedure TmnHttpClient.GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
begin
  GetStream(vURL, OutStream);
  OutStream.Seek(0, soFromBeginning);
end;

procedure TmnHttpClient.SendFile(const vURL: UTF8String; AFileName: UTF8String);
begin
  //TODO
end;

end.
