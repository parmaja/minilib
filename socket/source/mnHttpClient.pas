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
  SysUtils, Classes, StrUtils, mnOpenSSL,
  mnUtils, mnClasses, mnLogs, mnFields, mnParams, mnModules, mnSockets, mnJobs,
  mnClients, mnStreams, mnStreamUtils;

type

  TmnCustomHttpClient = class;

  { TmnCustomHttpHeader }

  TmnCustomHttpHeader = class(TmodCommunicate)
  private
    FClient: TmnCustomHttpClient;

    FAccept: UTF8String;
    FAcceptCharSet: UTF8String;
    FAcceptLanguage: UTF8String;
    FAcceptEncoding: TStringList;

    FContentLength: Integer;
    FContentType: UTF8String;

    FDate: TDateTime;
    FLastModified: TDateTime;
    FExpires: TDateTime;
    FReferer: UTF8String;

    FKeepAlive: Boolean;
    FChunked: Boolean;
  protected
  public
    constructor Create(AClient: TmnCustomHttpClient); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Accept: UTF8String read FAccept write FAccept;
    property AcceptCharSet: UTF8String read FAcceptCharSet write FAcceptCharSet;
    property AcceptEncoding: TStringList read FAcceptEncoding write FAcceptEncoding;
    property AcceptLanguage: UTF8String read FAcceptLanguage write FAcceptLanguage;

    property ContentType: UTF8String read FContentType write FContentType;
    property ContentLength: Integer read FContentLength write FContentLength;

    property Referer: UTF8String read FReferer write FReferer;

    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Chunked: Boolean read FChunked write FChunked;
    property Client: TmnCustomHttpClient read FClient;
  end;

  { TmnHttpRequest }

  TmnHttpRequest = class(TmnCustomHttpHeader)
  private
    FIsPatch: Boolean;
  protected
    procedure ApplyHeader; virtual;
    procedure SendCommand(Command: string; vData: PByte; vCount: Cardinal);
  public
    procedure Created; override;
    procedure SendGet;
    procedure SendHead;
    procedure SendPost(vData: PByte; vCount: Cardinal);
    procedure SendPatch(vData: PByte; vCount: Cardinal);

    property IsPatch: Boolean read FIsPatch write FIsPatch; //deprecated;
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: UTF8String;
    FServer: UTF8String;
    FHead: UTF8String;
    function GetStatusResult: string;
    function GetStatusVersion: string;
    function GetStatusCode: Integer;
  protected
    procedure RetrieveHeader; virtual;
  public
    procedure Receive;
    property Location: UTF8String read FLocation write FLocation;
    property Server: UTF8String read FServer write FServer;
    property Head: UTF8String read FHead;

    property StatusVersion: string read GetStatusVersion;
    property StatusCode: Integer read GetStatusCode;
    property StatusResult: string read GetStatusResult;
  end;

  { TmnCustomHttpClient }

  TmnCustomHttpClient = class abstract(TObject)
  private
    FCookies: TmnParams;
    FHost: UTF8String;
    FPort: UTF8String;
    FPath: UTF8String;
    FProtocol: UTF8String;
    FUseCompressing: Boolean;
    FUseKeepAlive: TmodKeepAlive;
    FUserAgent: UTF8String;

    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;

    FStream: TmnConnectionStream;
  protected
    ChunkedProxy: TmnChunkStreamProxy;
    CompressProxy: TmnCompressStreamProxy;
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream; virtual; abstract;

    function CreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream;
    procedure FreeStream; virtual;
    procedure Receive; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;

    //Use it to open connection and keep it connected
    procedure Connect(const vURL: UTF8String);
    function Open(const vURL: UTF8String; SendAndReceive: Boolean = True): Boolean;
    function Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean; overload;
    function Post(const vURL: UTF8String; vData: UTF8String): Boolean; overload;

    function Get(const vURL: UTF8String): Boolean;

    function ReadStream(AStream: TStream): TFileSize; overload;
    procedure ReadStream(AStream: TStream; Count: Integer); overload;

    procedure ReceiveStream(AStream: TStream); overload;
    procedure ReceiveMemoryStream(AStream: TStream);
    procedure Disconnect;
    //Some utils
    //This will download the content into a stream and disconnect
    function GetString(const vURL: UTF8String; var OutString: string): TFileSize;
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
    property UseCompressing: Boolean read FUseCompressing write FUseCompressing;
    property UserAgent: UTF8String read FUserAgent write FUserAgent;
    property UseKeepAlive: TmodKeepAlive read FUseKeepAlive write FUseKeepAlive default klvUndefined;
    property Stream: TmnConnectionStream read FStream;
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

  TmnHttpClient = class(TmnCustomHttpClient)
  protected
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream; override;
  end;

  TmnBIOHttpStream = class(TmnConnectionStream)
  protected
    Address: string;
    BindAddress: string;
    Port: string;
    BIOStream: TBIOStreamSSL;
  protected
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
    function GetConnected: Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;

    function WaitToRead(Timeout: Longint): TmnConnectionError; override;
    function WaitToWrite(Timeout: Longint): TmnConnectionError; override;

    procedure Connect; override;
    procedure Disconnect; override;
  end;

  { TmnBIOHttpClient }

  TmnBIOHttpClient = class(TmnCustomHttpClient)
  public
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream; override;
  end;

function HttpDownloadFile(URL: string; FileName: string): TFileSize;
function HttpGetFileSize(URL: string; out FileSize: TFileSize): Boolean;

function BIO_HttpDownloadFile(URL: string; FileName: string): TFileSize;

implementation

const
  ProtocolVersion = 'HTTP/1.1'; //* Capital letter
  sUserAgent = 'Mozilla/5.0';

function HttpDownloadFile(URL: string; FileName: string): TFileSize;
var
  aHttpClient: TmnCustomHttpClient;
begin
  aHttpClient := TmnHttpClient.Create;
  try
    Result := aHttpClient.GetFile(URL, FileName);
  finally
    FreeAndNil(aHttpClient);
  end;
end;

function BIO_HttpDownloadFile(URL: string; FileName: string): TFileSize;
var
  aHttpClient: TmnCustomHttpClient;
begin
  aHttpClient := TmnBIOHttpClient.Create;
  try
    Result := aHttpClient.GetFile(URL, FileName);
  finally
    FreeAndNil(aHttpClient);
  end;
end;

function HttpGetFileSize(URL: string; out FileSize: TFileSize): Boolean;
var
  aHttpClient: TmnCustomHttpClient;
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
  while p <= e do
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
  c: TmnCustomHttpClient;
begin
  c := TmnCustomHttpClient.Create;
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
    if SameText(vProtocol, 'https') or SameText(vProtocol, 'wss') then
      vPort := '443'
    else
      vPort := '80';
  end;
end;

{ TmnCustomHttpHeader }

constructor TmnCustomHttpHeader.Create(AClient: TmnCustomHttpClient);
begin
  inherited Create;
  FClient := AClient;
  FAcceptEncoding := TStringList.Create;
  FAcceptEncoding.Delimiter := ',';
end;

destructor TmnCustomHttpHeader.Destroy;
begin
  FreeAndNil(FAcceptEncoding);
  inherited;
end;

procedure TmnCustomHttpHeader.Clear;
begin
  Header.Clear;
end;

{ TmnHttpRequest }

procedure TmnHttpRequest.ApplyHeader;
begin
  inherited;
  Header['Host'] := Client.Host;
  Header['User-Agent'] := Client.UserAgent;

  Header['Accept'] := Accept;
  Header['Accept-CharSet'] := FAcceptCharSet;
  if Client.UseCompressing then
    Header['Accept-Encoding'] := 'deflate, gzip';
  if FAcceptLanguage<>'' then
    Header['Accept-Language'] := FAcceptLanguage;
  Header['Referer'] := FReferer;
end;

procedure TmnHttpRequest.Created;
begin
  inherited;
end;

procedure TmnHttpRequest.SendPatch(vData: PByte; vCount: Cardinal);
begin
  SendCommand('PATCH', vData, vCount)
end;

procedure TmnHttpRequest.SendPost(vData: PByte; vCount: Cardinal);
begin
  if IsPatch then
    SendCommand('PATCH', vData, vCount)
  else
    SendCommand('POST', vData, vCount);
end;

procedure TmnHttpRequest.SendCommand(Command: string; vData: PByte; vCount: Cardinal);

  procedure _Write(const s: UTF8String); overload;
  begin
    Stream.WriteLineUTF8(s);
    //TFile.AppendAllText('c:\temp\h.Log', s+#13);
  end;

  {$ifndef FPC}
  procedure _Write(const s: String); overload;
  begin
    _Write(UTF8Encode(s));
    //TFile.AppendAllText('c:\temp\h.Log', s+#13);
  end;
  {$endif}

var
  s: UTF8String;
  f: TmnField;
begin
  ApplyHeader;

  _Write(Command + ' ' + Client.Path + ' ' + ProtocolVersion);
  for f in Header do
    if f.AsString <> '' then
      _Write(f.FullString);

  s := UTF8Encode(Client.Cookies.AsString);
  if s <> '' then
    _Write('Cookie: ' + s);

  if (vCount > 0) then
    _Write('Content-Length: ' + IntToStr(vCount));
  _Write('');

  if (vData <> nil) and (vCount > 0) then
    Stream.Write(vData^, vCount);

  //TFile.AppendAllText('c:\temp\h.Log', TEncoding.UTF8.GetString(vData, vCount)+#13);
end;

procedure TmnHttpRequest.SendGet;
begin
  SendCommand('GET', nil, 0);
end;

procedure TmnHttpRequest.SendHead;
begin
  SendCommand('HEAD', nil, 0);
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.RetrieveHeader;
begin
  inherited;
  FLocation := Header['Location'];
  FServer := Header['Server'];
  FContentType:= Header['Content-Type'];
  FContentLength := StrToIntDef(Header['Content-Length'], 0);
  FAccept := Header['Accept'];
  FAcceptCharSet := Header['Accept-CharSet'];
  FAcceptLanguage := Header['Accept-Language'];
  FAcceptEncoding.DelimitedText := Header['Content-Encoding'];
  FChunked := Header.Field['Transfer-Encoding'].Have('chunked', [',']);
  FKeepAlive := SameText(Header['Connection'], 'Keep-Alive');
end;

function TmnHttpResponse.GetStatusCode: Integer;
var
  s: string;
begin
  s := SubStr(Head, ' ', 1);
  Result := StrToIntDef(s, 0);
end;

function TmnHttpResponse.GetStatusResult: string;
begin
  Result := SubStr(Head, ' ', 2); { TODO : to correct use remain text :) }
end;

function TmnHttpResponse.GetStatusVersion: string;
begin
  Result := SubStr(Head, ' ', 0);
end;

procedure TmnHttpResponse.Receive;
var
  s: UTF8String;
begin
  Header.Clear;
  Stream.ReadLine(FHead, True);
  // if FStream.Connected then
  begin
    Stream.ReadLine(s, True);
    s := Trim(s);
    repeat
      Header.AddItem(s, ':', True);
      Stream.ReadLine(s, True);
      s := Trim(s);
    until { FStream.Connected or } (s = '');
  end;
  RetrieveHeader;
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

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Connected: Boolean;
begin
  Result := FStream.Connected;
end;

procedure TmnCustomHttpClient.Connect(const vURL: UTF8String);
begin
  if FStream = nil then
    FStream := CreateStream(vURL, FProtocol, FHost, FPort, FPath);

  case UseKeepAlive of
    klvUndefined: ; //TODO
    klvKeepAlive:
    begin
      Request.Header['Connection'] := 'Keep-Alive';
      //Keep-Alive: timeout=1200
    end;
    klvClose:
      Request.Header['Connection'] := 'close';
  end;
  Stream.Connect;
end;

function TmnCustomHttpClient.Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean;
begin
  Connect(vUrl);

  Request.SendPost(vData, vCount);
  //
  Result := Stream.Connected;
  if Result then
    Receive;
end;

function TmnCustomHttpClient.CreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream;
begin
  Result := DoCreateStream(vURL, vProtocol, vAddress, vPort, vParams);
  FRequest.SetStream(Result);
  FResponse.SetStream(Result);
end;

procedure TmnCustomHttpClient.FreeStream;
begin
  FRequest.SetStream(nil);
  FResponse.SetStream(nil);
  ChunkedProxy := nil;
  CompressProxy := nil;
  FreeAndNil(FStream);
end;

constructor TmnCustomHttpClient.Create;
begin
  inherited;
  FRequest := TmnHttpRequest.Create(Self);
  FResponse := TmnHttpResponse.Create(Self);
  FCookies := TmnParams.Create;
  FCookies.Delimiter := ';';
  FUserAgent := sUserAgent;
end;

destructor TmnCustomHttpClient.Destroy;
begin
  FreeStream;
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FCookies);
  inherited;
end;

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Open(const vURL: UTF8String; SendAndReceive: Boolean): Boolean;
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

function TmnCustomHttpClient.Post(const vURL: UTF8String; vData: UTF8String): Boolean;
begin
  Result := Post(vURL, PByte(vData), Length(vData));
end;

procedure TmnCustomHttpClient.ReceiveStream(AStream: TStream);
begin
  ReadStream(AStream);
end;

procedure TmnCustomHttpClient.ReadStream(AStream: TStream; Count: Integer);
begin
  FStream.ReadStream(AStream, Count);
end;

function TmnCustomHttpClient.ReadStream(AStream: TStream): TFileSize;
begin
  if Response.Chunked and (Response.ContentLength=0) then
    Result := FStream.ReadStream(AStream, -1)
  else if Response.KeepAlive then
  begin
    // and (Response.ContentLength<>0) nop and Response.ContentLength=0 checked in read stream
    Result := FStream.ReadStream(AStream, Response.ContentLength);
  end
  else
    Result := FStream.ReadStream(AStream, -1); //read complete stream
end;

procedure TmnCustomHttpClient.Receive;
var
  s: string;
  aCompressClass: TmnCompressStreamProxyClass;
begin
  Response.Receive;

  s := Response.Header['Set-Cookie'];
  Cookies.Delimiter := ';';
  Cookies.AsString := s;

  if Response.Chunked then
  begin
    if ChunkedProxy <> nil then
      ChunkedProxy.Enable
    else
    begin
      ChunkedProxy := TmnChunkStreamProxy.Create;
      Stream.AddProxy(ChunkedProxy);
    end;
  end
  else
  begin
    if ChunkedProxy <> nil then
      ChunkedProxy.Disable;
  end;

  if Response.Header.Field['Content-Encoding'].Have('gzip', [',']) then
    aCompressClass := TmnGzipStreamProxy
  else if Response.Header.Field['Content-Encoding'].Have('deflate', [',']) then
    aCompressClass := TmnDeflateStreamProxy
  else
    aCompressClass := nil;

  if aCompressClass <> nil then
  begin
    if CompressProxy <> nil then
      CompressProxy.Enable
    else
    begin
      CompressProxy := aCompressClass.Create([cprsRead], 9);
      Stream.AddProxy(CompressProxy);
    end;
  end
  else
  begin
    if CompressProxy <> nil then
      CompressProxy.Disable;
  end;
end;

procedure TmnCustomHttpClient.ReceiveMemoryStream(AStream: TStream);
begin
  ReceiveStream(AStream);
  AStream.Seek(0, soFromBeginning);
end;

procedure TmnCustomHttpClient.Disconnect;
begin
  if FStream <> nil then
    FStream.Disconnect;
  FreeStream;
end;

function TmnCustomHttpClient.GetStream(const vURL: UTF8String; OutStream: TStream): TFileSize;
begin
  if Open(vURL) then
    Result := ReadStream(OutStream)
  else
    Result := 0;
end;

function TmnCustomHttpClient.GetString(const vURL: UTF8String; var OutString: string): TFileSize;
var
  m: TMemoryStream;
  b: TBytes;
begin
  m := TMemoryStream.Create;
  try
    GetStream(vURL, m);

    SetLength(b, m.Size);
    if m.Size<>0 then
      Move(PByte(m.Memory)^, b[0], m.Size);
    OutString := TEncoding.UTF8.GetString(b);
    Result := m.Size;
  finally
    m.Free;
  end;
end;

function TmnCustomHttpClient.Get(const vURL: UTF8String): Boolean;
begin
  Connect(vUrl);
  Request.SendGet;
  //
  Result := Stream.Connected;
  if Result then
    Receive;
end;

function TmnCustomHttpClient.GetFile(const vURL: UTF8String; OutFileName: UTF8String): TFileSize;
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

function TmnCustomHttpClient.GetFileSize(vURL: string; out FileSize: TFileSize): Boolean;
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

procedure TmnCustomHttpClient.GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
begin
  GetStream(vURL, OutStream);
  OutStream.Seek(0, soFromBeginning);
end;

procedure TmnCustomHttpClient.SendFile(const vURL: UTF8String; AFileName: UTF8String);
begin
  //TODO
end;

{ TmnHttpClient }

function TmnHttpClient.DoCreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream;
var
  aStream: TmnHttpStream;
begin
  aStream := TmnHttpStream.Create;

  aStream.EndOfLine      := sWinEndOfLine;
  aStream.ReadTimeout    := 30000;
  aStream.ConnectTimeout := 5000;
  aStream.WriteTimeout   := 5000;
  aStream.Options := aStream.Options + [soWaitBeforeRead];

  ParseURL(vURL, FProtocol, FHost, FPort, FPath);
  aStream.Address := Host;
  aStream.Port := Port;

  aStream.Options := aStream.Options + [soNoDelay];
  if SameText(Protocol, 'https') or SameText(Protocol, 'wss') then
    aStream.Options := aStream.Options + [soSSL, soWaitBeforeRead]
  else
    aStream.Options := aStream.Options - [soSSL];

  Result := aStream;
end;

{ TmnBIOHttpClient }

function TmnBIOHttpClient.DoCreateStream(const vURL: UTF8String; out vProtocol, vAddress, vPort, vParams: UTF8String): TmnConnectionStream;
var
  aStream: TmnBIOHttpStream;
begin
  aStream := TmnBIOHttpStream.Create;

  aStream.EndOfLine      := sWinEndOfLine;
  aStream.ReadTimeout    := 5000;
  aStream.ConnectTimeout := 5000;
  aStream.WriteTimeout   := 5000;
  //aStream.Options := aStream.Options + [soWaitBeforeRead];

  ParseURL(vURL, FProtocol, FHost, FPort, FPath);
  aStream.Address := Host;
  aStream.Port := Port;

{  aStream.Options := aStream.Options + [soNoDelay];
  if SameText(Protocol, 'https') or SameText(Protocol, 'wss') then
    aStream.Options := aStream.Options + [soSSL, soWaitBeforeRead]
  else
    aStream.Options := aStream.Options - [soSSL];

  if SameText(Protocol, 'ws') or SameText(Protocol, 'wss') then
    aStream.Options := aStream.Options + [soWebsocket];
 }
  Result := aStream;
end;

{ TmnBIOHttpStream }

procedure TmnBIOHttpStream.Connect;
begin
  inherited;
  BIOStream := TBIOStreamSSL.Create;
  BIOStream.SetHost(Address, Port);
  BIOStream.Connect;
end;

constructor TmnBIOHttpStream.Create;
begin
  inherited Create;
end;

destructor TmnBIOHttpStream.Destroy;
begin
  FreeAndNil(BIOStream);
  inherited;
end;

procedure TmnBIOHttpStream.Disconnect;
begin
  BIOStream.Disconnect;
  inherited;
end;

function TmnBIOHttpStream.DoRead(var Buffer; Count: Longint): Longint;
begin
  Result := BIOStream.Read(Buffer, Count);
end;

function TmnBIOHttpStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  Result := BIOStream.Write(Buffer, Count);
end;

function TmnBIOHttpStream.GetConnected: Boolean;
begin
  Result := BIOStream.Connected;
end;

function TmnBIOHttpStream.WaitToRead(Timeout: Longint): TmnConnectionError;
begin
  Result := cerSuccess;
  exit;

  if BIOStream.GetSSL.Active then //testing
  begin
    if BIOStream.GetSSL.Pending then
    begin
      Result := cerSuccess;
      exit;
    end;
  end;
end;

function TmnBIOHttpStream.WaitToWrite(Timeout: Longint): TmnConnectionError;
begin
  Result := cerSuccess;
  exit;
end;

end.
