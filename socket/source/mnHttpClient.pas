unit mnHttpClient;
{ **
  *  This file is part of the "Mini Library"
  *
  * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    Jihad Khlaifa <jkhalifa at gmail dot com>
  * }

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  SysUtils, Classes, mnSockets, mnClients, mnStreams, mnUtils;

type

  { TmnHttpUrl }

  TmnHttpUrl = record
    Protocol: UTF8String;
    Address: UTF8String;
    Port: UTF8String;
    Params: UTF8String;
    procedure Create(const vURL: UTF8String); overload;
    procedure Create(const vProtocol, vAddress, vPort, vParams: UTF8String); overload;

    function GetUrlPart(var vPos: PChar; var vCount: Integer; const vTo: UTF8String; const vStops: TSysCharSet = []): UTF8String;
    procedure Parse(const vURL: UTF8String; var vProtocol, vAddress, vPort, vParams: UTF8String);
  end;

  { TmnCustomHttpHeader }

  TmnCustomHttpHeader = class(TObject)
  private
    FStream: TmnBufferStream;
    FDate: TDateTime;
    FLastModified: TDateTime;
    FExpires: TDateTime;
    FHeaders: TStringList;
    FCookies: TStringList;
  protected
    procedure DoSendHeaders; virtual;
    procedure DoReceiveHeaders; virtual;

    procedure SendHeaders;
    procedure ReceiveHeaders;
  public
    constructor Create(Stream: TmnBufferStream); virtual;
    destructor Destroy; override;
    procedure Receive;
    procedure Send;
    property Headers: TStringList read FHeaders write FHeaders;
    property Cookies: TStringList read FCookies write FCookies;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  { TmnHttpRequest }

  TmnHttpRequest = class(TmnCustomHttpHeader)
  private
    FAcceptCharSet: UTF8String;
    FAcceptEncoding: UTF8String;
    FContentLength: Integer;
    FContentType: UTF8String;
    FReferer: UTF8String;
    FAddress: UTF8String;
    FAcceptLanguage: UTF8String;
    FAccept: UTF8String;
    FUserAgent: UTF8String;
    FConnection: UTF8String;
  protected
    procedure DoReceiveHeaders; override;
    procedure DoSendHeaders; override;
  public
    property Accept: UTF8String read FAccept write FAccept;
    property AcceptCharSet: UTF8String read FAcceptCharSet write FAcceptCharSet;
    property AcceptEncoding: UTF8String read FAcceptEncoding write FAcceptEncoding;
    property AcceptLanguage: UTF8String read FAcceptLanguage write FAcceptLanguage;
    property Address: UTF8String read FAddress write FAddress;
    property Referer: UTF8String read FReferer write FReferer;
    property UserAgent: UTF8String read FUserAgent write FUserAgent;
    property ContentType: UTF8String read FContentType write FContentType;
    property ContentLength: Integer read FContentLength write FContentLength;
    property Connection: UTF8String read FConnection write FConnection;
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: UTF8String;
    FServer: UTF8String;
    FContentType: UTF8String;
    FContentLength: Integer;
  protected
    procedure DoReceiveHeaders; override;
    procedure DoSendHeaders; override;
  public
    function ReadLine: UTF8String;
    procedure ReadBuffer(var Buffer; Count: Integer);
    property Location: UTF8String read FLocation write FLocation;
    property Server: UTF8String read FServer write FServer;
    property ContentType: UTF8String read FContentType write FContentType;
    property ContentLength: Integer read FContentLength write FContentLength;
  end;

  { TmnCustomHttpStream }

  TmnCustomHttpStream = class abstract(TmnClientSocket)
  private
    FParams: UTF8String;
    FProtocol: UTF8String;
    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;
    FUserAgent: UTF8String;
  protected
    function CreateSocket(out vErr: Integer): TmnCustomSocket; override;
    function GetSize: Int64; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    property Request: TmnHttpRequest read FRequest;
    property Response: TmnHttpResponse read FResponse;

    property Protocol: UTF8String read FProtocol write FProtocol;
    property Params: UTF8String read FParams write FParams;
    procedure Open(const vURL: UTF8String);
    procedure Close;

    property UserAgent: UTF8String read FUserAgent write FUserAgent;
  end;

  TmnHttpStream = class(TmnCustomHttpStream);

  { TmnCustomHttpClient }

  TmnCustomHttpClient = class abstract(TObject)
  private
    FHttpStream: TmnHttpStream;
    function GetRequest: TmnHttpRequest;
    function GetResponse: TmnHttpResponse;
  protected
    property Stream: TmnHttpStream read FHttpStream;
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;
    property Request: TmnHttpRequest read GetRequest;
    property Response: TmnHttpResponse read GetResponse;
  end;

  { TmnHttpClient }

  TmnHttpClient = class(TmnCustomHttpClient)
  private
    FUserAgent: UTF8String;
  public
    constructor Create;
    //Use it to open connection and keep it connected
    function Connect(const vURL: UTF8String): Boolean;
    procedure Disconnect;
    //This will download the content into a stream and disconnect
    procedure GetStream(const vURL: UTF8String; OutStream: TStream);
    procedure GetFile(const vURL: UTF8String; OutFileName: UTF8String);
    //Just add seek to 0 after getting
    procedure GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
    procedure SendFile(const vURL: UTF8String; AFileName: UTF8String);

    property UserAgent: UTF8String read FUserAgent write FUserAgent;
    property Stream;
  end;

implementation

{ TmnCustomHttpHeader }

constructor TmnCustomHttpHeader.Create(Stream: TmnBufferStream);
begin
  inherited Create;
  FStream := Stream;
  FHeaders := TStringList.Create;
  FHeaders.NameValueSeparator := ':';
  FCookies := TStringList.Create;
end;

destructor TmnCustomHttpHeader.Destroy;
begin
  FHeaders.Free;
  FCookies.Free;
  inherited;
end;

procedure TmnCustomHttpHeader.DoReceiveHeaders;
begin
end;

procedure TmnCustomHttpHeader.DoSendHeaders;
begin
end;

procedure TmnCustomHttpHeader.ReceiveHeaders;
begin
  with FHeaders do
  begin
    Values['Date'] := '';
    Values['Last-Modified'] := '';
    Values['Expires'] := '';
  end;
  DoReceiveHeaders;
end;

procedure TmnCustomHttpHeader.SendHeaders;
begin
  FHeaders.Clear;
  with FHeaders do
  begin
    FDate := 0;;
    FLastModified := 0;;
    FExpires := 0;
  end;
  DoSendHeaders;
end;

procedure TmnCustomHttpHeader.Receive;
var
  s: UTF8String;
begin
  // if FStream.Connected then
  begin
    FStream.ReadLine(s, True);
    s := Trim(s);
    repeat
      Headers.Add(s);
      FStream.ReadLine(s, True);
      s := Trim(s);
    until { FStream.Connected or } (s = '');
  end;
  ReceiveHeaders;
  s := Headers.Values['Set-Cookie'];
  if s <> '' then
    StrToStrings(s, Cookies, [';'], []);
end;

procedure TmnCustomHttpHeader.Send;
var
  I: Integer;
  s: UTF8String;
begin
  SendHeaders;
  for I := 0 to Headers.Count - 1 do
    FStream.WriteLine(Headers[I]);
  for I := 0 to Cookies.Count - 1 do
    s := Cookies[I] + ';';
  if s <> '' then
    FStream.WriteLine('Cookie: ' + s);
  FStream.WriteLine;
end;

{ TmnHttpRequest }

procedure TmnHttpRequest.DoReceiveHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FAccept := Values['Accept'];
    FAcceptCharSet := Values['Accept-CharSet'];
    FAcceptEncoding := Values['Accept-Encoding'];
    FAcceptLanguage := Values['Accept-Language'];
    FAddress := Values['Host'];
    FReferer := Values['Referer'];
    FUserAgent := Values['User-Agent'];
    FConnection := Values['Connection'];
  end;
end;

procedure TmnHttpRequest.DoSendHeaders;
begin
  inherited;
  with FHeaders do
  begin
    Values['Accept'] := FAccept;
    Values['Accept-CharSet'] := FAcceptCharSet;
    Values['Accept-Encoding'] := FAcceptEncoding;
    Values['Accept-Language'] := FAcceptLanguage;
    Values['Host'] := FAddress;
    Values['Referer'] := FReferer;
    Values['User-Agent'] := FUserAgent;
    Values['Connection'] := FConnection;
  end;
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.DoReceiveHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FLocation := Values['Location'];
    FServer := Values['Server'];
    FContentType:= Values['Content-Type'];
    FContentLength := StrToIntDef(Values['Content-Length'], 0);
  end;
end;

procedure TmnHttpResponse.DoSendHeaders;
begin
  inherited;
  with FHeaders do
  begin
    Values['Location'] := FLocation;
    Values['Server'] := FServer;
  end;
end;

procedure TmnHttpResponse.ReadBuffer(var Buffer; Count: Integer);
begin
  FStream.ReadBuffer(Buffer, Count);
end;

function TmnHttpResponse.ReadLine: UTF8String;
begin
  FStream.ReadLine(Result, True);
end;

{ TmnHttpUrl }

procedure TmnHttpUrl.Create(const vURL: UTF8String);
begin
  Parse(vURL, Protocol, Address, Port, Params);
end;

procedure TmnHttpUrl.Create(const vProtocol, vAddress, vPort, vParams: UTF8String);
begin
  Protocol := vProtocol;
  Address := vAddress;
  Port := vPort;
  Params := vParams;
end;

procedure TmnHttpUrl.Parse(const vURL: UTF8String; var vProtocol, vAddress, vPort, vParams: UTF8String);
var
  p: PChar;
  l: Integer;
begin
  vProtocol := '';
  vAddress := '';
  vPort := '';
  vParams := '';

  p := PChar(vURL);
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

  if Port = '' then
  begin
    if SameText(Protocol, 'https') then
      Port := '443'
    else
      Port := '80';
  end;
end;

function TmnHttpUrl.GetUrlPart(var vPos: PChar; var vCount: Integer; const vTo: UTF8String; const vStops: TSysCharSet = []): UTF8String;

  function _IsMatch(vSrc, vDst: PChar; ACount: Integer): Boolean;
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
  p, e, d: PChar;
  aFound: Boolean;
begin
  l := Length(vTo);
  d := PChar(vTo);
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

{ TmnCustomHttpStream }

constructor TmnCustomHttpStream.Create;
begin
  inherited Create('', '');

  FRequest := TmnHttpRequest.Create(Self);
  FResponse := TmnHttpResponse.Create(Self);
  UserAgent := 'Mozilla/4.0';
end;

function TmnCustomHttpStream.CreateSocket(out vErr: Integer): TmnCustomSocket;
begin
  Result := inherited CreateSocket(vErr);
end;

destructor TmnCustomHttpStream.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  inherited;
end;

const
  ProtocolVersion = 'HTTP/1.1';

procedure TmnCustomHttpStream.Open(const vURL: UTF8String);
var
  u: TmnHttpUrl;
begin
  u.Create(vURL);
  Protocol := u.Protocol;
  Port := u.Port;
  Address := u.Address;
  ReadTimeout := 5000;
  if SameText(Protocol, 'https') then
    Options := Options + [soSSL, soWaitBeforeRead]
  else
    Options := Options - [soSSL];
  Params := u.Params;
  Request.UserAgent := UserAgent;
  FRequest.Address := Address;
  // FRequest.Connection := 'Keep-Alive';
  Connect;
  WriteLine('GET ' + Params + ' ' + ProtocolVersion);
  FRequest.Send;
  if Connected then
    FResponse.Receive;
end;

procedure TmnCustomHttpStream.Close;
begin
  Disconnect;
end;

function TmnCustomHttpStream.GetSize: Int64;
begin
  Result := FResponse.ContentLength;
end;

function TmnCustomHttpStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  Result := 0; // for loading from this stream like Image.loadfrom stream
end;

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Connected: Boolean;
begin
  Result := FHttpStream.Connected;
end;

function TmnCustomHttpClient.GetRequest: TmnHttpRequest;
begin
  Result := FHttpStream.Request;
end;

function TmnCustomHttpClient.GetResponse: TmnHttpResponse;
begin
  Result := FHttpStream.Response;
end;

constructor TmnCustomHttpClient.Create;
begin
  inherited;
  FHttpStream := TmnHttpStream.Create;
  FHttpStream.EndOfLine := sWinEndOfLine;
end;

destructor TmnCustomHttpClient.Destroy;
begin
  FHttpStream.Free;
  inherited;
end;

{ TmnHttpClient }

constructor TmnHttpClient.Create;
begin
  inherited Create;
  UserAgent := 'Mozilla/4.0';
end;

function TmnHttpClient.Connect(const vURL: UTF8String): Boolean;
begin
  FHttpStream.Open(vUrl);
  Result := FHttpStream.Connected;
end;

procedure TmnHttpClient.Disconnect;
begin
  FHttpStream.Disconnect;
end;

procedure TmnHttpClient.GetStream(const vURL: UTF8String; OutStream: TStream);
begin
  if Connect(vURL) then
  begin
    FHttpStream.ReadStream(OutStream);
  end;
end;

procedure TmnHttpClient.GetFile(const vURL: UTF8String; OutFileName: UTF8String);
var
  f: TFileStream;
begin
  f := TFileStream.Create(OutFileName, fmCreate or fmShareDenyWrite);
  try
    GetStream(vURL, f);
  finally
    f.Free;
  end;
end;

procedure TmnHttpClient.GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
begin
  GetStream(vURL, OutStream);
  OutStream.Seek(0, fsFromBeginning);
end;

procedure TmnHttpClient.SendFile(const vURL: UTF8String; AFileName: UTF8String);
begin
  //TODO
end;

end.
