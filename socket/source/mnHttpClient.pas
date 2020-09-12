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
  SysUtils, Classes, StrUtils,
  mnFields, mnConfigs, mnModules, mnSockets, mnClients, mnStreams, mnUtils;

type

  TmnCustomHttpStream = class;

  { TmnCustomHttpHeader }

  TmnCustomHttpHeader = class(TObject)
  private
    FAccept: UTF8String;
    FAcceptCharSet: UTF8String;
    FAcceptEncoding: UTF8String;
    FAcceptLanguage: UTF8String;
    FConnection: UTF8String;
    FContentLength: Integer;
    FContentType: UTF8String;

    FDate: TDateTime;
    FKeepAlive: Boolean;
    FLastModified: TDateTime;
    FExpires: TDateTime;

    FHeaders: TmnHeader;

    FStream: TmnCustomHttpStream;
  protected
  public
    constructor Create(Stream: TmnCustomHttpStream); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Headers: TmnHeader read FHeaders write FHeaders;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Accept: UTF8String read FAccept write FAccept;
    property AcceptCharSet: UTF8String read FAcceptCharSet write FAcceptCharSet;
    property AcceptEncoding: UTF8String read FAcceptEncoding write FAcceptEncoding;
    property AcceptLanguage: UTF8String read FAcceptLanguage write FAcceptLanguage;
    property ContentType: UTF8String read FContentType write FContentType;
    property ContentLength: Integer read FContentLength write FContentLength;
    property Connection: UTF8String read FConnection write FConnection;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
  end;

  { TmnHttpRequest }

  TmnHttpRequest = class(TmnCustomHttpHeader)
  private
    FAccept: UTF8String;
    FReferer: UTF8String;
    FUserAgent: UTF8String;
  protected
    procedure SendHeaders; virtual;
  public
    constructor Create(Stream: TmnCustomHttpStream); override;
    procedure Send;

    property UserAgent: UTF8String read FUserAgent write FUserAgent;
    property Referer: UTF8String read FReferer write FReferer;
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: UTF8String;
    FServer: UTF8String;
  protected
    function ReadLine: UTF8String;
    procedure ReceiveHeaders; virtual;
  public
    procedure Receive;

    procedure ReadBuffer(var Buffer; Count: Integer);
    property Location: UTF8String read FLocation write FLocation;
    property Server: UTF8String read FServer write FServer;
  end;

  { TmnCustomHttpStream }

  TmnCustomHttpStream = class abstract(TmnClientSocket)
  private
    FCookies: TmnParams;
    FParams: UTF8String;
    FProtocol: UTF8String;

    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;
  protected
    function CreateSocket(out vErr: Integer): TmnCustomSocket; override;
    function GetSize: Int64; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    property Request: TmnHttpRequest read FRequest;
    property Response: TmnHttpResponse read FResponse;
    property Cookies: TmnParams read FCookies write FCookies;

    property Protocol: UTF8String read FProtocol write FProtocol;
    property Params: UTF8String read FParams write FParams;
    procedure Open(const vURL: UTF8String);
    procedure Close;
  end;

  TmnHttpStream = class(TmnCustomHttpStream);

  { TmnCustomHttpClient }

  TmnCustomHttpClient = class abstract(TObject)
  private
    FCompressing: Boolean;
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
    property Compressing: Boolean read FCompressing write FCompressing; //TODO
  end;

  { TmnHttpClient }

  TmnHttpClient = class(TmnCustomHttpClient)
  private
  public
    constructor Create;
    //Use it to open connection and keep it connected
    function Connect(const vURL: UTF8String; SendAndReceive: Boolean = True): Boolean;
    procedure Disconnect;
    //This will download the content into a stream and disconnect
    procedure GetStream(const vURL: UTF8String; OutStream: TStream);
    procedure GetFile(const vURL: UTF8String; OutFileName: UTF8String);
    //Just add seek to 0 after getting
    procedure GetMemoryStream(const vURL: UTF8String; OutStream: TMemoryStream);
    procedure SendFile(const vURL: UTF8String; AFileName: UTF8String);

    property Stream;
  end;

implementation

const
  ProtocolVersion = 'HTTP/1.0';

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

procedure ParseURL(const vURL: UTF8String; out vProtocol, vPort, vAddress, vParams: UTF8String);
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

constructor TmnCustomHttpHeader.Create(Stream: TmnCustomHttpStream);
begin
  inherited Create;
  FStream := Stream;
  FHeaders := TmnHeader.Create;
end;

destructor TmnCustomHttpHeader.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TmnCustomHttpHeader.Clear;
begin
  FHeaders.Clear;
end;

{ TmnHttpRequest }

procedure TmnHttpRequest.SendHeaders;
begin
  inherited;
  with FHeaders do
  begin
    Values['Host'] := FStream.Address;
    Values['User-Agent'] := FUserAgent;
    Values['Connection'] := FConnection;

    Values['Accept'] := FAccept;
    Values['Accept-CharSet'] := FAcceptCharSet;
    Values['Accept-Encoding'] := FAcceptEncoding;
    Values['Accept-Language'] := FAcceptLanguage;
    Values['Referer'] := FReferer;
  end;
end;

constructor TmnHttpRequest.Create(Stream: TmnCustomHttpStream);
begin
  inherited Create(Stream);
  UserAgent := 'Mozilla/5.0';
end;

procedure TmnHttpRequest.Send;
var
  s: UTF8String;
  f: TmnField;
begin
  FHeaders.Clear;
  FStream.WriteLineUTF8('GET ' + FStream.Params + ' ' + ProtocolVersion);
  SendHeaders;
  for f in Headers do
    if f.AsString <> '' then
      FStream.WriteLineUTF8(f.FullString);
  for f in FStream.Cookies do
    s := f.Value + ';';
  if s <> '' then
    FStream.WriteLineUTF8('Cookie: ' + s);
  FStream.WriteLineUTF8(FStream.EndOfLine);
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.ReceiveHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FLocation := Field['Location'].AsString;
    FServer := Field['Server'].AsString;
    FContentType:= Field['Content-Type'].AsString;
    FContentLength := StrToIntDef(Trim(Field['Content-Length'].AsString), 0);
    FAccept := Field['Accept'].AsString;
    FAcceptCharSet := Field['Accept-CharSet'].AsString;
    FAcceptEncoding := Field['Accept-Encoding'].AsString;
    FAcceptLanguage := Field['Accept-Language'].AsString;
    FConnection := Field['Connection'].AsString;
  end;
end;

procedure TmnHttpResponse.Receive;
var
  s: UTF8String;
begin
  FHeaders.Clear;
  // if FStream.Connected then
  begin
    FStream.ReadLine(s, True);
    s := Trim(s);
    repeat
      Headers.AddItem(s, ':', True);
      FStream.ReadLine(s, True);
      s := Trim(s);
    until { FStream.Connected or } (s = '');
  end;
  ReceiveHeaders;
  s := Headers.Field['Set-Cookie'].AsString;
  FStream.Cookies.Delimiter := ';';
  FStream.Cookies.AsString := s;
end;

procedure TmnHttpResponse.ReadBuffer(var Buffer; Count: Integer);
begin
  FStream.ReadBuffer(Buffer, Count);
end;

function TmnHttpResponse.ReadLine: UTF8String;
begin
  FStream.ReadLine(Result, True);
end;

{ TmnCustomHttpStream }

constructor TmnCustomHttpStream.Create;
begin
  inherited Create('', '');

  FRequest := TmnHttpRequest.Create(Self);
  FResponse := TmnHttpResponse.Create(Self);
  FCookies := TmnParams.Create;
end;

function TmnCustomHttpStream.CreateSocket(out vErr: Integer): TmnCustomSocket;
begin
  Result := inherited CreateSocket(vErr);
end;

destructor TmnCustomHttpStream.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FCookies);
  inherited;
end;

procedure TmnCustomHttpStream.Open(const vURL: UTF8String);
var
  aProtocol, aPort, aAddress, aParams: utf8string;
begin
  ParseURL(vURL, aProtocol, aPort, aAddress, aParams);
  Address := aAddress;
  Port := aPort;
  Protocol := aProtocol;
  Params := aParams;

  if SameText(Protocol, 'https') then
    Options := Options + [soSSL, soWaitBeforeRead]
  else
    Options := Options - [soSSL];

  //FRequest.Connection := 'Keep-Alive';
  FRequest.Connection := 'close';
  Connect;
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
end;

function TmnHttpClient.Connect(const vURL: UTF8String; SendAndReceive: Boolean): Boolean;
begin
  Stream.Open(vUrl);
  if SendAndReceive then
  begin
    Stream.Request.Send;
    if Stream.Connected then
      Stream.Response.Receive;
  end;
  Result := Stream.Connected;
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
  OutStream.Seek(0, soFromBeginning);
end;

procedure TmnHttpClient.SendFile(const vURL: UTF8String; AFileName: UTF8String);
begin
  //TODO
end;

end.
