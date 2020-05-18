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

  TmnHttpUrl = record
    Protocol: string;
    Host: string;
    Port: string;
    Params: string;
    procedure Create(const vURL: string); overload;
    procedure Create(const vProtocol, vHost, vPort, vParams: string); overload;

    procedure Encode(const vURL: string; var vProtocol, vHost, vPort,
      vParams: string);
    function GetUrlPart(var vPos: PChar; var vCount: Integer; const vTo: string;
      const vStops: TSysCharSet = []): string;

    function GetProtocol: string;
    function GetHost: string;
    function GetPort: string;
    function GetParams: string;
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
    procedure DoReadHeaders; virtual;
    procedure DoWriteHeaders; virtual;
  public
    constructor Create(Stream: TmnBufferStream); virtual;
    destructor Destroy; override;
    procedure ReadHeaders;
    procedure WriteHeaders;
    property Headers: TStringList read FHeaders write FHeaders;
    property Cookies: TStringList read FCookies write FCookies;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  { TmnHttpRequest }

  TmnHttpRequest = class(TmnCustomHttpHeader)
  private
    FAcceptCharSet: string;
    FAcceptEncoding: string;
    FReferer: string;
    FHost: string;
    FAcceptLanguage: string;
    FAccept: string;
    FUserAgent: string;
    FConnection: string;
  protected
    procedure DoReadHeaders; override;
    procedure DoWriteHeaders; override;
  public
    function Write(const Buffer; Count: Longint): Longint;
    function Writeln(const Value: string): Cardinal;
    property Accept: string read FAccept write FAccept;
    property AcceptCharSet: string read FAcceptCharSet write FAcceptCharSet;
    property AcceptEncoding: string read FAcceptEncoding write FAcceptEncoding;
    property AcceptLanguage: string read FAcceptLanguage write FAcceptLanguage;
    property Host: string read FHost write FHost;
    property Referer: string read FReferer write FReferer;
    property UserAgent: string read FUserAgent write FUserAgent;
    property Connection: string read FConnection write FConnection;
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: string;
    FServer: string;
    FContentLength: Integer;
  protected
    procedure DoReadHeaders; override;
    procedure DoWriteHeaders; override;
  public
    function ReadLn: string;
    procedure ReadBuffer(var Buffer; Count: Integer);
    property Location: string read FLocation write FLocation;
    property Server: string read FServer write FServer;
    property ContentLength: Integer read FContentLength write FContentLength;
  end;

  { TmnCustomHttpClient }

  // TmnCustomHttpClient = class(TmnClient)
  TmnCustomHttpClient = class(TObject)
  private
    FStream: TmnClientSocketStream;
    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;
    FPort: string;
    FAddress: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;
    property Request: TmnHttpRequest read FRequest write FRequest;
    property Response: TmnHttpResponse read FResponse write FResponse;
    property Address: string read FAddress write FAddress;
    property Port: string read FPort write FPort;
  end;

  { TmnHttpClient }

  TmnHttpClient = class(TmnCustomHttpClient)
  public
    procedure Get(const vURL: string);
  end;

  TmnCustomHttpStream = class(TmnClientSocketStream)
  private
    FParams: string;
    FProtocol: string;
    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;
  protected
    function CreateSocket(out vErr: Integer): TmnCustomSocket; override;
    function GetSize: Int64; override;
  public
    constructor Create(const vAddress, vPort: string);
    destructor Destroy; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    property Request: TmnHttpRequest read FRequest;
    property Response: TmnHttpResponse read FResponse;

    property Protocol: string read FProtocol write FProtocol;
    property Params: string read FParams write FParams;
    procedure Get(const vURL: string);
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

procedure TmnCustomHttpHeader.DoReadHeaders;
begin
  with FHeaders do
  begin
    Values['Date'] := '';
    Values['Last-Modified'] := '';
    Values['Expires'] := '';
  end;
end;

procedure TmnCustomHttpHeader.DoWriteHeaders;
begin
  FHeaders.Clear;
  with FHeaders do
  begin
    FDate := 0;;
    FLastModified := 0;;
    FExpires := 0;
  end;

end;

procedure TmnCustomHttpHeader.ReadHeaders;
var
  s: string;
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
  DoReadHeaders;
  s := Headers.Values['Set-Cookie'];
  if s <> '' then
    StrToStrings(s, Cookies, [';'], []);
end;

procedure TmnCustomHttpHeader.WriteHeaders;
var
  I: Integer;
  s: string;
begin
  DoWriteHeaders;
  for I := 0 to Headers.Count - 1 do
    FStream.WriteLine(Headers[I]);
  for I := 0 to Cookies.Count - 1 do
    s := Cookies[I] + ';';
  if s <> '' then
    Writeln('Cookie: ' + s);
  FStream.WriteLine;
end;

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Connected: Boolean;
begin
  Result := FStream.Connected;
end;

constructor TmnCustomHttpClient.Create;
begin
  inherited;
  FStream := TmnClientSocketStream.Create('', '80');
  FStream.EndOfLine := sWinEndOfLine;
  FRequest := TmnHttpRequest.Create(FStream);
  FResponse := TmnHttpResponse.Create(FStream);
end;

destructor TmnCustomHttpClient.Destroy;
begin
  FStream.Free;
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

{ TmnHttpRequest }

procedure TmnHttpRequest.DoReadHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FAccept := Values['Accept'];
    FAcceptCharSet := Values['Accept-CharSet'];
    FAcceptEncoding := Values['Accept-Encoding'];
    FAcceptLanguage := Values['Accept-Language'];
    FHost := Values['Host'];
    FReferer := Values['Referer'];
    FUserAgent := Values['User-Agent'];
    FConnection := Values['Connection'];
  end;
end;

procedure TmnHttpRequest.DoWriteHeaders;
begin
  inherited;
  with FHeaders do
  begin
    Values['Accept'] := FAccept;
    Values['Accept-CharSet'] := FAcceptCharSet;
    Values['Accept-Encoding'] := FAcceptEncoding;
    Values['Accept-Language'] := FAcceptLanguage;
    Values['Host'] := FHost;
    Values['Referer'] := FReferer;
    Values['User-Agent'] := FUserAgent;
    Values['Connection'] := FConnection;
  end;
end;

function TmnHttpRequest.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

function TmnHttpRequest.Writeln(const Value: string): Cardinal;
begin
  Result := FStream.WriteLine(Value);
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.DoReadHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FLocation := Values['Location'];
    FServer := Values['Server'];
    FContentLength := StrToIntDef(Values['Content-Length'], 0);
  end;
end;

procedure TmnHttpResponse.DoWriteHeaders;
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

function TmnHttpResponse.ReadLn: string;
begin
  FStream.ReadLine(Result, True);
end;

{ TmnHttpClient }

procedure TmnHttpClient.Get(const vURL: string);
var
  u: TmnHttpUrl;
begin
  u.Create(vURL);
  Request.Host := u.GetHost;
  Request.UserAgent := 'Mozilla/4.0';

  FStream.Address := u.GetHost; // Should be published
  FStream.Port := u.GetPort; // Should be published
  FStream.Connect;
  FStream.WriteLine('GET' + ' ' + u.GetParams + ' ' + 'HTTP/1.0');
  FRequest.WriteHeaders;
  if FStream.Connected then
    FResponse.ReadHeaders;
end;

{ TmnHttpUrl }

procedure TmnHttpUrl.Create(const vURL: string);
begin
  Encode(vURL, Protocol, Host, Port, Params);
end;

procedure TmnHttpUrl.Create(const vProtocol, vHost, vPort, vParams: string);
begin
  Protocol := vProtocol;
  Host := vHost;
  Port := vPort;
  Params := vParams;
end;

procedure TmnHttpUrl.Encode(const vURL: string; var vProtocol, vHost, vPort,
  vParams: string);
var
  p: PChar;
  l: Integer;
begin
  vProtocol := '';
  vHost := '';
  vPort := '';
  vParams := '';

  p := PChar(vURL);
  l := Length(vURL);
  if l > 0 then
    vProtocol := GetUrlPart(p, l, '://', ['.']);
  if l > 0 then
  begin
    vHost := GetUrlPart(p, l, ':', ['/']);
    if vHost <> '' then
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
      vHost := GetUrlPart(p, l, '/', []);
      if vHost = '' then
      begin
        SetString(vHost, p, l);
        l := 0;
      end;
    end;
  end;
  if l > 0 then
    SetString(vParams, p, l);
end;

function TmnHttpUrl.GetProtocol: string;
begin
  if Protocol <> '' then
    Result := Protocol
  else
    Result := 'http';
end;

function TmnHttpUrl.GetHost: string;
begin
  Result := Host;
end;

function TmnHttpUrl.GetPort: string;
begin
  if Port <> '' then
    Result := Port
  else
    Result := '80';
end;

function TmnHttpUrl.GetParams: string;
begin
  if Params <> '' then
    Result := '/' + Params
  else
    Result := '/';
end;

function TmnHttpUrl.GetUrlPart(var vPos: PChar; var vCount: Integer; const vTo: string; const vStops: TSysCharSet = []): string;

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

constructor TmnCustomHttpStream.Create(const vAddress, vPort: string);
begin
  inherited Create(vAddress, vPort);

  FRequest := TmnHttpRequest.Create(Self);
  FResponse := TmnHttpResponse.Create(Self);
  Request.UserAgent := 'Mozilla/4.0';

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

procedure TmnCustomHttpStream.Get(const vURL: string);
var
  u: TmnHttpUrl;
begin
  u.Create(vURL);
  Protocol := u.GetProtocol;
  Address := u.GetHost;
  Port := u.GetPort;
  Params := u.GetParams;
  Connect;
  // FRequest.Connection := 'Keep-Alive';
  FRequest.Host := Address;
  Writeln('GET' + ' ' + Params + ' ' + 'HTTP/1.0');
  FRequest.WriteHeaders;
  if Connected then
    FResponse.ReadHeaders;
end;

function TmnCustomHttpStream.GetSize: Int64;
begin
  Result := FResponse.ContentLength;
end;

function TmnCustomHttpStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  Result := 0; // for loading from this stream like Image.loadfrom streaM
end;

end.
