unit mnHttpClient;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Jihad Khlaifa <jkhalifa at gmail dot com>
 *}

{$h+}{$m+}
{$ifdef fpc}
{$mode objfpc}
{$endif}

interface

uses
  SysUtils, Classes, mnSockets, mnClients, mnSocketStreams, mnStreams, mnUtils;

type
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
  end;

  { TmnHttpResponse }

  TmnHttpResponse = class(TmnCustomHttpHeader)
  private
    FLocation: string;
    FServer: string;
  protected
    procedure DoReadHeaders; override;
    procedure DoWriteHeaders; override;
  public
    function ReadLn: string;
    procedure ReadBuffer(var Buffer; Count: Integer);
    property Location: string read FLocation write FLocation;
    property Server: string read FServer write FServer;
  end;

  { TmnCustomHttpClient }

  TmnCustomHttpClient = class(TmnClient)
  private
    FStream: TmnClientStream;
    FRequest: TmnHttpRequest;
    FResponse: TmnHttpResponse;
    FPort: string;
    FAddress: string;
  public
    constructor Create(AOwner: TComponent); override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Get(const vUrl: string);
  end;

implementation

const
  sEOL = #$A;

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
  //if FStream.Connected then
  begin
    s := Trim(FStream.ReadLine(sEOL));
    repeat
      Headers.Add(s);
      s := Trim(FStream.ReadLine(sEOL));
    until not {FStream.Connected or} (s = '');
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
    FStream.WriteLine(Headers[I], sEOL);
  for I := 0 to Cookies.Count - 1 do
    s := Cookies[I] + ';';
  if s <> '' then
    WriteLn('Cookie: ' + s);
  FStream.WriteLn(sEOL);
end;

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Connected: Boolean;
begin
  Result := FStream.Connected;
end;

constructor TmnCustomHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  //WallSocket.Startup;
  FStream := TmnClientStream.Create;
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
  end;
end;

function TmnHttpRequest.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count); 
end;

function TmnHttpRequest.Writeln(const Value: string): Cardinal;
begin
  Result := FStream.WriteLine(Value, sEOL);
end;

{ TmnHttpResponse }

procedure TmnHttpResponse.DoReadHeaders;
begin
  inherited;
  with FHeaders do
  begin
    FLocation := Values['Location'];
    FServer := Values['Server'];
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
  Result := FStream.ReadLine(sEOL);
end;

{ TmnHttpClient }

constructor TmnHttpClient.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TmnHttpClient.Destroy;
begin
  inherited;
end;

procedure TmnHttpClient.Get(const vUrl: string);
begin
  FStream.Address := Request.Host; //Should be published
  FStream.Port := '80'; //Should be published
  FStream.Connect;
  FStream.WriteLn('GET' + ' ' + vUrl +  ' ' + 'HTTP/1.0');
  FRequest.WriteHeaders;
  if FStream.Connected then
    FResponse.ReadHeaders;
end;

end.

