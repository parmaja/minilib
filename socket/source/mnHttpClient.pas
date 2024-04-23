unit mnHttpClient;
{ **
  *  This file is part of the "Mini Library"
  *
  * @license   MIT
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    Zaher Dirkey zaherdirkey
  * @author    Belal Hamed <belal, belalhamed@gmail.com>
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
  mnWebModules, mnClients, mnStreams, mnStreamUtils;

type

  TmnCustomHttpClient = class;

  { TmnCustomHttpClient }

  TmnCustomHttpClient = class abstract(TmnCustomClientCommand)
  private
    FPort: UTF8String;
    FPath: UTF8String;
    FProtocol: UTF8String;

    FStream: TmnConnectionStream;
    function GetRequest: TmodHttpRequest;
    function GetRespond: TmodHttpRespond;
  protected
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream; virtual; abstract;

    function CreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream;

    procedure FreeStream; virtual;
    procedure Receive; virtual;

    procedure SendCommand(Command: string; vData: PByte; vCount: Cardinal);
    procedure SendPatch(vData: PByte; vCount: Cardinal);
    procedure SendPost(vData: PByte; vCount: Cardinal);
    procedure SendGet;
    procedure SendHead;

    function CreateRequest(AStream: TmnConnectionStream): TmodRequest; override;
    function CreateRespond: TmodRespond; override;
    procedure Created; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;


    //Use it to open connection and keep it connected
    procedure Connect(const vURL: UTF8String);
    function Open(const vURL: UTF8String; SendAndReceive: Boolean = True): Boolean;
    function Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean; overload;
    function Post(const vURL: UTF8String; vData: UTF8String): Boolean; overload;

    function Patch(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean; overload;
    function Patch(const vURL: UTF8String; vData: UTF8String): Boolean; overload;

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

    property Protocol: UTF8String read FProtocol write FProtocol;
    property Port: UTF8String read FPort write FPort;
    property Path: UTF8String read FPath write FPath;
    property Stream: TmnConnectionStream read FStream;
    property Request: TmodHttpRequest read GetRequest;
    property Respond: TmodHttpRespond read GetRespond;
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
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream; override;
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
    function DoCreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream; override;
  end;

function HttpDownloadFile(URL: string; FileName: string): TFileSize;
function HttpGetFileSize(URL: string; out FileSize: TFileSize): Boolean;

function BIO_HttpDownloadFile(URL: string; FileName: string): TFileSize;

implementation

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
  c := TmnHttpClient.Create;
  try
    //c.Compressing := True;
    //m.SaveToFile('c:\temp\1.json');
    c.GetFile(URL, FileName);
  finally
    FreeAndNil(c);
  end;
end;

procedure ParseURL(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String);
var
  p: PUTF8Char;
  l: Integer;
begin
  vProtocol := '';
  vHost := '';
  vPort := '';
  vParams := '';

  p := PUtf8Char(vURL);
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

  if not StartsStr('/', vParams) then
    vParams := '/' + vParams;

  if vPort = '' then
  begin
    if SameText(vProtocol, 'https') or SameText(vProtocol, 'wss') then
      vPort := '443'
    else
      vPort := '80';
  end;
end;

procedure TmnCustomHttpClient.SendPatch(vData: PByte; vCount: Cardinal);
begin
  SendCommand('PATCH', vData, vCount)
end;

procedure TmnCustomHttpClient.SendPost(vData: PByte; vCount: Cardinal);
begin
  SendCommand('POST', vData, vCount);
end;

procedure TmnCustomHttpClient.SendCommand(Command: string; vData: PByte; vCount: Cardinal);
begin
  Request.Head := Command + ' ' + Path + ' ' + ProtocolVersion;

  if Request.Use.Compressing<>ovYes then
    Request.ContentLength := vCount;

  Request.SendHeader;

  if (vData <> nil) and (vCount > 0) then
    Stream.Write(vData^, vCount);
end;

procedure TmnCustomHttpClient.SendGet;
begin
  SendCommand('GET', nil, 0);
end;

procedure TmnCustomHttpClient.SendHead;
begin
  SendCommand('HEAD', nil, 0);
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
var
  aHost: UTF8String;
begin
  if FStream = nil then
    FStream := CreateStream(vURL, FProtocol, aHost, FPort, FPath);
  Request.Host := aHost;
  Stream.Connect;
end;

function TmnCustomHttpClient.CreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream;
begin
  Result := DoCreateStream(vURL, vProtocol, vHost, vPort, vParams);
  FRequest.SetStream(Result, True);

  //need set trigger
  //Request.SetStream(Result, True);
  //Respond.SetStream(Result, False);
end;

procedure TmnCustomHttpClient.FreeStream;
begin
//  Request.SetTrigger(False);
//  Request.ChunkedProxy := nil;
  FreeAndNil(FStream);
end;

constructor TmnCustomHttpClient.Create;
begin
  inherited Create;
  FRequest := CreateRequest(nil);
  FRespond := CreateRespond;
end;

procedure TmnCustomHttpClient.Created;
begin
  inherited;
end;

function TmnCustomHttpClient.CreateRequest(AStream: TmnConnectionStream): TmodRequest;
begin
  Result := TmodhttpRequest.Create(Self, AStream);
  Result.Use.AcceptCompressing := ovYes;
end;

function TmnCustomHttpClient.CreateRespond: TmodRespond;
begin
  Result := TmodHttpRespond.Create(Request);
end;

destructor TmnCustomHttpClient.Destroy;
begin
  inherited;
  FreeStream;
end;

{ TmnCustomHttpClient }

function TmnCustomHttpClient.Open(const vURL: UTF8String; SendAndReceive: Boolean): Boolean;
begin
  Connect(vUrl);
  if SendAndReceive then
  begin
    SendGet;
    if Stream.Connected then
      Receive;
  end;
  Result := Stream.Connected;
end;

function TmnCustomHttpClient.Post(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean;
begin
  Connect(vUrl);

  SendPost(vData, vCount);
  //
  Result := Stream.Connected;
  if Result then
  begin
    Receive;
  end;
end;


function TmnCustomHttpClient.Post(const vURL: UTF8String; vData: UTF8String): Boolean;
begin
  Result := Post(vURL, PByte(vData), Length(vData));
end;

function TmnCustomHttpClient.Patch(const vURL: UTF8String; vData: PByte; vCount: Integer): Boolean;
begin
  Connect(vUrl);

  SendPatch(vData, vCount);
  //
  Result := Stream.Connected;
  if Result then
    Receive;
end;

function TmnCustomHttpClient.Patch(const vURL: UTF8String; vData: UTF8String): Boolean;
begin
  Result := Patch(vURL, PByte(vData), Length(vData));
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
  if (Request.ChunkedProxy<>nil) and (Respond.ContentLength = 0) then
    Result := FStream.ReadStream(AStream, -1)
  else if (Respond.ContentLength > 0) and Respond.KeepAlive then //Respond.KeepAlive because we cant use compressed with keeplive or contentlength >0
  begin
    Result := FStream.ReadStream(AStream, Respond.ContentLength);
  end
  else
    Result := FStream.ReadStream(AStream, -1); //read complete stream
end;

procedure TmnCustomHttpClient.Receive;
begin
  Respond.ReceiveHeader;
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
    if m.Size <> 0 then
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
  SendGet;
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
    SendHead;
    Receive;
    aSizeStr := Respond.Header['Content-Length'];
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

function TmnCustomHttpClient.GetRequest: TmodHttpRequest;
begin
  Result := inherited Request as TmodHttpRequest;
end;

function TmnCustomHttpClient.GetRespond: TmodHttpRespond;
begin
  Result := inherited Respond as TmodHttpRespond;
end;

procedure TmnCustomHttpClient.SendFile(const vURL: UTF8String; AFileName: UTF8String);
begin
  //TODO
end;

{ TmnHttpClient }

function TmnHttpClient.DoCreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream;
var
  aStream: TmnHttpStream;
begin
  aStream := TmnHttpStream.Create;

  aStream.EndOfLine      := sWinEndOfLine;
  aStream.ReadTimeout    := 30000;
  aStream.ConnectTimeout := 5000;
  aStream.WriteTimeout   := 5000;
  aStream.Options := aStream.Options + [soWaitBeforeRead];

  ParseURL(vURL, vProtocol, vHost, vPort, FPath);
  aStream.Address := vHost;
  aStream.Port := vPort;

  aStream.Options := aStream.Options + [soNoDelay];
  if SameText(Protocol, 'https') or SameText(Protocol, 'wss') then
    aStream.Options := aStream.Options + [soSSL, soWaitBeforeRead]
  else
    aStream.Options := aStream.Options - [soSSL];

  Result := aStream;
end;

{ TmnBIOHttpClient }

function TmnBIOHttpClient.DoCreateStream(const vURL: UTF8String; out vProtocol, vHost, vPort, vParams: UTF8String): TmnConnectionStream;
var
  aStream: TmnBIOHttpStream;
begin
  aStream := TmnBIOHttpStream.Create;

  aStream.EndOfLine      := sWinEndOfLine;
  aStream.ReadTimeout    := 5000;
  aStream.ConnectTimeout := 5000;
  aStream.WriteTimeout   := 5000;
  //aStream.Options := aStream.Options + [soWaitBeforeRead];

  ParseURL(vURL, vProtocol, vHost, vPort, FPath);
  aStream.Address := vHost;
  aStream.Port := vPort;

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
