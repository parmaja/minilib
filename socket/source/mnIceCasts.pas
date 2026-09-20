unit mnIceCasts;
{$M+}{$H+}
{$ifdef fpc}
{$mode delphi}
{$endif}
{**
 *  This file is part of the "Mini Library"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>

   @Ref:
    https://github.com/CelliesProjects/ESP32_VS1053_Stream/blob/master/src/ESP32_VS1053_Stream.cpp#L121-L319
    https://gist.github.com/ePirat/adc3b8ba00d85b7e3870
 *}

interface

uses
  Classes, SysUtils, SyncObjs,
  mnTypes, mnStreams, mnSockets, mnConnections, mnClients;

type

  TmnIceCastClient = class;

  TmnIceCastState = (
    icStopped, //closed by user or stream ended
    icConnecting, //trying to connect to the server
    icOpening, //connected, sending request and reading headers
    icReady, //streaming audio into Buffer
    icError
  );

  TmnIceCastNotify = procedure(Client: TmnIceCastClient) of object;

  { TmnIceCastSocket }

  TmnIceCastSocket = class(TmnClientSocket)
  public
    //disconnect and release the socket so it can be connected again (used when following HTTP redirects)
    procedure ResetSocket;
  protected
    procedure DoHandleError(var Handle: Boolean; AError: Integer); override;
  end;

  { TmnIceCastConnection }

  TmnIceCastConnection = class(TmnClient)
  private
    FClient: TmnIceCastClient;
    FStream: TmnIceCastSocket;
    FScratch: TMemoryStream;
    FBuffer: TMemoryStream;
    FAddress: string;
    FPort: string;
    FPath: string;
    FProtocol: string;
    FSSL: Boolean;
    FUseMetaData: Boolean;
    FUseMeta: Boolean;
    FMetaInterval: Integer;
    FBytesToMeta: Integer;
    FCode: Integer;
    FHeaders: TStringList;
    FState: TmnIceCastState;
    FError: string;
    FTitle: string;
    FStationName: string;
    FGenre: string;
    FBitrate: string;
    FStationURL: string;
    FContentType: string;
    FReadTimeout: Integer;
    FStreamEnded: Boolean;
    FConnectedTo: string;
    FChunked: Boolean;
    FChunkRemain: Int64;
    FBodyEnded: Boolean;
    procedure SetState(AState: TmnIceCastState; const AMessage: string = '');
    procedure SetTitle(const ATitle: string);
    procedure SendRequest;
    procedure ReadHeaders;
    function ReadBody(var Buffer; Count: Longint): Longint;
    procedure ReadMeta;
    procedure ParseMeta(const AMeta: AnsiString);
    procedure StreamAudio(AChunk: Integer);
    procedure StopByEnd;
    procedure HandleError(E: Exception);
    function GetHeader(const AName: string): string;
  protected
    function GetConnected: Boolean; override;
    procedure Prepare; override;
    procedure Process; override;
    procedure Unprepare; override;
  public
    constructor Create(vOwner: TmnConnections; AClient: TmnIceCastClient);
    destructor Destroy; override;
    procedure CloseStream; //stop connection, called from out of thread
  end;

  { TmnIceCastClient }

  TmnIceCastClient = class(TObject)
  private
    FLock: TCriticalSection;
    FConnection: TmnIceCastConnection;
    FUseMetaData: Boolean;
    FReadTimeout: Integer;
    FOnStateChanged: TmnIceCastNotify;
    FOnTitleChanged: TmnIceCastNotify;
    FURL: string;
    function GetState: TmnIceCastState;
    function GetError: string;
    function GetTitle: string;
    function GetStationName: string;
    function GetGenre: string;
    function GetBitrate: string;
    function GetStationURL: string;
    function GetContentType: string;
    function GetConnectedTo: string;
    function GetResponseCode: Integer;
    function GetStreamEnded: Boolean;
    function GetBuffer: TMemoryStream;
    function GetHeaders: TStringList;
    procedure PostStateChanged;
    procedure PostTitleChanged;
    procedure SetOnStateChanged(aValue: TmnIceCastNotify);
    procedure SetOnTitleChanged(aValue: TmnIceCastNotify);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const vURL: string);
    procedure Close;
    function Connected: Boolean;
    function BufferSize: Int64;
    procedure ClearBuffer;
    //Buffer contain the downloaded audio, protect it with Lock
    property Buffer: TMemoryStream read GetBuffer;
    property Lock: TCriticalSection read FLock;
    property State: TmnIceCastState read GetState;
    property Error: string read GetError;
    property Title: string read GetTitle;
    property StationName: string read GetStationName;
    property Genre: string read GetGenre;
    property Bitrate: string read GetBitrate;
    property StationURL: string read GetStationURL;
    property ContentType: string read GetContentType;
    property ResponseCode: Integer read GetResponseCode;
    property StreamEnded: Boolean read GetStreamEnded;
    property Headers: TStringList read GetHeaders;
    property URL: string read FURL;
    //the real URL used for streaming, after following any redirects
    property ConnectedTo: string read GetConnectedTo;
    //if the server announce Icy-MetaData, default True
    property UseMetaData: Boolean read FUseMetaData write FUseMetaData;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    //Fired in main thread
    property OnStateChanged: TmnIceCastNotify read FOnStateChanged write SetOnStateChanged;
    property OnTitleChanged: TmnIceCastNotify read FOnTitleChanged write SetOnTitleChanged;
  end;

procedure IceCastParseURL(const vURL: string; out vProtocol, vHost, vPort, vParams: string);

implementation

const
  cIceCastReadChunk = 8192;
  cIceCastMaxRedirects = 5;

procedure IceCastParseURL(const vURL: string; out vProtocol, vHost, vPort, vParams: string);
var
  i: Integer;
  aRest: string;
begin
  vProtocol := 'http';
  vHost := '';
  vPort := '';
  vParams := '/';
  aRest := vURL;
  i := Pos('://', aRest);
  if i > 0 then
  begin
    vProtocol := LowerCase(Copy(aRest, 1, i - 1));
    aRest := Copy(aRest, i + 3, MaxInt);
  end;
  i := Pos('/', aRest);
  if i > 0 then
  begin
    vParams := Copy(aRest, i, MaxInt);
    aRest := Copy(aRest, 1, i - 1);
  end;
  i := Pos(':', aRest);
  if i > 0 then
  begin
    vHost := Copy(aRest, 1, i - 1);
    vPort := Copy(aRest, i + 1, MaxInt);
  end
  else
    vHost := aRest;
  if vPort = '' then
  begin
    if SameText(vProtocol, 'https') then
      vPort := '443'
    else
      vPort := '80';
  end;
end;

//Resolve a redirect Location header against the current connection
function ResolveURL(const AProtocol, AHost, APort, APath, ALocation: string): string;
var
  i: Integer;
  path: string;
begin
  Result := '';
  if ALocation = '' then
    Exit
  else if Pos('://', ALocation) > 0 then
    Result := ALocation
  else if (Length(ALocation) > 2) and (ALocation[1] = '/') and (ALocation[2] = '/') then
    Result := AProtocol + ':' + ALocation //protocol-relative
  else if ALocation[1] = '/' then
    Result := AProtocol + '://' + AHost + ':' + APort + ALocation //host-relative
  else
  begin //path-relative
    path := APath;
    i := LastDelimiter('/', path);
    if i > 0 then
      path := Copy(path, 1, i);
    Result := AProtocol + '://' + AHost + ':' + APort + path + ALocation;
  end;
end;

function ParseChunkSize(const s: string): Int64;
var
  i, n: Integer;
  c: Char;
begin
  Result := 0;
  n := Length(s);
  i := 1;
  while (i <= n) and ((s[i] = ' ') or (s[i] = #9)) do
    Inc(i);
  while i <= n do
  begin
    c := s[i];
    if (c >= '0') and (c <= '9') then
      Result := (Result shl 4) + (Ord(c) - Ord('0'))
    else if (c >= 'a') and (c <= 'f') then
      Result := (Result shl 4) + (Ord(c) - Ord('a') + 10)
    else if (c >= 'A') and (c <= 'F') then
      Result := (Result shl 4) + (Ord(c) - Ord('A') + 10)
    else
      Break; //';' extension or anything else
    Inc(i);
  end;
end;

function ExtractMetaValue(const AMeta, AName: string): string;
var
  i, j, n: Integer;
  aKey, aValue: string;
begin
  Result := '';
  i := 1;
  n := Length(AMeta);
  while i <= n do
  begin
    if ((AMeta[i] = '''') or (AMeta[i] = '"') or (AMeta[i] = ' ')) then
    begin
      Inc(i);
      Continue;
    end;
    j := i;
    while (j <= n) and (AMeta[j] <> '=') and (AMeta[j] <> ';') do
      Inc(j);
    if (j <= n) and (AMeta[j] = '=') then
    begin
      aKey := Trim(Copy(AMeta, i, j - i));
      i := j + 1;
      aValue := '';
      while (i <= n) and (AMeta[i] <> ';') do
      begin
        aValue := aValue + AMeta[i];
        Inc(i);
      end;
      aValue := Trim(aValue);
      if Length(aValue) >= 2 then
        if ((aValue[1] = '''') or (aValue[1] = '"')) and (aValue[Length(aValue)] = aValue[1]) then
          aValue := Copy(aValue, 2, Length(aValue) - 2);
      if SameText(aKey, AName) then
      begin
        Result := aValue;
        Exit;
      end;
    end;
    Inc(i);
  end;
end;

{ TmnIceCastSocket }

procedure TmnIceCastSocket.DoHandleError(var Handle: Boolean; AError: Integer);
begin
end;

procedure TmnIceCastSocket.ResetSocket;
begin
  Disconnect;
  FreeSocket;
end;

{ TmnIceCastConnection }

constructor TmnIceCastConnection.Create(vOwner: TmnConnections; AClient: TmnIceCastClient);
begin
  inherited Create(vOwner);
  FreeOnTerminate := False;
  FClient := AClient;
  FBuffer := TMemoryStream.Create;
  FScratch := TMemoryStream.Create;
  FHeaders := TStringList.Create;
  FState := icStopped;
  FReadTimeout := AClient.ReadTimeout;
  FUseMetaData := AClient.UseMetaData;
  IceCastParseURL(AClient.URL, FProtocol, FAddress, FPort, FPath);
  FSSL := SameText(FProtocol, 'https') or SameText(FProtocol, 'wss');
end;

destructor TmnIceCastConnection.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FScratch);
  FreeAndNil(FBuffer);
  inherited;
end;

function TmnIceCastConnection.GetConnected: Boolean;
begin
  Result := (FStream <> nil) and FStream.Connected;
end;

procedure TmnIceCastConnection.SetState(AState: TmnIceCastState; const AMessage: string);
begin
  FClient.Lock.Enter;
  try
    if AMessage <> '' then
      FError := AMessage;
    if FState = AState then
      Exit;
    FState := AState;
  finally
    FClient.Lock.Leave;
  end;
  if (AState = icError) or (AState = icReady) or (AState = icStopped) then
    Queue(FClient.PostStateChanged);
end;

procedure TmnIceCastConnection.SetTitle(const ATitle: string);
var
  vChanged: Boolean;
begin
  FClient.Lock.Enter;
  try
    vChanged := FTitle <> ATitle;
    if vChanged then
      FTitle := ATitle;
  finally
    FClient.Lock.Leave;
  end;
  if vChanged then
    Queue(FClient.PostTitleChanged);
end;

function TmnIceCastConnection.GetHeader(const AName: string): string;
var
  i: Integer;
begin
  Result := '';
  i := FHeaders.IndexOfName(AName);
  if i >= 0 then
    Result := FHeaders.ValueFromIndex[i];
end;

procedure TmnIceCastConnection.SendRequest;
begin
  FStream.WriteLine(UTF8String('GET ' + FPath + ' HTTP/1.1'));
  FStream.WriteLine(UTF8String('Host: ' + FAddress + ':' + FPort));
  if FUseMetaData then
    FStream.WriteLine(UTF8String('Icy-MetaData: 1'));
  FStream.WriteLine(UTF8String('User-Agent: MiniLib.IceCastClients/1.0'));
  FStream.WriteLine(UTF8String('Accept: */*'));
  FStream.WriteLine(UTF8String('Connection: close'));
  FStream.WriteLine(UTF8String(''));
end;

procedure TmnIceCastConnection.ReadHeaders;
var
  line: UTF8String;
  s: string;
  p: Integer;
  aCode: string;
begin
  FCode := 0;
  FHeaders.Clear;
  FMetaInterval := 0;
  while FStream.ReadLine(line) do
  begin
    s := String(line);
    if s = '' then
      Break;
    if FCode = 0 then
    begin
      if (Pos('HTTP/', s) = 1) or (Pos('ICY', s) = 1) then
      begin
        p := Pos(' ', s);
        if p > 0 then
        begin
          aCode := Trim(Copy(s, p + 1, MaxInt));
          p := Pos(' ', aCode);
          if p > 0 then
            aCode := Copy(aCode, 1, p - 1);
          FCode := StrToIntDef(aCode, 0);
        end;
      end;
    end
    else
    begin
      p := Pos(':', s);
      if p > 0 then
        FHeaders.Add(Copy(s, 1, p - 1) + '=' + Trim(Copy(s, p + 1, MaxInt)));
    end;
  end;
  if FCode = 0 then
    raise EmnStreamException.Create('Invalid response from server');
  FContentType := GetHeader('Content-Type');
  FChunked := Pos('chunked', LowerCase(GetHeader('Transfer-Encoding'))) > 0;
  FMetaInterval := StrToIntDef(GetHeader('Icy-MetaInt'), 0);
  FStationName := GetHeader('Icy-Name');
  FGenre := GetHeader('Icy-Genre');
  FBitrate := GetHeader('Icy-Br');
  FStationURL := GetHeader('Icy-URL');
end;

//read the stream body, transparently decoding Transfer-Encoding: chunked,
//so the caller sees the pure audio/metadata stream (and Icy-MetaInt counting works)
function TmnIceCastConnection.ReadBody(var Buffer; Count: Longint): Longint;
var
  p: PByte;
  r, c: Longint;
  s: AnsiString;
begin
  Result := 0;
  if not FChunked then
    Exit(FStream.Read(Buffer, Count));
  p := @Buffer;
  while Count > 0 do
  begin
    if FChunkRemain <= 0 then
    begin
      if FBodyEnded then
        Break;
      if not FStream.ReadLine(s) then
        Break; //connection closed (chunk header line)
      FChunkRemain := ParseChunkSize(String(s));
      if FChunkRemain <= 0 then
      begin
        //last chunk reached, skip possible trailers
        FBodyEnded := True;
        while FStream.ReadLine(s) do
          if s = '' then
            Break;
        Break;
      end;
    end;
    c := Count;
    if FChunkRemain < c then
      c := FChunkRemain;
    r := FStream.Read(p^, c);
    if r <= 0 then
      Break;
    Inc(p, r);
    Inc(Result, r);
    Dec(FChunkRemain, r);
    Dec(Count, r);
    if FChunkRemain = 0 then
      FStream.ReadLine(s); //skip CRLF between chunks
  end;
end;

procedure TmnIceCastConnection.ReadMeta;
var
  n: Integer;
  b: Byte;
  r: Longint;
  aMeta: AnsiString;
begin
  r := ReadBody(b, 1);
  if r <= 0 then
  begin
    StopByEnd;
    Exit;
  end;
  n := Integer(b) * 16;
  if n > 0 then
  begin
    SetLength(aMeta, n);
    r := ReadBody(PAnsiChar(aMeta)^, n);
    if r < n then
      SetLength(aMeta, r);
    if Length(aMeta) > 0 then
      ParseMeta(aMeta);
  end;
  FBytesToMeta := FMetaInterval;
end;

procedure TmnIceCastConnection.ParseMeta(const AMeta: AnsiString);
var
  aTitle: string;
begin
  aTitle := ExtractMetaValue(String(AMeta), 'StreamTitle');
  if aTitle <> '' then
    SetTitle(aTitle);
end;

procedure TmnIceCastConnection.StreamAudio(AChunk: Integer);
var
  r: Integer;
begin
  FScratch.SetSize(AChunk);
  r := ReadBody(FScratch.Memory^, AChunk);
  FScratch.SetSize(r);
  if r > 0 then
  begin
    FScratch.Position := 0;
    FClient.Lock.Enter;
    try
      FBuffer.Position := FBuffer.Size;
      FBuffer.WriteBuffer(FScratch.Memory^, r);
      FBuffer.Position := FBuffer.Size;
    finally
      FClient.Lock.Leave;
    end;
    if FUseMeta then
      Dec(FBytesToMeta, r);
  end
  else
    StopByEnd;
end;

procedure TmnIceCastConnection.StopByEnd;
begin
  FClient.Lock.Enter;
  try
    FStreamEnded := True;
  finally
    FClient.Lock.Leave;
  end;
  SetState(icStopped);
  if FStream <> nil then
    FStream.Disconnect;
end;

procedure TmnIceCastConnection.HandleError(E: Exception);
begin
  if E is EmnStreamExceptionAbort then
    Exit;
  SetState(icError, E.Message);
  if FStream <> nil then
    FStream.Disconnect;
end;

procedure TmnIceCastConnection.CloseStream;
var
  aStream: TmnIceCastSocket;
begin
  Terminate;
  FClient.Lock.Enter;
  try
    if FState = icReady then
      FState := icStopped;
    FStreamEnded := False;
    aStream := FStream;
  finally
    FClient.Lock.Leave;
  end;
  if aStream <> nil then
    aStream.Disconnect;
end;

procedure TmnIceCastConnection.Prepare;
var
  vRedirects: Integer;
  aLocation: string;
begin
  FCode := 0;
  FError := '';
  FStreamEnded := False;
  FTitle := '';
  FUseMeta := False;
  FMetaInterval := 0;
  FBytesToMeta := 0;
  FConnectedTo := '';
  FChunked := False;
  FChunkRemain := 0;
  FBodyEnded := False;
  FStream := TmnIceCastSocket.Create;
  FStream.EndOfLine := sWinEndOfLine;
  FStream.ReadTimeout := FReadTimeout;
  FStream.ConnectTimeout := 10000;
  FStream.WriteTimeout := 5000;
  FStream.Options := FStream.Options + [soWaitBeforeRead];
  FStream.Address := FAddress;
  FStream.Port := FPort;
  vRedirects := 0;
  try
    repeat
      if FSSL then
        FStream.Options := FStream.Options + [soSSL]
      else
        FStream.Options := FStream.Options - [soSSL];
      SetState(icConnecting);
      FStream.Connect;
      SetState(icOpening);
      SendRequest;
      ReadHeaders;
      if (FCode >= 300) and (FCode < 400) then
      begin //follow the redirect
        aLocation := GetHeader('Location');
        if aLocation = '' then
          raise EmnStreamException.CreateFmt('Server error, HTTP %d', [FCode]);
        Inc(vRedirects);
        if vRedirects > cIceCastMaxRedirects then
          raise EmnStreamException.Create('Too many redirects');
        FStream.ResetSocket;
        aLocation := ResolveURL(FProtocol, FAddress, FPort, FPath, aLocation);
        IceCastParseURL(aLocation, FProtocol, FAddress, FPort, FPath);
        FSSL := SameText(FProtocol, 'https') or SameText(FProtocol, 'wss');
        FStream.Address := FAddress;
        FStream.Port := FPort;
      end
      else if (FCode < 200) or (FCode >= 300) then
        raise EmnStreamException.CreateFmt('Server error, HTTP %d', [FCode])
      else
        Break;
    until False;
    FConnectedTo := FProtocol + '://' + FAddress + ':' + FPort + FPath;
    FUseMeta := FUseMetaData and (FMetaInterval > 0);
    FBytesToMeta := FMetaInterval;
    SetState(icReady);
  except
    on E: Exception do
      HandleError(E);
  end;
end;

procedure TmnIceCastConnection.Process;
var
  vChunk: Integer;
begin
  try
    if FState = icReady then
    begin
      if FUseMeta and (FBytesToMeta <= 0) then
        ReadMeta;
      if FStream.Connected then
      begin
        if FUseMeta and (FBytesToMeta > 0) and (FBytesToMeta < cIceCastReadChunk) then
          vChunk := FBytesToMeta
        else
          vChunk := cIceCastReadChunk;
        StreamAudio(vChunk);
      end;
    end;
  except
    on E: Exception do
      HandleError(E);
  end;
end;

procedure TmnIceCastConnection.Unprepare;
var
  aStream: TmnIceCastSocket;
  aState: TmnIceCastState;
begin
  aStream := FStream;
  FStream := nil;
  if aStream <> nil then
  begin
    try
      aStream.Disconnect;
    except
    end;
    aStream.Free;
  end;
  FClient.Lock.Enter;
  try
    aState := FState;
    if aState = icReady then
      FStreamEnded := True;
  finally
    FClient.Lock.Leave;
  end;
  if aState <> icError then
    SetState(icStopped);
end;

{ TmnIceCastClient }

constructor TmnIceCastClient.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FUseMetaData := True;
  FReadTimeout := 10000;
end;

destructor TmnIceCastClient.Destroy;
begin
  Close;
  FreeAndNil(FLock);
  inherited;
end;

function TmnIceCastClient.GetState: TmnIceCastState;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := icStopped
    else
      Result := FConnection.FState;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetError: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FError;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetTitle: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FTitle;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetStationName: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FStationName;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetGenre: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FGenre;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetBitrate: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FBitrate;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetStationURL: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FStationURL;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetContentType: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FContentType;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetConnectedTo: string;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := ''
    else
      Result := FConnection.FConnectedTo;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetResponseCode: Integer;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := 0
    else
      Result := FConnection.FCode;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetStreamEnded: Boolean;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := False
    else
      Result := FConnection.FStreamEnded;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetBuffer: TMemoryStream;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := nil
    else
      Result := FConnection.FBuffer;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.GetHeaders: TStringList;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := nil
    else
      Result := FConnection.FHeaders;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.Connected: Boolean;
begin
  FLock.Enter;
  try
    if (FConnection <> nil) and (FConnection.FStream <> nil) then
      Result := FConnection.FStream.Connected
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

function TmnIceCastClient.BufferSize: Int64;
begin
  FLock.Enter;
  try
    if FConnection = nil then
      Result := 0
    else
      Result := FConnection.FBuffer.Size;
  finally
    FLock.Leave;
  end;
end;

procedure TmnIceCastClient.ClearBuffer;
begin
  FLock.Enter;
  try
    if FConnection <> nil then
      FConnection.FBuffer.SetSize(0);
  finally
    FLock.Leave;
  end;
end;

procedure TmnIceCastClient.Open(const vURL: string);
begin
  Close;
  FLock.Enter;
  try
    FURL := vURL;
    FConnection := TmnIceCastConnection.Create(nil, Self);
  finally
    FLock.Leave;
  end;
  FConnection.Start;
end;

procedure TmnIceCastClient.Close;
var
  aConnection: TmnIceCastConnection;
begin
  FLock.Enter;
  try
    aConnection := FConnection;
  finally
    FLock.Leave;
  end;
  if aConnection = nil then
    Exit;
  aConnection.CloseStream;
  if not aConnection.Finished then
    aConnection.WaitFor;
  FLock.Enter;
  try
    if FConnection = aConnection then
      FConnection := nil;
    aConnection.Free;
  finally
    FLock.Leave;
  end;
end;

procedure TmnIceCastClient.PostStateChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TmnIceCastClient.PostTitleChanged;
begin
  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self);
end;

procedure TmnIceCastClient.SetOnStateChanged(aValue: TmnIceCastNotify);
begin
  FOnStateChanged := aValue;
end;

procedure TmnIceCastClient.SetOnTitleChanged(aValue: TmnIceCastNotify);
begin
  FOnTitleChanged := aValue;
end;

end.
