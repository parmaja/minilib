unit SendAndRecv;

{$ifdef FPC}
{$mode Delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  mnUtils, mnStreams, mnMultipartData, mnHttpClient2, mnWebModules, mnFields, mnHeaders,
  mnLogs, mnStreamUtils, mnSockets, mnClients, mnServers;

{$ifdef GUI}
var
  WriteLn : procedure(S: string= '');
  Write : procedure(S: string= '');
  ReadLn:  procedure(var S: UTF8String);
{$endif}

type
  { TThreadReciever }

  TThreadReciever = class(TThread) //Server
  protected
    procedure RunServer;
    procedure RunHttp;
    procedure Execute; override;
  public
    Stream: TmnServerSocket;
  end;

  { TThreadSender }

  TThreadSender = class(TThread) //Client
  protected
    procedure Execute; override;
  public
    Stream: TmnClientSocket;
  end;

  { TTestStream }

  TTestStream = class(TObject)
  protected
    procedure ExampleReadLinesFile;
    procedure ExampleReadStringsFile;

    procedure CopyFileWrite;
    procedure CopyFileRead;

    procedure InternalExampleSocket(WithServer: Boolean = True; WithClient: Boolean = True); //Socket threads

    procedure ExampleSocket;

    procedure ExampleTimeout;
    procedure ExampleSocketOpenStreet;
    procedure ExampleSocketTestTimeout;
    procedure ExampleSocketTestCancel;
    procedure ExamplePrintServer;
    procedure ExampleEchoAliveServer;

    procedure ExampleBIOPostmanEcho;
    procedure ExampleWriteFormData;
    procedure ExampleReadFormData;

    procedure ExamplePostmanEcho;
    procedure ExampleCloudFlare;

    procedure ExampleWriteReadWSFile;
    procedure ExampleWebSocket;

    procedure ExampleSmallBuffer; //read write line with small buffer
    procedure ExampleHexLine; //Hex lines
    procedure ExampleHexImage; //Hex image
    procedure ExampleCopyHexImage; //Hex image2 images and read one

    procedure ExampleChunkedRead;
    procedure ExampleChunkedWrite;
    procedure ExampleChunkedImage;

    procedure InternalCompressImage(GZ, WithHex: Boolean); //GZ image
    procedure ExampleInflateImage; //Inflate image
    procedure ExampleGZImage; //GZ image
    procedure ExampleGZText; //GZ image
    procedure ExampleGZTextWithHeader;
    procedure ExampleUnGZImage; //Unzip GZ image

    procedure DoRun;
  public
    Location: UTF8String;
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

const
  sMsg: AnsiString = '0123456789';
  sPort: UTF8String = '8443';
  sHost = '127.0.0.1';


type
  TmyInfo = record
    Address: UTF8String;
    EndOfLine: UTF8String;
    SocketOptionsStr: UTF8String;
    NoDelay: Boolean;
    CancelAfter: Boolean;
    KeepAlive: Boolean;
    WaitBeforeRead: Boolean;
    UseSSL: Boolean;
    QuickAck: Boolean;
    TestTimeOut: Longint;// = -1;
    SocketOptions: TmnsoOptions; //soWaitBeforeRead
    Http: Boolean; //soWaitBeforeRead
    procedure Clear;
  end;

var
  Application: TTestStream;
  Reciever: TThreadReciever;
  Sender: TThreadSender;
  info: TmyInfo;
  ini: TIniFile;

implementation

{ TThreadReciever }

procedure TThreadReciever.Execute;
begin
  try
    Stream := TmnServerSocket.Create('', sPort); //if u pass address, server will listen only on this network
    Stream.ReadTimeout := info.TestTimeOut;
    Stream.CertificateFile := Application.Location + 'certificate.pem';
    Stream.PrivateKeyFile := Application.Location + 'privatekey.pem';
    Stream.Options := info.SocketOptions;
    if info.NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if info.WaitBeforeRead then
      Stream.Options := Stream.Options + [soWaitBeforeRead];
    if info.KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if info.QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];
    if info.UseSSL then
      Stream.Options := Stream.Options + [soSSL];

    if info.EndOfLine<>'' then
      Stream.EndOfLine := info.EndOfLine;


    Stream.Connect;
    try
      if info.Http then
        RunHttp
      else
        RunServer;
      Stream.Disconnect;
    finally
      Stream.Free;
    end;
    //WriteLn('Server Count: ' + IntToStr(Count));
    WriteLn('Server end execute');
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

{ ThreadSender }

procedure TThreadSender.Execute;
var
  S: UTF8String;
  i: Integer;
  t: int64;
  b: Boolean;
const
  ACount: Integer = 100;
begin
  try
    Stream := TmnClientSocket.Create(info.Address, sPort);
    Stream.ReadTimeout := info.TestTimeOut;
    Stream.Options := info.SocketOptions;
    if info.NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if info.WaitBeforeRead then
      Stream.Options := Stream.Options + [soWaitBeforeRead];
    if info.KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if info.QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];
    if info.UseSSL then
      Stream.Options := Stream.Options + [soSSL];
    try
      t := TThread.GetTickCount;
      Stream.Connect;
      WriteLn(TicksToString(GetTickCount - t));
      if Stream.Connected then
      begin
        for i := 0 to ACount -1 do
        begin
          if info.CancelAfter then
            Reciever.Stream.Disconnect;
          b := Stream.WriteUTF8Line(sMsg) > 0;
          b := Stream.ReadUTF8Line(s);
          WriteLn('client After read Line "' + s + '"');
          if sMsg <> s then
          begin
            Log.WriteLn('Error msg: ' + s + ' b: ' + b.ToString(True) + ' connected ' + Stream.Connected.ToString(True));
            Break;
          end;
          if not Stream.Connected then
            break;
        end;
      end;
      WriteLn(TicksToString(GetTickCount - t));
      Stream.Disconnect;
    finally
      Stream.Free;
    end;
    WriteLn('Client end execute');
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

procedure TThreadReciever.RunHttp;

  procedure _ReadHeader;
  var
    s: UTF8String;
  begin
    repeat
      Stream.ReadUTF8Line(s);
      WriteLn('Server read header "' + s + '"');
    until s = '';
  end;

  procedure _WriteHeader;
  begin
    Stream.WriteUTF8Line('HTTP/1.1 200 OK');
    Stream.WriteUTF8Line('content-length: 0');
    //Stream.WriteUTF8Line('Connection: keep-alive');
    Stream.WriteUTF8Line('Connection: close');
    Stream.WriteUTF8Line('content-type: text/html; charset=UTF-8');
    Stream.WriteUTF8Line('');
  end;

var
  s: UTF8String;
  Count: Integer;
begin
  Count := 0;
  while true do
  begin
    if not Stream.Connected then
      break;

    _ReadHeader;
    _WriteHeader;
  end;
end;

procedure TThreadReciever.RunServer;
var
  s: UTF8String;
  Count: Integer;
begin
  Count := 0;
  while true do
  begin
    Stream.ReadUTF8Line(s);
    WriteLn('Server After read Line "' + s + '"');
    if s = '' then
    begin
      WriteLn('Seem server socket canceled :(');
      Break;
    end;

    if not Stream.Connected then
      break;
    if sMsg <> s then
    begin
      //Log.WriteLn('Error msg: ' + s);
      //Break;
    end;
    if info.TestTimeOut > 0 then
      Sleep(info.TestTimeOut * 2);

    //Stream.WriteLine(sMsg);
    Inc(Count);
    if not Stream.Connected then
      break;
  end;
end;

{ TTestStream }

procedure TTestStream.ExampleSmallBuffer;
var
  Stream: TmnBufferStream;
  s: UTF8String;
  ss: string;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test.txt', fmOpenRead));
  try
    Stream.ReadBufferSize := 5;
    Stream.ReadLine(s);
    WriteLn(S);
    Stream.ReadLine(s);
    WriteLn(S);

    Stream.ReadString(ss);
    WriteLn('['+ss+']');

    Stream.ReadLine(s);
    WriteLn(S);

    Stream.ReadLine(s);
    WriteLn(S);

  finally
    FreeAndNil(Stream);
  end;
end;

function BoolToStr(B: boolean; const TrueS, FalseS: UTF8String): UTF8String; //ported from FPC
begin
  if B then Result:=TrueS else BoolToStr:=FalseS;
end;

procedure TTestStream.InternalCompressImage(GZ, WithHex: Boolean);
var
  cFile: string;
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  HexProxy: TmnHexStreamProxy;
  CompressProxy: TmnDeflateStreamProxy;
begin
  if GZ then
    cFile := Location + 'image.gz'
  else
    cFile := Location + 'image.inflate';
  //image.gz is a compressed file of hex file of image
  WriteLn('Read image to compressed file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(cFile, fmCreate or fmOpenWrite));
  if GZ then
    CompressProxy := TmnGzipStreamProxy.Create([cprsRead, cprsWrite], 9)
  else
    CompressProxy := TmnDeflateStreamProxy.Create([cprsRead, cprsWrite], 9);
  Stream.AddProxy(CompressProxy);

  if WithHex then
  begin
    HexProxy := TmnHexStreamProxy.Create;
    Stream.AddProxy(HexProxy);
  end;

  //CompressProxy.Disable;
  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aImageFile)));
  finally
    Stream.Free;
    FreeAndNil(aImageFile);
  end;

//---------------------------------------------------------

  WriteLn('Read compressed file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(cFile, fmOpenRead));
  if GZ then
    CompressProxy := TmnGzipStreamProxy.Create([cprsRead, cprsWrite], 9)
  else
    CompressProxy := TmnDeflateStreamProxy.Create([cprsRead, cprsWrite], 9);
  Stream.AddProxy(CompressProxy);

  if WithHex then
  begin
    HexProxy := TmnHexStreamProxy.Create;
    Stream.AddProxy(HexProxy);
  end;
  //CompressProxy.Disable;
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

procedure TTestStream.InternalExampleSocket(WithServer: Boolean; WithClient: Boolean);
begin
  if WithServer then
  begin
    WriteLn('Main: Server starting');
    Reciever := TThreadReciever.Create(True);
    Reciever.Start;
    WriteLn('Main: Server started, Sleep for 1s before Client start, Port:' + sPort);
    Sleep(1000);
  end;

  if WithClient then
  begin
    WriteLn('Main: Client started');
    Sender := TThreadSender.Create(True);
    Sender.Start;
  end;

  if WithClient then
  begin
    WriteLn('Main: Waiting for client');
    Sender.WaitFor;
  end;
  if WithServer then
  begin
    WriteLn('Main: Waiting for server');
    Reciever.WaitFor;
  end;

  FreeAndNil(Reciever);
  FreeAndNil(Sender);

end;

function GetAnswer(Q: UTF8String; Default: Boolean = true): Boolean; overload;
var
  s: UTF8String;
begin
  Write(Q);
  Write(': ');
  ReadLn(s);
  if s = '' then
  begin
    Result := Default;
    Write(BoolToStr(Result, 'Yes', 'No'));
  end
  else if s = 'x' then
    Halt
  else
    Result := SameText(s, 'y');
  Writeln;
  Result := True;
end;

function GetAnswer(Q: UTF8String; Default: UTF8String = ''; AddClear: UTF8String = ''): UTF8String; overload;
var
  s: UTF8String;
begin
  Write(Q);
  if Default <> '' then
    Write(' (' + Default + ')');
  Write(': ');
  ReadLn(s);
  if s = '' then
  begin
    Result := Default;
    Write(Result);
  end
  else if s = 'x' then
    Halt
  else if (AddClear <> '') and (s = AddClear) then
    Result := ''
  else
    Result := s;
  Writeln;
end;

procedure TTestStream.ExampleSocket;
var
  WithServer, WithClient: boolean;
  s: UTF8String;
begin
  WithServer := GetAnswer('With Server? ', True);
  WithClient := not WithServer or GetAnswer('With Client? ', True);
  if WithClient then
  begin
    if not WithServer then
    begin
      Info.Address := GetAnswer('Enter IP address', Info.Address);
      ini.WriteString('Options', 'Address', Info.Address);
    end
    else
      Info.Address := sHost;
  end;

  Info.SocketOptionsStr := ini.ReadString('Options', 'SocketOptions', Info.SocketOptionsStr);
  S := LowerCase(GetAnswer('w=WaitBeforeRead, n=NoDelay, k=KeepAlive, q=QuickAck s=SSL or c to clear', Info.SocketOptionsStr, 'c'));
  Info.NoDelay := Pos('n', S) > 0;
  Info.KeepAlive := Pos('k', S) > 0;
  Info.QuickAck := Pos('q', S) > 0;
  Info.WaitBeforeRead := Pos('w', S) > 0;
  Info.UseSSL := Pos('s', S) > 0;
  Info.TestTimeOut := 1000;
  Info.CancelAfter := False;

  if s <> 'c' then
    ini.WriteString('Options', 'SocketOptions', S);
  InternalExampleSocket(WithServer, WithClient);
end;

procedure TTestStream.ExampleTimeout;
var
  WithServer, WithClient: boolean;
begin
  WithServer := True;
  WithClient := True;

  Info.Address := '127.0.0.1';

  Info.NoDelay := True;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.WaitBeforeRead := True;
  Info.UseSSL := False;
  Info.TestTimeOut := 100;
  Info.CancelAfter := False;

  InternalExampleSocket(WithServer, WithClient);
end;

procedure TTestStream.ExampleSocketOpenStreet;
var
  Stream: TmnClientSocket;
  aFile: TFileStream;
  S: UTF8String;
  t: int64;
const
  //sURL = 'www.openstreetmap.org';
  sURL = 'https://c.tile.openstreetmap.de/17/65536/65536.png';
  //sURL = 'zaherdirkey.wordpress.com';
begin
  Info.NoDelay := True;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.UseSSL := False;
  try
    Stream := TmnClientSocket.Create('c.tile.openstreetmap.org', '443');
    Stream.ReadTimeout := Info.TestTimeOut;
    Stream.Options := Info.SocketOptions;
    Stream.Options := Stream.Options + [soNoDelay];
//  Stream.Options := Stream.Options + [soKeepAlive];
//    if QuickAck then
//      Stream.Options := Stream.Options + [soQuickAck];
    Stream.Options := Stream.Options + [soSSL];
    try
      t := TThread.GetTickCount;
      Stream.EndOfLine := #13#10;
      //Stream.ConnectTimeout := 10000;
      Stream.Connect;
      WriteLn(TicksToString(TThread.GetTickCount - t));
      if Stream.Connected then
      begin
        Stream.WriteUTF8Line('GET /17/65536/65536.png HTTP/1.1');
        Stream.WriteUTF8Line('Host: c.tile.openstreetmap.org');
        Stream.WriteUTF8Line('User-Agent: Mozilla');
        Stream.WriteUTF8Line('Connection: close');
        Stream.WriteUTF8Line('');

        //read phase
        Stream.ReadUTF8Line(s);
        WriteLn(s);
        while Stream.Connected and Stream.ReadUTF8Line(s) do
        begin
          if s ='' then
          begin
            aFile := TFileStream.Create(Location + 'map.png', fmCreate or fmOpenWrite);
            try
              Stream.CopyToStream(aFile);
            finally
              aFile.Free;
            end;
            break;
          end
          else
            WriteLn(s);
        end;
      end;
      WriteLn(TicksToString(TThread.GetTickCount - t));
      Stream.Disconnect;
    finally
      Stream.Free;
    end;
    WriteLn('Client end execute');
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

procedure TTestStream.ExampleSocketTestTimeout;
begin
  Info.NoDelay := False;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.UseSSL := False;
  Info.TestTimeOut := 1000;
  Info.CancelAfter := False;
  InternalExampleSocket(true, true);
end;

procedure TTestStream.ExampleSocketTestCancel;
begin
  Info.NoDelay := False;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.UseSSL := False;
  Info.TestTimeOut := 1000;
  Info.CancelAfter := True;
  InternalExampleSocket(true, true);
end;

procedure TTestStream.ExampleUnGZImage;
var
  cFile: string;
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  HexProxy: TmnHexStreamProxy;
  CompressProxy: TmnDeflateStreamProxy;
begin
  cFile := Location + 'image.gz';
  WriteLn('Read compressed file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(cFile, fmOpenRead));
  CompressProxy := TmnGzipStreamProxy.Create([cprsRead, cprsWrite], 9);
  Stream.AddProxy(CompressProxy);

  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

procedure TTestStream.ExampleWebSocket;
var
  Stream: TmnClientSocket;
  aFile: TFileStream;
  S: UTF8String;
  t: int64;
  Proxy: TmnWebSocket13StreamProxy;
begin
  Info.NoDelay := True;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.UseSSL := False;
  try
    Stream := TmnClientSocket.Create('echo.websocket.org', '443');
//    Stream := TmnClientSocket.Create('localhost', '8080');
    Stream.ReadTimeout := Info.TestTimeOut;
    Stream.Options := Info.SocketOptions;
    Stream.Options := Stream.Options + [soNoDelay];
//  Stream.Options := Stream.Options + [soKeepAlive];
//    if QuickAck then
//      Stream.Options := Stream.Options + [soQuickAck];
    if Stream.Port = '443' then
      Stream.Options := Stream.Options + [soSSL];
    try
      t := TThread.GetTickCount;
      Stream.EndOfLine := #13#10;
      //Stream.ConnectTimeout := 10000;
      WriteLn('Connecting to ' + Stream.Address);
      Stream.Connect;
      WriteLn(TicksToString(TThread.GetTickCount - t));
      if Stream.Connected then
      begin
        WriteLn('Connected to ' + Stream.Address);
        Stream.WriteUTF8Line('GET / HTTP/1.1');
        Stream.WriteUTF8Line('Host: ' + Stream.Address);
        Stream.WriteUTF8Line('Connection: keep-alive, Upgrade');
        Stream.WriteUTF8Line('Content-Type: text/html');
//        Stream.WriteUTF8Line('Origin: localhost');
        Stream.WriteUTF8Line('Upgrade: websocket');
        Stream.WriteUTF8Line('Sec-Fetch-Site: cross-site');
        Stream.WriteUTF8Line('Sec-WebSocket-Key: ccCoUoR7ORNSVEc1ReiLWg==');
        Stream.WriteUTF8Line('Sec-WebSocket-Version: 13');
        Stream.WriteUTF8Line('X-Send-Server-Hostname: false');

        Stream.WriteUTF8Line('');

        Stream.ReadUTF8Line(s);
        WriteLn('>' + s);

        while Stream.Connected do
        begin
          Stream.ReadUTF8Line(s);
          WriteLn('>' + s);
          if s ='' then
            break;
        end;

        Proxy := TmnWebSocket13StreamProxy.Create(True, True, 0);
        Stream.AddProxy(Proxy);

        //Stream.ReadUTF8String(s);
        //WriteLn('ws>' + s);

        if Stream.Connected then
        begin
          Stream.WriteUTF8String('Hi');
          Stream.ReadUTF8String(s);
          WriteLn('ws>'+s);
        end;

      end;
      WriteLn(TicksToString(TThread.GetTickCount - t));
      Stream.Disconnect;
    finally
      Stream.Free;
    end;
    WriteLn('Client end execute');
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

procedure TTestStream.ExampleWriteFormData;
var
  m: TMemoryStream;
  Stream: TmnBufferStream;
  aFormData: TmnMultipartData;
  aItm: TmnMultipartDataItem;
begin
  m := TMemoryStream.Create;
  Stream := TmnWrapperStream.Create(m, False);
  try
    Stream.EndOfLine := sWinEndOfLine;
    aFormData := TmnMultipartData.Create;
    try
      aFormData.Boundary := TGUID.NewGuid.ToString;
//      TmnMultipartDataValue.Create(aFormData).Value := 'test@code.com';
      TmnMultipartDataFileName.Create(aFormData).FileName := 'image.jpg';

      aFormData.Write(Stream);
    finally
      FreeAndNil(aFormData);
    end;

    m.SaveToFile('formdata.txt');
  finally
    Stream.Free;
  end;
end;

procedure TTestStream.ExampleWriteReadWSFile;
var
  f: TmnWrapperStream;
  Proxy: TmnWebSocket13StreamProxy;
  s: utf8string;
begin
  WriteLn('Writing WS File');
  f := TmnWrapperStream.Create(TFileStream.Create(Application.Location + 'test\ws.file', fmCreate));
  try
    Proxy := TmnWebSocket13StreamProxy.Create(False);
    f.AddProxy(Proxy);

    f.WriteUTF8String('Hello');
    f.WriteUTF8String('Hi');

    f.WriteUTF8String('My Name'+#13#10'Is No Name'#13#10);
    f.WriteUTF8String('My Power'+#13#10'Not Same'#13#10);

  finally
    f.free;
  end;

  WriteLn('Reading WS File');
  f := TmnWrapperStream.Create(TFileStream.Create(Application.Location + 'ws.file', fmOpenRead));
  try
    Proxy := TmnWebSocket13StreamProxy.Create(False);
    f.AddProxy(Proxy);

    f.ReadUTF8String(s);
    WriteLn('>'+s);

//    f.ReadUTF8String(s);
    f.ReadUTF8Line(s);
    WriteLn('>'+s);

    f.ReadUTF8Line(s);
    WriteLn('>'+s);
    f.ReadUTF8Line(s);
    WriteLn('>'+s);
    f.ReadUTF8Line(s);
    WriteLn('>'+s);
    f.ReadUTF8Line(s);
    WriteLn('>'+s);
  finally
    f.free;
  end;
end;

procedure TTestStream.ExampleHexLine;
var
  Stream: TmnBufferStream;
  Proxy: TmnHexStreamProxy;
  S: UTF8String;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    Stream.WriteUTF8Line('0123456789');
    Stream.WriteUTF8Line('0123456789');
    Stream.WriteUTF8Line('0123456789');
  finally
    FreeAndNil(Stream);
  end;


  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmOpenRead));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    while Stream.Connected do
    begin
      Stream.ReadUTF8Line(S);
      WriteLn('"' + Trim(S) + '"');
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestStream.ExampleInflateImage;
begin
  InternalCompressImage(False, False);
end;

procedure TTestStream.ExamplePostmanEcho;
var
  m: TStringStream;
  c: TmnHttpClient;
  s: string;
  h: TmnField;
begin
  //https://documenter.getpostman.com/view/5025623/SWTG5aqV
  m := TStringStream.Create;
  c := TmnHttpClient.Create;
  try
    //c.UserAgent := 'curl/7.83.1';
    c.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/113.0';
    c.Request.Header.Add('Accept', '*/*');
//    c.Request.Header.Add('x-forwarded-proto', 'https');
//    c.Request.Header.Add('x-forwarded-port', '443');
    //c.Compressing := True;
    s := m.DataString;

    //c.GetString('https://api.oursms.com/api-a/msgs?username=Alhayatsweets&token=2NgwEKQgO18yLAgXfTU0&src=ALHAYAT&body=12347&dests=+966504544896', s);
    c.GetString('https://postman-echo.com/get?test=1', s);
    //c.GetString('https://raw.githubusercontent.com/paramjani12/paramjani12/main/README.md', s);

    //c.Get('https://api.oursms.com/api-a/msgs?username=Alhayatsweets&token=2NgwEKQgO18yLAgXfTU0&src=ALHAYAT&body=12347&dests=+966504544896');
    //c.ReadStream(m);

    Writeln('');
    for h in c.Respond.Header do
      Writeln('>'+h.GetNameValue);
    Writeln(s);

//    Writeln(c.Respond.StatusCode.ToString);

  finally
    c.Free;
    m.Free;
  end;
end;

procedure TTestStream.ExampleReadFormData;
var
  aTextFile: TFileStream;
  Stream: TmnBufferStream;
  aFormData: TmnMultipartData;
  aItm: TmnMultipartDataItem;
  h: TmnField;
begin
  aTextFile:=TFileStream.Create(Location + 'test\formdata_noheader.txt', fmOpenRead or fmShareDenyWrite);
  Stream := TmnWrapperStream.Create(aTextFile, True);
  try
    Stream.EndOfLine := sWinEndOfLine;
    aFormData := TmnMultipartData.Create;
    aFormData.Boundary := '---------------------------9051914041544843365972754266';
    try
      //aFormData.Read(Stream);
      aFormData.Read(Stream);
      for aItm in aFormData do
      begin
        for h in aItm.Header do
          Writeln('>'+h.GetNameValue);
        Writeln(aItm.Name);
      end;

    finally
      FreeAndNil(aFormData);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTestStream.ExampleReadLinesFile;
var
  aTextFile: TFileStream;
  Stream: TmnBufferStream;
  aProxy: TmnStreamOverProxy;
  t: Cardinal;
  s: UTF8String;
begin
  aTextFile := TFileStream.Create(Location + 'small.json', fmOpenRead);
  Stream := TmnWrapperStream.Create(aTextFile, True);
  try
    while Stream.ReadLine(s) do
    begin
      WriteLn(s)
    end;
  finally
    Stream.Free;
  end;

end;

procedure TTestStream.ExampleReadStringsFile;
var
  aTextFile: TFileStream;
  Stream: TmnBufferStream;
  aProxy: TmnStreamOverProxy;
  s: UTF8String;
  aStrings: TStringList;
begin
  aTextFile := TFileStream.Create(Location + 'small.json', fmOpenRead);
  Stream := TmnWrapperStream.Create(aTextFile, True);
  aStrings := TStringList.Create;
  try
    Stream.ReadUTF8Strings(aStrings);
    for s in aStrings do
      WriteLn(s);
  finally
    Stream.Free;
    aStrings.Free;
  end;
end;

procedure TTestStream.ExampleHexImage;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnHexStreamProxy;
begin
  WriteLn('Read image to hex file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;

  WriteLn('Read hex file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmOpenRead));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

procedure TTestStream.ExampleBIOPostmanEcho;
var
  c: TmnBIOHttpClient;
  s: string;
  h: TmnField;
begin
  //https://documenter.getpostman.com/view/5025623/SWTG5aqV
  c := TmnBIOHttpClient.Create;
  try
    //c.UserAgent := 'curl/7.83.1';
    c.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/113.0';
//    c.Request.Accept := '*/*';
//    c.Request.Header.Add('x-forwarded-proto', 'https');
//    c.Request.Header.Add('x-forwarded-port', '443');
    //c.Compressing := True;

    //c.GetString('https://api.oursms.com/api-a/msgs?username=Alhayatsweets&token=2NgwEKQgO18yLAgXfTU0&src=ALHAYAT&body=12347&dests=+966504544896', s);
    //c.GetString('https://raw.githubusercontent.com/paramjani12/paramjani12/main/README.md', s);
    c.GetString('https://postman-echo.com/get?test=1', s);
    //c.GetString('https://community.cloudflare.com/', s);


    Writeln('');
    for h in c.Respond.Header do
      Writeln('>'+h.GetNameValue);
    Writeln(s);

//    Writeln(c.Response.StatusCode.ToString);

  finally
    c.Free;
  end;
end;

procedure TTestStream.ExampleChunkedImage;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnChunkStreamProxy;
begin
  WriteLn('Read image to hex file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnChunkStreamProxy.Create;
  Stream.AddProxy(Proxy);
  Stream.AddProxy(TmnHexStreamProxy.Create);
  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;

  WriteLn('Read hex file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmOpenRead));
  Proxy := TmnChunkStreamProxy.Create;
  Stream.AddProxy(Proxy);
  Stream.AddProxy(TmnHexStreamProxy.Create);
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

procedure TTestStream.ExampleChunkedRead;
var
  Stream: TmnBufferStream;
  Proxy: TmnChunkStreamProxy;
  f: TFileStream;
begin

  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_chunk.txt', fmOpenRead));
  Proxy := TmnChunkStreamProxy.Create;
  Stream.AddProxy(Proxy);
  //ReadWriteBufferSize := 3;

  f := TFileStream.Create('temp1.txt', fmCreate or fmOpenWrite);
  try
    Stream.ReadStream(f);
  finally
    f.Free;
  end;

  f := TFileStream.Create('temp2.txt', fmCreate or fmOpenWrite);
  try
    Stream.ReadStream(f);
  finally
    f.Free;
  end;

  FreeAndNil(Stream);
end;

procedure TTestStream.ExampleChunkedWrite;
var
  Stream: TmnBufferStream;
  Proxy: TmnChunkStreamProxy;
begin

  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_chunk.txt', fmCreate or fmOpenWrite));
  Proxy := TmnChunkStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    Stream.WriteUTF8Line('0123456789');
    Stream.WriteUTF8Line('0123456789 kjhdkajshd kjh fdksajdf hdfjas kdfh ksdh fklsdhf ksdhf ksdhf ksdh fklshfj dffdff');
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestStream.ExampleCloudFlare;
var
  m: TStringStream;
  c: TmnHttpClient;
  s: string;
  h: TmnField;
begin
  m := TStringStream.Create;
  c := TmnHttpClient.Create;
  try
    //c.UserAgent := 'curl/7.83.1';
    c.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/113.0';
//    c.Request.Accept := '*/*';
//    c.Request.Header.Add('x-forwarded-proto', 'https');
//    c.Request.Header.Add('x-forwarded-port', '443');
    //c.Compressing := True;
    s := m.DataString;

    //c.GetString('https://api.oursms.com/api-a/msgs?username=Alhayatsweets&token=2NgwEKQgO18yLAgXfTU0&src=ALHAYAT&body=12347&dests=+966504544896', s);
    c.GetString('https://community.cloudflare.com/', s);
    //c.GetString('https://raw.githubusercontent.com/paramjani12/paramjani12/main/README.md', s);

    //c.Get('https://api.oursms.com/api-a/msgs?username=Alhayatsweets&token=2NgwEKQgO18yLAgXfTU0&src=ALHAYAT&body=12347&dests=+966504544896');
    //c.ReadStream(m);


    Writeln('');
//    Writeln('<'+c.Request.Head);
    for h in c.Request.Header do
      Writeln('<'+h.GetNameValue);
    Writeln('');
    for h in c.Respond.Header do
      Writeln('>'+h.GetNameValue);
    Writeln('');
    Writeln(s);

//    Writeln(c.Response.StatusCode.ToString);

  finally
    c.Free;
    m.Free;
  end;
end;

procedure TTestStream.ExampleCopyHexImage;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnHexStreamProxy;
  aSize: Integer;
begin
  WriteLn('Read image to hex file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    Stream.WriteStream(aImageFile);
    aSize := aImageFile.Size;
    aImageFile.Position := 0;
    Stream.WriteStream(aImageFile);
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;

  WriteLn('Read hex file to image');
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmOpenRead));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    aImageFile := TFileStream.Create(Location + 'image_copy1.jpg', fmCreate or fmOpenWrite);
    Stream.ReadStream(aImageFile, aSize);
    FreeAndNil(aImageFile);
    aImageFile := TFileStream.Create(Location + 'image_copy2.jpg', fmCreate or fmOpenWrite);
    Stream.ReadStream(aImageFile, aSize);
    FreeAndNil(aImageFile);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestStream.ExampleEchoAliveServer;
begin
  Info.Clear;
  Info.Address := '127.0.0.1';

  Info.NoDelay := True;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.WaitBeforeRead := True;
  Info.UseSSL := False;
  Info.TestTimeOut := -1;
  Info.CancelAfter := False;
  Info.EndOfLine := #$D#$A;
  Info.Http := True;

  InternalExampleSocket(True, False);
end;

procedure TTestStream.ExamplePrintServer;
begin
  Info.Clear;
  Info.Address := '127.0.0.1';

  Info.NoDelay := True;
  Info.KeepAlive := False;
  Info.QuickAck := False;
  Info.WaitBeforeRead := True;
  Info.UseSSL := True;
  Info.TestTimeOut := -1;
  Info.CancelAfter := False;
  Info.EndOfLine := #$D#$A;

  InternalExampleSocket(True, False);
end;

procedure TTestStream.ExampleGZImage;
begin
  InternalCompressImage(True, False);
end;

procedure TTestStream.ExampleGZText;
var
  cFile: string;
  aTextFile: TFileStream;
  Stream: TmnBufferStream;
  HexProxy: TmnHexStreamProxy;
  CompressProxy: TmnDeflateStreamProxy;
begin
  cFile := Location + 'file.gz';
  //image.gz is a compressed file of hex file of image
  WriteLn('Read text to compressed file');
  aTextFile := TFileStream.Create(Location + 'file.txt', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(cFile, fmCreate or fmOpenWrite));
  CompressProxy := TmnGzipStreamProxy.Create([cprsRead, cprsWrite], 9);
  Stream.AddProxy(CompressProxy);

  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aTextFile)));
  finally
    Stream.Free;
    FreeAndNil(aTextFile);
  end;

//---------------------------------------------------------

  WriteLn('Read compressed file to image');
  aTextFile := TFileStream.Create(Location + 'file_copy.txt', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(cFile, fmOpenRead));
  CompressProxy := TmnGzipStreamProxy.Create([cprsRead, cprsWrite], 9);
  Stream.AddProxy(CompressProxy);

  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aTextFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aTextFile);
  end;
end;

procedure TTestStream.ExampleGzTextWithHeader;
var
  aTextFile: TFileStream;
  Stream: TmnBufferStream;
  HexProxy: TmnHexStreamProxy;
  aProxy: TmnStreamOverProxy;
  s: utf8string;
  b: TBytes;
  c: Integer;
begin
  aTextFile := TFileStream.Create(Location + 'header.txt', fmOpenRead);
  Stream := TmnWrapperStream.Create(aTextFile, True);
  aProxy := TmnPlainStreamProxy.Create;

  Stream.ReadLine(S);
  WriteLn(s);
  Stream.ReadLine(S);
  WriteLn(s);
  SetLength(b, 1024);

  Stream.AddProxy(aProxy);

  try
    c := Stream.Read(b[0], 1024);
    SetLength(b, c);
    s := TEncoding.UTF8.GetString(b);
    WriteLn(s);

    {while Stream.ReadLine(S, False) do
      WriteLn(s);}
  finally
    Stream.Free;
  end;

end;

procedure TTestStream.CopyFileWrite;
var
  Stream1: TmnBufferStream;
  Stream2: TFileStream;
begin
  Stream1 := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite));
  Stream2 := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  try
    Stream1.CopyFromStream(Stream2);
  finally
    FreeAndNil(Stream2);
    FreeAndNil(Stream1);
  end;
  WriteLn('source size = ' + GetSizeOfFile('image.jpg').ToString);
  WriteLn('destination size = ' + GetSizeOfFile('image_copy.jpg').ToString);
end;

procedure TTestStream.CopyFileRead;
var
  Stream1: TmnBufferStream;
  Stream2: TFileStream;
begin
  Stream1 := TmnWrapperStream.Create(TFileStream.Create(Location + 'image.jpg', fmOpenRead));
  Stream2 := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  try
    Stream1.CopyToStream(Stream2);

  finally
    FreeAndNil(Stream2);
    FreeAndNil(Stream1);
  end;

  WriteLn('source size = ' + GetSizeOfFile('image.jpg').ToString);
  WriteLn('destination size = ' + GetSizeOfFile('image_copy.jpg').ToString);
end;

constructor TTestStream.Create;
begin
  inherited Create;
  Location := ExtractFilePath(ParamStr(0));
  //StopOnException := True;
end;

destructor TTestStream.Destroy;
begin
  inherited Destroy;
end;

procedure TTestStream.Run;
begin
  DoRun;
end;

type
  TProcedureObject = procedure of object;

procedure TTestStream.DoRun;
var
  s: UTF8String;
  n: Integer;

  Commands: array of record
    name: UTF8String;
    proc: TProcedureObject;
  end;

  procedure AddProc(Name: UTF8String; Proc: TProcedureObject);
  begin
    SetLength(Commands, Length(Commands) + 1);
    Commands[Length(Commands) - 1].name := Name;
    Commands[Length(Commands) - 1].proc := proc;
  end;
var
  BypassList: Boolean;
  RunCount: Integer;
  SearchStr: string;
begin
  RunCount := 0;
  BypassList := False;
  //InitOpenSSL;
  //if not FileExists(Application.Location + 'certificate.pem') then
  //MakeCert2('certificate.pem', 'privatekey.pem', 'PARMAJA', 'PARMAJA TEAM', 'SY', '', 2048, 0, 365);

  ini := TIniFile.Create(Application.Location + 'Options.ini');
  try
    try
      WriteLn('Welcome to testing Streams');
      WriteLn('');
      InstallConsoleLog;
      Info.Address := ini.ReadString('options', 'Address', sHost);
      AddProc('Readlines Text', ExampleReadLinesFile);
      AddProc('Read Strings File', ExampleReadStringsFile);
      AddProc('[httpclient] Download Cloud Flare ', ExampleCloudFlare);
      AddProc('[httpclient] BIO Postman Echo ', ExampleBIOPostmanEcho);
      AddProc('[httpclient] Postman Echo ', ExamplePostmanEcho);
      AddProc('Line Print Server ', ExamplePrintServer);
      AddProc('Echo Keep Alive Server ', ExampleEchoAliveServer);

      AddProc('Socket threads', ExampleSocket);
      AddProc('Timout Socket threads', ExampleTimeout);
      AddProc('Socket OpenStreetMap', ExampleSocketOpenStreet);
      AddProc('Socket Timout: Socket threads', ExampleSocketTestTimeout);
      AddProc('Socket Test Cancel', ExampleSocketTestCancel);

      AddProc('Write FormData', ExampleWriteFormData);
      AddProc('Read FormData', ExampleReadFormData);

      AddProc('SmallBuffer: read write line with small buffer', ExampleSmallBuffer);
      AddProc('CopyHexImage: Hex image2 images and read one', ExampleCopyHexImage);
      AddProc('InflateImage: Inflate image', ExampleInflateImage);
      AddProc('GZImage: GZ image', ExampleGZImage);
      AddProc('UnGZImage: Unzip GZ image', ExampleGZImage);
      AddProc('HexLine: Hex lines', ExampleHexLine);
      AddProc('WriteWSFile: Write Read WS File', ExampleWriteReadWSFile);
      AddProc('WebSocket: WebSocket', ExampleWebSocket);
      AddProc('HexImage: Hex image', ExampleHexImage);
      AddProc('CopyFile Write', CopyFileWrite);
      AddProc('CopyFile Read', CopyFileRead);
      AddProc('GZText: GZ Text', ExampleGZText);
      AddProc('GZText: Headered Text', ExampleGzTextWithHeader);

      AddProc('Chunked: Read Chunked lines', ExampleChunkedRead);
      AddProc('Chunked: Write Chunked lines', ExampleChunkedWrite);
      AddProc('Chunked: Image Chunked lines', ExampleChunkedImage);

      while true do
      begin
        if (ParamCount>0) then
        begin
          if (RunCount>0) then
          begin
            s := 'exit';
            WriteLn('Press Enter to exit');
            ReadLn;
          end
          else
            s := ParamStr(1);
        end
        else
        begin
          if not BypassList then
          begin
            for n := 0 to Length(Commands) - 1 do
            begin
              if (SearchStr = '') or (Pos(SearchStr, LowerCase(Commands[n].name))>0) then
                WriteLn(IntToStr(n + 1) + ': ' + Commands[n].name);
            end;
            WriteLn;
            WriteLn('0: Type 0 to exit');
            WriteLn;
          end;
          BypassList := False;
          Write('Enter command: ');
          s := '';
          ReadLn(s);
          WriteLn;
          s := Trim(s);
          SearchStr := '';
        end;

        if s = '' then
          //Nothing
        else if SameText(s, 'exit') or SameText(s, 'quit') or SameText(s, 'q') or SameText(s, '0') then
          Break
        else
        begin
          n := StrToIntDef(s, 0);
          if (n = 0) or (n > Length(Commands)) then
          begin
            SearchStr := LowerCase(s);
          end
          else
          begin
            WriteLn('Running "' + Commands[n - 1].Name + '"');
            WriteLn;
            Info.Clear;
            try
              Commands[n - 1].proc();
            except
              on E: Exception do
              begin
                WriteLn(E.Message);
//                raise;
              end;
            end;

            BypassList := True;
            Inc(RunCount);
          end;
        end;
        WriteLn;
      end;
    except
      on E: Exception do
      begin
        WriteLn(E.Message);
        raise;
      end;
    end;
  finally
    ini.Free;
    //Halt;
  end;
end;

{ TmyInfo }

procedure TmyInfo.Clear;
begin
  Finalize(info);
  FillChar(Self, SizeOf(Self), 0);
end;

end.

