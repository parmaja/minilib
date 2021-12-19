unit SendAndRecv;

{$ifdef FPC}
{$mode Delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  mnUtils, mnStreams,
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
    procedure InternalExampleSocket(WithServer: Boolean = True; WithClient: Boolean = True); //Socket threads
    procedure InternalCompressImage(GZ, WithHex: Boolean); //GZ image

    procedure ExampleSocket;
    procedure ExampleSocketOpenStreet;
    procedure ExampleSocketTestTimeout;
    procedure ExampleSocketTestCancel;

    procedure ExampleSmallBuffer; //read write line with small buffer
    procedure ExampleHexLine; //Hex lines
    procedure ExampleHexImage; //Hex image
    procedure ExampleCopyHexImage; //Hex image2 images and read one

    procedure ExampleInflateImage; //Inflate image
    procedure ExampleGZImage; //GZ image

    procedure ExampleGZText; //GZ image

    procedure ExampleUnGZImage; //Unzip GZ image

    procedure CopyFileWrite;
    procedure CopyFileRead;

    procedure DoRun;
  public
    Location: UTF8String;
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

const
  sMsg: AnsiString = '0123456789';
  sPort: UTF8String = '9000';
  sHost = '127.0.0.1';

var
  Application: TTestStream;

  Reciever: TThreadReciever = nil;
  Sender: TThreadSender = nil;

  Address: UTF8String;
  SocketOptionsStr: UTF8String;
  NoDelay: Boolean = False;
  CancelAfter: Boolean = False;
  KeepAlive: Boolean = False;
  WaitBeforeRead: Boolean = False;
  UseSSL: Boolean = False;
  QuickAck: Boolean = False;
  TestTimeOut: Longint = -1;
  SocketOptions: TmnsoOptions = []; //soWaitBeforeRead
  ini: TIniFile;

implementation

{ TThreadReciever }

procedure TThreadReciever.Execute;
var
  s: UTF8String;
  Count: Integer;
begin
  try
    Count := 0;
    Stream := TmnServerSocket.Create('', sPort); //if u pass address, server will listen only on this network
    Stream.ReadTimeout := TestTimeOut;
    Stream.CertificateFile := Application.Location + 'certificate.pem';
    Stream.PrivateKeyFile := Application.Location + 'privatekey.pem';
    Stream.Options := SocketOptions;
    if NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if WaitBeforeRead then
      Stream.Options := Stream.Options + [soWaitBeforeRead];
    if KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];
    if UseSSL then
      Stream.Options := Stream.Options + [soSSL];

    Stream.Connect;
    try
      while true do
      begin
        Stream.ReadLineUTF8(s);
        WriteLn('Server After read Line "' + s + '"');
        if s = '' then
          WriteLn('Seem server socket canceled :(');

        if not Stream.Connected then
          break;
        if sMsg <> s then
        begin
          Log.WriteLn('Error msg: ' + s);
          Break;
        end;
        if TestTimeOut > 0 then
          Sleep(TestTimeOut * 2);

        Stream.WriteLine(sMsg);
        Inc(Count);
        if not Stream.Connected then
          break;
      end;
      Stream.Disconnect;
    finally
      Stream.Free;
    end;
    WriteLn('Server Count: ' + IntToStr(Count));
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
    Stream := TmnClientSocket.Create(Address, sPort);
    Stream.ReadTimeout := TestTimeOut;
    Stream.Options := SocketOptions;
    if NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if WaitBeforeRead then
      Stream.Options := Stream.Options + [soWaitBeforeRead];
    if KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];
    if UseSSL then
      Stream.Options := Stream.Options + [soSSL];
    try
      t := TThread.GetTickCount;
      Stream.Connect;
      WriteLn(TicksToString(GetTickCount - t));
      if Stream.Connected then
      begin
        for i := 0 to ACount -1 do
        begin
          if CancelAfter then
            Reciever.Stream.Disconnect;
          b := Stream.WriteLineUTF8(sMsg) > 0;
          b := Stream.ReadLineUTF8(s);
          if sMsg <> s then
          begin
            Log.WriteLn('Error msg: ' + s);
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

{ TTestStream }

procedure TTestStream.ExampleSmallBuffer;
var
  Stream: TmnBufferStream;
  s: UTF8String;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test.txt', fmOpenRead));
  try
    Stream.ReadBufferSize := 5;
    s := Stream.ReadLine;
    WriteLn(S);
    s := Stream.ReadLine;
    WriteLn(S);

    s := Stream.ReadString;
    WriteLn('['+S+']');

    s := Stream.ReadLine;
    WriteLn(S);

    s := Stream.ReadLine;
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
    WriteLn('Main: Server started, Sleep for 1s before Client start');
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
      Address := GetAnswer('Enter IP address', Address);
      ini.WriteString('Options', 'Address', Address);
    end
    else
      Address := sHost;
  end;

  SocketOptionsStr := ini.ReadString('Options', 'SocketOptions', SocketOptionsStr);
  S := LowerCase(GetAnswer('w=WaitBeforeRead, n=NoDelay, k=KeepAlive, q=QuickAck s=SSL or c to clear', SocketOptionsStr, 'c'));
  NoDelay := Pos('n', S) > 0;
  KeepAlive := Pos('k', S) > 0;
  QuickAck := Pos('q', S) > 0;
  WaitBeforeRead := Pos('w', S) > 0;
  UseSSL := Pos('s', S) > 0;
  TestTimeOut := 100;
  CancelAfter := False;

  if s <> 'c' then
    ini.WriteString('Options', 'SocketOptions', S);
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
  NoDelay := True;
  KeepAlive := False;
  QuickAck := False;
  UseSSL := False;
  try
    Stream := TmnClientSocket.Create('c.tile.openstreetmap.de', '443');
    Stream.ReadTimeout := TestTimeOut;
    Stream.Options := SocketOptions;
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
        Stream.WriteLineUTF8('GET /17/65536/65536.png HTTP/1.1');
        Stream.WriteLineUTF8('Host: c.tile.openstreetmap.de');
        Stream.WriteLineUTF8('User-Agent: Mozilla');
        Stream.WriteLineUTF8('Connection: close');
        Stream.WriteLineUTF8('');

        //read phase
        Stream.ReadLineUTF8(s);
        WriteLn(s);
        while Stream.Connected and Stream.ReadLineUTF8(s) do
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
  NoDelay := False;
  KeepAlive := False;
  QuickAck := False;
  UseSSL := False;
  TestTimeOut := 1000;
  CancelAfter := False;
  InternalExampleSocket(true, true);
end;

procedure TTestStream.ExampleSocketTestCancel;
begin
  NoDelay := False;
  KeepAlive := False;
  QuickAck := False;
  UseSSL := False;
  TestTimeOut := 1000;
  CancelAfter := True;
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
    Stream.WriteLineUTF8('0123456789');
    Stream.WriteLineUTF8('0123456789');
    Stream.WriteLineUTF8('0123456789');
  finally
    FreeAndNil(Stream);
  end;


  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmOpenRead));
  Proxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(Proxy);
  try
    while not Stream.EndOfStream do
    begin
      S := Stream.ReadLine;
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
begin
  //InitOpenSSL;
  //if not FileExists(Application.Location + 'certificate.pem') then
    //MakeCert('certificate.pem', 'privatekey.pem', 'PARMAJA', 'PARMAJA TEAM', 'SY', '', 2048, 0, 365);

  ini := TIniFile.Create(Application.Location + 'Options.ini');
  try
    try
      WriteLn('Welcome to testing Streams');
      WriteLn('');
      InstallConsoleLog;
      Address := ini.ReadString('options', 'Address', sHost);
      AddProc('ExampleSocket: Socket threads', ExampleSocket);
      AddProc('ExampleSocket: Socket OpenStreetMap', ExampleSocketOpenStreet);
      AddProc('Example Socket Timout: Socket threads', ExampleSocketTestTimeout);
      AddProc('Example Socket Test Cancel', ExampleSocketTestCancel);
      AddProc('ExampleSmallBuffer: read write line with small buffer', ExampleSmallBuffer);
      AddProc('ExampleCopyHexImage: Hex image2 images and read one', ExampleCopyHexImage);
      AddProc('ExampleInflateImage: Inflate image', ExampleInflateImage);
      AddProc('ExampleGZImage: GZ image', ExampleGZImage);
      AddProc('ExampleUnGZImage: Unzip GZ image', ExampleGZImage);
      AddProc('ExampleHexLine: Hex lines', ExampleHexLine);
      AddProc('ExampleHexImage: Hex image', ExampleHexImage);
      AddProc('CopyFile Write', CopyFileWrite);
      AddProc('CopyFile Read', CopyFileRead);
      AddProc('ExampleGZText: GZ Text', ExampleGZText);
      while true do
      begin
        for n := 0 to Length(Commands) - 1 do
          WriteLn(IntToStr(n + 1) + ': ' + Commands[n].name);
        WriteLn('0: Type 0 to exit');
        WriteLn;
        Write('Enter command: ');
        s := '';
        ReadLn(s);
        WriteLn;
        s := trim(s);
        if s = '' then
          //Nothing
        else if (s = '') or SameText(s, 'exit') then
          Break
        else
        begin
          n := StrToIntDef(s, 0);
          if (n = 0) or (n > Length(Commands)) then
            Break;
          WriteLn('Running "' + Commands[n - 1].Name + '"');
          WriteLn;
          Commands[n - 1].proc();
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

end.

