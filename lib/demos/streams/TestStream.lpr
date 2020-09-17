program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, zdeflate, zlib, zstream, mnUtils, mnStreams,
  mnLogs, mnStreamUtils, mnSockets, mnClients, mnServers, mnWinSockets, IniFiles;

type

  { ThreadSender }

  TThreadSender = class(TThread) //Client
  protected
    procedure Execute; override;
  public

  end;

  { TThreadReciever }

  TThreadReciever = class(TThread) //Server
  protected
    procedure Execute; override;
  public
  end;

  { TTestStream }

  TTestStream = class(TCustomApplication)
  protected
    procedure InternalExampleSocket(WithServer: Boolean = True; WithClient: Boolean = True); //Socket threads
    procedure ExampleSocket;
    procedure ExampleSmallBuffer; //read write line with small buffer
    procedure ExampleHexLine; //Hex lines
    procedure ExampleHexImage; //Hex image
    procedure ExampleCopyHexImage; //Hex image2 images and read one
    procedure ExampleGZImage; //GZ image
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TTestStream;
  Address: string;
  SocketOptionsStr: string;
  NoDelay: Boolean = False;
  KeepAlive: Boolean = False;
  QuickAck: Boolean = False;
  SocketOptions: TmnsoOptions = [soWaitBeforeRead];
  ini: TIniFile;

const
  sMsg: AnsiString = '0123456789';
  sPort: string = '82';
  sHost = '127.0.0.1';

{ TThreadReciever }

procedure TThreadReciever.Execute;
var
  Stream: TmnServerSocket;
  s: string;
begin
  try
    Stream := TmnServerSocket.Create('', sPort); //if u pass address, server will listen only on this network
    Stream.ReadTimeout := WaitForEver;
    Stream.Options := SocketOptions;
    if NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];

    WriteLn('Before connect');
    Stream.Connect;
    WriteLn('After connect');
    try
      while true do
      begin
        s := Stream.ReadLine;
        //WriteLn(s);
        if not Stream.Connected then
          break;
        if sMsg <> s then
          raise Exception.Create('Error msg: ' + s);
        Stream.WriteLine(sMsg);
        if not Stream.Connected then
          break;
      end;
      Stream.Disconnect;
      WriteLn('After disonnect');
    finally
      Stream.Free;
    end;
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
  Stream: TmnClientSocket;
  S: string;
  i: Integer;
  t: int64;
const
  ACount: Integer = 100;
begin
  try
    Stream := TmnClientSocket.Create(Address, sPort);
    Stream.ReadTimeout := WaitForEver;
    Stream.Options := SocketOptions;
    if NoDelay then
      Stream.Options := Stream.Options + [soNoDelay];
    if KeepAlive then
      Stream.Options := Stream.Options + [soKeepAlive];
    if QuickAck then
      Stream.Options := Stream.Options + [soQuickAck];
    try
      t := GetTickCount64;
      WriteLn('Before connect');
      Stream.Connect;
      WriteLn('After connect');
      WriteLn(TicksToString(GetTickCount64 - t));
      if Stream.Connected then
      begin
        for i := 0 to ACount -1 do
        begin
          Stream.WriteLine(sMsg);
          Stream.ReadLine(s);
          if sMsg <> s then
            raise Exception.Create('Error msg: ' + s);
          if not Stream.Connected then
          break;
      end;
      end;
      WriteLn(TicksToString(GetTickCount64 - t));
      Stream.Disconnect;
      WriteLn('After disconnect');
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
  s: string;
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

procedure TTestStream.InternalExampleSocket(WithServer: Boolean; WithClient: Boolean);
var
  Reciever: TThreadReciever;
  Sender: TThreadSender;
begin
  if WithServer then
  begin
    WriteLn('Main: Server started');
    Reciever := TThreadReciever.Create(True);
    Reciever.Start;
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
end;

function GetAnswer(Q: string; Default: Boolean = true): Boolean;
var
  s: string;
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
    Application.Terminate
  else
    Result := SameText(s, 'y');
  Writeln();
end;

function GetAnswer(Q: string; Default: string = ''; AddClear: string = ''): string;
var
  s: string;
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
    Application.Terminate
  else if (AddClear <> '') and (s = AddClear) then
    Result := ''
  else
    Result := s;
  Writeln();
end;

procedure TTestStream.ExampleSocket;
var
  WithServer, WithClient: boolean;
  s: string;
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
  S := LowerCase(GetAnswer('n=NoDelay, k=KeepAlive, q=QuickAck or c to clear', SocketOptionsStr, 'c'));
  NoDelay := Pos('n', S) > 0;
  KeepAlive := Pos('k', S) > 0;
  QuickAck := Pos('q', S) > 0;
  if s <> 'c' then
    ini.WriteString('Options', 'SocketOptions', S);
  InternalExampleSocket(WithServer, WithClient);
end;

procedure TTestStream.ExampleHexLine;
var
  Stream: TmnBufferStream;
  Proxy: TmnHexStreamProxy;
  S: string;
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
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  HexProxy: TmnHexStreamProxy;
  GzProxy: TmnDeflateStreamProxy;
begin
  //image.gz is a compressed file of hex file of image
  WriteLn('Read image to gz file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image.gz', fmCreate or fmOpenWrite));
  GzProxy := TmnDeflateStreamProxy.Create([cprsRead, cprsWrite], 9, true);
  Stream.AddProxy(GzProxy);
  HexProxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(HexProxy);

  //GzProxy.Disable;
  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aImageFile)));
  finally
    Stream.Free;
    FreeAndNil(aImageFile);
  end;

  WriteLn('Read gz file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image.gz', fmOpenRead));
  GzProxy := TmnDeflateStreamProxy.Create([cprsRead, cprsWrite], 9, true);
  Stream.AddProxy(GzProxy);
  HexProxy := TmnHexStreamProxy.Create;
  Stream.AddProxy(HexProxy);

  //GzProxy.Disable;
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

constructor TTestStream.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //StopOnException := True;
end;

destructor TTestStream.Destroy;
begin
  inherited Destroy;
end;

type
  TProcedureObject = procedure of object;

procedure TTestStream.DoRun;
var
  s: string;
  n: Integer;
  Commands: array of record
    name: string;
    proc: TProcedureObject;
  end;
  procedure AddProc(Name: string; Proc: TProcedureObject);
  begin
    SetLength(Commands, Length(Commands) + 1);
    Commands[Length(Commands) - 1].name := Name;
    Commands[Length(Commands) - 1].proc := proc;
  end;
begin
  ini := TIniFile.Create(Application.Location + 'Options.ini');
  try
    try
      WriteLn('Welcome to testing Streams');
      WriteLn('');
      InstallConsoleLog;
      Address := ini.ReadString('options', 'Address', sHost);
      AddProc('ExampleSocket: Socket threads', @ExampleSocket);
      AddProc('ExampleSmallBuffer: read write line with small buffer', @ExampleSmallBuffer);
      AddProc('ExampleHexLine: Hex lines', @ExampleHexLine);
      AddProc('ExampleHexImage: Hex image', @ExampleHexImage);
      AddProc('ExampleCopyHexImage: Hex image2 images and read one', @ExampleCopyHexImage);
      AddProc('ExampleGZImage: GZ image', @ExampleGZImage);
      while true do
      begin
        for n := 0 to Length(Commands) - 1 do
          WriteLn(IntToStr(n + 1) + ': ' + Commands[n].name);
        WriteLn('0: Type 0 to exit');
        WriteLn();
        Write('Enter command: ');
        s := '1';
        ReadLn(s);
        WriteLn();
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
          WriteLn();
          Commands[n - 1].proc();
        end;
        WriteLn();
      end;
    except
      on E: Exception do
      begin
        WriteLn(E.Message);
        raise;
      end;
    end;
  finally
    Write('Press Enter to Exit');
    ReadLn();
    ini.Free;
    Terminate;
  end;
end;

begin
  Application := TTestStream.Create(nil);
  Application.Title :='Test Stream';
  Application.Run;
  Application.Free;
end.

