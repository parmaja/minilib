program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, zdeflate, zlib, zstream,
  mnUtils, mnStreams, mnStreamUtils, mnSockets, mnClients, mnServers;

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
    procedure ExampleSmallBuffer; //read write line with small buffer
    procedure ExampleSocket; //Socket threads
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
  NoDelay: Boolean = False;
  KeepAlive: Boolean = False;
  QuickAck: Boolean = False;

{ TThreadReciever }

procedure TThreadReciever.Execute;
var
  Stream: TmnServerSocket;
  s: string;
begin
  WriteLn('Server started');
  Stream := TmnServerSocket.Create('localhost', '82');
  Stream.ReadTimeout := WaitForEver;
  Stream.Options := Stream.Options - [soWaitBeforeRead, soWaitBeforeWrite];
  Stream.Connect;
  try
    while true do
    begin
      s := Trim(Stream.ReadLine);
      WriteLn(s);
      if not Stream.Connected then
        break;
    end;
    Stream.Disconnect;
  finally
    Stream.Free;
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
  ACount: Integer = 2000;
begin
  Stream := TmnClientSocket.Create('localhost', '82');
  Stream.ReadTimeout := WaitForEver;
  Stream.Options := Stream.Options - [soWaitBeforeRead, soWaitBeforeWrite];
  try
    Stream.Connect;
    if Stream.Connected then
    begin
      s := '0123456789';
      Stream.WriteLine(s);
    end;
    t := GetTickCount64 - t;
    WriteLn(t.ToString);
    WriteLn(TicksToString(t));
    Stream.Disconnect;
  finally
    Stream.Free;
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

procedure TTestStream.ExampleSocket;
var
  Reciever: TThreadReciever;
  Sender: TThreadSender;
begin
  NoDelay := False;
  KeepAlive := False;
  QuickAck := False;

  Reciever := TThreadReciever.Create(True);
  Reciever.Start;
  Sleep(1000);

  Sender := TThreadSender.Create(True);
  Sender.Start;
  Sender.WaitFor;
  Reciever.WaitFor;
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
  StopOnException :=True;
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
  try
    AddProc('ExampleSmallBuffer: read write line with small buffer', @ExampleSmallBuffer);
    AddProc('ExampleSocket: Socket threads', @ExampleSocket);
    AddProc('ExampleHexLine: Hex lines', @ExampleHexLine);
    AddProc('ExampleHexImage: Hex image', @ExampleHexImage);
    AddProc('ExampleCopyHexImage: Hex image2 images and read one', @ExampleCopyHexImage);
    AddProc('ExampleGZImage: GZ image', @ExampleGZImage);
    while true do
    begin
      for n := 0 to Length(Commands) - 1 do
        WriteLn(IntToStr(n + 1) + ': ' + Commands[n].name);
      WriteLn();
      Write('Enter command: ');
      ReadLn(s);
      WriteLn();
      s := trim(s);
      n := StrToIntDef(s, 0);
      if (n < 1) or (n > Length(Commands)) then
        exit;
      WriteLn('Running "' + Commands[n - 1].Name + '"');
      WriteLn();
      Commands[n - 1].proc();
    end;
  finally
    Write('Press Enter to Exit');
    ReadLn();
    Terminate;
  end;
end;

var
  Application: TTestStream;
begin
  Application := TTestStream.Create(nil);
  Application.Title :='Test Stream';
  Application.Run;
  Application.Free;
end.

