program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, zdeflate, zlib, zstream,
  mnStreams, mnSockets, mnClients, mnServers;

type

  { TmnDeflateWriteStreamProxy }

  TmnDeflateWriteStreamProxy = class(TmnStreamOverProxy)
  private
    FZStream: z_stream;
    FBuffer:pointer;
    const
      cBufsize = 16384;
  protected
  public
    constructor Create(Level: TCompressionlevel; GZip: Boolean = False);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    procedure CloseWrite; override;
  end;

  { ThreadSender }

  TThreadSender = class(TThread)
  protected
    procedure Execute; override;
  public

  end;

  { TThreadReciever }

  TThreadReciever = class(TThread)
  protected
    procedure Execute; override;
  public
  end;

  { TTestStream }

  TTestStream = class(TCustomApplication)
  protected
    procedure Example1; //read write line with small buffer
    procedure Example2; //Socket threads
    procedure Example3; //Hex lines
    procedure Example4; //Hex image
    procedure Example5; //Hex image2 images and read one
    procedure Example6; //GZ image
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TmnDeflateWriteStreamProxy }

function TmnDeflateWriteStreamProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
var
  err:smallint;
  Writen, R: longint;
begin
  FZStream.next_in := @Buffer;
  FZStream.avail_in := Count;
  while FZStream.avail_in <> 0 do
  begin
    if FZStream.avail_out = 0 then
    begin
      { Flush the buffer to the stream and update progress }
      Over.Write(FBuffer^, cBufsize, Writen, R);
      { reset output buffer }
      FZStream.next_out := FBuffer;
      FZStream.avail_out := cBufsize;
    end;
    err := deflate(FZStream, Z_NO_FLUSH);
    if err <> Z_OK then
      raise Exception.Create(zerror(err));
  end;
  ResultCount := Count;
  Result := True;
end;

procedure TmnDeflateWriteStreamProxy.CloseWrite;
var
  err: smallint;
  Written, R: longint;
begin
  {Compress remaining data still in internal zlib data buffers.}
  repeat
    if FZStream.avail_out = 0 then
    begin
      { Flush the buffer to the stream and update progress }
      Over.Write(FBuffer^, cBufsize, Written, R);
      { reset output buffer }
      FZStream.next_out := FBuffer;
      FZStream.avail_out := cbufsize;
    end;
    err := deflate(FZStream, Z_FINISH);
    if err = Z_STREAM_END then
      break;
    if (err <> Z_OK) then
      raise Exception.create(zerror(err));
  until false;

  if FZStream.avail_out < cBufsize then
  begin
    Over.Write(FBuffer^, cBufsize - FZStream.avail_out, Written, R);
  end;
  inherited;
end;

constructor TmnDeflateWriteStreamProxy.Create(Level: TCompressionlevel; GZip: Boolean);
var
  err: smallint;
  l: smallint;
  WindowBits: Integer;
const
  MAX_WBITS = 15;
  DEF_MEM_LEVEL = 8;
begin
  inherited Create;
  GetMem(FBuffer, cBufsize);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := cBufsize;

  case level of
    clnone:
      l := Z_NO_COMPRESSION;
    clfastest:
      l := Z_BEST_SPEED;
    cldefault:
      l := Z_DEFAULT_COMPRESSION;
    clmax:
      l := Z_BEST_COMPRESSION;
  end;

  WindowBits := MAX_WBITS;
  if GZip then
    WindowBits := WindowBits + 16;
  err := deflateInit2(FZStream, l, Z_DEFLATED, WindowBits, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY);
  if err <> Z_OK then
    raise Exception.Create(zerror(err));
end;

destructor TmnDeflateWriteStreamProxy.Destroy;
begin
  deflateEnd(FZStream);
  freemem(FBuffer);
  inherited destroy;
end;


{ TThreadReciever }

procedure TThreadReciever.Execute;
var
  Stream: TmnServerSocket;
  s: string;
begin
  WriteLn('Server started');
  Stream := TmnServerSocket.Create('localhost', '82');
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
begin
  Stream := TmnClientSocket.Create('localhost', '82');
  try
    Stream.Connect;
    if Stream.Connected then
    for i := 0 to 10 do
    begin
      s := '0123456789';
      Stream.WriteLine(s);
      if not Stream.Connected then
        break;
    end;
    Stream.Disconnect;
  finally
    Stream.Free;
  end;
end;

{ TTestStream }

procedure TTestStream.Example1;
var
  Stream: TmnBufferStream;
  s: string;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test.txt', fmOpenRead));
  try
    Stream.BufferSize := 5;
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

procedure TTestStream.Example2;
var
  Reciever: TThreadReciever;
  Sender: TThreadSender;
begin
  Reciever := TThreadReciever.Create(True);
  Reciever.Start;
  Sleep(1000);

  Sender := TThreadSender.Create(True);
  Sender.Start;
  Sender.WaitFor;
  Reciever.WaitFor;
end;

procedure TTestStream.Example3;
var
  Stream: TmnBufferStream;
  Proxy: TmnStreamHexProxy;
  S: string;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnStreamHexProxy.Create;
  Stream.AddProxy(Proxy);
  try
    Stream.WriteLineUTF8('0123456789');
    Stream.WriteLineUTF8('0123456789');
    Stream.WriteLineUTF8('0123456789');
  finally
    FreeAndNil(Stream);
  end;


  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmOpenRead));
  Proxy := TmnStreamHexProxy.Create;
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

procedure TTestStream.Example4;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnStreamHexProxy;
begin
  WriteLn('Read image to hex file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnStreamHexProxy.Create;
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
  Proxy := TmnStreamHexProxy.Create;
  Stream.AddProxy(Proxy);
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;
end;

procedure TTestStream.Example5;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnStreamHexProxy;
  aSize: Integer;
begin
  WriteLn('Read image to hex file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image_hex.txt', fmCreate or fmOpenWrite));
  Proxy := TmnStreamHexProxy.Create;
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
  Proxy := TmnStreamHexProxy.Create;
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

procedure TTestStream.Example6;
var
  aImageFile: TFileStream;
  Stream: TmnBufferStream;
  Proxy: TmnDeflateWriteStreamProxy;
begin
  WriteLn('Read image to gz file');
  aImageFile := TFileStream.Create(Location + 'image.jpg', fmOpenRead);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image.gz', fmCreate or fmOpenWrite));
  Proxy := TmnDeflateWriteStreamProxy.Create(clmax, true);
  Stream.AddProxy(Proxy);

  try
    WriteLn('Size write: ' + IntToStr(Stream.WriteStream(aImageFile)));
  finally
    Stream.Free;
    FreeAndNil(aImageFile);
  end;

  {WriteLn('Read gz file to image');
  aImageFile := TFileStream.Create(Location + 'image_copy.jpg', fmCreate or fmOpenWrite);
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'image.gz', fmOpenRead));
  Proxy := TmnDeflateWriteStreamProxy.Create(clmax, false);
  Stream.AddProxy(Proxy);
  try
    WriteLn('Size read: ' + IntToStr(Stream.ReadStream(aImageFile)));
  finally
    FreeAndNil(Stream);
    FreeAndNil(aImageFile);
  end;}
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

procedure TTestStream.DoRun;
begin
  try
    Example6;
  finally
    Write('Press Enter to Exit');
    ReadLn();
    Terminate;
  end;
end;

var
  Application: TTestStream;
begin
  Application :=TTestStream.Create(nil);
  Application.Title :='Test Strean';
  Application.Run;
  Application.Free;
end.

