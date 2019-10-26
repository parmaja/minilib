program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  hexcipher,
  mnStreams, mnSockets, mnClients, mnServers;

type

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
    procedure Example1;
    procedure Example2;
    procedure Example3;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  HexProxy: TmnStreamHexProxy;
  S: string;
begin
  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmCreate or fmOpenWrite));
  HexProxy := TmnStreamHexProxy.Create;
  Stream.AddProxy(HexProxy);
  try
    Stream.WriteString('0123456789');
  finally
    FreeAndNil(Stream);
  end;


  Stream := TmnWrapperStream.Create(TFileStream.Create(Location + 'test_hex.txt', fmOpenRead));
  HexProxy := TmnStreamHexProxy.Create;
  Stream.AddProxy(HexProxy);
  try
    S := Stream.ReadString(10);
    WriteLn('"' + S + '"');
  finally
    FreeAndNil(Stream);
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

procedure TTestStream.DoRun;
begin
  try
    Example3;
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

