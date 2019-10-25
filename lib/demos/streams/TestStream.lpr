program TestStream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, hexcipher,
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
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TThreadReciever }

procedure TThreadReciever.Execute;
var
  Stream: TmnServerSocketStream;
  s: string;
begin
  WriteLn('Server started');
  Stream := TmnServerSocketStream.Create('localhost', '82');
  Stream.Connect;
  try
    while true do
    begin
      s := Trim(Stream.ReadLine);
      WriteLn(s);
      if (s = '') or not Stream.Connected then
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
  Stream: TmnClientSocketStream;
  S: string;
  i: Integer;
begin
  Stream := TmnClientSocketStream.Create('localhost', '82');
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

procedure TTestStream.DoRun;
var
  Stream: TmnBufferStream;
  Reciever: TThreadReciever;
  Sender: TThreadSender;
  s: string;
begin
  Reciever := TThreadReciever.Create(True);
  Reciever.Start;
  Sleep(1000);

  Sender := TThreadSender.Create(True);
  Sender.Start;
  Reciever.WaitFor;
  Sender.WaitFor;

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
  Write('Press Enter to Exit');
  ReadLn();
  Terminate;
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

var
  Application: TTestStream;
begin
  Application :=TTestStream.Create(nil);
  Application.Title :='Test Strean';
  Application.Run;
  Application.Free;
end.

