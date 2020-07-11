unit mnLogs;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
{$ifdef FPC}
{$mode delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  {$ifdef WINDOWS} Windows, {$endif}
  {$ifdef FPC}
  {$else}
  System.Types,
  {$endif}
  Classes, SysUtils, Contnrs, syncobjs;

type

  { ILog }

  ILog = interface(IInterface)
    ['{ADAAE11A-FEED-450C-818F-04915E7730AA}']
    procedure LogWrite(S: string);
  end;

  { TLogDispatcher }

  TLogDispatcher = class(TObjectList)
  private
  protected
  public
    destructor Destroy; override;
    function Add(AObject: TObject): Integer;
    procedure Write(S: string); overload;
    procedure WriteLn(S: string);

    procedure Write(R: TRect); overload;
    procedure Write(I: Integer); overload;
    procedure Write(X, Y: Integer); overload;
    procedure Write(S: string; I: Integer); overload;
  end;

  { TFileLog }

  TFileLog = class(TInterfacedPersistent, ILog)
  private
    FFileName: string;
    procedure InternalWrite(S: string);
    function OpenStream: TStream;
    procedure LogWrite(S: string);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
  end;

  TLogEvent = procedure(S: String) of object;

  { TEventLog }

  TEventLog = class(TInterfacedPersistent, ILog)
  private
    Event: TLogEvent;
    procedure LogWrite(S: string);
  public
    constructor Create(AEvent: TLogEvent);
  end;

  { TDebugOutputLog }

  TDebugOutputLog = class(TInterfacedPersistent, ILog)
  private
    procedure LogWrite(S: string);
  public
  end;

  { TConsoleLog }

  TConsoleLog = class(TInterfacedPersistent, ILog)
  private
    procedure LogWrite(S: string);
  public
  end;

procedure InstallFileLog(FileName: string);
procedure InstallEventLog(AEvent: TLogEvent);
procedure InstallConsoleLog;
procedure InstallDebugOutputLog;
{$ifdef FPC}
procedure InstallExceptLog(WithIO: Boolean = False);
{$endif}

function Log: TLogDispatcher;

implementation

var
  FLog: TLogDispatcher = nil;
  Lock: TCriticalSection = nil;

function Log: TLogDispatcher;
begin
  if not Assigned(FLog) then
  begin
    Lock := TCriticalSection.Create;
    FLog := TLogDispatcher.Create(True);
  end;
  Result := FLog;
end;

procedure InstallFileLog(FileName: string);
begin
  Log.Add(TFileLog.Create(FileName));
end;

procedure InstallEventLog(AEvent: TLogEvent);
begin
  Log.Add(TEventLog.Create(AEvent));
end;

procedure InstallConsoleLog;
begin
  Log.Add(TConsoleLog.Create);
end;

procedure InstallDebugOutputLog;
begin
  Log.Add(TDebugOutputLog.Create);
end;

{$ifdef FPC}
var
  FOldExceptProc: TExceptProc = nil;
  HandlingException: Boolean  = False;

procedure ExceptionOccurred(Sender: TObject; Addr:Pointer; FrameCount: Longint; Frames: PPointer);
  procedure Dump;
  var
    fn: string;
    f: text;
  begin
    fn := ExtractFilePath(ParamStr(0)) + 'stack.txt';
    Assign(f, fn);
    Rewrite(f);
    Dump_Stack(f, get_frame);
    Close(f);
  end;
Begin
  if HandlingException then
    Halt;
  HandlingException := True;
  if Sender <> nil then
  begin
    if Sender is Exception then
    begin
      Log.Write('Exception: ' + Exception(Sender).Message);
      //Log.Write(GetStackTrace(False));
    end;
    //Dump;
  end;
  if @FOldExceptProc <> nil then
    FOldExceptProc(Sender, Addr, FrameCount, Frames);
  HandlingException := False;
end;

procedure InstallExceptLog(WithIO: Boolean);
begin
  if @FOldExceptProc = nil then
    FOldExceptProc := ExceptProc;
  ExceptProc:=@ExceptionOccurred;
end;

procedure InstallLogIO;
begin
end;
{$endif}

{ TEventLog }

procedure TEventLog.LogWrite(S: string);
begin
  if Assigned(Event) then
    Event(S);
end;

constructor TEventLog.Create(AEvent: TLogEvent);
begin
  inherited Create;
  Event := AEvent;
end;

{ TConsoleLog }

procedure TConsoleLog.LogWrite(S: string);
begin
  if IsConsole then
    Write(S);
end;

{ TDebugOutputLog }

procedure TDebugOutputLog.LogWrite(S: string);
begin
  {$ifdef WINDOWS}
  s := IntToStr(GetTickCount64) + ': ' +s;
  {$ifdef FPC}
  OutputDebugString(PAnsiChar(S));
  {$else}
  OutputDebugString(PWideChar(S));
  {$endif}
  {$else}
  {$endif}
end;

{ TLogDispatcher }

destructor TLogDispatcher.Destroy;
begin
  FLog := nil;
  inherited;
end;

function TLogDispatcher.Add(AObject: TObject): Integer;
begin
  if not Supports(AObject, ILog) then
    raise Exception.Create('Object is no ILog');

  Result := inherited Add(AObject);
end;

procedure TLogDispatcher.Write(S: string);
var
  i: Integer;
  ALog: ILog;
begin
  Lock.Enter;
  try
    for i := 0 to Count -1 do
    begin
      ALog := ILog(Pointer(Items[i]));
      ALog.LogWrite(S);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TLogDispatcher.WriteLn(S: string);
begin
  Write(s + #13#10);
end;

procedure TLogDispatcher.Write(R: TRect);
begin
  Write('Rect(Left:'+IntToStr(R.Left)+', Right:' + IntToStr(R.Right)+', Top:' + IntToStr(R.Top)+', Bottom:' + IntToStr(R.Bottom) + ')');
end;

procedure TLogDispatcher.Write(I: Integer);
begin
  Write(IntToStr(I));
end;

procedure TLogDispatcher.Write(X, Y: Integer);
begin
  Write(IntToStr(X) + ', ' + IntToStr(Y));
end;

procedure TLogDispatcher.Write(S: string; I: Integer);
begin
  Write(S + ': ' + IntToStr(I));
end;

{ TFileLog }

constructor TFileLog.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName
end;

destructor TFileLog.Destroy;
begin
  inherited;
end;

procedure TFileLog.InternalWrite(S: string);
var
  aStream: TStream;
begin
  aStream := OpenStream;
  if aStream <> nil then
  try
    aStream.Write(PChar(s)^, Length(s));
  finally
    aStream.Free;
  end;
end;

function TFileLog.OpenStream: TStream;
var
  aPath: string;
begin
  try
    if not FileExists(FFileName) then
    begin
      aPath := ExtractFilePath(FFileName);
      if aPath <> '' then
        ForceDirectories(aPath);
      Result := TFileStream.Create(FFileName, fmCreate or fmOpenWrite or fmShareDenyNone)
    end
    else
    begin
      Result := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyNone);
      Result.Seek(0, soFromEnd);
    end;
  except
    Result := nil;
  end;
end;

procedure TFileLog.LogWrite(S: string);
var
  a: string;
begin
  a := StringReplace(s, #13#10, ' ', []);
  a := StringReplace(a, #13, ' ', []);
  InternalWrite(a + #13#10);
end;

initialization
finalization
  FreeAndNil(FLog);
  FreeAndNil(Lock);
end.
