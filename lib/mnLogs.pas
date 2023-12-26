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

  TLogLevel = (lglInfo, lglWarning, lglDebug);

  { ILog }

  ILog = interface(IInterface)
    ['{ADAAE11A-FEED-450C-818F-04915E7730AA}']
    procedure LogWrite(S: string);
  end;

  TLogDispatcherItem = class(TObject)
  public
    LogLevel: TLogLevel;
    LogObject: TObject;
    destructor Destroy; override;
  end;

  { TLogDispatcher }

  TLogDispatcher = class(TObjectList)
  private
  protected
  public
    destructor Destroy; override;
    function Install(ALogLevel: TLogLevel; AObject: TObject): Integer;
    function Add(AObject: TLogDispatcherItem): Integer;

    procedure Write(LogLevel: TLogLevel; const S: string); overload;
    procedure WriteLn(LogLevel: TLogLevel; const S: string); overload;

    procedure Write(const S: string); overload; inline;
    procedure WriteLn(const S: string); overload;
    procedure WriteLn(const S: string; const vArgs: array of const); overload;

    procedure Write(R: TRect); overload;
    procedure Write(I: Integer); overload;
    procedure Write(X, Y: Integer); overload;
    procedure Write(const S: string; I: Integer); overload;
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

procedure InstallFileLog(FileName: string; LogLevel: TLogLevel = lglDebug);
procedure InstallEventLog(AEvent: TLogEvent; LogLevel: TLogLevel = lglDebug);
procedure UninstallEventLog(AEvent: TLogEvent; LogLevel: TLogLevel = lglDebug);
procedure InstallConsoleLog(LogLevel: TLogLevel = lglDebug);
procedure InstallDebugOutputLog(LogLevel: TLogLevel = lglDebug);
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

procedure InstallFileLog(FileName: string; LogLevel: TLogLevel);
begin
  Log.Install(LogLevel, TFileLog.Create(FileName));
end;

procedure InstallEventLog(AEvent: TLogEvent; LogLevel: TLogLevel);
begin
  Log.Install(LogLevel, TEventLog.Create(AEvent));
end;

procedure UninstallEventLog(AEvent: TLogEvent; LogLevel: TLogLevel);
var
  i: Integer;
begin
  for i := 0 to log.Count -1 do
  begin
    if Log[i] is TEventLog then
    begin
      if @(Log[i] as TEventLog).Event = @AEvent then
      begin
        Log.Delete(i);
        exit;
      end;
    end;
  end;
  raise Exception.Create('There is no Event install for it');
end;

procedure InstallConsoleLog(LogLevel: TLogLevel);
begin
  Log.Install(LogLevel, TConsoleLog.Create);
end;

procedure InstallDebugOutputLog(LogLevel: TLogLevel);
begin
  Log.Install(LogLevel, TDebugOutputLog.Create);
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
  begin
    System.Write(S);
  end;
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

function TLogDispatcher.Add(AObject: TLogDispatcherItem): Integer;
begin
  Result := inherited Add(AObject);
end;

destructor TLogDispatcher.Destroy;
begin
  FLog := nil;
  inherited;
end;

function TLogDispatcher.Install(ALogLevel: TLogLevel; AObject: TObject): Integer;
var
  item: TLogDispatcherItem;
begin
  {$ifndef FPC} //Delphi
  if not (AObject is TInterfacedPersistent) then
    raise Exception.Create('Object is not InterfacedPersistent');
  {$endif}

  if not Supports(AObject, ILog) then
    raise Exception.Create('Object is no ILog');

  item := TLogDispatcherItem.Create;
  item.LogLevel := ALogLevel;
  item.LogObject := AObject;
  Result := inherited Add(item);
end;

procedure TLogDispatcher.Write(LogLevel: TLogLevel; const S: string);
var
  i: Integer;
  ALog: ILog;
  item: TLogDispatcherItem;
begin
  Lock.Enter;
  try
    for i := 0 to Count -1 do
    begin
      item := Items[i] as TLogDispatcherItem;
      {$ifdef FPC}
      ALog := (item.LogObject as ILog);
      {$else}
      ALog := (TInterfacedPersistent(item.LogObject) as ILog);
      {$endif}
      if item.LogLevel >= LogLevel then
        ALog.LogWrite(S);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TLogDispatcher.WriteLn(const S: string);
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

procedure TLogDispatcher.Write(const S: string; I: Integer);
begin
  Write(S + ': ' + IntToStr(I));
end;

procedure TLogDispatcher.Write(const S: string);
begin
  Write(lglInfo, S);
end;

procedure TLogDispatcher.WriteLn(LogLevel: TLogLevel; const S: string);
begin
  Write(LogLevel, s + #13#10);
end;

procedure TLogDispatcher.WriteLn(const S: string; const vArgs: array of const);
begin
  Self.WriteLn(Format(s, vArgs));
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

{ TLogDispatcherItem }

destructor TLogDispatcherItem.Destroy;
begin
  FreeAndNil(LogObject);
  inherited;
end;

initialization
finalization
  FreeAndNil(FLog);
  FreeAndNil(Lock);
end.

