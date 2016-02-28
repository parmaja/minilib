unit mnLogs;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}

{$mode delphi}

interface

uses
  Classes, SysUtils, contnrs;

type
  ILog = interface(IInterface)
    ['{ADAAE11A-FEED-450C-818F-04915E7730AA}']
    procedure LogWrite(const S: string);
  end;

  { TLogDispatcher }

  TLogDispatcher = class(TObjectList)
  private
  protected
  public
    destructor Destroy; override;
    function Add(AObject: TObject): Integer;
    procedure Write(const S: string);
  end;

  TLogFile = class(TInterfacedPersistent, ILog)
  private
    FFileName: string;
    procedure InternalWrite(const S: string);
    function OpenStream: TStream;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure LogWrite(const S: string);
  end;

procedure InstallLogFile(FileName: string);
{$ifdef FPC}
procedure InstallLogExcept(WithIO: Boolean = False);
{$endif}

function Log: TLogDispatcher;

implementation

var
  FLog: TLogDispatcher = nil;

procedure InstallLogFile(FileName: string);
begin
  Log.Add(TLogFile.Create(FileName));
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

procedure InstallLogExcept(WithIO: Boolean);
begin
  if @FOldExceptProc = nil then
    FOldExceptProc := ExceptProc;
  ExceptProc:=@ExceptionOccurred;
end;

procedure InstallLogIO;
begin
end;
{$endif}

function Log: TLogDispatcher;
begin
  if not Assigned(FLog) then
    FLog := TLogDispatcher.Create;
  Result := FLog;
end;

{ TLogDispatcher }

destructor TLogDispatcher.Destroy;
begin
  FLog := nil;
  inherited;
end;

function TLogDispatcher.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TLogDispatcher.Write(const S: string);
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    (Items[i] as ILog).LogWrite(S);
  end;
end;

{ TLogFile }

constructor TLogFile.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName
end;

destructor TLogFile.Destroy;
begin
  inherited;
end;

procedure TLogFile.InternalWrite(const S: string);
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

function TLogFile.OpenStream: TStream;
begin
  try
    if not FileExists(FFileName) then
    begin
      ForceDirectories(ExtractFilePath(FFileName));
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

procedure TLogFile.LogWrite(const S: string);
var
  a: string;
begin
  a := StringReplace(s, #13#10, ' ', []);
  a := StringReplace(a, #13, ' ', []);
  InternalWrite(a + LineEnding);
end;

initialization
finalization
  FreeAndNil(FLog);
end.
