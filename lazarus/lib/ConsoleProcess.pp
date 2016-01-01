unit ConsoleProcess;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  TmnOnWrite = procedure(S: string) of object;

  { TmnProcessObject }

  TmnProcessObject = class(TObject)
  protected
    FBuffer: string;
  public
    Process: TProcess;
    Thread: TThread;
    OnWrite: TmnOnWrite;
    function IsTerminated: Boolean;
    function Read: Integer;
    procedure FlushBuffer;
    constructor Create(AProcess: TProcess; AThread: TThread; AOnWrite: TmnOnWrite);
  end;

  { TmnConsoleThread }

  TmnConsoleThread = class(TThread)
  private
    FOnWrite: TmnOnWrite;
    FParameters: TStrings;
    procedure SetParameters(AValue: TStrings);
  protected
  public
    Process: TProcess;
    Status: Integer;
    Executable: string;
    CurrentDirectory: string;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    Property Parameters : TStrings Read FParameters Write SetParameters;
    property OnWrite: TmnOnWrite read FOnWrite write FOnWrite;
  end;

implementation

type
  THackThread = class(TThread)
  end;

procedure TmnProcessObject.FlushBuffer;
begin
  if Assigned(OnWrite) then
  begin
    OnWrite(FBuffer);
    FBuffer := '';
  end;
end;

constructor TmnProcessObject.Create(AProcess: TProcess; AThread: TThread; AOnWrite: TmnOnWrite);
begin
  inherited Create;
  Process := AProcess;
  Thread := AThread;
  OnWrite := AOnWrite;
end;

function TmnProcessObject.IsTerminated: Boolean;
begin
  Result := (Thread <> nil) and THackThread(Thread).Terminated;
end;

function TmnProcessObject.Read: Integer;
const
  READ_BYTES = 1024;
var
  C, Count, L: DWORD;
  FirstTime: Boolean;
begin
  FirstTime := True;
  Count := 0;
  C := 0;
    try
      Process.Execute;
      while not IsTerminated and (FirstTime or Process.Running or (C > 0)) do
      begin
        L := Length(FBuffer);
        Setlength(FBuffer, L + READ_BYTES);
        C := Process.Output.Read(FBuffer[1 + L], READ_BYTES);
        if Assigned(OnWrite) then
        begin
          SetLength(FBuffer, L + C);
          if Length(FBuffer) > 0 then
          begin
            if Thread <> nil then
              Thread.Synchronize(Thread, @FlushBuffer)
            else
              FlushBuffer;
          end;
        end;

        if C > 0 then
          Inc(Count, C)
        else
          Sleep(100);

        if not Assigned(OnWrite) then
        begin
          //SetLength(FBuffer, L + C);
        end;

        FirstTime := False;
      end;

      Process.WaitOnExit;

      Result := Process.ExitStatus;
    except
      on e : Exception do
      begin
        if Process.Running and IsTerminated then
          Process.Terminate(0);
      end;
    end;
end;

{ TmnConsoleThread }

procedure TmnConsoleThread.SetParameters(AValue: TStrings);
begin
  FParameters.Assign(AValue);
end;

constructor TmnConsoleThread.Create;
begin
  inherited Create(true);
  FParameters := TStringList.Create;
end;

destructor TmnConsoleThread.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TmnConsoleThread.Execute;
const
  READ_BYTES = 1024;
var
  ProcessObject: TmnProcessObject;
begin
  Process := TProcess.Create(nil);
  Process.Options :=  [poUsePipes];
  Process.ShowWindow := swoHIDE;

  //Process.PipeBufferSize := 10;
  //Process.ConsoleTitle := Info.RunFile;
  Process.Executable := Executable;
  Process.Parameters := Parameters;
  Process.CurrentDirectory := CurrentDirectory;
  Process.InheritHandles := True;

  ProcessObject := TmnProcessObject.Create(Process, Self, OnWrite);
  try
    ProcessObject.Read;
  finally
    FreeAndNil(Process);
    FreeAndNil(ProcessObject);
  end;
end;

end.

