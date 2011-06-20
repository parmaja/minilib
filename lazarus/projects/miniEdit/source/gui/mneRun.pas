unit mneRun;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, StrUtils, Classes, mnStreams, mnXMLUtils,
  mnXMLNodes, SyncObjs;

{$i '..\lib\mne.inc'}
type
  TRunErrorType = (
  errError,
  errWarning,
  errParse,
  errNotice
  );

  TRunLog = record
    Error: Integer;
    Caption: string;
    Msg: string;
    FileName: string;
    LineNo: Integer;
  end;

  TOnLogEvent = procedure(Error: Integer; Caption, Msg, FileName: string; LineNo: Integer) of object;
  TOnBuffer = procedure(const Buffer: string) of object;

  TRunProject = class(TThread)
  private
    LogInfo: TRunLog;
    FFileName: string;
    FCheckOnly: Boolean;
    //
    FBuffer: string;
    FOnLog: TOnLogEvent;
    FOnBuffer: TOnBuffer;
    FConsole: TObject;
    procedure SendLog;
    procedure SendBuffer;
  protected
    procedure RunBeforeExecute(Sender: TObject);
    procedure RunScriptError(Sender: TObject; AText: string; AType: TRunErrorType; AFileName: string; ALineNo: Integer);
    procedure Log(Error: Integer; Caption, Msg, FileName: string; LineNo: Integer);
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    property Console: TObject read FConsole write FConsole;
    property FileName: string read FFileName write FFileName;
    property CheckOnly: Boolean read FCheckOnly write FCheckOnly;
    property OnLog: TOnLogEvent read FOnLog write FOnLog;
    property OnBuffer: TOnBuffer read FOnBuffer write FOnBuffer;
  end;

const
  RunErrorTypes:array[TRunErrorType] of string = (
    'Error',
    'Warning',
    'Parse',
    'Notice'
  );

var
  ProjectLock: TCriticalSection = nil;

implementation

{ TRunProject }

constructor TRunProject.Create;
begin
  inherited Create(True);
end;

destructor TRunProject.Destroy;
begin
  inherited;
end;

procedure TRunProject.Execute;
begin
  while not Terminated do
  begin
    if CheckOnly then
    begin
    end
    else
    begin
      Synchronize(SendBuffer);
    end;
    Console := nil; //zaher
  end;
end;

procedure TRunProject.SendLog;
begin
  ProjectLock.Enter;
  try
    if Assigned(FOnLog) then
      FOnLog(LogInfo.Error, LogInfo.Caption, LogInfo.Msg, LogInfo.FileName, LogInfo.LineNo);
  finally
    ProjectLock.Leave;
  end;
end;

procedure TRunProject.RunBeforeExecute(Sender: TObject);
begin
end;

procedure TRunProject.RunScriptError(Sender: TObject; AText: string;  AType: TRunErrorType; AFileName: string; ALineNo: Integer);
begin
  Log(0, RunErrorTypes[AType], AText, AFileName, ALineNo);
end;

procedure TRunProject.SendBuffer;
begin
  ProjectLock.Enter;
  try
    if Assigned(FOnBuffer) then
      FOnBuffer(FBuffer);
  finally
    ProjectLock.Leave;
  end;
end;

procedure TRunProject.Log(Error: Integer; Caption, Msg, FileName: string; LineNo: Integer);
begin
  LogInfo.Error := Error;
  LogInfo.Caption := Caption;
  LogInfo.Msg := Msg;
  LogInfo.FileName := FileName;
  LogInfo.LineNo := LineNo;
  Synchronize(SendLog);
end;

procedure TRunProject.Run;
begin
  Start;
end;

initialization
  ProjectLock := TCriticalSection.Create;
finalization
  FreeAndNil(ProjectLock);
end.

