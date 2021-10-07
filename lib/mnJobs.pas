unit mnJobs;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}

{$IFDEF FPC}
{$MODE delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$M+}{$H+}

{**
*  To run somthing in background as job
 *}

interface

uses
  Classes, SysUtils, mnClasses, syncobjs;

type
  TJobThread = class;

  TOnFinished = procedure of object;

  TExecuteObjectOptions = set of (
    eooIgnoreError,
    eooSilent, //No Log
    eooRunService, //when run it, move to the next job and keep this one running
    eooRunOnce, //TODO: do not run if one of already running in the pool or current
    eooRunImmediately, //Do not add to the pool, run it when demand (trying to add)
    eooDisabled //Do not execute
  );

  TLogKind = (
    lgLog,
    lgHint, //show in bottom but disapear after seconds
    lgStartStatus,
    lgEndStatus //end lgPermStatus
  );

  TOnLog = procedure(S: String; Kind: TLogKind) of object;

  { TExecuteObject }

  TExecuteObject = class(TObject)
  private
    FMessage: string;
    FExecuteName: string;
    FName: string;
    FNext: TExecuteObject;
    FOptions: TExecuteObjectOptions;
  protected
    FOnFinished: TOnFinished;
    JobThread: TJobThread;
    procedure Created; virtual;
    procedure Log(S: String; Kind: TLogKind = lgLog);
    procedure Ouput(S: string); virtual;
  public
    Terminated: Boolean;
    Status: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Prepare(const AJobThread: TJobThread); virtual;
    procedure Execute; virtual; abstract;
    procedure Done; virtual;
    procedure Unprepare; virtual;
    procedure Kill; virtual;
    procedure Finished; virtual; //executed in main thread
    property OnFinished: TOnFinished read FOnFinished write FOnFinished;
    property Options: TExecuteObjectOptions read FOptions write FOptions;
    property Message: string read FMessage write FMessage;
    procedure SetNext(ANext: TExecuteObject);
    property Next: TExecuteObject read FNext; //Hold the next object to execute, so we can have a chained list of executes
    property ExecuteName: string read FExecuteName write FExecuteName;
    property Name: string read FName write FName;
  end;

  TExecuteObjectClass = class of TExecuteObject;

  TOnPercentProgress = procedure(ProgressPercent: Double; ProgressMessage: string = '') of object;

  { TProgressExecuteObject }

  TProgressExecuteObject = class abstract(TExecuteObject)
  private
    FOnPercentProgress: TOnPercentProgress;
  protected
    ProgressPercent: Double;
    ProgressMessage: String;
    procedure PostProgress;
    procedure ResetProgress;
    procedure SetProgress(const APercent: Double; AProgressMessage: string = '');
    procedure Created; override;
  public
    procedure Unprepare; override;
    property OnPercentProgress: TOnPercentProgress read FOnPercentProgress write FOnPercentProgress;
  end;

  { TJobThread }

  TJobThread = class(TThread)
  private
    FExecuteObject: TExecuteObject;
    FMessage: string;
    FOnLog: TOnLog;
    FStarted: Boolean;
    procedure SetExecuteObject(AValue: TExecuteObject);
  protected
    FLog: String;
    FKind: TLogKind;
    procedure PostLog; virtual; //To Sync
    procedure PostChanged; virtual; //To Sync
  public
    IgnoreError: Boolean;
    constructor Create(vOnLog: TOnLog = nil);
    destructor Destroy; override;
    procedure Kill(WaitIt: Boolean = False);
    procedure Execute; override;
    procedure Log(S: String; Kind: TLogKind = lgLog);
    procedure JobsChanged; //To Sync
    property Started: Boolean read FStarted;
    property OnLog: TOnLog read FOnLog write FOnLog;
    property Message: string read FMessage write FMessage;
    property ExecuteObject: TExecuteObject read FExecuteObject write SetExecuteObject;
  end;

  { TJobsPool }

  TJobsPool = class(TmnObjectList<TJobThread>)
  public
  end;

  { TJobs }

  TJobs = class(TObject)
  private
    FLock: TCriticalSection;
    FTerminated: Boolean;
    function GetUpdateing: Boolean;
    procedure SetTerminated(AValue: Boolean);
  protected
    FPool: TJobsPool;
    FServices: TJobsPool;
    FOnLog: TOnLog;
    FUpdatCount: Integer;
    procedure JobTerminated(Sender: TObject);
    procedure JobsChanged; virtual;
  public
    constructor Create(AOnLog: TOnLog);
    destructor Destroy; override;
    procedure Add(AJob: TJobThread);
    procedure Remove(AJob: TJobThread);
    procedure Resume;
    procedure StopAll(WaitIt: Boolean = False);
    procedure StopCurrent;
    //Suspend resume until
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Enter; //TODO
    procedure Leave;
    property Updateing: Boolean read GetUpdateing;
    procedure LaunchJob(vMessage: String; vExecuteObject: TExecuteObject = nil; AOptions: TExecuteObjectOptions = []);
    property Terminated: Boolean read FTerminated write SetTerminated;
    property Pool: TJobsPool read FPool;
    property Services: TJobsPool read FServices;
    property Lock: TCriticalSection read FLock;
  end;

implementation

{ TProgressExecuteObject }

procedure TProgressExecuteObject.PostProgress;
begin
  if Assigned(FOnPercentProgress) then
    FOnPercentProgress(ProgressPercent, ProgressMessage);
end;

procedure TProgressExecuteObject.ResetProgress;
begin
  SetProgress(0);
end;

procedure TProgressExecuteObject.SetProgress(const APercent: Double; AProgressMessage: string);
begin
  ProgressPercent := APercent;
  ProgressMessage := AProgressMessage;
  JobThread.Synchronize(PostProgress);
end;

procedure TProgressExecuteObject.Unprepare;
begin
  inherited Unprepare;
  ResetProgress;
end;

procedure TProgressExecuteObject.Created;
begin
  inherited;
  ExecuteName := 'Run';
end;

{ TExecuteObject }

constructor TExecuteObject.Create;
begin
  inherited Create;
  ExecuteName := 'Execute';
  Created;
end;

destructor TExecuteObject.Destroy;
begin
  FreeAndNil(FNext);
  inherited;
end;

procedure TExecuteObject.Created;
begin
end;

procedure TExecuteObject.Log(S: String; Kind: TLogKind);
begin
  if not (eooSilent in Options) then
  begin
    if JobThread <> nil then
      JobThread.Log(S, Kind);
  end;
end;

procedure TExecuteObject.Ouput(S: string);
begin
end;

procedure TExecuteObject.Prepare(const AJobThread: TJobThread);
begin
  JobThread := AJobThread;
end;

procedure TExecuteObject.Done;
begin

end;

procedure TExecuteObject.Unprepare;
begin
end;

procedure TExecuteObject.Kill;
begin
  Status := 1;
  Terminated := True;
end;

procedure TExecuteObject.Finished;
begin
  if Assigned(FOnFinished) then
    OnFinished();
end;

procedure TExecuteObject.SetNext(ANext: TExecuteObject);
begin
  if FNext <> nil then
    raise Exception.Create('Alread you set FNext');
  FNext := ANext;
end;

{ TJobThread }

procedure TJobThread.SetExecuteObject(AValue: TExecuteObject);
begin
  if FExecuteObject <> AValue then
    FExecuteObject :=AValue;
end;

procedure TJobThread.PostLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FLog, FKind);
end;

procedure TJobThread.PostChanged;
begin
  //JobsChanged
end;

procedure TJobThread.Log(S: String; Kind: TLogKind);
begin
  FLog := S;
  FKind := Kind;
  Synchronize(PostLog);
  FLog := '';
end;

procedure TJobThread.JobsChanged;
begin
  Queue(PostChanged);
end;

constructor TJobThread.Create(vOnLog: TOnLog);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOnLog := vOnLog;
end;

destructor TJobThread.Destroy;
begin
  FreeAndNil(FExecuteObject);
  inherited;
end;

procedure TJobThread.Kill(WaitIt: Boolean);
begin
  if FExecuteObject <> nil then
    FExecuteObject.Kill;
  Terminate;
  If WaitIt then
    WaitFor;
end;

procedure TJobThread.Execute;
var
  CurrentObject: TExecuteObject;
begin
  FStarted := True;
  JobsChanged;
  try
    CurrentObject := FExecuteObject;
    while not Terminated and (CurrentObject <> nil) do
    begin
      try
        CurrentObject.Prepare(Self);
        Log(Message  + ': ' + CurrentObject.Message + ' (Running)', lgStartStatus);
        CurrentObject.Execute;
        if IgnoreError or ((CurrentObject.Status = 0) and not CurrentObject.Terminated) then
          CurrentObject.Done;
        CurrentObject.Unprepare;
      except
        on E: Exception do
        begin
          CurrentObject.Status := 1;
          Log(E.Message, lgEndStatus);
          raise;
        end;
      end;

      if CurrentObject.Status = 0 then
        Log(Message  + ': ' + CurrentObject.Message + ' (Done)', lgEndStatus)
      else
        Log(Message  + ': ' + CurrentObject.Message + ' (Failed)', lgEndStatus);

      if IgnoreError or ((CurrentObject.Status = 0) and not CurrentObject.Terminated) then
        CurrentObject := CurrentObject.Next
      else
        CurrentObject := nil;
    end;
  finally
    Terminate;
    JobsChanged;
  end;
end;

{ TJobs }

procedure TJobs.StopAll(WaitIt: Boolean);
var
  aJob: TJobThread;
begin
  aJob := nil;
  Lock.Enter;
  try
    if Pool.Count > 0 then
    begin
      aJob := Pool.First;
      aJob.FreeOnTerminate := WaitIt;
      Pool.Clear;
    end;
  finally
    Lock.Leave;
  end;
  if aJob <> nil then
    aJob.Kill(WaitIt);

  while Services.Count > 0 do
  begin
    Lock.Enter;
    try
      aJob := Services.Last;
      aJob.FreeOnTerminate := WaitIt;
      Services.Remove(aJob);
    finally
      Lock.Leave;
    end;
    aJob.Kill(WaitIt);
  end;
  JobsChanged;
end;

procedure TJobs.StopCurrent;
var
  aJob: TJobThread;
begin
  if Pool.Count > 0 then
  begin
    aJob := Pool.First;
    aJob.Kill;
    //aJob.WaitFor;//free on terminate
  end;
  JobsChanged;
end;

procedure TJobs.BeginUpdate;
begin
  Inc(FUpdatCount);
end;

procedure TJobs.EndUpdate;
begin
  Dec(FUpdatCount);
  if FUpdatCount  = 0 then
    Resume;
end;

procedure TJobs.Enter;
begin
  Lock.Enter;
end;

procedure TJobs.Leave;
begin
  Lock.Leave;
end;

procedure TJobs.LaunchJob(vMessage: String; vExecuteObject: TExecuteObject; AOptions: TExecuteObjectOptions);
var
  aJobThread: TJobThread;
  i: Integer;
begin
  vExecuteObject.FOptions := AOptions;
  if eooRunOnce in AOptions then
  begin
    for i := 0 to Pool.Count -1 do
    begin
      //if Items[i].ClassType = vExecuteObject.ClassType then
      //TODO
    end;
  end;
  aJobThread := TJobThread.Create(FOnLog);
  aJobThread.OnTerminate := JobTerminated;
  aJobThread.Message := vMessage;
  aJobThread.ExecuteObject := vExecuteObject;
  aJobThread.IgnoreError := eooIgnoreError in AOptions;
  Add(aJobThread);
  Resume;
end;

function TJobs.GetUpdateing: Boolean;
begin
  Result := FUpdatCount > 0;
end;

procedure TJobs.SetTerminated(AValue: Boolean);
begin
  if FTerminated =AValue then Exit;
  FTerminated :=AValue;
end;

procedure TJobs.JobTerminated(Sender: TObject);
var
  AJob: TJobThread;
begin
  if not Terminated then
  begin
    AJob := Sender as TJobThread;
    AJob.ExecuteObject.Finished;
    Remove(AJob);
    Resume;
  end;
end;

procedure TJobs.JobsChanged;
begin
end;

constructor TJobs.Create(AOnLog: TOnLog);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FPool := TJobsPool.Create(False);
  FServices := TJobsPool.Create(False);
  FOnLog := AOnLog;
end;

destructor TJobs.Destroy;
begin
  FreeAndNil(FServices);
  FreeAndNil(FPool);
  FreeAndNil(FLock);
  inherited;
end;

procedure TJobs.Add(AJob: TJobThread);
begin
  Lock.Enter;
  try
    if (eooRunService in AJob.ExecuteObject.Options) and (eooRunImmediately in AJob.ExecuteObject.Options) then
      Services.Add(AJob)
    else
      Pool.Add(AJob);
  finally
    Lock.Leave;
  end;
  if (eooRunService in AJob.ExecuteObject.Options) and (eooRunImmediately in AJob.ExecuteObject.Options) then
    aJob.Start;
  JobsChanged;
end;

procedure TJobs.Remove(AJob: TJobThread);
begin
  Lock.Enter;
  try
    //extract, it will kill it self
    if (eooRunService in AJob.ExecuteObject.Options) then
      Services.Extract(AJob)
    else
      Pool.Extract(AJob);
  finally
    Lock.Leave;
  end;
  JobsChanged;
end;

procedure TJobs.Resume;
var
  AJob: TJobThread;
begin
  Lock.Enter;
  try
    if (Pool.Count > 0) and not Updateing then
    begin
      AJob := Pool.First;
      if not AJob.Started then
      begin
        if (eooRunService in AJob.ExecuteObject.Options) then
        begin
          Pool.Extract(AJob);
          Services.Add(AJob);
        end;
        AJob.Start;
      end;
    end
  finally
    Lock.Leave;
  end;
end;

initialization
end.
