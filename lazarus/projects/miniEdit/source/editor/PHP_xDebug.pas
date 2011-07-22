unit PHP_xDebug;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, syncobjs,
  mnServers, dbgpServers,
  SynEdit,
  EditorDebugger;

type
  TPHP_xDebug = class;

  TPHP_xDebugServer = class(TdbgpServer)
  private
    FKey: string;
  protected
    FDebug: TPHP_xDebug;
    procedure DoChanged(vListener: TmnListener); override;
  public
    destructor Destroy; override;
    property Key: string read FKey;
  end;

  { TPHP_xDebugBreakPoints }

  TPHP_xDebugBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TPHP_xDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TEditBreakpoint; override;
  public
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function Found(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(FileName: string; Line: integer); override; overload;
    procedure Remove(Handle: integer); override; overload;
  end;

  { TPHP_xDebugWatches }

  TPHP_xDebugWatches = class(TEditorWatches)
  protected
    FDebug: TPHP_xDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TEditWatch; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; var vValue: variant; var vType: string): boolean; override;
  end;

  { TPHP_xDebug }

  TPHP_xDebug = class(TEditorDebugger)
  private
    FServer: TPHP_xDebugServer;
  protected
    function GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;
    procedure DoShowFile(const Key, FileName: string; Line: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Reset; override;
    procedure Resume; override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Run; override;
    procedure RunTo(FileName: string; LineNo: integer); override;
    procedure Lock; override;
    procedure Unlock; override;
    function IsRuning: boolean; override;
    function GetKey: string; override;
  end;

implementation

{ TPHP_xDebugWatches }

function TPHP_xDebugWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TPHP_xDebugWatches.GetItems(Index: integer): TEditWatch;
var
  aWt: TdbgpWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result.Name := aWt.VariableName;
  Result.Value := aWt.Value;
  Result.VarType := aWt.VariableType;
end;

procedure TPHP_xDebugWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TPHP_xDebugWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TPHP_xDebugWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

function TPHP_xDebugWatches.GetValue(vName: string; var vValue: variant; var vType: string): boolean;
var
  aAction: TdbgpGetWatchInstance;
begin
  Result := False;
  if FDebug.IsRuning then   //there is a connection from XDebug
  begin
    aAction := TdbgpGetWatchInstance.Create;
    aAction.CreateEvent;
    aAction.VariableName := vName;
    with FDebug.FServer do
    begin
      AddAction(aAction);
      Resume;
      aAction.Event.WaitFor(INFINITE);
      begin
        vValue := aAction.VariableValue;
        vType := aAction.VariableType;
        Result := True;
      end;
      ExtractAction(aAction);
      aAction.Free;
    end;
  end;
end;

{ TPHP_xDebugBreakPoints }

function TPHP_xDebugBreakPoints.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Breakpoints.Count;
end;

function TPHP_xDebugBreakPoints.GetItems(Index: integer): TEditBreakpoint;
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TPHP_xDebugBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
end;

procedure TPHP_xDebugBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
end;

function TPHP_xDebugBreakPoints.Found(FileName: string; LineNo: integer): boolean;
begin
  with FDebug.FServer do
    Result := Breakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TPHP_xDebugBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
end;

procedure TPHP_xDebugBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);
  if aBP <> nil then
    with FDebug.FServer do
      Breakpoints.Remove(aBP);
end;

procedure TPHP_xDebugBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
end;

{ TPHP_xDebug }

function TPHP_xDebug.GetActive: boolean;
begin
  Result := FServer.Active;
end;

procedure TPHP_xDebug.SetActive(const AValue: boolean);
begin
  FServer.Active := AValue;
end;

function TPHP_xDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TPHP_xDebugBreakPoints.Create;
  (Result as TPHP_xDebugBreakPoints).FDebug := Self;
end;

function TPHP_xDebug.CreateWatches: TEditorWatches;
begin
  Result := TPHP_xDebugWatches.Create;
  (Result as TPHP_xDebugWatches).FDebug := Self;
end;

procedure TPHP_xDebug.DoShowFile(const Key, FileName: string; Line: integer);
begin
  SetExecuted(Key, FileName, Line);
end;

constructor TPHP_xDebug.Create;
begin
  inherited Create;
  FServer := TPHP_xDebugServer.Create(nil);
  FServer.FDebug := Self;
  DBGP.OnShowFile := @DoShowFile;
end;

destructor TPHP_xDebug.Destroy;
begin
  FreeAndNil(FServer);
  DBGP.OnShowFile := nil;
  inherited;
end;

procedure TPHP_xDebug.Start;
begin
  FServer.Start;
end;

procedure TPHP_xDebug.Stop;
var
  aAction: TdbgpDetach;
begin
  if IsRuning then
  begin
    aAction := TdbgpDetach.Create;
    aAction.CreateEvent;
    FServer.AddAction(aAction);
    FServer.Resume;
    aAction.Event.WaitFor(INFINITE);
    aAction.Free;
  end;
  FServer.Stop;
end;

procedure TPHP_xDebug.Reset;
begin
  FServer.Clear; //no need to any exists actions
  FServer.AddAction(TdbgpStop.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.Resume;
begin
  FServer.AddAction(TdbgpDetach.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepInto;
begin
  FServer.AddAction(TdbgpStepInto.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepOver;
begin
  FServer.AddAction(TdbgpStepOver.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepOut;
begin
  FServer.AddAction(TdbgpStepOut.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.Run;
begin
  FServer.AddAction(TdbgpRun.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.Lock;
begin
  DBGP.Lock.Enter;
end;

procedure TPHP_xDebug.Unlock;
begin
  DBGP.Lock.Leave;
end;

function TPHP_xDebug.IsRuning: boolean;
begin
  Result := FServer.IsRuning;
end;

procedure TPHP_xDebug.RunTo(FileName: string; LineNo: integer);
begin
end;

function TPHP_xDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

{procedure TPHP_xDebugServer.ShowFile(const Key, FileName: string; Line: integer);
begin
  //FDebug.SetExecuted(Key, FileName, Line);
end;}

procedure TPHP_xDebugServer.DoChanged(vListener: TmnListener);
begin
  inherited;
{  if vListener.Count = 0 then
    FDebug.SetExecuted('', nil, -1);}
end;

destructor TPHP_xDebugServer.Destroy;
begin
  inherited;
end;

initialization
//  Addons.Add('Debug', 'XDebug', TPHP_xDebug);//most not created /??!!!
end.

