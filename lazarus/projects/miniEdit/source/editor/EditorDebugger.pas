unit EditorDebugger;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  SynEdit;

type
  TEditBreakpoint = record
    Handle: integer;
    FileName: string;
    Line: integer;
  end;

  TEditWatch = record
    Name: string;
    VarType: string;
    Value: variant;
  end;

  { TEditorElements }

  TEditorItem = class(TObject)
  protected
    function GetCount: integer; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    property Count: integer read GetCount;
  end;

  { TEditorBreakPoints }

  TEditorBreakPoints = class(TEditorItem)
  protected
    function GetItems(Index: integer): TEditBreakpoint; virtual; abstract;
  public
    procedure Toggle(FileName: string; LineNo: integer); virtual; abstract;
    function Found(FileName: string; LineNo: integer): boolean; virtual; abstract;
    procedure Add(FileName: string; LineNo: integer); virtual; abstract;
    procedure Remove(FileName: string; Line: integer); virtual; overload; abstract;
    procedure Remove(Handle: integer); virtual; overload; abstract;

    property Items[Index: integer]: TEditBreakpoint read GetItems; default;
  end;

  { TEditorWatches }

  TEditorWatches = class(TEditorItem)
  private
  protected
    function GetItems(Index: integer): TEditWatch; virtual; abstract;
  public
    procedure Add(vName: string); virtual; abstract;
    procedure Remove(vName: string); virtual; abstract;
    function GetValue(vName: string; var vValue: Variant; var vType: string): boolean; virtual; abstract;
    property Items[Index: integer]: TEditWatch read GetItems; default;
  end;

  { TEditorDebugLink }

  TEditorDebugLink = class(TComponent) //to use Notification :P
  private
    procedure SetExecutedExit(const AValue: TCustomSynEdit);
  public
    FExecutedLine: integer;
    FExecutedEdit: TCustomSynEdit;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property ExecutedLine: Integer read FExecutedLine write FExecutedLine;
    property ExecutedEdit: TCustomSynEdit read FExecutedEdit write SetExecutedExit;
  end;

  { TEditorDebugger }

  TEditorDebugger = class(TObject)
  private
    FBreakpoints: TEditorBreakPoints;
    FWatches: TEditorWatches;
    FKey: string;
    FLink: TEditorDebugLink;
    function GetExecutedEdit: TCustomSynEdit;
    function GetExecutedLine: Integer;
    function GetCaption: string; virtual;
    procedure Click(Sender: TObject); virtual;
    procedure SetExecutedEdit(const AValue: TCustomSynEdit);
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
    function CreateBreakPoints: TEditorBreakPoints; virtual; abstract;
    function CreateWatches: TEditorWatches; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start; virtual;
    procedure Stop; virtual;

    procedure Reset; virtual; //stop debug and stop the run
    procedure Resume; virtual; //run and do not stop at breakpoints, or run without debug
    procedure StepInto; virtual;
    procedure StepOver; virtual;
    procedure StepOut; virtual;
    procedure Run; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function IsRuning: boolean; virtual;
    procedure RunTo(FileName: string; LineNo: integer); virtual;//todo runto

    property ExecutedLine: Integer read GetExecutedLine;
    property ExecutedEdit: TCustomSynEdit read GetExecutedEdit write SetExecutedEdit;

    function GetKey: string; virtual;
    property Active: boolean read GetActive write SetActive;

    procedure SetExecuted(Key: string; Edit: TCustomSynEdit; const Line: integer); overload;
    procedure SetExecuted(Key: string; FileName: string; const Line: integer); overload;
    property Breakpoints: TEditorBreakPoints read FBreakpoints;
    property Watches: TEditorWatches read FWatches;
  end;

implementation

uses
  EditorEngine;

{ TEditorDebugger }

function TEditorDebugger.GetCaption: string;
begin
  Result := 'Debug';
end;

procedure TEditorDebugger.Click(Sender: TObject);
begin
  Active := not Active;
end;

procedure TEditorDebugger.SetExecutedEdit(const AValue: TCustomSynEdit);
begin
  FLink.ExecutedEdit := AValue;
end;

constructor TEditorDebugger.Create;
begin
  inherited;
  FLink := TEditorDebugLink.Create(nil);
  FBreakpoints := CreateBreakPoints;
  FWatches := CreateWatches;
end;

destructor TEditorDebugger.Destroy;
begin
  FLink.ExecutedEdit := nil;//just for safe free
  FreeAndNil(FBreakpoints);
  FreeAndNil(FWatches);
  FreeAndNil(FLink);
  inherited;
end;

function TEditorDebugger.GetActive: boolean;
begin
  Result := False;
end;

function TEditorDebugger.GetExecutedEdit: TCustomSynEdit;
begin
  Result := FLink.ExecutedEdit;
end;

function TEditorDebugger.GetExecutedLine: Integer;
begin
  Result := FLink.ExecutedLine;
end;

procedure TEditorDebugger.SetActive(const AValue: boolean);
begin
end;

procedure TEditorDebugger.Start;
begin

end;

procedure TEditorDebugger.Stop;
begin

end;

procedure TEditorDebugger.Reset;
begin

end;

procedure TEditorDebugger.StepInto;
begin

end;

procedure TEditorDebugger.StepOver;
begin

end;

procedure TEditorDebugger.StepOut;
begin
end;

procedure TEditorDebugger.Run;
begin

end;

procedure TEditorDebugger.Resume;
begin
end;

procedure TEditorDebugger.Lock;
begin
end;

procedure TEditorDebugger.Unlock;
begin
end;

function TEditorDebugger.IsRuning: boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.RunTo(FileName: string; LineNo: integer);
begin
end;

function TEditorDebugger.GetKey: string;
begin
  Result := FKey;
end;

procedure TEditorDebugger.SetExecuted(Key: string; Edit: TCustomSynEdit; const Line: integer);
var
  OldLine: integer;
  OldEdit: TCustomSynEdit;
begin
  FKey := Key;
  if (FLink.ExecutedEdit <> Edit) or (FLink.ExecutedLine <> Line) then
  begin
    OldLine := FLink.ExecutedLine;
    OldEdit := FLink.ExecutedEdit;

    FLink.ExecutedLine := Line;
    FLink.ExecutedEdit := Edit;

    if OldEdit <> nil then
      OldEdit.InvalidateLine(OldLine);

    if ExecutedEdit <> nil then
    begin
      ExecutedEdit.CaretY := FLink.ExecutedLine;
      ExecutedEdit.CaretX := 1;
      ExecutedEdit.InvalidateLine(FLink.ExecutedLine);
    end;
  end;
  Engine.UpdateState([ecsDebug, ecsShow]);
end;

procedure TEditorDebugger.SetExecuted(Key: string; FileName: string; const Line: integer);
var
  aFile: TEditorFile;
begin
  inherited;
  aFile := Engine.Files.ShowFile(FileName);
  SetExecuted(Key, aFile.SynEdit, Line);
end;

procedure TEditorDebugLink.SetExecutedExit(const AValue: TCustomSynEdit);
begin
  if FExecutedEdit <> AValue then
  begin
    if FExecutedEdit <> nil then
      RemoveFreeNotification(FExecutedEdit);
    FExecutedEdit :=AValue;
    if FExecutedEdit <> nil then
      FreeNotification(FExecutedEdit)
  end;
end;

procedure TEditorDebugLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FExecutedEdit) then
  begin
    FExecutedEdit := nil;
    FExecutedLine := 0;
  end;
end;

end.

