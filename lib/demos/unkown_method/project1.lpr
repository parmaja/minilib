program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Variants, ComObj;

type

  { TSampleVariant }

  TSampleVariant = class(TInvokeableVariantType)
  protected
    {$ifndef FPC}
    function FixupIdent(const AText: string): string; override;
    {$endif}
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean ); override;
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
  end;

procedure TSampleVariant.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
end;

procedure TSampleVariant.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else with Dest do
    VType:=Source.VType;
end;

function TSampleVariant.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
begin
  assert(V.VType=varType);
  if Name='MyValue' then
  begin
    variant(Dest) := V.VInt64;
    result := true;
  end
  else
    result := false;
end;

function TSampleVariant.SetProperty(var V: TVarData; const Name: string; const Value: TVarData): Boolean;
begin
  assert(V.VType=varType);
  if Name='MyValue' then
  begin
    PVarData(@V)^.VInt64 := variant(Value);
    result := true;
  end
  else
    result := false;
end;

function TSampleVariant.DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  assert(V.VType=varType);
  if Name='Wow' then
  begin
    Writeln('Oh WOW');
    result := true;
  end
  else
    result := false;
end;

type
  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  SampleVariant: TSampleVariant;
  v: Variant;
begin

  // quick check parameters
  ErrorMsg :=CheckOptions('h', 'help');
  if ErrorMsg <>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  SampleVariant := TSampleVariant.Create;
  v := null;

  TVarData(v).vType := SampleVariant.VarType;

  //TVarData(Value).VType:= VarByRef or VarUnknown
  //TVarData(v).vPointer := SampleVariant;

  //TVarData(Value).VType:= vtObject;
  //TVarData(v).Pointer := SampleVariant;

  v.MyValue := 100;
  if v.MyValue<>100 then
    WriteLn('Oh not same')
  else
    WriteLn('Same');

  v.Wow('Test');

  // stop program loop
  WriteLn('Press Enter to exit');
  ReadLn();
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException :=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application :=TMyApplication.Create(nil);
  Application.Title :='My Application';
  Application.Run;
  Application.Free;
end.

