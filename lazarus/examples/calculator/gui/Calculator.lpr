program Calculator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, Interfaces, Forms, Main;

procedure Run;
var
  hWnd:THandle;
begin
  {$IFDEF WINDOWS}
  hWnd := FindWindow('Window', 'Lazarus Calculator');
  {$else}
  hWnd := 0;
  {$ENDIF}
  if hWnd <> 0 then
  begin
  {$IFDEF WINDOWS}
    ShowWindow(hWnd, SW_SHOW);
    SetForegroundWindow(hWnd);
  {$ENDIF}
  end
  else
  begin
    Application.Initialize;
    Application.Title := 'Lazarus Calculator';
    Application.BidiMode := bdLeftToRight;
    Application.CreateForm(TCalcForm, CalcForm);
    Application.Run;
  end;
end;

{$R Calculator.res}

begin
  Run;
end.

