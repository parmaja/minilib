program Universal;

{$mode objfpc}{$H+}

uses
  //{.$ifdef DEBUG}heaptrc,{$endif}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, MainForms, appSchema, mncORM, mncMeta, mncPGORM, mncFirebird,
  mncMySQLORM, mncSQLiteORM
  { you can add units after this };

{$R *.res}

begin
{  {$ifdef DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
{$endif}}
  RequireDerivedFormResource :=True;
  Application.Title :='Universal';
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

