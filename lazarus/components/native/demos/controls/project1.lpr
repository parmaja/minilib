program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes,
  Forms, Unit1, NativeLib, LResources, lazcontrols, ntvTabs, ntvTabSets, Unit2;

begin
  Application.Initialize;
  Application.BidiMode := bdLeftToRight;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

