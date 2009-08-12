unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ntvProgressBars, ntvPageControls, ntvDotMatrix;

type

  { TForm1 }

  TForm1 = class(TForm)
    ntvGauge1: TntvGauge;
    ntvPage1: TntvPage;
    ntvPage2: TntvPage;
    ntvPageControl1: TntvPageControl;
    ntvPageControl1Page0: TntvPage;
    ntvPageControl1Page1: TntvPage;
    ntvPageControl1Page2: TntvPage;
    ntvProgressBar1: TntvProgressBar;
    procedure ntvPageControl1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ntvPageControl1Click(Sender: TObject);
begin

end;

initialization
  {$I unit1.lrs}

end.

