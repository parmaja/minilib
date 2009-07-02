unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtDlgs, DotMatrix;

type

  { TForm1 }

  TForm1 = class(TForm)
    DotMatrix1: TDotMatrix;
    DrawBtn: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

initialization
  {$I unit1.lrs}

end.

