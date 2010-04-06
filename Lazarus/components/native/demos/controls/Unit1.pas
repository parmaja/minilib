unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Alhamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, ComCtrls, StdCtrls, ntvPageControls,
  Controls, ExtCtrls, Classes, ntvRegCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    ntvPage1: TntvPage;
    ntvPage2: TntvPage;
    ntvPageControl1: TntvPageControl;
    ntvPageControl1Page1: TntvPage;
    ntvPageControl1Page2: TntvPage;
    procedure FormCreate(Sender: TObject);
    procedure ntvPageControl1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ntvPageControl1Click(Sender: TObject);
begin

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I unit1.lrs}
end.

