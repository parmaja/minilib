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
  LResources, Forms, SysUtils, ComCtrls, StdCtrls, ntvPageControls,
  Controls, ExtCtrls, Classes, ntvRegCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ntvPage1: TntvPage;
    ntvPage2: TntvPage;
    ntvPageControl1: TntvPageControl;
    ntvPageControl1Page1: TntvPage;
    ntvPageControl1Page2: TntvPage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ntvPageControl1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    FCount: Integer;
    FPageControl: TntvPageControl;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FPageControl = nil then
  begin
    FPageControl := TntvPageControl.Create(Self);
    FPageControl.Left := 50;
    FPageControl.Top := 50;
    FPageControl.Parent := Self;
    FPageControl.Visible := True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aPanel: TPanel;
begin
  if FPageControl <> nil then
  begin
    aPanel:=TPanel.Create(Self);
    aPanel.Caption := 'Panel' + IntToStr(FCount);
    aPanel.Name := 'Panel' + IntToStr(FCount);
    aPanel.Parent := FPageControl;
    Inc(FCount);
  end;
end;

initialization
  {$I unit1.lrs}
end.

