unit Unit1; 
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, IntfGraphics, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  FPCanvas, FPImage,
  StdCtrls, ExtCtrls, ntvDotMatrix;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    ThemeList: TComboBox;
    Edit1: TEdit;
    Image1: TImage;
    TextDotMatrix: TTextDotMatrix;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThemeListSelect(Sender: TObject);
  private
    //DotMatrix: TDotMatrix;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Edit1Change(Sender: TObject);
begin
  TextDotMatrix.Text := Edit1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ThemeList.Items.Add('tdmClassicLCD');
  ThemeList.Items.Add('tdmOrange');
  ThemeList.Items.Add('tdmGreenLED');
  ThemeList.Items.Add('tdmRedLED');
  ThemeList.Items.Add('tdmBlueLED');
end;

procedure TForm1.ThemeListSelect(Sender: TObject);
begin
  case ThemeList.ItemIndex of
    0: TextDotMatrix.Dots.Theme := tdmClassicLCD;
    1: TextDotMatrix.Dots.Theme := tdmOrange;
    2: TextDotMatrix.Dots.Theme := tdmGreenLED;
    3: TextDotMatrix.Dots.Theme := tdmRedLED;
    4: TextDotMatrix.Dots.Theme := tdmBlueLED;
  end
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TextDotMatrix.Text := '';
  TextDotMatrix.Dots.Canvas.Draw(0, 0, Image1.Picture.Graphic);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  TextDotMatrix.Dots.Bright := CheckBox1.Checked;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I Unit1.lrs}

end.

