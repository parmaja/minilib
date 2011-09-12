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
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    ThemeList: TComboBox;
    Edit1: TEdit;
    Image1: TImage;
    TextDotMatrix: TntvTextDotMatrix;
    TimerX: TTimer;
    DimTimer: TTimer;
    TimerY: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure DimTimerTimer(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThemeListSelect(Sender: TObject);
    procedure TimerXTimer(Sender: TObject);
    procedure TimerYTimer(Sender: TObject);
  private
    DimReverse: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  Form1: TForm1; 

implementation

{$r '*.lfm'}

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
  TextDotMatrix.Dots.RotateOffset := True;
  TextDotMatrix.Dots.Power := true;
  TextDotMatrix.Text := Edit1.Text;
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

procedure TForm1.TimerXTimer(Sender: TObject);
begin
  TextDotMatrix.Dots.Scroll(2, 0);
end;

procedure TForm1.TimerYTimer(Sender: TObject);
begin
  TextDotMatrix.Dots.Scroll(0, 1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TextDotMatrix.Text := '';
  TextDotMatrix.Dots.Canvas.Draw(0, 0, Image1.Picture.Graphic);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TimerX.Enabled := not TimerX.Enabled;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  DimTimer.Enabled := not DimTimer.Enabled;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TimerX.Enabled := False;
  TimerY.Enabled := False;
  DimTimer.Enabled := False;
  TextDotMatrix.Dots.Reset;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  TimerY.Enabled := not TimerY.Enabled;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  TextDotMatrix.Dots.Power := not TextDotMatrix.Dots.Power;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  TextDotMatrix.Dots.Glow := CheckBox1.Checked;
end;

procedure TForm1.DimTimerTimer(Sender: TObject);
begin
  if DimReverse then
  begin
    TextDotMatrix.Dots.Dim := TextDotMatrix.Dots.Dim - 10;
    if TextDotMatrix.Dots.Dim <= 0 then
      DimReverse := False;
  end
  else
  begin
    TextDotMatrix.Dots.Dim := TextDotMatrix.Dots.Dim + 10;
    if TextDotMatrix.Dots.Dim >= 100 then
      DimReverse := True;
  end;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
end;

end.

