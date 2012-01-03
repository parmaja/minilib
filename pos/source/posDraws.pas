unit posDraws;

{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types;

type
  TposShapeKind = (shpNone, shpNext, shpLeft, shpRight, shpFirst, shpLast, shpUp, shpDown, shpEllipsis, shpPin,
    shpPlus, shpOK, shpCheck, shpMinus, shpCross, shpStar, shpDiv, shpPoint);

//Restaurant tables not Database table :)

procedure DrawChair(Canvas: TCanvas; const vRect: TRect; UpDown: Boolean; Opaque: Boolean);
procedure DrawTable(Canvas: TCanvas; const vRect: TRect; Chairs: Integer; Center, Opaque: Boolean);

procedure DrawShape(Canvas: TCanvas; R: TRect; Shape: TposShapeKind; ADown, AEnabled, UseRightToLeft: Boolean; Size: Integer; Color: TColor);

implementation

uses
  mnUtils, posUtils;

procedure DrawShape(Canvas: TCanvas; R: TRect; Shape: TposShapeKind; ADown, AEnabled, UseRightToLeft: Boolean; Size: Integer; Color: TColor);
var
  x, y: Integer;
  w, h: Integer;
  z: Integer;

  procedure CalcZ(DivTo: Integer);
  begin
    if z = 0 then
    begin
      if w > h then
        z := h div DivTo
      else
        z := w div DivTo;
      if z <= 0 then
        z := 1;
    end;
  end;

begin
  w := (R.Right - R.Left);
  h := (R.Bottom - R.Top);
  x := w div 2 + R.Left;
  y := h div 2 + R.Top;
  if ADown then
  begin
    Inc(x);
    Inc(y);
  end;
  z := Size;
  if not AEnabled then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  case Shape of
    shpEllipsis:
    begin
      CalcZ(8);
      Canvas.Rectangle(x, y, x + z * 2, y + z * 2);
      x := x + 3;
      Canvas.Rectangle(x, y, x + z * 2, y + 2);
      x := x + 3;
      Canvas.Rectangle(x, y, x + z * 2, y + z * 2);
    end;
    shpDown, shpLast:
    begin
      CalcZ(8);
      if Shape = shpLast then
      begin
        Canvas.Rectangle(x - z * 2 + 1, y + 2 * z + 1, x + 2 * z, y + z + 1);
        Dec(y, z);
      end;
      Canvas.Polygon([Point(x, y + z), Point(x + 2 * z - 1, y - z + 1), Point(x - 2 * z + 1, y - z + 1)]);
    end;
    shpUp, shpFirst:
    begin
      CalcZ(8);
      if Shape = shpFirst then
      begin
        Canvas.Rectangle(x - z * 2 + 1, y - 2 * z, x + 2 * z, y - z);
        Inc(y, z);
      end;
      Canvas.Polygon([Point(x, y - z), Point(x + 2 * z - 1, y + z - 1), Point(x - 2 * z + 1, y + z - 1)]);
    end;
    shpCross:
    begin
      CalcZ(8);
      if z = 1 then
      begin
        Canvas.MoveTo(x - 2, y - 2);
        Canvas.LineTo(x + 3, y + 3);
        Canvas.MoveTo(x + 2, y - 2);
        Canvas.LineTo(x - 3, y + 3);
      end
      else
      begin
        z := z - 1;
        Canvas.Polygon([Point(x + z * 2, y - z * 3), Point(x + z * 3, y - z * 2), Point(x - z * 2, y + z * 3), Point(x - 3 * z, y + z * 2)]);
        Canvas.Polygon([Point(x - z * 2, y - z * 3), Point(x - z * 3, y - z * 2), Point(x + z * 2, y + z * 3), Point(x + 3 * z, y + z * 2)]);
      end;
    end;
    shpStar:
    begin
      x := (R.Right - R.Left) div 2 + R.Left;
      y := (R.Bottom - R.Top) div 2 + R.Top;
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(x, y - 2);
      Canvas.LineTo(x, y + 3);
      Canvas.MoveTo(x - 2, y - 1);
      Canvas.LineTo(x, y + 1);
      Canvas.MoveTo(x + 2, y - 1);
      Canvas.LineTo(x, y + 1);
      Canvas.MoveTo(x + 2, y + 1);
      Canvas.LineTo(x, y - 1);
      Canvas.MoveTo(x - 2, y + 1);
      Canvas.LineTo(x, y - 1);
    end;
    shpOK, shpCheck:
    begin
      CalcZ(9);
      Canvas.Polygon([Point(x - z * 3, y - z), Point(x - z * 3, y + z), Point(x - z, y + z * 3), Point(x + z * 3, y - z), Point(x + z * 3, y - z * 3), Point(x - z, y + z)]);
    end;
    shpMinus:
    begin
      x := (R.Right - R.Left) div 2 + R.Left;
      y := (R.Bottom - R.Top) div 2 + R.Top;
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(x - 3, y);
      Canvas.LineTo(x + 4, y);
    end;
    shpPlus:
    begin
      x := (R.Right - R.Left) div 2 + R.Left;
      y := (R.Bottom - R.Top) div 2 + R.Top;
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(x - 3, y);
      Canvas.LineTo(x + 4, y);
      Canvas.MoveTo(x, y - 3);
      Canvas.LineTo(x, y + 4);
    end;
    shpDiv:
    begin
      x := (R.Right - R.Left) div 2 + R.Left;
      y := (R.Bottom - R.Top) div 2 + R.Top;
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(x, y - 3);
      Canvas.LineTo(x, y - 1);
      Canvas.MoveTo(x - 3, y);
      Canvas.LineTo(x + 4, y);
      Canvas.MoveTo(x, y + 3);
      Canvas.LineTo(x, y + 1);
    end;
    shpNext:
    begin
      if UseRightToLeft then
      begin
        CalcZ(9);
        Canvas.Polygon([Point(x - z * 2, y), Point(x + z, y - z * 3), Point(x + z, y + z * 3)]);
      end
      else
      begin
        CalcZ(9);
        Canvas.Polygon([Point(x + 2, y), Point(x - 1, y - 3), Point(x - 1, y + 3)]);
      end;
    end;
    shpLeft:
    begin
      CalcZ(9);
      Canvas.Polygon([Point(x - z * 2, y), Point(x + z, y - z * 3), Point(x + z, y + z * 3)]);
    end;
    shpRight:
    begin
      CalcZ(9);
      Canvas.Polygon([Point(x + z * 2, y), Point(x - z, y - z * 3), Point(x - z, y + z * 3)]);
    end;
    shpPoint:
    begin
      CalcZ(6);
      Canvas.Ellipse(x - z * 2, y - z * 2, x + z * 3, y + z * 3);
    end;
    shpPin:
    begin
      begin
        CalcZ(8);
        Canvas.Pen.Color := Color;
        Canvas.MoveTo(x - z * 2, y - z * 2);
        Canvas.LineTo(x + z * 3, y + z * 3);
        Canvas.MoveTo(x - z * 1, y - z * 1);
        Canvas.LineTo(x + z * 1, y - z * 3);
        Canvas.LineTo(x + z * 3, y - z * 1);
        Canvas.LineTo(x + z * 1, y + z * 1);
        Canvas.MoveTo(x - z * 3, y + z * 3);
        Canvas.LineTo(x, y);
        Canvas.MoveTo(x - z * 2, y + z * 3);
        Canvas.LineTo(x + z * 1, y);
        Canvas.MoveTo(x - z * 3, y + z * 2);
        Canvas.LineTo(x, y - z * 1);
      end;
    end;
  end;
end;

procedure DrawChair(Canvas: TCanvas; const vRect: TRect; UpDown: Boolean; Opaque: Boolean);
var
  r: TRect;
begin
  if UpDown then
  begin
    r := vRect;
    r.Bottom := r.Top + 4;
    r.Left := r.Left + 3;
    r.Right := r.Right - 3;
    Canvas.Rectangle(r);

    r := vRect;
    r.Bottom := r.Top + 2;
    Canvas.Rectangle(r);

    r := vRect;
    r.Top := r.Top + 2;
    InflateRect(r, -1, -1);
    Canvas.Rectangle(r);
  end
  else
  begin
    r := vRect;
    r.Top := r.Bottom - 4;
    r.Left := r.Left + 3;
    r.Right := r.Right - 3;
    Canvas.Rectangle(r);

    r := vRect;
    r.Top := r.Bottom - 2;
    Canvas.Rectangle(r);

    r := vRect;
    r.Bottom := r.Bottom - 2;
    InflateRect(r, -1, -1);
    Canvas.Rectangle(r);
  end;
end;

procedure DrawTable(Canvas: TCanvas; const vRect: TRect; Chairs: Integer; Center, Opaque: Boolean);
var
  i, x: Integer;
  Parts: Integer;
  r, rTable: TRect;
const
  cSpace = 2;
  cChairHeight = 8;
  cChairWidth = 8;
  cWhale = cSpace + cChairWidth + cSpace;
begin
  Parts := Chairs div 2;
  if (Chairs mod 2) > 0 then
    Parts := Parts + 1;
  rTable.Left := 0;
  rTable.Top := 0;
  rTable.Right := cSpace + Parts * (cSpace + cChairWidth);
  rTable.Bottom := rTable.Top + cWhale;
  if Center then
    CenterRect(rTable, vRect)
  else
    OffsetRect(rTable, vRect.Left, vRect.Top + cWhale);
  x := rTable.Left + cSpace;
  for i := 1 to Chairs do
  begin
    r.Left := x;
    r.Right := r.Left + cChairWidth;
    if Odd(i) then
    begin
      r.Bottom := rTable.Top;
      r.Top := r.Bottom - cChairHeight;
      DrawChair(Canvas, r, True, Opaque);
    end
    else
    begin
      r.Top := rTable.Bottom;
      r.Bottom := r.Top + cChairHeight;
      DrawChair(Canvas, r, False, Opaque);
      x := x + cSpace + cChairWidth;
    end;
  end;
  Canvas.RoundRect(rTable.Left, rTable.Top, rTable.Right, rTable.Bottom, 5, 5);
end;

end.

