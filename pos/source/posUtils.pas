unit posUtils;

{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, posTypes,
{$IFDEF FPC}
  LCLIntf,
  Types;
{$ELSE}
  Windows,
  Messages;
{$ENDIF}

{$IFDEF FPC}
{$ELSE}
type
  TTextStyle = packed record
    Alignment: TAlignment; // TextRect Only: horizontal alignment
    Layout: TTextLayout; // TextRect Only: vertical alignment
    SingleLine: boolean; // If WordBreak is false then process #13, #10 as
                              // standard chars and perform no Line breaking.
    Clipping: boolean; // TextRect Only: Clip Text to passed Rectangle
    ExpandTabs: boolean; // currently ignored
    ShowPrefix: boolean; // TextRect Only: Process first single '&' per
                              //    line as an underscore and draw '&&' as '&'
    Wordbreak: boolean; // TextRect Only: If line of text is too long
                              //    too fit between left and right boundaries
                              //    try to break into multiple lines between
                              //    words
    Opaque: boolean; // TextRect: Fills background with current Brush
                              // TextOut : Fills background with current
                              //            foreground color
    SystemFont: Boolean; // Use the system font instead of Canvas Font
    RightToLeft: Boolean; //For RightToLeft text reading (Text Direction)
  end;

function InflateRect(var Rect: TRect; dx, dy: Integer): TRect;
{$ENDIF}

type
  TposOutline = (sinNone, sinLeft, sinRight, sinFirst, sinLast, sinUp, sinDown, sinEllipsis, sinPin,
    sinClose, sinPlus, sinOK, sinCheck, sinMinus, sinCross, sinStar, sinDiv, sinPoint);

// Center
procedure CenterRect(var R1: TRect; R2: TRect);
function CollideRect(const R1, R2: TRect): Boolean;

procedure ExcludeClipRect(vCanvas: TCanvas; vRect: TRect);

function RGBToColor(R, G, B: Byte): TColor;
function Lighten(Color: TColor; Amount: Integer): TColor;
function MixColors(Color1, Color2: TColor; W1: Integer): TColor;
function Blend(Color1, Color2: TColor; W1: Integer): TColor;

{$IFDEF FPC}
{$ELSE}
function TextStyleToFormat(Style: TTextStyle): Longint;
{$ENDIF}
procedure PaintText(Canvas: TCanvas; Text: string; vRect: TRect; Style: TTextStyle);
procedure PaintTextButton(Canvas: TCanvas; Text: string; Rect: TRect; States: TposDrawStates);
procedure PaintBorderButton(Canvas: TCanvas; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
procedure PaintButton(Canvas: TCanvas; Caption: string; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
procedure PaintOutline(Canvas: TCanvas; R: TRect; Outline: TposOutline; ADown, AEnabled: Boolean; Color: TColor = clBlack);
procedure PaintCheckBox(Canvas: TCanvas; R: TRect; AState: TCheckBoxState; AEnabled: Boolean; Outline: TposOutline = sinCheck);

//Restaurant tables not Database table :)
procedure PaintRect(Canvas: TCanvas; const vRect: TRect);
procedure PaintChair(Canvas: TCanvas; const vRect: TRect; UpDown: Boolean; Opaque: Boolean);
procedure PaintBuffet(Canvas: TCanvas; const vRect: TRect; Chairs: Integer; Center, Opaque: Boolean);

//
procedure BidiAlignment(var Style:TTextStyle);
 
implementation

{$IFNDEF FPC}

function InflateRect(var Rect: TRect; dx, dy: Integer): TRect;
begin
  Windows.InflateRect(Rect, dx, dy);
end;
{$ENDIF}

procedure CenterRect(var R1: TRect; R2: TRect);
begin
  OffsetRect(R1, ((R2.Right - R2.Left) div 2) - ((R1.Right - R1.Left) div 2) + (R2.Left - R1.Left), ((R2.Bottom - R2.Top) div 2) - ((R1.Bottom - R1.Top) div 2) + (R2.Top - R1.Top));
end;

function CollideRect(const R1, R2: TRect): Boolean;
var
  aRect: TRect;
begin
  aRect := R1;
  if R2.Left > R1.Left then
    aRect.Left := R2.Left;
  if R2.Top > R1.Top then
    aRect.Top := R2.Top;
  if R2.Right < R1.Right then
    aRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then
    aRect.Bottom := R2.Bottom;
  Result := not IsRectEmpty(aRect);
end;

function Lighten(Color: TColor; Amount: Integer): TColor;
var
  C: Integer;
  R, G, B: Integer;
begin
  C := ColorToRgb(Color);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then
    R := 0                                 
  else if R > 255 then
    R := 255;
  if G < 0 then
    G := 0
  else if G > 255 then
    G := 255;
  if B < 0 then
    B := 0
  else if B > 255 then
    B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function MixColors(Color1, Color2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
  C1, C2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  C1 := ColorToRgb(Color1);
  C2 := ColorToRgb(Color2);
  Result := Integer(
    ((C1 and $FF00FF) * Cardinal(W1) +
    (C2 and $FF00FF) * W2) and $FF00FF00 +
    ((C1 and $00FF00) * Cardinal(W1) +
    (C2 and $00FF00) * W2) and $00FF0000) shr 8;
end;

function Blend(Color1, Color2: TColor; W1: Integer): TColor;
var
  C1, C2: Cardinal;
  W2, A1, A2, D, F, G: Integer;
begin
  C1 := ColorToRgb(Color1);
  C2 := ColorToRgb(Color2);

  if W1 >= 100 then
    D := 1000
  else
    D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * Cardinal(W2);
  A1 := C1 shr 16 * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * Cardinal(W2);
  A1 := (C1 shr 8 and $FF) * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * Cardinal(W2);
  A1 := (C1 and $FF) * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
{$IFDEF FPC}
  Result := Graphics.RGBToColor(R, G, B);
{$ELSE}
  Result := RGB(R, G, B);
{$ENDIF}
end;

{$IFDEF FPC}
{$ELSE}
function TextStyleToFormat(Style: TTextStyle): Longint;
begin
  Result := 0;
  case Style.Alignment of
    taRightJustify: Result := DT_RIGHT;
    taCenter: Result := DT_CENTER;
  end;
  case Style.Layout of
    tlCenter: Result := Result or DT_VCENTER;
    tlBottom: Result := Result or DT_BOTTOM;
  end;
  if Style.WordBreak then
    Result := Result or DT_WORDBREAK;

  if Style.SingleLine then
    Result := Result or DT_SINGLELINE;

  if not Style.Clipping then
    Result := Result or DT_NOCLIP;

  if not Style.ShowPrefix then
    Result := Result or DT_NOPREFIX;

  if Style.RightToLeft then
    Result := Result or DT_RTLREADING;
end;
{$ENDIF}

procedure PaintText(Canvas: TCanvas; Text: string; vRect: TRect; Style: TTextStyle);
{$IFDEF FPC}
{$ELSE}
var
  aFormat: Longint;
  R: TRect;
{$ENDIF}
begin
{$IFDEF WINCE}
  {$IFDEF DisableWindowsUnicodeSupport}
  Text := AnsiToUtf8(Text);
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  Canvas.TextRect(vRect, vRect.Left, vRect.Top, Text, Style);
{$ELSE}
  aFormat := TextStyleToFormat(Style);
  if Style.Opaque then
    SetBkMode(Canvas.Handle, Opaque)
  else
  begin
    SetBKColor(Canvas.Handle, Canvas.Brush.Color);
    SetBkMode(Canvas.Handle, TRANSPARENT);
  end;
  if Style.WordBreak and (Style.Layout = tlCenter) then
  begin
    R := vRect;
    OffsetRect(R, -R.Left, -R.Top);
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R, aFormat or DT_CALCRECT and not DT_VCENTER);
    CenterRect(R, vRect);
    vRect := R;
  end;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), vRect, aFormat);
{$ENDIF}
end;

procedure PaintTextButton(Canvas: TCanvas; Text: string; Rect: TRect; States: TposDrawStates);
var
  aStyle: TTextStyle;
begin
  FillChar(aStyle, SizeOf(aStyle), #0);

  aStyle.SingleLine := not (pdsMultiLine in States);
  aStyle.Wordbreak := not aStyle.SingleLine;
  aStyle.Alignment := taCenter;
  aStyle.Layout := tlCenter;
  aStyle.RightToLeft := pdsRightToLeft in States;
  Rect.Right := Rect.Right - 1;
  Rect.Bottom := Rect.Bottom - 1;
  if pdsDown in States then
    OffsetRect(Rect, 1, 1);
  PaintText(Canvas, Text, Rect, aStyle);
end;

procedure PaintBorderButton(Canvas: TCanvas; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;

  if BorderColor = clDefault then
    BorderColor := Lighten(Color, 30);

  if pdsDown in States then
    Canvas.Pen.Color := Lighten(Color, 20)
  else
    Canvas.Pen.Color := BorderColor;

  Canvas.MoveTo(Rect.Right - 1, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 1);

  if pdsDown in States then
    Canvas.Pen.Color := Lighten(BorderColor, -75)
  else
    Canvas.Pen.Color := BorderColor;

  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right, Rect.Top);
end;

procedure PaintButton(Canvas: TCanvas; Caption: string; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
const
  cPending = 4;
begin 
  if pdsActive in States then
    Color := Lighten(Color, 40);
  if Color <> clDefault then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;

  if BorderColor = clDefault then
    BorderColor := Lighten(Color, 30);

  if pdsBorder in States then
    PaintBorderButton(Canvas, Rect, Color, BorderColor, States);

  if pdsPending in States then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := Lighten(BorderColor, 50);
    Canvas.MoveTo(Rect.Right - cPending, Rect.Top + cPending);
    Canvas.LineTo(Rect.Right - cPending, Rect.Bottom - cPending);
    Canvas.LineTo(Rect.Left + cPending, Rect.Bottom - cPending);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := Lighten(BorderColor, -100);
    Canvas.LineTo(Rect.Left + cPending, Rect.Top + cPending);
    Canvas.LineTo(Rect.Right - cPending, Rect.Top + cPending);
  end;

{  if pdsBorder in States then
    InflateRect(Rect, 1, 1);}

  if Caption <> '' then
  begin
    PaintTextButton(Canvas, Caption, Rect, States);
  end;
end;

{procedure PaintChair(Canvas:TCanvas;const vRect:TRect; Opaque:Boolean);
var
  aRect:TRect;
  H,W:integer;
begin
  W:=(vRect.Right-vRect.Left)div 11;
  H:= (vRect.Bottom-vRect.Top)div 11;
  aRect:=Rect(vRect.Left+4*W, vRect.Top+H , vRect.Right-4*W,  vRect.Bottom-5*H);
  Canvas.Brush.Color := RGB(204,204,254);
  canvas.Rectangle(arect);


  aRect:=Rect(vRect.Left, vRect.Top , vRect.Right,  vRect.Top+2*H);
  canvas.RoundRect(arect.Left,arect.top,arect.right,aRect.Bottom,10,10);

  aRect:=Rect(vRect.Left, vRect.top+3*H , vRect.Right,  vRect.Bottom);
  canvas.RoundRect(arect.Left,arect.top,arect.right,aRect.Bottom,h,h);
end;}

//more compatiple with Win32 and WinCE, FrameRect or Rectangle diff between thos OSs
procedure PaintRect(Canvas: TCanvas; const vRect: TRect);
begin
  Canvas.MoveTo(vRect.Left, vRect.Top);
  Canvas.LineTo(vRect.Right - 1, vRect.Top);
  Canvas.LineTo(vRect.Right - 1, vRect.Bottom - 1);
  Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
  Canvas.LineTo(vRect.Left, vRect.Top);
end;

procedure PaintChair(Canvas: TCanvas; const vRect: TRect; UpDown: Boolean; Opaque: Boolean);
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
  end
end;

procedure PaintBuffet(Canvas: TCanvas; const vRect: TRect; Chairs: Integer; Center, Opaque: Boolean);
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
      PaintChair(Canvas, r, True, Opaque);
    end
    else
    begin
      r.Top := rTable.Bottom;
      r.Bottom := r.Top + cChairHeight;
      PaintChair(Canvas, r, False, Opaque);
      x := x + cSpace + cChairWidth;
    end;
  end;
  Canvas.RoundRect(rTable.Left, rTable.Top, rTable.Right, rTable.Bottom, 5, 5);
end;

procedure ExcludeClipRect(vCanvas: TCanvas; vRect: TRect);
begin
{$IFDEF FPC}
  LCLintf.ExcludeClipRect(vCanvas.Handle, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
{$ELSE}
  Windows.ExcludeClipRect(vCanvas.Handle, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
{$ENDIF}
end;

procedure PaintOutline(Canvas: TCanvas; R: TRect; Outline: TposOutline; ADown, AEnabled: Boolean; Color: TColor = clBlack);
var
  b, x, y: Integer;
begin
  case Outline of
    sinEllipsis:
      begin
        x := (R.Right - R.Left) div 2 - 4 + R.Left;
        y := (R.Bottom - R.Top) div 2 - 1 + R.Top;
        if ADown then
          Inc(y, 1);
        Canvas.Brush.Color := Color;
        Canvas.Pen.Color := Color;
        Canvas.Rectangle(x, y, x + 2, y + 2);
        x := x + 3;
        Canvas.Rectangle(x, y, x + 2, y + 2);
        x := x + 3;
        Canvas.Rectangle(x, y, x + 2, y + 2);
      end;
    sinDown, sinLast:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top - 1;
        if AEnabled then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Pen.Color := Color;
          if ADown then
          begin
            Inc(x);
            Inc(y);
          end;
          if Outline = sinLast then
          begin
            Canvas.Rectangle(x - 3, y + 6, x + 4, y + 4);
          end;
          Canvas.Polygon([Point(x, y + 2), Point(x + 3, y - 1), Point(x - 3, y - 1)]);
        end
        else
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Color := clWhite;
          if Outline = sinLast then
          begin
            Canvas.Rectangle(x - 3, y + 6, x + 4, y + 4);
          end;
          Inc(x);
          Inc(y);
          Canvas.Polygon([Point(x, y + 2), Point(x + 3, y - 1), Point(x - 3, y - 1)]);

          Canvas.Brush.Color := clGray;
          Canvas.Pen.Color := clGray;
          Dec(x);
          Dec(y);
          Canvas.Polygon([Point(x, y + 2), Point(x + 3, y - 1), Point(x - 3, y - 1)]);
        end;
      end;
    sinUp, sinFirst:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top + 1;
        if AEnabled then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Pen.Color := Color;
          if ADown then
          begin
            Inc(x);
            Inc(y);
          end;
          if Outline = sinFirst then
          begin
            Canvas.Rectangle(x - 3, y - 5, x + 4, y - 3);
          end;
          Canvas.Polygon([Point(x, y - 2), Point(x + 3, y + 1), Point(x - 3, y + 1)]);
        end
        else
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Color := clWhite;
          if Outline = sinFirst then
          begin
            Canvas.Rectangle(x - 3, y - 5, x + 4, y - 3);
          end;
          Inc(x);
          Inc(y);
          Canvas.Polygon([Point(x, y - 2), Point(x + 3, y + 1), Point(x - 3, y + 1)]);
          Dec(x);
          Dec(y);
          Canvas.Brush.Color := clGray;
          Canvas.Pen.Color := clGray;
          Canvas.Polygon([Point(x, y - 2), Point(x + 3, y + 1), Point(x - 3, y + 1)]);
        end;
      end;
    sinCross:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        Canvas.Pen.Color := Color;
        Canvas.MoveTo(x - 3, y - 3);
        Canvas.LineTo(x + 4, y + 4);
        Canvas.MoveTo(x + 3, y - 3);
        Canvas.LineTo(x - 4, y + 4);
      end;
    sinStar:
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
    sinOK, sinCheck:
      begin
        b := ((R.Right - R.Left) - 8) div 2;
        x := R.Left + b;
        b := ((R.Bottom - R.Top) - 8) div 2;
        y := R.Top + b;
        Canvas.Brush.Color := Color;
        if AEnabled then
          Canvas.Pen.Color := Color
        else
          Canvas.Pen.Color := clltGray;
        Canvas.Polygon([Point(x + 1, y + 5), Point(x + 3, y + 7), Point(x + 7, y + 3), Point(x + 7, y + 1), Point(x + 3, y + 5), Point(x + 1, y + 3)]);
      end;
    sinMinus:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        Canvas.Pen.Color := Color;
        Canvas.MoveTo(x - 3, y);
        Canvas.LineTo(x + 4, y);
      end;
    sinPlus:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        Canvas.Pen.Color := Color;
        Canvas.MoveTo(x - 3, y);
        Canvas.LineTo(x + 4, y);
        Canvas.MoveTo(x, y - 3);
        Canvas.LineTo(x, y + 4);
      end;
    sinDiv:
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
    sinLeft:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        if AEnabled then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Pen.Color := Color;
          if ADown then
          begin
            Inc(x);
            Inc(y);
          end;
          Canvas.Polygon([Point(x - 2, y), Point(x + 1, y - 3), Point(x + 1, y + 3)]);
        end
        else
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Color := clWhite;
          Inc(x);
          Inc(y);
          Canvas.Polygon([Point(x - 2, y), Point(x + 1, y - 3), Point(x + 1, y + 3)]);
          Canvas.Brush.Color := clGray;
          Dec(x);
          Dec(y);
          Canvas.Pen.Color := clGray;
          Canvas.Polygon([Point(x - 2, y), Point(x + 1, y - 3), Point(x + 1, y + 3)]);
        end;
      end;
    sinRight:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        if AEnabled then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Pen.Color := Color;
          if ADown then
          begin
            Inc(x);
            Inc(y);
          end;
          Canvas.Polygon([Point(x + 2, y), Point(x - 1, y - 3), Point(x - 1, y + 3)]);
        end
        else
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Color := clWhite;
          Inc(x);
          Inc(y);
          Canvas.Polygon([Point(x + 2, y), Point(x - 1, y - 3), Point(x - 1, y + 3)]);
          Canvas.Brush.Color := clGray;
          Dec(x);
          Dec(y);
          Canvas.Pen.Color := clGray;
          Canvas.Polygon([Point(x + 2, y), Point(x - 1, y - 3), Point(x - 1, y + 3)]);
        end;
      end;
    sinPoint:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        if ADown then
          Inc(y, 1);
        Canvas.Brush.Color := Color;
        Canvas.Pen.Color := Color;
        Canvas.Ellipse(x - 2, y - 2, x + 3, y + 3);
      end;
    sinClose:
      begin
        x := (R.Right - R.Left) div 2 + R.Left;
        y := (R.Bottom - R.Top) div 2 + R.Top;
        Canvas.Pen.Color := Color;
        Canvas.MoveTo(x - 3, y - 3);
        Canvas.LineTo(x + 4, y + 4);
        Canvas.MoveTo(x - 2, y - 3);
        Canvas.LineTo(x + 4, y + 3);
        Canvas.MoveTo(x - 3, y - 2);
        Canvas.LineTo(x + 3, y + 4);
        Canvas.MoveTo(x + 3, y - 3);
        Canvas.LineTo(x - 4, y + 4);
        Canvas.MoveTo(x + 2, y - 3);
        Canvas.LineTo(x - 4, y + 3);
        Canvas.MoveTo(x + 3, y - 2);
        Canvas.LineTo(x - 3, y + 4);
      end;
    sinPin:
      begin
        begin
          x := (R.Right - R.Left) div 2 + R.Left;
          y := (R.Bottom - R.Top) div 2 + R.Top;
          Canvas.Pen.Color := Color;
          Canvas.MoveTo(x - 2, y - 2);
          Canvas.LineTo(x + 3, y + 3);
          Canvas.MoveTo(x - 1, y - 1);
          Canvas.LineTo(x + 1, y - 3);
          Canvas.LineTo(x + 3, y - 1);
          Canvas.LineTo(x + 1, y + 1);
          Canvas.MoveTo(x - 3, y + 3);
          Canvas.LineTo(x, y);
          Canvas.MoveTo(x - 2, y + 3);
          Canvas.LineTo(x + 1, y);
          Canvas.MoveTo(x - 3, y + 2);
          Canvas.LineTo(x, y - 1);
        end;
      end;
  end;
end;

procedure PaintCheckBox(Canvas: TCanvas; R: TRect; AState: TCheckBoxState; AEnabled: Boolean; Outline: TposOutline);
var
  x, y, b: Integer;
  aColor: TColor;
begin
  b := ((R.Right - R.Left) - 8) div 2;
  x := R.Left + b;
  b := ((R.Bottom - R.Top) - 8) div 2;
  y := R.Top + b;
  with Canvas do
  begin
    Pen.Color := clGray;
    Brush.Color := clWhite;
    Rectangle(x - 1, y - 1, x + 10, y + 10);
    if AEnabled then
      Pen.Color := clBlack
    else
      Pen.Color := clltGray;
    if AState <> cbUnchecked then
    begin
      case AState of
        cbChecked:
          begin
            if AEnabled then
              aColor := clBlack
            else
              aColor := clLtGray;
          end;
      else
        aColor := clGray;
      end;
      PaintOutline(Canvas, R, Outline, False, True, aColor);
    end;
  end;
end;

procedure BidiAlignment(var Style:TTextStyle);
begin
  if Style.RightToLeft then
     if Style.Alignment = taLeftJustify then
        Style.Alignment := taRightJustify
     else if Style.Alignment = taRightJustify then
        Style.Alignment := taLeftJustify;
end;

initialization
finalization
end.

