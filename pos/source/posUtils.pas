unit posUtils;
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
{$MODE delphi}
{$else}
{$define WINDOWS}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls,
  posTypes, posDraws,
{$ifdef WINDOWS}
  Windows,
{$endif}

{$IFDEF FPC}
  LCLIntf,
  Types;
{$ELSE}
  StdCtrls,
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
{$endif}

procedure AlignRect(var R: TRect; ToRect: TRect; Alignment: TAlignment; Layout: TTextLayout);
function CollideRect(const R1, R2: TRect): Boolean;

procedure ExcludeClipRect(vCanvas: TCanvas; vRect: TRect);

function RGBToColor(R, G, B: Byte): TColor;
function Lighten(Color: TColor; Amount: Integer): TColor;
function MixColors(Color1, Color2: TColor; W1: Integer): TColor;
function BlendColor(Color1, Color2: TColor; W1: Integer): TColor;
function InverseColor(Color: TColor): TColor;
function GrayLevelColor(const Color: TColor): Integer;
function OppositeColor(const Color: TColor): TColor;

{$IFDEF FPC}
{$ELSE}
function InflateRect(var Rect: TRect; dx, dy: Integer): TRect;
function Red(RGBColor:TColor): Byte;
function Green(RGBColor:TColor): Byte;
function Blue(RGBColor:TColor): Byte;
{$ENDIF}
{$ifdef WINDOWS}
function TextStyleToFormat(Style: TTextStyle): Longint;
{$endif}
procedure PaintText(Canvas: TCanvas; Text: string; vRect: TRect; Style: TTextStyle);
procedure PaintTextButton(Canvas: TCanvas; Text: string; Rect: TRect; States: TposDrawStates);
procedure PaintBorderButton(Canvas: TCanvas; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates; Down:Boolean = False);
procedure PaintButton(Canvas: TCanvas; Caption: string; vShape: TposShapeKind; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates); overload;
procedure PaintButton(Canvas: TCanvas; Caption: string; vShape: TposShapeKind; Rect, TextRect: TRect; Color, BorderColor: TColor; States: TposDrawStates); overload;
procedure PaintRect(Canvas: TCanvas; const vRect: TRect);

//
procedure BidiAlignment(var Style:TTextStyle);

implementation

uses
  mnUtils, posControls;

{$IFNDEF FPC}
function InflateRect(var Rect: TRect; dx, dy: Integer): TRect;
begin
  Windows.InflateRect(Rect, dx, dy);
end;
{$ENDIF}

procedure AlignRect(var R: TRect; ToRect: TRect; Alignment: TAlignment; Layout: TTextLayout);
var
  ox, oy: Integer;
begin
  ox := 0;
  oy := 0;
  case Alignment of
    taLeftJustify: ox := ToRect.Left - R.Left;
    taCenter: ox := (ToRect.Left - R.Left + ToRect.Right - R.Right) div 2;
    taRightJustify: ox := ToRect.Right - R.Right
  end;

  case Layout of
    tlTop: oy := ToRect.Top - R.Top;
    tlCenter: oy := (ToRect.Top - R.Top + ToRect.Bottom - R.Bottom) div 2;
    tlBottom: oy := ToRect.Bottom - R.Bottom;
  end;
  OffsetRect(R, ox, oy);
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

function BlendColor(Color1, Color2: TColor; W1: Integer): TColor;
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

function InverseColor(Color: TColor): TColor;
begin
  Color := ColorToRGB(Color);
	Result :=  RGBToColor(255 - Red(Color), 255 - Green(Color), 255 - Blue(Color)) ;
end;

//Taked from http://www.delphigroups.info/2/10/314913.html
function GrayLevelColor(const Color: TColor): Integer;
begin
  Result := (77 * (Color and $FF) + 151 * (Color shr 8 and $FF) + 28 * (Color shr 16 and $FF)) shr 8;
end;

function OppositeColor(const Color: TColor): TColor;
begin
  if GrayLevelColor(Color) < 128 then
    Result := clWhite
  else
    Result := clBlack;
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
function Red(RGBColor:TColor): Byte;
begin
  Result := GetRValue(RGBColor);
end;

function Green(RGBColor:TColor): Byte;
begin
  Result := GetGValue(RGBColor);
end;

function Blue(RGBColor:TColor): Byte;
begin
  Result := GetBValue(RGBColor);
end;
{$endif}

{$ifdef WINDOWS}
function TextStyleToFormat(Style: TTextStyle): Longint;
begin
  Result := 0;
  case Style.Alignment of
    taLeftJustify: Result := DT_LEFT;
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
  procedure DrawText(var R: TRect; Format: Integer);
  var
  {$ifdef FPC}
    s: WideString;
  {$else}
    s: string;
  {$endif}
  begin
    {$ifdef WINDOWS}
      {$ifdef FPC}
      s := UTF8Decode(Text);
      {$else}
      s := Text;
      {$endif}       {$ifdef FPC}
      Windows.DrawTextW(Canvas.Handle, PWideChar(s), Length(s), R, Format);
      {$else}
      Windows.DrawText(Canvas.Handle, PChar(s), Length(s), R, Format);
      {$endif}
    {$else}
      Canvas.TextRect(R, vRect.Left, vRect.Top, Text, Style);
    {$endif}
  end;
{$IFNDEF WINDOWS}
{$ELSE}
var
  R: TRect;
{$ENDIF}
var
  aFormat: Longint;
begin
  if Style.Wordbreak then
    Style.SingleLine := False;
{$IFDEF WINDOWS}
{  if Style.Opaque then
    SetBkMode(Canvas.Handle, Windows.OPAQUE)
  else
  begin}
    SetBKColor(Canvas.Handle, ColorToRGB(Canvas.Brush.Color));
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
//  end;

  aFormat := TextStyleToFormat(Style);
  if Style.Opaque then
    Canvas.FillRect(vRect);
  if Style.Wordbreak then //for now only Wordbreak
  begin
    aFormat := TextStyleToFormat(Style) and not DT_VCENTER and not DT_BOTTOM;
    R := vRect;
    OffsetRect(R, -R.Left, -R.Top);
    DrawText(R, aFormat or DT_CALCRECT);
    AlignRect(R, vRect, Style.Alignment, Style.Layout);
    vRect := R;
  end;
  {$ifdef FPC}
    DrawText(vRect, aFormat);
  {$else}
    DrawText(vRect, aFormat);
  {$endif}
{$ELSE}
  DrawText(vRect, aFormat);
{$ENDIF}
end;

procedure PaintTextButton(Canvas: TCanvas; Text: string; Rect: TRect; States: TposDrawStates);
var
  aStyle: TTextStyle;
begin
  InitMemory(aStyle, SizeOf(aStyle));

  aStyle.Wordbreak := (pdsMultiLine in States);
  aStyle.SingleLine := not aStyle.Wordbreak;
  aStyle.Alignment := taCenter;
  aStyle.Layout := tlCenter;
  aStyle.RightToLeft := pdsRightToLeft in States;
  Rect.Right := Rect.Right - 1;
  Rect.Bottom := Rect.Bottom - 1;
  if pdsDown in States then
    OffsetRect(Rect, posEngine.Scale(1), posEngine.Scale(1));
  PaintText(Canvas, Text, Rect, aStyle);
end;

procedure PaintBorderButton(Canvas: TCanvas; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates; Down:Boolean);
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;

  if BorderColor = clDefault then
    BorderColor := Lighten(Color, 30);

  if Down or (pdsDown in States) then
    Canvas.Pen.Color := Lighten(Color, 20)
  else
    Canvas.Pen.Color := BorderColor;

  Canvas.MoveTo(Rect.Right - 1, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 1);

  if Down or (pdsDown in States) then
    Canvas.Pen.Color := Lighten(BorderColor, -75)
  else
    Canvas.Pen.Color := BorderColor;

  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right, Rect.Top);
end;

procedure PaintButton(Canvas: TCanvas; Caption: string; vShape: TposShapeKind; Rect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
begin
  PaintButton(Canvas, Caption, vShape, Rect, Rect, Color, BorderColor, States);
end;

procedure PaintButton(Canvas: TCanvas; Caption: string; vShape: TposShapeKind; Rect, TextRect: TRect; Color, BorderColor: TColor; States: TposDrawStates);
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

  if Caption <> '' then
    PaintTextButton(Canvas, Caption, TextRect, States);

  if pdsBorder in States then
    InflateRect(Rect, -1, -1);

  if vShape <> shpNone then
    DrawShape(Canvas, TextRect, vShape, False, True, (pdsRightToLeft in States), 0, Canvas.Font.Color);
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

procedure ExcludeClipRect(vCanvas: TCanvas; vRect: TRect);
begin
{$IFDEF FPC}
  LCLintf.ExcludeClipRect(vCanvas.Handle, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
{$ELSE}
  Windows.ExcludeClipRect(vCanvas.Handle, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
{$ENDIF}
end;

procedure BidiAlignment(var Style:TTextStyle);
begin
  if Style.RightToLeft then
     if Style.Alignment = taLeftJustify then
        Style.Alignment := taRightJustify
     else if Style.Alignment = taRightJustify then
        Style.Alignment := taLeftJustify;
end;

end.

