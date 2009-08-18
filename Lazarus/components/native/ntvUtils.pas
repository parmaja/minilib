unit ntvUtils;
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
  Dialogs, Classes, Messages, Controls, ExtCtrls, SysUtils, Math, Contnrs, Graphics,
  LCLType, Forms, LCLIntf;

procedure ExcludeRect (Canvas: TCanvas; Rect: TRect);
function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
procedure ntvMapWindowRect(vFrom, vTo: THandle; var R: TRect);

implementation

uses
  Types;

procedure ntvMapWindowRect(vFrom, vTo: THandle; var R: TRect);
var
  tl, br: TPoint;
begin
  tl := R.TopLeft;
  br := R.BottomRight;
  ClientToScreen(vFrom, tl);
  ClientToScreen(vFrom, br);
  ScreenToClient(vTo, tl);
  ScreenToClient(vTo, br);

  R.TopLeft := tl;
  R.BottomRight := br;
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  if Integer(C1) < 0 then
    C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then
    C2 := GetSysColor(C2 and $000000FF);
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

procedure ExcludeRect (Canvas: TCanvas; Rect: TRect);
begin
  ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

end.

