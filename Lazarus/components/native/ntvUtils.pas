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

procedure ExcludeClipRect(Canvas: TCanvas; Rect: TRect);
function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;

function MapPoint(vFrom, vTo: TControl; vPoint: TPoint): TPoint;
procedure MapPoint(vFrom, vTo: TControl; var X, Y: Integer);

implementation

uses
  Types;

procedure ExcludeClipRect(Canvas: TCanvas; Rect: TRect);
begin
  LCLIntf.ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function MapPoint(vFrom, vTo: TControl; vPoint: TPoint): TPoint;
var
  o1, o2: TPoint;
begin
  if vTo = nil then
    Result := vPoint
  else
  begin
    o1 := vFrom.ClientOrigin;
    o2 := vTo.ClientOrigin;
    Result.X := vPoint.X + (o1.X - o2.X);
    Result.Y := vPoint.Y + (o1.Y - o2.Y);
  end;
end;

procedure MapPoint(vFrom, vTo: TControl; var X, Y: Integer);
var
  p: TPoint;
begin
  p := MapPoint(vFrom, vTo, Point(X,Y));
  X := p.x;
  Y := p.y;
end;

end.

