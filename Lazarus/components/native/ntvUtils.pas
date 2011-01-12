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

end.

