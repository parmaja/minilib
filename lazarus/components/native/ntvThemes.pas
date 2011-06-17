unit ntvThemes;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Classes, Controls, SysUtils, Contnrs, Graphics,
  LCLType, LCLIntf;

type
  TdrawState = (pdsFocused, pdsSelected, pdsActive, pdsAutoSize, pdsDown, pdsMultiLine, pdsRightToLeft);
  TdrawStates = set of TdrawState;

  TdrawButtonKind = (kndNone, kndNext, kndLeft, kndRight, kndFirst, kndLast, kndUp, kndDown, kndEllipsis,
                     kndPin, kndPlus, kndOK, kndCheck, kndMinus, kndCross, kndStar, kndDiv, kndPoint);

  TdrawCorner = (crnTopLeft, crnTopRight, crnBottomRight, crnBottomLeft);
  TdrawCorners = set of TdrawCorner;
  TdrawSide = (sidTop, sidLeft, sidBottom, sidRight);
  TdrawSides = set of TdrawSide;

  { TntvThemePainter }

  TntvThemePainter = class(TObject)
  private
    FName: string;
    procedure SetName(const AValue: string);
  public
    procedure DrawCorner(Canvas: TCanvas; Rect: TRect; Corner: TdrawCorner); virtual; abstract;
    procedure DrawTab(Canvas: TCanvas; Rect: TRect; Corner: TdrawCorner); virtual; abstract;
    procedure DrawText(Canvas: TCanvas; Text: string; Rect: TRect; Style: TTextStyle); virtual; abstract;
    procedure DrawTextButton(Canvas: TCanvas; Text: string; Rect: TRect; States: TdrawStates); virtual; abstract;
    procedure DrawButtonEdges(Canvas: TCanvas; Rect: TRect; Color, BorderColor: TColor; States: TdrawStates); virtual; abstract;

    procedure DrawButton(Canvas: TCanvas; Rect: TRect; Caption: string; Kind: TdrawButtonKind; Color, BorderColor: TColor; States: TdrawStates); virtual; abstract;
    procedure DrawButton(Canvas: TCanvas; Rect, TextRect: TRect; Caption: string; Kind: TdrawButtonKind; Color, BorderColor: TColor; States: TdrawStates); virtual; abstract;
    procedure DrawRect(Canvas: TCanvas; const Rect: TRect; Color, BorderColor: TColor); virtual; abstract;
    property Name: string read FName write SetName;
  end;

  TntvThemePainters = class(TObjectList)
  end;

  { TntvThemeEngine }

  TntvThemeEngine = class(TObject)
  private
    FPainter: TntvThemePainter;
    FPainters: TntvThemePainters;
    FShowButtonImages: Boolean;
  public
    constructor Create;
    function Switch(NewPainter: TntvThemePainter):Boolean;// use Painter class
    function Switch(NewPainter: string):Boolean; //use Painter name
    property Painter: TntvThemePainter read FPainter;
    property ShowButtonImages: Boolean read FShowButtonImages write FShowButtonImages;
  end;

implementation

uses
  Types, ntvStdThemes;

{ TntvThemePainter }

procedure TntvThemePainter.SetName(const AValue: string);
begin
  if FName =AValue then exit;
  FName :=AValue;
end;

{ TntvThemeEngine }

constructor TntvThemeEngine.Create;
begin
  inherited;
end;

function TntvThemeEngine.Switch(NewPainter: TntvThemePainter): Boolean;
begin
  Result := False;
end;

function TntvThemeEngine.Switch(NewPainter: string): Boolean;
begin
  Result := False;
end;

end.

