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
  Classes, Controls, SysUtils, Contnrs, Graphics, ImgList, GraphType,
  mnClasses,
  LCLType, LCLIntf;

type
  TdrawState = (pdsFocused, pdsSelected, pdsActive, pdsDisabled, pdsDown, pdsMultiLine);
  TdrawStates = set of TdrawState;

  TdrawButtonKind = (kndNone, kndNext, kndLeft, kndRight, kndFirst, kndLast, kndUp, kndDown, kndEllipsis,
                     kndPin, kndPlus, kndOK, kndCheck, kndMinus, kndCross, kndStar, kndDiv, kndPoint);

  TdrawCorner = (crnTopLeft, crnTopRight, crnBottomRight, crnBottomLeft);
  TdrawCorners = set of TdrawCorner;
  TdrawSide = (sidTop, sidLeft, sidBottom, sidRight);
  TdrawSides = set of TdrawSide;

  TntvThemeEngine = class;

  { TntvThemePainter }

  TntvThemePainter = class(TObject)
  private
    FEngine: TntvThemeEngine;
    FLoweredColor: TColor;
    FName: string;
    FRaisedColor: TColor;
    procedure SetName(const AValue: string);
  public
    constructor Create(AEngine: TntvThemeEngine; AName: string); virtual;

    procedure DrawText(Canvas: TCanvas; Text: string; Rect: TRect; Style: TTextStyle; UseRightToLeft: Boolean); virtual;
    procedure DrawText(Canvas: TCanvas; Text: string; Rect: TRect; UseRightToLeft: Boolean = False);
    procedure DrawRect(Canvas: TCanvas; const Rect: TRect; Color, BorderColor: TColor); virtual;
    procedure DrawButtonText(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean); virtual;
    procedure DrawButtonEdge(Canvas: TCanvas; Rect: TRect; States: TdrawStates); virtual;
    procedure DrawButton(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean); virtual;
    procedure DrawImage(Canvas: TCanvas; ImageList:TImageList; ImageIndex: TImageIndex; Rect: TRect; States: TdrawStates); virtual;
    property Name: string read FName write SetName;
  published
    property LoweredColor: TColor read FLoweredColor write FLoweredColor;
    property RaisedColor: TColor read FRaisedColor write FRaisedColor;
  end;

  TntvThemePainters = class(specialize GNamedItems<TntvThemePainter>)
  end;

  { TntvThemeEngine }

  TntvThemeEngine = class(TObject)
  private
    FPainter: TntvThemePainter;
    FPainters: TntvThemePainters;
    FShowButtonImages: Boolean;
  protected
    procedure Switched;
  public
    constructor Create;
    destructor Destroy; override;
    //function Switch(NewPainter: TntvThemePainter):Boolean;// use Painter class
    //function Switch(NewPainter: string):Boolean; //use Painter name
    property Painter: TntvThemePainter read FPainter;
    property ShowButtonImages: Boolean read FShowButtonImages write FShowButtonImages;
  end;

function ntvTheme: TntvThemeEngine;

function DrawStates(Enabled, Down, Focused: Boolean): TdrawStates;
function DrawStates(ADrawStates:TdrawStates; Enabled, Down, Focused: Boolean): TdrawStates;

implementation

uses
  Types, ntvStdThemes;

var
  FntvTheme: TntvThemeEngine = nil;

function ntvTheme: TntvThemeEngine;
begin
  if FntvTheme = nil then
    FntvTheme := TntvThemeEngine.Create;
  Result := FntvTheme;
end;

function DrawStates(Enabled, Down, Focused: Boolean): TdrawStates;
begin
  Result := DrawStates([], Enabled, Down, Focused);
end;

function DrawStates(ADrawStates: TdrawStates; Enabled, Down, Focused: Boolean): TdrawStates;
begin
  Result := ADrawStates;
  if Down then
    Result := Result + [pdsDown];
  if Focused then
    Result := Result + [pdsFocused];
  if not Enabled then
    Result := Result + [pdsDisabled];
end;

{ TntvThemePainter }

procedure TntvThemePainter.SetName(const AValue: string);
begin
  if FName =AValue then exit;
  FName :=AValue;
end;

constructor TntvThemePainter.Create(AEngine: TntvThemeEngine; AName: string);
begin
  inherited Create;
  //RaisedColor := clLtGray;
  //LoweredColor := clDkGray;
  FRaisedColor := cl3DHilight;
  FLoweredColor := cl3DShadow;
  FEngine:= AEngine;
  FName := AName;
end;

procedure TntvThemePainter.DrawText(Canvas: TCanvas; Text: string; Rect: TRect; Style: TTextStyle; UseRightToLeft: Boolean);
begin
  Style.RightToLeft := UseRightToLeft;
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, Text, Style);
end;

procedure TntvThemePainter.DrawText(Canvas: TCanvas; Text: string; Rect: TRect; UseRightToLeft: Boolean);
var
  TS: TTextStyle;
begin
  Finalize(TS);
  with TS do
  begin
    Alignment := BidiFlipAlignment(Alignment, UseRightToLeft);
    WordBreak := False;
    SingleLine:= True;
    Clipping := True;
    ShowPrefix := False;
    SystemFont := False;
    RightToLeft := UseRightToLeft;
    ExpandTabs := True;
  end;
  DrawText(Canvas, Text, Rect, TS, UseRightToLeft);
end;

procedure TntvThemePainter.DrawButtonText(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean);
var
  TS: TTextStyle;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Finalize(TS);
    with TS do
    begin
      if ImageWidth = 0 then
        Alignment := BidiFlipAlignment(taCenter)
      else
        Alignment := BidiFlipAlignment(taLeftJustify, UseRightToLeft);
      WordBreak := False;
      SingleLine:= True;
      Clipping := True;
      ShowPrefix := False;
      SystemFont := False;
      RightToLeft := UseRightToLeft;
      ExpandTabs := True;
    end;

    if pdsDisabled in States then
      Font.Color := clBtnShadow;

    if pdsDown in States then
      OffsetRect(Rect, 1 , 1);
    DrawText(Canvas, Text, Rect, TS, UseRightToLeft);
  end;
end;

procedure TntvThemePainter.DrawButtonEdge(Canvas: TCanvas; Rect: TRect; States: TdrawStates);
begin
  if not (pdsDown in States) then
  begin
    Canvas.Pen.Color := RaisedColor;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right - 1, Rect.Top);
    Canvas.Pen.Color := LoweredColor;
    Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
  end
  else
  begin
    Canvas.Pen.Color := RaisedColor;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right - 1, Rect.Top);
    Canvas.Pen.Color := LoweredColor;
    Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
  end;
end;

procedure TntvThemePainter.DrawButton(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean);
begin
  DrawButtonEdge(Canvas, Rect, States);
  InflateRect(Rect, -1 , -1);
  DrawButtonText(Canvas, Text, ImageWidth, Rect, States, UseRightToLeft);
end;

procedure TntvThemePainter.DrawImage(Canvas: TCanvas; ImageList: TImageList; ImageIndex: TImageIndex; Rect: TRect; States: TdrawStates);
var
  X, Y: integer;
begin
  x := Rect.Left + ((Rect.Right - Rect.Left) div 2) - (ImageList.Width div 2);
  y := Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (ImageList.Height div 2);
  ImageList.Draw(Canvas, x, y, ImageIndex, gdeNormal);
end;

procedure TntvThemePainter.DrawRect(Canvas: TCanvas; const Rect: TRect; Color, BorderColor: TColor);
begin
  Canvas.Pen.Color := BorderColor;
  Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
end;

{ TntvThemeEngine }

procedure TntvThemeEngine.Switched;
begin
end;

constructor TntvThemeEngine.Create;
begin
  inherited;
  FPainters := TntvThemePainters.Create;
  FPainters.Add(TntvThemePainter.Create(Self, 'Standard'));
  FPainter := FPainters[0];
  Switched;
end;

destructor TntvThemeEngine.Destroy;
begin
  FreeAndNil(FPainters);
  inherited Destroy;
end;

end.

