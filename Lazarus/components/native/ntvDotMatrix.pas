unit ntvDotMatrix;
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
  SysUtils, Classes, Graphics, Controls, Variants, 
  LCLIntf, LCLType, IntfGraphics, FPimage, GraphType,
  Types;

type
  TntvDisplayDotMode = (dmLED, dmLCD);

  TDotMatrixInfo = record
    BackColor: TColor;
    ForeColor: TColor;

    DotSize: Integer;
    DotsSpace: Integer;
    CellWidth: Integer;
    CellHeight: Integer;
    Mode: TntvDisplayDotMode;
    //
    DimColor: TColor;
    LightColor: TColor;
    DimLightColor: TColor;
  end;

  { TntvDisplayDots }

  TntvDisplayDots = class(TPersistent)
  private
    FControl: TControl;
    FRawImage: TLazIntfImage;
    FBitmap: TBitmap;
    FDisplayInfo: TDotMatrixInfo;
    FUpdateCount: Integer;
    FNeedUpdate: Boolean;
    FNeedToLoad: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FRealWidth: Integer;
    FRealHeight: Integer;
    procedure CanvasChanged(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetPixel(x, y: integer): TColor;
    procedure SetCellHeight(const AValue: Integer);
    procedure SetCellWidth(const AValue: Integer);
    procedure SetDotsSpace(const Value: Integer);
    procedure SetDotSize(const Value: Integer);
    function GetInUpdating: Boolean;
    procedure InternalUpdate;
    procedure SetHeight(const AValue: Integer);
    procedure SetPixel(x, y: integer; const AValue: TColor);
    procedure SetWidth(const AValue: Integer);
    procedure UpdateColors;
    procedure UpdateBitmap;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetMode(const Value: TntvDisplayDotMode);
  protected
    procedure Refresh; virtual;
    procedure Update;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; Rect: TRect);
    procedure SetSize(const AWidth, AHeight: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    property InUpdating: Boolean read GetInUpdating;
    property RawImage: TLazIntfImage read FRawImage;
    property Bitmap: TBitmap read FBitmap;
    property Canvas: TCanvas read GetCanvas;
    property Pixels[x, y:integer] : TColor read GetPixel write SetPixel;
    //set Control for auto refresh it when draw to Canvas
    property Control: TControl read FControl write FControl;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  published
    property BackColor: TColor read FDisplayInfo.BackColor write SetBackColor;
    property ForeColor: TColor read FDisplayInfo.ForeColor write SetForeColor;
    property DotSize: Integer read FDisplayInfo.DotSize write SetDotSize default 2;
    property DotsSpace: Integer read FDisplayInfo.DotsSpace write SetDotsSpace default 1;
    //Emulate characher in Display dot matrix
    property CellHeight: Integer read FDisplayInfo.CellHeight write SetCellHeight default 0;
    property CellWidth: Integer read FDisplayInfo.CellWidth write SetCellWidth default 0;
    property Mode: TntvDisplayDotMode read FDisplayInfo.Mode write SetMode default dmLed;
  end;

  { TDotMatrix }

  TDotMatrix = class(TCustomControl)
  private
    FDots: TntvDisplayDots;
  protected
    procedure Paint; override;
    procedure EraseBackground(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Color default clBlack;
    property BidiMode;
    property Dots: TntvDisplayDots read FDots;
    property Font;
    property ParentBidiMode;
    property ParentFont;
  end;

  { TTextDotMatrix }

  TTextDotMatrix = class(TDotMatrix)
  private
    FText: TCaption;
    procedure SetText(const Value: TCaption);
  protected
    procedure Resize; override;
    procedure UpdateText;
  public
    procedure Loaded; override;
  published
    property Text: TCaption read FText write SetText;
  end;

procedure DrawDotMatrix(vCanvas: TCanvas; vRect: TRect; vInfo: TDotMatrixInfo; vDots: TntvDisplayDots);

implementation

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

procedure DrawDot(Canvas: TCanvas; x: integer; y: integer; v: boolean; vInfo: TDotMatrixInfo);
var
  R: TRect;
begin
  R.Left := x;
  R.Top := y;
  R.Right := x + 2;
  R.Bottom := Y + 2;
  if v then
    Canvas.Brush.Color := vInfo.ForeColor
  else
    Canvas.Brush.Color := vInfo.DimColor;
  Canvas.FillRect(R);
  R.Left := x;
  R.Top := y;
  R.Right := x + 1;
  R.Bottom := Y + 1;
  if v then
    Canvas.Brush.Color := vInfo.LightColor
  else
    Canvas.Brush.Color := vInfo.DimLightColor;
  Canvas.FillRect(R);
end;

procedure DrawDotMatrix(vCanvas: TCanvas; vRect: TRect; vInfo: TDotMatrixInfo; vDots: TntvDisplayDots);
var
  y, x, w, h: integer;
  aActive: Boolean;
begin
  y := vRect.Top;
  h := 0;
  while h < vDots.FRawImage.Height do
  begin
    x := vRect.Left;
    w := 0;
    while w < vDots.FRawImage.Width do
    begin
      aActive := vDots.FRawImage.TColors[w, h] <> clBlack;
      DrawDot(vCanvas, x, y, aActive, vInfo);
      x := x + (vInfo.DotSize + vInfo.DotsSpace);
      Inc(w);
    end;
    Inc(h);
    y := y + (vInfo.DotSize + vInfo.DotsSpace);
  end;
end;

{ TDotMatrix }

constructor TDotMatrix.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks] - [csOpaque];
  FDots := TntvDisplayDots.Create;
  FDots.Control := Self;
  Color := clBlack;
end;

destructor TDotMatrix.Destroy;
begin
  FreeAndNil(FDots);
  inherited;
end;

procedure TDotMatrix.Paint;
begin
  inherited;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Dots.Draw(Canvas, ClientRect);
end;

procedure TDotMatrix.EraseBackground(DC: HDC);
begin
end;

procedure TDotMatrix.Resize;
begin
  inherited Resize;
  if not (csLoading in ComponentState) then
  begin
    FDots.SetSize(ClientWidth, ClientHeight);
  end;
end;

procedure TDotMatrix.Loaded;
begin
  inherited Loaded;
  FDots.SetSize(ClientWidth, ClientHeight);
  //FDots.Font.Assgin(Font);
  FDots.Canvas.Font.Assign(Font);
end;

procedure TDotMatrix.BeginUpdate;
begin
  Dots.BeginUpdate;
end;

procedure TDotMatrix.EndUpdate;
begin
  Dots.EndUpdate;
end;

{ TntvDisplayDots }

procedure TntvDisplayDots.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TntvDisplayDots.Draw(Canvas: TCanvas; Rect: TRect);
begin
  if FNeedToLoad then
    FRawImage.LoadFromBitmap(FBitmap.BitmapHandle, FBitmap.MaskHandle);
  FNeedToLoad := False;
  DrawDotMatrix(Canvas, Rect, FDisplayInfo, Self);
end;

procedure TntvDisplayDots.SetSize(const AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  UpdateBitmap;
end;

procedure TntvDisplayDots.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and FNeedUpdate then
      Update;
  end;
end;

procedure TntvDisplayDots.Clear;
begin
  UpdateBitmap;
end;

function TntvDisplayDots.GetInUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TntvDisplayDots.Update;
begin
  if InUpdating then
    FNeedUpdate := True
  else
    InternalUpdate;
end;

procedure TntvDisplayDots.SetDotsSpace(const Value: Integer);
begin
  if FDisplayInfo.DotsSpace <> Value then
  begin
    FDisplayInfo.DotsSpace := Value;
    Update;
  end;
end;

procedure TntvDisplayDots.CanvasChanged(Sender: TObject);
begin
  FNeedToLoad := True;
  Update;
end;

function TntvDisplayDots.GetCanvas: TCanvas;
begin
  Result := Bitmap.Canvas;
end;

function TntvDisplayDots.GetPixel(x, y: integer): TColor;
begin
  Result := RawImage.TColors[x, y];
end;

procedure TntvDisplayDots.SetCellHeight(const AValue: Integer);
begin
  if FDisplayInfo.CellHeight <> AValue then
  begin
    FDisplayInfo.CellHeight := AValue;
    Update;
  end;
end;

procedure TntvDisplayDots.SetCellWidth(const AValue: Integer);
begin
  if FDisplayInfo.CellWidth <> AValue then
  begin
    FDisplayInfo.CellWidth := AValue;
    Update;
  end;
end;

procedure TntvDisplayDots.InternalUpdate;
begin
  Refresh;
end;

procedure TntvDisplayDots.SetHeight(const AValue: Integer);
begin
  FRawImage.Height := AValue;
  UpdateBitmap;
end;

procedure TntvDisplayDots.SetPixel(x, y: integer; const AValue: TColor);
begin
  RawImage.TColors[x, y] := AValue;
  Update;
end;

procedure TntvDisplayDots.SetWidth(const AValue: Integer);
begin
  FRawImage.Width := AValue;
  UpdateBitmap;
end;

constructor TntvDisplayDots.Create;
begin
  inherited;
  //FComponentStyle := FComponentStyle + [csSubComponent];
  FRawImage := TLazIntfImage.Create(100, 100, [riqfMono]);
  FBitmap := TBitmap.Create;
  {$ifndef WINCE}
  FBitmap.Monochrome := True;//maybe not work in WINCE
  {$endif}
  FBitmap.Canvas.OnChange := @CanvasChanged;
  FDisplayInfo.BackColor := clBlack;
  FDisplayInfo.ForeColor := clRed;
  FDisplayInfo.DotSize := 2;
  FDisplayInfo.DotsSpace := 1;
  UpdateBitmap;
  UpdateColors;
  Update;
end;

procedure TntvDisplayDots.SetDotSize(const Value: Integer);
begin
  if FDisplayInfo.DotSize <> Value then
  begin
    FDisplayInfo.DotSize := Value;
    Update;
  end;
end;

destructor TntvDisplayDots.Destroy;
begin
  FreeAndNil(FRawImage);
  inherited;
end;

procedure TntvDisplayDots.SetBackColor(const Value: TColor);
begin
  if FDisplayInfo.BackColor <> Value then
  begin
    FDisplayInfo.BackColor := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.SetForeColor(const Value: TColor);
begin
  if FDisplayInfo.ForeColor <> Value then
  begin
    FDisplayInfo.ForeColor := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.UpdateColors;
begin
  if FDisplayInfo.Mode = dmLED then
  begin
    FDisplayInfo.DimColor := MixColors(FDisplayInfo.ForeColor, FDisplayInfo.BackColor, 90);
    FDisplayInfo.LightColor := Lighten(FDisplayInfo.ForeColor, 120);
  end
  else
  begin
    FDisplayInfo.DimColor := MixColors(FDisplayInfo.ForeColor, FDisplayInfo.BackColor, 30);
    FDisplayInfo.LightColor := FDisplayInfo.ForeColor;
  end;
//  FDisplayInfo.DimLightColor := MixColors(FDisplayInfo.DimColor, clWhite, 200);
  FDisplayInfo.DimLightColor := FDisplayInfo.DimColor;
  Update;
end;

procedure TntvDisplayDots.UpdateBitmap;
begin
  FRealWidth := FWidth div (DotSize + DotsSpace);
  FRealHeight := FHeight div (DotSize + DotsSpace);
  FRawImage.SetSize(FRealWidth, FRealHeight);
  FRawImage.FillPixels(FPColor(0, 0, 0, 0));
  FBitmap.SetSize(FRealWidth, FRealHeight);
  FBitmap.Canvas.Brush.Color := clBlack;
  FBitmap.Canvas.Pen.Color := clWhite;
  FBitmap.Canvas.Font.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TntvDisplayDots.SetMode(const Value: TntvDisplayDotMode);
begin
  if FDisplayInfo.Mode <> Value then
  begin
    FDisplayInfo.Mode := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.Refresh;
begin
  if Control <> nil then
    Control.Refresh;
end;

{ TTextDotMatrix }

procedure TTextDotMatrix.SetText(const Value: TCaption);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    if not (csLoading in ComponentState) then
      UpdateText;
  end;
end;

procedure TTextDotMatrix.Loaded;
begin
  inherited Loaded;
  UpdateText;
end;

procedure TTextDotMatrix.Resize;
begin
  inherited Resize;
  if not (csLoading in ComponentState) then
    UpdateText;
end;

procedure TTextDotMatrix.UpdateText;
var
  aRect: TRect;
//  y: Integer;
begin
  BeginUpdate;
  try
    aRect := Rect(0 ,0, Dots.Width, Dots.Height);
    Dots.Canvas.Font := Font;
    Dots.Canvas.Font.Color := clWhite;
    Dots.Canvas.Pen.Color := clWhite;
    Dots.Canvas.Brush.Color := clBlack;
    Dots.Canvas.FillRect(aRect);
//    y := (Dots.Height - Dots.Canvas.TextHeight(Text)) div 2;
    Dots.Canvas.TextRect(aRect, 0, 0, FText);
  finally
    EndUpdate;
  end;
end;

end.

