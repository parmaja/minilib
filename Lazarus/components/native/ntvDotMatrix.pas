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
  Messages, LMessages, SysUtils, Classes, Graphics, Controls, Variants,
  LCLIntf, LCLType, IntfGraphics, FPimage, GraphType,
  Types;

type
  TntvDisplayDotMode = (dmLED, dmLCD);
  TntvDisplayDotTheme = (tdmCustom, tdmClassicLCD, tdmOrange, tdmGreenLED, tdmRedLED, tdmBlueLED);

  TDotMatrixInfo = record
    BackColor: TColor;
    ForeColor: TColor;
    DimColor: TColor;

    DotSize: Integer;
    DotsSpace: Integer;
    CellWidth: Integer;
    CellHeight: Integer;
    CellSpace: Integer;
    Bright: Boolean;
    Mode: TntvDisplayDotMode;
    //
    LightColor: TColor;
    BrightColor: TColor;
  end;

  { TntvDisplayDots }
  TntvDisplayDotsInfo = record
    DimColor: TColor;
    RawImage: TLazIntfImage;
    Bitmap: TBitmap;
    Matrix: TDotMatrixInfo;
    Theme: TntvDisplayDotTheme;
    Width: Integer;
    Height: Integer;
    RealWidth: Integer;
    RealHeight: Integer;
  end;

  TntvDisplayDots = class(TPersistent)
  private
    FInfo: TntvDisplayDotsInfo;
    FUpdateCount: Integer;
    FNeedUpdate: Boolean;
    FNeedToLoad: Boolean;
    FOnRefresh: TNotifyEvent;
    FOnUpdateDisplay: TNotifyEvent;
    procedure CanvasChanged(Sender: TObject);
    procedure DrawDotMatrix(vCanvas: TCanvas; vRect: TRect);
    function GetCanvas: TCanvas;
    procedure SetBright(const AValue: Boolean);
    procedure SetCellSpace(const AValue: Integer);
    procedure SetDimColor(const AValue: TColor);
    function GetPixel(x, y: integer): TColor;
    procedure SetCellHeight(const AValue: Integer);
    procedure SetCellWidth(const AValue: Integer);
    procedure SetDotsSpace(const Value: Integer);
    procedure SetDotSize(const Value: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetTheme(const AValue: TntvDisplayDotTheme);
    procedure SetWidth(const AValue: Integer);
    function GetInUpdating: Boolean;
    procedure InternalUpdate;
    procedure SetPixel(x, y: integer; const AValue: TColor);
    procedure UpdateColors;
    procedure UpdateBitmap;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetMode(const Value: TntvDisplayDotMode);
  protected
    procedure Refresh; virtual;
    procedure UpdateDisplay; virtual;
    procedure Update;
    property RealWidth: Integer read FInfo.RealWidth;
    property RealHeight: Integer read FInfo.RealHeight;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(vCanvas: TCanvas; vRect: TRect);
    procedure SetSize(const AWidth, AHeight: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    procedure Assign(Source: TPersistent); override;
    property InUpdating: Boolean read GetInUpdating;
    property RawImage: TLazIntfImage read FInfo.RawImage;
    property Bitmap: TBitmap read FInfo.Bitmap;
    property Canvas: TCanvas read GetCanvas;
    property Pixels[x, y:integer] : TColor read GetPixel write SetPixel;
    //For auto refresh it when draw to Canvas
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    //Some control need to redraw bitmap after bitmap is clear
    property OnUpdateDisplay: TNotifyEvent read FOnUpdateDisplay write FOnUpdateDisplay;
    property Width: Integer read FInfo.Width write SetWidth;
    property Height: Integer read FInfo.Height write SetHeight;
  published
    property BackColor: TColor read FInfo.Matrix.BackColor write SetBackColor;
    property ForeColor: TColor read FInfo.Matrix.ForeColor write SetForeColor;
    property DimColor: TColor read FInfo.DimColor write SetDimColor;
    property DotSize: Integer read FInfo.Matrix.DotSize write SetDotSize default 2;
    property DotsSpace: Integer read FInfo.Matrix.DotsSpace write SetDotsSpace default 1;
    //Emulate characher in Display dot matrix
    property CellHeight: Integer read FInfo.Matrix.CellHeight write SetCellHeight default 0;
    property CellWidth: Integer read FInfo.Matrix.CellWidth write SetCellWidth default 0;
    property CellSpace: Integer read FInfo.Matrix.CellSpace write SetCellSpace default 0;
    //Add antialiasing to Dot, it slow down the render
    property Bright: Boolean read FInfo.Matrix.Bright write SetBright default False;
    property Mode: TntvDisplayDotMode read FInfo.Matrix.Mode write SetMode default dmLed;
    property Theme: TntvDisplayDotTheme read FInfo.Theme write SetTheme stored false;
  end;

  { TDotMatrix }

  TDotMatrix = class(TCustomControl)
  private
    FDots: TntvDisplayDots;
    procedure SetDots(const AValue: TntvDisplayDots);
  protected
    procedure Paint; override;
    procedure EraseBackground(DC: HDC); override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure UpdateDisplay; virtual;
    procedure DoRefresh(Sender: TObject);
    procedure DoUpdateDisplay(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Anchors;
    property Color default clBlack;
    property BidiMode;
    property Dots: TntvDisplayDots read FDots write SetDots;
    property Font;
    property ParentBidiMode;
    property ParentFont;
  end;

  { TTextDotMatrix }

  TTextDotMatrix = class(TDotMatrix)
  private
    FAlignment: TAlignment;
    FMargin: Integer;
    FText: TCaption;
    FWordWrap: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetMargin(const AValue: Integer);
    procedure SetText(const Value: TCaption);
    procedure SetWordWrap(const AValue: Boolean);
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateDisplay; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Text: TCaption read FText write SetText;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Margin: Integer read FMargin write SetMargin default 0;
  end;

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
  R, T: TRect;
begin
  R.Left := x;
  R.Top := y;
  R.Right := x + vInfo.DotSize;
  R.Bottom := Y + vInfo.DotSize;


  if v then
  begin
    if (vInfo.Bright) then
    begin
      if (vInfo.Mode = dmLED) then
      begin
        T := R;
        Canvas.Brush.Color := vInfo.BrightColor;
        InflateRect(T, vInfo.DotsSpace, 0);
        Canvas.FillRect(T);
        T := R;
        InflateRect(T, 0, vInfo.DotsSpace);
        Canvas.FillRect(T);
      end
      else if (vInfo.Mode = dmLCD) then
      begin
        T := R;
        Canvas.Brush.Color := vInfo.BrightColor;
        OffsetRect(T, vInfo.DotsSpace, vInfo.DotsSpace);
        Canvas.FillRect(T);
      end;
    end;
    Canvas.Brush.Color := vInfo.ForeColor;
  end
  else
    Canvas.Brush.Color := vInfo.DimColor;
  Canvas.FillRect(R);
  R.Left := x;
  R.Top := y;
  R.Right := x + vInfo.DotSize div 2;
  R.Bottom := Y + vInfo.DotSize div 2;
  if v then
    Canvas.Brush.Color := vInfo.LightColor
  else
    Canvas.Brush.Color := vInfo.DimColor;
  Canvas.FillRect(R);
end;

procedure TntvDisplayDots.DrawDotMatrix(vCanvas: TCanvas; vRect: TRect);
var
  y, x, w, h: integer;
  aActive: Boolean;
begin
  vCanvas.Brush.Color := FInfo.Matrix.BackColor;
  vCanvas.FillRect(vRect);
  y := vRect.Top;
  h := 0;
  while h < RawImage.Height do
  begin
    x := vRect.Left;
    w := 0;
    while w < RawImage.Width do
    begin
      aActive := RawImage.TColors[w, h] <> clWhite;
      DrawDot(vCanvas, x, y, aActive, FInfo.Matrix);
      x := x + (FInfo.Matrix.DotSize + FInfo.Matrix.DotsSpace);
      Inc(w);
    end;
    Inc(h);
    y := y + (FInfo.Matrix.DotSize + FInfo.Matrix.DotsSpace);
  end;
end;

{ TDotMatrix }

constructor TDotMatrix.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks] - [csOpaque];
  FDots := TntvDisplayDots.Create;
  FDots.OnRefresh := @DoRefresh;
  FDots.OnUpdateDisplay := @DoUpdateDisplay;
  Color := clBlack;
end;

destructor TDotMatrix.Destroy;
begin
  FreeAndNil(FDots);
  inherited;
end;

procedure TDotMatrix.SetDots(const AValue: TntvDisplayDots);
begin
  FDots.Assign(AValue);
end;

procedure TDotMatrix.Paint;
begin
  inherited;
  Dots.Draw(Canvas, ClientRect);
end;

procedure TDotMatrix.EraseBackground(DC: HDC);
begin
  //To reduce the flicker do not inherite
end;

procedure TDotMatrix.CMBiDiModeChanged(var Message: TLMessage);
begin
  BeginUpdate;
  try
    Inherited;
    UpdateDisplay;
  finally
    EndUpdate;
  end;
end;

procedure TDotMatrix.UpdateDisplay;
begin
end;

procedure TDotMatrix.DoRefresh(Sender: TObject);
begin
  Refresh;
end;

procedure TDotMatrix.DoUpdateDisplay(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TDotMatrix.Resize;
begin
  inherited Resize;
  if not (csLoading in ComponentState) then
  begin
    FDots.SetSize(ClientWidth, ClientHeight);
    UpdateDisplay;
  end;
end;

procedure TDotMatrix.Loaded;
begin
  inherited Loaded;
  FDots.Canvas.Font.Assign(Font);
  FDots.SetSize(ClientWidth, ClientHeight);
  UpdateDisplay;
  //FDots.Font.Assgin(Font);
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

procedure TntvDisplayDots.Draw(vCanvas: TCanvas; vRect: TRect);
begin
  if FNeedToLoad then
    RawImage.LoadFromBitmap(Bitmap.BitmapHandle, Bitmap.MaskHandle);
  FNeedToLoad := False;
  DrawDotMatrix(vCanvas, vRect);
end;

procedure TntvDisplayDots.SetSize(const AWidth, AHeight: Integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    FInfo.Width := AWidth;
    FInfo.Height := AHeight;
    UpdateBitmap;
    Update;
  end;
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
  Update;
end;

procedure TntvDisplayDots.Assign(Source: TPersistent);
begin
  if Source is TntvDisplayDots then
  begin
    FInfo := (Source as TntvDisplayDots).FInfo;
  end
  else
    inherited;
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
  if FInfo.Matrix.DotsSpace <> Value then
  begin
    FInfo.Matrix.DotsSpace := Value;
    UpdateBitmap;
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

procedure TntvDisplayDots.SetBright(const AValue: Boolean);
begin
  if FInfo.Matrix.Bright <> AValue then
  begin
    FInfo.Matrix.Bright := AValue;
    UpdateColors;
    Update;
  end;
end;

procedure TntvDisplayDots.SetCellSpace(const AValue: Integer);
begin
  if FInfo.Matrix.CellSpace <> AValue then
  begin
    FInfo.Matrix.CellSpace := AValue;
    UpdateBitmap;
    Update;
  end;
end;

function TntvDisplayDots.GetPixel(x, y: integer): TColor;
begin
  Result := RawImage.TColors[x, y];
end;

procedure TntvDisplayDots.SetCellHeight(const AValue: Integer);
begin
  if FInfo.Matrix.CellHeight <> AValue then
  begin
    FInfo.Matrix.CellHeight := AValue;
    UpdateBitmap;
    Update;
  end;
end;

procedure TntvDisplayDots.SetCellWidth(const AValue: Integer);
begin
  if FInfo.Matrix.CellWidth <> AValue then
  begin
    Finfo.Matrix.CellWidth := AValue;
    UpdateBitmap;
    Update;
  end;
end;

procedure TntvDisplayDots.SetDimColor(const AValue: TColor);
begin
  if FInfo.DimColor <> AValue then
  begin
    FInfo.DimColor := AValue;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.InternalUpdate;
begin
  Refresh;
end;

procedure TntvDisplayDots.SetHeight(const AValue: Integer);
begin
  if Finfo.RawImage.Height <> AValue then
  begin
    Finfo.RawImage.Height := AValue;
    UpdateBitmap;
    Update;
  end;
end;

procedure TntvDisplayDots.SetTheme(const AValue: TntvDisplayDotTheme);
begin
  if Finfo.Theme = AValue then exit;
  Finfo.Theme := AValue;
  case Finfo.Theme of
    tdmClassicLCD:
    begin
      Mode := dmLCD;
      BackColor := $00C8DCDC;
      DimColor := clDefault;
      ForeColor := clBlack;
      Update;
    end;
    tdmOrange:
    begin
      Mode := dmLED;
      BackColor := clBlack;
      DimColor := clDefault;
      ForeColor := $0000C9FB;
      Update;
    end;
    tdmGreenLED:
    begin
      Mode := dmLED;
      BackColor := clBlack;
      DimColor := clDefault;
      ForeColor := $0032C850;
      Update;
    end;
    tdmRedLED:
    begin
      Mode := dmLED;
      BackColor := clBlack;
      DimColor := clDefault;
      ForeColor := $000000FF;
      Update;
    end;
    tdmBlueLED:
    begin
      Mode := dmLED;
      BackColor := clBlack;
      DimColor := clDefault;
      ForeColor := $00FF7850;
      Update;
    end;
  end;
end;

procedure TntvDisplayDots.SetPixel(x, y: integer; const AValue: TColor);
begin
  RawImage.TColors[x, y] := AValue;
  Update;
end;

procedure TntvDisplayDots.SetWidth(const AValue: Integer);
begin
  if FInfo.RawImage.Width <> AValue then
  begin
    FInfo.RawImage.Width := AValue;
    UpdateBitmap;
    Update;
  end;
end;

constructor TntvDisplayDots.Create;
begin
  inherited;
  FInfo.RawImage := TLazIntfImage.Create(100, 100, [riqfMono]);
  FInfo.Bitmap := TBitmap.Create;
  {$ifndef WINCE}
  FInfo.Bitmap.Monochrome := True;//maybe not work in WINCE
  {$endif}
  FInfo.Bitmap.Canvas.OnChange := @CanvasChanged;
  FInfo.DimColor := clDefault;
  FInfo.Matrix.BackColor := clBlack;
  FInfo.Matrix.ForeColor := clRed;
  FInfo.Matrix.DimColor := clDefault;
  FInfo.Matrix.LightColor := clDefault;
  FInfo.Matrix.DotSize := 2;
  FInfo.Matrix.DotsSpace := 1;
  UpdateColors;
  Clear;
  Update;
end;

procedure TntvDisplayDots.SetDotSize(const Value: Integer);
begin
  if FInfo.Matrix.DotSize <> Value then
  begin
    FInfo.Matrix.DotSize := Value;
    UpdateBitmap;
    Update;
  end;
end;

destructor TntvDisplayDots.Destroy;
begin
  FreeAndNil(FInfo.RawImage);
  inherited;
end;

procedure TntvDisplayDots.SetBackColor(const Value: TColor);
begin
  if FInfo.Matrix.BackColor <> Value then
  begin
    FInfo.Matrix.BackColor := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.SetForeColor(const Value: TColor);
begin
  if FInfo.Matrix.ForeColor <> Value then
  begin
    FInfo.Matrix.ForeColor := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.UpdateColors;
begin
  FInfo.Matrix.DimColor := FInfo.DimColor;
  if FInfo.Matrix.Mode = dmLED then
  begin
    if FInfo.Matrix.DimColor = clDefault then
      FInfo.Matrix.DimColor := MixColors(FInfo.Matrix.ForeColor, FInfo.Matrix.BackColor, 90);
    FInfo.Matrix.LightColor := Lighten(FInfo.Matrix.ForeColor, 120);
    FInfo.Matrix.BrightColor := MixColors(FInfo.Matrix.ForeColor, FInfo.Matrix.BackColor, 150);
  end
  else //dmLCD
  begin
    if FInfo.Matrix.DimColor = clDefault then
      FInfo.Matrix.DimColor := MixColors(FInfo.Matrix.ForeColor, FInfo.Matrix.BackColor, 15);
    FInfo.Matrix.LightColor := FInfo.Matrix.ForeColor;
    FInfo.Matrix.BrightColor := MixColors(FInfo.Matrix.ForeColor, FInfo.Matrix.BackColor, 50);
  end;
  Update;
end;

procedure TntvDisplayDots.UpdateBitmap;
begin
  FInfo.RealWidth := Width div (DotSize + DotsSpace);
  FInfo.RealHeight := Height div (DotSize + DotsSpace);
  FInfo.RawImage.SetSize(FInfo.RealWidth, FInfo.RealHeight);
  FInfo.RawImage.FillPixels(FPColor(0, 0, 0, 0));
  FInfo.Bitmap.SetSize(FInfo.RealWidth, FInfo.RealHeight);
  FInfo.Bitmap.Canvas.Brush.Color := clBlack;
  FInfo.Bitmap.Canvas.Pen.Color := clWhite;
  FInfo.Bitmap.Canvas.Font.Color := clWhite;
  FInfo.Bitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  UpdateDisplay;
end;

procedure TntvDisplayDots.SetMode(const Value: TntvDisplayDotMode);
begin
  if FInfo.Matrix.Mode <> Value then
  begin
    FInfo.Matrix.Mode := Value;
    UpdateColors;
  end;
end;

procedure TntvDisplayDots.Refresh;
begin
  if Assigned(OnRefresh) then
    OnRefresh(Self);
end;

procedure TntvDisplayDots.UpdateDisplay;
begin
  if Assigned(OnUpdateDisplay) then
    OnUpdateDisplay(Self);
end;

{ TTextDotMatrix }

procedure TTextDotMatrix.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    UpdateDisplay;
  end;
end;

procedure TTextDotMatrix.SetMargin(const AValue: Integer);
begin
  if FMargin =AValue then exit;
  FMargin :=AValue;
  UpdateDisplay;
end;

procedure TTextDotMatrix.SetText(const Value: TCaption);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    if not (csLoading in ComponentState) then
      UpdateDisplay;
  end;
end;

procedure TTextDotMatrix.SetWordWrap(const AValue: Boolean);
begin
  if FWordWrap =AValue then exit;
  FWordWrap :=AValue;
  UpdateDisplay;
end;

procedure TTextDotMatrix.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdateDisplay;
end;

procedure TTextDotMatrix.UpdateDisplay;
var
  aRect: TRect;
  TR: TTextStyle;
begin
  BeginUpdate;
  try
    aRect := Rect(0 ,0, Dots.RealWidth, Dots.RealHeight);
    Dots.Canvas.Font := Font;
    Dots.Canvas.Font.Color := clBlack;
    Dots.Canvas.Pen.Color := clBlack;
    Dots.Canvas.Brush.Color := clWhite;
    Dots.Canvas.FillRect(aRect);

    FillChar(TR,SizeOf(TR),0);
    with TR do
    begin
      Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
      WordBreak := WordWrap;
      SingleLine:= not WordWrap;// and not HasMultiLine;
      Clipping := True;
      ShowPrefix := False;
      SystemFont := False;
      RightToLeft := UseRightToLeftReading;
      ExpandTabs := True;
    end;

    InflateRect(aRect, -Margin, -Margin);
    Dots.Canvas.TextRect(aRect, aRect.Left, aRect.Top, FText, TR);
  finally
    EndUpdate;
  end;
end;

constructor TTextDotMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
end;

end.

