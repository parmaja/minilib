unit DotMatrix;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils, Variants, Classes, Graphics, Controls,
  Contnrs, Types;

type
  TDisplayDotActive = (daOff, daDim, daOn);
  TDisplayDotMode = (dmLED, dmLCD);

  TDisplayDot = class(TObject)
  private
    FColor: TColor;
    FActive: TDisplayDotActive;
    procedure SetColor(const Value: TColor);
  public
    property Color: TColor read FColor write SetColor;
    property Active: TDisplayDotActive read FActive write FActive;
  end;

  TDisplayDotList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDisplayDot;
    procedure SetItem(Index: Integer; const Value: TDisplayDot);
  public
    property Items[Index: Integer]: TDisplayDot read GetItem write SetItem; default;
  end;

  TDotMatrixInfo = record
    BackColor: TColor;
    ForeColor: TColor;
    Lines: Integer;
    CharWidth: Integer;
    DotSize: Integer;
    DotsSpace: Integer;
    CharsSpace: Integer;
    CharHeight: Integer;
    CharsPerLine: Integer;
    Mode:TDisplayDotMode;
    //
    DimColor: TColor;
    LightColor: TColor;
    DimLightColor: TColor;
  end;

  TDisplayDots = class(TPersistent)
  private
    FList: TDisplayDotList;
    FDisplayInfo: TDotMatrixInfo;
    FHeight: Integer;
    FWidth: Integer;
    FNeedUpdate: Boolean;
    FUpdateCount: Integer;
    function GetItems(Index: Integer): TDisplayDot;
    procedure SetCharHeight(const Value: Integer);
    procedure SetCharWidth(const Value: Integer);
    procedure SetLines(const Value: Integer);
    procedure SetDotsSpace(const Value: Integer);
    procedure SetCharsPerLine(const Value: Integer);
    procedure SetCharsSpace(const Value: Integer);
    procedure SetDotSize(const Value: Integer);
    function GetInUpdating: Boolean;
    procedure InternalUpdate;
    procedure UpdateColors;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetMode(const Value: TDisplayDotMode);
  protected
    procedure Update;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; Rect: TRect);
    procedure BeginUpdate;
    procedure EndUpdate;
    property InUpdating: Boolean read GetInUpdating;
    property Items[Index: Integer]: TDisplayDot read GetItems; default;
  published
    property BackColor: TColor read FDisplayInfo.BackColor write SetBackColor;
    property ForeColor: TColor read FDisplayInfo.ForeColor write SetForeColor;
    property DotSize: Integer read FDisplayInfo.DotSize write SetDotSize default 2;
    property DotsSpace: Integer read FDisplayInfo.DotsSpace write SetDotsSpace default 1;
    property Mode: TDisplayDotMode read FDisplayInfo.Mode write SetMode default dmLed;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  { TDotMatrix }

  TDotMatrix = class(TCustomControl)
  private
    FText: TCaption;
    FDots: TDisplayDots;
    procedure SetText(const Value: TCaption);
  protected
    procedure Paint; override;
    procedure UpdateFrom(vCanvas: TCanvas; vRect: TRect);
    procedure EraseBackground(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateText;
    procedure Resize; override;
  published
    property Color default clBlack;
    property Dots: TDisplayDots read FDots;
    property Text: TCaption read FText write SetText;
    property Align;
  end;

procedure DrawDotMatrix(vCanvas: TCanvas; vRect: TRect; vInfo: TDotMatrixInfo; vList: TDisplayDotList);

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
var R: TRect;
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
  canvas.FillRect(R);
end;

procedure DrawDotMatrix(vCanvas: TCanvas; vRect: TRect; vInfo: TDotMatrixInfo; vList: TDisplayDotList);
var
  l, y, x, d, w, h, n: integer;
begin
  y := vRect.Top;
  d := 0;
  l := 0;
  while l < vInfo.Lines do
  begin
    x := vRect.Left;
    n := 0;
    while n < vInfo.CharsPerLine do
    begin
      for h := 0 to vInfo.CharHeight - 1 do
      begin
        for w := 0 to vInfo.CharWidth - 1 do
        begin
          DrawDot(vCanvas, x + (w * (vInfo.DotSize + vInfo.DotsSpace)), y + (h * (vInfo.DotSize + vInfo.DotsSpace)), vList[d].FActive = daOn, vInfo);
          d := d + 1;
        end;
      end;
      Inc(n);
      x := x + (vInfo.CharWidth * (vInfo.DotSize + vInfo.DotsSpace));
    end;
    Inc(l);
    y := y + (vInfo.CharHeight * (vInfo.DotSize + vInfo.DotsSpace));
  end;
end;

{ TDotMatrix }

constructor TDotMatrix.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks] - [csOpaque];
  FDots := TDisplayDots.Create;
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
  Dots.Draw(Canvas, ClientRect);
end;

procedure TDotMatrix.SetText(const Value: TCaption);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateText;
    Invalidate;
  end;
end;

procedure TDotMatrix.UpdateFrom(vCanvas: TCanvas; vRect: TRect);
var
  l, y, x, d, w, h, n: integer;
begin
{  x := vRect.Left;
  y := vRect.Top;
  d := 0;
  l := 0;
  while l < Dots.Lines do
  begin
    n := 0;    
    while n < Dots.CharsPerLine do
    begin
      for h := 0 to Dots.CharHeight - 1 do
      begin
        for w := 0 to Dots.CharWidth - 1 do
        begin
          if vCanvas.Pixels[x + w, y + h] = clWhite then
            Dots[d].FActive := daOn
          else
            Dots[d].FActive := daOff;
          d := d + 1;
        end;
      end;
      Inc(n);
      x := x + Dots.CharWidth;
    end;
    Inc(l);
    y := y + Dots.CharHeight;
  end;}
end;

procedure TDotMatrix.EraseBackground(DC: HDC);
begin
end;

procedure TDotMatrix.UpdateText;
var
  aBitmap: TBitmap;
  aRect: TRect;
  y: Integer;
begin
  aBitmap := TBitmap.Create;
  try
    aBitmap.Width := Dots.Width;
    aBitmap.Height := Dots.Height;
    aRect := Rect(0 ,0, aBitmap.Width, aBitmap.Height);
    aBitmap.Canvas.Brush.Color := clBlack;
    aBitmap.Canvas.FillRect(aRect);
    aBitmap.Canvas.Font := Font;
    aBitmap.Canvas.Font.Color := clWhite;
    y := (aBitmap.Height - aBitmap.Canvas.TextHeight(Text)) div 2;
    aBitmap.Canvas.TextRect(aRect, 0, y, Text);
{    aBitmap.Canvas.Pen.Color := clWhite;
    aBitmap.Canvas.MoveTo(0, 0);
    aBitmap.Canvas.LineTo(10, 10);}
    UpdateFrom(aBitmap.Canvas, aRect);
    Invalidate;
  finally
    aBitmap.Free;
  end;
end;

procedure TDotMatrix.Resize;
begin
  inherited Resize;
  FDots.Width := ClientWidth div (FDots.DotSize + FDots.DotsSpace);
  FDots.Height := ClientHeight div (FDots.DotSize + FDots.DotsSpace);
end;

{ TDisplayDots }

procedure TDisplayDots.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDisplayDots.Draw(Canvas: TCanvas; Rect: TRect);
begin
  DrawDotMatrix(Canvas, Rect, FDisplayInfo, FList);
end;

procedure TDisplayDots.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and FNeedUpdate then
      Update;
  end;
end;

function TDisplayDots.GetItems(Index: Integer): TDisplayDot;
begin
  Result := FList[Index];
end;

function TDisplayDots.GetInUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TDisplayDots.Update;
begin
  if InUpdating then
    FNeedUpdate := True
  else
    InternalUpdate;
end;

procedure TDisplayDots.SetCharHeight(const Value: Integer);
begin
  if FDisplayInfo.CharHeight <> Value then
  begin
    FDisplayInfo.CharHeight := Value;
    Update;
  end;
end;

procedure TDisplayDots.SetCharsPerLine(const Value: Integer);
begin
  if FDisplayInfo.CharsPerLine <> Value then
  begin
    FDisplayInfo.CharsPerLine := Value;
    Update;
  end;
end;

procedure TDisplayDots.SetCharWidth(const Value: Integer);
begin
  if FDisplayInfo.CharWidth <> Value then
  begin
    FDisplayInfo.CharWidth := Value;
    Update;
  end;
end;

procedure TDisplayDots.SetLines(const Value: Integer);
begin
  if FDisplayInfo.Lines <> Value then
  begin
    FDisplayInfo.Lines := Value;
    Update;
  end;
end;

procedure TDisplayDots.SetDotsSpace(const Value: Integer);
begin
  if FDisplayInfo.DotsSpace <> Value then
  begin
    FDisplayInfo.DotsSpace := Value;
    Update;
  end;
end;

procedure TDisplayDots.InternalUpdate;
var
  aCount: Integer;
  i: Integer;
begin
  aCount := Width * Height;
  FList.Count := aCount;
  for i := 0 to aCount -1 do
  begin
    if FList[i] = nil then
      FList[i] := TDisplayDot.Create;
  end;
end;

constructor TDisplayDots.Create;
begin
  inherited;
  FList := TDisplayDotList.Create;
  //FComponentStyle := FComponentStyle + [csSubComponent];
  FDisplayInfo.BackColor := clBlack;
  FDisplayInfo.ForeColor := clRed;
  FDisplayInfo.Lines := 1;
  FDisplayInfo.CharWidth := 8;
  FDisplayInfo.CharHeight := 8;
  FDisplayInfo.DotSize := 2;
  FDisplayInfo.DotsSpace := 1;
  FDisplayInfo.CharsSpace := 1;
  FDisplayInfo.CharsPerLine := 20;
  Update;
  UpdateColors;
end;

procedure TDisplayDots.SetDotSize(const Value: Integer);
begin
  if FDisplayInfo.DotSize <> Value then
  begin
    FDisplayInfo.DotSize := Value;
    Update;
  end;
end;

destructor TDisplayDots.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TDisplayDots.SetCharsSpace(const Value: Integer);
begin
  if FDisplayInfo.CharsSpace <> Value then
  begin
    FDisplayInfo.CharsSpace := Value;
    Update;
  end;
end;

procedure TDisplayDots.SetBackColor(const Value: TColor);
begin
  if FDisplayInfo.BackColor <> Value then
  begin
    FDisplayInfo.BackColor := Value;
    UpdateColors;
  end;
end;

procedure TDisplayDots.SetForeColor(const Value: TColor);
begin
  if FDisplayInfo.ForeColor <> Value then
  begin
    FDisplayInfo.ForeColor := Value;
    UpdateColors;
  end;
end;

procedure TDisplayDots.UpdateColors;
begin
  if FDisplayInfo.Mode = dmLED then
  begin
    FDisplayInfo.DimColor := MixColors(FDisplayInfo.ForeColor, FDisplayInfo.BackColor, 90);
    FDisplayInfo.LightColor := Lighten(FDisplayInfo.ForeColor, 80);
  end
  else
  begin
    FDisplayInfo.DimColor := MixColors(FDisplayInfo.ForeColor, FDisplayInfo.BackColor, 30);
    FDisplayInfo.LightColor := FDisplayInfo.ForeColor;
  end;
//  FDisplayInfo.DimLightColor := MixColors(FDisplayInfo.DimColor, clWhite, 200);
  FDisplayInfo.DimLightColor := FDisplayInfo.DimColor;
end;

procedure TDisplayDots.SetMode(const Value: TDisplayDotMode);
begin
  if FDisplayInfo.Mode <> Value then
  begin
    FDisplayInfo.Mode := Value;
    UpdateColors;
  end;
end;

{ TDisplayDot }

procedure TDisplayDot.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

{ TDisplayDotList }

function TDisplayDotList.GetItem(Index: Integer): TDisplayDot;
begin
  Result := inherited Items[Index] as TDisplayDot;
end;

procedure TDisplayDotList.SetItem(Index: Integer; const Value: TDisplayDot);
begin
  inherited Items[Index] := Value;
end;

end.

