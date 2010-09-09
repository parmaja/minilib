unit posImages;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types,
  posControls, posDraws;

type
  TposImage = class(TposFrame)
  private
    FPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
    procedure PictureChanged(Sender: TObject); virtual;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TposShape = class(TposFrame)
  private
    FShape: TposShapeKind;
    procedure SetShape(const Value: TposShapeKind);
  protected
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Shape: TposShapeKind read FShape write SetShape;
  end;

implementation

uses
  posUtils;

{ TposImage }

constructor TposImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
//  FPicture.OnProgress := Progress;
end;

destructor TposImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TposImage.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
  function DestRect: TRect;
  var
    w, h, cw, ch: Integer;
    xyaspect: Double;
  begin
    w := posEngine.Scale(Picture.Width);
    h := posEngine.Scale(Picture.Height);
    cw := InnerWidth;
    ch := InnerHeight;
    if ((w > cw) or (h > ch)) then
    begin
      if (w > 0) and (h > 0) then
      begin
        xyaspect := w / h;
        if w > h then
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
          if h > ch then  // woops, too big
          begin
            h := ch;
            w := Trunc(ch * xyaspect);
          end;
        end
        else
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
          if w > cw then  // woops, too big
          begin
            w := cw;
            h := Trunc(cw / xyaspect);
          end;
        end;
      end
      else
      begin
        w := cw;
        h := ch;
      end;
    end;

    with Result do
    begin
      Left := 0;
      Top := 0;
      Right := w;
      Bottom := h;
    end;

    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
  end;

var
  R : TRect;
begin
  inherited;
  vCanvas.Brush.Color := vColor;
  if csDesigning in ComponentState then
  begin
    vCanvas.Pen.Color := Font.Color;
    vCanvas.Pen.Style := psDot;
    vCanvas.Pen.Width := 1;
    vCanvas.Rectangle(vRect);
  end
  else
  begin
    vCanvas.FillRect(vRect);
  end;

  R := DestRect;
  vCanvas.StretchDraw(R, FPicture.Graphic);
  ExcludeClipRect(vCanvas, R);
end;

procedure TposImage.PictureChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TposImage.SetPicture(const Value: TPicture);
begin
  if FPicture <> Value then
  begin
    FPicture.Assign(Value);
  end;
end;

{ TposShape }

constructor TposShape.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TposShape.Destroy;
begin

  inherited;
end;

procedure TposShape.PaintInner(vCanvas: TCanvas; var vRect: TRect;
  vColor: TColor);
begin
  inherited;
  vCanvas.Brush.Color := vColor;
  if csDesigning in ComponentState then
  begin
    vCanvas.Pen.Color := Font.Color;
    vCanvas.Pen.Style := psDot;
    vCanvas.Pen.Width := 1;
    vCanvas.Rectangle(vRect);
  end
  else
  begin
    vCanvas.FillRect(vRect);
  end;
  DrawShape(Canvas, vRect, Shape, False, True, UseRightToLeftAlignment,0, Font.Color);
end;

procedure TposShape.SetShape(const Value: TposShapeKind);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

end.

