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
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types, posControls;

type
  TposImage = class(TposFrame)
  private
    FPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
    procedure PictureChanged(Sender: TObject); virtual;
    procedure PaintInner(vCanvas: TCanvas; const vRect: TRect; vColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture: TPicture read FPicture write SetPicture;
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

procedure TposImage.PaintInner(vCanvas: TCanvas; const vRect: TRect;
  vColor: TColor);
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
  R := Rect(0, 0, FPicture.Width, FPicture.Height);
  CenterRect(R, vRect);
  vCanvas.Draw(R.Left, R.Top, FPicture.Graphic);
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

end.

