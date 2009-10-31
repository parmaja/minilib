unit posCheckBoxes;
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
  posTypes, posUtils, posControls;

type
  TposCheckBox = class(TposFocusFrame)
  private
    FChecked: Boolean;
    FCaption: TCaption;
    procedure SetChecked(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
  protected
    procedure PaintOuter(vCanvas: TCanvas; var Rect: TRect; vColor: TColor); override;
    procedure PaintInner(vCanvas: TCanvas; const Rect: TRect; vColor: TColor); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property Caption: TCaption read FCaption write SetCaption;
  end;

implementation
{ TposCheckBox }

procedure TposCheckBox.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TposCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TposCheckBox.PaintOuter(vCanvas: TCanvas; var Rect: TRect; vColor: TColor);
var
  aRect: TRect;
begin
  inherited;
  if Checked then
  begin
    aRect := Rect;
    if UseRightToLeftAlignment then
    begin
      aRect.Left := aRect.Right - (Rect.Bottom - Rect.Top);
      Rect.Right := aRect.Left;
    end
    else
    begin
      aRect.Right := aRect.Left + (Rect.Bottom - Rect.Top);
      Rect.Left := aRect.Right;
    end;
    vColor := Lighten(Color, -25);
    vCanvas.Brush.Style := bsSolid;
    vCanvas.Brush.Color := vColor;
    vCanvas.FillRect(aRect);
    vCanvas.Pen.Color := Font.Color;
    PaintCheckBox(Canvas, aRect, cbChecked, True);
  end;
end;

procedure TposCheckBox.PaintInner(vCanvas: TCanvas; const Rect: TRect; vColor: TColor);
var
  aCaption: string;
  aRect :TRect;
begin
  inherited;
  vColor := Color;
  if Active then
    vColor := Lighten(vColor, 30)
  else if Down then
    vColor := Lighten(vColor, -15);
  aRect := Rect;
  vCanvas.Font := Self.Font;
  aCaption := Caption;
  PaintButton(vCanvas, aCaption, aRect, vColor, clDefault, States + [pdsBorder] + cRightToLeftStates[UseRightToLeftAlignment]);
end;

procedure TposCheckBox.Click;
begin
  inherited;
  Checked := not Checked;
end;

constructor TposCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks] + [csOpaque];
  Style := Style + [fsOpaque];
  Width := 60;
  Height := 22;
  TabStop := True;
  FChecked := False;
end;

destructor TposCheckBox.Destroy;
begin
  inherited;
end;

initialization
finalization
end.

