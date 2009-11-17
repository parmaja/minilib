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
  {$ifdef fpc}
  LCLTypes,
  {$else}
  Windows,
  {$endif}
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types,
  posTypes, posUtils, posControls;

type
  TposCheckSubFrame = class(TposSubFrame)
  private
    FShape: TposShape;
  published
  public
    function GetRect(var vRect: TRect): TRect; override;
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    property Shape: TposShape read FShape write FShape;
  end;

  TposCheckBox = class(TposWinFrame)
  private
    FChecked: Boolean;
    FText: TCaption;
    FCheckFrame: TposCheckSubFrame;
    FTextMargin: Integer;
    procedure SetChecked(const Value: Boolean);
    procedure SetText(const Value: TCaption);
    procedure SetTextMargin(const Value: Integer);
  protected
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure Click; override;
    procedure Resize; override;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property Text: TCaption read FText write SetText;
    property TextMargin: Integer read FTextMargin write SetTextMargin default 1;
  end;

implementation
{ TposCheckBox }

procedure TposCheckBox.SetText(const Value: TCaption);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate;
  end;
end;

procedure TposCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if FChecked then
      FCheckFrame.Shape := shpCheck
    else
      FCheckFrame.Shape := shpCross;
    Invalidate;
  end;
end;

procedure TposCheckBox.SetTextMargin(const Value: Integer);
begin
  if FTextMargin <> Value then
  begin
    FTextMargin := Value;
    Invalidate;
  end;
end;

procedure TposCheckBox.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
var
  aRect: TRect;
  aStyle: TTextStyle;
begin
  inherited;
  aStyle := GetTextStyle;
  aRect := vRect;
  InflateRect(aRect, -TextMargin, -TextMargin);
  PaintText(Canvas, Text, aRect, aStyle);
end;

procedure TposCheckBox.Resize;
begin
  inherited;
  FCheckFrame.Width := InnerHeight;
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
  FChecked := False;
  FCheckFrame := TposCheckSubFrame.Create(Self);
  FCheckFrame.Place := sbpInner;
  FCheckFrame.Shape := shpCross;
  FTextMargin := 1;
  SubFrames.Add(FCheckFrame);
  Width := 60;
  Height := 22;
  TabStop := True;
  FCheckFrame.Width := InnerHeight;
end;

destructor TposCheckBox.Destroy;
begin
  inherited;
end;

function TposCheckBox.DoKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := inherited DoKeyDown(Key, Shift);
  if Key = VK_SPACE then
    Checked := not Checked;
end;

{ TposCheckSubFrame }

function TposCheckSubFrame.GetRect(var vRect: TRect): TRect;
begin
  Result := vRect;
  if Width > 0 then
  begin
    if Frame.UseRightToLeftAlignment then
    begin
      Result.Left := Result.Right - Width;
      vRect.Right := Result.Left;
    end
    else
    begin
      Result.Right := Result.Left + Width;
      vRect.Left := Result.Right;
    end;
  end;
end;

procedure TposCheckSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
var
  aRect: TRect;
begin
  inherited;
  if Width > 0 then
  begin
    aRect := GetRect(vRect);
    vCanvas.Brush.Color := Frame.Color;
    vCanvas.FillRect(aRect);
    if Frame.UseRightToLeftAlignment then
    begin
      aRect.Right := aRect.Right - 1;
    end
    else
    begin
      aRect.Right := aRect.Right + 1;
    end;
    PaintShape(vCanvas, aRect, Shape, False, True, 0, Frame.Font.Color);
  end;
end;

initialization
finalization
end.

