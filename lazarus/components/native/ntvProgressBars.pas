unit ntvProgressBars;
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
  Classes, Messages, Controls, ExtCtrls, SysUtils, Contnrs, Graphics, Forms,
  LCLType, LCLIntf, LMessages, LCLProc,
  ntvutils;

type
  TPaintStatus = (psMain, psSub, psAll);
  TPaintStatusSet = set of TPaintStatus;

  { TntvProgressBar }

  TntvProgressBar = class(TCustomControl)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FProgressColor: TColor;
    FStep: integer;
    FShowProgress: boolean;
    FSubStep: integer;
    FSubPosition: integer;
    FSubMax: integer;
    FSubMin: integer;
    FPaintStatus: TPaintStatusSet;
    FSubProgressColor: TColor;
    FSubHeight: integer;
    FFrameColor: TColor;

    procedure SetMax(Value: integer);
    procedure SetMin(Value: integer);
    procedure SetPosition(Value: integer);
    procedure SetProgressColor(const Value: TColor);
    procedure SetShowProgress(Value: boolean);

    procedure SetSubMax(Value: integer);
    procedure SetSubMin(Value: integer);
    procedure SetSubPosition(Value: integer);
    procedure SetSubProgressColor(const Value: TColor);
    procedure SetSubHeight(Value: Integer);
    function GetSubHeight: integer;
    //procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    //procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    //procedure SetBorderStyle(const Value: TItemBorderStyle);
    procedure SetFrameColor(const Value: TColor);
  protected
    procedure DoOnResize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetClientRect: TRect; override;
    function ProgressWidth(APos, AMax, AMin: integer): integer;
    function ProgressStep: integer;
    function MainRect: TRect;
    function SubRect: TRect;
    function RemainRect: TRect;
    procedure SetText(const S:string);
    procedure RedrawBorder(const Clip: HRGN);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure StepIt;
    procedure Reset;
    procedure StepBy(vStep: integer);
  published
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property Step: integer read FStep write FStep default 1;
    property SubMin: integer read FSubMin write SetSubMin default 0;
    property SubMax: integer read FSubMax write SetSubMax default 100;
    property SubHeight: integer read FSubHeight write SetSubHeight default 0;
    property SubPosition: integer read FSubPosition write SetSubPosition default 0;
    property SubStep: integer read FSubStep write FSubStep default 1;
    property ShowProgress: boolean read FShowProgress write SetShowProgress default True;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clNavy;
    property SubProgressColor: TColor read FSubProgressColor write SetSubProgressColor default clRed;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBlack;
    property Align;
    property Anchors;
    property BiDiMode;
    property ParentBiDiMode;
    property Color;
    property Font;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses Types;

{ TntvProgressBar }

constructor TntvProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csOpaque, csDoubleClicks];

  Width := 150;
  Height := 16;

	FMin := 0;
  FMax := 100;
  FPosition := 0;
  FStep := 1;

	FSubMin := 0;
  FSubMax := 100;
  FSubHeight := 0;
  FSubPosition := 0;
  FSubStep := 1;
  //FBorderStyle := ibsLightRaised;
  FShowProgress := True;


  FProgressColor := clNavy;
  FSubProgressColor := clRed;

  Color := clGray;
  Font.Color := clWhite;
  ControlStyle := ControlStyle - [csOpaque];
  FPaintStatus := [];
  //BevelKind := bkSoft;
  FFrameColor := clBlack;
end;

procedure TntvProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
  end;
end;

destructor TntvProgressBar.Destroy;
begin

  inherited;
end;

function TntvProgressBar.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  //InflateRect(Result, -1, -1);
end;

function TntvProgressBar.GetSubHeight: integer;
begin
  if FSubHeight=0 then
    Result := Round(ClientHeight*0.30)
  else
    Result := FSubHeight;
end;

function TntvProgressBar.MainRect: TRect;
begin
  Result := ClientRect;
  Result.Right := ProgressWidth (Position, Max, Min);
end;

procedure TntvProgressBar.Paint;
var
  Tmp: string;
  aRect: TRect;
begin
  with Canvas do
  begin
    if (psSub in FPaintStatus)or(psAll in FPaintStatus) then
    begin
      Brush.Color := SubProgressColor;
      aRect := SubRect;
      FillRect(aRect);
      ExcludeClipRect(Canvas, aRect);
    end;

    if (psMain in FPaintStatus)or(psAll in FPaintStatus) then
    begin
      Brush.Color := FProgressColor;
      FillRect(MainRect);
      Brush.Color := Color;
      FillRect(RemainRect);
      if ShowProgress and (FPosition <> FMin) then
      begin
        Tmp := IntToStr(Round((FPosition - FMin) * 100 / (FMax - FMin))) + ' %';
        Canvas.Font.Assign(Self.Font);
        aRect := ClientRect;
        Brush.Color := Self.Color;
        //DrawString(Canvas, Tmp, aRect, [txtMiddle, txtCenter, txtClear]);
      end;
    end;
    FPaintStatus := [];
  end;
end;

function TntvProgressBar.ProgressStep: integer;
begin
  Result := ClientWidth div 100;
end;

function TntvProgressBar.ProgressWidth(APos, AMax, AMin: Integer): integer;
begin
  Result := MulDiv(ClientWidth, (APos - Min), (AMax - AMin));
end;

procedure TntvProgressBar.RedrawBorder(const Clip: HRGN);
var
  DC: HDC;
  RW: TRect;
  OldDC: HDC;
begin
  {if (BorderStyle <> ibsNone) then
  begin
    DC := GetWindowDC(Handle);
    OldDc := Canvas.Handle;
    Canvas.Handle := DC;
    try
      GetWindowRect(Handle, RW);
      OffsetRect(RW, -RW.Left, -RW.Top);
      DrawItemFrame(Canvas, BorderStyle, RW, FrameColor);
    finally
      ReleaseDC(Handle, dc);
      Canvas.Handle := OldDC;
    end;
  end;}

end;

function TntvProgressBar.RemainRect: TRect;
var
  w, mw, sw: Integer;
begin
  //SubtractRect(Result, ClientRect, MainRect);
  Result := ClientRect;
  mw := ProgressWidth (Position, Max, Min);
  sw := ProgressWidth (Position, Max, Min);
  if mw>sw then
    Inc(Result.Left, mw)
  else
    Inc(Result.Left, sw);


  {Result.Right := ProgressWidth (Position, Max, Min);

  Result := ClientRect;
  Result.Right := ProgressWidth (SubPosition, SubMax, SubMin);
  Result.Top := Result.Bottom-GetSubHeight;}

end;

procedure TntvProgressBar.Reset;
begin
  Position := 0;
end;

{procedure TntvProgressBar.SetBorderStyle(const Value: TItemBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;}

procedure TntvProgressBar.SetFrameColor(const Value: TColor);
begin
	if FFrameColor<>Value then
	begin
		 FFrameColor := Value;
     //Perform(CM_BORDERCHANGED, 0, 0);
	end;
end;

procedure TntvProgressBar.DoOnResize;
begin
  inherited DoOnResize;
	SetPosition(Position);
end;

procedure TntvProgressBar.EraseBackground(DC: HDC);
begin
  FPaintStatus := [psAll];
  inherited EraseBackground(DC);
end;

procedure TntvProgressBar.SetMax(Value: integer);
begin
  if (FMax <> Value) and (Value > FMin) then
  begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetMin(Value: integer);
begin
  if (FMin <> Value) and (Value < FMax) then
	begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetPosition(Value: integer);
var
  aVal: Integer;
  OldWidth, NewWidth: integer;
begin
  if (Value >= Max) then aVal := Max else
    if (Value <= Min) then aVal := Min else
      aVal := Value;
  if FPosition<>aVal then
  begin
    OldWidth := ProgressWidth(FPosition, Max, Min);
    NewWidth := ProgressWidth(aVal, Max, Min);
    FPosition := aVal;
    if OldWidth<>NewWidth then
    begin
      include(FPaintStatus, psMain);
      Invalidate;
      //UpdateWindow(Parent.Handle);
      //Application.ProcessMessages;
      //Refresh;
      //Parent.Refresh;
      //Invalidate;
      //Update;
      //UpdateWindow(Handle);
    end;
  end;
end;

procedure TntvProgressBar.SetProgressColor(const Value: TColor);
begin
  if FProgressColor <> Value then
	begin
    FProgressColor := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetShowProgress(Value: boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetSubHeight(Value: integer);
var
  aVal: Integer;
begin
  if FSubHeight<>Value then
  begin
    if Value<=0 then aVal:=0
    else if Value>ClientHeight then aVal:=ClientHeight
    else aVal := Value;
    FSubHeight := aVal;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetSubMax(Value: integer);
begin
  if (FSubMax <> Value) and (Value > FSubMin) then
  begin
    FSubMax := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetSubMin(Value: integer);
begin
  if (FSubMin <> Value) and (Value < FSubMax) then
	begin
    FSubMin := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetSubPosition(Value: integer);
var
  R: TRect;
  aVal: Integer;
  NeedInvalidate: boolean;
  OldWidth, NewWidth: integer;
begin
  if (Value > SubMax) then aVal := SubMax else
    if (Value < SubMin) then aVal := SubMin else
      aVal := Value;

  if aVal <> FSubPosition then
  begin
    OldWidth := ProgressWidth(FSubPosition, SubMax, SubMin);
    NewWidth := ProgressWidth(aVal, SubMax, SubMin);
    FSubPosition := aVal;

    if OldWidth<>NewWidth then
    begin
      NeedInvalidate :=(aVal=SubMin)or(aVal=SubMax);

      if NeedInvalidate then
      begin
        include(FPaintStatus, psAll);
        Invalidate;
      end
      else
      begin
        include(FPaintStatus, psSub);
        R := SubRect;
        InvalidateRect(Handle, @R, False);
      end;
      UpdateWindow(Handle);
    end;
  end;
 end;

procedure TntvProgressBar.SetSubProgressColor(const Value: TColor);
begin
  if FSubProgressColor<>Value then
  begin
    FSubProgressColor := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetText(const S: string);
begin
  Exception.Create('not implement yet');
end;

procedure TntvProgressBar.StepBy(vStep: integer);
begin
  Position := Position + vStep;
end;

procedure TntvProgressBar.StepIt;
begin
  Position := Position + FStep;
end;

function TntvProgressBar.SubRect: TRect;
begin
  Result := ClientRect;
  Result.Right := ProgressWidth (SubPosition, SubMax, SubMin);
  Result.Top := Result.Bottom-GetSubHeight;
end;

{procedure TntvProgressBar.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  //if BorderStyle<>ibsNone then
    //InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;}

{procedure TntvProgressBar.WMNCPaint(var Message: TMessage);
begin
  //if BorderStyle<>ibsNone then
    //RedrawBorder(HRGN(Message.WParam));
end;}

end.

