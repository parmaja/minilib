unit posControls;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls,
{$IFDEF FPC}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows,
  Messages,
{$ENDIF}
  Forms, StdCtrls, Contnrs, Types,
  posUtils, posTypes;

const
  sSnapGridSize = 2;
  cMargin = 1;
  cPadding = 0;

type
  TposFrame = class;

  TposFrameStyle = set of (fsBorder, fsOpaque, fsLatedOpaque);
  //fsLatedOpaque take with fsOpaque but the FillRect run after PaintInner
  //When use fsLatedOpaque you must clip all rects you painted
  TposFrameInput = (fiText, fiTab, fiFocus, fiArrow, fiReadOnly);
  TposFrameInputs = set of TposFrameInput;

  TposHelper = class(TObject)
  protected
    function FreeOnRelease:Boolean; virtual;
  public
    UseCount: Integer;
    procedure Click(Sender: TposFrame); virtual;
    function KeyDown(Sender: TposFrame; var Key: Word; Shift: TShiftState): Boolean; virtual;
    function KeyPress(Sender: TposFrame; var Key: Char): Boolean; virtual;
  end;

  { TposFrame }

  TposFrame = class(TGraphicControl)
  private
    FFocused: boolean;
    FOnFocused: TNotifyEvent;
    FStyle: TposFrameStyle;
    FBorderWidth: Integer;
    FStates: TposDrawStates;
    FUpdateCount: Cardinal;
    FInvalidateCount: Integer;
    FMargin: Integer;
    FPadding: Integer;
    FHelper: TposHelper;
    procedure SetDown(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetPending(const Value: Boolean);
    procedure SetActive(const Value: Boolean);
    function GetDown: Boolean;
    function GetFocused: Boolean;
    function GetPending: Boolean;
    function GetActive: Boolean;

    procedure SetMargin(const Value: Integer);
    procedure SetPadding(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
{$IFNDEF FPC} //Emulating Lazarus
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
    function GetInUpdating: Boolean;
    function GetInnerHeight: Integer;
    function GetInnerWidth: Integer;
    procedure SetHelper(const Value: TposHelper);
  protected
    FAutoActive: Boolean;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function KeyPress(var Key: Char): Boolean; virtual;
{$IFNDEF FPC}
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
{$ELSE}
{$ENDIF}
    procedure Resized; virtual;
    procedure HelperChanged; virtual;
    procedure Resize; override;
    function GetInnerRect: TRect; virtual;
    procedure PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    procedure PaintBackground(vCanvas: TCanvas; vRect: TRect; vColor: TColor); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure Snap; virtual;
    procedure InvalidateRect(vRect: TRect); virtual;
    property InnerRect: TRect read GetInnerRect;
    property InnerHeight: Integer read GetInnerHeight;
    property InnerWidth: Integer read GetInnerWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetParentColor: TColor;
    function GetTextStyle: TTextStyle; virtual; //in public to calc the style outside the control
    function GetInputs: TposFrameInputs; virtual;
    property InUpdating: Boolean read GetInUpdating;
    property Style: TposFrameStyle read FStyle write FStyle default [fsBorder];
    property States: TposDrawStates read FStates write FStates;
    property Down: Boolean read GetDown write SetDown;
    property Focused: Boolean read GetFocused write SetFocused;
    property Pending: Boolean read GetPending write SetPending;
    property Active: Boolean read GetActive write SetActive;
    property Helper: TposHelper read FHelper write SetHelper;
  published
    property Align;
    property Anchors;
    property BidiMode;
    property ParentBidiMode;
    property ParentFont;
    property ParentColor;
    property Font;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property AutoActive: Boolean read FAutoActive write FAutoActive default False;
    property Margin: Integer read FMargin write SetMargin default cMargin;
    property Padding: Integer read FPadding write SetPadding default cPadding;
    property OnFocused: TNotifyEvent read FOnFocused write FOnFocused;
  end;

  TposFocusFrame = class(TposFrame)
  private
    FTabOrder: Integer;
    FTabStop: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function DoKeyPress(var Key: Char): Boolean; virtual;
  public
    destructor Destroy; override;
    function GetInputs: TposFrameInputs; override;
    function ReceiveText(S: string): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function KeyPress(var Key: Char): Boolean; override;
    procedure SetFocus; virtual;
  published
    property TabOrder: Integer read FTabOrder write FTabOrder default 0;
    property TabStop: Boolean read FTabStop write FTabStop default True;
  end;

  TposSubFramePlace = (sbpOuter, sbpInner);
  TposSubFrame = class(TObject)
  private
    FFrame: TposFrame;
    FWidth: Integer;
    FHeight: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FPlace: TposSubFramePlace;
  public
    constructor Create(AFrame: TposFrame); virtual;
    function GetRect(var vRect: TRect): TRect; virtual;
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    property Frame: TposFrame read FFrame write FFrame;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property Visible: Boolean read FVisible write FVisible default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Place: TposSubFramePlace read FPlace write FPlace;
  end;

  TposSubFrames = class(TObjectList)
  private
    function GetItem(Index: Integer): TposSubFrame;
    procedure SetItem(Index: Integer; const Value: TposSubFrame);
  published
  public
    procedure Paint(vPlace: TposSubFramePlace; vCanvas: TCanvas; var Rect: TRect; vColor: TColor); virtual;
    property Items[Index: Integer]: TposSubFrame read GetItem write SetItem; default;
  end;

  TposLabelSubFrame = class(TposSubFrame)
  private
    FCaption: TCaption;
  published
  public
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    function GetRect(var vRect: TRect): TRect; override;
    property Caption: TCaption read FCaption write FCaption;
  end;

  TposHaftSubFrame = class(TposSubFrame)
  private
    FShape: TposShape;
  published
  public
    function GetRect(var vRect: TRect): TRect; override;
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    property Shape: TposShape read FShape write FShape;
  end;

  //WinFrame can have internal TposSubFrames
  TposWinFrame = class(TposFocusFrame)
  private
    FSubFrames: TposSubFrames;
  protected
    procedure PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SubFrames: TposSubFrames read FSubFrames;
  end;

  //TposSybariteFrame Have label and button
  TposSybariteFrame = class(TposWinFrame)
  private
    FLabelFrame: TposLabelSubFrame;
    FHaftFrame: TposHaftSubFrame;
    FLabelMode: Boolean;
    procedure SetLabelCaption(const Value: TCaption);
    procedure SetLabelWidth(const Value: Integer);
    function GetLabelCaption: TCaption;
    function GetLabelWidth: Integer;
    procedure SetShowHaft(const Value: Boolean);
    function GetShowHaft: Boolean;
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LabelCaption: TCaption read GetLabelCaption write SetLabelCaption;
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth default 0;
    //LabelMode make it as TLabel not TEdit usefull for translating engine
    property LabelMode: Boolean read FLabelMode write FLabelMode default False;
    property ShowHaft: Boolean read GetShowHaft write SetShowHaft default False;
  end;

  TposCustomFrame = class(TposWinFrame)
  public
  end;

  { TposEngine }

  TposEngine = class(TObject)
  private
    FFocusedFrame: TposFocusFrame;
    FSnapToGrid: Boolean;
    FGridSize: Integer;
    procedure SetFocusedFrame(const Value: TposFocusFrame);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean;
    function KeyPress(var Key: Char): Boolean;
    procedure ProcessKeys(Keys:string);
{$IFDEF FPC}
{$ELSE}
    procedure ShortCut(var Msg: TWMKey; var Handled: Boolean);
{$ENDIF}
    procedure FocusPrior;
    procedure FocusNext;
  //published
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid default True;
    property GridSize: Integer read FGridSize write FGridSize default sSnapGridSize;
    property FocusedFrame: TposFocusFrame read FFocusedFrame write SetFocusedFrame;
  end;

function posEngine: TposEngine;

implementation

{$IFDEF FPC}
{$ELSE}
uses
  MMSystem;
{$ENDIF}

var
  FposEngine: TposEngine = nil;

function posEngine: TposEngine;
begin
  if FposEngine = nil then
    FposEngine := TposEngine.Create;
  Result := FposEngine;
end;

constructor TposEngine.Create;
begin
  inherited;
  FSnapToGrid := True;
  FGridSize := sSnapGridSize;
end;

destructor TposEngine.Destroy;
begin
  inherited;
end;

procedure TposEngine.FocusNext;
var
  aForm: TCustomForm;
  i: Integer;
  aEdit: TposFocusFrame;
  aFirstFrame: TposFocusFrame;
begin
  if FFocusedFrame <> nil then
  begin
    aForm := GetParentForm(FFocusedFrame);
    if aForm <> nil then
    begin
      aEdit := nil;
      aFirstFrame := nil;
      for i := 0 to aForm.ComponentCount - 1 do
      begin
        if aForm.Components[i] is TposFocusFrame then
          if ((aForm.Components[i] as TposFocusFrame) <> FFocusedFrame) and ((aForm.Components[i] as TposFocusFrame).TabStop) and ((aForm.Components[i] as TposFocusFrame).Enabled) and ((aForm.Components[i] as TposFocusFrame).Visible) then
          begin
            if ((TposFocusFrame(aForm.Components[i]).FTabOrder > FFocusedFrame.FTabOrder)) then
            begin
              if (aEdit = nil) or ((TposFocusFrame(aForm.Components[i]).FTabOrder < aEdit.FTabOrder)) then
                aEdit := (aForm.Components[i] as TposFocusFrame);
            end;
            if (aFirstFrame = nil) or ((TposFocusFrame(aForm.Components[i]).FTabOrder < aFirstFrame.FTabOrder)) then
              aFirstFrame := (aForm.Components[i] as TposFocusFrame);
          end;
      end;
      if aEdit = nil then
        FocusedFrame := aFirstFrame
      else
        FocusedFrame := aEdit;
    end;
  end;
end;

procedure TposEngine.FocusPrior;
begin

end;

function TposEngine.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (FocusedFrame <> nil) and FocusedFrame.KeyDown(Key, Shift);
  if not Result then
  begin
    if Shift = [] then
    begin
      case Key of
        9: FocusNext;
      end;
    end
    else if Shift = [ssShift] then
    begin
      case Key of
        9: FocusPrior;
      end;
    end;
  end;
end;

function TposEngine.KeyPress(var Key: Char): Boolean;
var
 aInputs: TposFrameInputs;
begin
  Result := (FocusedFrame <> nil);
  if Result then
  begin
    aInputs := FocusedFrame.GetInputs;
    if (Key = #9) and not (fiTab in aInputs) then
      FocusNext
    else if (fiText in aInputs) and not (fiReadOnly in aInputs) then
      Result := FocusedFrame.KeyPress(Key);
  end;
end;

procedure TposEngine.ProcessKeys(Keys: string);
var
  i: Integer;
begin
  for i := 1 to Length(Keys) do
  begin
    KeyPress(Keys[i]);
  end;
end;

procedure TposEngine.SetFocusedFrame(const Value: TposFocusFrame);
begin
  if FFocusedFrame <> Value then
  begin
    if FFocusedFrame <> nil then
      FFocusedFrame.Focused := False;
    if (Value <> nil) and (Value.Enabled) then
      FFocusedFrame := Value
    else
      FFocusedFrame := nil;
    if FFocusedFrame <> nil then
      FFocusedFrame.Focused := True;
  end;
end;

{ TposFrame }

procedure TposFrame.Paint;
var
  aRect: TRect;
  TmpRect: TRect;
  aColor: TColor;
begin
  inherited;
  if Parent <> nil then
  begin
    if ParentColor then
      aColor := Parent.Brush.Color
    else
      aColor := Color;
  end
  else
    aColor := clBtnFace;

  Canvas.Font := Self.Font;
  Canvas.Brush.Style := bsSolid;
  aRect := ClientRect;
  if (Margin > 0) and (Parent <> nil) then
  begin
    if Margin = 1 then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := GetParentColor;
      TmpRect := aRect;
      PaintRect(Canvas, TmpRect);
    end
    else
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := GetParentColor;
      Canvas.FillRect(Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Top + Margin));
      Canvas.FillRect(Rect(aRect.Right - Margin, aRect.Top, aRect.Right, aRect.Bottom));
      Canvas.FillRect(Rect(aRect.Left, aRect.Bottom - Margin, aRect.Right, aRect.Bottom));
      Canvas.FillRect(Rect(aRect.Left, aRect.Top, aRect.Left + Margin, aRect.Bottom));
    end;
    Canvas.Brush.Style := bsSolid;
  end;
  InflateRect(aRect, -Margin, -Margin);
  PaintOuter(Canvas, aRect, aColor);
  if fsBorder in Style then
  begin
    TmpRect := aRect;
    InflateRect(aRect, -BorderWidth, -BorderWidth);
    if Focused then
      Canvas.Pen.Color := Lighten(aColor, 50)
    else
      Canvas.Pen.Color := Lighten(aColor, 25);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := BorderWidth;
    Canvas.Brush.Style := bsClear;
    PaintRect(Canvas, TmpRect);
    Canvas.Brush.Style := bsSolid;
  end;
  if (fsOpaque in Style) and not (fsLatedOpaque in Style) then
    PaintBackground(Canvas, aRect, aColor);
  InflateRect(aRect, -Padding, -Padding);
  PaintInner(Canvas, aRect, aColor);
  InflateRect(aRect, +Padding, +Padding);
  if (fsLatedOpaque in Style) then
    PaintBackground(Canvas, aRect, aColor);
end;

procedure TposFrame.PaintBackground(vCanvas: TCanvas; vRect: TRect; vColor: TColor);
begin
  if Focused then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Lighten(vColor, 30);
    Canvas.FillRect(vRect);
  end
  else if Active then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Lighten(vColor, 15);
    Canvas.FillRect(vRect);
  end
  else
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := vColor;
    Canvas.FillRect(vRect);
  end;
end;

procedure TposFrame.SetActive(const Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      FStates := FStates + [pdsActive]
    else
      FStates := FStates - [pdsActive];
    Invalidate;
  end;
end;

procedure TposFrame.SetDown(Value: Boolean);
begin
  if Down <> Value then
  begin
    if Value then
      FStates := FStates + [pdsDown]
    else
      FStates := FStates - [pdsDown];
    Invalidate;
  end;
end;

procedure TposFrame.SetFocused(Value: Boolean);
begin
  if Focused <> Value then
  begin
    if Value then
      FStates := FStates + [pdsFocused]
    else
      FStates := FStates - [pdsFocused];
    FFocused := Value;
    Invalidate;
    if FFocused and Assigned(FOnFocused) then
      FOnFocused(Self);
  end;
end;

procedure TposFrame.SetHelper(const Value: TposHelper);
begin
  if FHelper <> Value then
  begin
    if FHelper <> nil then
      Dec(FHelper.UseCount);
    FHelper := Value;
    if FHelper <> nil then
      Inc(FHelper.UseCount);
    HelperChanged;
  end;
end;

procedure TposFrame.SetMargin(const Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    if not (csLoading in ComponentState) then
    begin
      BeginUpdate;
      try
        Resized;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TposFrame.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    if not (csLoading in ComponentState) then
    begin
      BeginUpdate;
      try
        Resized;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TposFrame.SetPending(const Value: Boolean);
begin
  if Pending <> Value then
  begin
    if Value then
      FStates := FStates + [pdsPending]
    else
      FStates := FStates - [pdsPending];
    Invalidate;
  end;
end;


function TposFrame.GetDown: Boolean;
begin
  Result := pdsDown in FStates;
end;

function TposFrame.GetFocused: Boolean;
begin
  Result := pdsFocused in FStates;
end;

function TposFrame.GetParentColor: TColor;
begin
{$IFDEF FPC}
  Result := Parent.Color;
{$ELSE}
  Result := Parent.Brush.Color;
{$ENDIF}
end;

function TposFrame.GetPending: Boolean;
begin
  Result := pdsPending in FStates;
end;

function TposFrame.GetActive: Boolean;
begin
  Result := pdsActive in FStates;
end;

constructor TposFrame.Create(AOwner: TComponent);
begin
  inherited;
  FStyle := [fsBorder];
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
  {$ifdef WINCE}
  //ControlStyle := ControlStyle - [csDoubleClicks];
  {$endif}
  FMargin := cMargin;
  FBorderWidth := 1;
  FAutoActive := False;
end;

destructor TposFrame.Destroy;
begin
  if FposEngine <> nil then
    if FposEngine.FocusedFrame = Self then
      FposEngine.FocusedFrame := nil;
  if (FHelper <> nil) then
  begin
    Dec(FHelper.UseCount);
    if FHelper.FreeOnRelease then
      FreeAndNil(FHelper);
  end;
  inherited;
end;

procedure TposFrame.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
end;

procedure TposFrame.PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
end;

function TposFrame.GetInputs: TposFrameInputs;
begin
  Result := [];
end;

procedure TposFrame.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Refresh;
  end;
end;

procedure TposFrame.Loaded;
begin
  inherited;
end;

procedure TposFrame.ChangeScale(M, D: Integer);
begin
  Snap;
  inherited;
//  BorderWidth := MulDiv(BorderWidth, M, D);
end;

{$IFNDEF FPC}

procedure TposFrame.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
end;

procedure TposFrame.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;

procedure TposFrame.MouseEnter;
begin
end;

procedure TposFrame.MouseLeave;
begin
end;

{$ENDIF}

procedure TposFrame.Snap;
  function SnapNow(x: Integer; Invert: Boolean = False): Integer;
  var
    n: Extended;
  begin
    n := Int(x / posEngine.GridSize) * posEngine.GridSize;
    if n <> x then
    begin
      if Invert then
        Result := Trunc(n) + posEngine.GridSize
      else
        Result := Trunc(n);
    end
    else
      Result := x;
  end;
var
  aRect: TRect;
begin
  inherited;
  if posEngine.SnapToGrid and not (csDesigning in ComponentState) then
  begin
    aRect := BoundsRect;
    with aRect do
    begin
      Left := SnapNow(Left, True);
      Top := SnapNow(Top, True);
      Right := SnapNow(Right);
      Bottom := SnapNow(Bottom);
    end;
    BoundsRect := aRect;
  end;
end;

function TposFrame.GetTextStyle: TTextStyle;
begin
  FillChar(Result, Sizeof(Result), #0);
  Result.SingleLine := True;
  Result.Opaque := False;
  Result.Clipping := False;
  Result.Layout := tlCenter;
  Result.RightToLeft := UseRightToLeftAlignment;
  Result.Alignment := taLeftJustify;
  BidiAlignment(Result);
end;

procedure TposFrame.HelperChanged;
begin

end;

procedure TposFrame.Invalidate;
begin
  if not InUpdating then
    inherited Invalidate
  else
    Inc(FInvalidateCount);
end;

procedure TposFrame.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TposFrame.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and (FInvalidateCount > 0) then
  begin
    FInvalidateCount := 0;
    Invalidate;
  end;
end;

function TposFrame.GetInUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TposFrame.InvalidateRect(vRect: TRect);
begin
  if InUpdating then
    Inc(FInvalidateCount)
  else if Parent <> nil then
  begin
    OffsetRect(vRect, Left, Top);
{$IFDEF FPC}
    LCLIntf.InvalidateRect(Parent.Handle, @vRect, False);
{$ELSE}
    Windows.InvalidateRect(Parent.Handle, @vRect, False);
{$ENDIF}
  end
  else
    Invalidate;
end;

function TposFrame.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  if Helper <> nil then
    Result := Helper.KeyDown(Self, Key, Shift)
  else
    Result := False;
end;

function TposFrame.KeyPress(var Key: Char): Boolean;
begin
  if Helper <> nil then
    Result := Helper.KeyPress(Self, Key)
  else
    Result := False;
end;

function TposFrame.GetInnerRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -Margin, -Margin);
  InflateRect(Result, -Padding, -Padding);
  if fsBorder in Style then
    InflateRect(Result, -BorderWidth, -BorderWidth);
end;

function TposFrame.GetInnerHeight: Integer;
var
  R: TRect;
begin
  R := GetInnerRect;
  Result := R.Bottom - R.Top;
end;

function TposFrame.GetInnerWidth: Integer;
var
  R: TRect;
begin
  R := GetInnerRect;
  Result := R.Right - R.Left;
end;

procedure TposFrame.Resized;
begin
end;

procedure TposFrame.Resize;
begin
  inherited;
  Resized;
end;

{ TposFocusFrame }

destructor TposFocusFrame.Destroy;
begin
  if posEngine.FFocusedFrame = Self then
    posEngine.FFocusedFrame := nil;
  inherited;
end;

function TposFocusFrame.DoKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TposFocusFrame.DoKeyPress(var Key: Char): Boolean;
begin
  Result := False;
end;

function TposFocusFrame.GetInputs: TposFrameInputs;
begin
  Result := inherited GetInputs;
  if TabStop then
    Result := Result + [fiFocus]
end;

function TposFocusFrame.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  inherited KeyDown(Key, Shift);
{  if not (fiTab in GetInputs) and (Key = 9) then
  begin
    posEngine.FocusNext;
    Result := True;
  end
  else}
  if Visible then//it useful when try to disatch KeyDown to more than one controls
    Result := DoKeyDown(Key, Shift)
  else
    Result := False;
end;

function TposFocusFrame.KeyPress(var Key: Char): Boolean;
begin
  inherited KeyPress(Key);
  Result := DoKeyPress(Key);
end;

procedure TposFocusFrame.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fiFocus in GetInputs then
    posEngine.FocusedFrame := Self;
  inherited;
end;

function TposFocusFrame.ReceiveText(S: string): Boolean;
begin
  Result := False;
end;

procedure TposFocusFrame.SetFocus;
begin
  posEngine.FocusedFrame := Self;
end;

{ TposSybariteFrame }

procedure TposSybariteFrame.SetLabelCaption(const Value: TCaption);
begin
  if FLabelFrame.Caption <> Value then
  begin
    FLabelFrame.Caption := Value;
    Invalidate;
  end;
end;

procedure TposSybariteFrame.SetLabelWidth(const Value: Integer);
begin
  if FLabelFrame.Width <> Value then
  begin
    FLabelFrame.Width := Value;
    Invalidate;
  end;
end;

procedure TposSybariteFrame.SetShowHaft(const Value: Boolean);
begin
  if FHaftFrame.Visible <> Value then
  begin
    FHaftFrame.Visible := Value;
    Invalidate;
  end;
end;

procedure TposSybariteFrame.Resize;
begin
  inherited;
  FHaftFrame.Width := InnerHeight;
end;

procedure TposSybariteFrame.ChangeScale(M, D: Integer);
begin
  inherited;
  LabelWidth := MulDiv(LabelWidth, M, D);
end;

constructor TposSybariteFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLabelFrame := TposLabelSubFrame.Create(Self);
  FLabelFrame.Place := sbpInner;
  FHaftFrame := TposHaftSubFrame.Create(Self);
  FHaftFrame.Place := sbpOuter;
  FHaftFrame.Visible := False;
  SubFrames.Add(FLabelFrame);
  SubFrames.Add(FHaftFrame);
  FHaftFrame.Width := InnerHeight;
end;

destructor TposSybariteFrame.Destroy;
begin
  inherited;
end;

function TposSybariteFrame.GetLabelCaption: TCaption;
begin
  Result := FLabelFrame.Caption
end;

function TposSybariteFrame.GetLabelWidth: Integer;
begin
  Result := FLabelFrame.Width;
end;

function TposSybariteFrame.GetShowHaft: Boolean;
begin
  Result := FHaftFrame.Visible;
end;

{$IFDEF FPC}
{$ELSE}

procedure TposEngine.ShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  ShiftState: TShiftState;
  Key: Word;
begin
  ShiftState := KeyDataToShiftState(Msg.KeyData);
  Key := Msg.CharCode;
{  if (Key = VK_F4) and (ShiftState = [ssAlt]) then
    Handled := KeyDown(Key, ShiftState);}
  Handled := KeyDown(Key, ShiftState);
end;
{$ENDIF}

{ TposSubFrame }

constructor TposSubFrame.Create(AFrame: TposFrame);
begin
  inherited Create;
  FFrame := AFrame;
  FVisible := True;
  FEnabled := True;
end;

function TposSubFrame.GetRect(var vRect: TRect): TRect;
begin
end;

procedure TposSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
end;

{ TposSubFrames }

function TposSubFrames.GetItem(Index: Integer): TposSubFrame;
begin
  Result := inherited Items[Index] as TposSubFrame;
end;

procedure TposSubFrames.Paint(vPlace: TposSubFramePlace; vCanvas: TCanvas; var Rect: TRect; vColor: TColor);
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if (vPlace = Items[i].Place) and Items[i].Visible then
      Items[i].Paint(vCanvas, Rect, vColor);
  end;
end;

procedure TposSubFrames.SetItem(Index: Integer; const Value: TposSubFrame);
begin
  inherited Items[Index] := Value;
end;

{ TposLabelSubFrames }

function TposLabelSubFrame.GetRect(var vRect: TRect): TRect;
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

procedure TposLabelSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
var
  aRect: TRect;
  aStyle: TTextStyle;
begin
  inherited;
  if Width > 0 then
  begin
    aRect := GetRect(vRect);
    aStyle := Frame.GetTextStyle;
    aStyle.Alignment := taCenter;
    vColor := Lighten(Frame.Color, -25);
    vCanvas.Brush.Style := bsSolid;
    vCanvas.Brush.Color := vColor;
    vCanvas.FillRect(aRect);
    PaintText(vCanvas, Caption, aRect, aStyle);
  end;
end;

{ TposWinFrame }

constructor TposWinFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSubFrames := TposSubFrames.Create;
end;

destructor TposWinFrame.Destroy;
begin
  FreeAndNil(FSubFrames);
  inherited;
end;

procedure TposWinFrame.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
  FSubFrames.Paint(sbpInner, vCanvas, vRect, vColor);
  inherited;
end;

procedure TposWinFrame.PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
  FSubFrames.Paint(sbpOuter, vCanvas, vRect, vColor);
  inherited;
end;

{ TposHaftSubFrame }

function TposHaftSubFrame.GetRect(var vRect: TRect): TRect;
begin
  Result := vRect;
  if Width > 0 then
  begin
    if Frame.UseRightToLeftAlignment then
    begin
      Result.Right := Result.Left + Width;
      vRect.Left := Result.Right;
    end
    else
    begin
      Result.Left := Result.Right - Width;
      vRect.Right := Result.Left;
    end;
  end;
end;

procedure TposHaftSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
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
    //ParentColor
    PaintBorderButton(vCanvas, aRect, Frame.Color, clDefault, [pdsBorder]);
    PaintShape(vCanvas, aRect, Shape, False, True, 0, Frame.Font.Color);
  end;
end;

{ TposHelper }

procedure TposHelper.Click(Sender: TposFrame);
begin
end;

function TposHelper.FreeOnRelease: Boolean;
begin
  Result := True;
end;

function TposHelper.KeyDown(Sender: TposFrame; var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TposHelper.KeyPress(Sender: TposFrame; var Key: Char): Boolean;
begin
  Result := False;
end;

initialization
finalization
  FreeAndNil(FposEngine);
end.

