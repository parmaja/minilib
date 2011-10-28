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

{
M Margin filled with parent color
O Outer SubFrames
B Border filled with border color
P Padding filled with border color
I Inner SubFrames
"White space" + SubFrames: named ContentsRect
"White space": named InnerRect

MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MOOOBBBBBBBBBBBBBBBBBBBBBOOOM
MOOOBPPPPPPPPPPPPPPPPPPPBOOOM
MOOOBPII             IIPBOOOM
MOOOBPII             IIPBOOOM
MOOOBPPPPPPPPPPPPPPPPPPPBOOOM
MOOOBBBBBBBBBBBBBBBBBBBBBOOOM
MMMMMMMMMMMMMMMMMMMMMMMMMMMMM

SubFrame must fill its background
}

interface

uses
  SysUtils, Classes, Graphics, Controls,
{$IFDEF FPC}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows,
  StdCtrls,
  Messages,
{$ENDIF}
  Forms, Contnrs, Types,
  posDraws, posUtils, posTypes;

const
  sSnapGridSize = 2;
  cMargin = 1;
  cPadding = 0;

type
  TposFrame = class;

  TposFrameStyle = set of (fsMouse, fsBorder, fsOpaque, fsLatedOpaque);
  //fsLatedOpaque take with fsOpaque but the FillRect run after PaintInner
  //When use fsLatedOpaque you must clip all rects you painted
  TposFrameInput = (fiText, fiTab, fiFocus, fiArrow, fiKeys, fiReadOnly);
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
    FDownSize: Integer;
    FFocused: Boolean;
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
    procedure Resize; override;
    procedure PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    procedure PaintBackground(vCanvas: TCanvas; vRect: TRect; vColor: TColor); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure HelperChanged; virtual;
    procedure Snap; virtual;
    procedure InvalidateRect(vRect: TRect); virtual;
    procedure ExcludeClipRect(vCanvas: TCanvas; vRect: TRect); virtual;
    property InnerHeight: Integer read GetInnerHeight;
    property InnerWidth: Integer read GetInnerWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetOuterRect: TRect; virtual;
    function GetContentsRect: TRect; virtual;
    function GetInnerRect: TRect; virtual;
    function GetParentColor: TColor;
    function GetTextStyle: TTextStyle; virtual; //in public to calc the style outside the control
    function GetInputs: TposFrameInputs; virtual;
    property InUpdating: Boolean read GetInUpdating;
    property Style: TposFrameStyle read FStyle write FStyle default [fsBorder];
    property States: TposDrawStates read FStates write FStates;
    property Down: Boolean read GetDown write SetDown;
    property DownSize: Integer read FDownSize write FDownSize default 1;
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

  TposWinFrame = class;
  
  { TposSubFrame }

  TposSubFrame = class(TObject)
  private
    FFrame: TposWinFrame;
    FWidth: Integer;
    FHeight: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FPlace: TposSubFramePlace;
    FInteractive: Boolean;
  protected
    procedure Click; virtual;
    function UseRightToLeft: Boolean; virtual;
  public
    constructor Create(AFrame: TposWinFrame); virtual;
    function GetRect(var vRect: TRect): TRect; virtual;
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); virtual;
    property Frame: TposWinFrame read FFrame write FFrame;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property Visible: Boolean read FVisible write FVisible default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Place: TposSubFramePlace read FPlace write FPlace;
    //Interactive: take Keys and Mouse events, only outer subframes
    property Interactive: Boolean read FInteractive write FInteractive default False;
  end;

  TposSubFrames = class(TObjectList)
  private
    function GetItem(Index: Integer): TposSubFrame;
    procedure SetItem(Index: Integer; const Value: TposSubFrame);
  published
  public
    procedure Paint(vPlace: TposSubFramePlace; vCanvas: TCanvas; var Rect: TRect; vColor: TColor); virtual;
    function GetByXY(vRect: TRect; X, Y: Integer): TposSubFrame;
    procedure GetRect(vPlace: TposSubFramePlace; var vRect: TRect);
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

  TposShapeSubFrame = class(TposSubFrame)
  private
    FShape: TposShapeKind;
  published
  public
    function GetRect(var vRect: TRect): TRect; override;
    procedure Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    property Shape: TposShapeKind read FShape write FShape;
  end;

  //WinFrame can have internal TposSubFrames
  TposWinFrame = class(TposFocusFrame)
  private
    FActiveSubFrame: TposSubFrame;
    FActiveSubFrameDown: Boolean;
    FSubFrames: TposSubFrames;
  protected
    procedure PaintOuter(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetOuterRect: TRect; override;
    function GetContentsRect: TRect; override;
    function GetInnerRect: TRect; override;
    property SubFrames: TposSubFrames read FSubFrames;
  end;

  //TposSybariteFrame Have label and button
  TposSybariteButtonSubFrame = class(TposShapeSubFrame)
  protected
    procedure Click; override;
  public
  end;

  TposSybariteHelper = class(TposHelper)
  protected
    procedure ButtonClick(Sender: TposFrame); virtual;
  public
  end;

  TposSybariteFrame = class(TposWinFrame)
  private
    FLabelFrame: TposLabelSubFrame;
    FButtonFrame: TposSybariteButtonSubFrame;
    FLabelMode: Boolean;
    FOnButtonClick: TNotifyEvent;
    procedure SetLabelCaption(const Value: TCaption);
    procedure SetLabelWidth(const Value: Integer);
    function GetLabelCaption: TCaption;
    function GetLabelWidth: Integer;
    procedure SetButtonShow(const Value: Boolean);
    function GetButtonShow: Boolean;
    function GetButtonShape: TposShapeKind;
    procedure SetButtonShape(const Value: TposShapeKind);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoButtonClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LabelCaption: TCaption read GetLabelCaption write SetLabelCaption;
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth default 0;
    //LabelMode make it as TLabel not TEdit usefull for translating engine
    property LabelMode: Boolean read FLabelMode write FLabelMode default False;
    property ButtonShow: Boolean read GetButtonShow write SetButtonShow default False;
    property ButtonShape: TposShapeKind read GetButtonShape write SetButtonShape default shpNone;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  published
  end;

  TposCustomFrame = class(TposWinFrame)
  public
  end;

  TScaleSize = record
    Multiplier, Divider: Integer
  end;

  { TposEngine }

  TposEngine = class(TObject)
  private
    FAutoScale: Boolean;
    FFocusedFrame: TposFocusFrame;
    FScaleSize: TScaleSize;
    FSnapToGrid: Boolean;
    FGridSize: Integer;
    procedure SetFocusedFrame(const Value: TposFocusFrame);
  protected
    property ScaleSize: TScaleSize read FScaleSize write FScaleSize;
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
    //Scale function
    property AutoScale: Boolean read FAutoScale write FAutoScale default False;
    procedure SetScale(Multiplier, Divider: Integer);
    function Scale(Size: Integer): Integer;
    //published
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid default True;
    property GridSize: Integer read FGridSize write FGridSize default sSnapGridSize;
    property FocusedFrame: TposFocusFrame read FFocusedFrame write SetFocusedFrame;
  end;

function posEngine: TposEngine;

implementation

uses
  mnUtils,
{$IFDEF FPC}
{$ELSE}
  MMSystem,
{$ENDIF}
  posThemes;

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
  FScaleSize.Multiplier := 1;
  FScaleSize.Divider := 1;
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

procedure TposEngine.SetScale(Multiplier, Divider: Integer);
begin
  FScaleSize.Multiplier := Multiplier;
  FScaleSize.Divider := Divider;
  FAutoScale := True;
end;

function TposEngine.Scale(Size: Integer): Integer;
begin
  if AutoScale then
    Result := MulDiv(Size, ScaleSize.Multiplier, ScaleSize.Divider)
  else
    Result := Size;
end;

procedure TposEngine.FocusPrior;
begin
end;

function TposEngine.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  k: Char;
  //aInputs: TposFrameInputs;
begin
  Result := (FocusedFrame <> nil) and FocusedFrame.KeyDown(Key, Shift);
  if not Result then
  begin
    //aInputs := FocusedFrame.GetInputs;
    if Shift = [] then
    begin
      case Key of
        VK_DELETE:
        begin
          k := #24;   
          Result := KeyPress(k);//ahh stupid way
        end;
{        9:
          if not (fiTab in aInputs) then
            FocusNext;}
      end;
    end
    else if Shift = [ssShift] then
    begin
{      case Key of
        9:
          if not (fiTab in aInputs) then
            FocusPrior;
      end;}
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
    else if ((fiKeys in aInputs) or (fiText in aInputs)) and not (fiReadOnly in aInputs) then
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
  aInnerRect: TRect;
  aColor: TColor;
  procedure FrameRect(R: TRect; vSize: Integer; vColor:TColor);
  begin
    if (vSize > 0) then
    begin
      Canvas.Brush.Color := vColor;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + vSize));
      Canvas.FillRect(Rect(R.Right - vSize, R.Top, R.Right, R.Bottom));
      Canvas.FillRect(Rect(R.Left, R.Bottom - vSize, R.Right, R.Bottom));
      Canvas.FillRect(Rect(R.Left, R.Top, R.Left + vSize, R.Bottom));
    end;
  end;
var
  TmpRect: TRect;
begin
  inherited;
  aColor := Color;
  Canvas.Font := Self.Font;
  Canvas.Brush.Style := bsSolid;
  aRect := ClientRect;
  if (Margin > 0) then
  begin
    FrameRect(aRect, Margin, GetParentColor);
    InflateRect(aRect, -Margin, -Margin);
  end;

  PaintOuter(Canvas, aRect, aColor);

  if fsBorder in Style then
  begin
    TmpRect := aRect;
    if Focused then
      Canvas.Pen.Color := Lighten(aColor, 50)
    else
      Canvas.Pen.Color := Lighten(aColor, 25);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := BorderWidth;
    Canvas.Brush.Style := bsClear;
    FrameRect(TmpRect, BorderWidth, Canvas.Pen.Color);
    Canvas.Brush.Style := bsSolid;
    InflateRect(aRect, -BorderWidth, -BorderWidth);
  end;

  FrameRect(aRect, Padding, aColor);
  InflateRect(aRect, -Padding, -Padding);

  aInnerRect := GetInnerRect;
  if (fsOpaque in Style) and not (fsLatedOpaque in Style) then
    PaintBackground(Canvas, aInnerRect, aColor);
  PaintInner(Canvas, aRect, aColor);
  if (fsOpaque in Style) and  (fsLatedOpaque in Style) then
    PaintBackground(Canvas, aInnerRect, aColor);
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

function TposFrame.GetContentsRect: TRect;
begin
  Result := GetOuterRect;
  if fsBorder in Style then
    InflateRect(Result, -BorderWidth, -BorderWidth);
  InflateRect(Result, -Padding, -Padding);
end;

constructor TposFrame.Create(AOwner: TComponent);
begin
  inherited;
  FStyle := [fsBorder];
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks] - [csClickEvents];
  FMargin := cMargin;
  FBorderWidth := 1;
  FDownSize := 1;
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
  {$ifdef FPC}
  BorderWidth := MulDiv(BorderWidth, M, D);
  FDownSize := MulDiv(FDownSize, M, D);
  {$endif}
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

{$ENDIF}

procedure TposWinFrame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aRect, R: TRect;
begin
  if fsMouse in Style then
  begin
    if Button = mbLeft then
    try
      inherited;
      FActiveSubFrame := FSubFrames.GetByXY(ClientRect, X, Y);
      if FActiveSubFrame <> nil then
      begin
        FActiveSubFrameDown := True;
        aRect := ClientRect;
        R := FActiveSubFrame.GetRect(aRect);
        InvalidateRect(R);
      end
      else
        Down := True;
    except
      FActiveSubFrame := nil;
      FActiveSubFrameDown := False;
      Down := False;
      raise;
    end;
  end
  else
    inherited;
end;

{$IFNDEF FPC}
procedure TposFrame.MouseEnter;
begin
end;

procedure TposFrame.MouseLeave;
begin
end;
{$ENDIF}

procedure TposWinFrame.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aRect, R: TRect;
  OldDown: Boolean;
begin
  if fsMouse in Style then
  begin
    if MouseCapture then
    begin
      if FActiveSubFrame <> nil then
      begin
        aRect := ClientRect;
        OldDown := FActiveSubFrameDown;
        R := FActiveSubFrame.GetRect(aRect);
        if PtInRect(R, Point(X, Y)) then
          FActiveSubFrameDown := True
        else
          FActiveSubFrameDown := False;
        if OldDown <> FActiveSubFrameDown then
          InvalidateRect(R);
      end
      else
      begin
        if PtInRect(ClientRect, Point(X, Y)) then
          Down := True
        else
          Down := False;
      end;
    end;
  end
  else
    inherited;
end;

procedure TposWinFrame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aRect, R: TRect;
begin
  inherited;
  if fsMouse in Style then
  begin
    if Button = mbLeft then
    try
      if FActiveSubFrame = nil then
      begin
        if PtInRect(ClientRect, Point(X, Y)) then
          Click;
      end;
    finally
      FActiveSubFrameDown := False;
      if FActiveSubFrame <> nil then
      begin
        aRect := ClientRect;
        R := FActiveSubFrame.GetRect(aRect);
        InvalidateRect(R);
        try
          if PtInRect(R, Point(X, Y)) then
            FActiveSubFrame.Click;
        finally
          FActiveSubFrame := nil;
        end;
      end
      else
        Down := False;
    end;
  end
  else if Button = mbLeft then
    if PtInRect(ClientRect, Point(X, Y)) then
      Click;
end;

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
  InitMemory(Result, Sizeof(Result));
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

function TposFrame.GetOuterRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -Margin, -Margin);
end;

procedure TposFrame.InvalidateRect(vRect: TRect);
begin
  if InUpdating then
    Inc(FInvalidateCount)
  else if Parent <> nil then
  begin
    OffsetRect(vRect, Left, Top); //Because it use the Canvas of parent
{$IFDEF FPC}
    LCLIntf.InvalidateRect(Parent.Handle, @vRect, False);
{$ELSE}
    Windows.InvalidateRect(Parent.Handle, @vRect, False);
{$ENDIF}
  end
  else
    Invalidate;
end;

procedure TposFrame.ExcludeClipRect(vCanvas: TCanvas; vRect: TRect);
begin
  if Parent <> nil then
  begin
    posUtils.ExcludeClipRect(vCanvas, vRect);
  end;
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
  Result := GetContentsRect;
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

procedure TposSybariteFrame.SetButtonShape(const Value: TposShapeKind);
begin
  if FButtonFrame.Shape <> Value then
  begin
    FButtonFrame.Shape := Value;
    Invalidate;
  end;
end;

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

procedure TposSybariteFrame.SetButtonShow(const Value: Boolean);
begin
  if FButtonFrame.Visible <> Value then
  begin
    FButtonFrame.Visible := Value;
    Invalidate;
  end;
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
  FButtonFrame := TposSybariteButtonSubFrame.Create(Self);
  FButtonFrame.Place := sbpOuter;
  FButtonFrame.Interactive := True;
  FButtonFrame.Visible := False;
  SubFrames.Add(FLabelFrame);
  SubFrames.Add(FButtonFrame);
end;

destructor TposSybariteFrame.Destroy;
begin
  inherited;
end;

procedure TposSybariteFrame.DoButtonClick;
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Self);
  if Helper is TposSybariteHelper then
    (Helper as TposSybariteHelper).ButtonClick(self);
end;

function TposSybariteFrame.GetButtonShape: TposShapeKind;
begin
  Result := FButtonFrame.Shape;
end;

function TposSybariteFrame.GetLabelCaption: TCaption;
begin
  Result := FLabelFrame.Caption
end;

function TposSybariteFrame.GetLabelWidth: Integer;
begin
  Result := FLabelFrame.Width;
end;

function TposSybariteFrame.GetButtonShow: Boolean;
begin
  Result := FButtonFrame.Visible;
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

procedure TposSubFrame.Click;
begin
  Themes.PlaySound('CLICK', True, True);
end;

function TposSubFrame.UseRightToLeft: Boolean;
begin
  if FFrame <> nil then
    Result := FFrame.UseRightToLeftAlignment
  else
    Result := False;
end;

constructor TposSubFrame.Create(AFrame: TposWinFrame);
begin
  inherited Create;
  FFrame := AFrame;
  FVisible := True;
  FEnabled := True;
end;

function TposSubFrame.GetRect(var vRect: TRect): TRect;
begin
  Result :=vRect;
end;

procedure TposSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
end;

{ TposSubFrames }

function TposSubFrames.GetByXY(vRect: TRect; X, Y: Integer): TposSubFrame;
var
  i: Integer;
  R: TRect;
  aItem: TposSubFrame;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    aItem := Items[i];
    if aItem.Visible and aItem.Interactive and (aItem.Place = sbpOuter) then
    begin
      R := aItem.GetRect(vRect);
      if PtInRect(R, Point(X, Y)) then
      begin
        Result := aItem;
        Break;
      end;
    end;
  end;
end;

function TposSubFrames.GetItem(Index: Integer): TposSubFrame;
begin
  Result := inherited Items[Index] as TposSubFrame;
end;

procedure TposSubFrames.GetRect(vPlace: TposSubFramePlace; var vRect: TRect);
var
  i: Integer;
begin
  for i :=0 to Count -1 do
  begin
    if Items[i].Visible and (Items[i].Place = vPlace) then
      Items[i].GetRect(vRect);
  end;
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
    vCanvas.Brush.Style := bsSolid;
    vColor := Lighten(Frame.Color, -25);
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

function TposWinFrame.GetContentsRect: TRect;
begin
  Result := inherited GetContentsRect;
end;

function TposWinFrame.GetInnerRect: TRect;
begin
  Result := inherited GetInnerRect;
  FSubFrames.GetRect(sbpInner, Result);
end;

function TposWinFrame.GetOuterRect: TRect;
begin
  Result := inherited GetOuterRect;
  FSubFrames.GetRect(sbpOuter, Result);
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

{ TposShapeSubFrame }

function TposShapeSubFrame.GetRect(var vRect: TRect): TRect;
begin
  Result := vRect;
  if Visible then
  begin
    if Frame.UseRightToLeftAlignment then
    begin
      Result.Right := Result.Left + Frame.Height + 1;
      vRect.Left := Result.Right;
    end
    else
    begin
      Result.Left := Result.Right - Frame.Height - 1;
      vRect.Right := Result.Left;
    end;
  end;
end;

procedure TposShapeSubFrame.Paint(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
var
  aRect: TRect;
  TR: TRect;
  function IsDown:Boolean;
  begin
    Result := (Frame.FActiveSubFrame = Self) and Frame.FActiveSubFrameDown;
  end;
begin
  inherited;
  if Visible then
  begin
    aRect := GetRect(vRect);
    TR := aRect;
    if Frame.UseRightToLeftAlignment then
    begin
      aRect.Right := aRect.Right - 1;
      TR.Left := aRect.Right;
    end
    else
    begin
      aRect.Left := aRect.Left + 1;
      TR.Right := aRect.Left;
    end;
    vCanvas.Brush.Color := Frame.GetParentColor;
    vCanvas.FillRect(TR);
    vCanvas.Brush.Color := Frame.Color;
    vCanvas.FillRect(aRect);

    PaintBorderButton(vCanvas, aRect, Frame.Color, clDefault, [pdsBorder], IsDown);
    DrawShape(vCanvas, aRect, Shape, IsDown, True, UseRightToLeft, 0, Frame.Font.Color);
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

{ TposSybariteButtonSubFrame }

procedure TposSybariteButtonSubFrame.Click;
begin
  inherited;
  if Frame is TposSybariteFrame then
    (Frame as TposSybariteFrame).DoButtonClick;
end;

{ TposSybariteHelper }

procedure TposSybariteHelper.ButtonClick(Sender: TposFrame);
begin
end;

initialization
finalization
  FreeAndNil(FposEngine);
end.

