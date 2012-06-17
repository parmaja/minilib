unit ntvSpliters;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @ported    ported from Lazarus component TSpliter in ExtCtrls
 *}

{$mode objfpc}{$H+}

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Variants,
  LCLIntf, LCLType, IntfGraphics, FPimage, GraphType,
  Types;

type
  { TntvCustomSplitter }

  TntvResizeStyle = (
    nrsLine,     // draw a line, don't update splitter position during moving
    nrsNone,     // draw nothing and don't update splitter position during moving
    nrsPattern,  // draw a dot pattern, don't update splitter position during moving
    nrsUpdate    // draw nothing, update splitter position during moving
  );

  TCanOffsetEvent = procedure(Sender: TObject; var NewOffset: Integer;
    var Accept: Boolean) of object;
  TCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer;
    var Accept: Boolean) of object;

  TntvSplitterStyle = (spsSpace, spsLoweredLine, spsRaisedLine);

  TntvCustomSplitter = class(TGraphicControl)
  private
    FAutoSnap: boolean;
    FMouseInControl: Boolean;
    FOnCanOffset: TCanOffsetEvent;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FResizeAnchor: TAnchorKind;
    FResizeStyle: TntvResizeStyle;
    FSplitDragging: Boolean;
    FSplitterStartMouseXY: TPoint; // in screen coordinates
    FSplitterStartLeftTop: TPoint; // in screen coordinates
    FResizeControl: TControl;
    FSplitterWindow: HWND;
    FStyle: TntvSplitterStyle;
    procedure SetResizeControl(AValue: TControl);
    procedure SetStyle(AValue: TntvSplitterStyle);
  protected
    procedure AlignTo(Control:TControl);
    function AdaptAnchors(const aAnchors: TAnchors): TAnchors;
    function CheckNewSize(var NewSize: Integer): Boolean; virtual;
    function CheckOffset(var NewOffset: Integer): Boolean; virtual;

    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlign(Value: TAlign); override;
    procedure SetAnchors(const AValue: TAnchors); override;
    procedure SetResizeAnchor(const AValue: TAnchorKind); virtual;
    procedure StartSplitterMove(const MouseXY: TPoint);
    procedure StopSplitterMove(const MouseXY: TPoint);
    procedure UpdateCursor;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Loaded; override;
    procedure MoveSplitter(Offset: Integer);
  public
    property AutoSnap: boolean read FAutoSnap write FAutoSnap default False;
    property Cursor default crHSplit;
    property Style: TntvSplitterStyle read FStyle write SetStyle default spsLoweredLine;
    property ResizeAnchor: TAnchorKind read FResizeAnchor write SetResizeAnchor default akLeft;
    property ResizeStyle: TntvResizeStyle read FResizeStyle write FResizeStyle default nrsUpdate;
    property ResizeControl: TControl read FResizeControl write SetResizeControl;
    property OnCanOffset: TCanOffsetEvent read FOnCanOffset write FOnCanOffset;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

  TntvSplitter = class(TntvCustomSplitter)
  published
    property Anchors;
    property AutoSnap;
    property Color;
    property Constraints;
    property Cursor;
    property Height;
    property OnCanOffset;
    property OnCanResize;
    property OnChangeBounds;
    property OnMoved;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ResizeStyle;
    property ResizeControl;
    property ShowHint;
    property Visible;
    property Width;
  end;

implementation

uses
  Math, Themes;

{ TntvCustomSplitter }

procedure TntvCustomSplitter.MoveSplitter(Offset: integer);
var
  aMinSize: Integer;
  aMaxSize: Integer;
  function GetParentClientSize: Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Parent.ClientWidth;
      akTop, akBottom: Result := Parent.ClientHeight;
    end;
  end;

  function GetControlMinPos(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft,akRight: Result := Control.Left;
      akTop,akBottom: Result := Control.Top;
    end
  end;

  function GetControlSize(Control: TControl): Integer;
  begin
    Result := 0;
    if Assigned(Control) then
      case ResizeAnchor of
        akLeft, akRight: Result := Control.Width;
        akTop, akBottom: Result := Control.Height;
      end;
  end;

  function GetControlConstraintsMinSize(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Control.Constraints.EffectiveMinWidth;
      akTop, akBottom: Result := Control.Constraints.EffectiveMinHeight;
    end;
  end;

  function GetControlConstraintsMaxSize(Control: TControl): Integer;
  begin
    case ResizeAnchor of
      akLeft, akRight: Result := Control.Constraints.EffectiveMaxWidth;
      akTop, akBottom: Result := Control.Constraints.EffectiveMaxHeight;
    end;
  end;

  procedure SetAlignControlSize(NewSize: Integer);
  var
    NewBounds: TRect;
  begin
    NewBounds := ResizeControl.BoundsRect;
    case ResizeAnchor of
      akLeft:
        NewBounds.Right := NewBounds.Left + NewSize;
      akRight:
        NewBounds.Left := NewBounds.Right - NewSize;
      akTop:
        NewBounds.Bottom := NewBounds.Top + NewSize;
      akBottom:
        NewBounds.Top := NewBounds.Bottom - NewSize;
    end;
    ResizeControl.BoundsRect := NewBounds;
  end;

  procedure DrawAlignControlSize(NewSize: Integer);
  var
    NewRect: TRect;
    OldSize: Integer;
  begin
    // get the splitter position
    NewRect := BoundsRect;
    NewRect.TopLeft := Parent.ClientToScreen(NewRect.TopLeft);
    NewRect.BottomRight := Parent.ClientToScreen(NewRect.BottomRight);

    // offset it accordinly
    OldSize := GetControlSize(ResizeControl);
    case ResizeAnchor of
      akLeft:
        OffsetRect(NewRect, NewSize - OldSize, 0);
      akRight:
        OffsetRect(NewRect, OldSize - NewSize, 0);
      akTop:
        OffsetRect(NewRect, 0, NewSize - OldSize);
      akBottom:
        OffsetRect(NewRect, 0, OldSize - NewSize);
    end;
    SetRubberBandRect(FSplitterWindow, NewRect);
  end;

  function CalcNewSize(MinSize, EndSize, Offset: Integer): Integer;
  var
    NewSize: Integer;
  begin
    NewSize := GetControlSize(ResizeControl);
    case ResizeAnchor of
      akLeft, akTop:     Inc(NewSize, Offset);
      akRight, akBottom: Dec(NewSize, Offset);
    end;

    if NewSize > EndSize then
      NewSize := EndSize;
    if NewSize < MinSize then
      NewSize := MinSize;

    if AutoSnap and (NewSize < MinSize) then
      NewSize := MinSize;
    Result := NewSize;
  end;

var
  NewSize: Integer;
  i: Integer;
begin
  if Offset = 0 then
    Exit;

  if not Assigned(ResizeControl) then
    Exit;

  // calculate minimum size
  if not AutoSnap then
    aMinSize := GetControlConstraintsMinSize(ResizeControl)
  else
    aMinSize := 1;
  if aMinSize > 1 then
    Dec(aMinSize);

  // calculate maximum size
  case ResizeAnchor of
    akLeft, akTop:
    begin
      aMaxSize := GetControlSize(ResizeControl) - GetControlMinPos(ResizeControl)
                  + GetParentClientSize - GetControlSize(ResizeControl)
    end;
    akRight, akBottom:
    begin
      aMaxSize := GetControlSize(ResizeControl) + GetControlMinPos(ResizeControl);
    end;
  end;
  aMaxSize := Max(GetControlConstraintsMaxSize(ResizeControl), aMaxSize);

  NewSize := CalcNewSize(aMinSize, aMaxSize, Offset);
  // OnCanResize event
  if CheckOffset(Offset) and CheckNewSize(NewSize) then
    if not FSplitDragging or (ResizeStyle = nrsUpdate) then
      SetAlignControlSize(NewSize)
    else
      DrawAlignControlSize(NewSize);
end;

procedure TntvCustomSplitter.SetResizeControl(AValue: TControl);
begin
  if FResizeControl =AValue then Exit;
  if FResizeControl <> nil then
    FResizeControl.RemoveFreeNotification(Self);
  FResizeControl :=AValue;
  if FResizeControl <> nil then
    FResizeControl.FreeNotification(Self);
  AlignTo(FResizeControl);
end;

procedure TntvCustomSplitter.SetStyle(AValue: TntvSplitterStyle);
begin
  if FStyle =AValue then Exit;
  FStyle :=AValue;
end;

procedure TntvCustomSplitter.AlignTo(Control: TControl);
begin
  if Control <> nil then
  begin
    Align := Control.Align;
  end;
end;

procedure TntvCustomSplitter.SetResizeAnchor(const AValue: TAnchorKind);
begin
  if FResizeAnchor = AValue then Exit;
  FResizeAnchor := AValue;
  UpdateCursor;
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TntvCustomSplitter.StartSplitterMove(const MouseXY: TPoint);
var
  NewRect: TRect;
  Pattern: HBrush;
begin
  if FSplitDragging then
    Exit;
  FSplitDragging := True;
  FSplitterStartMouseXY := MouseXY;
  FSplitterStartLeftTop := Point(Left, Top);
  if ResizeStyle in [nrsLine, nrsPattern] then
  begin
    NewRect := BoundsRect;
    NewRect.TopLeft := Parent.ClientToScreen(NewRect.TopLeft);
    NewRect.BottomRight := Parent.ClientToScreen(NewRect.BottomRight);

    if ResizeStyle = nrsLine then
      Pattern := GetStockObject(BLACK_BRUSH)
    else
      Pattern := ThemeServices.DottedBrush;

    FSplitterWindow := CreateRubberband(NewRect, Pattern);
  end;
end;

procedure TntvCustomSplitter.StopSplitterMove(const MouseXY: TPoint);
var
  Offset: Integer;
begin
  if FSplitDragging then
  begin
    case ResizeAnchor of
      akLeft, akRight:
        Offset := (MouseXY.X - FSplitterStartMouseXY.X) - (Self.Left - FSplitterStartLeftTop.X);
      akTop, akBottom:
        Offset := (MouseXY.Y - FSplitterStartMouseXY.Y) - (Self.Top - FSplitterStartLeftTop.Y);
    else
      Offset := 0;
    end;

    FSplitDragging := False;
    if Offset <> 0 then
      MoveSplitter(Offset);

    if Assigned(OnMoved) then
      OnMoved(Self);
    if ResizeStyle in [nrsLine, nrsPattern] then
      DestroyRubberBand(FSplitterWindow);
  end;
end;

procedure TntvCustomSplitter.UpdateCursor;
begin
  if ResizeAnchor in [akLeft, akRight] then
    Cursor := crHSplit
  else
    Cursor := crVSplit;
end;

procedure TntvCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  MousePos: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  // While resizing X, Y are not valid. Use absolute mouse position.
  if Button = mbLeft then
  begin
    GetCursorPos(MousePos);
    StartSplitterMove(MousePos);
  end;
end;

procedure TntvCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
  MousePos: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and (Parent <> nil) and (FSplitDragging) then
  begin
    // While resizing X, Y are not valid. Use the absolute mouse position.
    GetCursorPos(MousePos);
    case ResizeAnchor of
      akLeft, akRight:
        Offset := (MousePos.X - FSplitterStartMouseXY.X) - (Self.Left - FSplitterStartLeftTop.X);
      akTop, akBottom:
        Offset := (MousePos.Y - FSplitterStartMouseXY.Y) - (Self.Top - FSplitterStartLeftTop.Y);
    else
      Offset := 0;
    end;

    if Offset <> 0 then
      MoveSplitter(Offset);
  end;
end;

procedure TntvCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  GetCursorPos(MousePos);
  StopSplitterMove(MousePos);
end;

procedure TntvCustomSplitter.SetAlign(Value: TAlign);
begin
  inherited;
  case Value of
    alLeft:   ResizeAnchor := akLeft;
    alTop:    ResizeAnchor := akTop;
    alRight:  ResizeAnchor := akRight;
    alBottom: ResizeAnchor := akBottom;
  end;
end;

procedure TntvCustomSplitter.SetAnchors(const AValue: TAnchors);
var
  NewValue: TAnchors;
begin
  NewValue:=AdaptAnchors(AValue);
  if NewValue = Anchors then exit;
  inherited SetAnchors(NewValue);
end;

function TntvCustomSplitter.AdaptAnchors(const aAnchors: TAnchors): TAnchors;
begin
  Result := aAnchors;
  case Align of
    alLeft:   Result := Result - [akRight] + [akLeft];
    alTop:    Result := Result - [akBottom] + [akTop];
    alRight:  Result := Result + [akRight] - [akLeft];
    alBottom: Result := Result + [akBottom] - [akTop];
  end;
end;

function TntvCustomSplitter.CheckNewSize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanResize) then
    OnCanResize(Self, NewSize, Result);
end;

function TntvCustomSplitter.CheckOffset(var NewOffset: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanOffset) then
    OnCanOffset(Self, NewOffset, Result);
end;

procedure TntvCustomSplitter.Paint;
var
  C1, C2: TColor;
  x, y: Integer;
begin
  case Style of
    spsRaisedLine:
      begin
        C1 := cl3DHilight;
        C2 := cl3DShadow;
      end;
    spsLoweredLine:
      begin
        C1 := cl3DShadow;
        C2 := cl3DHilight;
      end;
    else
    begin
      inherited;
      exit;
    end;
  end;
  Canvas.Pen.Width:=1;
  with Canvas, ClientRect do
    case ResizeAnchor of
      akTop, akBottom:
      begin
        Pen.Color := C1;
        y := (Bottom - Top) div 2;
        MoveTo(Left, y);
        LineTo(Right - 1, y);
        Pen.Color := C2;
        MoveTo(Left, y + 1);
        LineTo(Right - 1, y + 1);
      end;
      akLeft, akRight:
      begin
        Pen.Color:=C1;
        x := (Right - Left) div 2;
        MoveTo(x, Top);
        LineTo(x, Bottom - 1);
        Pen.Color := C2;
        MoveTo(x + 1, Top);
        LineTo(x + 1, Bottom - 1);
      end;
    end;
end;

procedure TntvCustomSplitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ResizeControl then
      ResizeControl := nil;
  end;
end;

procedure TntvCustomSplitter.MouseEnter;
begin
  inherited MouseEnter;
  if csDesigning in ComponentState then exit;

  if not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    invalidate;
  end;
end;

procedure TntvCustomSplitter.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then exit;

  if FMouseInControl then
  begin
    FMouseInControl := False;
    invalidate;
  end;
end;

constructor TntvCustomSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FResizeStyle := nrsUpdate;
  FAutoSnap := False;
  FMouseInControl := False;
  FResizeAnchor := akLeft;
  FStyle := spsLoweredLine;
  Width := 5;
end;

procedure TntvCustomSplitter.Loaded;
begin
  inherited Loaded;
  UpdateCursor;
end;

end.

