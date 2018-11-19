unit ntvPanels;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @ported    ported from Lazarus component TSplitter in ExtCtrls
 *}

{$mode objfpc}{$H+}
{$define USE_NTV_THEME}

interface

uses
  SysUtils, Classes, Graphics, Controls, Variants,
  {$ifdef USE_NTV_THEME}
  ntvThemes,
  {$endif USE_NTV_THEME}
  LCLIntf, LCLType, GraphType;

type
  { TntvCustomPanel }

  TntvResizeStyle = (
    nrsNone,     // draw nothing and don't update splitter position during moving
    nrsUpdate,    // draw nothing, update splitter position during moving
    nrsLine     // draw a line, don't update splitter position during moving
    );

  TCanOffsetEvent = procedure(Sender: TObject; var NewOffset: Integer; var Accept: Boolean) of object;
  TCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer; var Accept: Boolean) of object;

  TntvResizeBevel = (spsNone, spsSpace, spsLoweredLine, spsRaisedLine);

  TntvCustomPanel = class(TCustomControl)
  private
    FAutoSnap: Boolean;
    FMouseInControl: Boolean;
    FOnCanOffset: TCanOffsetEvent;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FResizeStyle: TntvResizeStyle;
    FResizeBevel: TntvResizeBevel;
    FSplitDragging: Boolean;
    FSplitterSize: Integer;
    FSplitterPoint: TPoint; // in screen coordinates
    FSplitterStartPoint: TPoint; // in screen coordinates
    FSplitterWindow: HWND;
    procedure SetResizeBevel(AValue: TntvResizeBevel);
    procedure SetSplitterSize(AValue: Integer);
  protected
    procedure Paint; override;

    function CheckNewSize(var NewSize: Integer): Boolean; virtual;
    function CheckOffset(var NewOffset: Integer): Boolean; virtual;

    function CalcOffset(MousePos: TPoint): Integer;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateCursor(X, Y: Integer);
    function GetSplitterRect: TRect;

    procedure StartSplitterMove(const MousePos: TPoint);
    procedure StopSplitterMove(const MousePos: TPoint);
    procedure MoveSplitter(Offset: Integer);

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Loaded; override;
  public
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default False;
    property ResizeBevel: TntvResizeBevel read FResizeBevel write SetResizeBevel default spsLoweredLine;
    property ResizeStyle: TntvResizeStyle read FResizeStyle write FResizeStyle default nrsNone;
    property SplitterSize: Integer read FSplitterSize write SetSplitterSize default 8;
    property OnCanOffset: TCanOffsetEvent read FOnCanOffset write FOnCanOffset;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

  TntvPanel = class(TntvCustomPanel)
  published
    property Anchors;
    property Align;
    property Color;
    property Constraints;
    property Height;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width;

    property OnMoved;
    property OnChangeBounds;
    property OnCanOffset;
    property OnCanResize;
    property AutoSnap;
    property ResizeStyle;
    property SplitterSize;
  end;

implementation

uses
  Math;

{ TntvCustomPanel }

procedure TntvCustomPanel.MoveSplitter(Offset: Integer);
var
  aMinSize: Integer;
  aMaxSize: Integer;

  function GetParentClientSize: Integer;
  begin
    case Align of
      alLeft, alRight: Result := Parent.ClientWidth;
      alTop, alBottom: Result := Parent.ClientHeight;
    end;
  end;

  function GetControlMinPos: Integer;
  begin
    case Align of
      alLeft, alRight: Result := Left;
      alTop, alBottom: Result := Top;
      else
        Result := 0;
    end;
  end;

  function GetControlSize: Integer;
  begin
    case Align of
      alLeft, alRight: Result := Width;
      alTop, alBottom: Result := Height;
      else
        Result := 0;
    end;
  end;

  function GetControlConstraintsMinSize: Integer;
  begin
    case Align of
      alLeft, alRight: Result := Constraints.EffectiveMinWidth;
      alTop, alBottom: Result := Constraints.EffectiveMinHeight;
    end;
  end;

  function GetControlConstraintsMaxSize: Integer;
  begin
    case Align of
      alLeft, alRight: Result := Constraints.EffectiveMaxWidth;
      alTop, alBottom: Result := Constraints.EffectiveMaxHeight;
    end;
  end;

  procedure SetAlignControlSize(NewSize: Integer);
  var
    NewBounds: TRect;
  begin
    NewBounds := BoundsRect;
    case Align of
      alLeft:
        NewBounds.Right := NewBounds.Left + NewSize;
      alRight:
        NewBounds.Left := NewBounds.Right - NewSize;
      alTop:
        NewBounds.Bottom := NewBounds.Top + NewSize;
      alBottom:
        NewBounds.Top := NewBounds.Bottom - NewSize;
    end;
    BoundsRect := NewBounds;
  end;

  procedure DrawAlignControlSize(NewSize: Integer);
  var
    NewRect: TRect;
    OldSize: Integer;
  begin
    // get the splitter position
    NewRect := GetSplitterRect;

    NewRect.TopLeft := ClientToScreen(NewRect.TopLeft);
    NewRect.BottomRight := ClientToScreen(NewRect.BottomRight);

    OldSize := GetControlSize;
    case Align of
      alLeft:
        OffsetRect(NewRect, NewSize - OldSize, 0);
      alRight:
        OffsetRect(NewRect, OldSize - NewSize, 0);
      alTop:
        OffsetRect(NewRect, 0, NewSize - OldSize);
      alBottom:
        OffsetRect(NewRect, 0, OldSize - NewSize);
    end;
    SetRubberBandRect(FSplitterWindow, NewRect);
  end;

  function CalcNewSize(MinSize, EndSize, Offset: Integer): Integer;
  var
    NewSize: Integer;
  begin
    NewSize := GetControlSize;
    case Align of
      alLeft:
        Inc(NewSize, Offset);
      alTop:
        Inc(NewSize, Offset);
      alRight:
        Dec(NewSize, Offset);
      alBottom:
        Dec(NewSize, Offset);
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
begin
  if Offset <> 0 then
  begin
    // calculate minimum size
    if not AutoSnap then
      aMinSize := GetControlConstraintsMinSize
    else
      aMinSize := 1;

    if aMinSize > 1 then
      Dec(aMinSize);

    // calculate maximum size
    case Align of
      alLeft, alTop:
        aMaxSize := GetControlSize - GetControlMinPos + GetParentClientSize - GetControlSize;
      alRight, alBottom:
        aMaxSize := GetControlSize + GetControlMinPos;
    end;
    aMaxSize := Max(GetControlConstraintsMaxSize, aMaxSize);

    NewSize := CalcNewSize(aMinSize, aMaxSize, Offset);

    if CheckOffset(Offset) and CheckNewSize(NewSize) then
      if not FSplitDragging or (ResizeStyle = nrsUpdate) then
      begin
        SetAlignControlSize(NewSize);
      end
      else
        DrawAlignControlSize(NewSize);
  end;
end;

procedure TntvCustomPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if FResizeStyle > nrsNone then
    case Align of
      alTop, alBottom:
      begin
        if Align = alTop then
          Rect.Bottom := Rect.Bottom - SplitterSize
        else
          Rect.Top := Rect.Top + SplitterSize;
      end;
      alLeft, alRight:
      begin
        if Align = alLeft then
          Rect.Right := Rect.Right - SplitterSize
        else
          Rect.Left := Rect.Left + SplitterSize;
      end;
    end;
  inherited AlignControls(AControl, Rect);
end;

procedure TntvCustomPanel.SetResizeBevel(AValue: TntvResizeBevel);
begin
  if FResizeBevel <> AValue then
  begin
    FResizeBevel := AValue;
    Invalidate;
  end;
end;

procedure TntvCustomPanel.SetSplitterSize(AValue: Integer);
begin
  if FSplitterSize <> AValue then
  begin
    FSplitterSize :=AValue;
  end;
end;

procedure TntvCustomPanel.StartSplitterMove(const MousePos: TPoint);
var
  NewRect: TRect;
  P: TPoint;
  Pattern: HBrush;
begin
  if not FSplitDragging then
  begin
    FSplitDragging := True;
    FSplitterPoint := MousePos;
    P := GetSplitterRect.TopLeft;
    case Align of
      alLeft: P.x := P.x + FSplitterSize;
      //alRight: P.x := P.x + FSplitterSize;
      alTop: P.y := P.y + FSplitterSize;
//      alBottom: P.y := P.y + FSplitterSize;
    end;
    FSplitterStartPoint := ClientToParent(P);
    if ResizeStyle in [nrsLine] then
    begin
      NewRect := GetSplitterRect;
      NewRect.TopLeft := ClientToScreen(NewRect.TopLeft);
      NewRect.BottomRight := ClientToScreen(NewRect.BottomRight);
      Pattern := GetStockObject(BLACK_BRUSH);
      FSplitterWindow := CreateRubberband(NewRect, Pattern);
    end;
  end;
end;

procedure TntvCustomPanel.StopSplitterMove(const MousePos: TPoint);
var
  Offset: Integer;
begin
  if FSplitDragging then
  begin
    Offset := CalcOffset(MousePos);

    FSplitDragging := False;
    if Offset <> 0 then
      MoveSplitter(Offset);

    if Assigned(OnMoved) then
      OnMoved(Self);
    if ResizeStyle in [nrsLine] then
      DestroyRubberBand(FSplitterWindow);
  end;
end;

function TntvCustomPanel.GetSplitterRect: TRect;
begin
  Result := ClientRect;
  case Align of
    alTop:
      Result.Top := Result.Bottom - SplitterSize;
    alBottom:
      Result.Bottom := Result.Top + SplitterSize;
    alLeft:
      Result.Left := Result.Right - SplitterSize;
    alRight:
      Result.Right := Result.Left + SplitterSize;
    else
      Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TntvCustomPanel.UpdateCursor(X, Y: Integer);
var
  R: TRect;
begin
  R := GetSplitterRect;
  if FSplitDragging or PtInRect(R, Point(x, y)) then
    case Align of
      alLeft:
        Cursor := crHSplit;
      alRight:
        Cursor := crHSplit;
      alBottom:
        Cursor := crVSplit;
      alTop:
        Cursor := crVSplit;
      else
        Cursor := crDefault;
    end
  else
    Cursor := crDefault;
end;

procedure TntvCustomPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  inherited;
  // While resizing X, Y are not valid. Use absolute mouse position.
  if Button = mbLeft then
  begin
    MousePos := Point(X, Y);
    GetCursorPos(MousePos);
    StartSplitterMove(MousePos);
  end;
end;

procedure TntvCustomPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
  MousePos: TPoint;
begin
  inherited;
  if FResizeStyle > nrsNone then
    UpdateCursor(x, y);
  if (FSplitDragging) and (ssLeft in Shift) and (Parent <> nil) then
  begin
    MousePos := Point(X, Y);
    // While resizing X, Y are not valid. Use the absolute mouse position.
    GetCursorPos(MousePos);
    Offset := CalcOffset(MousePos);
    if Offset <> 0 then
      MoveSplitter(Offset);
  end;
end;

procedure TntvCustomPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  inherited;
  MousePos := Point(X, Y);
  GetCursorPos(MousePos);
  StopSplitterMove(MousePos);
end;

function TntvCustomPanel.CheckNewSize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanResize) then
    OnCanResize(Self, NewSize, Result);
end;

function TntvCustomPanel.CheckOffset(var NewOffset: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanOffset) then
    OnCanOffset(Self, NewOffset, Result);
end;

function TntvCustomPanel.CalcOffset(MousePos: TPoint): Integer;
begin
  case Align of
    alLeft:
      Result := (MousePos.X - FSplitterPoint.X) - (BoundsRect.Right - FSplitterStartPoint.X);
    alRight:
      Result := (MousePos.X - FSplitterPoint.X) - (BoundsRect.Left - FSplitterStartPoint.X);
    alTop:
      Result := (MousePos.Y - FSplitterPoint.Y) + (BoundsRect.Bottom - FSplitterStartPoint.Y);
    alBottom:
      Result := (MousePos.Y - FSplitterPoint.Y) - (BoundsRect.Top - FSplitterStartPoint.Y);
    else
      Result := 0;
  end;
end;

procedure TntvCustomPanel.Paint;
var
  C1, C2: TColor;
  p: Integer;
begin
  inherited;
  if FResizeStyle > nrsNone then
  begin
    case ResizeBevel of
      spsRaisedLine:
      begin
        {$ifdef USE_NTV_THEME}
        C1 := ntvTheme.Painter.RaisedColor;
        C2 := ntvTheme.Painter.LoweredColor;
        {$else}
        C1 := clBtnHiLight;
        C2 := clBtnShadow;
        {$endif}
      end;
      spsLoweredLine:
      begin
        {$ifdef USE_NTV_THEME}
        C1 := ntvTheme.Painter.LoweredColor;
        C2 := ntvTheme.Painter.RaisedColor;
        {$else}
        C1 := clBtnShadow;
        C2 := clBtnHiLight;
        {$endif}
      end;
      else
      begin
        inherited;
        exit;
      end;
    end;
    Canvas.Pen.Width := 1;
    with Canvas, ClientRect do
      case Align of
        alTop, alBottom:
        begin
          if Align = alBottom then
            p := Top + SplitterSize div 2
          else
            p := Bottom - SplitterSize div 2;
          Pen.Color := C1;
          MoveTo(Left, p);
          LineTo(Right - 1, p);
          Pen.Color := C2;
          MoveTo(Left, p + 1);
          LineTo(Right - 1, p + 1);
        end;
        alLeft, alRight:
        begin
          if Align = alRight then
            p := Left + SplitterSize div 2
          else
            p := Right - SplitterSize div 2;
          Pen.Color := C1;
          MoveTo(p, Top);
          LineTo(p, Bottom - 1);
          Pen.Color := C2;
          MoveTo(p + 1, Top);
          LineTo(p + 1, Bottom - 1);
        end;
      end;
   end;
end;

procedure TntvCustomPanel.MouseEnter;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if not FMouseInControl and Enabled and (GetCapture = 0) then
    begin
      FMouseInControl := True;
      invalidate;
    end;
  end;
end;

procedure TntvCustomPanel.MouseLeave;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FMouseInControl then
    begin
      FMouseInControl := False;
      Cursor := crDefault;
      invalidate;
    end;
  end;
end;

constructor TntvCustomPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks, csReplicatable,  csNoFocus, csAutoSize0x0, csParentBackground] - [csOpaque];
  FResizeStyle := nrsNone;
  FSplitterSize := 8;
  FAutoSnap := False;
  FMouseInControl := False;
  FResizeBevel := spsLoweredLine;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

procedure TntvCustomPanel.Loaded;
begin
  inherited Loaded;
end;

end.
