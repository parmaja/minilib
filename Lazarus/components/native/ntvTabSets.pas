unit ntvTabSets;
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
  Classes, Messages, Controls, SysUtils, Math, Contnrs, Graphics, Forms, StdCtrls, Types,
  LMessages, LCLType, LCLIntf, LCLProc,
  ntvTabs, ntvUtils;

const
  cHeaderHeightMargin = 1;

type
  TntvCustomTabSet = class;

  TOnSelectTab = procedure(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean) of object;
  TOnTabChanged = procedure(Sender: TObject; OldTab, NewTab: TntvTabItem) of object;

  { TntvPageControl }

  TntvCustomTabSet = class(TCustomControl)
  private
    FItems: TntvTabs;
    FHeaderHeight: Integer;
    FOnTabChanged: TOnTabChanged;
    FOnSelectTab: TOnSelectTab;
    FStoreIndex: Boolean;
    FShowTabs: Boolean;
    function GetImageList: TImageList;
    function GetShowButtons: Boolean;
    function GetTopIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetTopIndex(const Value: Integer);
    function GetItemIndex: Integer;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GetDlgCode;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDesignHitTest(var Message: TLMMouse); message CM_DESIGNHITTEST;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    procedure SetShowButtons(const Value: Boolean);
    procedure DoTabChanged(OldItem, NewItem: TntvTabItem);
    procedure SetShowTabs(const Value: Boolean);
    function GetHeaderHeight: Integer;
    procedure SetImageList(const Value: TImageList);
    procedure AdjustTextRect(var vRect: TRect; vImageIndex: Integer);
  protected
    function ChildKey(var Message: TLMKey): boolean; override;
    function GetClientRect: TRect; override;
    procedure FontChanged(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateHeaderRect;

    procedure NextClick;
    procedure PriorClick;

    function GetTabsRect: TRect; virtual;
    function GetInnerRect: TRect; virtual;

    procedure DoShowTab(Index: Integer; Force: Boolean = False; vSetfocus: Boolean = True); virtual;
    procedure ShowTab(Index: Integer; Force: Boolean = False; vSetfocus: Boolean = True);
    function SelectTab(Index: Integer; Force: Boolean = False): Boolean;
    function LeftMouseDown(Point: TPoint): boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property HeaderHeight: Integer read GetHeaderHeight;
    procedure Loaded; override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetFlags: TntvFlags;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;

    procedure Next;
    procedure Prior;
    procedure Clear;
  //to published
    property StoreIndex: Boolean read FStoreIndex write FStoreIndex;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons default True;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored FStoreIndex default 0;
    property ImageList: TImageList read GetImageList write SetImageList;

    property Items: TntvTabs read FItems write FItems;
    property OnTabChanged: TOnTabChanged read FOnTabChanged write FOnTabChanged;
    property OnSelectTab: TOnSelectTab read FOnSelectTab write FOnSelectTab;

    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TntvTabSet = class(TntvCustomTabSet)
  published
    property StoreIndex;
    property ShowButtons;
    property ShowTabs;
    property ItemIndex;
    property ImageList;

    property Items;
    property OnTabChanged;
    property OnSelectTab;

    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  WSControls, WSLCLClasses;

type
  { TntvWSPageControl }

  TntvWSPageControl = class(TWSWinControl)
  protected
  published
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
  end;

{ TntvWSPageControl }

class function TntvWSPageControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
end;

{ TntvCustomTabSet }

constructor TntvCustomTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDesignInteractive, csClickEvents, csAcceptsControls, csSetCaption, csOpaque, csDoubleClicks];
  FItems := TntvTabs.Create(TntvTabItem);
  Width := 250;
  Height := 150;
  Items.ItemIndex := -1;
  Items.TopIndex := 0;
  UpdateHeaderRect;
  FShowTabs := True;
  SetInitialBounds(0, 0, GetControlClassDefaultSize.cx, GetControlClassDefaultSize.cy);
end;

destructor TntvCustomTabSet.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TntvCustomTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    WindowClass.style := WindowClass.style or (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TntvCustomTabSet.SetItemIndex(Value: Integer);
begin
  if Items.ItemIndex <> Value then
  begin
    if csLoading in ComponentState then
      Items.ItemIndex := Value
    else if not SelectTab(Value) then
        //beep;
  end;
end;

procedure TntvCustomTabSet.Paint;
var
  i: Integer;
  Rect: TRect;
  NavRect: TRect;
begin
  //inherited; do not inherited
  with Canvas do
  begin
    Canvas.Font.Assign(Self.Font);
    if Items.Visibles.Count = 0 then
    begin
      Exit;
    end
    else
    begin
    end;

    if ShowTabs then
    begin
      Items.Paint(Canvas, GetTabsRect, GetFlags);
    end;
  end;
end;

procedure TntvCustomTabSet.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    LeftMouseDown(Point(x, y));
end;

procedure TntvCustomTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TntvCustomTabSet.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TntvCustomTabSet.SelectTab(Index: Integer; Force: Boolean): Boolean;
begin
  if (Index < Items.Visibles.Count) and (Index <> ItemIndex) and (Index > -1) then
  begin
    Result := True;
    if not Force and Assigned(FOnSelectTab) then
      FOnSelectTab(Self, Items.Visibles[ItemIndex], Items.Visibles[Index], Result);
    if Result then
      ShowTab(Index, True);
  end
  else
    Result := False;
end;

procedure TntvCustomTabSet.Loaded;
var
  i: Integer;
begin
  inherited;
  if StoreIndex then
    ShowTab(Items.ItemIndex, True, False)
  else
    ShowTab(0, True, False);
end;

procedure TntvCustomTabSet.UpdateHeaderRect;
var
  TmpCanvas: TCanvas;
begin
  TmpCanvas := TCanvas.Create;
  try
    TmpCanvas.Handle := GetDC(0);
    TmpCanvas.Font.Assign(Font);
    FHeaderHeight := TmpCanvas.TextHeight('A') + cHeaderHeightMargin;
  finally
    ReleaseDC(0, TmpCanvas.Handle);
    TmpCanvas.Free;
  end;
end;

procedure TntvCustomTabSet.AdjustTextRect(var vRect: TRect; vImageIndex: Integer);
begin
  if (ImageList <> nil) and (vImageIndex > -1) then
  begin
    if IsRightToLeft then
    begin
      vRect.Left := vRect.Left + 3;
      vRect.Right := vRect.Right - ImageList.Width - 3;
    end
    else
    begin
      vRect.Left := vRect.Left + 6;
      vRect.Left := vRect.Left + ImageList.Width;
    end;
  end;
end;

procedure TntvCustomTabSet.SetTopIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < Items.Visibles.Count) then
  begin
    Items.TopIndex := Value;
    Invalidate;
  end;
end;

function TntvCustomTabSet.GetTabsRect: TRect;
begin
  if not ShowTabs then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    Result := ClientRect;
    Result.Bottom := Result.Top + HeaderHeight;
  end;
end;

procedure TntvCustomTabSet.CMDesignHitTest(var Message: TLMMouse);
var
  R: TRect;
  pt: TPoint;
  i: Integer;
  ht: TntvhtTabHitTest;
begin
  inherited;
  if Items.Visibles.Count > 0 then
  begin
    pt := SmallPointToPoint(Message.Pos);
    if PtInRect(GetTabsRect, pt) then
    begin
      ht := Items.HitTest(pt, GetTabsRect, i, GetFlags);
      if (ht <> htNone) and (i <> Items.ItemIndex) then
      begin
        Message.Result := 1;
      end;
    end;
  end;
end;

procedure TntvCustomTabSet.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin

end;


function TntvCustomTabSet.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  //Result.Top := Result.Top + HeaderHeight;
end;

function TntvCustomTabSet.LeftMouseDown(Point: TPoint): boolean;
var
  i: Integer;
  ht: TntvhtTabHitTest;
begin
  Result := False;
  if (Items.Visibles.Count > 0) and PtInRect(GetTabsRect, Point) then
  begin
    ht := Items.HitTest(Point, GetTabsRect, i, GetFlags);
    if (ht <> htNone) then
    begin
      case ht of
        htTab:
        begin
          if (i <> Items.ItemIndex) then
          begin
            SelectTab(i);
            Result := True;
          end
          else
            SetFocus;
        end;
        htNext:
        begin
          NextClick;
          Result := True;
        end;
        htPrior:
        begin
          PriorClick;
          Result := True;
        end;
      end;
    end;
  end;
end;
procedure TntvCustomTabSet.DoShowTab(Index: Integer; Force: Boolean = False; vSetfocus: Boolean = True);
begin
end;

procedure TntvCustomTabSet.ShowTab(Index: Integer; Force: Boolean; vSetfocus: Boolean);
var
  R: TRect;
  aTopIndex: Integer;
  i: Integer;
  OldIndex: Integer;
  w: Integer;
begin
//  if HandleAllocated then
  begin
    if ((Index <> Items.ItemIndex) or Force) and (Index < Items.Visibles.Count) then
    begin
      OldIndex := Items.ItemIndex;
      DoShowTab(Index, Force, vSetfocus);
      //and (Items[Index].Control.Enabled)

      Items.ItemIndex := Index;

      if (Items.ItemIndex < 0) and (Items.Visibles.Count > 0) then
        Items.ItemIndex := 0
      else if (Items.ItemIndex > Items.Visibles.Count - 1) then
        Items.ItemIndex := Items.Visibles.Count - 1;
      if Items.ItemIndex < TopIndex then
        TopIndex := Items.ItemIndex;
      if Items.ItemIndex >= 0 then
      begin
        Invalidate;
      end
      else if Items.ItemIndex < 0 then
      begin
        Items.ItemIndex := Index;
        Invalidate;
      end;

      if ((OldIndex <> -1) and (OldIndex < Items.Visibles.Count)) then
      begin
        if ((Index <> -1) and (Index < Items.Visibles.Count)) then
          DoTabChanged((Items.Visibles[OldIndex]), (Items.Visibles[Index]))
        else
          DoTabChanged((Items.Visibles[OldIndex]), nil)
      end
      else
      begin
        if ((Index <> -1) and (Index < Items.Visibles.Count)) then
          DoTabChanged(nil, Items.Visibles[Index])
        else
          DoTabChanged(nil, nil)
      end;
    end
  end;
end;

procedure TntvCustomTabSet.NextClick;
begin
end;

procedure TntvCustomTabSet.PriorClick;
begin
end;

function TntvCustomTabSet.GetInnerRect: TRect;
begin
  Result := ClientRect;
  if ShowTabs then
    Inc(Result.Top, HeaderHeight);
end;

function TntvCustomTabSet.GetItemIndex: Integer;
begin
  Result := Items.ItemIndex;
end;

procedure TntvCustomTabSet.EraseBackground(DC: HDC);
begin
  //inherited;
end;

function TntvCustomTabSet.ChildKey(var Message: TLMKey): boolean;
var
  ShiftState: TShiftState;
begin
  if not (csDesigning in ComponentState) and (Message.CharCode in [VK_PRIOR, VK_NEXT]) then
  begin
    ShiftState :=  KeyDataToShiftState(Message.KeyData);
    if (ShiftState = [ssCtrl]) then
    begin
      if Message.CharCode = VK_NEXT then
      begin
        if ItemIndex <> Items.Visibles.Count - 1 then
          SelectTab(ItemIndex + 1)
        else
          SelectTab(0);
      end
      else
      begin
        if ItemIndex = 0 then
          SelectTab(Items.Visibles.Count - 1)
        else
          SelectTab(ItemIndex - 1);
      end;
      Result := True;
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

procedure TntvCustomTabSet.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Invalidate;
end;

{begin
  Result:=inherited ChildKey(Message);
end;}

procedure TntvCustomTabSet.CMDialogKey(var Message: TCMDialogKey);
begin
  //if not (csDesigning in ComponentState) and (Focused or IsChild(Handle, Windows.GetFocus)) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  if not (csDesigning in ComponentState) and (Focused ) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    if GetKeyState(VK_SHIFT) >= 0 then
    begin
      //nextpage
      if ItemIndex <> Items.Visibles.Count - 1 then
        SelectTab(ItemIndex + 1)
      else
        SelectTab(0);
    end
    else
    begin
      if ItemIndex = 0 then
        SelectTab(Items.Visibles.Count - 1)
      else
        SelectTab(ItemIndex - 1);
    end;
    Message.Result := 1;
  end
  else
  begin
    inherited;
    case Message.CharCode of
      VK_NEXT: Next;
      VK_PRIOR: Prior;
    end;
  end;
end;

function TntvCustomTabSet.GetTopIndex: Integer;
begin
  Result := Items.TopIndex;
end;

function TntvCustomTabSet.GetImageList: TImageList;
begin
    Result := Items.Images;
end;

function TntvCustomTabSet.GetShowButtons: Boolean;
begin
  Result := Items.ShowButtons;
end;

procedure TntvCustomTabSet.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TntvCustomTabSet.Next;
begin
  ItemIndex := ItemIndex + 1;
end;

procedure TntvCustomTabSet.Prior;
begin
  ItemIndex := ItemIndex - 1;
end;

procedure TntvCustomTabSet.SetShowButtons(const Value: Boolean);
begin
  if Items.ShowButtons <> Value then
  begin
    Items.ShowButtons := Value;
    Invalidate;
  end;
end;

procedure TntvCustomTabSet.DoTabChanged(OldItem, NewItem: TntvTabItem);
begin
  if Assigned(FOnTabChanged) then
    FOnTabChanged(Self, OldItem, NewItem);
end;

procedure TntvCustomTabSet.SetShowTabs(const Value: Boolean);
begin
  if FShowTabs <> Value then
  begin
    FShowTabs := Value;
    Realign;
    Invalidate;
  end;
end;

function TntvCustomTabSet.GetHeaderHeight: Integer;
begin
  if ShowTabs then
    Result := FHeaderHeight
  else
    Result := 0;
end;

procedure TntvCustomTabSet.Clear;
begin
  Items.Clear;
  while Items.Count > 0 do
  begin
    Items[Items.Count - 1].Free;
  end;
end;

procedure TntvCustomTabSet.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  message.Result := message.Result or DLGC_WANTARROWS; //or DLGC_WANTTAB or DLGC_WANTMESSAGE or DLGC_WANTALLKEYS;
end;

procedure TntvCustomTabSet.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift = [] then
  begin
    case Key of
      VK_LEFT:
        begin
          if UseRightToLeftAlignment then
            Next
          else
            Prior;
        end;
      VK_RIGHT:
        begin
          if UseRightToLeftAlignment then
            Prior
          else
            Next;
        end;
    end;
  end;
end;

procedure TntvCustomTabSet.SetImageList(const Value: TImageList);
begin
  if (Items.Images <> Value) then
  begin
    Items.Images := Value;
  end;
end;

class function TntvCustomTabSet.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 200;
  Result.cy := 60;
end;

function TntvCustomTabSet.GetFlags: TntvFlags;
begin
  Result := [];
  if UseRightToLeftAlignment then
    Result := Result + [tbfUseRightToLeft];
  if Focused then
    Result := Result + [tbfFocused];
end;

initialization
end.

