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

{
  You must apply the patch in this bug tracker
  http://bugs.freepascal.org/view.php?id=18458
}

{$mode objfpc}{$H+}

interface

uses
  Classes, Messages, Controls, SysUtils, Math, Contnrs, Graphics, Forms, StdCtrls, Types,
  LMessages, LCLType, LCLIntf, LCLProc,
  ntvTabs, ntvUtils, ntvThemes;

type
  TntvCustomTabSet = class;

  TOnTabSelect = procedure(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean) of object;
  TOnTabSelected = procedure(Sender: TObject; OldTab, NewTab: TntvTabItem) of object;

  { TntvTabSetItems }

  TntvTabSetItems = class(TntvTabs)
  protected
    FControl: TCustomControl;
    procedure Invalidate; override;
    procedure UpdateCanvas(vCanvas: TCanvas); override;
  public
    property Control: TCustomControl read FControl write FControl;
  end;

  { TntvCustomTabSet }

  TntvCustomTabSet = class(TCustomControl)
  private
    FItems: TntvTabs;
    FHeaderHeight: Integer;
    FInternalHeaderHeight: Integer;
    FOnTabSelected: TOnTabSelected;
    FOnTabSelect: TOnTabSelect;
    FShowBorder: Boolean;
    FStoreIndex: Boolean;
    FShowTabs: Boolean;
    function GetImageList: TImageList;
    function GetShowButtons: Boolean;
    function GetTopIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetShowBorder(const AValue: Boolean);
    procedure SetTopIndex(const Value: Integer);
    function GetItemIndex: Integer;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    //procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDesignHitTest(var Message: TLMMouse); message CM_DESIGNHITTEST;

    procedure SetShowButtons(const Value: Boolean);
    procedure DoTabChanged(OldItem, NewItem: TntvTabItem);
    procedure SetShowTabs(const Value: Boolean);
    procedure SetImageList(const Value: TImageList);
  protected
    function ChildKey(var Message: TLMKey): boolean; override;
    procedure FontChanged(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateHeaderRect;
    function GetHeaderHeight: Integer;

    procedure NextClick;
    procedure PriorClick;

    function GetTabsRect: TRect; virtual;

    procedure DoTabShow(Index: Integer; vSetfocus: Boolean); virtual;
    procedure DoTabShowed(Index: Integer; vSetfocus: Boolean); virtual;

    procedure ShowTab(Index: Integer; Force: Boolean = False; vSetfocus: Boolean = True);
    function SelectTab(Index: Integer; Force: Boolean = False): Boolean;
    function LeftMouseDown(Point: TPoint): boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    procedure Loaded; override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetFlags: TntvFlags;
    function CreateTabs: TntvTabs; virtual;
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
    property ShowBorder: Boolean read FShowBorder write SetShowBorder default True;
    property HeaderHeight: Integer read FHeaderHeight write FHeaderHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored FStoreIndex default 0;
    property ImageList: TImageList read GetImageList write SetImageList;

    property Items: TntvTabs read FItems write FItems;
    property OnTabSelected: TOnTabSelected read FOnTabSelected write FOnTabSelected;
    property OnTabSelect: TOnTabSelect read FOnTabSelect write FOnTabSelect;

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
    property OnTabSelect;
    property OnTabSelected;

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

{ TntvTabSetItems }

procedure TntvTabSetItems.Invalidate;
begin
  inherited Invalidate;
  if (FControl <> nil) and not (csLoading in FControl.ComponentState) and FControl.HandleAllocated then
  begin
    FControl.Invalidate;
  end;
end;

procedure TntvTabSetItems.UpdateCanvas(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Font.Assign(FControl.Font);
end;

  { TntvCustomTabSet }

constructor TntvCustomTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDesignInteractive, csCaptureMouse, csClickEvents, csAcceptsControls, csSetCaption, csOpaque, csDoubleClicks];
  FItems := CreateTabs;
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

procedure TntvCustomTabSet.SetShowBorder(const AValue: Boolean);
begin
  if FShowBorder =AValue then exit;
  FShowBorder :=AValue;
  Invalidate;
end;

procedure TntvCustomTabSet.Paint;
var
  i: Integer;
  R: TRect;
  aTabsRect: TRect;
begin
  inherited; //do not inherited
  with Canvas do
  begin
    Font.Assign(Self.Font);
    if not ShowTabs or (Items.Visibles.Count = 0) then
    begin
      if (csDesigning in ComponentState) then
      begin
        Pen.Style := psDot;
        Pen.Color := clDkGray;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(GetTabsRect);
        Pen.Style := psSolid;
        Pen.Color := clDkGray;
        aTabsRect := GetClientRect;
        MoveTo(ClientRect.Left, aTabsRect.Bottom);
        LineTo(ClientRect.Left, ClientRect.Bottom - 1);
        LineTo(ClientRect.Right -1 , ClientRect.Bottom - 1);
        LineTo(ClientRect.Right - 1, aTabsRect.Bottom);
        LineTo(ClientRect.Left, aTabsRect.Bottom);
      end
    end
    else
    begin
      if ShowTabs then
      begin
        Items.Paint(Canvas, GetTabsRect, GetFlags);
        aTabsRect := GetTabsRect;
        Items.GetTabRect(aTabsRect, ItemIndex, R, GetFlags);

        Pen.Style := psSolid;
        Pen.Color := clDkGray;

        MoveTo(ClientRect.Left, aTabsRect.Bottom);
        LineTo(R.Left, aTabsRect.Bottom);

        MoveTo(R.Right, aTabsRect.Bottom);
        LineTo(ClientRect.Right, aTabsRect.Bottom);

        if ShowBorder then
        begin
          LineTo(ClientRect.Left, ClientRect.Bottom - 1);
          LineTo(ClientRect.Right, ClientRect.Bottom - 1);
          LineTo(ClientRect.Right, aTabsRect.Bottom - 1);
        end;
      end;
    end;
  end;
end;

procedure TntvCustomTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    if not Force and Assigned(FOnTabSelect) then
      OnTabSelect(Self, Items.Visibles[ItemIndex], Items.Visibles[Index], Result);
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
    if FHeaderHeight = 0 then
      FInternalHeaderHeight := TmpCanvas.TextHeight('A') + cHeaderHeightMargin
    else
      FInternalHeaderHeight := FHeaderHeight;
  finally
    ReleaseDC(0, TmpCanvas.Handle);
    TmpCanvas.Free;
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
    Result.Bottom := Result.Top + GetHeaderHeight;
  end;
end;

procedure TntvCustomTabSet.CMDesignHitTest(var Message: TLMMouse);
var
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
      ht := Items.HitTest(Canvas, pt, GetTabsRect, i, GetFlags);
      if (ht <> htNone) and (i <> Items.ItemIndex) then
        Message.Result := 1;
    end;
  end;
end;

function TntvCustomTabSet.LeftMouseDown(Point: TPoint): boolean;
var
  i: Integer;
  ht: TntvhtTabHitTest;
begin
  Result := False;
  if (Items.Visibles.Count > 0) and PtInRect(GetTabsRect, Point) then
  begin
    ht := Items.HitTest(Canvas, Point, GetTabsRect, i, GetFlags);
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

procedure TntvCustomTabSet.DoTabShow(Index: Integer; vSetfocus: Boolean);
begin
end;

procedure TntvCustomTabSet.DoTabShowed(Index: Integer; vSetfocus: Boolean);
begin
end;

procedure TntvCustomTabSet.ShowTab(Index: Integer; Force: Boolean; vSetfocus: Boolean);
var
  i: Integer;
  OldIndex: Integer;
  w: Integer;
begin
//  if HandleAllocated then
  begin
    if ((Index <> Items.ItemIndex) or Force) and (Index < Items.Visibles.Count) then
    begin
      OldIndex := Items.ItemIndex;
      DoTabShow(Index, vSetfocus);
      //and (Items[Index].Control.Enabled)
      Items.ItemIndex := Index;

      if (Items.ItemIndex < 0) and (Items.Visibles.Count > 0) then
        Items.ItemIndex := 0
      else if (Items.ItemIndex > Items.Visibles.Count - 1) then
        Items.ItemIndex := Items.Visibles.Count - 1;
      if Items.ItemIndex < TopIndex then
        TopIndex := Items.ItemIndex
      else
        Items.ShowTab(Canvas, GetTabsRect, ItemIndex, GetFlags);
      if Items.ItemIndex >= 0 then
      begin
        Invalidate;
        DoTabShowed(Index, vSetfocus);
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

function TntvCustomTabSet.GetItemIndex: Integer;
begin
  Result := Items.ItemIndex;
end;

procedure TntvCustomTabSet.EraseBackground(DC: HDC);
begin
  inherited;
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
(*
procedure TntvCustomTabSet.CMDialogKey(var Message: TCMDialogKey);
begin
  //if not (csDesigning in ComponentState) and (Focused or IsChild(Handle, Windows.GetFocus)) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  if not (csDesigning in ComponentState) and (Focused ) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    if GetKeyState(VK_SHIFT) >= 0 then
    begin
      //nexttab
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
*)
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
  if Assigned(FOnTabSelected) then
    FOnTabSelected(Self, OldItem, NewItem);
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
    Result := FInternalHeaderHeight
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
  message.Result := message.Result or DLGC_WANTARROWS;
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
    Result := Result + [tbfRightToLeft];
  if Focused then
    Result := Result + [tbfFocused];
end;

function TntvCustomTabSet.CreateTabs: TntvTabs;
begin
  Result := TntvTabSetItems.Create(TntvTabItem);
  (Result as TntvTabSetItems).Control := Self;
end;

end.

