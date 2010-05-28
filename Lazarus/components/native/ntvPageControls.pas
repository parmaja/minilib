unit ntvPageControls;
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
  LCLType, LCLIntf, LMessages, LCLProc,
  ntvTabs, ntvUtils;

const
  ControlTabWidth = 35;
  cHeaderHeightMargin = 10;
  nMixFlag = 75;

type
  TOnSelectPage = procedure(Sender: TObject; OldPage: TWinControl; NewPage: TWinControl; var CanSelect: boolean) of object;
  TOnPageChanged = procedure(Sender: TObject; OldPage: TWinControl; NewPage: TWinControl) of object;

  TntvPageControl = class;
  TntvPageItem = class;
  { TntvPage }

  TntvPage = class(TCustomControl)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TabStop default False;
    property Align default alClient;
  end;

  { TntvPageItem }

  TntvPageItem = class(TntvTabItem)
  private
    FControl: TWinControl;
    procedure SetControl(const Value: TWinControl);
    function GetPageControl: TntvPageControl;
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure Update; override;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property PageControl: TntvPageControl read GetPageControl;
    property Control: TWinControl read FControl write SetControl;
  published
  end;

  TntvPageItemClass = class of TntvPageItem;

  { TntvPages }

  TntvPages = class(TntvTabs)
  private
    FPageControl: TntvPageControl;
  protected
    function GetItem(Index: Integer): TntvPageItem;
    procedure SetItem(Index: Integer; Value: TntvPageItem);
    procedure Update(Item: TCollectionItem); override;
    procedure CalcTabWidth;
  public
    constructor Create(APageControl: TntvPageControl);
    destructor Destroy; override;
    function GetOwner: TPersistent; override;
    function Add: TntvPageItem;
    function FindControl(vControl: TWinControl): TntvPageItem;
    function AddControl(vControl: TWinControl): TntvPageItem;
    function ExtractControl(vControl: TWinControl): TWinControl;
    property Items[Index: Integer]: TntvPageItem read GetItem write SetItem stored False; default;
  published
  end;

  TPagesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TntvPageItem;
    procedure SetItem(Index: Integer; const Value: TntvPageItem);
  public
    property Items[Index: Integer]: TntvPageItem read GetItem write SetItem; default;
  end;

  { TntvPageControl }

  TntvPageControl = class(TCustomControl)
  private
    FItems: TntvPages;
    FPageList: TPagesList;
    FHeaderHeight: Integer;
    FOnPageChanged: TOnPageChanged;
    FOnSelectPage: TOnSelectPage;
    FStoreIndex: Boolean;
    FTabStyle: TTabStyle;
    FWrapper: TObject;
    FPageBorder: Integer;
    FShowButtons: Boolean;
    FShowTabs: Boolean;
    function GetImageList: TImageList;
    function GetTopIndex: Integer;
    procedure ReadWrapper(Reader: TReader);
    procedure WriteWrapper(Writer: TWriter);
    procedure SetPageIndex(Value: Integer);
    procedure SetTopIndex(const Value: Integer);
    function GetPageIndex: Integer;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GetDlgCode;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDesignHitTest(var Message: TLMMouse); message CM_DESIGNHITTEST;

    procedure SetPageBorder(const Value: Integer);
    procedure SetActivePage(const Value: TWinControl);
    function GetActivePage: TWinControl;
    procedure SetShowButtons(const Value: Boolean);
    procedure DoPageChanged(OldPage, NewPage: TWinControl);
    function GetPageItem(vControl: TWinControl): TntvPageItem;
    procedure SetShowTabs(const Value: Boolean);
    function GetHeaderHeight: Integer;

    procedure SetTabStyle(const Value: TTabStyle);
    procedure DrawVertLines(vRect: TRect; Index: Integer);
    procedure AdjustBtnRect(var vRect: TRect; Index: Integer);
    procedure SetImageList(const Value: TImageList);
    procedure AdjustTextRect(var vRect: TRect; vImageIndex: Integer);
  protected
    function ChildKey(var Message: TLMKey): boolean; override;
    function GetClientRect: TRect; override;
    procedure FontChanged(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ShowControl(AControl: TControl); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

    procedure CalcHeaderRect;

    function NextPageBtnRect: TRect;
    function PriorPageBtnRect: TRect;

    procedure NextPageBtnClick;
    procedure PriorPageBtnClick;

    function GetTabSetRect: TRect; virtual;
    function GetInnerRect: TRect; virtual;
    function GetPageRect: TRect; virtual;

    function GetTabRect(TopIndex, Index: Integer): TRect; overload;
    function GetTabRect(Index: Integer): TRect; overload; virtual;
    function GetTabOffset(Index: Integer): Integer;

    procedure DrawBorder; virtual;
    procedure DrawHeader; virtual;
    procedure DrawTab(Index: Integer; var Rect: TRect); virtual;
    procedure ShowPage(Index: Integer; Force: Boolean = False; vSetfocus: Boolean = True);
    function SelectPage(Index: Integer; Force: Boolean = False): Boolean;
    function LeftMouseDown(Point: TPoint): boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property HeaderHeight: Integer read GetHeaderHeight;
    function PageFromIndex(Index: Integer): TWinControl;
    procedure Loaded; override;

    procedure DrawRTLHeaderBorder(vRect: TRect; vColor: TColor; const Brd3D: Boolean = True);
    procedure DrawLTRHeaderBorder(vRect: TRect; vColor: TColor; const Brd3D: Boolean = True);
    procedure DrawNormalHeader; virtual;
    procedure DrawImage(var vRect: TRect; vImageIndex: Integer);

    procedure DrawNormalTab(Index: Integer; var Rect: TRect); virtual;
    procedure InternalDrawRaisedBorder; virtual;
    procedure InternalDrawBorder; virtual;
    class function GetControlClassDefaultSize: TSize; override;
    procedure WndProc(var TheMessage: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure EraseBackground(DC: HDC); override;

    procedure NextPage;
    procedure PriorPage;
    property ActivePage: TWinControl read GetActivePage write SetActivePage;
    procedure UpdatePagesList;
    procedure Clear;
    property PageItem[Control: TWinControl]: TntvPageItem read GetPageItem;
  published
    property StoreIndex: Boolean read FStoreIndex write FStoreIndex;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored FStoreIndex default 0;
    property PageBorder: Integer read FPageBorder write SetPageBorder default 3;
    property TabStyle: TTabStyle read FTabStyle write SetTabStyle default tbDefault;
    property ImageList: TImageList read GetImageList write SetImageList;

    property Items: TntvPages read FItems write FItems;
    property OnPageChanged: TOnPageChanged read FOnPageChanged write FOnPageChanged;
    property OnSelectPage: TOnSelectPage read FOnSelectPage write FOnSelectPage;

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

type
  TPageWrapperItem = class(TCollectionItem)
  private
    FControl: TWinControl;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
  published
    property Control: TWinControl read FControl write FControl;
  end;

  TPageWrappers = class(TCollection)
  private
    FPageControl: TntvPageControl;
  protected
    function GetItem(Index: Integer): TPageWrapperItem;
    procedure SetItem(Index: Integer; Value: TPageWrapperItem);
  public
    constructor Create(APageControl: TntvPageControl);
    function Add: TPageWrapperItem;
    property Items[Index: Integer]: TPageWrapperItem read GetItem write SetItem; default;
  published
  end;

{ TntvPage }


constructor TntvPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  TabStop := False;
  Align := alClient;
end;

{ TntvPageControl }

constructor TntvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDesignInteractive, csClickEvents, csAcceptsControls, csSetCaption, csOpaque, csDoubleClicks];
  FPageList := TPagesList.Create(False);
  FPageBorder := 3;
  FWrapper := TPageWrappers.Create(Self);
  FItems := TntvPages.Create(Self);
  Width := 250;
  Height := 150;
  Items.ItemIndex := -1;
  Items.TopIndex := 0;
  CalcHeaderRect;
  FShowButtons := True;
  FShowTabs := True;
  FTabStyle := tbDefault;
  SetInitialBounds(0, 0, GetControlClassDefaultSize.cx, GetControlClassDefaultSize.cy);
end;

destructor TntvPageControl.Destroy;
begin
  FItems.Free;
  FreeAndNil(FWrapper);
  FPageList.Free;
  inherited Destroy;
end;

procedure TntvPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    WindowClass.style := WindowClass.style or (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TntvPageControl.ShowControl(AControl: TControl);
var
  I: Integer;
begin
  if AControl <> nil then
  begin
    for I := 0 to FPageList.Count - 1 do
      if FPageList[I].Control = AControl then
      begin
        SelectPage(I);
        Exit;
      end;
  end;
  inherited;
end;

procedure TntvPageControl.SetPageIndex(Value: Integer);
begin
  if Items.ItemIndex <> Value then
  begin
    if csLoading in ComponentState then
      Items.ItemIndex := Value
    else if not SelectPage(Value) then
        //beep;
  end;
end;

procedure TntvPageControl.AlignControls(AControl: TControl; var Rect: TRect);
begin
  Rect := GetPageRect;
  inherited;
end;

procedure TntvPageControl.Paint;
var
  i: Integer;
  Rect: TRect;
  NavRect: TRect;
begin
  //inherited;
  with Canvas do
  begin
    //UpdatePagesList;
    if FPageList.Count = 0 then
    begin
      Rect := ClientRect;
      Brush.Color := Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
      DrawBorder;
      Exit;
    end
    else
    begin
      Rect := GetPageRect;
      ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      Rect := ClientRect;
      Brush.Color := Color;
      Brush.Style := bsSolid;
      Inc(Rect.Top, HeaderHeight - 1);
      InflateRect(Rect, -1, -1);
      FillRect(Rect);
    end;

    if ShowTabs then
    begin
      NavRect := GetTabSetRect;
      if ShowButtons then
      begin
        DrawHeader;
        ExcludeClipRect(Handle, NavRect.Left, NavRect.Top, NavRect.Right, NavRect.Bottom);
      end;
      Rect := GetTabRect(PageIndex);
      DrawTab(PageIndex, Rect);
      ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      for i := TopIndex to FPageList.Count - 1 do
      begin
        if i = PageIndex then
          Continue;
        Rect := GetTabRect(i);
        DrawTab(i, Rect);
        ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
        if ShowButtons then
          if UseRightToLeftAlignment then
          begin
            if Rect.Left < NavRect.Right then
              Break;
          end
          else
          begin
            if Rect.Right > NavRect.Left then
              Break;
          end;
      end;
    end;
    DrawBorder;
  end;
end;

procedure TntvPageControl.DrawBorder;
begin
  with Canvas do
  begin
    if ShowTabs then
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(0, ClientHeight - 1);
      LineTo(0, HeaderHeight - 1);
      LineTo(ClientWidth - 1, HeaderHeight - 1);
      Pen.Color := clBtnShadow;
      LineTo(ClientWidth - 1, ClientHeight - 1);
      LineTo(0, ClientHeight - 1);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, ClientWidth, HeaderHeight - 1));
    end
    else
    begin
      Pen.Color := clBlack;
      MoveTo(0, ClientHeight - 1);
      LineTo(0, 1);
      LineTo(ClientWidth - 1, 1);
      Pen.Color := clBtnShadow;
      LineTo(ClientWidth - 1, ClientHeight - 1);
      LineTo(0, ClientHeight - 1);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, ClientWidth, 1));
    end;
  end;
end;

procedure TntvPageControl.DrawTab(Index: Integer; var Rect: TRect);
var
  R, aTextRect: TRect;
  aTextStyle: TTextStyle;
begin
  with Canvas do
  begin
    aTextStyle := TextStyle;
    if (Index < 0) or (Index >= FPageList.Count) then
      Exit; //belal
    if PageIndex <> Index then
    begin
      Dec(Rect.Bottom);
    end
    else if Index > TopIndex then
      InflateRect(Rect, 1, 0)
    else
    begin
      if UseRightToLeftAlignment then
        Dec(Rect.Left)
      else
        Inc(Rect.Right);
    end;
    R := Rect;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(R);
    Inc(R.Top, 2);
    if Index <> Items.ItemIndex then
    begin
      Inc(R.Top, 2);
    end;
    Brush.Color := Color;
    Pen.Color := clBtnHighlight;
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Top);

    LineTo(R.Left + 1, R.Top - 1);
    LineTo(R.Right - 2, R.Top - 1);
    LineTo(R.Right - 1, R.Top);

    Pen.Color := clGray;
    LineTo(R.Right - 1, R.Bottom);

    aTextRect := R;
    if Index = Items.ItemIndex then
    begin
      MoveTo(R.Right - 2, R.Bottom);
      Pen.Color := Color;
      LineTo(R.Left, R.Bottom);
      InflateRect(R, -3, -1);
      if Focused then
        DrawFocusRect(R);
      OffsetRect(aTextRect, 0, -2);
    end;
    Brush.Style := bsClear;
    Canvas.Font.Assign(Self.Font);

    Brush.Style := bsClear;
    Canvas.Font.Assign(Self.Font);

    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    if UseRightToLeftAlignment then
       aTextStyle.RightToLeft := True;
    TextRect(aTextRect, 0, 0, FPageList[Index].Caption, aTextStyle);
  end;
end;

function TntvPageControl.GetTabOffset(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := index - 1 downto TopIndex do
    Result := Result + FPageList[index].Width;
end;

procedure TntvPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    LeftMouseDown(Point(x, y));
end;

procedure TntvPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Found, HeadarFalge: Boolean;
  R: TRect;
begin
  {Found := False;
  HeadarFalge := False;
  for I := 0 to Items.Count - 1 do
  begin
    R := GetTabRect(I);
    InflateRect(R, -8, -8);
    if PtInRect(R, Point(X, Y)) then
    begin
      if UseRightToLeftAlignment then
        HeadarFalge := PtInRect(Rect(0, 0, (R.Right - R.Left), R.Bottom), Point(X, Y))
      else
        HeadarFalge := PtInRect(Rect(Width - (R.Right - R.Left), 0, Width, R.Bottom), Point(X, Y));
      Found := True;
      if not HeadarFalge then
        UnderMouseIndex := I;
      Break;
    end;
  end;
  if not Found and not HeadarFalge then
    UnderMouseIndex := -1;}
  inherited;
end;

procedure TntvPageControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TntvPageControl.SelectPage(Index: Integer; Force: Boolean): Boolean;
begin
  if (Index < FPageList.Count) and (Index <> PageIndex) and (Index > -1) then
  begin
    Result := True;
    if not Force and Assigned(FOnSelectPage) then
      FOnSelectPage(Self, PageFromIndex(PageIndex), PageFromIndex(Index), Result);
    if Result then
      ShowPage(Index, True);
  end
  else
    Result := False;
end;

procedure TntvPageControl.Loaded;
var
  i: Integer;
begin
  inherited;
  for i := 0 to TPageWrappers(FWrapper).Count - 1 do
    if i < Items.Count then
      Items[i].Control := TPageWrappers(FWrapper)[i].Control
    else
      Break;
  TPageWrappers(FWrapper).Clear;
  Items.CalcTabWidth;
  UpdatePagesList;
  if StoreIndex then
    ShowPage(Items.ItemIndex, True, False)
  else
    ShowPage(0, True, False);
end;

{procedure TntvPageControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;}

procedure TntvPageControl.CalcHeaderRect;
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

procedure TntvPageControl.DrawHeader;
begin
  DrawNormalHeader;
end;


procedure TntvPageControl.DrawImage(var vRect: TRect; vImageIndex: Integer);
var
  Y: Integer;
begin
  if (ImageList <> nil) and (vImageIndex > -1) then
  begin
    Y := vRect.Top + (vRect.Bottom - vRect.Top) div 2 - ImageList.Height div 2;
    if IsRightToLeft then
    begin
      vRect.Right := vRect.Right - ImageList.Width - 3;
      ImageList.Draw(Canvas, vRect.Right, Y, vImageIndex, True);
    end
    else
    begin
      vRect.Left := vRect.Left + 3;
      ImageList.Draw(Canvas, vRect.Left, Y, vImageIndex, True);
      vRect.Left := vRect.Left + ImageList.Width;
    end;
  end;
end;

procedure TntvPageControl.AdjustTextRect(var vRect: TRect; vImageIndex: Integer);
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

procedure TntvPageControl.SetTopIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FPageList.Count) then
  begin
    Items.TopIndex := Value;
    Invalidate;
  end;
end;

function TntvPageControl.NextPageBtnRect: TRect;
begin
  if UseRightToLeftAlignment then
    Result := Rect(5, 5, 15, 15)
  else
    Result := Rect(ClientWidth - 15, 5, ClientWidth - 5, 15);
end;

function TntvPageControl.PriorPageBtnRect: TRect;
begin
  if UseRightToLeftAlignment then
    Result := Rect(20, 5, 30, 15)
  else
    Result := Rect(ClientWidth - 30, 5, ClientWidth - 20, 15);
end;

function TntvPageControl.GetTabSetRect: TRect;
begin
  if UseRightToLeftAlignment then
  begin
    if not ShowButtons then
      Result := Rect(0, 0, 0, 0)
    else
      Result := Rect(0, 0, ControlTabWidth, HeaderHeight)
  end
  else
  begin
    if not ShowButtons then
      Result := Rect(ClientWidth, 0, ClientWidth, 0)
    else
      Result := Rect(ClientWidth - ControlTabWidth, 0, ClientWidth, HeaderHeight);
  end;
  //Result := Rect (0, 0, 0, 0);
end;

procedure TntvPageControl.CMDesignHitTest(var Message: TLMMouse);
var
  pt: TPoint;
  i: Integer;
begin
  inherited;
  if FPageList.Count > 0 then
  begin
    pt := SmallPointToPoint(Message.Pos);
    if PtInRect(GetTabSetRect, pt) then
      Message.Result := 1
    else
    begin
      for i := TopIndex to FPageList.Count - 1 do
        if PtInRect(GetTabRect(i), pt) then
        begin
          if ActivePage <> FPageList[i].Control then
            Message.Result := 1;
          Break;
        end;
    end;
  end;
end;


function TntvPageControl.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  //Result.Top := Result.Top + HeaderHeight;
end;

function TntvPageControl.LeftMouseDown(Point: TPoint): boolean;
var
  i: Integer;
  Old: Integer;
begin
  if FPageList.Count = 0 then
  begin
    Result := False;
    Exit;
  end;
  if PtInRect(GetTabSetRect, Point) then
  begin
    Old := TopIndex;
    if PtInRect(NextPageBtnRect, Point) then
      NextPageBtnClick
    else if PtInRect(PriorPageBtnRect, Point) then
      PriorPageBtnClick;
    Result := Old <> TopIndex;
  end
  else
  begin
    Result := False;
    Old := Items.ItemIndex;
    for i := TopIndex to FPageList.Count - 1 do
      if PtInRect(GetTabRect(i), Point) then
      begin
        SelectPage(i);
        Result := True;
        if Old = Items.ItemIndex then
          SetFocus;
        Break;
      end;
  end;
end;

procedure TntvPageControl.ShowPage(Index: Integer; Force: Boolean; vSetfocus: Boolean);
var
  R: TRect;
  aTopIndex: Integer;
  ParentForm: TCustomForm;
  i: Integer;
  OldIndex: Integer;
  aList: TFPList;
  w: Integer;
begin
//  if HandleAllocated then
  begin
    if ((Index <> Items.ItemIndex) or Force) and (Index < FPageList.Count) then
    begin
      OldIndex := Items.ItemIndex;
      for i := 0 to FPageList.Count - 1 do
        if FPageList[i].Control <> nil then
          FPageList[i].Control.Visible := False;

      //and (Items[Index].Control.Enabled)

      Items.ItemIndex := Index;
      if (Items.ItemIndex < 0) and (FPageList.Count > 0) then
        Items.ItemIndex := 0
      else if (Items.ItemIndex > FPageList.Count - 1) then
        Items.ItemIndex := FPageList.Count - 1;
      if Items.ItemIndex < TopIndex then
        TopIndex := Items.ItemIndex;
      if Items.ItemIndex >= 0 then
      begin
        R := GetTabRect(Items.ItemIndex);
        aTopIndex := TopIndex;
        if UseRightToLeftAlignment then
        begin
          if ShowButtons then
            w := ControlTabWidth
          else
            w := 0;
          if R.Left < w then
          begin
            while (R.Left < w) and (aTopIndex < Items.ItemIndex) do
            begin
              aTopIndex := aTopIndex + 1;
              R := GetTabRect(aTopIndex, Items.ItemIndex);
            end;
          end;
        end
        else
        begin
          if ShowButtons then
            w := ClientWidth - ControlTabWidth
          else
            w := ClientWidth;
          if R.Right > w then
          begin
            while (R.Right > w) and (aTopIndex < Items.ItemIndex) do
            begin
              aTopIndex := aTopIndex + 1;
              R := GetTabRect(aTopIndex, Items.ItemIndex);
            end;
          end;
        end;

        Items.TopIndex := aTopIndex;
        Invalidate;
        if FPageList[Items.ItemIndex].Control <> nil then
        begin
          with FPageList[Items.ItemIndex].Control do
          begin
            BringToFront;
            Visible := True;
            Align := alClient;
            if (vSetFocus) and {not (csDesigning in ComponentState) and }not (csLoading in ComponentState) and (not Self.Focused) then
            begin
              ParentForm := GetParentForm(Self);
              if ParentForm <> nil then
              begin
                if TabStop and (FPageList[Items.ItemIndex].Control.CanFocus) then
                  ParentForm.ActiveControl := FPageList[Items.ItemIndex].Control
                else
                begin
                  aList := TFPList.Create;
                  try
                    FPageList[Items.ItemIndex].Control.GetTabOrderList(aList);
                    for i := 0 to aList.Count - 1 do
                    begin
                      if TWinControl(aList[i]).CanFocus and TWinControl(aList[i]).TabStop then
                      begin
                        ParentForm.ActiveControl := TWinControl(aList[i]);
                        break;
                      end;
                    end;
                  finally
                    aList.Free;
                  end;
                end;
              end;
            end;
          end;
        end;
      end
      else if Items.ItemIndex < 0 then
      begin
        Items.ItemIndex := Index;
        Invalidate;
      end;

      if ((OldIndex <> -1) and (OldIndex < FPageList.Count)) then
      begin
        if ((Index <> -1) and (Index < FPageList.Count)) then
          DoPageChanged(FPageList[OldIndex].Control, FPageList[Index].Control)
        else
          DoPageChanged(FPageList[OldIndex].Control, nil)
      end
      else
      begin
        if ((Index <> -1) and (Index < FPageList.Count)) then
          DoPageChanged(nil, FPageList[Index].Control)
        else
          DoPageChanged(nil, nil)
      end;


      {if ((OldIndex <> -1) and (OldIndex < FPageList.Count)) and ((Index <> -1) and (Index < FPageList.Count)) then
        DoPageChanged(FPageList[OldIndex].Control, FPageList[Index].Control)
      else if (OldIndex <> -1) and (OldIndex < FPageList.Count) then
        DoPageChanged(FPageList[OldIndex].Control, nil)
      else
        DoPageChanged(nil, nil);}
    end
  end;
end;

procedure TntvPageControl.NextPageBtnClick;
var
  R: TRect;
begin
  R := GetTabRect(FPageList.Count - 1);
  if UseRightToLeftAlignment then
  begin
    if R.Left < ControlTabWidth then
      TopIndex := TopIndex + 1;
  end
  else
  begin
    if R.Right > (ClientWidth - ControlTabWidth) then
      TopIndex := TopIndex + 1;
  end;
end;

procedure TntvPageControl.PriorPageBtnClick;
begin
  if TopIndex > 0 then
    TopIndex := TopIndex - 1;
end;

function TntvPageControl.GetInnerRect: TRect;
begin
  Result := ClientRect;
  if ShowTabs then
    Inc(Result.Top, HeaderHeight);
end;

procedure TntvPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent <> Self) and (AComponent.Owner <> Self)
    and (AComponent is TWinControl) and (Items.FindControl((AComponent as TWinControl)) <> nil) then
  begin
    Items.ExtractControl(AComponent as TWinControl);
  end;
end;

function TntvPageControl.GetPageIndex: Integer;
begin
  Result := Items.ItemIndex;
end;

procedure TntvPageControl.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
  begin
    if not (csLoading in ComponentState) then
      if Inserting and (Control is TWinControl) and not (Control is TntvPage) then
      begin
        Items.AddControl(Control as TWinControl);
        Result := 1;
      end
  end;
end;

procedure TntvPageControl.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

function TntvPageControl.ChildKey(var Message: TLMKey): boolean;
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
        if PageIndex <> FPageList.Count - 1 then
          SelectPage(PageIndex + 1)
        else
          SelectPage(0);
      end
      else
      begin
        if PageIndex = 0 then
          SelectPage(FPageList.Count - 1)
        else
          SelectPage(PageIndex - 1);
      end;
      Result := True;
    end
    else
      Result := inherited;
  end
  else
    Result := inherited;
end;

procedure TntvPageControl.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Invalidate;
end;

{begin
  Result:=inherited ChildKey(Message);
end;}

procedure TntvPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  //if not (csDesigning in ComponentState) and (Focused or IsChild(Handle, Windows.GetFocus)) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  if not (csDesigning in ComponentState) and (Focused ) and (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    if GetKeyState(VK_SHIFT) >= 0 then
    begin
      //nextpage
      if PageIndex <> FPageList.Count - 1 then
        SelectPage(PageIndex + 1)
      else
        SelectPage(0);
    end
    else
    begin
      if PageIndex = 0 then
        SelectPage(FPageList.Count - 1)
      else
        SelectPage(PageIndex - 1);
    end;
    Message.Result := 1;
  end
  else
  begin
    inherited;
    case Message.CharCode of
      VK_NEXT: NextPage;
      VK_PRIOR: PriorPage;
    end;
  end;
end;


procedure TntvPageControl.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Pages', @ReadWrapper, @WriteWrapper, True);
end;

procedure TntvPageControl.ReadWrapper(Reader: TReader);
begin
  TPageWrappers(FWrapper).Clear;
  Reader.ReadValue;
  Reader.ReadCollection(TPageWrappers(FWrapper));
end;

function TntvPageControl.GetTopIndex: Integer;
begin
  Result := Items.TopIndex;
end;

function TntvPageControl.GetImageList: TImageList;
begin
    Result := Items.Images;
end;

procedure TntvPageControl.WriteWrapper(Writer: TWriter);
var
  i: Integer;
begin
  (FWrapper as TPageWrappers).Clear;
  for i := 0 to Items.Count - 1 do
    with (FWrapper as TPageWrappers).Add do
      Control := Items[i].Control;
  Writer.WriteCollection((FWrapper as TPageWrappers));
end;

procedure TntvPageControl.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TntvPageControl.NextPage;
begin
  PageIndex := PageIndex + 1;
end;

procedure TntvPageControl.PriorPage;
begin
  PageIndex := PageIndex - 1;
end;

procedure TntvPageControl.SetPageBorder(const Value: Integer);
begin
  if FPageBorder <> Value then
  begin
    FPageBorder := Value;
    Realign;
  end;
end;

function TntvPageControl.GetPageRect: TRect;
begin
  Result := ClientRect;
  if ShowTabs then
    Inc(Result.Top, HeaderHeight - 1);
  InflateRect(Result, -FPageBorder, -FPageBorder);
end;

procedure TntvPageControl.SetActivePage(const Value: TWinControl);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
  begin
    if FPageList[i].Control = Value then
    begin
      SelectPage(i, True);
      Break;
    end;
  end;
end;

function TntvPageControl.GetActivePage: TWinControl;
begin
  if (PageIndex >= 0) and (PageIndex < FPageList.Count) then
    Result := FPageList[PageIndex].Control
  else
    Result := nil;
end;

procedure TntvPageItem.Assign(Source: TPersistent);
begin
  if Source is TntvPageItem then
  begin
  end;
  inherited;
end;

constructor TntvPageItem.Create(vCollection: TCollection);
begin
  inherited;
  if (vCollection <> nil) then
  begin
    PageControl.UpdatePagesList; //belal  حدوث خطا عند حذف آخر صفحة
    if not (csLoading in PageControl.ComponentState) then
    begin
      Caption := Format('Control [%d]', [Index]);
      Name := Format(PageControl.Name + 'Control%d', [Index]);
      FControl := TntvPage.Create(PageControl.Owner);
//      (FControl as TntvPage).Item := Self;
      FControl.Name := Name;
      FControl.Parent := PageControl;
      FControl.Show;
    end;
    PageControl.UpdatePagesList;
  end;
end;

destructor TntvPageItem.Destroy;
begin
  inherited;
end;

function TntvPageItem.GetPageControl: TntvPageControl;
begin
  if Collection <> nil then
    Result := (Collection as TntvPages).FPageControl
  else
    Result := nil;
end;


procedure TntvPageItem.SetControl(const Value: TWinControl);
begin
  if (FControl <> Value) then
  begin
    FControl := Value;
    if Value <> nil then
      FControl.Align := alClient;
  end;
end;

procedure TntvPageControl.SetShowButtons(const Value: Boolean);
begin
  if FShowButtons <> Value then
  begin
    FShowButtons := Value;
    Invalidate;
  end;
end;

procedure TntvPageControl.DoPageChanged(OldPage, NewPage: TWinControl);
begin
  if (OldPage <> NewPage) and Assigned(FOnPageChanged) then
    FOnPageChanged(Self, OldPage, NewPage);
end;

procedure TntvPageControl.UpdatePagesList;
var
  i: Integer;
begin
  FPageList.Clear;
  for i := 0 to Items.Count - 1 do
    if (csDesigning in ComponentState) or (Items[i].Visible) then
      FPageList.Add(Items[i]);
end;

function TntvPageControl.GetPageItem(vControl: TWinControl): TntvPageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].Control = vControl then
    begin
      Result := Items[i];
      Break;
    end;

end;

procedure TntvPageControl.SetShowTabs(const Value: Boolean);
begin
  if FShowTabs <> Value then
  begin
    FShowTabs := Value;
    Realign;
    Invalidate;
  end;
end;

function TntvPageControl.GetHeaderHeight: Integer;
begin
  if ShowTabs then
    Result := FHeaderHeight
  else
    Result := 0;
end;

function TntvPageControl.PageFromIndex(Index: Integer): TWinControl;
begin
  if (Index < FPageList.Count) and (Index > -1) then
    Result := FPageList[Index].Control
  else
    Result := nil;
end;

procedure TntvPageControl.Clear;
begin
  FPageList.Clear;
  while Items.Count > 0 do
  begin
    Items[Items.Count - 1].Free;
  end;
end;

procedure TntvPageControl.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  message.Result := message.Result or DLGC_WANTARROWS; //or DLGC_WANTTAB or DLGC_WANTMESSAGE or DLGC_WANTALLKEYS;
end;

procedure TntvPageControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift = [] then
  begin
    case Key of
      VK_LEFT:
        begin
          if UseRightToLeftAlignment then
            NextPage
          else
            PriorPage;
        end;
      VK_RIGHT:
        begin
          if UseRightToLeftAlignment then
            PriorPage
          else
            NextPage;
        end;
    end;
  end;
end;

//<Ayman>

procedure TntvPageControl.SetTabStyle(const Value: TTabStyle);
begin
  if FTabStyle <> Value then
  begin
    FTabStyle := Value;
    Invalidate;
  end;
end;

procedure TntvPageControl.SetImageList(const Value: TImageList);
begin
  if (Items.Images <> Value) then
  begin
    Items.Images := Value;
    Items.CalcTabWidth;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TntvPageControl.DrawVertLines(vRect: TRect; Index: Integer);
begin
  if UseRightToLeftAlignment then
  begin
    if Index = PageIndex + 1 then
    begin
      Canvas.MoveTo(vRect.Left, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Left, vRect.Top);
    end
    else
    begin
      Canvas.MoveTo(vRect.Left + 1, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Left + 1, vRect.Top);
    end;
    if ((Index = 0)) then
    begin
      Canvas.MoveTo(vRect.Right - 2, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Right - 2, vRect.Top);
    end
  end
  else
  begin
    if Index in [PageIndex, PageIndex + 1] then
    begin
      Canvas.MoveTo(vRect.Left + 1, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Left + 1, vRect.Top);
    end
    else
    begin
      Canvas.MoveTo(vRect.Left, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Left, vRect.Top);
    end;
    if ((Index = Items.Count - 1)) then
    begin
      Canvas.MoveTo(vRect.Right - 2, vRect.Bottom - 3);
      Canvas.LineTo(vRect.Right - 2, vRect.Top);
    end
  end;
end;

procedure TntvPageControl.AdjustBtnRect(var vRect: TRect; Index: Integer);
begin
  OffsetRect(vRect, 1, 0);
  Inc(vRect.Left, 3);
  if ((UseRightToLeftAlignment and (Index = 0))
    or (not UseRightToLeftAlignment and (Index = Items.Count - 1))) then
    Dec(vRect.Right, 4)
  else
    Dec(vRect.Right, 2);
  Inc(vRect.Top, 1);
  Dec(vRect.Bottom, 2);
end;

procedure TntvPageControl.InternalDrawBorder;
var
   R: TRect;
begin
  with Canvas do
  begin
    R := GetPageRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, PageBorder, PageBorder);

    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect (R);

    Pen.Color := clBtnHighlight;
    MoveTo(R.Left, R.Bottom);
    LineTo(R.Left, R.Top);
    LineTo (R.Right-1, R.Top);
    Pen.Color := clBtnShadow;
    LineTo (R.Right-1, R.Bottom-1);
    LineTo (R.Left, R.Bottom-1);
  end;
end;

procedure TntvPageControl.InternalDrawRaisedBorder;
var
  BackGroundColor: TColor;
begin
  if Parent <> nil then
    BackGroundColor := Parent.Brush.Color
  else
    BackGroundColor := Color;

  with Canvas do
  begin
    if ShowTabs then
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(0, ClientHeight - 1);
      LineTo(0, HeaderHeight - 1);
      LineTo(ClientWidth - 1, HeaderHeight - 1);
      Pen.Color := clBtnShadow;
      LineTo(ClientWidth - 1, ClientHeight - 1);
      LineTo(0, ClientHeight - 1);
      Brush.Style := bsSolid;
      Brush.Color := BackGroundColor;
      FillRect(Rect(0, 0, ClientWidth, HeaderHeight - 1));
    end
    else
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(0, ClientHeight - 1);
      LineTo(0, 1);
      LineTo(ClientWidth - 1, 1);
      Pen.Color := clBtnShadow;
      LineTo(ClientWidth - 1, ClientHeight - 1);
      LineTo(0, ClientHeight - 1);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, ClientWidth, 1));
    end;
  end;
end;

class function TntvPageControl.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 200;
  Result.cy := 240;
end;

procedure TntvPageControl.WndProc(var TheMessage: TLMessage);
begin
  inherited;
end;

procedure TntvPageControl.DrawNormalHeader;
const
  FillColor: array[boolean] of TColor = (clDkGray, clBlack);
var
  Rect: TRect;
  b: boolean;
begin
  with Canvas do
  begin
    Rect := GetTabSetRect;
    Brush.Style := bsSolid;
    Brush.Color := Color;

    FillRect(Rect);
    if UseRightToLeftAlignment then
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Rect.Left, Rect.Bottom);
      LineTo(Rect.Left, Rect.Top + 2);
      LineTo(Rect.Right - 4, Rect.Top + 2);
      LineTo(Rect.Right - 1, Rect.Top + 5);
      LineTo(Rect.Right - 1, Rect.Bottom);
      Pen.Color := Color;

      b := GetTabRect(FPageList.Count - 1).Left < (ControlTabWidth);
      Brush.Color := FillColor[b];
      Polygon([Point(15, 5), Point(15, 15), Point(5, 10)]);
      b := TopIndex > 0;
      Brush.Color := FillColor[b];
      Polygon([Point(20, 5), Point(20, 15), Point(30, 10)]);
    end
    else
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Rect.Left, Rect.Bottom-1);
      LineTo(Rect.Left, Rect.Top + 5);
      LineTo(Rect.Left + 3, Rect.Top + 2);
      LineTo(Rect.Right - 1, Rect.Top + 2);
      Pen.Color := clBtnShadow;
      LineTo(Rect.Right - 1, Rect.Bottom);
      Pen.Color := Color;

      b := GetTabRect(FPageList.Count - 1).Right > (ClientWidth - ControlTabWidth);
      Brush.Color := FillColor[b];
      Polygon([Point(ClientWidth - 5, 10), Point(ClientWidth - 15, 15), Point(ClientWidth - 15, 5)]);
      b := TopIndex > 0;
      Brush.Color := FillColor[b];
      Polygon([Point(ClientWidth - 30, 10), Point(ClientWidth - 20, 15), Point(ClientWidth - 20, 5)]);
    end;
  end;
end;

procedure TntvPageControl.DrawNormalTab(Index: Integer; var Rect: TRect);
var
  R, aTextRect: TRect;
  aTextStyle: TTextStyle;
begin
  with Canvas do
  begin
    aTextStyle := TextStyle;
    if (Index < 0) or (Index >= FPageList.Count) then
      Exit; //belal
    if PageIndex <> Index then
    begin
      Dec(Rect.Bottom);
    end
    else if Index > TopIndex then
      InflateRect(Rect, 1, 0)
    else
    begin
      if UseRightToLeftAlignment then
        Dec(Rect.Left)
      else
        Inc(Rect.Right);
    end;
    R := Rect;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Inc(R.Bottom, 2);
    FillRect(R);
    Inc(R.Top, 2);
    if Index <> Items.ItemIndex then
    begin
      Inc(R.Top, 2);
    end;
    Brush.Color := Color;
    Pen.Color := clBtnHighlight;
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Top);

    LineTo(R.Left + 1, R.Top - 1);
    LineTo(R.Right - 2, R.Top - 1);
    LineTo(R.Right - 1, R.Top);

    Pen.Color := clGray;
    LineTo(R.Right - 1, R.Bottom);

    aTextRect := R;
    if Index = Items.ItemIndex then
    begin
      MoveTo(R.Right - 2, R.Bottom);
      Pen.Color := Color;
      LineTo(R.Left, R.Bottom);
      InflateRect(R, -3, -1);
      if Focused then
        DrawFocusRect(R);
      OffsetRect(aTextRect, 0, -2);
    end;
    Brush.Style := bsClear;
    Canvas.Font.Assign(Self.Font);

    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    if UseRightToLeftAlignment then
       aTextStyle.RightToLeft := True;
    TextRect(aTextRect, 0, 0, FPageList[Index].Caption, aTextStyle);

  end;
end;


procedure TntvPageControl.DrawRTLHeaderBorder(vRect: TRect;
  vColor: TColor; const Brd3D: Boolean = True);
begin
  Canvas.Pen.Color := vColor;
  Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
  Canvas.LineTo(vRect.Left, vRect.Top + 2);
  Canvas.LineTo(vRect.Right - 4, vRect.Top + 2);
  Canvas.LineTo(vRect.Right - 1, vRect.Top + 5);
  Canvas.LineTo(vRect.Right - 1, vRect.Bottom - 1);

  if Brd3D then
  begin
    Canvas.Pen.Color := MixColors(Color, clBtnHighlight, nMixFlag); ;
    Canvas.MoveTo(vRect.Left + 1, vRect.Bottom - 1);
    Canvas.LineTo(vRect.Left + 1, vRect.Top + 3);
    Canvas.LineTo(vRect.Right - 4, vRect.Top + 3);
    Canvas.LineTo(vRect.Right - 1, vRect.Top + 6);
  //Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  end;
end;

procedure TntvPageControl.DrawLTRHeaderBorder(vRect: TRect;
  vColor: TColor; const Brd3D: Boolean = True);
begin
  Canvas.Pen.Color := vColor;
  Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
  Canvas.LineTo(vRect.Left, vRect.Top + 5);
  Canvas.LineTo(vRect.Left + 3, vRect.Top + 2);
  Canvas.LineTo(vRect.Right - 2, vRect.Top + 2);
  Canvas.LineTo(vRect.Right - 2, vRect.Bottom - 1);

  if Brd3D then
  begin
    Canvas.Pen.Color := MixColors(Color, clBtnHighlight, nMixFlag); ;
    Canvas.MoveTo(vRect.Right - 1, vRect.Top + 4);
    Canvas.LineTo(vRect.Right - 1, vRect.Bottom - 1);


    Canvas.MoveTo(vRect.Left + 1, vRect.Bottom - 1);
    Canvas.LineTo(vRect.Left + 1, vRect.Top + 5);
    Canvas.LineTo(vRect.Left + 3, vRect.Top + 3);
    Canvas.LineTo(vRect.Right - 2, vRect.Top + 3);
  //Canvas.LineTo(vRect.Right - 2, vRect.Bottom - 1);
  end;

end;

{ TntvPages }

function TntvPages.Add: TntvPageItem;
begin
  Result := TntvPageItem(inherited Add);
end;

function TntvPages.AddControl(vControl: TWinControl): TntvPageItem;
var
  Buffer: PChar;
  Size: Byte;
begin
  if vControl <> nil then
  begin
    BeginUpdate;
    try
      Result := TntvPageItem.Create(nil);
      with Result do
      begin
        Control := vControl;
        Control.Name := Control.Name;
        Size := Control.GetTextLen; {Get length of string in Edit1}
        Inc(Size); {Add room for null character}
        GetMem(Buffer, Size); {Creates Buffer dynamic variable}
        try
          Control.GetTextBuf(Buffer, Size); {Puts Edit1.Text into Buffer}
          Caption := StrPas(Buffer); {Converts Buffer to a Pascal-style string}
        finally
          FreeMem(Buffer, Size); {Frees memory allocated to Buffer}
        end;
        Control.Parent := FPageControl;
        //LclType.SetParent(Control.Handle, FPageControl.Handle);
        Control.FreeNotification(FPageControl);
        Collection := Self;
        Control.Align := alClient;
        FPageControl.UpdatePagesList;
        Control.Show; //zaher
      end;
    finally
      EndUpdate;
    end;
  end
  else
    Result := nil;
end;

procedure TntvPages.CalcTabWidth;
var
  TmpCanvas: TCanvas;
  i: Integer;
begin
  if not (csLoading in FPageControl.ComponentState) then
  begin
    TmpCanvas := TCanvas.Create;
    try
      TmpCanvas.Handle := GetDC(0);
      TmpCanvas.Font.Assign(FPageControl.Font);
      for i := 0 to Count - 1 do
      begin
        Items[i].Width := TmpCanvas.TextWidth(Items[i].Caption) + 20;
        if (FPageControl.ImageList <> nil) and (Items[I].ImageIndex > -1) then
          Items[i].Width := Items[i].Width + FPageControl.ImageList.Width - 8;
      end;
    finally
      ReleaseDC(0, TmpCanvas.Handle);
      TmpCanvas.Free;
    end;
    FPageControl.Invalidate;
  end;
end;

constructor TntvPages.Create(APageControl: TntvPageControl);
begin
  inherited Create(TntvPageItem);
  FPageControl := APageControl;
end;

destructor TntvPages.Destroy;
begin
  inherited;
end;

function TntvPages.ExtractControl(vControl: TWinControl): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Control = vControl then
    begin
      Result := Items[i].Control;
      Items[i].Control := nil;
      Delete(i);
      if not (csDestroying in FPageControl.ComponentState) then
      begin
        FPageControl.UpdatePagesList;
        FPageControl.ShowPage(i - 1)
      end;
      Break;
    end;
end;

function TntvPages.FindControl(vControl: TWinControl): TntvPageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Control = vControl then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TntvPages.GetItem(Index: Integer): TntvPageItem;
begin
  Result := (inherited GetItem(Index) as TntvPageItem);
end;

function TntvPages.GetOwner: TPersistent;
begin
  Result := FPageControl;
end;

procedure TntvPages.SetItem(Index: Integer; Value: TntvPageItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TntvPages.Update(Item: TCollectionItem);
begin
  inherited;
  if (FPageControl <> nil) and not (csLoading in FPageControl.ComponentState) then
  begin
    CalcTabWidth;
    //FPageControl.ShowPage(FPageControl.PageIndex,True);
    FPageControl.UpdatePagesList;
    FPageControl.Invalidate;
  end;
end;

{ TPageWrappers }

function TPageWrappers.Add: TPageWrapperItem;
begin
  Result := TPageWrapperItem(inherited Add);
end;

constructor TPageWrappers.Create(APageControl: TntvPageControl);
begin
  inherited Create(TPageWrapperItem);
  FPageControl := APageControl;
end;

function TPageWrappers.GetItem(Index: Integer): TPageWrapperItem;
begin
  Result := TPageWrapperItem(inherited GetItem(Index));
end;

procedure TPageWrappers.SetItem(Index: Integer; Value: TPageWrapperItem);
begin
  inherited SetItem(Index, Value);
end;

{ TPageWrapperItem }

constructor TPageWrapperItem.Create(vCollection: TCollection);
begin
  inherited;

end;

destructor TPageWrapperItem.Destroy;
begin
  FControl := nil;
  inherited;
end;

procedure TntvPageItem.SetVisible(const Value: Boolean);
begin
end;

{ TPagesList }

function TPagesList.GetItem(Index: Integer): TntvPageItem;
begin
  Result := TntvPageItem(inherited GetItem(Index));
end;

procedure TPagesList.SetItem(Index: Integer; const Value: TntvPageItem);
begin
  inherited SetItem(Index, Value)
end;

procedure TntvPageItem.SetIndex(Value: Integer);
var
  ShowSelf: Boolean;
begin
  if PageControl <> nil then
    ShowSelf := PageControl.PageIndex = Index
  else
    ShowSelf := False;

  inherited;

  if PageControl <> nil then
  begin
    PageControl.UpdatePagesList;
    if ShowSelf then
    begin
      PageControl.ShowPage(Index, True);
    end
    else
      PageControl.ShowPage(PageControl.PageIndex, True);
  end;
end;

procedure TntvPageItem.Update;
begin

end;

initialization
  RegisterClasses([TntvPage]);
end.

