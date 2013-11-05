unit ntvTabs;
{$mode objfpc}{$H+}
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
  ntvTabs object to manage and draw tab controls, it is not a control, it can be used inside another controls.
}
interface

uses
  Classes, SysUtils, Graphics, Controls, Contnrs, Types,
  LCLType, LCLIntf, LCLProc;

const
  cMinTabWidth = 10;
  cHeaderHeightMargin = 5;
  cImageMargin = 3;
  cTextMargin = 4;

type
  TntvhtTabHitTest = (htNone, htTab, htNext, htPrior, htClose);
  TntvFlag = (tbfFocused, tbfRightToLeft);
  TntvFlags = set of TntvFlag;

  TntvTabPosition = (tpTop, tpBottom{, tpLeft, tpRight});

  TntvTabItem = class;

  {
    tdsActive: Current Tab
    tdsAfter: It is after the current tab
    tdsBefore: It is after the current tab
    tdsNear: near to current tab
    tdsFirst: First tab Index = 0
    tdsLast: Last tab Index = Count -1
  }
  TTabDrawState = (tdsActive, tdsAfter, tdsBefore, tdsNear,tdsFirst, tdsLast);
  TTabDrawStates = set of TTabDrawState;

  { TntvTabDraw }

  TntvTabDraw = class(TObject)
  public
    function GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer; virtual; abstract;
    procedure PaintText(vItem: TntvTabItem; Canvas:TCanvas; vRect: TRect; vPosition: TntvTabPosition; State: TTabDrawStates ; vFlags: TntvFlags); virtual;
    procedure Paint(vItem: TntvTabItem; Canvas:TCanvas; vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags);
    {
      Paint the tab without the text area, and return in vRect the rect of text area
    }
    procedure DoPaint(vItem: TntvTabItem; Canvas:TCanvas; var vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags); virtual; abstract;
  end;

  { TntvTabDrawSheet }

  TntvTabDrawSheet = class(TntvTabDraw)
  public
    function GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer; override;
    procedure DoPaint(vItem: TntvTabItem; Canvas: TCanvas; var vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags); override;
  end;

  { TntvTabDrawCart }

  TntvTabDrawCart = class(TntvTabDraw)
  public
    function GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer; override;
    procedure DoPaint(vItem: TntvTabItem; Canvas: TCanvas; var vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags); override;
  end;

  { TntvTabItem }

  TntvTabItem = class(TCollectionItem)
  private
    FName: string;
    FCaption: string;
    FWidth: Integer;
    FImageIndex: Integer;
    FAutoWidth: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    procedure SetAutoWidth(const AValue: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const AValue: boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetImageIndex(const AValue: Integer);
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Update;
    procedure Invalidate;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //Caption width
    property Width: Integer read FWidth write SetWidth;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Name: string read GetName write SetName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TntvTabItemClass = class of TntvTabItem;

  { TntvTabList }

  TntvTabList = class(TObjectList)
  private
    function GetItem(Index: Integer): TntvTabItem;
    procedure SetItem(Index: Integer; const Value: TntvTabItem);
  public
    property Items[Index: Integer]: TntvTabItem read GetItem write SetItem; default;
  end;

  { TntvTabs }

  TntvTabs = class(TCollection)
  private
    FImages: TImageList;
    FItemIndex: Integer;
    FPosition: TntvTabPosition;
    FShowAll: Boolean;
    FShowButtons: Boolean;
    FTopIndex: Integer;
    FVisibles: TntvTabList;
    FUpdateItems: Boolean;
    procedure SetImages(const AValue: TImageList);
  protected
    function GetItem(Index: Integer): TntvTabItem;
    procedure SetItem(Index: Integer; Value: TntvTabItem);
    procedure Update(Item: TCollectionItem); override;
    procedure DoShowTab(Item:TntvTabItem); virtual;
    procedure UpdateCanvas(vCanvas: TCanvas); virtual;
    procedure VisibleChanged;
    procedure Invalidate; virtual;
    function IndexToState(Index: Integer): TTabDrawStates;
    function CreateTabDraw: TntvTabDraw; virtual;
    procedure DrawButtons(Canvas: TCanvas; var vRect: TRect; vFlags: TntvFlags); //TODO
    procedure DrawTab(Canvas: TCanvas; Index: Integer; vRect: TRect; vFlags: TntvFlags);
    //ShowAll for DesignMode or special states, visible and non visible tab
    property ShowAll: Boolean read FShowAll write FShowAll;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TntvTabItem;
    function AddItem(vName, vCaption: string): TntvTabItem;
    //Control functions
    function HitTest(vCanvas: TCanvas; vPoint: TPoint; vRect:TRect; out vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
    procedure Paint(Canvas: TCanvas; vRect: TRect; vFlags: TntvFlags = []);
    function ShowTab(Canvas: TCanvas; const vRect:TRect; Index: Integer; vFlags: TntvFlags): Boolean;

    //Check function
    procedure UpdateItems(vCanvas: TCanvas); //call it before use next functions direclty
    function GetTabRect(const vTabsRect: TRect; TopIndex, Index: Integer; out vTabRect: TRect; vFlags: TntvFlags): Boolean; overload;
    function GetTabRect(const vTabsRect: TRect; Index: Integer; out vTabRect: TRect; vFlags: TntvFlags): Boolean; overload; virtual;
    function GetTabOffset(Index: Integer): Integer;

    //Items
    property Items[Index: Integer]: TntvTabItem read GetItem write SetItem stored False; default;
    //Properites
    property Visibles: TntvTabList read FVisibles write FVisibles;
    property Images: TImageList read FImages write SetImages;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property TopIndex: Integer read FTopIndex write FTopIndex;
    property ShowButtons: Boolean read FShowButtons write FShowButtons;
    property Position: TntvTabPosition read FPosition write FPosition;
  published
  end;

implementation

{ TntvTabDraw }

procedure TntvTabDraw.PaintText(vItem: TntvTabItem; Canvas: TCanvas; vRect: TRect; vPosition: TntvTabPosition; State: TTabDrawStates; vFlags: TntvFlags);
var
  aTextStyle: TTextStyle;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    if tbfRightToLeft in vFlags then
       aTextStyle.RightToLeft := True;
    Font.Color := clBlack;
    InflateRect(vRect, -1, -1);
    TextRect(vRect, 0, 0, vItem.Caption, aTextStyle);

    if tdsActive in State then
    begin
      if tbfFocused in vFlags then
      begin
        InflateRect(vRect, 1, 1);
        DrawFocusRect(vRect);
      end;
    end;
  end;
end;

procedure TntvTabDraw.Paint(vItem: TntvTabItem; Canvas: TCanvas; vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags);
begin
  DoPaint(vItem, Canvas, vRect, vPosition, vState, vFlags);
  PaintText(vItem, Canvas, vRect, vPosition, vState, vFlags);
end;

{ TntvTabDrawCart }

function TntvTabDrawCart.GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer;
var
  m, mw: Integer;
begin
  m := 2;
  mw := (vTabsRect.Bottom - vTabsRect.Top) div 4;
  Result := Width + mw + m * 2; //margin of text
  if (tdsLast in State) then
    Result := Result + mw;
end;

procedure TntvTabDrawCart.DoPaint(vItem: TntvTabItem; Canvas: TCanvas; var vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags);
var
  aTextRect: TRect;
  aTextStyle: TTextStyle;
  m: Integer;
  mw: Integer;
  a: array[0..3] of TPoint;
  procedure DrawPrevious;
  begin
    with Canvas do
    begin
      if (tdsNear in vState) and (tdsAfter in vState) then
        Brush.Color := clBtnFace
      else
        Brush.Color := clWhite;
      a[0] := Point(vRect.Left, vRect.Top);
      a[1] := Point(vRect.Left + mw, vRect.Bottom);
      a[2] := Point(vRect.Left, vRect.Bottom);
      Brush.Style := bsSolid;
      Pen.Style := psClear;
      Polygon(a, 3);
      Pen.Style := psSolid;
      Polyline(a, 0, 2);
    end;
  end;
begin
  m := 2;
  mw := (vRect.Bottom - vRect.Top) div 4;

  with Canvas do
  begin
    aTextRect := vRect;
    aTextRect.Left := aTextRect.Left + mw;
    if (tdsLast in vState) then
    aTextRect.Right := aTextRect.Right + mw;
    InflateRect(aTextRect, -m, -m);

    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    if not (tdsFirst in vState) then
    begin
      if (tdsActive in vState) then
      begin
        DrawPrevious
      end;
    end;

    if tdsActive in vState then
      Brush.Color := clBtnFace
    else
      Brush.Color := clWhite;
    Pen.Color := clBlack;
    Pen.Style := psClear;
    a[0] := Point(vRect.Left, vRect.Bottom);
    a[1] := Point(vRect.Left + mw, vRect.Top);
    if (tdsLast in vState) then
      a[2] := Point(vRect.Right - mw + 1, vRect.Top)
    else
      a[2] := Point(vRect.Right + 1, vRect.Top);
    a[3] := Point(vRect.Right + 1, vRect.Bottom);
    Polygon(a, 4);
    Pen.Style := psSolid;
    if (tdsLast in vState) then
      Polyline(a, 0, 4)
    else
      Polyline(a, 0, 3);

    if not (tdsFirst in vState) then
    begin
      if not (tdsActive in vState) then
      begin
        DrawPrevious;
      end;
    end;
  end;
  vRect := aTextRect;
end;

{ TntvTabItem }

procedure TntvTabItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Update;
    Invalidate;
  end;
end;

procedure TntvTabItem.SetAutoWidth(const AValue: Boolean);
begin
  if FAutoWidth =AValue then exit;
  FAutoWidth :=AValue;
  Update;
  Invalidate;
end;

procedure TntvTabItem.SetEnabled(const AValue: boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Invalidate;
  end;
end;

procedure TntvTabItem.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    Update;
    Invalidate;
  end;
end;

procedure TntvTabItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex = AValue then exit;
  FImageIndex :=AValue;
  Update;
  Invalidate;
end;

function TntvTabItem.GetName: string;
begin
  Result := FName;
end;

procedure TntvTabItem.SetName(const Value: string);
begin
  FName := Value;
  DisplayName:=Value;
end;

procedure TntvTabItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

function TntvTabItem.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TntvTabItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  Update;
end;

procedure TntvTabItem.Update;
begin
  if Collection <> nil then
    (Collection as TntvTabs).Update(Self);
end;

procedure TntvTabItem.Invalidate;
begin
  if Collection <> nil then
    (Collection as TntvTabs).Invalidate;
end;

constructor TntvTabItem.Create(vCollection: TCollection);
begin
  inherited Create(vCollection);
  FAutoWidth := True;
  FVisible := True;
  FEnabled := True;
  FImageIndex := -1;
end;

destructor TntvTabItem.Destroy;
begin
  inherited Destroy;
end;

procedure TntvTabItem.Assign(Source: TPersistent);
begin
  if Source is TntvTabItem then
  begin
    FCaption := TntvTabItem(Source).FCaption;
    FName := TntvTabItem(Source).FName;
    FImageIndex := TntvTabItem(Source).FImageIndex;
    FEnabled := TntvTabItem(Source).FEnabled;
    FVisible := TntvTabItem(Source).FVisible;
    Update;
  end
  else
    inherited;
end;

{ TntvTabs }

procedure TntvTabs.SetItem(Index: Integer; Value: TntvTabItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TntvTabs.SetImages(const AValue: TImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;

  end;
end;

function TntvTabs.GetItem(Index: Integer): TntvTabItem;
begin
  Result := (inherited GetItem(Index) as TntvTabItem);
end;

procedure TntvTabs.Update(Item: TCollectionItem);
begin
  inherited;
  FUpdateItems := True;
  if Item = nil then
  begin
    VisibleChanged;
    Invalidate;
  end
  else
    Invalidate;
end;

procedure TntvTabs.DoShowTab(Item: TntvTabItem);
begin

end;

procedure TntvTabs.UpdateCanvas(vCanvas: TCanvas);
begin
end;

procedure TntvTabs.UpdateItems(vCanvas: TCanvas);
var
  i, w: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[I].AutoWidth then
    begin
      w := vCanvas.TextWidth(Items[i].Caption) + cTextMargin;
      if (Images <> nil) and (Items[I].ImageIndex > -1) then
        w := w + Images.Width - cImageMargin;
      if w < cMinTabWidth then
        w := cMinTabWidth;
      Items[i].FWidth := w;
    end;
  FUpdateItems := False;
end;

procedure TntvTabs.VisibleChanged;
var
  i: Integer;
begin
  if FVisibles <> nil then
  begin
    FVisibles.Clear;
    for i := 0 to Count -1 do
    begin
      if ShowAll or Items[i].Visible then
        FVisibles.Add(Items[i]);
    end;
  end;
end;

procedure TntvTabs.Invalidate;
begin
end;

function TntvTabs.IndexToState(Index: Integer): TTabDrawStates;
begin
 Result := [];
 if (Index = ItemIndex) then
   Result := Result + [tdsActive]
 else if Index > ItemIndex then
   Result := Result + [tdsAfter]
 else if Index < ItemIndex then
   Result := Result + [tdsBefore];

 if abs(Index - ItemIndex) = 1 then
   Result := Result + [tdsNear];

  if (Index - TopIndex) = 0 then
    Result := Result + [tdsFirst];
  if Index = Visibles.Count - 1 then //need to review
    Result := Result + [tdsLast];
end;

function TntvTabs.CreateTabDraw: TntvTabDraw;
begin
  Result := TntvTabDrawCart.Create;
end;

constructor TntvTabs.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FVisibles := TntvTabList.create(False);
  FUpdateItems := True;
end;

destructor TntvTabs.Destroy;
begin
  FreeAndNil(FVisibles);
  inherited;
end;

function TntvTabs.Add: TntvTabItem;
begin
  Result := inherited Add as TntvTabItem;
end;

function TntvTabs.AddItem(vName, vCaption: string): TntvTabItem;
begin
  BeginUpdate;
  try
    Result := Add;
    Result.FName := vName;
    Result.FCaption := vCaption;
  finally
    EndUpdate;
  end;
end;

function TntvTabs.HitTest(vCanvas: TCanvas; vPoint: TPoint; vRect:TRect; out vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
var
  i: Integer;
  R: TRect;
begin
  if FUpdateItems then
    UpdateItems(vCanvas);
  Result := htNone;
  vIndex := -1;
  for i := TopIndex to Visibles.Count - 1 do
  begin
    if GetTabRect(vRect, i, R, vFlags) and PtInRect(R, vPoint) then
    begin
      Result := htTab;
      vIndex := i;
      break;
    end;
  end;
end;

procedure TntvTabs.Paint(Canvas: TCanvas; vRect: TRect; vFlags: TntvFlags);
var
  aRect: TRect;
  i: Integer;
begin
  if FUpdateItems then
    UpdateItems(Canvas);
  for i := TopIndex to Visibles.Count - 1 do
  begin
    if GetTabRect(vRect, i, aRect, vFlags) then
      DrawTab(Canvas, i, aRect, vFlags);
    if tbfRightToLeft in vFlags then
    begin
      if aRect.Left < vRect.Left then
        Break;
    end
    else
    begin
      if aRect.Right > vRect.Right then
        Break;
    end;
  end;
end;

function TntvTabs.GetTabRect(const vTabsRect: TRect; TopIndex, Index: Integer; out vTabRect: TRect; vFlags: TntvFlags): Boolean;
var
  R: Trect;
  w, i, x: Integer;
  TabDraw: TntvTabDraw;
  function GetW(i: Integer): Integer;
  begin
    Result := TabDraw.GetWidth(IndexToState(i), vTabsRect, FVisibles[i].Width);
  end;
begin
  TabDraw := CreateTabDraw;
  try
    if FUpdateItems then
      raise Exception.Create('You can not GetTabRect directly after changed');
    vTabRect := Rect(0, 0, 0, 0);
    if Index < TopIndex then
      exit; //nothing to do :(
    Result := False;
    if (Index < FVisibles.Count) and (Index > -1) then
    begin
      w := GetW(Index);
      x := 0;
      for i := TopIndex to Index - 1 do
        x := x + GetW(i);
      R := Rect(0, vTabsRect.Top, 0, vTabsRect.Bottom);
      if tbfRightToLeft in vFlags then
      begin
        R.Right := vTabsRect.Right - x - 1;
        R.Left := R.Right - w;
      end
      else
      begin
        R.Left := vTabsRect.Left + x;
        R.Right := R.Left + w - 1;
      end;
      vTabRect := R;
      Result := True;
    end;
  finally
    FreeAndNil(TabDraw)
  end;
end;

procedure TntvTabs.DrawButtons(Canvas: TCanvas; var vRect: TRect; vFlags: TntvFlags);
const
  FillColor: array[boolean] of TColor = (clDkGray, clBlack);
var
  aRect: TRect;
begin
  with Canvas do
  begin
  end;
end;

procedure TntvTabs.DrawTab(Canvas: TCanvas; Index: Integer; vRect: TRect; vFlags: TntvFlags);
var
  TabDraw: TntvTabDraw;
begin
  TabDraw := CreateTabDraw;
  try
    TabDraw.Paint(Visibles[Index], Canvas, vRect, Position, IndexToState(Index), vFlags);
  finally
    FreeAndNil(TabDraw)
  end;
end;

function TntvTabs.GetTabRect(const vTabsRect: TRect; Index: Integer; out vTabRect: TRect; vFlags: TntvFlags): Boolean;
begin
  Result := GetTabRect(vTabsRect, TopIndex, Index, vTabRect, vFlags);
end;

function TntvTabs.GetTabOffset(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Index - 1 downto TopIndex do
    Result := Result + FVisibles[index].Width;
end;

function TntvTabs.ShowTab(Canvas: TCanvas; const vRect:TRect; Index: Integer; vFlags: TntvFlags): Boolean;
var
  R: TRect;
  aTopIndex: Integer;
begin
  Result := True;//must be false if can not show the Tab
  if FUpdateItems then
    UpdateItems(Canvas);
  GetTabRect(vRect, ItemIndex, R, vFlags);
  aTopIndex := TopIndex;
  if tbfRightToLeft in vFlags then
  begin
    if R.Left < vRect.Left then
    begin
      while (R.Left < vRect.Left) and (aTopIndex < ItemIndex) do
      begin
        aTopIndex := aTopIndex + 1;
        GetTabRect(vRect, aTopIndex, ItemIndex, R, vFlags);
      end;
    end;
  end
  else
  begin
    if R.Right > vRect.Right then
    begin
      while (R.Right > vRect.Right) and (aTopIndex < ItemIndex) do
      begin
        aTopIndex := aTopIndex + 1;
        GetTabRect(vRect, aTopIndex, ItemIndex, R, vFlags);
      end;
    end;
  end;
  TopIndex := aTopIndex;
end;

{ TntvTabList }

function TntvTabList.GetItem(Index: Integer): TntvTabItem;
begin
  Result := TntvTabItem(inherited GetItem(Index));
end;

procedure TntvTabList.SetItem(Index: Integer; const Value: TntvTabItem);
begin
  inherited SetItem(Index, Value)
end;

{ TntvTabDrawSheet }

function TntvTabDrawSheet.GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer;
var
  w: Integer;
begin
  w := vTabsRect.Bottom - vTabsRect.Top;
  Result := Width + w; //margin
{  if tdsFirst in State then
    Result := Result + w;}
end;

procedure TntvTabDrawSheet.DoPaint(vItem: TntvTabItem; Canvas: TCanvas; var vRect: TRect; vPosition: TntvTabPosition; vState: TTabDrawStates; vFlags: TntvFlags);
var
  aTextRect: TRect;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    if tdsActive in vState then
      Brush.Color := clBtnFace
    else
      Brush.Color := clWhite;
    FillRect(vRect);

    aTextRect := vRect;
    InflateRect(aTextRect, -2, -2);
{    if not (tdsFirst in State) then
    begin
      if tbfRightToLeft in vFlags then
        Dec(aTextRect.Left)
      else
        Inc(aTextRect.Right);
    end;}

{    Brush.Color := clRed;
    FillRect(aTextRect);}

    //PaintText(aTextRect);
    //Dec(aTextRect.Right);

    Pen.Style := psSolid;
    Pen.Color := clDkGray;
    if vPosition = tpTop then
    begin
      if tbfRightToLeft in vFlags then
      begin
        MoveTo(vRect.Left, vRect.Bottom);
        LineTo(vRect.Left, vRect.Top);
        LineTo(vRect.Right + 1, vRect.Top);
        if (tdsFirst in vState) then
        begin
          MoveTo(vRect.Right, vRect.Bottom);
          LineTo(vRect.Right, vRect.Top);
        end;
      end
      else
      begin
        MoveTo(vRect.Right, vRect.Bottom);
        LineTo(vRect.Right, vRect.Top);
        LineTo(vRect.Left - 1, vRect.Top);
        if (tdsFirst in vState) then
        begin
          MoveTo(vRect.Left, vRect.Bottom);
          LineTo(vRect.Left, vRect.Top);
        end;
      end;
    end
    else if vPosition = tpBottom then
    begin
      if tbfRightToLeft in vFlags then
      begin
        MoveTo(vRect.Left, vRect.Top);
        LineTo(vRect.Left, vRect.Bottom);
        LineTo(vRect.Right + 1, vRect.Bottom);
        if (tdsFirst in vState) then
        begin
          MoveTo(vRect.Right, vRect.Top);
          LineTo(vRect.Right, vRect.Bottom);
        end;
      end
      else
      begin
        MoveTo(vRect.Right, vRect.Top);
        LineTo(vRect.Right, vRect.Bottom);
        LineTo(vRect.Left - 1, vRect.Bottom);
        if (tdsFirst in vState) then
        begin
          MoveTo(vRect.Left, vRect.Top);
          LineTo(vRect.Left, vRect.Bottom);
        end;
      end;
    end;
  end;
  vRect := aTextRect;
end;

end.

