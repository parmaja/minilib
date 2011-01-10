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
  LMessages, LCLType, LCLIntf, LCLProc;

const
  cMinTabWidth = 10;
  cHeaderHeightMargin = 3;
  cImageMargin = 3;

type
  TntvhtTabHitTest = (htNone, htTab, htNext, htPrior, htClose);
  TntvFlag = (tbfFocused, tbfRightToLeft);
  TntvFlags = set of TntvFlag;

  TntvTabItem = class;

  TTabDrawState = (tdsFirst, tdsNormal, tdsActive, tdsLast);
  TTabDrawStates = set of TTabDrawState;

  TntvTabDraw = class(TObject)
  public
    function GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer; virtual; abstract;
    function Paint(vItem: TntvTabItem; State: TTabDrawStates ; vRect:TRect; Canvas:TCanvas; vFlags: TntvFlags): Boolean; virtual; abstract;
  end;

  { TTabDrawSheet }

  TTabDrawSheet = class(TntvTabDraw)
  public
    function GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer; override;
    function Paint(vItem: TntvTabItem; State: TTabDrawStates; vRect:TRect; Canvas:TCanvas; vFlags: TntvFlags): Boolean; override;
  end;

  { TntvTabItem }

  TntvTabItem = class(TCollectionItem)
  private
    FAutoWidth: Boolean;
    FCaption: string;
    FWidth: Integer;
    FImageIndex: Integer;
    FName: string;
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
    property Enabled: Boolean read FEnabled write SetEnabled;
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
    function CreateTabDraw: TntvTabDraw;
    procedure DrawButtons(Canvas: TCanvas; var vRect: TRect; vFlags: TntvFlags);
    procedure DrawTab(Canvas: TCanvas; Index: Integer; vRect: TRect; vFlags: TntvFlags);
    //ShowAll for DesignMode or special states, visible and non visible tab
    property ShowAll: Boolean read FShowAll write FShowAll;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TntvTabItem;
    //Control functions
    function HitTest(vCanvas: TCanvas; vPoint: TPoint; vRect:TRect; var vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
    procedure Paint(Canvas: TCanvas; vRect:TRect; vFlags: TntvFlags = []);

    //Check function
    procedure UpdateItems(vCanvas: TCanvas); //call it before use next functions direclty
    function GetTabRect(const vTabsRect:TRect; TopIndex, Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean; overload;
    function GetTabRect(const vTabsRect:TRect; Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean; overload; virtual;
    function GetTabOffset(Index: Integer): Integer;

    //Tab functions
    function ShowTab(const vRect:TRect; Index: Integer; vFlags: TntvFlags): Boolean;
    //Items
    property Items[Index: Integer]: TntvTabItem read GetItem write SetItem stored False; default;
    //Properites
    property Visibles: TntvTabList read FVisibles write FVisibles;
    property Images: TImageList read FImages write SetImages;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property TopIndex: Integer read FTopIndex write FTopIndex;
    property ShowButtons: Boolean read FShowButtons write FShowButtons;
  published
  end;

implementation

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
      w := vCanvas.TextWidth(Items[i].Caption);
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
   Result := Result + [tdsActive];
  if (Index - TopIndex) = 0 then
    Result := Result + [tdsFirst];
  if Index = Visibles.Count - 1 then //need to review
    Result := Result + [tdsLast];
end;

function TntvTabs.CreateTabDraw: TntvTabDraw;
begin
  Result := TTabDrawSheet.Create;
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

function TntvTabs.HitTest(vCanvas: TCanvas; vPoint: TPoint; vRect:TRect; var vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
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

function TntvTabs.GetTabRect(const vTabsRect:TRect; TopIndex, Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean;
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
  if FUpdateItems then
    raise Exception.Create('You can not GetTabRect directly after changed');
  vTabRect := Rect(0, 0, 0, 0);
  Result := False;
  if (Index < FVisibles.Count) and (Index > -1) then
  begin
    x := 0;
    for i := TopIndex to Index do
      x := x + GetW(i);
    R := Rect(0, vTabsRect.Top, 0, vTabsRect.Bottom);
    w := GetW(Index);
    if tbfRightToLeft in vFlags then
    begin
      x := vTabsRect.Right - x;
      R.Left := x;
      R.Right := x + w;
    end
    else
    begin
      R.Right := x - 1;
      R.Left := x - w;
    end;
    vTabRect := R;
    Result := True;
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
  TabDraw.Paint(Visibles[Index], IndexToState(Index), vRect, Canvas, vFlags);
end;

function TntvTabs.GetTabRect(const vTabsRect:TRect; Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean;
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

function TntvTabs.ShowTab(const vRect:TRect; Index: Integer; vFlags: TntvFlags): Boolean;
var
  R: TRect;
  aTopIndex: Integer;
  w: Integer;
begin
  //DoShowTab();
  GetTabRect(vRect, ItemIndex, R, vFLags);
  aTopIndex := TopIndex;
  if tbfRightToLeft in vFlags then
  begin
    w := 0;
    if R.Left < w then
    begin
      while (R.Left < w) and (aTopIndex < ItemIndex) do
      begin
        aTopIndex := aTopIndex + 1;
        GetTabRect(vRect, aTopIndex, ItemIndex, R, vFLags);
      end;
    end;
  end
  else
  begin
    w := 0;
    if R.Right > w then
    begin
      while (R.Right > w) and (aTopIndex < ItemIndex) do
      begin
        aTopIndex := aTopIndex + 1;
        GetTabRect(vRect, aTopIndex, ItemIndex, R, vFLags);
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

{ TTabDrawSheet }

function TTabDrawSheet.GetWidth(State: TTabDrawStates; vTabsRect: TRect; Width: Integer): Integer;
var
  w: Integer;
begin
  w := vTabsRect.Bottom - vTabsRect.Top;
  Result := Width + w; //margin
{  if tdsFirst in State then
    Result := Result + w;}
end;

function TTabDrawSheet.Paint(vItem: TntvTabItem; State: TTabDrawStates; vRect: TRect; Canvas: TCanvas; vFlags: TntvFlags): Boolean;
var
  R, aTextRect: TRect;
  aTextStyle: TTextStyle;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    if tdsActive in State then
      Brush.Color := clBtnFace
    else
      Brush.Color := clWhite;
    FillRect(vRect);

    aTextRect := vRect;
    if (tdsFirst in State) then
    begin
      Inc(aTextRect.Left);
    end;
    //Dec(aTextRect.Right);
    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    if tbfRightToLeft in vFlags then
       aTextStyle.RightToLeft := True;
    Font.Color := clBlack;
    TextRect(aTextRect, 0, 0, vItem.Caption, aTextStyle);

    Pen.Style := psSolid;
    Pen.Color := clBlack;
    MoveTo(vRect.Right, vRect.Bottom);
    LineTo(vRect.Right, vRect.Top);
    if (tdsFirst in State) then
    begin
      LineTo(vRect.Left, vRect.Top);
      LineTo(vRect.Left, vRect.Bottom);
    end
    else
      LineTo(vRect.Left - 1, vRect.Top);

    if tdsActive in State then
    begin
      if tbfFocused in vFlags then
        DrawFocusRect(R);
    end;
  end;
end;

end.

