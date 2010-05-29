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
interface

uses
  Classes, SysUtils, Graphics, Controls, Contnrs, Types;

type
  TTabStyle = (tbDefault, tbNormal, tbSheets);

  { TntvTabItem }

  TntvTabItem = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FImageIndex: Integer;
    FName: string;
    FEnabled: Boolean;
    FVisible: Boolean;
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
    procedure VisibleChanged;
    procedure Changed;
    procedure Update; virtual;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Width: Integer read FWidth write SetWidth;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Name: string read GetName write SetName;
    property Enabled: boolean read FEnabled write SetEnabled;
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

  TntvhtTabHitTest = (htNone, htTab, htNext, htPrior, htClose);
  TntvFlag = (tbfUseRightToLeft, tbfFocused);
  TntvFlags = set of TntvFlag;

  { TntvTabs }

  TntvTabs = class(TCollection)
  private
    FImages: TImageList;
    FItemIndex: Integer;
    FShowButtons: Boolean;
    FTabStyle: TTabStyle;
    FTopIndex: Integer;
    FVisibles: TntvTabList;
  protected
    function GetItem(Index: Integer): TntvTabItem;
    procedure SetItem(Index: Integer; Value: TntvTabItem);
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateItems(vCanvas: TCanvas);
    procedure VisibleChanged;

    procedure DrawButtons(Canvas: TCanvas; var vRect: TRect; vFlags: TntvFlags);
    procedure DrawTab(Canvas: TCanvas; Index: Integer; vRect: TRect; vFlags: TntvFlags);
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TntvTabItem;

    function HitTest(vPoint: TPoint; vRect:TRect; var vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
    procedure Paint(Canvas: TCanvas; vRect:TRect; vFlags: TntvFlags);

    function GetTabRect(const vRect:TRect; TopIndex, Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean; overload;
    function GetTabRect(const vRect:TRect; Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean; overload; virtual;
    function GetTabOffset(Index: Integer): Integer;

    property Items[Index: Integer]: TntvTabItem read GetItem write SetItem stored False; default;
    property Visibles: TntvTabList read FVisibles write FVisibles;
    property Images: TImageList read FImages write FImages;
    property TabStyle: TTabStyle read FTabStyle write FTabStyle;
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
    Changed;
    Update;
  end;
end;

procedure TntvTabItem.SetEnabled(const AValue: boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    VisibleChanged;
    Update;
  end;
end;

procedure TntvTabItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  Changed;
end;

procedure TntvTabItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex = AValue then exit;
  FImageIndex :=AValue;
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
    VisibleChanged;
    Changed;
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
  VisibleChanged;
end;

procedure TntvTabItem.VisibleChanged;
begin
  (Collection as TntvTabs).VisibleChanged;
end;

procedure TntvTabItem.Changed;
begin

end;

procedure TntvTabItem.Update;
begin
end;

constructor TntvTabItem.Create(vCollection: TCollection);
begin
  inherited Create(vCollection);
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

function TntvTabs.GetItem(Index: Integer): TntvTabItem;
begin
  Result := (inherited GetItem(Index) as TntvTabItem);
end;

procedure TntvTabs.Update(Item: TCollectionItem);
begin
  inherited;
end;

procedure TntvTabs.UpdateItems(vCanvas: TCanvas);
begin

end;

procedure TntvTabs.VisibleChanged;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if Items[i].Visible then
      FVisibles.Add(Items[i]);
  end;
end;

constructor TntvTabs.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(TntvTabItem);
  FVisibles := TntvTabList.create(False);
end;

destructor TntvTabs.Destroy;
begin
  FVisibles.Free;
  inherited;
end;

function TntvTabs.Add: TntvTabItem;
begin
  Result := inherited Add as TntvTabItem;
end;

function TntvTabs.HitTest(vPoint: TPoint; vRect:TRect; var vIndex: Integer; vFlags: TntvFlags): TntvhtTabHitTest;
var
  i: Integer;
  R: TRect;
begin
  Result := htNone;
  vIndex := -1;
  for i := TopIndex to Visibles.Count - 1 do
  begin
    if GetTabRect(vRect, i, R, vFlags) and PtInRect(R, vPoint) then
    begin
      Result := htTab;
    end;
  end;
end;

procedure TntvTabs.Paint(Canvas: TCanvas; vRect: TRect; vFlags: TntvFlags);
var
  aRect: TRect;
  i: Integer;
begin
  if ShowButtons then
  begin
    DrawButtons(Canvas, vRect, vFlags);
  end;
  GetTabRect(vRect, ItemIndex, aRect, vFlags);
  DrawTab(Canvas, ItemIndex, aRect, vFlags);
  //ExcludeClipRect(Handle, aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
  for i := TopIndex to Visibles.Count - 1 do
  begin
    if i = ItemIndex then
      Continue;
    if GetTabRect(vRect, i, aRect, vFlags) then
      DrawTab(Canvas, i, aRect, vFlags);
    //ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    if tbfUseRightToLeft in vFlags then
    begin
      if aRect.Left < vRect.Right then
        Break;
    end
    else
    begin
      if aRect.Right > vRect.Left then
        Break;
    end;
  end;
end;

function TntvTabs.GetTabRect(const vRect:TRect; TopIndex, Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean;
var
  R: Trect;
  i, x: Integer;
begin
  vTabRect := Rect(0, 0, 0, 0);
  Result := False;
  if (Index < FVisibles.Count) and (Index > -1) then
  begin
    x := 0;
    for i := TopIndex to Index do
      x := x + FVisibles[i].Width;
    R := Rect(0, vRect.Top, 0, vRect.Bottom);
    if tbfUseRightToLeft in TntvFlags then
    begin
      x := vRect.Right - x;
      R.Left := x;
      R.Right := x + FVisibles[Index].Width;
    end
    else
    begin
      R.Right := x;
      R.Left := x - FVisibles[Index].Width;
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
  R, aTextRect: TRect;
  aTextStyle: TTextStyle;
begin
  with Canvas do
  begin
    aTextStyle := TextStyle;
    if ItemIndex <> Index then
    begin
      Dec(vRect.Bottom);
    end
    else if Index > TopIndex then
      InflateRect(vRect, 1, 0)
    else
    begin
      if tbfUseRightToLeft in TntvFlags then
        Dec(vRect.Left)
      else
        Inc(vRect.Right);
    end;
    R := vRect;
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(R);
    Inc(R.Top, 2);
    if Index <> ItemIndex then
    begin
      Inc(R.Top, 2);
    end;
    Brush.Color := clBtnFace;
    Pen.Color := clBtnHighlight;
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Top);

    LineTo(R.Left + 1, R.Top - 1);
    LineTo(R.Right - 2, R.Top - 1);
    LineTo(R.Right - 1, R.Top);

    Pen.Color := clGray;
    LineTo(R.Right - 1, R.Bottom);

    aTextRect := R;
    if Index = ItemIndex then
    begin
      MoveTo(R.Right - 2, R.Bottom);
      Pen.Color := clBtnFace;
      LineTo(R.Left, R.Bottom);
      InflateRect(R, -3, -1);
      if tbfFocused in TntvFlags then
        DrawFocusRect(R);
      OffsetRect(aTextRect, 0, -2);
    end;
    Brush.Style := bsClear;
    Brush.Style := bsClear;
    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    if tbfUseRightToLeft in TntvFlags then
       aTextStyle.RightToLeft := True;
    TextRect(aTextRect, 0, 0, Visibles[Index].Caption, aTextStyle);
  end;
end;

function TntvTabs.GetTabRect(const vRect:TRect; Index: Integer; var vTabRect: TRect; vFlags: TntvFlags): Boolean;
begin
  Result := GetTabRect(vRect, TopIndex, Index, vTabRect, vFlags);
end;

function TntvTabs.GetTabOffset(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Index - 1 downto TopIndex do
    Result := Result + FVisibles[index].Width;
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

end.

