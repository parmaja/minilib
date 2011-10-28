unit posLists;
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
  Classes, Graphics, Controls, StdCtrls, Forms, Contnrs,
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  Math, 
  posControls, Types;

type
  TposItemInfo = record
    Text: string;
    Width: Integer;
  end;

  TOnUpdate = procedure of object;

  TposItems = class(TObject)
  private
    FUpdateCount: Integer;
    FOnUpdate: TOnUpdate;
    function GetUpdating: Boolean;
    procedure SetUpdating(const Value: Boolean);
  protected
    function GetCount: Integer; virtual; abstract;
    procedure SetCount(const Value: Integer); virtual; abstract;
    function GetItemIndex: Integer; virtual; abstract;
    procedure SetItemIndex(const Value: Integer); virtual; abstract;
    procedure Update; virtual;
  public
    constructor Create; virtual;
    procedure PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    property Count: Integer read GetCount write SetCount;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Updating: Boolean read GetUpdating write SetUpdating;
    property OnUpdate: TOnUpdate read FOnUpdate write FOnUpdate;
  end;

  { TposAbstractList }

  TposAbstractList = class(TposFocusFrame)
  private
    FItems: TposItems;
    FTopIndex: Integer;
    FItemHeight: Integer;
    FShowHeader: Boolean;
    FClkX: integer;
    FClkY: integer;
    FShowSelected: Boolean;
    FSelectedColor: TColor;
    procedure SetItemHeight(const Value: Integer);
    procedure SetTopIndex(Value: Integer);
    function GetPage: Integer;
    procedure SetPage(const Value: Integer);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetItemIndex(const Value: Integer);
    function GetItemIndex: Integer;
  protected
    procedure DoUpdate; virtual;
    function DoCreateItems: TposItems; virtual; abstract;
    function CreateItems: TposItems;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ChangeScale(M, D: Integer); override;

    procedure PaintItems(Items: TposItems; Canvas: TCanvas; TopIndex, ItemIndex: Integer; Rect: TRect);
    procedure PaintInner(Canvas: TCanvas; var vRect: TRect; Color: TColor); override;

    procedure DoItemIndexChanged; virtual;

    //Correct: when user make arrow or PageUp/Down
    function ChangeItemIndex(Value: Integer; Correct: Boolean = False): Boolean;
    function GetItemRect(vItem: Integer; out vRect: TRect): Boolean;
    function InvalidateItem(vItem: Integer): boolean;
    procedure ItemsScroll(vItems: Integer);
    function GetDistinctItems(vHeight: Integer): Integer; overload;
    function GetDistinctItems: Integer; overload;
    function PosToIndex(y: Integer): Integer;
    function IsValidItem(vItem: Integer): boolean;
    function IndexToView(vIndex: Integer): Integer;
    function ViewToIndex(vItem: Integer): Integer;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetInputs: TposFrameInputs; override;
    procedure ShowPageOfIndex(Index: Integer);
    function ShowItem(vItem: Integer): Boolean;
    function GetContentsRect: TRect; override;
    function GetInnerRect: TRect; override;
    function ProcessKey(var Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure PageUp;
    procedure PageDown;
    procedure Invalidate; override;
    property Items: TposItems read FItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property TopIndex: Integer read FTopIndex write SetTopIndex default 0;
    property Page: Integer read GetPage write SetPage;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader;
  published
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 22;
    property ShowSelected: Boolean read FShowSelected write FShowSelected default True;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor default clBlack;
  end;

//* Items for managed in memory and used with Listbox and Grids
//* Items but not painted

  TposCustomListItems = class;

  TposCustomListItem = class(TObject)
  private
    FParent: TposCustomListItems;
  protected
    function UseRightToLeftAlignment: Boolean; virtual;
    procedure SetParent(const Value: TposCustomListItems); virtual;
  public
    procedure Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); virtual;
    property Parent: TposCustomListItems read FParent write SetParent;
  end;

  TposCustomListItems = class(TposItems)
  private
    FList: TObjectList;
    FItemIndex: Integer;
    function GetItem(Index: Integer): TposCustomListItem;
    procedure SetItem(Index: Integer; const Value: TposCustomListItem);
  protected
    function GetCount: Integer; override;
    procedure SetCount(const Value: Integer); override;
    function GetItemIndex: Integer; override;
    procedure SetItemIndex(const Value: Integer); override;
  protected
    function CreateItem: TposCustomListItem; virtual;
    function UseRightToLeftAlignment: Boolean; virtual;
    procedure AddItem(Item: TposCustomListItem); virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    destructor Destroy; override;
    property Items[Index: Integer]: TposCustomListItem read GetItem write SetItem; default;
  end;

//* This list box
//Items with painting as list box

  TposListItem = class(TposCustomListItem)
  private
    FText: string;
    FName: string;
    FColor: TColor;
  protected
  public
    constructor Create;
    procedure Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); override;
    property Text: string read FText write FText;
    property Name: string read FName write FName;
    property Color: TColor read FColor write FColor;
  end;

  TposList = class;

  TposListItems = class(TposCustomListItems)
  protected
    FList: TposList;
    function CreateItem: TposCustomListItem; override;
    function UseRightToLeftAlignment: Boolean; override;
  public
    procedure PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); override;
    procedure Add(const Text: string); overload;
    procedure Add(const Name, Text: string); overload;
  end;

  TposList = class(TposAbstractList)
  private
    function GetItems: TposListItems;
  protected
    function DoCreateItems: TposItems; override;
  public
    property Items: TposListItems read GetItems;
  end;

implementation

uses
  mnUtils, posUtils;

procedure TposAbstractList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  r: TRect;
begin
  if not (ssDouble in Shift) then
  begin
    FClkX := X;
    FClkY := Y;
    i := PosToIndex(Y);
    if IsValidItem(i) then
      ItemIndex := i;
    R := GetInnerRect;
    if (i >= 0) and (GetItemRect(i, R)) then
    begin
{      with Items[i] do
      begin
        MouseDown(Button, Shift, X, Y);
      end;}
    end;
  end;
  inherited;
end;

procedure TposAbstractList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TposAbstractList.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

constructor TposAbstractList.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
  Style := Style - [fsOpaque];
  FItems := CreateItems;
  Width := 60;
  Height := 22;
  FItemHeight := 22;
  FShowSelected := True;
  TabStop := True;
  FSelectedColor := clBlack;
end;

destructor TposAbstractList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TposAbstractList.PaintInner(Canvas: TCanvas; var vRect: TRect; Color: TColor);
begin
  inherited;
  PaintItems(Items, Canvas, TopIndex, ItemIndex, vRect);
end;

{ TposCustomListItems }

procedure TposListItems.Add(const Text: string);
begin
  Add('', Text);
end;

procedure TposCustomListItems.AddItem(Item: TposCustomListItem);
begin
  FList.Add(Item);
end;

procedure TposCustomListItems.Clear;
begin
  inherited;
  FList.Clear;
end;

constructor TposCustomListItems.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

function TposCustomListItems.CreateItem: TposCustomListItem;
begin
  Result := TposCustomListItem.Create;
end;

destructor TposCustomListItems.Destroy;
begin
  FList.Free;
  inherited;
end;

function TposCustomListItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TposCustomListItems.GetItem(Index: Integer): TposCustomListItem;
begin
  Result := FList[Index] as TposCustomListItem;
end;

function TposCustomListItems.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TposCustomListItems.SetCount(const Value: Integer);
begin
  FList.Count := Value;
end;

procedure TposCustomListItems.SetItem(Index: Integer; const Value: TposCustomListItem);
begin
  FList[Index] := Value;
end;

procedure TposCustomListItems.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
end;

function TposCustomListItems.UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;

{ TposCustomListItem }

constructor TposListItem.Create;
begin
  inherited Create;
  FColor := clDefault;
end;

procedure TposListItems.PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
begin
  Items[Index].Paint(Canvas, Index, Rect, Color, RightToLeft);
end;

procedure TposListItem.Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
var
  aStyle: TTextStyle;
  aColor: TColor;
begin
  inherited;
  InitMemory(aStyle, SizeOf(aStyle));
  aStyle.RightToLeft := RightToLeft;
  aStyle.SingleLine := True;
  aStyle.Layout := tlCenter;
  if Odd(Index) then
    aColor := MixColors(clWhite, Color, 30)
  else
    aColor := MixColors(clBlack, Color, 30);
  Canvas.Brush.Color := aColor;
  Canvas.FillRect(Rect);
  BidiAlignment(aStyle);

  InflateRect(Rect, -1, -2);
  PaintText(Canvas, Text, Rect, aStyle);
end;

procedure TposCustomListItem.Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
begin
end;

procedure TposCustomListItem.SetParent(const Value: TposCustomListItems);
begin
  if FParent <> Value then
  begin
    if FParent <> nil then
      FParent.FList.Extract(Self);
    FParent := Value;
    if FParent <> nil then
      FParent.FList.Add(Self);
  end;
end;

function TposCustomListItem.UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;

{ TposList }

function TposList.DoCreateItems: TposItems;
begin
  Result := TposListItems.Create;
  TposListItems(Result).FList := Self;
end;

function TposList.GetItems: TposListItems;
begin
  Result := (inherited Items) as TposListItems;
end;

{ TposListItems }

procedure TposListItems.Add(const Name, Text: string);
var
  aItem: TposCustomListItem;
begin
  aItem := CreateItem;
  (aItem as TposListItem).Text := Text;
  (aItem as TposListItem).Name := Name;
  aItem.Parent := Self;
end;

function TposListItems.CreateItem: TposCustomListItem;
begin
  Result := TposListItem.Create;
end;

function TposListItems.UseRightToLeftAlignment: Boolean;
begin
  Result := FList.UseRightToLeftAlignment;
end;

{ TposItems }

procedure TposItems.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TposItems.Clear;
begin
  Update;
end;

constructor TposItems.Create;
begin
  inherited;
end;

procedure TposItems.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Update;
end;

procedure TposAbstractList.ChangeScale(M, D: Integer);
begin
  inherited;
{$ifdef FPC}
  ItemHeight := MulDiv(ItemHeight, M, D)
{$endif}
end;

function TposItems.GetUpdating: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TposItems.SetUpdating(const Value: Boolean);
begin
  if Value then
    Inc(FUpdateCount)
  else
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Update;
end;

procedure TposItems.Update;
begin
  if not Updating and Assigned(FOnUpdate) then
    FOnUpdate;
end;

procedure TposAbstractList.Invalidate;
begin
  if not Items.Updating then
    inherited;
end;

procedure TposAbstractList.SetTopIndex(Value: Integer);
var
  DeltaY, OldTopIndex, v, Allowed: integer;
begin
  v := GetDistinctItems;
  Allowed := Items.Count - v;
  if Allowed < 0 then
    Allowed := 0;
  if (Value >= Allowed) then
    Value := Allowed;
  if (Value < 0) then
    Value := 0;

  OldTopIndex := FTopIndex;
  FTopIndex := Value;
  DeltaY := OldTopIndex - Value;
  if Abs(DeltaY) < v then
    ItemsScroll(DeltaY)
  else
    Invalidate;
  //UpdateScrollBar;
end;

procedure TposAbstractList.SetItemHeight(const Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TposAbstractList.ShowPageOfIndex(Index: Integer);
var
  c: Integer;
begin
  c := GetDistinctItems;
  if c > 0 then
    TopIndex := (Index div c) * c;
end;

function TposAbstractList.GetPage: Integer;
var
  c: Integer;
begin
  c := GetDistinctItems;
  if c > 0 then
    Result := TopIndex div c
  else
    Result := 0;
end;

procedure TposAbstractList.SetPage(const Value: Integer);
var
  c, Index: Integer;
begin
  c := GetDistinctItems;
  if c > 0 then
  begin
    Index := Value * c;
    if Index > Items.Count - 1 then
      Index := ((Items.Count - 1) div c) * c;
    TopIndex := Index;
  end
  else
    TopIndex := 0;
end;

procedure TposAbstractList.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    Invalidate;
  end;
end;

function TposAbstractList.CreateItems: TposItems;
begin
  Result := DoCreateItems;
  Result.OnUpdate := DoUpdate;
end;

procedure TposAbstractList.DoUpdate;
begin
  Invalidate;
end;

function TposAbstractList.IsValidItem(vItem: Integer): boolean;
begin
  Result := (vItem < (Items.Count)) and (vItem >= 0);
end;

function TposAbstractList.PosToIndex(y: Integer): Integer;
var
  r: TRect;
begin
  r := GetInnerRect;
  if y < r.Top then
    Result := -1
  else
  begin
    y := y - r.Top;
    Result := ViewToIndex(GetDistinctItems(y))
  end;
end;

function TposAbstractList.ViewToIndex(vItem: Integer): Integer;
begin
  Result := FTopIndex + vItem;
end;

function TposAbstractList.GetItemIndex: Integer;
begin
  Result := Items.ItemIndex
end;

function TposAbstractList.GetItemRect(vItem: Integer; out vRect: TRect): Boolean;
var
  wRect: TRect;
begin
  Result := False;
  vItem := IndexToView(vItem);
  if vItem >= 0 then
  begin
    wRect := GetInnerRect;
    vRect := wRect;
    vRect.Top := vRect.Top + vItem * FItemHeight;
    vRect.Bottom := vRect.Top + FItemHeight;
    if vRect.Top <= wRect.Bottom then
      GetItemRect := True;
  end;
end;

function TposAbstractList.GetDistinctItems(vHeight: Integer): Integer;
begin
//  Result := ceil(vHeight / FItemHeight);
  Result := vHeight div FItemHeight;
end;

function TposAbstractList.GetContentsRect: TRect;
begin
  Result := inherited GetContentsRect;
end;

function TposAbstractList.GetDistinctItems: Integer;
begin
  Result := GetDistinctItems(InnerHeight);
end;

function TposAbstractList.IndexToView(vIndex: Integer): Integer;
begin
  Result := vIndex - FTopIndex;
end;

procedure TposAbstractList.ItemsScroll(vItems: Integer);
{var
  R: TRect;}
begin
  Invalidate;
//  R := InnerRect;
//  ScrollWindowEx(Handle, 0, FItemHeight * vItems, @R, @R, 0, nil, SW_SCROLLCHILDREN or SW_INVALIDATE); //belal
end;

procedure TposAbstractList.SetItemIndex(const Value: Integer);
begin
  if Items.ItemIndex <> Value then
  begin
    ChangeItemIndex(Value, False);
  end;
end;

function TposAbstractList.ChangeItemIndex(Value: Integer; Correct: Boolean): Boolean;
var
  OldIndex: Integer;
begin
  if (ItemIndex <> Value) then
  begin
    if (Value >= (Items.Count)) then
      Value := Items.Count - 1;
    if Correct and (Value < 0) then
      Value := 0
    else if (Value < -1) then
      Value := -1;

    if (ItemIndex <> Value) then
    begin
      OldIndex := Items.ItemIndex;
      Items.ItemIndex := Value;
      InvalidateItem(OldIndex);
      ShowItem(Value);
      if not Items.Updating and not (csDesigning in ComponentState) then
        DoItemIndexChanged;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

function TposAbstractList.InvalidateItem(vItem: Integer): Boolean;
var
  aRect: TRect;
begin
  if GetItemRect(vItem, aRect) then
  begin
    InvalidateRect(aRect);
    Result := True;
  end
  else
    Result := False;
end;

function TposAbstractList.ShowItem(vItem: Integer): Boolean;
var
  v: integer;
begin
  Result := false;
  v := GetDistinctItems;
  if vItem < FTopIndex then
    TopIndex := vItem
  else if (vItem >= (FTopIndex + v)) then
  begin
    TopIndex := vItem - v + 1;
    if (vItem = (FTopIndex + v - 1)) then
      Result := InvalidateItem(vItem);
  end
  else
    Result := InvalidateItem(vItem);
end;

function TposAbstractList.GetInputs: TposFrameInputs;
begin
  Result := inherited GetInputs + [fiArrow];
end;

function TposAbstractList.DoKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := ProcessKey(Key, Shift) or inherited DoKeyDown(Key, Shift);
end;

function TposAbstractList.ProcessKey(var KEY: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: DblClick; //zaher: not good here
      Vk_Home:
        ChangeItemIndex(0, True);
      Vk_End:
        ChangeItemIndex(Items.Count - 1, True);
      Vk_Down:
        ChangeItemIndex(ItemIndex + 1, True);
      Vk_Up:
        ChangeItemIndex(ItemIndex - 1, True);
      Vk_PRIOR: PageUp;
      Vk_Next: PageDown;
    else
      Result := False;
    end
  end
  else
    Result := False;
end;

procedure TposAbstractList.PageDown;
begin
  ChangeItemIndex(ItemIndex + GetDistinctItems, True);
end;

procedure TposAbstractList.PageUp;
begin
  ChangeItemIndex(ItemIndex - GetDistinctItems, True);
end;

function TposAbstractList.GetInnerRect: TRect;
begin
  Result := inherited GetInnerRect;
  if FShowHeader then
    Result.Top := Result.Top + ItemHeight;
end;

procedure TposAbstractList.DoItemIndexChanged;
begin

end;

procedure TposAbstractList.PaintItems(Items: TposItems; Canvas: TCanvas; TopIndex, ItemIndex: Integer; Rect: TRect);
var
  aItemRect: TRect;
  i: Integer;
  aColor: TColor;
  aCount: Integer;
begin
  i := TopIndex;
  aCount := Items.Count;
  aItemRect := Rect;
  aItemRect.Bottom := aItemRect.Top + ItemHeight;
  while ((aItemRect.Top) < Rect.Bottom) and (i < aCount) and (i >= 0) do
  begin
    Canvas.Font := Font;
    Canvas.Brush.Style := bsSolid;
    if ShowSelected and (ItemIndex = i) then
    begin
      aColor := MixColors(SelectedColor, Color, 100);
      Canvas.Font.Color := OppositeColor(aColor);
    end
    else if Odd(i) then
      aColor := MixColors(clWhite, Color, 10)
    else
      aColor := MixColors(clBlack, Color, 10);
    Items.PaintItem(Canvas, I, aItemRect, aColor, UseRightToLeftAlignment);
    if ItemIndex = i then
    begin
{      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(aItemRect);}
    end;
    OffsetRect(aItemRect, 0, ItemHeight);
    Inc(i);
  end;
  if aItemRect.Top < Rect.Bottom then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    aItemRect.Bottom := Rect.Bottom;
    Canvas.FillRect(aItemRect);
  end;
end;

initialization
finalization
end.

