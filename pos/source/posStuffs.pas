unit posStuffs;
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
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  Contnrs, Types,
{$IFDEF FPC}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  posThemes, posControls, posUtils, posTypes;

const
  cStuffsItemHeight = 36;
  cDefaultStuffWidth = 64;
  cSpaceSize = 1;

type
  TRefreshMode = (rfrNormal, rfrItem, rfrControl);

  TposStuffObject = class(TObject, IInterface)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif FPC} IID: TGUID; out Obj): HResult; virtual; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  end;

  TposStuffs = class;

  TposStuffItem = class(TposStuffObject, IposStuff)
  private
    FStuffs: TposStuffs;
    FStuff: IposStuff;
    FStates: TposDrawStates;
  protected
    function GetObject: TObject;
    procedure SetStates(vStates: TposDrawStates); virtual;
    procedure Click; virtual;
    function GetDrawSize: Integer; virtual;
    function Draw(vCanvas: TCanvas; vRect: TRect; vColor: TColor; vStates: TposDrawStates): Boolean; virtual;
  public
    destructor Destroy; override;
    property Stuff: IposStuff read FStuff write FStuff;
  end;

  TposStuffItems = class(TObjectList, IposStuffList)
  private
    FRefCount: Integer;
    FStuffs: TposStuffs;
    function GetItem(Index: Integer): TposStuffItem;
    procedure SetItem(Index: Integer; const Value: TposStuffItem);
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetItems(Index: Integer): IposStuff;
    function GetCount: Integer;
  public
    constructor Create(AStuffs: TposStuffs);
    destructor Destroy; override;
    procedure Add(Stuff: IposStuff);
    function GetDrawSize: Integer; virtual;
    function Release: Boolean;
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif FPC} IID: TGUID; out Obj): HResult; virtual; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
    procedure Clear; override;
    procedure ItemClick(Sender: TObject); virtual;
    procedure ItemStates(Sender: TObject; var vStates: TposDrawStates); virtual;
    property Items[Index: Integer]: TposStuffItem read GetItem write SetItem; default;
  end;

  TOnStuffClick = procedure(Sender: TObject) of object;

  //Temporary list
  TposListStuffItem = class(TObject)
  public
    Rect: TRect;
    Item: IposStuff;
    Index: Integer;
    constructor Create;
    procedure Click;
  end;

  TposListStuffItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TposListStuffItem;
    procedure SetItem(Index: Integer; const Value: TposListStuffItem);
  public
    property Items[Index: Integer]: TposListStuffItem read GetItem write SetItem; default;
  end;

  { TposStuffLocator }

  TLocatorProc = function(Index: Integer; R: TRect; Stuff: IposStuff): Boolean of object; //stop enum whe result False

  TposStuffLocator = class(TObject)
  private
    FItems: TposListStuffItems;
    FStuffs: TposStuffs;
    //
    FX, FY: Integer;
    FIndex: Integer;
    FItem: TposListStuffItem; //do not free it
    FResultRect: TRect;
    function CalcRect(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
    function AddItem(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
    function CheckItemXY(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
    function CheckItemIndex(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TposListStuffItem;
  public
    constructor Create(vStuffs: TposStuffs); virtual;
    destructor Destroy; override;
    procedure CreateList(vTopIndex: Integer; Reverse: Boolean; vRect: TRect);
    function CheckXY(vTopIndex: Integer; Reverse: Boolean; vRect: TRect; X, Y: Integer): TposListStuffItem;
    function CheckIndex(vTopIndex: Integer; Reverse: Boolean; vRect: TRect; Index: Integer): TposListStuffItem;
    function IndexFound(vTopIndex: Integer; Reverse: Boolean; vRect: TRect; Index: Integer): Boolean;
    function EnumStuffs(vTopIndex: Integer; Reverse: Boolean; vRect: TRect; Proc: TLocatorProc): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TposListStuffItem read GetItem; default;
  end;

  { TposStuffs }

  TposStuffs = class(TposCustomFrame)
  private
    FItems: IposStuffList;
    FDownItem: TposListStuffItem;
    FOnStuffClick: TOnStuffClick;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FButtonColor: TColor;
    FButtonWidth: Integer;
    FTopIndex: Integer;
    FSpaceSize: Integer;
    FAutoSelect: Boolean;
    FColumns: Integer;
    FRows: Integer;
    FReadOnly: Boolean;
    FMouseInDown: Boolean;
    FRefreshMode: TRefreshMode;
    FInteractive: Boolean;
    function GetCount: Integer;
    function GetSelected: IposStuff;
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetDownItem(const Value: TposListStuffItem);
    procedure SetItems(const Value: IposStuffList);
    function GetItems: IposStuffList;
    procedure SetTopIndex(const Value: Integer);
    property DownItem: TposListStuffItem read FDownItem write SetDownItem;
  protected
    function GetItemRect(Index: Integer; var vRect: TRect): Boolean;
    procedure InvalidateItem(Index: Integer);
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    function ProcessKey(var Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure StuffClicked(Stuff: IposStuff);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetInnerRect: TRect; override;
    function GetInputs: TposFrameInputs; override;
    procedure Clear;
    procedure NextItem;
    procedure PriorItem;
    procedure ClickItem; //Click the selected Item
    procedure PageUp;
    procedure PageDown;
    function CalcHeight: Integer;
    property Selected: IposStuff read GetSelected;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: IposStuffList read GetItems write SetItems;
    property Count: Integer read GetCount;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
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
    property OnConstrainedResize;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property RefreshMode: TRefreshMode read FRefreshMode write FRefreshMode default rfrNormal;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect stored False default False;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default cStuffsItemHeight;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Columns: Integer read FColumns write FColumns default 0;
    property Rows: Integer read FRows write FRows default 0;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clBtnFace;
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth default cDefaultStuffWidth;
    property SpaceSize: Integer read FSpaceSize write FSpaceSize default cSpaceSize;
    //Interactive call Click method of Item clicked
    property Interactive: Boolean read FInteractive write FInteractive default True;
    property OnStuffClick: TOnStuffClick read FOnStuffClick write FOnStuffClick;
  end;

implementation

{ TposStuffs }

constructor TposStuffs.Create(AOwner: TComponent);
begin
  inherited;
  FItems := nil;
  ControlStyle := ControlStyle + [csSetCaption, csCaptureMouse];
  Style := Style - [fsBorder] + [fsLatedOpaque];
  FItemIndex := -1;
  FInteractive := True;
  FSpaceSize := cSpaceSize;
  FItemHeight := cStuffsItemHeight;
  FButtonColor := clBtnFace;
  FButtonWidth := cDefaultStuffWidth;
end;

procedure TposStuffs.Resize;
begin
  inherited;
end;

destructor TposStuffs.Destroy;
begin
  FreeAndNil(FDownItem);
  if FItems <> nil then
    if FItems.Release then
      Pointer(FItems) := nil
    else
      FItems := nil;
  inherited;
end;

procedure TposStuffs.StuffClicked(Stuff: IposStuff);
var
  o: TObject;
begin
  o := Stuff.GetObject;
  if o <> nil then
  begin
    if FItems <> nil then
      FItems.ItemClick(o);
    if Assigned(FOnStuffClick) then
      FOnStuffClick(o);
  end;
end;

function TposStuffs.CalcHeight: Integer;
var
  aItems: TposStuffLocator;
begin
  Result := 0;
  if Items <> nil then
  begin
    aItems := TposStuffLocator.Create(Self);
    try
      aItems.FResultRect := Rect(0, 0, 0, 0);
      aItems.EnumStuffs(TopIndex, False, GetInnerRect, aItems.CalcRect);
      Result := aItems.FResultRect.Bottom - aItems.FResultRect.Top; 
    finally
      aItems.Free;
    end;
  end;
end;

procedure TposStuffs.ChangeScale(M, D: Integer);
begin
  inherited;
  FButtonWidth := MulDiv(ButtonWidth, M, D);
  FItemHeight := MulDiv(ItemHeight, M, D);
  Invalidate;
end;

procedure TposStuffs.Clear;
begin
  BeginUpdate;
  try
    FTopIndex := 0;
    FItemIndex := -1;
    Down := False;
    FreeAndNil(FDownItem);
    if Items <> nil then
      Items.Clear;
  finally
    EndUpdate;
  end;
  Invalidate;
end;

procedure TposStuffs.NextItem;
var
  aLocator: TposStuffLocator;
begin
  if (Items <> nil) and (ItemIndex < Items.Count - 1) then
  begin
    aLocator := TposStuffLocator.Create(Self);
    try
      if not aLocator.IndexFound(TopIndex, False, GetInnerRect, ItemIndex + 1) then
        PageDown
      else
        ItemIndex := ItemIndex + 1;
    finally
      aLocator.Free;
    end;
  end;
end;

procedure TposStuffs.PriorItem;
begin
  if (Items <> nil) and (ItemIndex > 0) then
  begin
    if (ItemIndex - 1) < TopIndex then
    begin
      FItemIndex := ItemIndex - 1;
      PageUp;
    end
    else
      ItemIndex := ItemIndex - 1;
  end;
end;

procedure TposStuffs.SetItemHeight(const Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
  end;
end;

function TposStuffs.GetSelected: IposStuff;
begin
  if (Items <> nil) and (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    Result := Items[ItemIndex]
  else
    Result := nil;
end;

function TposStuffs.GetCount: Integer;
begin
  if Items = nil then
    Result := 0
  else
    Result := Items.Count;
end;

procedure TposStuffItems.Add(Stuff: IposStuff);
var
  aItem: TposStuffItem;
begin
  aItem := TposStuffItem.Create;
  aItem.FStuffs := FStuffs;
  aItem.FStuff := Stuff;
  inherited Add(aItem);
  FStuffs.Invalidate;
end;

procedure TposStuffs.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
var
  i: Integer;
  aStates: TposDrawStates;
  aItems: TposStuffLocator;
  aUpdateRect: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    vCanvas.Pen.Color := Font.Color;
    vCanvas.Pen.Style := psDot;
    vCanvas.Pen.Width := 1;
    vCanvas.Brush.Color := vColor;
    vCanvas.Rectangle(vRect);
    ExcludeClipRect(vCanvas, vRect);
  end
  else
  begin
    aUpdateRect := Canvas.ClipRect;
    aItems := TposStuffLocator.Create(Self);
    try
      aItems.CreateList(TopIndex, False, vRect);
      vCanvas.Pen.Style := psSolid;
      vCanvas.Pen.Width := 1;
      for i := 0 to aItems.Count - 1 do
      begin
        aStates := States;
        //if CollideRect(aItems[i].Rect, aUpdateRect) then
        begin
          aStates := aStates - [pdsDown];
          if (FDownItem <> nil) then
          begin
            if Down and (FDownItem.Item = aItems[i].Item) then
              aStates := aStates + [pdsDown];
          end;
          if AutoActive and (aItems[i].Index = ItemIndex) then
            aStates := aStates + [pdsActive];
          if UseRightToLeftReading then
            aStates := aStates + [pdsRightToLeft];
          vCanvas.Font.Assign(Font);
          if (FItems <> nil) and (aItems[i].Item <> nil) then
            FItems.ItemStates(aItems[i].Item.GetObject, aStates);
          aItems[i].Item.Draw(vCanvas, aItems[i].Rect, ButtonColor, aStates);
          ExcludeClipRect(vCanvas, aItems[i].Rect);
        end;
      end;
    finally
      aItems.Free;
    end;
    vCanvas.Brush.Color := vColor;
    vCanvas.FillRect(vRect);
  end;
end;

function TposStuffs.ProcessKey(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  if (Items <> nil) and (Shift = []) then
  begin
    case Key of
      VK_LEFT: NextItem;
      VK_RIGHT: PriorItem;
      VK_RETURN: ClickItem;
{      Vk_Home:
        ChangeItemIndex(0, True);
      Vk_End:
        ChangeItemIndex(Items.Count - 1, True);
      Vk_Down:
        ChangeItemIndex(ItemIndex + 1, True);
      Vk_Up:
        ChangeItemIndex(ItemIndex - 1, True);}
      Vk_PRIOR: PageUp;
      Vk_Next: PageDown;
    else
      Result := False;
    end
  end
  else
    Result := False;
end;

procedure TposStuffs.Click;
begin
  try
    inherited;
  finally
  end;
end;

procedure TposStuffs.ClickItem;
var
  aItem: IposStuff;
begin
  aItem := Selected;
  if aItem <> nil then
  begin
    FreeAndNil(FDownItem);
    FDownItem := TposListStuffItem.Create;
    try
      FDownItem.Item := aItem;
      if Interactive then
        DownItem.Click;
      if FDownItem <> nil then
        StuffClicked(aItem);
    finally
      FreeAndNil(FDownItem);
    end;
  end;
end;

procedure TposStuffs.DblClick;
begin
  try
    inherited;
//    Click;//moved to MouseDown
  finally
  end;
end;

procedure TposStuffs.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  aItems: TposStuffLocator;
begin
  FreeAndNil(FDownItem);
  if not ReadOnly and (Button = mbLeft) and not FMouseInDown then
  try
    inherited;
    FMouseInDown := True;
    aItems := TposStuffLocator.Create(Self);
    try
      DownItem := aItems.CheckXY(TopIndex, False, GetInnerRect, X, Y)
    finally
      aItems.Free;
    end;
  except
    Down := False;
    FMouseInDown := False;
    raise;
  end;
end;

procedure TposStuffs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PT: TPoint;
begin
  inherited;
  if MouseCapture then
  begin
    PT.X := X;
    PT.Y := Y;
    if (FDownItem <> nil) and PtInRect(FDownItem.Rect, PT) then
    begin
      if not (pdsDown in States) then
      begin
        States := States + [pdsDown];
        InvalidateRect(FDownItem.Rect);
      end;
    end
    else
    begin
      if (pdsDown in States) then
      begin
        States := States - [pdsDown];
        if FDownItem <> nil then
          InvalidateRect(FDownItem.Rect)
        else
          Invalidate;
      end;
    end;
  end;
end;

procedure TposStuffs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aRect: TRect;
  OldIndex: Integer;
begin
  try
    if Button = mbLeft then
    begin
      if (pdsDown in States) then
      begin
        States := States - [pdsDown];
        if FDownItem <> nil then
        begin
          // for not dublicating refresh the button we check the ItemIndex before invalidate
          aRect := FDownItem.Rect;
          OldIndex := FItemIndex;
          FItemIndex := DownItem.Index;
          if not ReadOnly then
          begin
            Themes.PlaySound('CLICK', False, False);
            if Interactive then
              DownItem.Click; // Click here can be free DownItem
            if (DownItem <> nil) then //may be prior Click change the Items of Stuffs
              StuffClicked(DownItem.Item);
          end;
          if not ReadOnly and (RefreshMode = rfrControl) then
            Invalidate
          else
          begin
            if (RefreshMode = rfrItem) then
            begin
              if (OldIndex <> FItemIndex) then
                InvalidateItem(OldIndex);
            end;
            InvalidateRect(aRect);
          end;
        end
        else
          Invalidate;
      end;
    end;
    inherited;
  finally
    FMouseInDown := False;
    FreeAndNil(FDownItem);
  end;
end;

function TposStuffs.DoKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := ProcessKey(Key, Shift) or inherited DoKeyDown(Key, Shift);
end;

procedure TposStuffs.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    if (FItemIndex >= 0) then
      InvalidateItem(FItemIndex);
    FItemIndex := Value;
    if (FItemIndex >= 0) then
      InvalidateItem(FItemIndex);
  end;
end;

procedure TposStuffs.SetItems(const Value: IposStuffList);
var
  OldItems: IposStuffList;
begin
  BeginUpdate;
  try
    FTopIndex := 0;
    FItemIndex := -1;
    Down := False;
    OldItems := FItems;
    FItems := Value;
    FreeAndNil(FDownItem);
    if OldItems <> nil then
      OldItems.Release;
    Pointer(OldItems) := nil; //resolve problem in Delphi7
    Invalidate;
  finally
    EndUpdate;
  end;
end;

procedure TposStuffs.SetTopIndex(const Value: Integer);
begin
  if FTopIndex <> Value then
  begin
    FTopIndex := Value;
    Invalidate;
  end;
end;

function TposStuffs.GetItemRect(Index: Integer; var vRect: TRect): Boolean;
var
  aItems: TposStuffLocator;
  aItem: TposListStuffItem;
begin
  Result := False;
  if Index < FTopIndex then
    vRect := Rect(0, 0, 0, 0)
  else
  begin
    aItems := TposStuffLocator.Create(Self);
    try
      aItem := aItems.CheckIndex(TopIndex, False, GetInnerRect, Index);
      if aItem <> nil then
      begin
        vRect := aItem.Rect;
        Result := True;
        aItem.Free;
      end;
    finally
      aItems.Free;
    end;
  end;
end;

procedure TposStuffs.InvalidateItem(Index: Integer);
var
  aRect: TRect;
begin
  if GetItemRect(Index, aRect) then
    InvalidateRect(aRect);
end;

procedure TposStuffs.SetDownItem(const Value: TposListStuffItem);
begin
  if FDownItem <> Value then
  begin
    if FDownItem <> nil then
    begin
      States := States - [pdsDown];
      InvalidateRect(FDownItem.Rect);
      FreeAndNil(FDownItem);
    end;
    FDownItem := Value;
    if FDownItem <> nil then
    begin
      States := States + [pdsDown];
      InvalidateRect(FDownItem.Rect);
    end;
  end;
end;

function TposStuffs.GetInnerRect: TRect;
begin
  Result := inherited GetInnerRect;
end;

function TposStuffs.GetItems: IposStuffList;
begin
  Result := FItems;
end;

function TposStuffs.GetInputs: TposFrameInputs;
begin
  Result := inherited GetInputs + [fiArrow];
end;

procedure TposStuffs.PageDown;
var
  aItems: TposStuffLocator;
  i: Integer;
begin
  if Items <> nil then
  begin
    aItems := TposStuffLocator.Create(Self);
    BeginUpdate;
    try
      i := aItems.EnumStuffs(TopIndex, False, GetInnerRect, nil) + 1;
      if (i >= 0) and (i < Items.Count) then
      begin
        TopIndex := i;
        ItemIndex := i;
      end;
    finally
      EndUpdate;
      aItems.Free;
    end;
  end;
end;

procedure TposStuffs.PageUp;
var
  i: Integer;
  aItems: TposStuffLocator;
begin
  if Items <> nil then
  begin
    if TopIndex > 0 then
    begin
      aItems := TposStuffLocator.Create(Self);
      try
        i := aItems.EnumStuffs(TopIndex - 1, True, GetInnerRect, nil);
        if i >= 0 then
        begin
          BeginUpdate;
          try
            ItemIndex := TopIndex - 1;
            TopIndex := i;
          finally
            EndUpdate;
          end;
        end;
      finally
        aItems.Free;
      end;
    end;
  end;
end;

{ TposStuffItem }

procedure TposStuffs.Loaded;
begin
  inherited;
end;

{ TposStuffItem }

procedure TposStuffItem.Click;
begin
  if FStuff <> nil then
    FStuff.Click;
end;

destructor TposStuffItem.Destroy;
begin
  FStuff := nil;
  inherited;
end;

function TposStuffItem.Draw(vCanvas: TCanvas; vRect: TRect; vColor: TColor; vStates: TposDrawStates): Boolean;
begin
  Result := True;
  if (FStuff = nil) or (not FStuff.Draw(vCanvas, vRect, vColor, vStates)) then
    vCanvas.Rectangle(vRect);
end;

function TposStuffItem.GetObject: TObject;
begin
  if FStuff <> nil then
    Result := FStuff.GetObject
  else
    Result := Self;
end;

procedure TposStuffItem.SetStates(vStates: TposDrawStates);
begin
  FStates := vStates;
end;

function TposStuffItem.GetDrawSize: Integer;
begin
  if FStuff = nil then
    Result := 100
  else
    Result := FStuff.GetDrawSize;
end;

{ TposStuffItems }

procedure TposStuffItems.Clear;
begin
  inherited;
  FStuffs.Invalidate;
end;

constructor TposStuffItems.Create(AStuffs: TposStuffs);
begin
  inherited Create;
  FStuffs := AStuffs;
end;

destructor TposStuffItems.Destroy;
begin
  inherited;
end;

function TposStuffItems.GetCount: Integer;
begin
  Result := Count;
end;

function TposStuffItems.GetDrawSize: Integer;
begin
  Result := 100;
end;

function TposStuffItems.GetItem(Index: Integer): TposStuffItem;
begin
  Result := inherited Items[Index] as TposStuffItem;
end;

function TposStuffItems.GetItems(Index: Integer): IposStuff;
begin
  Result := Items[Index];
end;

procedure TposStuffItems.ItemClick(Sender: TObject);
begin
end;

procedure TposStuffItems.ItemStates(Sender: TObject; var vStates: TposDrawStates);
begin
end;

function TposStuffItems.QueryInterface({$ifdef FPC}constref{$else}const{$endif FPC} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TposStuffItems.Release: Boolean;
begin
  Result := False;
  //Auto release by interface
end;

procedure TposStuffItems.SetItem(Index: Integer; const Value: TposStuffItem);
begin
  inherited Items[Index] := Value;
end;

function TposStuffItems._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TposStuffItems._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Self.Destroy;
end;

{ TposListStuffItems }

function TposListStuffItems.GetItem(Index: Integer): TposListStuffItem;
begin
  Result := inherited Items[Index] as TposListStuffItem;
end;

procedure TposListStuffItems.SetItem(Index: Integer; const Value: TposListStuffItem);
begin
  inherited Items[Index] := Value;
end;

{ IposStuffObject }

function TposStuffObject._AddRef: Integer;
begin
  Result := 0;
end;

function TposStuffObject._Release: Integer;
begin
  Result := 0;
end;

function TposStuffObject.QueryInterface({$ifdef FPC}constref{$else}const{$endif FPC} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ TposListStuffItem }

procedure TposListStuffItem.Click;
begin
  if Item <> nil then
    Item.Click;
end;

constructor TposListStuffItem.Create;
begin
  inherited;
end;

{ TposStuffLocator }

constructor TposStuffLocator.Create(vStuffs: TposStuffs);
begin
  inherited Create;
  FStuffs := vStuffs;
end;

destructor TposStuffLocator.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TposStuffLocator.AddItem(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
var
  aItem: TposListStuffItem;
begin
  aItem := TposListStuffItem.Create;
  aItem.Rect := R;
  aItem.Item := Stuff;
  aItem.Index := Index;
  FItems.Add(aItem);
  Result := True; //Continue enum
end;

function TposStuffLocator.CheckItemXY(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
var
  PT: TPoint;
begin
  PT.X := FX;
  PT.Y := FY;
  if PtInRect(R, PT) then
  begin
    FItem := TposListStuffItem.Create;
    FItem.Rect := R;
    FItem.Item := Stuff;
    FItem.Index := Index;
    Result := False;
  end
  else
    Result := True;
end;

function TposStuffLocator.CheckItemIndex(Index: Integer; R: TRect;
  Stuff: IposStuff): Boolean;
begin
  if FIndex = Index then
  begin
    FItem := TposListStuffItem.Create;
    FItem.Rect := R;
    FItem.Item := Stuff;
    FItem.Index := Index;
    Result := False;
  end
  else
    Result := True;
end;

function TposStuffLocator.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TposStuffLocator.GetItem(Index: Integer): TposListStuffItem;
begin
  Result := FItems[Index];
end;

procedure TposStuffLocator.CreateList(vTopIndex: Integer; Reverse: Boolean; vRect: TRect);
begin
  FItems := TposListStuffItems.Create;
  EnumStuffs(vTopIndex, Reverse, vRect, AddItem);
end;

function TposStuffLocator.CheckXY(vTopIndex: Integer; Reverse: Boolean;
  vRect: TRect; X, Y: Integer): TposListStuffItem;
begin
  FX := X;
  FY := Y;
  FItem := nil;
  EnumStuffs(vTopIndex, Reverse, vRect, CheckItemXY);
  Result := FItem;
end;

function TposStuffLocator.CalcRect(Index: Integer; R: TRect; Stuff: IposStuff): Boolean;
begin
  //Just ToDo
  Result := False;
end;

function TposStuffLocator.CheckIndex(vTopIndex: Integer; Reverse: Boolean;
  vRect: TRect; Index: Integer): TposListStuffItem;
begin
  FIndex := Index;
  FItem := nil;
  EnumStuffs(vTopIndex, Reverse, vRect, CheckItemIndex);
  Result := FItem;
end;

function TposStuffLocator.IndexFound(vTopIndex: Integer; Reverse: Boolean;
  vRect: TRect; Index: Integer): Boolean;
begin
  FIndex := Index;
  FItem := nil;
  EnumStuffs(vTopIndex, Reverse, vRect, CheckItemIndex);
  Result := FItem <> nil;
  FItem.Free;
end;

function TposStuffLocator.EnumStuffs(vTopIndex: Integer; Reverse: Boolean; vRect: TRect; Proc: TLocatorProc): Integer;
var
  i: Integer;
  r: TRect;
  aItemHeight: Integer;
  w: Integer;
  procedure SetRightNow(Stuff: IposStuff);
  var
    aSize: Integer;
  begin
    with FStuffs do
    begin
      aSize := Stuff.DrawSize;
      if FColumns > 0 then
        r.Right := r.Left + w
      else
        r.Right := r.Left + ((aSize * ButtonWidth) div 100);
    end;
  end;
var
  aCol, aRow: Integer;
  function EOI: Boolean;
  begin
    with FStuffs do
    begin
      Result := ((Reverse and (i < 0)) or (not Reverse and (i >= Items.Count)));
    end;
  end;
var
  aBidiRect: TRect;
begin
  inherited;
  Result := -1;
  with FStuffs do
    if FColumns > 0 then
      //left and right 0..99  99 - 0+1 = 100 pixel
      w := ((vRect.Right - vRect.Left + 1) - (SpaceSize * (FColumns - 1)) - 1) div FColumns
    else
      w := 0;

  with FStuffs do
  begin
    if (Items <> nil) then
    begin
      i := vTopIndex;
      r := vRect;
      if i < Items.Count then
      begin
        if FRows > 0 then
          aItemHeight := (vRect.Bottom - vRect.Top + SpaceSize) div FRows - SpaceSize
        else
          aItemHeight := ((Items.DrawSize * ItemHeight) div 100);
        aRow := 0;
        r.Bottom := r.Top + aItemHeight;
        while (r.Bottom <= vRect.Bottom) do
        begin
          if aRow > 0 then
            r.Top := r.Top + SpaceSize;
          r.Left := vRect.Left;
          SetRightNow(Items[i]);
          aCol := 0;
          while (r.Right <= vRect.Right) do
          begin
            if @Proc <> nil then
            begin
              aBidiRect := R;
              if FStuffs.UseRightToLeftAlignment then
              begin
                aBidiRect.Right := vRect.Right - R.Left + vRect.Left;
                aBidiRect.Left := vRect.Right - R.Right + vRect.Left;
              end;
              if not Proc(i, aBidiRect, Items[i]) then
                Exit;
            end;

            Result := i;
            if Reverse then
              Dec(i)
            else
              Inc(i);
            Inc(aCol);
            if (EOI) or ((FColumns > 0) and (aCol >= FColumns)) then
              break;
            r.Left := r.Right;
            if aCol > 0 then
              r.Left := r.Left + SpaceSize;
            SetRightNow(Items[i]);
          end;
          if EOI then
            break;
          Inc(aRow);
          r.Top := r.Bottom;
          r.Bottom := r.Top + aItemHeight;
        end;
      end;
    end;
  end;
end;

end.

