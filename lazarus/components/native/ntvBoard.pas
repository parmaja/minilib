{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
unit ntvBoard;

interface

uses
  Messages, SysUtils, Classes, Controls, LCLIntf, LCLType,
  Forms, Graphics, Contnrs, ImgList,
  StdCtrls, ExtCtrls,
  mnClasses;

const
  cWedgeCursors: array[0..7] of TCursor = (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE);
  cWedgeSize = 4;
  cBoardWidth = 600;
  cBoardHeight = 400;
  cSnapSize = 10;

type
  TPointArray = array of TPoint;

  TLayout = class;
  TLayouts = class;
  TElement = class;
  TElements = class;
  TReceiver = class;
  TContainer = class;
  TCustomBoard = class;

  TReceiverClass = class of TReceiver;

  TSizableElement = class;

  { TReceiver }

  TReceiver = class(TObject)
  public
    Finished: Boolean; //temporary
    Board: TCustomBoard;
    constructor Create(ABoard: TCustomBoard);
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
  end;

  { TWedge wedge stake pin}
  TWedgeInfo = record
    ReceiverClass: TReceiverClass;
    Cursor: TCursor;
  end;

  TWedge = class(TObject)
  private
    function PointToWedgeRect(P: TPoint): TRect;
  protected
    Point: TPoint;
    Info: TWedgeInfo;
    procedure Paint(vCanvas: TCanvas; Active: Boolean); virtual;
    function IsOver(P: TPoint): Boolean; virtual;
  end;

  { TWedges }

  TWedges = class(specialize TmnObjectList<TWedge>)
  private
  public
    function Add(x, y: Integer; Cursor: TCursor = crDefault; ReceiverClass: TReceiverClass = nil): TWedge;
    procedure Paint(Element: TSizableElement; vCanvas: TCanvas; Active: Boolean); virtual;
    procedure UpdateItems(Element: TSizableElement); virtual;
    function FindAt(Element: TSizableElement; P: TPoint; out vWedgeIndex: Integer): Boolean;
  end;

  { TSizeWedges }

  TSizeWedges = class(TWedges)
  public
    procedure UpdateItems(Element: TSizableElement); override;
    constructor Create;
  end;

  { TElement }

  TElement = class(TPersistent)
  private
    FColor: TColor;
    FContainer: TContainer;
    FUpdateCount: Integer;
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    procedure SetContainer(const Value: TContainer);
  protected
    function ScalePoint(P: TPoint): TPoint;
    function ScaleRect(R: TRect): TRect;
    function SnapPoint(P: TPoint): TPoint;
    function SnapRect(R: TRect): TRect;

    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;

    procedure BeginUpdate; virtual;
    procedure Updating; virtual;
    procedure EndUpdate; virtual;
    procedure Update(Shift: TShiftState; X, Y: Integer); virtual;

    procedure Change; virtual;
  public
    constructor Create(AOwner: TContainer); overload; virtual;
    constructor Create(AOwner: TContainer; X: Integer; Y: Integer); overload; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); virtual;

    procedure Invalidate; virtual;
    procedure PaintBackground(vCanvas: TCanvas); virtual;
    procedure Paint(vCanvas: TCanvas); virtual;
    procedure PaintFrames(vCanvas: TCanvas; Active: Boolean); virtual;

    function IsOver(X, Y: Integer): Boolean; virtual;

    property Color: TColor read FColor write FColor;
    property Selected: Boolean read GetSelected write SetSelected;
    property Container: TContainer read FContainer write SetContainer;
  end;

  TElementClass = class of TElement;

  { TElements }

  TElements = class(TObjectList)
  private
    function GetItem(Index: Integer): TElement;
    procedure SetItem(Index: Integer; const Value: TElement);
  public
    property Items[Index: Integer]: TElement read GetItem write SetItem; default;
  end;

  { TSelected }

  TSelected = class(TElements)
  public
    procedure Switch(vElement: TElement);
  end;

  { TContainer }

  TContainer = class(TComponent)
  private
    FElements: TElements;
    FBoundRect: TRect;
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    procedure SetBoundRect(const Value: TRect); virtual;
    procedure PaintBackground(vCanvas: TCanvas); virtual;
    procedure Paint(vCanvas: TCanvas); virtual;
  public
    Board: TCustomBoard;
    Index: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Invalidate; virtual;
    procedure Change; virtual;
    function GetLayoutByPoint(X, Y: Integer): TLayout; virtual;
    function GetLayoutByIndex(vIndex: Integer): TLayout; virtual;
    procedure Init; virtual;
    function ElementAt(X, Y: Integer; out vElement: TElement): Boolean;
    property Elements: TElements read FElements;
    property BoundRect: TRect read FBoundRect write SetBoundRect;
    property Cursor: TCursor read GetCursor write SetCursor;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { TLayout }

  TLayout = class(TContainer)
  private
    FLayouts: TLayouts;
    FEnabled: Boolean;
    FName: string;
  protected
    procedure SetBoundRect(const Value: TRect); override;
  public
    function GetLayoutByIndex(vIndex: Integer): TLayout; override;
    function GetLayoutByPoint(X, Y: Integer): TLayout; override;
    constructor Create(AOwner: TComponent); override;
    property Name: string read FName write FName;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  { TPageLayout }

  TPageLayout = class(TLayout)
  protected
    procedure PaintBackground(vCanvas: TCanvas); override;
  end;

  TLayoutList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLayout;
    procedure SetItem(Index: Integer; AObject: TLayout);
  public
    property Items[Index: Integer]: TLayout read GetItem write SetItem; default;
  end;

  { TLayouts }

  TLayouts = class(TContainer)
  private
    FLayoutList: TLayoutList;
    FCaption: String;
    FBackground: TColor;
    function GetItem(vIndex: Integer): TLayout;
    procedure SetItem(vIndex: Integer; AValue: TLayout);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function ElementAt(X, Y: Integer; out vElement: TElement): Boolean;
    procedure PaintBackground(vCanvas: TCanvas); override;
    procedure Paint(vCanvas: TCanvas); override;
    property LayoutList: TLayoutList read FLayoutList;
    property Background: TColor read FBackground write FBackground;
    property Items[vIndex: Integer]: TLayout read GetItem write SetItem; default;
  published
    property Caption: String read FCaption write FCaption;
  end;

  TOnGetCreateElement= procedure (Sender: TObject; X, Y: Integer; var vElement: TElement) of object;
  TBoardUpdate = set of (brdInvalidate);

  { TCustomBoard }

  TCustomBoard = class(TCustomControl)
  private
    FSizeWedges: TSizeWedges;
    FBoardHeight: Integer;
    FBoardWidth: Integer;
    FOnGetCreateElement: TOnGetCreateElement;
    FSnapSize: Integer;
    FScaleSize: Integer;
    FOffset: TPoint;
    FCurrentLayout: TContainer;
    FLayouts: TLayouts;
    FStateUpdate: TBoardUpdate;
    FUpdateCount: Integer;
    FSelected: TSelected;

    FScrollBars: TScrollStyle;
    function GetActiveElement: TElement;
    procedure SetBoardHeight(AValue: Integer);
    procedure SetBoardWidth(AValue: Integer);
    procedure SetActiveElement(const Value: TElement);
    procedure SetCurrentLayout(const Value: TContainer);
    procedure SetOffset(AValue: TPoint);
    procedure SetScaleSize(AValue: Integer);
    procedure SetScrollBars(AValue: TScrollStyle);
    procedure SetSnapSize(AValue: Integer);
  protected
    Receiver: TReceiver;
    procedure PostReceiver(AReceiver: TReceiver);
    function CheckReciver: Boolean;
    function MapToCanvas(P: TPoint): TPoint;
    function MapToScreen(P: TPoint): TPoint;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;

    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; virtual;
    procedure Reset; virtual;

    procedure NextDesginElement;
    procedure PreviousDesginElement;
    procedure DeleteDesginElement;

    procedure BeginUpdate;
    procedure Update(vStateUpdate: TBoardUpdate); reintroduce;
    procedure CheckUpdate;
    procedure EndUpdate;
    procedure SizeChanged;

    function DoGetCreateElement(X, Y: Integer; out vElement: TElement): Boolean; virtual;

    property SizeWedges: TSizeWedges read FSizeWedges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property ActiveElement: TElement read GetActiveElement write SetActiveElement;
    property CurrentLayout: TContainer read FCurrentLayout write SetCurrentLayout;
    property Offset: TPoint read FOffset write SetOffset;
    property Selected: TSelected read FSelected;
  published
    property SnapSize: Integer read FSnapSize write SetSnapSize default cSnapSize;
    property ScaleSize: Integer read FScaleSize write SetScaleSize default 0;
    property BoardWidth: Integer read FBoardWidth write SetBoardWidth default cBoardWidth;
    property BoardHeight: Integer read FBoardHeight write SetBoardHeight default cBoardHeight;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Color default clWindow;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Caption;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Visible;
    property OnGetCreateElement: TOnGetCreateElement read FOnGetCreateElement write FOnGetCreateElement;
  end;

  TntvBoard = class(TCustomBoard)
  end;

{---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------}

  { TVisualReceiver }

  TVisualReceiver = class(TReceiver)
  public
    Element: TSizableElement;
    constructor Create(ABoard: TCustomBoard; AElement: TSizableElement);
  end;

{ TMoveReceiver }

  TMoveReceiver = class(TVisualReceiver)
  private
  protected
    LastX, LastY: Integer;
  public
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  end;

  { TSizeReceiver }

  TSizeReceiver = class(TVisualReceiver)
  private
  protected
    LastX, LastY: Integer;
    Kind: Integer;
  public
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  end;

  { TVisualElement }

  TVisualElement = class abstract(TElement)
  private
    FCaptured: Boolean;
    FVisible: Boolean;
  public
    constructor Create(AOwner: TContainer); overload; override;
    property Captured: Boolean read FCaptured write FCaptured;
    property Visible: Boolean read FVisible write FVisible default true;
  end;

  { TSizableElement }

  TSizableElement = class abstract(TVisualElement)
  private
    FBoundRect: TRect;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    procedure Update(Shift: TShiftState; X, Y: Integer); override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;

    procedure Updating; override;
    procedure EndUpdate; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TContainer); override;
    destructor Destroy; override;
    procedure CreateWedgeList; virtual;
    function IsOver(X, Y: Integer): Boolean; override;
    procedure SetBoundRect(NewBounds: TRect);

    procedure Move(DX, DY: Integer); virtual;
    procedure Resize(DX1, DY1, DX2, DY2: Integer); virtual;
    procedure Paint(vCanvas: TCanvas); override;
    procedure PaintFrames(vCanvas: TCanvas; Active: Boolean); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property BoundRect: TRect read FBoundRect write SetBoundRect;
    procedure CorrectSize; virtual;
  published
    property Top: Integer read FBoundRect.Top write FBoundRect.Top;
    property Left: Integer read FBoundRect.Left write FBoundRect.Left;
    property Right: Integer read FBoundRect.Right write FBoundRect.Right;
    property Bottom: Integer read FBoundRect.Bottom write FBoundRect.Bottom;
  end;

  { TEllipseElement }

  TEllipseElement = class(TSizableElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas); override;
  end;

  { TRectangleElement }

  TRectangleElement = class(TSizableElement)
  public
    constructor Create(AOwner: TContainer); override;
    procedure Paint(vCanvas: TCanvas); override;
    property Color: TColor read FColor write FColor default clGreen;
  end;

  { TCircleElement }

  TCircleElement = class(TSizableElement)
  public
    constructor Create(AOwner: TContainer); override;
    procedure Paint(vCanvas: TCanvas); override;
    property Color: TColor read FColor write FColor default clBlue;
  end;

  { TPNGImageElement }

  TPNGImageElement = class(TSizableElement)
  public
    Image: TPortableNetworkGraphic;
    constructor Create(AOwner: TContainer); override;
    destructor Destroy; override;
    procedure Paint(vCanvas: TCanvas); override;
  end;

  { THeavyElement }

  THeavyElement = class(TSizableElement)
  private
    function GetBounds: TRect;
  protected
    DesignRect: TRect;
  public
    procedure Move(DX, DY: Integer); override;
    procedure CreateWedgeList; override;
    function IsOver(X, Y: Integer): Boolean; override;
    procedure EndUpdate; override;
    procedure DoPaint(vCanvas: TCanvas); virtual;
    procedure Paint(vCanvas: TCanvas); override;
    constructor Create(AOwner: TContainer); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
  end;

  { TPolygonElement }

  TPolygonElement = class(TSizableElement)
  private
    Polygon: TPointArray;
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Move(DX, DY: Integer); override;
    procedure Paint(vCanvas: TCanvas); override;
    procedure CreateWedgeList; override;
  end;

  TCariesElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas); override;
  end;

  { TDebateElement }

  TDebateElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas); override;
  end;

procedure RegisterElements(const Layout: String; TElements: array of TElementClass);

var
  ElementClasses: TList;

implementation

uses
  Types;

procedure CorrectRect(var vRect: TRect);
var
  i: Integer;
begin
  if vRect.Left > vRect.Right then
  begin
    i := vRect.Left;
    vRect.Left := vRect.Right;
    vRect.Right := i;
  end;
  if vRect.Top > vRect.Bottom then
  begin
    i := vRect.Top;
    vRect.Top := vRect.Bottom;
    vRect.Bottom := i;
  end;
end;

procedure RegisterElements(const Layout: String; TElements: array of TElementClass);
var
  i: Integer;
begin
  for i := 0 to Length(TElements) - 1 do
  begin
    ElementClasses.Add(TElements[i]);
    RegisterClass(TElements[i]);
  end;
end;

{ TSizeWedges }

procedure TSizeWedges.UpdateItems(Element: TSizableElement);
begin
  inherited;
  with Element do
  begin
    Self.Add(Left, Top);
    Self.Add(Left + Width div 2, Top);
    Self.Add(Right, Top);
    Self.Add(Right, Top + Height div 2);
    Self.Add(Right, Bottom);
    Self.Add(Left + Width div 2, Bottom);
    Self.Add(Left, Bottom);
    Self.Add(Left, Top + Height div 2);
  end
end;

constructor TSizeWedges.Create;
begin
  inherited;
end;

{ TVisualElement }

constructor TVisualElement.Create(AOwner: TContainer);
begin
  inherited Create(AOwner);
  FVisible := True;
end;

{ TSizeReceiver }

function TSizeReceiver.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  LastX := X;
  LastY := Y;
  Result := True;
end;

function TSizeReceiver.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Element.Move(X - LastX, Y - LastY);
  LastX := X;
  LastY := Y;
  Result := True;
end;

function TSizeReceiver.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  inherited;
  Finished := True;
  Result := True;
end;

{ TVisualReceiver }

constructor TVisualReceiver.Create(ABoard: TCustomBoard; AElement: TSizableElement);
begin
  inherited Create(ABoard);
  Element := AElement;
end;

{ TMoveReceiver }

function TMoveReceiver.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  LastX := X;
  LastY := Y;
  Result := True;
end;

function TMoveReceiver.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Board.Selected.Count -1 do
  begin
    if Board.Selected[i] is TSizableElement then
      (Board.Selected[i] as TSizableElement).Move(X - LastX, Y - LastY);
  end;
  LastX := X;
  LastY := Y;
  Result := True;
end;

function TMoveReceiver.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  inherited;
  Finished := True;
  Result := True;
end;

{ TReceiver }

constructor TReceiver.Create(ABoard: TCustomBoard);
begin
  inherited Create;
  if ABoard = nil then
    raise Exception.Create('You cant create TReceiver without owner');
  Board := ABoard;
end;

function TReceiver.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TReceiver.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TReceiver.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TReceiver.MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TReceiver.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

{ TPNGImageElement }

constructor TPNGImageElement.Create(AOwner: TContainer);
begin
  inherited;
  Image := TPortableNetworkGraphic.Create;
end;

destructor TPNGImageElement.Destroy;
begin
  FreeAndNil(Image);
  inherited Destroy;
end;

procedure TPNGImageElement.Paint(vCanvas: TCanvas);
begin
  inherited;
end;

{ TSelected }

procedure TSelected.Switch(vElement: TElement);
var
  i: Integer;
begin
  i := IndexOf(vElement);
  if i < 0 then
    Add(vElement)
  else
    Remove(vElement);
end;

{ TPageLayout }

procedure TPageLayout.PaintBackground(vCanvas: TCanvas);
var
  x, y: Integer;
begin
  inherited;
  vCanvas.Pen.Color := $E0E0E0;
  vCanvas.Pen.Width := 1;
  vCanvas.Frame(0, 0, Board.BoardWidth, Board.BoardHeight);
  x := 0;
  while x < Board.BoardWidth - 1 do
  begin
    vCanvas.Line(x, 0, x, Board.BoardHeight - 1);
    x := x + 10;
  end;
  y := 0;
  while y < Board.BoardHeight - 1 do
  begin
    vCanvas.Line(0, y, Board.BoardWidth - 1, y);
    y := y + 10;
  end;
end;

{ TCircleElement }

constructor TCircleElement.Create(AOwner: TContainer);
begin
  inherited;
  Color := clBlue;
end;

procedure TCircleElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Pen.Color := clBlack;
  vCanvas.Pen.Width := 1;
  vCanvas.Brush.Color := Color;
  vCanvas.Brush.Style := bsSolid;
  vCanvas.Ellipse(FBoundRect);
end;

{ TWedges }

function TWedges.Add(x, y: Integer; Cursor: TCursor; ReceiverClass: TReceiverClass): TWedge;
begin
  Result := TWedge.Create;
  Result.Point.x := x;
  Result.Point.y := y;
  Result.Info.Cursor := Cursor;
  Result.Info.ReceiverClass := ReceiverClass;
  inherited Add(Result);
end;

procedure TWedges.Paint(Element: TSizableElement; vCanvas: TCanvas; Active: Boolean);
var
  i: Integer;
begin
  inherited;
  UpdateItems(Element);
  vCanvas.Brush.Color := clBlack;
  for i := 0 to Count - 1 do
  begin
    Items[i].Paint(vCanvas, Active);
  end;
end;

procedure TWedges.UpdateItems(Element: TSizableElement);
begin
  Clear;
end;

function TWedges.FindAt(Element: TSizableElement; P: TPoint; out vWedgeIndex: Integer): Boolean;
var
  i: Integer;
begin
  UpdateItems(Element);
  vWedgeIndex := -1;
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Items[i].IsOver(P) then
    begin
      vWedgeIndex := i;
      Result := True;
      break;
    end;
  end;
end;

{ TWedge }

procedure TWedge.Paint(vCanvas: TCanvas; Active: Boolean);
begin
  vCanvas.Pen.Color := clGray;
  vCanvas.Pen.Width := 1;
  vCanvas.Brush.Style := bsSolid;
  if Active then
    vCanvas.Brush.Color := clBlack
  else
    vCanvas.Brush.Color := clSilver;
  vCanvas.Rectangle(PointToWedgeRect(Point))
end;

function TWedge.IsOver(P: TPoint): Boolean;
begin
  Result := PtInRect(PointToWedgeRect(Point), P);
end;

function TWedge.PointToWedgeRect(P: TPoint): TRect;
begin
  Result := Rect(P.X - cWedgeSize, P.Y - cWedgeSize, P.X + cWedgeSize, P.Y + cWedgeSize);
end;

{ TLayouts }

procedure TLayouts.Assign(Source: TPersistent);
begin
  inherited;
end;

procedure TLayouts.Clear;
var
  i: Integer;
begin
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].Clear;
  end;
  inherited;
end;

constructor TLayouts.Create(AOwner: TComponent);
begin
  inherited;
  Background := $00DEE9FA;
  FLayoutList := TLayoutList.Create;
  with TPageLayout.Create(Self) do
  begin
    FName := 'Sheet';
  end;
  {with TLayout.Create(Self) do
  begin
    FName := 'Bottom';
  end;}
end;

destructor TLayouts.Destroy;
begin
  FreeAndNil(FLayoutList);
  inherited;
end;

function TLayouts.ElementAt(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Result then
    for i := 0 to FLayoutList.Count - 1 do
    begin
      Result := FLayoutList[i].ElementAt(X, Y, vElement);
      if Result then
        break;
    end;
end;

procedure TLayouts.Paint(vCanvas: TCanvas);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].Paint(vCanvas);
  end;

  for i := 0 to Board.Selected.Count - 1 do
  begin
    if (Board.Selected[i] is TSizableElement) then //selected should be always TSizableElement
      Board.SizeWedges.Paint(Board.Selected[i] as TSizableElement, vCanvas, i = 0);
  end
end;

procedure TLayouts.PaintBackground(vCanvas: TCanvas);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].PaintBackground(vCanvas);
  end;
end;

function TLayouts.GetItem(vIndex: Integer): TLayout;
begin
  Result := FLayoutList[vIndex];
end;

procedure TLayouts.SetItem(vIndex: Integer; AValue: TLayout);
begin
  FLayoutList[vIndex] := AValue;
end;

{ TLayoutList }

function TLayoutList.GetItem(Index: Integer): TLayout;
begin
  Result := TLayout(inherited Items[Index]);
end;

procedure TLayoutList.SetItem(Index: Integer; AObject: TLayout);
begin
  inherited Items[Index] := AObject;
end;

{ TCustomBoard }

procedure TCustomBoard.Change;
begin
end;

constructor TCustomBoard.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FScrollBars := ssBoth;
  FBoardWidth := cBoardWidth;
  FBoardHeight := cBoardHeight;
  FSnapSize := cSnapSize;
  DoubleBuffered := False;
  FSizeWedges := TSizeWedges.Create;
  FSelected := TSelected.Create(False); //before layouts
  FLayouts := TLayouts.Create(Self);
  FLayouts.Init;
  FCurrentLayout := FLayouts[0];
  Width := 100;
  Height := 100;
  Color := clWindow;
end;

destructor TCustomBoard.Destroy;
begin
  FreeAndNil(FLayouts);
  FreeAndNil(FSelected);
  FreeAndNil(FSizeWedges);
  FreeAndNil(Receiver);
  inherited;
end;

procedure TCustomBoard.DoEnter;
begin
  inherited;
  //Update([brdInvalidate]);
end;

procedure TCustomBoard.DoExit;
begin
  inherited;
  //Update([brdInvalidate]);
end;

procedure TCustomBoard.Loaded;
begin
  inherited;
  CurrentLayout.BoundRect := ClientRect;
end;

procedure TCustomBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aElement: TElement;
begin
  inherited;
  if not CheckReciver or not Receiver.MouseDown(Button, Shift, X, Y) then
  begin
    aElement := nil;
    if (Button = mbLeft) and (Shift = [ssLeft]) then //Normal Click
    begin
      if DoGetCreateElement(X, Y, aElement) then //Checking if We need to create new component
        ActiveElement := aElement;

      if (ActiveElement = nil) or not ActiveElement.MouseDown(Button, Shift, X, Y) then
      begin
        CurrentLayout.ElementAt(X, Y, aElement);
        if (ActiveElement <> aElement) then
        begin
          ActiveElement := aElement;
          if ActiveElement <> nil then
            ActiveElement.MouseDown(Button, Shift, X, Y);
        end;
      end;
    end
    else if (Button = mbLeft) and (Shift = [ssLeft, ssShift]) then
    begin
      if CurrentLayout.ElementAt(X, Y, aElement) then
        Selected.Switch(aElement);
      Update([brdInvalidate]);
    end
    else if (Button = mbRight) and (Shift = [ssRight]) then
    begin
      if CurrentLayout.ElementAt(X, Y, aElement) then
        ActiveElement := aElement;
    end
  end
end;

procedure TCustomBoard.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aElement: TElement;
begin
  inherited;
  if not CheckReciver or not Receiver.MouseMove(Shift, X, Y) then
  begin
    if (ActiveElement = nil) or not ActiveElement.MouseMove(Shift, X, Y) then
      Cursor := crDefault;
  end
  else
    Cursor := crDefault;
end;

procedure TCustomBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not CheckReciver or not Receiver.MouseUp(Button, Shift, X, Y) then
  begin
    if ActiveElement <> nil then
      ActiveElement.MouseUp(Button, Shift, X, Y);
  end;
end;

function TCustomBoard.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  inherited;
  if not CheckReciver or not Receiver.MouseWheel(Shift, WheelDelta, MousePos) then
  begin
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
    if WheelDelta < 0 then
      ScaleSize := ScaleSize - 1
    else if WheelDelta > 0 then
      ScaleSize := ScaleSize + 1
  end;
end;

procedure TCustomBoard.KeyDown(var Key: Word; Shift: TShiftState);
var
  e: Integer;
begin
  inherited;
  if not CheckReciver or not Receiver.KeyDown(Key, Shift) then
  begin
    if Shift = [] then
      case Key of
        VK_ESCAPE: ActiveElement := nil;
        VK_TAB: NextDesginElement;
        VK_DELETE: DeleteDesginElement;
        VK_1:
        begin
          BeginUpdate;
          try
            Offset := Point(0, 0);
            ScaleSize := 0;
          finally
            EndUpdate;
          end
        end;
      end
    else if Shift = [ssShift] then
      case Key of
        VK_TAB :  PreviousDesginElement;
      end
    else if Shift = [ssCtrl] then
      case Key of
        VK_PRIOR :
          if ActiveElement <> nil then
          begin
            e := CurrentLayout.Elements.IndexOf(ActiveElement);
            if e < CurrentLayout.Elements.Count - 1 then
              CurrentLayout.Elements.Move(e, e + 1);
            Key := 0;
            Update([brdInvalidate]);
          end;
        VK_NEXT :
          if ActiveElement <> nil then
          begin
            e := CurrentLayout.Elements.IndexOf(ActiveElement);
            if e > 0 then
              CurrentLayout.Elements.Move(e - 1, e);
            Key := 0;
            Update([brdInvalidate]);
          end;
      end;
  end;
end;

//BasePaint

procedure TCustomBoard.Paint;
begin
  inherited;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  FLayouts.PaintBackground(Canvas);
  FLayouts.Paint(Canvas);
end;

procedure TCustomBoard.Reset;
begin
end;

procedure TCustomBoard.PreviousDesginElement;
var
  e: Integer;
begin
  if CurrentLayout.Elements.Count > 0 then
  begin
    if ActiveElement = nil then
      ActiveElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
    else
    begin
      e := CurrentLayout.Elements.IndexOf(ActiveElement);
      Selected.Clear;
      if e < 0 then
        ActiveElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
      else if e > 0 then
        ActiveElement := CurrentLayout.Elements[e - 1]
      else
        ActiveElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
    end;
  end;
end;

procedure TCustomBoard.NextDesginElement;
var
  e: Integer;
begin
  if CurrentLayout.Elements.Count > 0 then
  begin
    if ActiveElement = nil then
      ActiveElement := CurrentLayout.Elements[0]
    else
    begin
      e := CurrentLayout.Elements.IndexOf(ActiveElement);
      Selected.Clear;
      if e < 0 then
        ActiveElement := CurrentLayout.Elements[0]
      else if e < CurrentLayout.Elements.Count - 1 then
        ActiveElement := CurrentLayout.Elements[e + 1]
      else
        ActiveElement := CurrentLayout.Elements[0];
    end;
  end;
end;

procedure TCustomBoard.DeleteDesginElement;
begin
  if CurrentLayout.Elements.Count > 0 then
  begin
    BeginUpdate;
    try
      while Selected.Count > 0 do
        Selected[0].Free;
      Update([brdInvalidate]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomBoard.BeginUpdate;
begin
  CheckUpdate;
  FUpdateCount := FUpdateCount  + 1;
end;

procedure TCustomBoard.Update(vStateUpdate: TBoardUpdate);
begin
  FStateUpdate := FStateUpdate + vStateUpdate;
  CheckUpdate;
end;

procedure TCustomBoard.CheckUpdate;
begin
  if FUpdateCount = 0 then
  begin
    if brdInvalidate in FStateUpdate then
      Invalidate;
    FStateUpdate := [];
  end;
end;

procedure TCustomBoard.EndUpdate;
begin
  FUpdateCount := FUpdateCount  - 1;
  if FUpdateCount < 0 then
    raise Exception.Create('Board: Update count less than 0');
  CheckUpdate;
end;

procedure TCustomBoard.SizeChanged;
begin

end;

function TCustomBoard.DoGetCreateElement(X, Y: Integer; out vElement: TElement): Boolean;
begin
  vElement := nil;
  if Assigned(FOnGetCreateElement) then
  begin
    FOnGetCreateElement(Self, X, Y, vElement);
  end;
  Result := vElement <> nil;
end;

procedure TCustomBoard.Resize;
begin
  inherited;
  FLayouts.BoundRect := ClientRect;
  if not (csLoading in ComponentState) then
  begin
    FLayouts.BoundRect := ClientRect;
  end;
end;

procedure TCustomBoard.SetCurrentLayout(const Value: TContainer);
begin
  FCurrentLayout := Value;
  Update([brdInvalidate]);
end;

procedure TCustomBoard.SetOffset(AValue: TPoint);
begin
  if (FOffset.X = AValue.X) and (FOffset.Y = AValue.Y) then
     Exit;
  FOffset := AValue;
  Update([brdInvalidate]);
end;

procedure TCustomBoard.SetScaleSize(AValue: Integer);
begin
  if FScaleSize =AValue then Exit;
  FScaleSize := AValue;
  Update([brdInvalidate]);
end;

procedure TCustomBoard.SetScrollBars(AValue: TScrollStyle);
begin
  if FScrollBars =AValue then Exit;
  FScrollBars :=AValue;
end;

procedure TCustomBoard.SetSnapSize(AValue: Integer);
begin
  if FSnapSize =AValue then Exit;
  FSnapSize := AValue;
  Update([brdInvalidate]);
end;

procedure TCustomBoard.PostReceiver(AReceiver: TReceiver);
begin
  if (Receiver = nil) or not Receiver.Finished then
  begin
    FreeAndNil(Receiver);
    Receiver := AReceiver;
  end
  else
    raise Exception.Create('You cant post receiver now');
end;

function TCustomBoard.CheckReciver: Boolean;
begin
  if (Receiver <> nil) and Receiver.Finished then
    FreeAndNil(Receiver);
  Result := Receiver <> nil;
end;

function TCustomBoard.MapToCanvas(P: TPoint): TPoint;
begin
  P.x := P.x + Offset.x;
  P.y := P.y + Offset.y;
  Result := P;
end;

function TCustomBoard.MapToScreen(P: TPoint): TPoint;
begin
  P.x := P.x - Offset.x;
  P.y := P.y - Offset.y;
  Result := P;
end;

procedure TCustomBoard.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  message.Result := message.Result or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

{ TElements }

function TElements.GetItem(Index: Integer): TElement;
begin
  Result := TElement(inherited Items[Index]);
end;

procedure TElements.SetItem(Index: Integer; const Value: TElement);
begin
  inherited Items[Index] := Value;
end;

{ TElement }

function TElement.IsOver(X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TElement.Update(Shift: TShiftState; X, Y: Integer);
begin
  Updating;
end;

function TElement.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TElement.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TElement.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TElement.Paint(vCanvas: TCanvas);
begin
end;

procedure TElement.PaintFrames(vCanvas: TCanvas; Active: Boolean);
begin
end;

{ TCariesElement }

procedure TCariesElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 3);
  Polygon[0].X := 0;
  Polygon[0].Y := 0;
  Polygon[1].X := 30;
  Polygon[1].Y := 30;
  Polygon[2].X := -30;
  Polygon[2].Y := 30;
end;

procedure TCariesElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Brush.Color := clGreen;
  vCanvas.Polygon(Polygon);
end;

{ TSizableElement }

procedure TSizableElement.CreateWedgeList;
begin
  inherited;
end;

function TSizableElement.IsOver(X, Y: Integer): Boolean;
begin
  if PtInRect(FBoundRect, Point(X, Y)) then
    Result := True
  else
    Result := False;
end;

function TSizableElement.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

procedure TSizableElement.SetHeight(AValue: Integer);
begin
  FBoundRect.Bottom := FBoundRect.Top + AValue;
end;

procedure TSizableElement.SetWidth(AValue: Integer);
begin
  FBoundRect.Right := FBoundRect.Left + AValue;
end;

procedure TSizableElement.Update(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TSizableElement.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  inherited;
  if Container.Board.SizeWedges.FindAt(Self, Point(X, Y), i) then
  begin
    Result := True;
  end
  else
  begin
    if IsOver(X, Y) then
    begin
      Container.Board.PostReceiver(TMoveReceiver.Create(Container.Board, Self));
      Container.Board.Receiver.MouseDown(Button, Shift, X, Y);
      Result := True;
    end
    else
      Result := False;
  end;
end;

function TSizableElement.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  inherited;
  if Container.Board.SizeWedges.FindAt(Self, Point(X, Y), i) then
  begin
    Container.Cursor := cWedgeCursors[i];//use Wedget object, but now it is not permanent object :(
    Result := True;
  end
  else
    Result := False;
end;

function TSizableElement.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  inherited;
  Result := False; //not sure
end;

function TSizableElement.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TSizableElement.Paint(vCanvas: TCanvas);
begin
  inherited;
end;

procedure TSizableElement.PaintFrames(vCanvas: TCanvas; Active: Boolean);
begin
  inherited PaintFrames(vCanvas, Active);
end;

procedure TSizableElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  {if Dummy then
  begin
    FBoundRect := Rect(X, Y, X, Y);
  end
  else}
    FBoundRect := Rect(X, Y, X + 50, Y + 50);
end;

procedure TSizableElement.SetBoundRect(NewBounds: TRect);
begin
  FBoundRect := NewBounds;
end;

procedure TSizableElement.CorrectSize;
begin
end;

procedure TSizableElement.Move(DX, DY: Integer);
begin
  inherited;
  OffsetRect(FBoundRect, DX, DY);
  Change;
  exit;
  case 0 of
    -1:
    begin
      OffsetRect(FBoundRect, DX, DY);
    end;
    0:
    begin
      Inc(FBoundRect.Left, DX);
      Inc(FBoundRect.Top, DY);
    end;
    1:
    begin
      Inc(FBoundRect.Top, DY);
    end;
    2:
    begin
      Inc(FBoundRect.Top, DY);
      Inc(FBoundRect.Right, DX);
    end;
    3:
    begin
      Inc(FBoundRect.Right, DX);
    end;
    4:
    begin
      Inc(FBoundRect.Bottom, DY);
      Inc(FBoundRect.Right, DX);
    end;
    5:
    begin
      Inc(FBoundRect.Bottom, DY);
    end;
    6:
    begin
      Inc(FBoundRect.Bottom, DY);
      Inc(FBoundRect.Left, DX);
    end;
    7:
    begin
      Inc(FBoundRect.Left, DX);
    end;
  end;
  Invalidate;
end;

procedure TSizableElement.Resize(DX1, DY1, DX2, DY2: Integer);
begin

end;

procedure TSizableElement.Updating;
begin
  inherited Updating;
  CreateWedgeList;
end;

procedure TSizableElement.EndUpdate;
begin
  CorrectRect(FBoundRect);
  inherited;
end;

procedure TSizableElement.Change;
begin
  inherited;
  CreateWedgeList;
end;

constructor TSizableElement.Create(AOwner: TContainer);
begin
  inherited Create(AOwner);
end;

destructor TSizableElement.Destroy;
begin
  inherited Destroy;
end;

{ TContainer }

destructor TContainer.Destroy;
begin
  Elements.Clear;
  FreeAndNil(FElements);
  inherited;
end;

procedure TContainer.Paint(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
  begin
    Elements[i].Paint(vCanvas);
  end;
end;

function TContainer.ElementAt(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := False;
  vElement := nil;
  for i := Elements.Count - 1 downto 0 do
  begin
    if Elements[i].IsOver(X, Y) then
    begin
      vElement := Elements[i];
      Result := True;
      break;
    end;
  end;
end;

procedure TContainer.SetBoundRect(const Value: TRect);
begin
  FBoundRect := Value;
end;

function TContainer.GetCursor: TCursor;
begin
  Result := Board.Cursor;
end;

procedure TContainer.SetCursor(const Value: TCursor);
begin
  Board.Cursor := Value;
end;

procedure TContainer.Invalidate;
begin
  if Board <> nil then
    Board.Update([brdInvalidate]);
end;

procedure TCustomBoard.SetActiveElement(const Value: TElement);
var
  i: Integer;
begin
  if GetActiveElement <> Value then
  begin
    Selected.Clear;
    if Value <> nil then
      Selected.Add(Value);
    {
    i := Selected.IndexOf(Value);
    if i < 0 then
      Selected.Insert(0, Value)
    else
      Selected.Move(i, 0);}
    Update([brdInvalidate]);
  end;
end;

procedure TCustomBoard.SetBoardHeight(AValue: Integer);
begin
  if FBoardHeight =AValue then Exit;
  FBoardHeight :=AValue;
  SizeChanged;
end;

function TCustomBoard.GetActiveElement: TElement;
begin
  if Selected.Count > 0 then
    Result := Selected[0]
  else
    Result := nil;
end;

procedure TCustomBoard.SetBoardWidth(AValue: Integer);
begin
  if FBoardWidth =AValue then Exit;
  FBoardWidth :=AValue;
end;

procedure TContainer.PaintBackground(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
  begin
    Elements[i].PaintBackground(vCanvas);
  end;
end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  FElements := TElements.Create;
  if (AOwner <> nil) then
  begin
    if (AOwner is TCustomBoard) then
      Board := AOwner as TCustomBoard
    else if (AOwner is TLayouts) then
      Board := (AOwner as TLayouts).Board;
  end;
end;

procedure TContainer.Change;
begin
  Board.Change;
  Invalidate;
end;

procedure TContainer.Clear;
begin
  Elements.Clear;
end;

function TContainer.GetHeight: Integer;
begin
  Result := BoundRect.Bottom - BoundRect.Top;
end;

function TContainer.GetWidth: Integer;
begin
  Result := BoundRect.Right - BoundRect.Left;
end;

function TContainer.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := nil;
end;

function TContainer.GetLayoutByIndex(vIndex: Integer): TLayout;
begin
  Result := nil;
end;

procedure TContainer.Init;
begin
end;

{ TElement }

constructor TElement.Create(AOwner: TContainer; X: Integer; Y: Integer);
begin
  Create(AOwner);
  AfterCreate(X, Y, True);
end;

procedure TElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
end;

procedure TElement.Invalidate;
begin
  Container.Invalidate;
end;

procedure TElement.PaintBackground(vCanvas: TCanvas);
begin
end;

function TElement.GetSelected: Boolean;
begin
  if Container.Board <> nil then
    Result := Container.Board.ActiveElement = Self
  else
    Result := False;
end;

procedure TElement.SetSelected(const Value: Boolean);
begin
  if Value and (Container.Board <> nil) then
    Container.Board.ActiveElement := Self
  else
    Container.Board.ActiveElement := nil;
end;

procedure TElement.BeginUpdate;
begin
  FUpdateCount := FUpdateCount  + 1;
  Invalidate;
end;

procedure TElement.Updating;
begin
  Invalidate;
end;

procedure TElement.EndUpdate;
begin
  FUpdateCount := FUpdateCount  - 1;
  if FUpdateCount = 0 then
  begin
    Change;
    Invalidate;
  end;
end;

constructor TElement.Create(AOwner: TContainer);
begin
  inherited Create;
  Container := AOwner as TContainer; //do not use FContainer
end;

procedure TElement.SetContainer(const Value: TContainer);
begin
  if Value <> FContainer then
  begin
    if FContainer <> nil then
      FContainer.Elements.Extract(Self);
    FContainer := Value;
    if FContainer <> nil then
      FContainer.Elements.Add(Self);
  end;
end;

destructor TElement.Destroy;
begin
  Container.Board.Selected.Remove(Self);
  Container := nil; //remove my self from elements list
  inherited;
end;

procedure TElement.Change;
begin
  Container.Change;
end;

function TElement.ScalePoint(P: TPoint): TPoint;
begin
  with Container.Board do
  begin
    P.x := P.x + P.x * ScaleSize;
    P.y := P.y + P.y * ScaleSize;
  end;
  Result := P;
end;

function TElement.ScaleRect(R: TRect): TRect;
begin
  R.TopLeft := ScalePoint(R.TopLeft);
  R.BottomRight := ScalePoint(R.BottomRight);
  Result := R;
end;

function TElement.SnapPoint(P: TPoint): TPoint;
begin
  with Container.Board do
  begin
    P.x := P.x div SnapSize * SnapSize;
    P.y := P.y div SnapSize * SnapSize;
  end;
  Result := P;
end;

function TElement.SnapRect(R: TRect): TRect;
begin
  R.TopLeft := SnapPoint(R.TopLeft);
  R.BottomRight := SnapPoint(R.BottomRight);
  Result := R;
end;

procedure TElement.AfterConstruction;
begin
  inherited;
  Change;
end;

{ TEllipseElement }

procedure TEllipseElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  if Dummy then
  else
    FBoundRect := Rect(X, Y, X + 10, Y + 10);
end;

procedure TEllipseElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Pen.Color := clBlack;
  vCanvas.Brush.Color := Color;
  vCanvas.Ellipse(BoundRect);
end;

{ TRectangleElement }

constructor TRectangleElement.Create(AOwner: TContainer);
begin
  inherited Create(AOwner);
  FColor := clGreen;
end;

procedure TRectangleElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Pen.Color := clBlack;
  vCanvas.Pen.Width := 1;
  vCanvas.Brush.Color := Color;
  vCanvas.Brush.Style := bsSolid;
  vCanvas.Rectangle(FBoundRect);
end;

{ TPolygonElement }

procedure TPolygonElement.CreateWedgeList;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Length(Polygon) -1 do
  begin
    //FWedges.Add(Polygon[i].x, Polygon[i].y);
  end
end;

procedure TPolygonElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 1);
  Color := clRed;
end;

procedure TPolygonElement.Move(DX, DY: Integer);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Inc(Polygon[i].X, DX);
    Inc(Polygon[i].Y, DY);
  end;
  Invalidate;
end;

procedure TPolygonElement.Paint(vCanvas: TCanvas);
begin
  inherited;
end;

{ TDebateElement }

procedure TDebateElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 6);
  Polygon[0].X := 0;
  Polygon[0].Y := 0;
  Polygon[1].X := 30;
  Polygon[1].Y := 30;
  Polygon[2].X := 100;
  Polygon[2].Y := 30;
  Polygon[3].X := 100;
  Polygon[3].Y := 60;
  Polygon[4].X := 30;
  Polygon[4].Y := 60;
  Polygon[5].X := 30;
  Polygon[5].Y := 35;
end;


procedure TDebateElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Brush.Color := clInfoBk;
  vCanvas.Polygon(Polygon);
end;

{ THeavyElement }

procedure THeavyElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  if Dummy then
    OffsetRect(DesignRect, X - (DesignRect.Right - DesignRect.Left) div 2, Y - (DesignRect.Bottom - DesignRect.Top) div 2);
end;

constructor THeavyElement.Create(AOwner: TContainer);
begin
  if not (AOwner is TLayout) then
    raise Exception.Create('Must Create On Layout')
  else
  begin
    inherited;
    DesignRect := GetBounds;
  end;
end;

procedure THeavyElement.CreateWedgeList;
begin
  inherited;
  with DesignRect do
  begin
{    FWedges.Add(Left, Top);
    FWedges.Add(Left + (Right - Left) div 2, Top);
    FWedges.Add(Right, Top);
    FWedges.Add(Right, Top + (Bottom - Top) div 2);
    FWedges.Add(Right, Bottom);
    FWedges.Add(Left + (Right - Left) div 2, Bottom);
    FWedges.Add(Left, Bottom);
    FWedges.Add(Left, Top + (Bottom - Top) div 2);}
  end;
end;

procedure THeavyElement.DoPaint(vCanvas: TCanvas);
begin
end;

procedure THeavyElement.EndUpdate;
begin
  Invalidate;
  DesignRect := GetBounds;
  inherited;
end;

function THeavyElement.GetBounds: TRect;
begin
  Result := Container.BoundRect;
end;

function THeavyElement.IsOver(X, Y: Integer): Boolean;
begin
  Result := PtInRect(GetBounds, Point(X, Y));
end;

procedure THeavyElement.Move(DX, DY: Integer);
begin
  inherited;
  Invalidate;
  OffsetRect(DesignRect, DX, DY);
  Invalidate;
end;

procedure THeavyElement.Paint(vCanvas: TCanvas);
begin
  inherited;
  if Captured then
  begin
    vCanvas.Brush.Style := bsClear;
    vCanvas.Rectangle(DesignRect);
  end
  else
    DoPaint(vCanvas);
end;

{ TLayout }

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  if AOwner is TLayouts then
  begin
    FLayouts := (AOwner as TLayouts);
    Index := FLayouts.FLayoutList.Add(Self);
  end;
end;

function TLayout.GetLayoutByIndex(vIndex: Integer): TLayout;
begin
  Result := Self;
end;

function TLayout.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := Self;
end;

{ TLayout }

procedure TLayout.SetBoundRect(const Value: TRect);
begin
  inherited;
  FBoundRect := Value;
end;

initialization
  ElementClasses := TList.Create;
  RegisterElements('', [TEllipseElement]);
  RegisterElements('', [TRectangleElement]);
finalization
  FreeAndNil(ElementClasses);
end.
