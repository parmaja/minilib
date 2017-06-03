{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
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

  { TReceiver }

  TReceiver = class(TObject)
  public
    Finished: Boolean; //temporary
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
  end;

  { TWedge wedge stake pin}

  TWedge = class(TObject)
  private
    function PointToWedgeRect(P: TPoint): TRect;
  protected
    Point: TPoint;
    procedure Paint(vCanvas: TCanvas; MainSelected: Boolean); virtual;
    function HitTest(P: TPoint): Boolean; virtual;
  end;

  { TWedges }

  TWedges = class(specialize GItems<TWedge>)
  private
  public
    function Add(x, y: Integer): TWedge;
    procedure Paint(vCanvas: TCanvas; MainSelected: Boolean); virtual;
    function HitTest(P: TPoint; out vWedgeIndex: Integer): Boolean;
  end;

  { TElement }

  TElement = class(TPersistent)
  private
    FCaptured: Boolean;
    FDesignX: Integer;
    FDesignY: Integer;
    FColor: TColor;
    FWedges: TWedges;
    FWedgeIndex: Integer;
    FContainer: TContainer;
    FModified: Integer;
    FVisible: Boolean;
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetContainer(const Value: TContainer);
    function GetModified: Boolean;
  protected
    function ScalePoint(P: TPoint): TPoint;
    function ScaleRect(R: TRect): TRect;
    function SnapPoint(P: TPoint): TPoint;
    function SnapRect(R: TRect): TRect;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure BeginModify; virtual;
    procedure Modifing; virtual;
    procedure EndModify; virtual;
    procedure Change; virtual;
    function GetClientRect: TRect; virtual;
    procedure CreateWedgeList; virtual;
  public
    constructor Create(AOwner: TContainer); overload; virtual;
    constructor Create(AOwner: TContainer; X: Integer; Y: Integer); overload; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Loaded; virtual;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); virtual;
    function InWedge(X, Y: Integer; out vWedgeIndex: Integer): Boolean; overload; virtual;
    function InWedge(X, Y: Integer): Boolean; overload;
    procedure CatchMouse(X, Y: Integer); virtual;
    property Captured: Boolean read FCaptured write FCaptured;
    property Modified: Boolean read GetModified write SetModified;
    property Visible: Boolean read FVisible write FVisible default true;
    procedure Invalidate; virtual;

    procedure SetCursor(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Modify(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); virtual;
    procedure Move(DX, DY: Integer); virtual;
    function HitTest(X, Y: Integer): Boolean; virtual;
    property Color: TColor read FColor write FColor;
    property Selected: Boolean read GetSelected write SetSelected;
    property DesignX: Integer read FDesignX;
    property DesignY: Integer read FDesignY;
    property Container: TContainer read FContainer write SetContainer;
    property Wedges: TWedges read FWedges;
  end;

  TElementClass = class of TElement;

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

  TCustomBoard = class;

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
    function HitTest(X, Y: Integer; out vElement: TElement): Boolean; virtual;
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
    procedure PaintBackground(vCanvas: TCanvas); override;
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
    function HitTest(X, Y: Integer; out vElement: TElement): Boolean; override;
    procedure Paint(vCanvas: TCanvas); override;
    procedure PaintBackground(vCanvas: TCanvas); override;
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
    function GetDesignElement: TElement;
    procedure SetBoardHeight(AValue: Integer);
    procedure SetBoardWidth(AValue: Integer);
    procedure SetDesignElement(const Value: TElement);
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
    procedure Update(vStateUpdate: TBoardUpdate);
    procedure CheckUpdate;
    procedure EndUpdate;
    procedure SizeChanged;

    function DoGetCreateElement(X, Y: Integer; out vElement: TElement): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property DesignElement: TElement read GetDesignElement write SetDesignElement;
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

//-----------------------------------

  { TPolygonElement }

  TPolygonElement = class(TElement)
  private
    Polygon: TPointArray;
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Move(DX, DY: Integer); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    procedure CreateWedgeList; override;
  end;

  TCariesElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { TSizableElement }

  TSizableElement = class abstract(TElement)
  private
    FBoundRect: TRect;
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    procedure BeginModify; override;
    procedure EndModify; override;
  public
    procedure Loaded; override;
    procedure CreateWedgeList; override;
    procedure SetCursor(Shift: TShiftState; X, Y: Integer); override;
    function HitTest(X, Y: Integer): Boolean; override;

    procedure Move(DX, DY: Integer); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
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
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { TRectangleElement }

  TRectangleElement = class(TSizableElement)
  public
    constructor Create(AOwner: TContainer); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    property Color: TColor read FColor write FColor default clGreen;
  end;

  { TCircleElement }

  TCircleElement = class(TSizableElement)
  public
    constructor Create(AOwner: TContainer); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    property Color: TColor read FColor write FColor default clBlue;
  end;

  { TPNGImageElement }

  TPNGImageElement = class(TSizableElement)
  public
    Image: TPortableNetworkGraphic;
    constructor Create(AOwner: TContainer); override;
    destructor Destroy; override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { TDebateElement }

  TDebateElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { THeavyElement }

  THeavyElement = class(TElement)
  private
    function GetBounds: TRect;
  protected
    DesignRect: TRect;
  public
    procedure Move(DX, DY: Integer); override;
    procedure CreateWedgeList; override;
    procedure SetCursor(Shift: TShiftState; X, Y: Integer); override;
    function HitTest(X, Y: Integer): Boolean; override;
    procedure EndModify; override;
    procedure DoPaint(vCanvas: TCanvas; vRect: TRect); virtual;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    constructor Create(AOwner: TContainer); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
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

{ TReceiver }

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

procedure TPNGImageElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited Paint(vCanvas, vRect);
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
  vCanvas.Pen.Color := clSilver;
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

procedure TCircleElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := Color;
  vCanvas.Brush.Style := bsSolid;
  vCanvas.Ellipse(FBoundRect);
end;

{ TWedges }

function TWedges.Add(x, y: Integer): TWedge;
begin
  Result := TWedge.Create;
  Result.Point.x := x;
  Result.Point.y := y;
  inherited Add(Result);
end;

procedure TWedges.Paint(vCanvas: TCanvas; MainSelected: Boolean);
var
  i: Integer;
begin
  inherited;
  vCanvas.Brush.Color := clBlack;
  for i := 0 to Count - 1 do
  begin
    Items[i].Paint(vCanvas, MainSelected);
  end;
end;

function TWedges.HitTest(P: TPoint; out vWedgeIndex: Integer): Boolean;
var
  i: Integer;
begin
  vWedgeIndex := -1;
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Items[i].HitTest(P) then
    begin
      vWedgeIndex := i;
      Result := True;
      break;
    end;
  end;
end;

{ TWedge }

procedure TWedge.Paint(vCanvas: TCanvas; MainSelected: Boolean);
begin
  vCanvas.Pen.Color := clGray;
  vCanvas.Brush.Style := bsSolid;
  if MainSelected then
    vCanvas.Brush.Color := clBlack
  else
    vCanvas.Brush.Color := clSilver;
  vCanvas.Rectangle(PointToWedgeRect(Point))
end;

function TWedge.HitTest(P: TPoint): Boolean;
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
    FName := 'Page';
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

function TLayouts.HitTest(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := inherited HitTest(X, Y, vElement);
  if not Result then
    for i := 0 to FLayoutList.Count - 1 do
    begin
      Result := FLayoutList[i].HitTest(X, Y, vElement);
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
    Board.Selected[i].Wedges.Paint(vCanvas, i = 0);
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
  aNewElement: Boolean;
begin
  inherited;
  if not CheckReciver or not Receiver.MouseDown(Button, Shift, X, Y) then
  begin
    aElement := nil;
    if (Button = mbLeft) and (Shift = [ssLeft, ssShift]) then
    begin
      if CurrentLayout.HitTest(X, Y, aElement) then
        Selected.Switch(aElement);
      Update([brdInvalidate]);
    end
    else if (Button = mbLeft) and (Shift = [ssLeft]) then
    begin
      if DoGetCreateElement(X, Y, aElement) then
      begin
        Selected.Clear;
        DesignElement := aElement;
      end
      else if (DesignElement = nil) or not ((DesignElement.Captured) or (DesignElement.InWedge(X, Y))) then
      begin
        Selected.Clear;
        if CurrentLayout.HitTest(X, Y, aElement) then
          DesignElement := aElement
        else
          Update([brdInvalidate]);
      end;

      if DesignElement <> nil then
        DesignElement.MouseDown(Button, Shift, X, Y);
    end
    else
    begin
      if (Button = mbRight) and (Shift = [ssRight]) then
      begin
        if CurrentLayout.HitTest(X, Y, aElement) then
          DesignElement := aElement;
      end;
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
    aElement := DesignElement;
    if (aElement = nil) or not ((aElement.Captured) or (aElement.InWedge(X, Y))) then
      CurrentLayout.HitTest(X, Y, aElement);

    if aElement <> nil then
      aElement.MouseMove(Shift, X, Y)
    else
      Cursor := crDefault;
  end;
end;

procedure TCustomBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not CheckReciver or not Receiver.MouseUp(Button, Shift, X, Y) then
  begin
    if DesignElement <> nil then
      DesignElement.MouseUp(Button, Shift, X, Y);
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
        VK_ESCAPE: DesignElement := nil;
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
          if DesignElement <> nil then
          begin
            e := CurrentLayout.Elements.IndexOf(DesignElement);
            if e < CurrentLayout.Elements.Count - 1 then
              CurrentLayout.Elements.Move(e, e + 1);
            Key := 0;
            Update([brdInvalidate]);
          end;
        VK_NEXT :
          if DesignElement <> nil then
          begin
            e := CurrentLayout.Elements.IndexOf(DesignElement);
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
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWindow;
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
    if DesignElement = nil then
      DesignElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
    else
    begin
      Selected.Clear;
      e := CurrentLayout.Elements.IndexOf(DesignElement);
      if e < 0 then
        DesignElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
      else if e > 0 then
        DesignElement := CurrentLayout.Elements[e - 1]
      else
        DesignElement := CurrentLayout.Elements[CurrentLayout.Elements.Count - 1]
    end;
  end;
end;

procedure TCustomBoard.NextDesginElement;
var
  e: Integer;
begin
  if CurrentLayout.Elements.Count > 0 then
  begin
    if DesignElement = nil then
      DesignElement := CurrentLayout.Elements[0]
    else
    begin
      Selected.Clear;
      e := CurrentLayout.Elements.IndexOf(DesignElement);
      if e < 0 then
        DesignElement := CurrentLayout.Elements[0]
      else if e < CurrentLayout.Elements.Count - 1 then
        DesignElement := CurrentLayout.Elements[e + 1]
      else
        DesignElement := CurrentLayout.Elements[0];
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
  if FOffset =AValue then Exit;
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

procedure TElement.CatchMouse(X, Y: Integer);
begin
  FDesignX := X;
  FDesignY := Y;
end;

procedure TElement.CreateWedgeList;
begin
end;

function TElement.HitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TElement.Modify(Shift: TShiftState; X, Y: Integer);
begin
  Move(X - FDesignX, Y - FDesignY);
  Modifing;
end;

procedure TElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  Container.Cursor := crDefault;
end;

procedure TElement.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CatchMouse(X, Y);
  if Container.Board .DesignElement = Self then
    InWedge(X, Y, FWedgeIndex);
  Captured := True;
end;

procedure TElement.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Captured then
  begin
    if not Modified then
      Modified := True;
    Modify(Shift, X, Y);
  end
  else if Container.Board.DesignElement = Self then
  begin
    InWedge(X, Y, FWedgeIndex);
    SetCursor(Shift, X, Y);
  end;
  CatchMouse(X, Y);
end;

procedure TElement.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Captured := False;
  FWedgeIndex := -1;
  CatchMouse(X, Y);
  if Modified then
    Modified := False
  else
    Invalidate;
end;

procedure TElement.Move(DX, DY: Integer);
begin

end;

procedure TElement.Paint(vCanvas: TCanvas; vRect: TRect);
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

procedure TCariesElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := clGreen;
  vCanvas.Polygon(Polygon);
end;

{ TSizableElement }

procedure TSizableElement.CreateWedgeList;
begin
  inherited;
  FWedges.Clear;
  with FWedges do
  begin
    Add(Left, Top);
    Add(Left + Width div 2, Top);
    Add(Right, Top);
    Add(Right, Top + Height div 2);
    Add(Right, Bottom);
    Add(Left + Width div 2, Bottom);
    Add(Left, Bottom);
    Add(Left, Top + Height div 2);
  end;
end;

function TSizableElement.HitTest(X, Y: Integer): Boolean;
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

function TSizableElement.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TSizableElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
end;

procedure TSizableElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  if Dummy then
  begin
    FWedgeIndex := 4;
    FBoundRect := Rect(X, Y, X, Y);
  end
  else
    FBoundRect := Rect(X, Y, X + 20, Y + 20);
end;

procedure TSizableElement.Move(DX, DY: Integer);
begin
  inherited;
  case FWedgeIndex of
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
  //FBoundRect := SnapRect(FBoundRect);
  Invalidate;
end;

procedure TSizableElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  if FWedgeIndex >= 0 then
  begin
    Container.Cursor := cWedgeCursors[FWedgeIndex];
  end
  else
    Container.Cursor := crDefault;
end;

procedure TSizableElement.BeginModify;
begin
  inherited;
end;

procedure TSizableElement.EndModify;
begin
  CorrectRect(FBoundRect);
  inherited;
end;

procedure TSizableElement.Loaded;
begin
  inherited;
  CorrectRect(FBoundRect);
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
    Elements[i].Paint(vCanvas, BoundRect);
  end;
end;

function TContainer.HitTest(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := False;
  vElement := nil;
  for i := Elements.Count - 1 downto 0 do
  begin
    if Elements[i].HitTest(X, Y) then
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
  if Board <> nil then
    Result := Board.Cursor
  else
    Result := crDefault;
end;

procedure TContainer.SetCursor(const Value: TCursor);
begin
  if Board <> nil then
    Board.Cursor := Value;
end;

procedure TContainer.Invalidate;
begin
  if Board <> nil then
    Board.Update([brdInvalidate]);
end;

procedure TCustomBoard.SetDesignElement(const Value: TElement);
var
  i: Integer;
begin
  if GetDesignElement <> Value then
  begin
    //Selected.Clear; todo
    i := Selected.IndexOf(Value);
    if i < 0 then
      Selected.Insert(0, Value)
    else
      Selected.Move(i, 0);
    Update([brdInvalidate]);
  end;
end;

procedure TCustomBoard.SetBoardHeight(AValue: Integer);
begin
  if FBoardHeight =AValue then Exit;
  FBoardHeight :=AValue;
  SizeChanged;
end;

function TCustomBoard.GetDesignElement: TElement;
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
begin
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
  if Board <> nil then
    Board.Change;
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

function TElement.InWedge(X, Y: Integer; out vWedgeIndex: Integer): Boolean;
begin
  Result := Wedges.HitTest(Point(x, y), vWedgeIndex);
end;

function TElement.GetSelected: Boolean;
begin
  if Container.Board <> nil then
    Result := Container.Board.DesignElement = Self
  else
    Result := False;
end;

procedure TElement.SetSelected(const Value: Boolean);
begin
  if Value and (Container.Board <> nil) then
    Container.Board.DesignElement := Self
  else
    Container.Board.DesignElement := nil;
end;

function TElement.InWedge(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := InWedge(X, Y, i);
end;

procedure TElement.SetModified(const Value: Boolean);
begin
  if Value then
  begin
    Inc(FModified);
    if FModified = 1 then
      BeginModify;
  end
  else
  begin
    if FModified <= 0 then
      raise Exception.Create('Modified Counter Unexcepted')
    else
    begin
      Dec(FModified);
      if FModified = 0 then
        EndModify;
    end;
  end;
end;

procedure TElement.BeginModify;
begin
  Invalidate;
end;

procedure TElement.Modifing;
begin
  CreateWedgeList;
  Invalidate;
end;

procedure TElement.EndModify;
begin
  Change;
  Invalidate;
end;

constructor TElement.Create(AOwner: TContainer);
begin
  inherited Create;
  FWedges := TWedges.Create;
  FVisible := True;
  Container := AOwner as TContainer; //do not use FContainer
end;

procedure TElement.Loaded;
begin
  inherited;
  Invalidate;
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
  FreeAndNil(FWedges);
  inherited;
end;

procedure TElement.Change;
begin
  CreateWedgeList;
  Container.Change;
end;

function TElement.GetClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TElement.GetModified: Boolean;
begin
  Result := FModified > 0;
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

procedure TEllipseElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Pen.Color := clBlack;
  vCanvas.Brush.Color := Color;
  vCanvas.Ellipse(FBoundRect);
end;

{ TRectangleElement }

constructor TRectangleElement.Create(AOwner: TContainer);
begin
  inherited Create(AOwner);
  FColor := clGreen;
end;

procedure TRectangleElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Pen.Color := clBlack;
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
    FWedges.Add(Polygon[i].x, Polygon[i].y);
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

procedure TPolygonElement.Paint(vCanvas: TCanvas; vRect: TRect);
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


procedure TDebateElement.Paint(vCanvas: TCanvas; vRect: TRect);
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
    FWedges.Add(Left, Top);
    FWedges.Add(Left + (Right - Left) div 2, Top);
    FWedges.Add(Right, Top);
    FWedges.Add(Right, Top + (Bottom - Top) div 2);
    FWedges.Add(Right, Bottom);
    FWedges.Add(Left + (Right - Left) div 2, Bottom);
    FWedges.Add(Left, Bottom);
    FWedges.Add(Left, Top + (Bottom - Top) div 2);
  end;
end;

procedure THeavyElement.DoPaint(vCanvas: TCanvas; vRect: TRect);
begin
end;

procedure THeavyElement.EndModify;
begin
  Invalidate;
  DesignRect := GetBounds;
  inherited;
end;

function THeavyElement.GetBounds: TRect;
begin
  Result := Container.BoundRect;
end;

function THeavyElement.HitTest(X, Y: Integer): Boolean;
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

procedure THeavyElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  if Captured then
  begin
    vCanvas.Brush.Style := bsClear;
    vCanvas.Rectangle(DesignRect);
  end
  else
    DoPaint(vCanvas, vRect);
end;

procedure THeavyElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  Container.Cursor := crHandPoint;
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

procedure TLayout.PaintBackground(vCanvas: TCanvas);
begin
  inherited;
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


