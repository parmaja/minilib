unit ntvCtrls; 

{$mode objfpc}{$H+}
interface

uses
  Classes, Messages, Controls, ExtCtrls, SysUtils, Math, Contnrs, Graphics, Forms,
  LCLType, LCLIntf, LMessages, LCLProc,
  ntvutils;

type

  TntvPageControl = class;
  TntvPageItem = class;

  { TntvProgressBar }

  TntvProgressBar = class(TCustomControl)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FProgressWidth: integer;
    FProgressColor: TColor;
    FStep: integer;
    FShowProgress: boolean;

    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetProgressColor(const Value: TColor);
    procedure SetShowProgress(const Value: boolean);
  protected
    procedure EraseBackground(DC: HDC); override;
    procedure DoOnResize; override;

    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
    procedure StepIt;
    procedure Reset;
    procedure StepBy(vStep: integer);
  published
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property Step: integer read FStep write FStep default 1;
    property ShowProgress: boolean read FShowProgress write SetShowProgress;

    property ProgressColor: TColor read FProgressColor write SetProgressColor default clNavy;

    property Align;
    property Anchors;
    //property BevelEdges;
    //property BevelInner;
    //property BevelOuter;
    //property BevelKind;
    //property BevelWidth;
    property BorderWidth;
    //property Ctl3D;

    property Color;
    property Font;

    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  { TntvGauge }

  TntvGauge = class(TCustomControl)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FProgressColor: TColor;
    FStep: integer;
    FShowProgress: boolean;
    FSubStep: integer;
    FSubPosition: integer;
    FSubMax: integer;
    FSubMin: integer;
    FPaintStatus: TPaintStatusSet;
    FSubProgressColor: TColor;
    FSubHeight: integer;
    //FBorderStyle: TItemBorderStyle;
    FFrameColor: TColor;

    procedure SetMax(Value: integer);
    procedure SetMin(Value: integer);
    procedure SetPosition(Value: integer);
    procedure SetProgressColor(const Value: TColor);
    procedure SetShowProgress(Value: boolean);

    procedure SetSubMax(Value: integer);
    procedure SetSubMin(Value: integer);
    procedure SetSubPosition(Value: integer);
    procedure SetSubProgressColor(const Value: TColor);
    procedure SetSubHeight(Value: Integer);
    function GetSubHeight: integer;

    //procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    //procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    //procedure SetBorderStyle(const Value: TItemBorderStyle);
    procedure SetFrameColor(const Value: TColor);
  protected
    procedure DoOnResize; override;
    procedure EraseBackground(DC: HDC); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetClientRect: TRect; override;

    function ProgressWidth(APos, AMax, AMin: integer): integer;
    function ProgressStep: integer;

    function MainRect: TRect;
    function SubRect: TRect;
    function RemainRect: TRect;

    procedure SetText(const S:string);
    procedure RedrawBorder(const Clip: HRGN);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StepIt;
    procedure Reset;
    procedure StepBy(vStep: integer);

  published
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property Step: integer read FStep write FStep default 1;

    property SubMin: integer read FSubMin write SetSubMin default 0;
    property SubMax: integer read FSubMax write SetSubMax default 100;
    property SubHeight: integer read FSubHeight write SetSubHeight default 0;
    property SubPosition: integer read FSubPosition write SetSubPosition default 0;
    property SubStep: integer read FSubStep write FSubStep default 1;

    property ShowProgress: boolean read FShowProgress write SetShowProgress default True;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clNavy;
    property SubProgressColor: TColor read FSubProgressColor write SetSubProgressColor default clRed;
    //property BorderStyle: TItemBorderStyle read FBorderStyle write SetBorderStyle default ibsLightRaised;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBlack;

    property Align;
    property Anchors;
    //property Ctl3D;
    property BiDiMode;
    property ParentBiDiMode;

    property Color;
    property Font;

    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TntvPage = class(TCustomControl)
  private
    FItem: TntvPageItem;
  protected
    //procedure CMVisibleChanged(var Message: TLMessage); message CM_VISIBLECHANGED; belal
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Item: TntvPageItem read FItem write FItem;
  published
  end;

  { TntvPageItem }

  TntvPageItem = class(TCollectionItem)
  private
    FPage: TWinControl;
    FCaption: string;
    FPageWidth: Integer;
    FImageIndex: Integer;
    FName: string;
    FEnabled: boolean;
    FVisible: Boolean;

    procedure SetCaption(const Value: string);
    procedure SetPageWidth(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetPage(const Value: TWinControl);
    function GetPageControl: TntvPageControl;
    procedure SetVisible(const Value: Boolean);

  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    property PageControl: TntvPageControl read GetPageControl;
    property PageWidth: Integer read FPageWidth write SetPageWidth;
    property Page: TWinControl read FPage write SetPage;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Name: string read GetName write SetName;
    property Enabled: boolean read FEnabled write FEnabled;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TntvPageItemClass = class of TntvPageItem;

  { TntvPages }

  TntvPages = class(TCollection)
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
    function FindControl(Control: TWinControl): TntvPageItem;
    function AddControl(Control: TWinControl): TntvPageItem;
    function ExtractControl(Control: TWinControl): TWinControl;
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
    FPageList: TPagesList;
    FPageIndex: Integer;
    FHeaderHeight: Integer;
    FOnPageChanged: TOnPageChanged;
    FFirstPage: Integer;
    FOnSelectPage: TOnSelectPage;
    FItems: TntvPages;
    FStoreIndex: Boolean;
    FWrapper: TObject;
    FPageBorder: TPageBorder;
    FShowButtons: Boolean;
    FShowTabs: Boolean;

    TabRect: TRect;
    FTabStyle: TTabStyle;
    FUnderMouseIndex: Integer;
    OldUnderMouseIndex: Integer;
    FImageList: TImageList;

    procedure ReadWrapper(Reader: TReader);
    procedure WriteWrapper(Writer: TWriter);

    procedure SetPageIndex(Value: Integer);
    procedure SetFirstPage(const Value: Integer);
    function GetPageIndex: Integer;

    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GetDlgCode;

    procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED; //belal
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure EraseBackground(DC: HDC); override;
    //procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;

    function ChildKey(var Message: TLMKey): boolean; override;
    procedure FontChanged(Sender: TObject); override;

    procedure SetPageBorder(const Value: TPageBorder);
    procedure SetActivePage(const Value: TWinControl);
    function GetActivePage: TWinControl;
    procedure SetShowButtons(const Value: Boolean);
    procedure DoPageChanged(OldPage, NewPage: TWinControl);
    function GetPageItem(Page: TWinControl): TntvPageItem;
    procedure SetShowTabs(const Value: Boolean);
    function GetHeaderHeight: Integer;

    procedure SetTabStyle(const Value: TTabStyle);
    procedure SetUnderMouseIndex(const Value: Integer);
    procedure DrawButton(vRect: TRect; const IsDown: Boolean);
    procedure DrawVertLines(vRect: TRect; Index: Integer);
    procedure AdjustBtnRect(var vRect: TRect; Index: Integer);
    procedure DrawBottomLine(vRect: TRect);
    procedure SetImageList(const Value: TImageList);
    procedure AdjustTextRect(var vRect: TRect; vImageIndex: Integer);

  protected //dbgrids
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

    function GetControlTabRect: TRect; virtual;
    function GetPageTabRect: TRect; virtual;
    function GetPageRect: TRect; virtual;
    function GetTabRect(FirstPage, Index: Integer): TRect; overload;
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

    property FirstPage: Integer read FFirstPage write SetFirstPage;
    property HeaderHeight: Integer read GetHeaderHeight;
    function PageFromIndex(Index: Integer): TWinControl;
    procedure RedrawBorder(const Clip: HRGN);
    procedure Loaded; override;

    procedure DrawRTLHeaderBorder(vRect: TRect; vColor: TColor; const Brd3D: Boolean = True);
    procedure DrawLTRHeaderBorder(vRect: TRect; vColor: TColor; const Brd3D: Boolean = True);
    procedure DrawNormalHeader; virtual;
    procedure DrawImage(var vRect: TRect; vImageIndex: Integer);

    procedure DrawNormalTab(Index: Integer; var Rect: TRect); virtual;
    procedure InternalDrawRaisedBorder; virtual;
    procedure InternalDrawBorder; virtual;
    procedure InternalDrawShadowedBorder; virtual;
    property UnderMouseIndex: Integer read FUnderMouseIndex write SetUnderMouseIndex;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure NextPage;
    procedure PriorPage;
    property ActivePage: TWinControl read GetActivePage write SetActivePage;
    procedure UpdatePagesList;
    procedure Clear;
    property PageItem[Page: TWinControl]: TntvPageItem read GetPageItem;
  published
    property StoreIndex: Boolean read FStoreIndex write FStoreIndex;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored FStoreIndex default 0;
    property PageBorder: TPageBorder read FPageBorder write SetPageBorder default 3;

    //<Ayman>
    property TabStyle: TTabStyle read FTabStyle write SetTabStyle default tbGradientTabs;
    property ImageList: TImageList read FImageList write SetImageList;
    //</Ayman>

    property Items: TntvPages read FItems write FItems;

    property OnPageChanged: TOnPageChanged read FOnPageChanged write FOnPageChanged;
    property OnSelectPage: TOnSelectPage read FOnSelectPage write FOnSelectPage;

    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    //property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
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

uses Types;

{ TntvProgressBar }

constructor TntvProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 16;

	FMin := 0;
  FMax := 100;
  FPosition := 0;
  FProgressWidth := 0;
  FProgressColor := clNavy;
  Color := clGray;
  FStep := 1;
  Font.Color := clWhite;
  ControlStyle := ControlStyle - [csOpaque];
  //BevelKind := bkSoft;
end;

procedure TntvProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
  end;
end;

destructor TntvProgressBar.Destroy;
begin

  inherited;
end;

procedure TntvProgressBar.Paint;
var
  Tmp: string;
begin
  inherited;
  with Canvas do
  begin
    if (FShowProgress) and (FPosition <> FMin) then
    begin
      Tmp := IntToStr(Round((FPosition - FMin) * 100 / (FMax - FMin))) + ' %';
      Canvas.Font.Assign(Self.Font);
      Brush.Color := clWhite;
      //DrawString(Canvas, Tmp, ClientRect, [txtCenter, txtMiddle, txtClear]);

    end;
  end;
end;

procedure TntvProgressBar.Reset;
begin
  Position := 0;
end;

procedure TntvProgressBar.SetMax(const Value: integer);
begin
  if (FMax <> Value) and (Value > FMin) then
  begin
    FMax := Value;
    SetPosition(Position);
  end;
end;

procedure TntvProgressBar.SetMin(const Value: integer);
begin
  if (FMin <> Value) and (Value < FMax) then
	begin
    FMin := Value;
    SetPosition(Position);
  end;
end;

procedure TntvProgressBar.SetPosition(const Value: integer);
var
  aWidth: Integer;
begin
  if (Value > FMax) then FPosition := FMax else
    if (Value < FMin) then FPosition := FMin else
      FPosition := Value;

  aWidth := MulDiv(Width, (FPosition - FMin), (FMax - FMin));
  if aWidth <> FProgressWidth then
  begin
    FProgressWidth := aWidth;
    //Invalidate;
    //UpdateWindow(Handle);
    //Update;
    Refresh;
  end;
end;

procedure TntvProgressBar.SetProgressColor(const Value: TColor);
begin
  if FProgressColor <> Value then
	begin
    FProgressColor := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.SetShowProgress(const Value: boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    Invalidate;
  end;
end;

procedure TntvProgressBar.StepBy(vStep: integer);
begin
  Position := Position + vStep;
end;

procedure TntvProgressBar.StepIt;
begin
  Position := Position + FStep;
end;

procedure TntvProgressBar.EraseBackground(DC: HDC);
var
  aRect, ProgressRect: TRect;
begin
  if not (csLoading in ComponentState) then
  begin
    with Canvas do
    begin
      aRect := ClientRect;

      ProgressRect := aRect;
      //if (aRect.Left+FProgressWidth) <= aRect.Right then
      aRect.Left := aRect.Left + FProgressWidth;
      ProgressRect.Right := aRect.Left;

      Brush.Color := FProgressColor;
      //Windows.FillRect(Message.DC, ProgressRect, Canvas.Brush.Handle); belal
      FillRect(ProgressRect);
      Brush.Color := Color;
      //Windows.FillRect(Message.DC, aRect, Canvas.Brush.Handle);
      FillRect(aRect);
    end;
  end;
end;

procedure TntvProgressBar.DoOnResize;
begin
  inherited DoOnResize;
	SetPosition(Position);
end;

{ TntvGauge }

constructor TntvGauge.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csOpaque, csDoubleClicks];

  Width := 150;
  Height := 16;

	FMin := 0;
  FMax := 100;
  FPosition := 0;
  FStep := 1;

	FSubMin := 0;
  FSubMax := 100;
  FSubHeight := 0;
  FSubPosition := 0;
  FSubStep := 1;
  //FBorderStyle := ibsLightRaised;
  FShowProgress := True;


  FProgressColor := clNavy;
  FSubProgressColor := clRed;

  Color := clGray;
  Font.Color := clWhite;
  ControlStyle := ControlStyle - [csOpaque];
  FPaintStatus := [];
  //BevelKind := bkSoft;
  FFrameColor := clBlack;
end;

procedure TntvGauge.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
  end;
end;

destructor TntvGauge.Destroy;
begin

  inherited;
end;

function TntvGauge.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  //InflateRect(Result, -1, -1);
end;

function TntvGauge.GetSubHeight: integer;
begin
  if FSubHeight=0 then
    Result := Round(ClientHeight*0.30)
  else
    Result := FSubHeight;
end;

function TntvGauge.MainRect: TRect;
begin
  Result := ClientRect;
  Result.Right := ProgressWidth (Position, Max, Min);
end;

procedure TntvGauge.Paint;
var
  Tmp: string;
  aRect: TRect;
begin
  with Canvas do
  begin
    if (psSub in FPaintStatus)or(psAll in FPaintStatus) then
    begin
      Brush.Color := SubProgressColor;
      aRect := SubRect;
      FillRect(aRect);
      ExcludeRect (Canvas, aRect);
    end;

    if (psMain in FPaintStatus)or(psAll in FPaintStatus) then
    begin
      Brush.Color := FProgressColor;
      FillRect(MainRect);
      Brush.Color := Color;
      FillRect(RemainRect);
      if ShowProgress and (FPosition <> FMin) then
      begin
        Tmp := IntToStr(Round((FPosition - FMin) * 100 / (FMax - FMin))) + ' %';
        Canvas.Font.Assign(Self.Font);
        aRect := ClientRect;
        Brush.Color := Self.Color;
        //DrawString(Canvas, Tmp, aRect, [txtMiddle, txtCenter, txtClear]);
      end;
    end;
    FPaintStatus := [];
  end;
end;

function TntvGauge.ProgressStep: integer;
begin
  Result := ClientWidth div 100;
end;

function TntvGauge.ProgressWidth(APos, AMax, AMin: Integer): integer;
begin
  Result := MulDiv(ClientWidth, (APos - Min), (AMax - AMin));
end;

procedure TntvGauge.RedrawBorder(const Clip: HRGN);
var
  DC: HDC;
  RW: TRect;
  OldDC: HDC;
begin
  {if (BorderStyle <> ibsNone) then
  begin
    DC := GetWindowDC(Handle);
    OldDc := Canvas.Handle;
    Canvas.Handle := DC;
    try
      GetWindowRect(Handle, RW);
      OffsetRect(RW, -RW.Left, -RW.Top);
      DrawItemFrame(Canvas, BorderStyle, RW, FrameColor);
    finally
      ReleaseDC(Handle, dc);
      Canvas.Handle := OldDC;
    end;
  end;}

end;

function TntvGauge.RemainRect: TRect;
var
  w, mw, sw: Integer;
begin
  //SubtractRect(Result, ClientRect, MainRect);
  Result := ClientRect;
  mw := ProgressWidth (Position, Max, Min);
  sw := ProgressWidth (Position, Max, Min);
  if mw>sw then
    Inc(Result.Left, mw)
  else
    Inc(Result.Left, sw);


  {Result.Right := ProgressWidth (Position, Max, Min);

  Result := ClientRect;
  Result.Right := ProgressWidth (SubPosition, SubMax, SubMin);
  Result.Top := Result.Bottom-GetSubHeight;}

end;

procedure TntvGauge.Reset;
begin
  Position := 0;
end;

{procedure TntvGauge.SetBorderStyle(const Value: TItemBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;}

procedure TntvGauge.SetFrameColor(const Value: TColor);
begin
	if FFrameColor<>Value then
	begin
		 FFrameColor := Value;
     //Perform(CM_BORDERCHANGED, 0, 0);
	end;
end;

procedure TntvGauge.DoOnResize;
begin
  inherited DoOnResize;
	SetPosition(Position);
end;

procedure TntvGauge.EraseBackground(DC: HDC);
begin
  FPaintStatus := [psAll];
  inherited EraseBackground(DC);
end;

procedure TntvGauge.SetMax(Value: integer);
begin
  if (FMax <> Value) and (Value > FMin) then
  begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetMin(Value: integer);
begin
  if (FMin <> Value) and (Value < FMax) then
	begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetPosition(Value: integer);
var
  aVal: Integer;
  OldWidth, NewWidth: integer;
begin
  if (Value >= Max) then aVal := Max else
    if (Value <= Min) then aVal := Min else
      aVal := Value;
  if FPosition<>aVal then
  begin
    OldWidth := ProgressWidth(FPosition, Max, Min);
    NewWidth := ProgressWidth(aVal, Max, Min);
    FPosition := aVal;
    if OldWidth<>NewWidth then
    begin
      include(FPaintStatus, psMain);
      Invalidate;
      //UpdateWindow(Parent.Handle);
      //Application.ProcessMessages;
      //Refresh;
      //Parent.Refresh;
      //Invalidate;
      //Update;
      //UpdateWindow(Handle);
    end;
  end;
end;

procedure TntvGauge.SetProgressColor(const Value: TColor);
begin
  if FProgressColor <> Value then
	begin
    FProgressColor := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetShowProgress(Value: boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetSubHeight(Value: integer);
var
  aVal: Integer;
begin
  if FSubHeight<>Value then
  begin
    if Value<=0 then aVal:=0
    else if Value>ClientHeight then aVal:=ClientHeight
    else aVal := Value;
    FSubHeight := aVal;
    Invalidate;
  end;
end;

procedure TntvGauge.SetSubMax(Value: integer);
begin
  if (FSubMax <> Value) and (Value > FSubMin) then
  begin
    FSubMax := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetSubMin(Value: integer);
begin
  if (FSubMin <> Value) and (Value < FSubMax) then
	begin
    FSubMin := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetSubPosition(Value: integer);
var
  R: TRect;
  aVal: Integer;
  NeedInvalidate: boolean;
  OldWidth, NewWidth: integer;
begin
  if (Value > SubMax) then aVal := SubMax else
    if (Value < SubMin) then aVal := SubMin else
      aVal := Value;

  if aVal <> FSubPosition then
  begin
    OldWidth := ProgressWidth(FSubPosition, SubMax, SubMin);
    NewWidth := ProgressWidth(aVal, SubMax, SubMin);
    FSubPosition := aVal;

    if OldWidth<>NewWidth then
    begin
      NeedInvalidate :=(aVal=SubMin)or(aVal=SubMax);

      if NeedInvalidate then
      begin
        include(FPaintStatus, psAll);
        Invalidate;
      end
      else
      begin
        include(FPaintStatus, psSub);
        R := SubRect;
        InvalidateRect(Handle, @R, False);
      end;
      UpdateWindow(Handle);
    end;
  end;
 end;

procedure TntvGauge.SetSubProgressColor(const Value: TColor);
begin
  if FSubProgressColor<>Value then
  begin
    FSubProgressColor := Value;
    Invalidate;
  end;
end;

procedure TntvGauge.SetText(const S: string);
begin
  Exception.Create('not implement yet');
end;

procedure TntvGauge.StepBy(vStep: integer);
begin
  Position := Position + vStep;
end;

procedure TntvGauge.StepIt;
begin
  Position := Position + FStep;
end;

function TntvGauge.SubRect: TRect;
begin
  Result := ClientRect;
  Result.Right := ProgressWidth (SubPosition, SubMax, SubMin);
  Result.Top := Result.Bottom-GetSubHeight;
end;

{procedure TntvGauge.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  //if BorderStyle<>ibsNone then
    //InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;}

{procedure TntvGauge.WMNCPaint(var Message: TMessage);
begin
  //if BorderStyle<>ibsNone then
    //RedrawBorder(HRGN(Message.WParam));
end;}

type

  TPageWrapperItem = class(TCollectionItem)
  private
    FPage: TWinControl;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
  published
    property Page: TWinControl read FPage write FPage;
  end;

  TPagesWrapper = class(TCollection)
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
  TabStop := False;
  Visible := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csReflector];
  Align := alClient;
end;

destructor TntvPage.Destroy;
begin
  inherited;
end;

{ TntvPageControl }

constructor TntvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageList := TPagesList.Create;
  FPageList.OwnsObjects := False;
  FPageBorder := 3;
  FWrapper := TPagesWrapper.Create(Self);
  FItems := TntvPages.Create(Self);
  Width := 250;
  Height := 150;
  FPageIndex := -1;
  FFirstPage := 0;
  ControlStyle := [csCaptureMouse, csClickEvents, csAcceptsControls, csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  CalcHeaderRect;
  FShowButtons := True;
  FShowTabs := True;

  //<Ayman>
  FUnderMouseIndex := -1;
  OldUnderMouseIndex := -1;
  FTabStyle := tbGradientTabs;
  //</Ayman>

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
      if FPageList[I].Page = AControl then
      begin
        SelectPage(I);
        Exit;
      end;
  end;
  inherited ShowControl(AControl);
end;

procedure TntvPageControl.SetPageIndex(Value: Integer);
begin
  if FPageIndex <> Value then
  begin
    if csLoading in ComponentState then
      FPageIndex := Value
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
      Exit; //Note: -------------> Exit procedure
    end
    else
    begin
      Rect := GetPageRect;
      ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      Rect := ClientRect;
      Brush.Color := Color;
      Brush.Style := bsSolid;
      Inc(Rect.Top, HeaderHeight - 1);
      inflateRect(Rect, -1, -1);
      FillRect(Rect);
    end;

    if ShowTabs then
    begin
      NavRect := GetControlTabRect;
      if ShowButtons then
      begin
        DrawHeader;
        ExcludeClipRect(Handle, NavRect.Left, NavRect.Top, NavRect.Right, NavRect.Bottom);
      end;
      Rect := GetTabRect(PageIndex);
      DrawTab(PageIndex, Rect);
      ExcludeClipRect(Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      for i := FirstPage to FPageList.Count - 1 do
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

procedure TntvPageControl.DrawTab(Index: Integer; var Rect: TRect);
var
  R, aTextRect: TRect;
begin
  with Canvas do
  begin
    if (Index < 0) or (Index >= FPageList.Count) then
      Exit; //belal
    if PageIndex <> Index then
    begin
      Dec(Rect.Bottom);
    end
    else if Index > FirstPage then
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
    if Index <> FPageIndex then
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
    if Index = FPageIndex then
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

    TextStyle.Layout := tlCenter;
    TextStyle.Alignment := taCenter;
    if UseRightToLeftAlignment then
       TextStyle.RightToLeft := True;
    TextRect(aTextRect, 0, 0, FPageList[Index].Caption);
  end;
end;

function TntvPageControl.GetTabRect(FirstPage, Index: Integer): TRect;
var
  R: Trect;
  i, x: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < FPageList.Count) and (Index > -1) then
  begin
    x := 0;
    for i := FirstPage to Index do
      x := x + FPageList[i].PageWidth;

    R := Bounds(0, 0, 0, HeaderHeight);

    if UseRightToLeftAlignment then
    begin
      x := ClientWidth - x;
      r.Left := x;
      r.Right := x + FPageList[Index].PageWidth
    end
    else
    begin
      r.Right := x;
      r.Left := x - FPageList[Index].PageWidth;
    end;
    Result := R;
  end;
end;

function TntvPageControl.GetTabOffset(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := index - 1 downto FirstPage do
    Result := Result + FPageList[index].PageWidth;
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
  for i := 0 to TPagesWrapper(FWrapper).Count - 1 do
    if i < Items.Count then
      Items[i].Page := TPagesWrapper(FWrapper)[i].Page
    else
      Break;
  TPagesWrapper(FWrapper).Clear;
  Items.CalcTabWidth;
  UpdatePagesList;
  if StoreIndex then
    ShowPage(FPageIndex, True, False)
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
    FHeaderHeight := TmpCanvas.TextHeight('A') + HeighAdd;
  finally
    ReleaseDC(0, TmpCanvas.Handle);
    TmpCanvas.Free;
  end;
end;

procedure TntvPageControl.DrawHeader;
begin
  DrawNormalHeader;
  {case FTabStyle of
    tbTabs: DrawNormalHeader;
    tbGradientTabs: DrawGradientHeader;
    tbFlatButtons: DrawFlatButtonsHeader;
    tbOfficeXPButtons: DrawOfficeXPButtonsHeader;
  end;}
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
      if FTabStyle = tbFlatButtons then
        vRect.Left := vRect.Left + 7
      else
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

procedure TntvPageControl.SetFirstPage(const Value: Integer);
begin
  if (Value >= 0) and (Value < FPageList.Count) then
  begin
    FFirstPage := Value;
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

function TntvPageControl.GetControlTabRect: TRect;
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

{procedure TntvPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  pt: TPoint;
  i: Integer;
begin
  inherited;
  pt := SmallPointToPoint(Message.Pos);
  if FPageList.Count = 0 then
    Exit;
  if PtInRect(GetControlTabRect, pt) then
    Message.Result := 1
  else
  begin
    for i := FirstPage to FPageList.Count - 1 do
      if PtInRect(GetTabRect(i), pt) then
      begin
        if ActivePage <> FPageList[i].Page then
          Message.Result := 1;
        Break;
      end;
  end;
end;}

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
  if PtInRect(GetControlTabRect, Point) then
  begin
    Old := FirstPage;
    if PtInRect(NextPageBtnRect, Point) then
      NextPageBtnClick
    else if PtInRect(PriorPageBtnRect, Point) then
      PriorPageBtnClick;
    Result := Old <> FirstPage;
  end
  else
  begin
    Result := False;
    Old := FPageIndex;
    for i := FirstPage to FPageList.Count - 1 do
      if PtInRect(GetTabRect(i), Point) then
      begin
        SelectPage(i);
        Result := True;
        if Old = FPageIndex then
          SetFocus;
        Break;
      end;
  end;
end;

procedure TntvPageControl.ShowPage(Index: Integer; Force: Boolean; vSetfocus: Boolean);
var
  R: TRect;
  aFirstPage: Integer;
  ParentForm: TCustomForm;
  i: Integer;
  OldIndex: Integer;
  aList: TFPList;
  w: Integer;
begin
//  if HandleAllocated then
  begin
    if ((Index <> FPageIndex) or Force) and (Index < FPageList.Count) then
    begin
      OldIndex := FPageIndex;
      for i := 0 to FPageList.Count - 1 do
        if FPageList[i].Page <> nil then
          FPageList[i].Page.Visible := False;

      //and (Items[Index].Page.Enabled)

      FPageIndex := Index;
      if (FPageIndex < 0) and (FPageList.Count > 0) then
        FPageIndex := 0
      else if (FPageIndex > FPageList.Count - 1) then
        FPageIndex := FPageList.Count - 1;
      if FPageIndex < FirstPage then
        FirstPage := FPageIndex;
      if FPageIndex >= 0 then
      begin
        R := GetTabRect(FPageIndex);
        aFirstPage := FirstPage;
        if UseRightToLeftAlignment then
        begin
          if ShowButtons then
            w := ControlTabWidth
          else
            w := 0;
          if R.Left < w then
          begin
            while (R.Left < w) and (aFirstPage < FPageIndex) do
            begin
              aFirstPage := aFirstPage + 1;
              R := GetTabRect(aFirstPage, FPageIndex);
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
            while (R.Right > w) and (aFirstPage < FPageIndex) do
            begin
              aFirstPage := aFirstPage + 1;
              R := GetTabRect(aFirstPage, FPageIndex);
            end;
          end;
        end;

        FFirstPage := aFirstPage;
        Invalidate;
        if FPageList[FPageIndex].Page <> nil then
        begin
          with FPageList[FPageIndex].Page do
          begin
            BringToFront;
            Visible := True;
            Align := alClient;
            if (vSetFocus) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and (not Self.Focused) then
            begin
              ParentForm := GetParentForm(Self);
              if ParentForm <> nil then
              begin
                if TabStop and (FPageList[FPageIndex].Page.CanFocus) then
                  ParentForm.ActiveControl := FPageList[FPageIndex].Page
                else
                begin
                  aList := TFPList.Create;
                  try
                    FPageList[FPageIndex].Page.GetTabOrderList(aList);
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
      else if FPageIndex < 0 then
      begin
        FPageIndex := Index;
        Invalidate;
      end;

      if ((OldIndex <> -1) and (OldIndex < FPageList.Count)) then
      begin
        if ((Index <> -1) and (Index < FPageList.Count)) then
          DoPageChanged(FPageList[OldIndex].Page, FPageList[Index].Page)
        else
          DoPageChanged(FPageList[OldIndex].Page, nil)
      end
      else
      begin
        if ((Index <> -1) and (Index < FPageList.Count)) then
          DoPageChanged(nil, FPageList[Index].Page)
        else
          DoPageChanged(nil, nil)
      end;


      {if ((OldIndex <> -1) and (OldIndex < FPageList.Count)) and ((Index <> -1) and (Index < FPageList.Count)) then
        DoPageChanged(FPageList[OldIndex].Page, FPageList[Index].Page)
      else if (OldIndex <> -1) and (OldIndex < FPageList.Count) then
        DoPageChanged(FPageList[OldIndex].Page, nil)
      else
        DoPageChanged(nil, nil);}
    end
  end;
end;

function TntvPageControl.GetTabRect(Index: Integer): TRect;
begin
  Result := GetTabRect(FirstPage, Index);
end;

procedure TntvPageControl.NextPageBtnClick;
var
  R: TRect;
begin
  R := GetTabRect(FPageList.Count - 1);
  if UseRightToLeftAlignment then
  begin
    if R.Left < ControlTabWidth then
      FirstPage := FirstPage + 1;
  end
  else
  begin
    if R.Right > (ClientWidth - ControlTabWidth) then
      FirstPage := FirstPage + 1;
  end;
end;

procedure TntvPageControl.PriorPageBtnClick;
begin
  if FirstPage > 0 then
    FirstPage := FirstPage - 1;
end;

function TntvPageControl.GetPageTabRect: TRect;
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
  Result := FPageIndex;
end;

procedure TntvPageControl.CNNotify(var Message: TLMNotify);
begin
  inherited;
end;

procedure TntvPageControl.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
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
    ShiftState :=  KeyDataToShiftState(Message.KeyData);// GetShiftState;
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
  TPagesWrapper(FWrapper).Clear;
  Reader.ReadValue;
  Reader.ReadCollection(TPagesWrapper(FWrapper));
end;

procedure TntvPageControl.WriteWrapper(Writer: TWriter);
var
  i: Integer;
begin
  (FWrapper as TPagesWrapper).Clear;
  for i := 0 to Items.Count - 1 do
    with (FWrapper as TPagesWrapper).Add do
      Page := Items[i].Page;
  Writer.WriteCollection((FWrapper as TPagesWrapper));
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

procedure TntvPageControl.SetPageBorder(const Value: TPageBorder);
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
  inflateRect(Result, -FPageBorder, -FPageBorder);
end;

procedure TntvPageControl.SetActivePage(const Value: TWinControl);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
  begin
    if FPageList[i].Page = Value then
    begin
      SelectPage(i, True);
      Break;
    end;
  end;
end;

function TntvPageControl.GetActivePage: TWinControl;
begin
  if (PageIndex >= 0) and (PageIndex < FPageList.Count) then
    Result := FPageList[PageIndex].Page
  else
    Result := nil;
end;

procedure TntvPageItem.Assign(Source: TPersistent);
begin
  if Source is TntvPageItem then
  begin
    Caption := TntvPageItem(Source).Caption;
    Name := TntvPageItem(Source).Name;
    ImageIndex := TntvPageItem(Source).ImageIndex;
  end
  else
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
      Caption := Format('Page [%d]', [Index]);
      Name := Format(PageControl.Name + 'Page%d', [Index]);
      FPage := TntvPage.Create(PageControl.Owner);
      (FPage as TntvPage).Item := Self;
      FPage.Name := Name;
      FPage.Parent := PageControl;
      FPage.Show;
    end;
    PageControl.UpdatePagesList;
  end;
  FVisible := True;
  FImageIndex := -1;
end;

destructor TntvPageItem.Destroy;
begin
  //FreeAndNil(FPage); belal need review very important in lazarus
  inherited;
end;

function TntvPageItem.GetPageControl: TntvPageControl;
begin
  if Collection <> nil then
    Result := (Collection as TntvPages).FPageControl
  else
    Result := nil;
end;

function TntvPageItem.GetName: string;
begin
  Result := FName;
end;

procedure TntvPageItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if PageControl <> nil then
      PageControl.Items.CalcTabWidth;
  end;
end;

procedure TntvPageItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TntvPageItem.SetName(const Value: string);
begin
  FName := Value;
  DisplayName:=Value;
end;

procedure TntvPageItem.SetPage(const Value: TWinControl);
begin
  if (FPage <> Value) then
  begin
    FPage := Value;
    if Value <> nil then
      FPage.Align := alClient;

  end;
end;

procedure TntvPageItem.SetPageWidth(const Value: Integer);
begin
  FPageWidth := Value;
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

//zaher begin

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
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

function TntvPageControl.GetPageItem(Page: TWinControl): TntvPageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].Page = Page then
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
    Result := FPageList[Index].Page
  else
    Result := nil;
end;

procedure TntvPageControl.RedrawBorder(const Clip: HRGN);
var
  DC: HDC;
  RC, RW: TRect;
  aCanvas: TCanvas;
begin
  aCanvas := TCanvas.Create;
  //DC := GetWindowDC(Handle); belal
  DC := GetDC(Handle);
  try
    aCanvas.Handle := DC;

    GetWindowRect(Handle, RW);
    LCLIntf.GetClientRect(Handle, RC);
    //MapWindowPoints(0, Handle, RW, 2); belal xxxx
    ntvMapWindowRect(0, Handle, RW);

    OffsetRect(RC, -RW.Left, -RW.Top);
    OffsetRect(RW, -RW.Left, -RW.Top);

    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    aCanvas.Brush.Color := Color;
    FillRect(DC, RW, aCanvas.Brush.Handle);
    //DrawFlatEdge(aCanvas,RW,Color,False);

  finally
    aCanvas.Handle := 0;
    ReleaseDC(Handle, DC);
    aCanvas.Free;
  end;
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

procedure TntvPageControl.SetUnderMouseIndex(const Value: Integer);
var
  R: TRect;
begin
  if FUnderMouseIndex <> Value then
  begin
    FUnderMouseIndex := Value;
    if (FTabStyle in [tbGradientTabs, tbFlatButtons, tbOfficeXPButtons]) then
    begin
      R := GetTabRect(Value);
      DrawTab(Value, R);
      R := GetTabRect(OldUnderMouseIndex);
      DrawTab(OldUnderMouseIndex, R);
      R := GetTabRect(PageIndex);
      DrawTab(PageIndex, R);
    end;
    OldUnderMouseIndex := Value;
  end;
end;

procedure TntvPageControl.SetImageList(const Value: TImageList);
begin
  if (FImageList <> Value) then
  begin
    FImageList := Value;
    Items.CalcTabWidth;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TntvPageControl.DrawButton(vRect: TRect; const IsDown: Boolean);
  procedure DrawRizedEdge(vRect: TRect);
  begin
    with Canvas do
    begin
      MoveTo(vRect.Right, vRect.Top);
      LineTo(vRect.Left, vRect.Top);
      LineTo(vRect.Left, vRect.Bottom);
    end;
  end;
  procedure DrawDownEdge(vRect: TRect);
  begin
    with Canvas do
    begin
      MoveTo(vRect.Left, vRect.Bottom); //left Bottom corner
      LineTo(vRect.Right, vRect.Bottom);
      LineTo(vRect.Right, vRect.Top);
    end;
  end;
begin

  InflateRect(vRect, -4, 0);
  Dec(vRect.Bottom, 3);

  with Canvas do
  begin
    Pen.Color := clBtnShadow;
    MoveTo(vRect.Right, vRect.Bottom); //Right bottom corner
    LineTo(vRect.Right, vRect.Top);
    LineTo(vRect.Left, vRect.Top);
    LineTo(vRect.Left, vRect.Bottom);
    LineTo(vRect.Right, vRect.Bottom);

    InflateRect(vRect, -1, -1);
    Pen.Color := clBtnHighlight;
    if IsDown then
    begin
      DrawDownEdge(vRect);
      InflateRect(vRect, -1, -1);
      //Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight); belal
      FillRect(vRect);
    end
    else
    begin
      DrawRizedEdge(vRect);
      InflateRect(vRect, -1, -1);
      Brush.Color := Color;
      FillRect(vRect);
    end;
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

procedure TntvPageControl.DrawBottomLine(vRect: TRect);
begin
  if (PageIndex = 0) then
  begin
    if UseRightToLeftAlignment then
    begin
      Canvas.MoveTo(vRect.Right - 1, vRect.Bottom - 1);
      Canvas.LineTo(vRect.Left - 2, vRect.Bottom - 1);
    end
    else

    begin
      Canvas.MoveTo(vRect.Right, vRect.Bottom - 1);
      Canvas.LineTo(vRect.Left - 1, vRect.Bottom - 1);
    end
  end
  else
  begin
    Canvas.MoveTo(vRect.Right - 1, vRect.Bottom - 1);
    Canvas.LineTo(vRect.Left - 1, vRect.Bottom - 1);
  end;

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

procedure TntvPageControl.InternalDrawShadowedBorder;
var
  BackGroundColor: TColor;
begin
  if Parent <> nil then
    BackGroundColor := Parent.Brush.Color
  else
    BackGroundColor := Color;

  with Canvas do
  begin
    Brush.Style := bsSolid;
    if FTabStyle in [tbFlatButtons, tbOfficeXPButtons] then
      Brush.Color := Color
    else
      Brush.Color := BackGroundColor;

    Pen.Color := clGray;
    MoveTo(0, ClientHeight - 3);
    LineTo(0, HeaderHeight - 1);
    LineTo(ClientWidth - 2, HeaderHeight - 1);

    Pen.Color := clBtnHighlight;
    MoveTo(1, ClientHeight - 3);
    LineTo(1, HeaderHeight);
    LineTo(ClientWidth - 2, HeaderHeight);

    Pen.Color := MixColors(clBtnHighlight, clGray, 150);
    MoveTo(ClientWidth - 2, HeaderHeight);
    LineTo(ClientWidth - 2, ClientHeight - 2);
    LineTo(0, ClientHeight - 2);

    Pen.Color := MixColors(clBtnHighlight, clGray, 200);
    MoveTo(ClientWidth - 2, HeaderHeight - 1);
    LineTo(ClientWidth - 1, HeaderHeight - 1);
    LineTo(ClientWidth - 1, HeaderHeight - 1);
    LineTo(ClientWidth - 1, ClientHeight - 1);
    LineTo(-1, ClientHeight - 1);

    Pen.Color := clGray;
    MoveTo(ClientWidth - 3, HeaderHeight);
    LineTo(ClientWidth - 3, ClientHeight - 3);
    LineTo(0, ClientHeight - 3);

    if FTabStyle = tbGradientTabs then
    begin
      Pen.Color := Color;
      MoveTo(TabRect.Left + 2, TabRect.Bottom);
      LineTo(TabRect.Right - 2, TabRect.Bottom);
    end;

    FillRect(Rect(0, 0, ClientWidth, HeaderHeight - 1));

  end;
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
    Rect := GetControlTabRect;
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
      b := FirstPage > 0;
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
      b := FirstPage > 0;
      Brush.Color := FillColor[b];
      Polygon([Point(ClientWidth - 30, 10), Point(ClientWidth - 20, 15), Point(ClientWidth - 20, 5)]);
    end;
  end;
end;

procedure TntvPageControl.DrawNormalTab(Index: Integer; var Rect: TRect);
var
  R, aTextRect: TRect;
begin
  with Canvas do
  begin
    if (Index < 0) or (Index >= FPageList.Count) then
      Exit; //belal
    if PageIndex <> Index then
    begin
      Dec(Rect.Bottom);
    end
    else if Index > FirstPage then
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
    if Index <> FPageIndex then
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
    if Index = FPageIndex then
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

    TextStyle.Layout := tlCenter;
    TextStyle.Alignment := taCenter;
    if UseRightToLeftAlignment then
       TextStyle.RightToLeft := True;
    TextRect(aTextRect, 0, 0, FPageList[Index].Caption);

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

function TntvPages.AddControl(Control: TWinControl): TntvPageItem;
var
  Buffer: PChar;
  Size: Byte;
begin
  if Control <> nil then
  begin
    Result := TntvPageItem.Create(nil);
    with Result do
    begin
      Page := Control;
      Page.Name := Control.Name;
      Size := Page.GetTextLen; {Get length of string in Edit1}
      Inc(Size); {Add room for null character}
      GetMem(Buffer, Size); {Creates Buffer dynamic variable}
      try
        Page.GetTextBuf(Buffer, Size); {Puts Edit1.Text into Buffer}
        FCaption := StrPas(Buffer); {Converts Buffer to a Pascal-style string}
      finally
        FreeMem(Buffer, Size); {Frees memory allocated to Buffer}
      end;
      Page.Parent := FPageControl;
      //LclType.SetParent(Control.Handle, FPageControl.Handle);
      Page.FreeNotification(FPageControl);
      Collection := Self;
      Control.Align := alClient;
      FPageControl.UpdatePagesList;
      Page.Show; //zaher
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
        Items[i].PageWidth := TmpCanvas.TextWidth(Items[i].Caption) + 20;
        if (FPageControl.ImageList <> nil) and (Items[I].ImageIndex > -1) then
          Items[i].PageWidth := Items[i].PageWidth + FPageControl.ImageList.Width - 8;
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
  inherited Destroy;
end;

function TntvPages.ExtractControl(Control: TWinControl): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Page = Control then
    begin
      Result := Items[i].Page;
      Items[i].Page := nil;
      Delete(i);
      if not (csDestroying in FPageControl.ComponentState) then
      begin
        FPageControl.UpdatePagesList;
        FPageControl.ShowPage(i - 1)
      end;
      Break;
    end;
end;

function TntvPages.FindControl(Control: TWinControl): TntvPageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Page = Control then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TntvPages.GetItem(Index: Integer): TntvPageItem;
begin
  Result := TntvPageItem(inherited GetItem(Index));
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

{ TPagesWrapper }

function TPagesWrapper.Add: TPageWrapperItem;
begin
  Result := TPageWrapperItem(inherited Add);
end;

constructor TPagesWrapper.Create(APageControl: TntvPageControl);
begin
  inherited Create(TPageWrapperItem);
  FPageControl := APageControl;
end;

function TPagesWrapper.GetItem(Index: Integer): TPageWrapperItem;
begin
  Result := TPageWrapperItem(inherited GetItem(Index));
end;

procedure TPagesWrapper.SetItem(Index: Integer; Value: TPageWrapperItem);
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
  FPage := nil;
  inherited;
end;

procedure TntvPageItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    PageControl.UpdatePagesList;
    PageControl.ShowPage(PageControl.PageIndex, True, csFocusing in PageControl.ControlState);
    PageControl.Invalidate;
  end;
end;

function TntvPageItem.GetDisplayName: string;
begin
  Result:=Name;
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

initialization
  RegisterClasses([TntvPage]);


end.

