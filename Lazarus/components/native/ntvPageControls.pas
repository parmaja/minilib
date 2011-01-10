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
  Classes, Messages, Controls, SysUtils, Contnrs, Graphics, Forms, Types,
  LMessages, LCLType, LCLIntf, LCLProc,
  ntvTabs, ntvTabSets, ntvUtils;

type
  TntvPageControl = class;
  TntvPageItem = class;

  { TntvPageItem }

  TntvPageItem = class(TntvTabItem)
  private
    FControl: TWinControl;
    procedure SetControl(const Value: TWinControl);
    function GetPageControl: TntvPageControl;
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); override;
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
    function GetItem(Index: Integer): TntvPageItem;
    procedure SetItem(Index: Integer; Value: TntvPageItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Invalidate; override;
    procedure UpdateCanvas(vCanvas: TCanvas); override;
  public
    constructor Create(APageControl: TntvPageControl);
    destructor Destroy; override;
    function GetOwner: TPersistent; override;
    function Add: TntvPageItem;
    function FindControl(vControl: TWinControl): TntvPageItem;
    function IndexOf(vControl: TWinControl): Integer;
    function AddControl(vControl: TWinControl): TntvPageItem;
    function ExtractControl(vControl: TWinControl): TWinControl;
    property Items[Index: Integer]: TntvPageItem read GetItem write SetItem stored False; default;
  published
  end;

  { TntvPageControl }

  TntvPageControl = class(TntvCustomTabSet)
  private
    FWrapper: TObject;
    FMargin: Integer;
    procedure ReadWrapper(Reader: TReader);
    procedure WriteWrapper(Writer: TWriter);
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;

    procedure SetMargin(const Value: Integer);
    procedure SetActiveControl(const Value: TWinControl);
    function GetActiveControl: TWinControl;
    function GetItems: TntvPages;
    procedure SetItems(Value: TntvPages);
    function GetPageItem(vControl: TWinControl): TntvPageItem;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateTabs: TntvTabs; override;
    procedure ShowControl(AControl: TControl); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

    function GetInnerRect: TRect; virtual;
    function GetPageRect: TRect; virtual;

    procedure BringControl(vControl: TWinControl; vSetFocus: Boolean);

    procedure Loaded; override;
    procedure DoTabShow(Index: Integer; vSetfocus: Boolean); override;
    procedure DoTabShowed(Index: Integer; vSetfocus: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActiveControl: TWinControl read GetActiveControl write SetActiveControl;
    property PageItem[Control: TWinControl]: TntvPageItem read GetPageItem;
  published
    property Margin: Integer read FMargin write SetMargin default 3;
    property StoreIndex;
    property ShowButtons;
    property ShowTabs;
    property ItemIndex;
    property ImageList;

    property Items: TntvPages read GetItems write SetItems;
    property OnTabChanged;
    property OnSelectTab;

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

{ TntvPageControl }

constructor TntvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDesignInteractive, csCaptureMouse, csClickEvents, csAcceptsControls, csSetCaption, csOpaque, csDoubleClicks];
  FMargin := 3;
  FWrapper := TPageWrappers.Create(Self);
  //FItems := TntvPages.Create(Self);
  SetInitialBounds(0, 0, GetControlClassDefaultSize.cx, GetControlClassDefaultSize.cy);
end;

destructor TntvPageControl.Destroy;
begin
  FreeAndNil(FWrapper);
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

function TntvPageControl.CreateTabs: TntvTabs;
begin
  Result := TntvPages.Create(Self);
end;

procedure TntvPageControl.ShowControl(AControl: TControl);
var
  i: Integer;
begin
  if (AControl <> nil) and (AControl is TWinControl) then
  begin
    i := Items.IndexOf(AControl as TWinControl);
    if i >=0 then
    begin
      SelectTab(I);
      Exit;
    end;
  end;
  inherited;
end;

procedure TntvPageControl.AlignControls(AControl: TControl; var Rect: TRect);
begin
  Rect := GetPageRect;
  inherited;
end;

procedure TntvPageControl.BringControl(vControl: TWinControl; vSetFocus: Boolean);
var
  ParentForm: TCustomForm;
  aList: TFPList;
  i: Integer;
begin
  if vControl <> nil then
    with vControl do
    begin
      BringToFront;
      Visible := True;
      Align := alClient;
      if not (csLoading in ComponentState) and (vSetFocus) and (not Self.Focused) then
      begin
        ParentForm := GetParentForm(Self);
        if ParentForm <> nil then
        begin
          if TabStop and (vControl.CanFocus) then
            ParentForm.ActiveControl := vControl
          else
          begin
            aList := TFPList.Create;
            try
              vControl.GetTabOrderList(aList);
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

procedure TntvPageControl.Loaded;
var
  i: Integer;
  aControl: TWinControl;
begin
  for i := 0 to TPageWrappers(FWrapper).Count - 1 do
    if i < Items.Count then
    begin
      aControl := TPageWrappers(FWrapper)[i].Control;
      Items[i].Control := aControl;
    end
    else
      Break;
  TPageWrappers(FWrapper).Clear;
  inherited;//zaher look
end;

procedure TntvPageControl.DoTabShow(Index: Integer; vSetfocus: Boolean);
var
  i: Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    for i := 0 to Items.Visibles.Count - 1 do
      //To sure all tabs controls is hidden
      if (Items.Visibles[i] as TntvPageItem).Control <> nil then
        (Items.Visibles[i] as TntvPageItem).Control.Visible := False;
end;

procedure TntvPageControl.DoTabShowed(Index: Integer; vSetfocus: Boolean);
begin
  inherited;
  BringControl((Items.Visibles[Index] as TntvPageItem).Control, vSetfocus);
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

procedure TntvPageControl.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
  begin
    if not (csLoading in ComponentState) then
      if Inserting and (Control is TWinControl) then
      begin
        Items.AddControl(Control as TWinControl);
        ShowControl(Control as TWinControl);
        Result := 1;
      end
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

procedure TntvPageControl.SetMargin(const Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Realign;
  end;
end;

function TntvPageControl.GetPageRect: TRect;
begin
  Result := ClientRect;
  if ShowTabs then
    Inc(Result.Top, HeaderHeight);
  InflateRect(Result, -FMargin, -FMargin);
end;

procedure TntvPageControl.SetActiveControl(const Value: TWinControl);
var
  i: Integer;
begin
  for i := 0 to Items.Visibles.Count - 1 do
  begin
    if (Items.Visibles[i] as TntvPageItem).Control = Value then
    begin
      SelectTab(i, True);
      Break;
    end;
  end;
end;

function TntvPageControl.GetActiveControl: TWinControl;
begin
  if (ItemIndex >= 0) and (ItemIndex < Items.Visibles.Count) then
    Result := (Items.Visibles[ItemIndex] as TntvPageItem).Control
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
    if not (csLoading in PageControl.ComponentState) then
    begin
      Caption := Format('Control [%d]', [Index]);
      Name := Format(PageControl.Name + 'Control%d', [Index]);
    end;
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

function TntvPageControl.GetItems: TntvPages;
begin
  Result := (inherited Items) as TntvPages;
end;

procedure TntvPageControl.SetItems(Value: TntvPages);
begin
   inherited Items := Value;
end;


class function TntvPageControl.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 200;
  Result.cy := 240;
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
        Name := Control.Name;
        Size := Control.GetTextLen;
        Inc(Size);
        GetMem(Buffer, Size);
        try
          Control.GetTextBuf(Buffer, Size);
          Caption := StrPas(Buffer);
        finally
          FreeMem(Buffer, Size);
        end;
        Control.Parent := FPageControl;
        //LclType.SetParent(Control.Handle, FPageControl.Handle);
        Control.FreeNotification(FPageControl);
        Collection := Self; //Add to Pages list
        Control.Align := alClient;
        if not (csDesigning in Control.ComponentState) then
          Control.Show
      end;
    finally
      EndUpdate;
    end;
  end
  else
    Result := nil;
end;

procedure TntvPages.Invalidate;
begin
  if (FPageControl <> nil) and not (csLoading in FPageControl.ComponentState) and FPageControl.HandleAllocated then
  begin
    FPageControl.Invalidate;
  end;
end;

procedure TntvPages.UpdateCanvas(vCanvas: TCanvas);
begin
  inherited;
  vCanvas.Font.Assign(FPageControl.Font);
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
        FPageControl.ShowTab(i - 1)
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
  Result := (inherited Items[Index] as TntvPageItem);
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

function TntvPages.IndexOf(vControl: TWinControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count -1 do
  begin
    if Items[i].Control = vControl then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TntvPageItem.SetIndex(Value: Integer);
var
  ShowSelf: Boolean;
begin
  if PageControl <> nil then
    ShowSelf := PageControl.ItemIndex = Index
  else
    ShowSelf := False;

  inherited;

  if PageControl <> nil then
  begin
    if ShowSelf then
    begin
      PageControl.ShowTab(Index, True);
    end
    else
      PageControl.ShowTab(PageControl.ItemIndex, True);
  end;
end;

end.

