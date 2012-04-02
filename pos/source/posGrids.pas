unit posGrids;
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
  SysUtils, Classes, Graphics, Controls, Forms, Types, Contnrs, StdCtrls,
  posTypes, posDraws, posLists, posControls;

type
  TposCustomColumn = class;
  TposCustomColumns = class;
  TposCustomGrid = class;
  TposGridRow = class;
  TposGridRows = class;

  TposCellInfo = record
    Value: Variant;
    Color: TColor;
    StrikeThrough: Boolean;
    Data: Integer;
    AnObject: TObject;
  end;

  TposOnGetCell = procedure(Sender: TObject; ID, Row, Column: Integer; var Info: TposCellInfo) of object;
  TposOnGetCount = procedure(Sender: TObject; var Count: Integer) of object;
  TposOnGetItemIndex = procedure(Sender: TObject; var vItemIndex: Integer) of object;
  TposOnSetItemIndex = procedure(Sender: TObject; vItemIndex: Integer) of object;

  TPosDrawColumns = class(TposCustomListItems)
  public
  end;

  TposVisibleColumn = class(TObject)
  private
    Index: Integer;
    FWidth: Double;
    FColumn: TposCustomColumn;
  published
  public
    property Width: Double read FWidth write FWidth;
    property Column: TposCustomColumn read FColumn write FColumn;
  end;

  TposVisibleList = class(TObjectList)
  private
    function GetItem(Index: Integer): TposVisibleColumn;
    procedure SetItem(Index: Integer; const Value: TposVisibleColumn);
  published
  public
    constructor Create;
    function SumWidth: Double;
    property Items[Index: Integer]: TposVisibleColumn read GetItem write SetItem; default;
  end;

  { TposCustomColumn }

  TposCustomColumn = class(TObject)
  private
    FColumns: TposCustomColumns;
    FTitle: string;
    FWidth: Double;
    FID: Integer;
    FRightToLeftMode: TRightToLeftMode;
    FVisible: Boolean;
    FName: string;
    FAlignment: TAlignment;
    FIndex: Integer;
    procedure SetVisible(const Value: Boolean);
    procedure SetTitle(const Value: string);
  protected
    procedure PaintHeader(Canvas: TCanvas; Rect: TRect; Color: TColor; LastCell: Boolean); virtual;
    procedure PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean); virtual;
    function UseRightToLeft: Boolean; virtual;
  public
    constructor Create(AColumns: TposCustomColumns);
    property Columns: TposCustomColumns read FColumns;
    property Index: Integer read FIndex; //read only used for access data
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Title: string read FTitle write SetTitle;
    property Width: Double read FWidth write FWidth;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property RightToLeftMode: TRightToLeftMode read FRightToLeftMode write FRightToLeftMode;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TposCustomColumns = class(TObjectList)
  private
    FGrid: TposCustomGrid;
    function GetItem(Index: Integer): TposCustomColumn;
    procedure SetItem(Index: Integer; const Value: TposCustomColumn);
  protected
  public
    function ByName(vName: string; vSafe: Boolean = False): TposCustomColumn;
    function ByID(vID: Integer; vSafe: Boolean = False): TposCustomColumn;
    procedure Add(vID: Integer; vName: string; vTitle: string; vAlignment: TAlignment; vRightToLeftMode: TRightToLeftMode; vWidth: Double; vVisible: Boolean = True); overload;
    property Items[Index: Integer]: TposCustomColumn read GetItem write SetItem; default;
    property Grid: TposCustomGrid read FGrid;
  end;

//---------------------

  TposTextColumn = class(TposCustomColumn)
  public
    procedure PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean); override;
  end;

  TposCheckedColumn = class(TposCustomColumn)
  public
    procedure PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean); override;
  end;

  TposColumns = class(TposCustomColumns)
  public
  end;

//-------------------

  TposCustomGrid = class(TposAbstractList)
  private
    FColumns: TposCustomColumns;
    FCurrentColumn: Integer;
    FStartColumn: Integer;
    FCellMargin: Integer;
    FDualColor: Boolean;
    function GetItems: TposItems;
    procedure SetCurrentColumn(const Value: Integer);
    procedure SetStartColumn(const Value: Integer);
    procedure SetCellMargin(const Value: Integer);
  protected
    procedure PaintHeader(Canvas: TCanvas; Rect: TRect; Color: TColor);
    procedure PaintRow(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor);
    procedure PaintOuter(Canvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo); virtual;
    function CreateColumns: TposCustomColumns; virtual;
    function CreateVisibleColumns: TposVisibleList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Columns: TposCustomColumns read FColumns;
    property Items: TposItems read GetItems;
    property StartColumn: Integer read FStartColumn write SetStartColumn;
    property CurrentColumn: Integer read FCurrentColumn write SetCurrentColumn;
    function CurrentData: Integer; deprecated;
  published
    property ShowHeader default True;
    property DualColor: Boolean read FDualColor write FDualColor default True;
    property CellMargin: Integer read FCellMargin write SetCellMargin default 1;
  end;

{ Virtual Grid}

  TposVirtualGrid = class;

  TposVirtualRows = class(TposItems)
  private
  protected
    FGrid: TposVirtualGrid;
    function GetCount: Integer; override;
    procedure SetCount(const Value: Integer); override;
    function GetItemIndex: Integer; override;
    procedure SetItemIndex(const Value: Integer); override;
  public
    procedure Clear; override;
    procedure PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); override;
    property Grid: TposVirtualGrid read FGrid;
  end;

  TposGridHelper = class(TposHelper)
  public
    procedure CreateColumns(Sender: TposVirtualGrid); virtual; abstract;
    procedure GetCell(Sender: TposVirtualGrid; ID, Row, Column: Integer; var Info: TposCellInfo); virtual; abstract;
    procedure GetCount(Sender: TposVirtualGrid; var Count: Integer); virtual; abstract;
    procedure GetItemIndex(Sender: TposVirtualGrid; var vItemIndex: Integer); virtual; abstract;
    procedure SetItemIndex(Sender: TposVirtualGrid; vItemIndex: Integer); virtual; abstract;
  end;

  TposGridHelperClass = class of TposGridHelper;

  TposVirtualGrid = class(TposCustomGrid)
  private
    FOnGetCell: TposOnGetCell;
    FOnGetCount: TposOnGetCount;
    FOnGetItemIndex: TposOnGetItemIndex;
    FOnSetItemIndex: TposOnSetItemIndex;
    FOnItemIndexChanged: TNotifyEvent;
    function GetItems: TposVirtualRows;
    function GetHelper: TposGridHelper;
    procedure SetHelper(const Value: TposGridHelper);
  protected
    function DoCreateItems: TposItems; override;
    procedure GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo); override;
  public
    destructor Destroy; override;
    procedure HelperChanged; override;
    property Helper: TposGridHelper read GetHelper write SetHelper;
    property Items: TposVirtualRows read GetItems;
  published
    property OnGetCell: TposOnGetCell read FOnGetCell write FOnGetCell;
    property OnGetCount: TposOnGetCount read FOnGetCount write FOnGetCount;
    property OnGetItemIndex: TposOnGetItemIndex read FOnGetItemIndex write FOnGetItemIndex;
    property OnSetItemIndex: TposOnSetItemIndex read FOnSetItemIndex write FOnSetItemIndex;
    property OnItemIndexChanged: TNotifyEvent read FOnItemIndexChanged write FOnItemIndexChanged;
  end;

//------------------------

  TposGridRowCells = class;

  TposCell = class(TObject)
  private
    FParent: TposGridRowCells;
    Info: TposCellInfo;
  public
    constructor Create;
    property Value: Variant read Info.Value write Info.Value;
    property Color: TColor read Info.Color write Info.Color;
  end;

  TposGridRowCells = class(TObjectList)
  private
    function CreateItem: TposCell;
    function GetItems(Index: Integer): TposCell;
  protected
    FRow: TposGridRow;
  public
    procedure Add(ACell: TposCell); overload;
    function Add: TposCell; overload;
    function Add(AValue: Variant): TposCell; overload;
    property Row: TposGridRow read FRow;
    property Items[Index: Integer]: TposCell read GetItems; default;
  end;

//---------

  TposGridRow = class(TposCustomListItem)
  private
    FItems: TposGridRowCells;
    function GetRows: TposGridRows;
  public
    constructor Create; virtual;
    procedure Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); override;
    property Rows: TposGridRows read GetRows;
    property Items: TposGridRowCells read FItems;
  end;

  TposGridRows = class(TposCustomListItems)
  private
    FGrid: TposCustomGrid;
    function GetItem(Index: Integer): TposGridRow;
    procedure SetItem(Index: Integer; const Value: TposGridRow);
  protected
    function CreateItem: TposCustomListItem; override;
  public
    function Add: TposGridRow;
    procedure PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean); override;
    property Grid: TposCustomGrid read FGrid;
    property Items[Index: Integer]: TposGridRow read GetItem write SetItem; default;
  end;

  TposGrid = class(TposCustomGrid)
  private
    function GetItems: TposGridRows;
  protected
    procedure GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo); override;
    function DoCreateItems: TposItems; override;
  public
    property Items: TposGridRows read GetItems;
  published
  end;

implementation

uses
  mnUtils, posUtils, Variants;

{ TposGridRows }

function TposGridRowCells.Add(AValue: Variant): TposCell;
begin
  Result := Add;
  with Result do
  begin
    Info.Value := AValue;
  end;
end;

function TposGridRows.Add: TposGridRow;
begin
  Result := CreateItem as TposGridRow;
  Result.Parent := Self;
end;

function TposGridRows.CreateItem: TposCustomListItem;
begin
  Result := TposGridRow.Create;
end;

function TposGridRows.GetItem(Index: Integer): TposGridRow;
begin
  Result := inherited Items[Index] as TposGridRow;
end;

procedure TposGridRows.PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
begin
  Items[Index].Paint(Canvas, Index, Rect, Color, RightToLeft);
end;

procedure TposGridRows.SetItem(Index: Integer; const Value: TposGridRow);
begin
  inherited Items[Index] := Value;
end;

{ TposGridRow }

constructor TposGridRow.Create;
begin
  inherited Create;
  FItems := TposGridRowCells.Create;
  FItems.FRow := Self;
end;

function TposGridRow.GetRows: TposGridRows;
begin
  Result := Parent as TposGridRows;
end;

procedure TposGridRow.Paint(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
begin
  Rows.Grid.PaintRow(Canvas, Index, Rect, Color);
end;

{ TposGridList }

function TposGrid.DoCreateItems: TposItems;
begin
  Result := TposGridRows.Create;
  TposGridRows(Result).FGrid := Self;
end;

{ TPosGridList }

procedure TposGrid.GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo);
var
  aCell: TposCell;
begin
  if Row < Items.Count then
  begin
    if Column.Index < Items[Row].Items.Count then
    begin
      aCell := Items[Row].Items[Column.Index];
      if aCell <> nil then
      begin
        Info.Value := aCell.Value;
        Info.Color := aCell.Color;
      end;
    end;
  end
  else
  begin
    Info.Value := '';
    Info.Color := clWhite;
  end;
end;

function TposGrid.GetItems: TposGridRows;
begin
  Result := (inherited Items) as TposGridRows;
end;

{ TposCustomColumns }

procedure TposCustomColumns.Add(vID: Integer; vName: string; vTitle: string; vAlignment: TAlignment; vRightToLeftMode: TRightToLeftMode; vWidth: Double; vVisible: Boolean);
begin
  with TposTextColumn.Create(Self) do
  begin
    ID := vID;
    Title := vTitle;
    Name := vName;
    Width := vWidth;
    Alignment := vAlignment;
    RightToLeftMode := vRightToLeftMode;
    Visible := vVisible;
  end;
end;

function TposCustomColumns.ByID(vID: Integer; vSafe: Boolean): TposCustomColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ID = vID then
    begin
      Result := Items[i];
      break;
    end;
  end;
  if (not vSafe) and (Result = nil) then
    raise Exception.Create('Column not found: ' + IntToStr(vID));
end;

function TposCustomColumns.ByName(vName: string; vSafe: Boolean): TposCustomColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
  if (not vSafe) and (Result = nil) then
    raise Exception.Create('Column not found: ' + vName);
end;

function TposCustomColumns.GetItem(Index: Integer): TposCustomColumn;
begin
  Result := inherited Items[Index] as TposCustomColumn;
end;

procedure TposCustomColumns.SetItem(Index: Integer; const Value: TposCustomColumn);
begin
  inherited Items[Index] := Value;
end;

{ TposCustomColumn }

procedure TposTextColumn.PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean);
var
  R, TextRect: TRect;
  aStyle: TTextStyle;
begin
  inherited;
  InitMemory(aStyle, SizeOf(aStyle));
  TextRect := Rect;

  aStyle.Layout := tlCenter;
  aStyle.SingleLine := True;
  aStyle.Opaque := False;
  aStyle.RightToLeft := UseRightToLeft;
  aStyle.Alignment := Alignment;
  aStyle.Clipping := True;
  BidiAlignment(aStyle);

  R := TextRect;
  InflateRect(R, -1, 0);
  PaintText(Canvas, ACell.Value, R, aStyle);
end;

procedure TposCustomColumn.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TposCustomColumn.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Columns.FGrid <> nil then
      Columns.FGrid.Invalidate;
  end;
end;

constructor TposCustomColumn.Create(AColumns: TposCustomColumns);
begin
  inherited Create;
  FColumns := AColumns;
  FIndex := FColumns.Add(Self);
  FVisible := True;
  FWidth := 10;
end;

{ TposCustomColumn }

procedure TposCustomColumn.PaintHeader(Canvas: TCanvas; Rect: TRect; Color: TColor; LastCell: Boolean);
var
  R, TextRect: TRect;
  aStyle: TTextStyle;
  aRightToLeft: Boolean;
begin
  InitMemory(aStyle, SizeOf(aStyle));
  TextRect := Rect;
  aRightToLeft := Columns.Grid.UseRightToLeftAlignment;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.FillRect(TextRect);
  if aRightToLeft then
  begin
    if LastCell then
    begin
      Canvas.MoveTo(TextRect.Left, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Right, TextRect.Bottom - 1);
    end
    else
    begin
      Canvas.MoveTo(TextRect.Left, TextRect.Top);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Right, TextRect.Bottom - 1);
    end;
  end
  else
  begin
    if LastCell then
    begin
      Canvas.MoveTo(TextRect.Right - 1, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
    end
    else
    begin
      Canvas.MoveTo(TextRect.Right - 1, TextRect.Top);
      Canvas.LineTo(TextRect.Right - 1, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
    end;
  end;

  aStyle.RightToLeft := aRightToLeft;
  aStyle.Alignment := taCenter;
  aStyle.Layout := tlCenter;
  aStyle.SingleLine := True;
  aStyle.Opaque := False;
  aStyle.Clipping := True;
  R := TextRect;
  PaintText(Canvas, Title, R, aStyle);
end;

procedure TposCustomColumn.PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean);
var
  TextRect: TRect;
begin
  TextRect := Rect;
  if aCell.Color <> clDefault then
    Canvas.Brush.Color := aCell.Color
  else
    Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Lighten(Canvas.Brush.Color, -25);
  Canvas.FillRect(TextRect);

  if Columns.Grid.UseRightToLeftAlignment then
  begin
    if LastCell then
    begin
      Canvas.MoveTo(TextRect.Left, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Right, TextRect.Bottom - 1);
    end
    else
    begin
      Canvas.MoveTo(TextRect.Left, TextRect.Top);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Right, TextRect.Bottom - 1);
    end;
  end
  else
  begin
    if LastCell then
    begin
      Canvas.MoveTo(TextRect.Right - 1, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
    end
    else
    begin
      Canvas.MoveTo(TextRect.Right - 1, TextRect.Top);
      Canvas.LineTo(TextRect.Right - 1, TextRect.Bottom - 1);
      Canvas.LineTo(TextRect.Left, TextRect.Bottom - 1);
    end;
  end;
end;

function TposCustomColumn.UseRightToLeft: Boolean;
begin
  Result := RTLModeToRTL(Columns.Grid.UseRightToLeftAlignment, RightToLeftMode);
end;

{ TposCustomGrid }

constructor TposCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := CreateColumns;
  FCellMargin := 1;
  FDualColor := True;
  ShowHeader := True;
end;

function TposCustomGrid.CreateColumns: TposCustomColumns;
begin
  Result := TposColumns.Create;
  TposColumns(Result).FGrid := Self;
end;

destructor TposCustomGrid.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TposCustomGrid.GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo);
begin
end;

function TposCustomGrid.GetItems: TposItems;
begin
  Result := (inherited Items) as TposItems;
end;

function TposCustomGrid.CreateVisibleColumns: TposVisibleList;
var
  i: Integer;
  aItem: TposVisibleColumn;
  aTotal: Double;
  sPrecent: Double;
begin
  Result := TposVisibleList.Create;
  aTotal := 0;
  for i := 0 to Columns.Count - 1 do
  begin
    if Columns[i].Visible then
    begin
      aItem := TposVisibleColumn.Create;
      aItem.Index := i;
      aItem.FColumn := Columns[i];
      aTotal := aTotal + Columns[i].Width;
      Result.Add(aItem);
    end;
  end;

  if (aTotal <> 0) and (Result.Count > 0) then
  begin
    sPrecent := 0;
    for i := 0 to Result.Count - 1 do
    begin
      Result[i].Width := round((Result[i].Column.Width * 100) / aTotal);
      sPrecent := sPrecent + Result[i].Width;
    end;
    if 100 > sPrecent then
      Result[Result.Count - 1].Width := Result[Result.Count - 1].Width + (100 - sPrecent)
    else if 100 < sPrecent then
      Result[Result.Count - 1].Width := Result[Result.Count - 1].Width - (100 - sPrecent)
      //raise Exception.Create('There is an error here, check it!')
  end;
end;

function TposCustomGrid.CurrentData: Integer;
var
  aInfo: TposCellInfo;
begin
  InitMemory(aInfo, SizeOf(TposCellInfo));
  if (Columns.Count > 0) and (ItemIndex <> -1) then
    GetCellInfo(Columns[0], ItemIndex, aInfo); //? need review
  Result := aInfo.Data;
end;

procedure TposCustomGrid.PaintRow(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor);
var
  i: Integer;
  aItemRect: TRect;
  aTotal, aWidth: Double;
  W, H: Integer;
  Visibles: TposVisibleList;
  ACell: TposCellInfo;
begin
  //paint cells of columns
  aItemRect := Rect;
  W := aItemRect.Right - aItemRect.Left;
  Visibles := CreateVisibleColumns;
  try
    aTotal := Visibles.SumWidth;
    for i := 0 to Visibles.Count - 1 do
    begin
      aWidth := Visibles[i].Width;
      //aWidth := W * aWidth / 100;
      aWidth := W * aWidth / aTotal;
      if UseRightToLeftAlignment then
      begin
        aItemRect.Left := aItemRect.Right - Round(aWidth);
        if aItemRect.Left < Rect.Left then
          aItemRect.Left := Rect.Left;
      end
      else
      begin
        aItemRect.Right := aItemRect.Left + Round(aWidth);
        if aItemRect.Right > Rect.Right then
          aItemRect.Right := Rect.Right;
      end;
      InitMemory(ACell, SizeOf(ACell));
      VarClear(ACell.Value);
      ACell.Color := Color;
      ACell.Data := 0;
      ACell.AnObject := nil;
      GetCellInfo(Visibles[i].Column, Index, ACell);
      Visibles[i].Column.PaintCell(Canvas, ACell, Index, aItemRect, Color, (i >= Visibles.Count - 1));
      if ACell.StrikeThrough then
      begin
        H := aItemRect.Top + (aItemRect.Bottom - aItemRect.Top) div 2;
        Canvas.MoveTo(aItemRect.Left + 2, H);
        Canvas.LineTo(aItemRect.Right - 2, H);
      end;
      if UseRightToLeftAlignment then
        aItemRect.Right := aItemRect.Left
      else
        aItemRect.Left := aItemRect.Right;
    end;
    if UseRightToLeftAlignment then
    begin
      aItemRect.Right := aItemRect.Left;
      aItemRect.Left := Rect.Left;
    end
    else
    begin
      aItemRect.Left := aItemRect.Right;
      aItemRect.Right := Rect.Right;
    end;
  finally
    Visibles.Free;
  end;
  Canvas.FillRect(aItemRect);
end;

procedure TposCustomGrid.PaintHeader(Canvas: TCanvas; Rect: TRect; Color: TColor);
var
  i: Integer;
  aItemRect: TRect;
  aWidth: Double;
  aTotal: Double;
  W: Integer;
  Visibles: TposVisibleList;
begin
  //Paint the header of grid
  Canvas.Pen.Color := Lighten(Color, -25);
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := Color;
  aItemRect := Rect;
  W := aItemRect.Right - aItemRect.Left;

  Visibles := CreateVisibleColumns;
  try
    if Visibles.Count = 0 then
    begin
      Canvas.FillRect(Rect)
    end
    else
    begin
      aTotal := Visibles.SumWidth;
      for i := 0 to Visibles.Count - 1 do
      begin
        aWidth := Visibles[i].Width;
        //aWidth := W * aWidth / 100;
        aWidth := W * aWidth / aTotal;

        if UseRightToLeftAlignment then
        begin
          aItemRect.Left := aItemRect.Right - Round(aWidth);
          if aItemRect.Left < Rect.Left then
            aItemRect.Left := Rect.Left;
        end
        else
        begin
          aItemRect.Right := aItemRect.Left + Round(aWidth);
          if aItemRect.Right > Rect.Right then
            aItemRect.Right := Rect.Right;
        end;

        Visibles[i].Column.PaintHeader(Canvas, aItemRect, Color, (i >= Visibles.Count - 1));
        if UseRightToLeftAlignment then
          aItemRect.Right := aItemRect.Left
        else
          aItemRect.Left := aItemRect.Right;
      end;
      if UseRightToLeftAlignment then
      begin
        aItemRect.Right := aItemRect.Left;
        aItemRect.Left := Rect.Left;
      end
      else
      begin
        aItemRect.Left := aItemRect.Right;
        aItemRect.Right := Rect.Right;
      end;
    end;
  finally
    Visibles.Free;
  end;

  Canvas.FillRect(aItemRect);
end;

procedure TposCustomGrid.PaintInner(vCanvas: TCanvas; var vRect: TRect;
  vColor: TColor);
var
  aRect: TRect;
begin
  if ShowHeader then
  begin
    aRect := vRect;
    aRect.Bottom := aRect.Top + ItemHeight;
    Canvas.Brush.Color := clRed;
    PaintHeader(Canvas, aRect, Lighten(Color, -25));
    vRect.Top := aRect.Bottom;
  end;
  inherited;
end;

procedure TposCustomGrid.PaintOuter(Canvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
  inherited;
end;

function TposGridRowCells.Add: TposCell;
begin
  Result := CreateItem as TposCell;
  Add(Result);
end;

procedure TposGridRowCells.Add(ACell: TposCell);
begin
  ACell.FParent := Self;
  inherited Add(ACell);
end;

function TposGridRowCells.CreateItem: TposCell;
begin
  Result := TposCell.Create;
end;

procedure TposCustomGrid.SetCellMargin(const Value: Integer);
begin
  if FCellMargin <> Value then
  begin
    FCellMargin := Value;
    Refresh;
  end;
end;

procedure TposCustomGrid.SetCurrentColumn(const Value: Integer);
begin
  FCurrentColumn := Value;
end;

procedure TposCustomGrid.SetStartColumn(const Value: Integer);
begin
  FStartColumn := Value;
end;

{ TposVirtualGrid }

destructor TposVirtualGrid.Destroy;
begin
  inherited;
end;

function TposVirtualGrid.DoCreateItems: TposItems;
begin
  Result := TposVirtualRows.Create;
  TposVirtualRows(Result).FGrid := Self;
end;

procedure TposVirtualGrid.GetCellInfo(Column: TposCustomColumn; Row: Integer; var Info: TposCellInfo);
begin
  if Helper <> nil then
    Helper.GetCell(Self, Column.ID, Row, 0, Info)
  else if Assigned(FOnGetCell) then
    FOnGetCell(Self, Column.ID, Row, 0, Info);
end;

function TposVirtualGrid.GetHelper: TposGridHelper;
begin
  Result := inherited Helper as TposGridHelper;
end;

function TposVirtualGrid.GetItems: TposVirtualRows;
begin
  Result := (inherited Items) as TposVirtualRows;
end;

procedure TposVirtualGrid.HelperChanged;
begin
  inherited;
  Columns.Clear;
  if Helper <> nil then
  begin
    Helper.CreateColumns(Self);
  end;
end;

procedure TposVirtualGrid.SetHelper(const Value: TposGridHelper);
begin
  if Helper <> Value then
  begin
    inherited Helper := Value;
  end;
end;

{ TposVirtualRows }

procedure TposVirtualRows.Clear;
begin
  inherited;
end;

function TposVirtualRows.GetCount: Integer;
begin
  Result := 0;
  if Grid.Helper <> nil then
    Grid.Helper.GetCount(Grid, Result)
  else if Assigned(Grid.FOnGetCount) then
    Grid.FOnGetCount(Self, Result);
end;

function TposVirtualRows.GetItemIndex: Integer;
begin
  Result := -1;
  if Grid.Helper <> nil then
    Grid.Helper.GetItemIndex(Grid, Result)
  else if Assigned(Grid.FOnGetItemIndex) then
    Grid.FOnGetItemIndex(Self, Result);
end;

procedure TposVirtualRows.PaintItem(Canvas: TCanvas; Index: Integer; Rect: TRect; Color: TColor; RightToLeft: Boolean);
begin
  Grid.PaintRow(Canvas, Index, Rect, Color);
end;

procedure TposVirtualRows.SetCount(const Value: Integer);
begin
end;

procedure TposVirtualRows.SetItemIndex(const Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := GetItemIndex;
  if Grid.Helper <> nil then
    Grid.Helper.SetItemIndex(Grid, Value)
  else if Assigned(Grid.FOnSetItemIndex) then
    Grid.FOnSetItemIndex(Self, Value);
  if (Value <> OldIndex) and Assigned(Grid.FOnItemIndexChanged) then
    Grid.FOnItemIndexChanged(Self);
end;

{ TposVisibleList }

constructor TposVisibleList.Create;
begin
  inherited;
end;

function TposVisibleList.GetItem(Index: Integer): TposVisibleColumn;
begin
  Result := inherited Items[Index] as TposVisibleColumn;
end;

procedure TposVisibleList.SetItem(Index: Integer;
  const Value: TposVisibleColumn);
begin
  inherited Items[Index] := Value;
end;

function TposVisibleList.SumWidth: Double;
var
  i: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[i].Width;
end;

{ TposCheckedColumn }

procedure TposCheckedColumn.PaintCell(Canvas: TCanvas; ACell: TposCellInfo; Row: Integer; Rect: TRect; Color: TColor; LastCell: Boolean);
begin
  inherited;
  DrawShape(Canvas, Rect, shpCheck, False, True, UseRightToLeft, 0, Color);
end;

function TposGridRowCells.GetItems(Index: Integer): TposCell;
begin
  Result := inherited Items[Index] as TposCell;
end;

{ TposCell }

constructor TposCell.Create;
begin
  inherited Create;
  Info.Color := clDefault;
end;

initialization
finalization
end.

