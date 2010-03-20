unit mnrClasses;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, mnrLists;

const
  ID_SECTION_BASE           = 0;
  ID_SECTION_REPORT   = ID_SECTION_BASE + 1;
  ID_SECTION_HEADERREPORT   = ID_SECTION_BASE + 2;
  ID_SECTION_FOOTERREPORT   = ID_SECTION_BASE + 3;
  ID_SECTION_HEADERPAGE     = ID_SECTION_BASE + 4;
  ID_SECTION_FOOTERPAGE     = ID_SECTION_BASE + 5;
  ID_SECTION_HEADERDETAILS  = ID_SECTION_BASE + 6;
  ID_SECTION_DETAILS        = ID_SECTION_BASE + 7;


type
  TmnrSection = class;
  TmnrSections = class;
  TmnrCustomReport = class;
  TmnrLayout = class;
  TmnrLayouts = class;
  TmnrCustomReportCell = class;
  TmnrNodesRow = class;
  TmnrLayoutsRow = class;
  TmnrLayoutsRows = class;
  TmnrReferencesRow = class;
  TmnrReferences = class;
  TmnrReference = class;

  TmnrReportCellClass = class of TmnrReportCell;
  TmnrLayoutClass = class of TmnrLayout;

  TmnrNodesRowArray = array of TmnrNodesRow;

  TmnrSectionLoopWay = (slwSingle, slwMulti);
  TmnrFetchMode = (fmFirst, fmNext);
  TmnrAcceptMode = (acmAccept, acmSkip, acmSkipAll, acmRepeat, acmEof);
  TmnrSectionClassID = (sciReport, sciHeaderReport, sciHeaderPage, sciHeaderDetails, sciDetails, sciFooterDetails, sciFooterPage, sciFooterReport);

  TmnrFetchParams = record
    Mode: TmnrFetchMode;
    Accepted: TmnrAcceptMode;
  end;

  TOnRequest = procedure(vCell: TmnrCustomReportCell) of object;
  TOnFetch = procedure(var vParams: TmnrFetchParams) of object;

  TmnrCustomReportCell = class(TmnrLinkNode)
  private
    FLayout: TmnrLayout;
    FRow: TmnrNodesRow;
    FReference: TmnrReference;
    function GetNext: TmnrCustomReportCell;
    function GetPrior: TmnrCustomReportCell;
  protected

    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsCurrency: Currency; virtual; abstract;
    function GetAsDateTime: TDateTime; virtual; abstract;
    function GetAsFloat: Double; virtual; abstract;
    function GetAsInteger: Longint; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetAsVariant: Variant; virtual; abstract;
    function GetIsNull: Boolean; virtual; abstract;

    procedure SetAsBoolean(const Value: Boolean); virtual; abstract;
    procedure SetAsCurrency(const Value: Currency); virtual; abstract;
    procedure SetAsDateTime(const Value: TDateTime); virtual; abstract;
    procedure SetAsFloat(const Value: Double); virtual; abstract;
    procedure SetAsInteger(const Value: Longint); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetAsVariant(const Value: Variant); virtual; abstract;
  public

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Value: Variant read GetAsVariant write SetAsVariant;

    property Layout: TmnrLayout read FLayout;
    property Row: TmnrNodesRow read FRow;
    property Next: TmnrCustomReportCell read GetNext;
    property Prior: TmnrCustomReportCell read GetPrior;
    property Reference: TmnrReference read FReference;
  end;

  TmnrReportCell = class(TmnrCustomReportCell)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrReportCells = class(TmnrRowCells)
  private
    function GetFirst: TmnrReportCell;
    function GetLast: TmnrReportCell;
  public
    function Add: TmnrReportCell;
    property First: TmnrReportCell read GetFirst;
    property Last: TmnrReportCell read GetLast;
  end;

  TmnrNodesRow = class(TmnrRowNode)
  private
    FReferencesRow: TmnrReferencesRow;
    function GetCells: TmnrReportCells;
    function GetNext: TmnrNodesRow;
    function GetPrior: TmnrNodesRow;
    procedure SetReferencesRow(const Value: TmnrReferencesRow);
  protected
    function CreateCells: TmnrRowCells; override;
  public
    property Cells: TmnrReportCells read GetCells; //cells in row
    property Next: TmnrNodesRow read GetNext;
    property Prior: TmnrNodesRow read GetPrior;
    property ReferencesRow: TmnrReferencesRow read FReferencesRow write SetReferencesRow;
  end;

  TmnrNodesRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrNodesRow;
    function GetLast: TmnrNodesRow;
  public
    function Add: TmnrNodesRow;
    property First: TmnrNodesRow read GetFirst;
    property Last: TmnrNodesRow read GetLast;
  end;

  TmnrLayout = class(TmnrLinkNode)
  private
    FName: string;
    FOnRequest: TOnRequest;
    FReference: TmnrReference;
    function GetNext: TmnrLayout;
    function GetPrior: TmnrLayout;
    function GetCells: TmnrLayouts;
  protected
    procedure DoRequest(vCell: TmnrCustomReportCell); virtual;
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; virtual;
    procedure ScaleCell(vCell: TmnrCustomReportCell); virtual;
  public
    property Next: TmnrLayout read GetNext;
    property Prior: TmnrLayout read GetPrior;

    property Name: string read FName;
    procedure Request(vCell: TmnrCustomReportCell);
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    function NewCell(vRow: TmnrNodesRow): TmnrCustomReportCell;
    property Cells: TmnrLayouts read GetCells;
    property Reference: TmnrReference read FReference;
  end;

  TmnrLayouts = class(TmnrRowCells)
  private
    function GetFirst: TmnrLayout;
    function GetLast: TmnrLayout;
    function GetRow: TmnrLayoutsRow;
  public
    function Add: TmnrLayout;
    property First: TmnrLayout read GetFirst;
    property Last: TmnrLayout read GetLast;
    property Row: TmnrLayoutsRow read GetRow;
  end;

  TmnrLayoutsRow = class(TmnrRowNode)
  private
    function GetCells: TmnrLayouts;
    function GetNext: TmnrLayoutsRow;
    function GetPrior: TmnrLayoutsRow;
    function GetLayoutsRows: TmnrLayoutsRows;
  protected
    function CreateCells: TmnrRowCells; override;
  public
    property Next: TmnrLayoutsRow read GetNext;
    property Prior: TmnrLayoutsRow read GetPrior;
    property Cells: TmnrLayouts read GetCells; //cells in row
    procedure CreateLayout(vClass: TmnrLayoutClass; const vName: string; vRequest: TOnRequest=nil);//check useful ?????????
    property LayoutsRows: TmnrLayoutsRows read GetLayoutsRows;
  end;

  TmnrLayoutsRows = class(TmnrRowNodes)
  private
    FSection: TmnrSection;
    function GetFirst: TmnrLayoutsRow;
    function GetLast: TmnrLayoutsRow;
  public
    constructor Create(vSection: TmnrSection);
    function Add: TmnrLayoutsRow;
    property First: TmnrLayoutsRow read GetFirst;
    property Last: TmnrLayoutsRow read GetLast;
    property Section: TmnrSection read FSection;
  end;

  TmnrRowReference = class(TmnrLinkNode)
  private
    FRow: TmnrRowNode;
  public
    property Row: TmnrRowNode read FRow;
  end;

  TmnrRowReferences = class(TmnrLinkNodes)
  private
    function GetFirst: TmnrRowReference;
    function GetLast: TmnrRowReference;
  public
    function Add: TmnrRowReference;
    property First: TmnrRowReference read GetFirst;
    property Last: TmnrRowReference read GetLast;
  end;

  TmnrReference = class(TmnrLinkNode)
  private
    FTotal: Double;
    function GetNext: TmnrReference;
    function GetNodes: TmnrReferences;
    function GetPrior: TmnrReference;
    procedure SetNodes(const Value: TmnrReferences);
  public
    property Next: TmnrReference read GetNext;
    property Prior: TmnrReference read GetPrior;
    property Nodes: TmnrReferences read GetNodes write SetNodes;
    property Total: Double read FTotal write FTotal;
  end;

  TmnrReferences = class(TmnrRowCells)
  private
    function GetFirst: TmnrReference;
    function GetLast: TmnrReference;
    function GetRow: TmnrReferencesRow;
  public
    function Add: TmnrReference;
    property First: TmnrReference read GetFirst;
    property Last: TmnrReference read GetLast;
    property Row: TmnrReferencesRow read GetRow;
  end;

  TmnrReferencesRow = class(TmnrRowNode)
  private
    function GetCells: TmnrReferences;
    function GetNext: TmnrReferencesRow;
    function GetPrior: TmnrReferencesRow;
  protected
    function CreateCells: TmnrRowCells; override;
  public
    constructor Create(vNodes: TmnrNodes); override;
    destructor Destroy; override;

    property Next: TmnrReferencesRow read GetNext;
    property Prior: TmnrReferencesRow read GetPrior;
    property Cells: TmnrReferences read GetCells; //cells in row
  end;

  TmnrReferencesRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrReferencesRow;
    function GetLast: TmnrReferencesRow;
  public
    function Add: TmnrReferencesRow;
    property First: TmnrReferencesRow read GetFirst;
    property Last: TmnrReferencesRow read GetLast;
  end;

  TmnrSection = class(TmnrLinkNode)
  private
    FSections: TmnrSections;
    FName: string;
    FCaption: string;
    FID: integer;
    FLayoutsRows: TmnrLayoutsRows;
    FClassID: TmnrSectionClassID;
    FOnFetch: TOnFetch;
    FReferencesRows: TmnrReferencesRows;
    FItems: TmnrRowReferences;
    FAppendDetailTotals: Boolean;
    function GetNext: TmnrSection;
    function GetNodes: TmnrSections;
    function GetPrior: TmnrSection;
    procedure SetNodes(const Value: TmnrSections);
    function GetReport: TmnrCustomReport;
    function GetLoopWay: TmnrSectionLoopWay;
  protected
    function DoFetch(var vParams: TmnrFetchParams): TmnrAcceptMode; virtual;
  public
    constructor Create(vNodes: TmnrNodes); override;
    destructor Destroy; override;
    property Sections: TmnrSections read FSections;
    property Items: TmnrRowReferences read FItems;

    property Next: TmnrSection read GetNext;
    property Prior: TmnrSection read GetPrior;
    property Nodes: TmnrSections read GetNodes write SetNodes;
    property Report: TmnrCustomReport read GetReport;

    //function AddLayout
    property LayoutsRows: TmnrLayoutsRows read FLayoutsRows;
    property ReferencesRows: TmnrReferencesRows read FReferencesRows;
    function NewReference: TmnrReferencesRow;
    procedure AppendTotals(vSection: TmnrSection);

    property Name: string read FName;
    property ID: integer read FID;
    property ClassID: TmnrSectionClassID read FClassID;
    property Caption: string read FCaption;
    property LoopWay: TmnrSectionLoopWay read GetLoopWay;
    property OnFetch: TOnFetch read FOnFetch write FOnFetch; 

    procedure FillNow(vReference: TmnrReferencesRow);
    property AppendDetailTotals: Boolean read FAppendDetailTotals write FAppendDetailTotals; 
  end;

  TmnrSections = class(TmnrLinkNodes)
  private
    FReport: TmnrCustomReport;

    function GetFirst: TmnrSection;
    function GetLast: TmnrSection;
    function GetByName(const vName: string): TmnrSection;
    function GetReport: TmnrCustomReport;
  protected
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
    function RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer=0; vOnFetch: TOnFetch=nil): TmnrSection;
    property ByName[const vName: string]: TmnrSection read GetByName;

    property Report: TmnrCustomReport read GetReport;
    property First: TmnrSection read GetFirst;
    property Last: TmnrSection read GetLast;

    procedure Loop;
  end;

  TmnrIndexer = class
  protected
    procedure Compute(vReport: TmnrCustomReport); virtual;
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
  end;

  TmnrRowsListIndexer = class(TmnrIndexer)
  private
    FArray: TmnrNodesRowArray;
    function GetItems(vIndex: Integer): TmnrNodesRow;
  protected
    procedure Compute(vReport: TmnrCustomReport); override;
  public
    property Items[vIndex: Integer]: TmnrNodesRow read GetItems;
  end;

  TmnrCustomReportDesigner = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DesignReport(vClass: TmnrCustomReport); virtual;
  end;

  TmnrCustomReport = class
  private
    FCanceled: Boolean;
    FItems: TmnrNodesRows;
    FSections: TmnrSections;
    FRowsListIndexer: TmnrRowsListIndexer;
    FDetailTotals: TmnrSection;

    function GetCells(Row, Column: Integer): TmnrCustomReportCell;
  protected
    function Canceled: Boolean;
    procedure HandleNewRow(vRow: TmnrRowNode); virtual;
    procedure CreateSections; virtual;
    function CreateNewRow(vSection: TmnrSection): TmnrNodesRow; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Sections: TmnrSections read FSections;
    property Items: TmnrNodesRows read FItems;
    procedure Cancel;

    procedure Init; virtual;
    procedure Prepare; virtual;
    procedure Start; virtual;
    procedure Finish; virtual;
    procedure Loop;
    procedure Generate;

    procedure Fetch(vSection: TmnrSection; var vParams: TmnrFetchParams); virtual;

    property Cells[Row, Column: Integer]: TmnrCustomReportCell read GetCells;
    property RowsListIndexer: TmnrRowsListIndexer read FRowsListIndexer;

    procedure ExportCSV(const vFile: TFileName); overload; //test purpose only
    procedure ExportCSV(const vStream: TStream); overload; //test purpose only
  end;

var
  DefaultCellClass: TmnrReportCellClass = nil;

implementation

uses
  mnrNodes;

{ TmnrCustomReport }

procedure TmnrCustomReport.Cancel;
begin
  FCanceled := True;
end;

function TmnrCustomReport.Canceled: Boolean;
begin
  Result := FCanceled;
end;

constructor TmnrCustomReport.Create;
begin
  inherited Create;
  FSections := TmnrSections.Create(Self);
  FItems := TmnrNodesRows.Create;
  FRowsListIndexer := nil;
  FDetailTotals := TmnrSection.Create(nil);
  CreateSections;
  Init;
end;

function TmnrCustomReport.CreateNewRow(vSection: TmnrSection): TmnrNodesRow;
begin
  Result := TmnrNodesRow.Create(Items);
end;

procedure TmnrCustomReport.CreateSections;
begin
end;

destructor TmnrCustomReport.Destroy;
begin
  FDetailTotals.Free;
  FSections.Free;
  FItems.Free;
  FRowsListIndexer.Free;
  inherited;
end;

procedure TmnrCustomReport.ExportCSV(const vStream: TStream);
  procedure WriteStr(const vStr: string);
  begin
    vStream.Write(vStr[1], Length(vStr));
  end;
var
  r: TmnrNodesRow;
  n: TmnrCustomReportCell;
begin
  r := Items.First;
  while r<>nil do
  begin
    n := r.Cells.First;
    while n<>nil do
    begin
      WriteStr(n.AsString);
      n := n.Next;
      if n<>nil then
        WriteStr(';');
    end;

    r := r.Next;
    if r<>nil then
      WriteStr(#13#10);
  end;
end;

procedure TmnrCustomReport.ExportCSV(const vFile: TFileName);
var
  f: TFileStream;
begin
  f := TFileStream.Create(vFile, fmCreate);
  try
    ExportCSV(f);
  finally
    f.Free;
  end;
end;

procedure TmnrCustomReport.Fetch(vSection: TmnrSection; var vParams: TmnrFetchParams);
begin
end;

procedure TmnrCustomReport.Finish;
begin
  FRowsListIndexer := TmnrRowsListIndexer.Create(Self);
end;

procedure TmnrCustomReport.Generate;
begin
  Start;
  try
    Loop;
  finally //handle safe finish ........
    Finish;
  end;
end;

function TmnrCustomReport.GetCells(Row, Column: Integer): TmnrCustomReportCell;
var
  r: TmnrNodesRow;
  i: Integer;
begin
  if RowsListIndexer<>nil then
  begin
    r := RowsListIndexer.Items[Row];
    if r<>nil then
    begin
      i := 0;
      Result := r.Cells.First;
      while (Result<>nil) and (i<Column) do
      begin
        Result := Result.Next;
        Inc(i);
      end;
    end
    else
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TmnrCustomReport.HandleNewRow(vRow: TmnrRowNode);
begin
end;

procedure TmnrCustomReport.Init;
begin

end;

procedure TmnrCustomReport.Loop;
begin
  FCanceled := False;
  Sections.Loop;
end;

procedure TmnrCustomReport.Prepare;
begin

end;

procedure TmnrCustomReport.Start;
begin

end;

{ TmnrCustomReportRowNode }

function TmnrNodesRow.CreateCells: TmnrRowCells;
begin
  Result := TmnrReportCells.Create;
end;

{ TmnrNodesRows }

function TmnrNodesRows.Add: TmnrNodesRow;
begin
  Result := TmnrNodesRow.Create(Self);
end;

function TmnrNodesRows.GetFirst: TmnrNodesRow;
begin
  Result := TmnrNodesRow(inherited First);
end;

function TmnrNodesRows.GetLast: TmnrNodesRow;
begin
  Result := TmnrNodesRow(inherited Last);
end;

{ TmnrSection }

procedure TmnrSection.AppendTotals(vSection: TmnrSection);
var
  r: TmnrLayoutsRow;
  l: TmnrLayout;
  aRow: TmnrNodesRow;
  f: Boolean; //first
  c: TmnrCustomReportCell;
begin
  r := LayoutsRows.First;
  if r<>nil then
  begin
    f := True;
    while r<>nil do
    begin
      aRow := Report.CreateNewRow(vSection);
      try
        l := r.Cells.First;
        while l<>nil do
        begin
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow.Cells);
            c.AsString := '«·„Ã„Ê⁄';
          end
          else
          begin
            c := TmnrCurrencyReportCell.Create(aRow.Cells);
            if l.Reference<>nil then
              c.AsCurrency := l.Reference.Total;
          end;
          c.FRow := aRow;
          c.FLayout := l;
          c.FReference := l.Reference;


          //c := l.NewCell(aRow);
          //l.NewCell(aRow);
          l := l.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      with vSection.Items.Add do
      begin
        FRow := aRow;
      end;

      r := r.Next;
    end;
  end;
end;

constructor TmnrSection.Create(vNodes: TmnrNodes);
begin
  inherited;
  FSections := TmnrSections.Create(Report);
  FLayoutsRows := TmnrLayoutsRows.Create(Self);
  FReferencesRows := TmnrReferencesRows.Create;
  FItems := TmnrRowReferences.Create;
end;

destructor TmnrSection.Destroy;
begin
  FSections.Free;
  FLayoutsRows.Free;
  FReferencesRows.Free;
  FItems.Free;
  inherited;
end;

function TmnrSection.DoFetch(var vParams: TmnrFetchParams): TmnrAcceptMode;
begin
  Result := acmAccept;
  if Assigned(FOnFetch) then
    FOnFetch(vParams)
  else
    Report.Fetch(Self, vParams);
end;

procedure TmnrSection.FillNow(vReference: TmnrReferencesRow);
var
  r: TmnrLayoutsRow;
  l: TmnrLayout;
  aRow: TmnrNodesRow;
  //c: TmnrCustomReportCell;
begin
  r := LayoutsRows.First;
  if r<>nil then
  begin
    while r<>nil do
    begin
      aRow := Report.CreateNewRow(Self);
      try
        l := r.Cells.First;
        while l<>nil do
        begin
          //c := l.NewCell(aRow);
          l.NewCell(aRow);
          l := l.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      with Items.Add do
      begin
        FRow := aRow;
      end;

      r := r.Next;
    end;
  end;
end;

function TmnrSection.GetLoopWay: TmnrSectionLoopWay;
begin
  if ClassID in [sciHeaderDetails, sciDetails] then
    Result := slwMulti
  else
    Result := slwSingle;  
end;

function TmnrSection.GetNext: TmnrSection;
begin
  Result := TmnrSection(inherited GetNext);
end;

function TmnrSection.GetNodes: TmnrSections;
begin
  Result := TmnrSections(inherited GetNodes);
end;

function TmnrSection.GetPrior: TmnrSection;
begin
  Result := TmnrSection(inherited GetPrior);
end;

function TmnrSection.GetReport: TmnrCustomReport;
begin
  if Nodes<>nil then
    Result := Nodes.Report
  else
    Result := nil;
end;

function TmnrSection.NewReference: TmnrReferencesRow;
var
  r: TmnrLayoutsRow;
  l: TmnrLayout;
begin
  Result := ReferencesRows.Add;
  r := LayoutsRows.First;
  if r<>nil then
  begin
    while r<>nil do
    begin
      l := r.Cells.First;
      while l<>nil do
      begin
        l.FReference := Result.Cells.Add;
        l := l.Next;
      end;
      r := r.Next;
    end;
  end;
end;

procedure TmnrSection.SetNodes(const Value: TmnrSections);
begin
  inherited SetNodes(Value);
end;

{ TmnrSections }

constructor TmnrSections.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  FReport := vReport;
end;

destructor TmnrSections.Destroy;
begin

  inherited;
end;

function TmnrSections.GetByName(const vName: string): TmnrSection;
var
  p: TmnrSection;
begin
  Result := nil;
  p := First;
  while p<>nil do
  begin
    if SameText(p.Name, vName) then
    begin
      Result := p;
      Break;
    end;
    p := p.Next;
  end;
end;

function TmnrSections.GetFirst: TmnrSection;
begin
  Result := TmnrSection(inherited GetFirst);
end;

function TmnrSections.GetLast: TmnrSection;
begin
  Result := TmnrSection(inherited GetLast);
end;

function TmnrSections.GetReport: TmnrCustomReport;
begin
  Result := FReport;
end;

procedure TmnrSections.Loop;
var
  s: TmnrSection;
  fparams: TmnrFetchParams;
  r: TmnrReferencesRow;
begin
  s := First;
  while s<>nil do
  begin
    case s.LoopWay of
      slwSingle:
      begin
        fparams.Mode := fmFirst;
        s.DoFetch(fparams);
        if fparams.Accepted=acmAccept then
        begin
          s.FillNow(nil);
          s.Sections.Loop;
        end;
      end;
      slwMulti:
      begin
        fparams.Mode := fmFirst;
        fparams.Accepted := acmAccept;
        r := nil;
        while not Report.Canceled and (fparams.Accepted=acmAccept) do
        begin
          s.DoFetch(fparams);
          if (s.ClassID=sciDetails) and (fparams.Mode=fmFirst) then //improve add referance on first accepted ...
            r := s.NewReference;

          if fparams.Accepted = acmAccept then
          begin
            s.FillNow(r);
            s.Sections.Loop;
          end
          else if (fparams.Accepted = acmSkip) and (s.ClassID=sciHeaderDetails) then
            s.Sections.Loop;

          if (fparams.Accepted = acmEof) and (s.Items.Count<>0) then
          begin
            if (r<>nil) and s.AppendDetailTotals then
            begin
              s.AppendTotals(Report.FDetailTotals);
            end;
          end;


          if fparams.Mode=fmFirst then
            fparams.Mode := fmNext;
        end;
      end;
    end;
    s := s.Next;
  end;
end;

function TmnrSections.RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer; vOnFetch: TOnFetch): TmnrSection;
begin
  Result := TmnrSection.Create(Self);
  Result.FName := vName;
  Result.FClassID := vClass;
  Result.OnFetch := vOnFetch;
  Result.FCaption := vCaption;
  if vID=0 then
    Result.FID := Ord(vClass)
  else
    Result.FID := vID;
  //Result.FLoopWay := vLoopWay;
end;

{ TmnrLayoutsRow }

function TmnrLayoutsRow.CreateCells: TmnrRowCells;
begin
  Result := TmnrLayouts.Create;
end;

procedure TmnrLayoutsRow.CreateLayout(vClass: TmnrLayoutClass; const vName: string; vRequest: TOnRequest);
begin
  with vClass.Create(Cells) do
  begin
    FName := vName;
    OnRequest := vRequest;
  end;
end;

function TmnrLayoutsRow.GetCells: TmnrLayouts;
begin
  Result := TmnrLayouts(inherited Cells);
end;

function TmnrLayoutsRow.GetLayoutsRows: TmnrLayoutsRows;
begin
  Result := TmnrLayoutsRows(Nodes);
end;

function TmnrLayoutsRow.GetNext: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow(inherited GetNext);
end;

function TmnrLayoutsRow.GetPrior: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow(inherited GetPrior);
end;

{ TmnrLayoutsRows }

function TmnrLayoutsRows.Add: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow.Create(Self);
end;

constructor TmnrLayoutsRows.Create(vSection: TmnrSection);
begin
  inherited Create;
  FSection := vSection;
end;

function TmnrLayoutsRows.GetFirst: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow(inherited GetFirst);
end;

function TmnrLayoutsRows.GetLast: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow(inherited GetLast);
end;

function TmnrNodesRow.GetCells: TmnrReportCells;
begin
  Result := TmnrReportCells(inherited Cells);
end;

function TmnrNodesRow.GetNext: TmnrNodesRow;
begin
  Result := TmnrNodesRow(inherited GetNext);
end;

function TmnrNodesRow.GetPrior: TmnrNodesRow;
begin
  Result := TmnrNodesRow(inherited GetPrior);
end;

procedure TmnrNodesRow.SetReferencesRow(const Value: TmnrReferencesRow);
begin
  FReferencesRow := Value;
end;

{ TmnrLayout }

function TmnrLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := nil;
end;

procedure TmnrLayout.DoRequest(vCell: TmnrCustomReportCell);
begin
  if Assigned(FOnRequest) then
    FOnRequest(vCell);
end;

function TmnrLayout.GetCells: TmnrLayouts;
begin
  Result := TmnrLayouts(Nodes);
end;

function TmnrLayout.GetNext: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetNext);
end;

function TmnrLayout.GetPrior: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetPrior);
end;

function TmnrLayout.NewCell(vRow: TmnrNodesRow): TmnrCustomReportCell;
begin
  Result := CreateCell(vRow.Cells);
  if Result<>nil then
  begin
    try
      Result.FReference := Reference;
      Result.FRow := vRow;
      Result.FLayout := Self;
      DoRequest(Result);
      ScaleCell(Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else
    raise Exception.Create(Format('Error Creating Cell for %s', [Name]));
end;

procedure TmnrLayout.Request(vCell: TmnrCustomReportCell);
begin
  DoRequest(vCell);
end;

procedure TmnrLayout.ScaleCell(vCell: TmnrCustomReportCell);
begin

end;

{ TmnrLayouts }

function TmnrLayouts.Add: TmnrLayout;
begin
  Result := TmnrLayout.Create(Self);
end;

function TmnrLayouts.GetFirst: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetFirst);
end;

function TmnrLayouts.GetLast: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetLast);
end;

function TmnrLayouts.GetRow: TmnrLayoutsRow;
begin
  Result := TmnrLayoutsRow(inherited GetRow);
end;

{ TmnrReportCell }

function TmnrReportCell.GetAsBoolean: Boolean;
begin
  Result := False;
end;

function TmnrReportCell.GetAsCurrency: Currency;
begin
  Result := 0;
end;

function TmnrReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsCurrency;
end;

function TmnrReportCell.GetAsFloat: Double;
begin
  Result := AsCurrency;
end;

function TmnrReportCell.GetAsInteger: Longint;
begin
  Result := 0;
end;

function TmnrReportCell.GetAsString: string;
begin
  Result := '';
end;

function TmnrReportCell.GetAsVariant: Variant;
begin
  Result := '';
end;

function TmnrReportCell.GetIsNull: Boolean;
begin
  Result := AsString='';
end;

procedure TmnrReportCell.SetAsBoolean(const Value: Boolean);
begin

end;

procedure TmnrReportCell.SetAsCurrency(const Value: Currency);
begin

end;

procedure TmnrReportCell.SetAsDateTime(const Value: TDateTime);
begin

end;

procedure TmnrReportCell.SetAsFloat(const Value: Double);
begin

end;

procedure TmnrReportCell.SetAsInteger(const Value: Integer);
begin

end;

procedure TmnrReportCell.SetAsString(const Value: string);
begin

end;

procedure TmnrReportCell.SetAsVariant(const Value: Variant);
begin

end;

{ TmnrReportCells }

function TmnrReportCells.Add: TmnrReportCell;
begin
  if DefaultCellClass<>nil then
    Result := DefaultCellClass.Create(Self)
  else
    Result := nil;
end;

function TmnrReportCells.GetFirst: TmnrReportCell;
begin
  Result := TmnrReportCell(inherited GetFirst);
end;

function TmnrReportCells.GetLast: TmnrReportCell;
begin
  Result := TmnrReportCell(inherited GetLast);
end;

{ TmnrCustomReportCell }

function TmnrCustomReportCell.GetNext: TmnrCustomReportCell;
begin
  Result := TmnrCustomReportCell(inherited GetNext);
end;

function TmnrCustomReportCell.GetPrior: TmnrCustomReportCell;
begin
  Result := TmnrCustomReportCell(inherited GetPrior);
end;

{ TmnrReferences }

function TmnrReferences.Add: TmnrReference;
begin
  Result := TmnrReference.Create(Self);
end;

function TmnrReferences.GetFirst: TmnrReference;
begin
  Result := TmnrReference(inherited First);
end;

function TmnrReferences.GetLast: TmnrReference;
begin
  Result := TmnrReference(inherited First);
end;

function TmnrReferences.GetRow: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetRow);
end;

{ TmnrReferencesRow }

constructor TmnrReferencesRow.Create(vNodes: TmnrNodes);
begin
  inherited;
end;

function TmnrReferencesRow.CreateCells: TmnrRowCells;
begin
  Result := TmnrReferences.Create;
end;

destructor TmnrReferencesRow.Destroy;
begin
  inherited;
end;

function TmnrReferencesRow.GetCells: TmnrReferences;
begin
  Result := TmnrReferences(inherited Cells);
end;

function TmnrReferencesRow.GetNext: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetNext);
end;

function TmnrReferencesRow.GetPrior: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetPrior);
end;

{ TmnrReferencesRows }

function TmnrReferencesRows.Add: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow.Create(Self);
end;

function TmnrReferencesRows.GetFirst: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetFirst);
end;

function TmnrReferencesRows.GetLast: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetLast);
end;

{ TmnrRowReferences }

function TmnrRowReferences.Add: TmnrRowReference;
begin
  Result := TmnrRowReference.Create(Self);
end;

function TmnrRowReferences.GetFirst: TmnrRowReference;
begin
  Result := TmnrRowReference(inherited GetFirst);
end;

function TmnrRowReferences.GetLast: TmnrRowReference;
begin
  Result := TmnrRowReference(inherited GetLast);
end;

{ TmnrReference }

function TmnrReference.GetNext: TmnrReference;
begin
  Result := TmnrReference(inherited GetNext);
end;

function TmnrReference.GetNodes: TmnrReferences;
begin
  Result := TmnrReferences(inherited GetNodes);
end;

function TmnrReference.GetPrior: TmnrReference;
begin
  Result := TmnrReference(inherited GetPrior);
end;

procedure TmnrReference.SetNodes(const Value: TmnrReferences);
begin
  inherited SetNodes(Value);
end;

{ TmnrRowsListIndexer }

procedure TmnrRowsListIndexer.Compute(vReport: TmnrCustomReport);
var
  i: Integer;
  r: TmnrNodesRow;
begin
  SetLength(FArray, vReport.Items.Count);
  i := 0;
  r := vReport.Items.First;
  while r<>nil do
  begin
    FArray[i] := r;
    Inc(i);
    r := r.Next;
  end;
end;

function TmnrRowsListIndexer.GetItems(vIndex: Integer): TmnrNodesRow;
begin
  if (vIndex>=0) and (vIndex<Length(FArray)) then
    Result := FArray[vIndex]
  else
    Result := nil;
end;

{ TmnrCustomReportDesigner }

constructor TmnrCustomReportDesigner.Create;
begin
  inherited Create;
end;

procedure TmnrCustomReportDesigner.DesignReport(vClass: TmnrCustomReport);
begin
  
end;

destructor TmnrCustomReportDesigner.Destroy;
begin

  inherited;
end;

{ TmnrIndexer }

procedure TmnrIndexer.Compute(vReport: TmnrCustomReport);
begin

end;

constructor TmnrIndexer.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  Compute(vReport);
end;

destructor TmnrIndexer.Destroy;
begin

  inherited;
end;

end.
