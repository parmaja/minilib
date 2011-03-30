unit mnrClasses;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

{
[DONE]
Rename Init to Created

[TODO]
Rename AcceptMode to Resume

}

interface

uses
  SysUtils, Classes,
  mnFields,
  mnrLists;

const
  ID_SECTION_BASE = 0;
  ID_SECTION_REPORT = ID_SECTION_BASE + 1;
  ID_SECTION_HEADERREPORT = ID_SECTION_BASE + 2;
  ID_SECTION_FOOTERREPORT = ID_SECTION_BASE + 3;
  ID_SECTION_HEADERPAGE = ID_SECTION_BASE + 4;
  ID_SECTION_FOOTERPAGE = ID_SECTION_BASE + 5;
  ID_SECTION_HEADERDETAILS = ID_SECTION_BASE + 6;
  ID_SECTION_DETAILS = ID_SECTION_BASE + 7;
  ID_SECTION_LAST = ID_SECTION_BASE + 8;

type
  TmnrSection = class;
  TmnrSections = class;
  TmnrCustomReport = class;
  TmnrLayout = class;
  TmnrLayouts = class;
  TmnrCell = class;
  TmnrRow = class;
  TmnrReferencesRow = class;
  TmnrReferences = class;
  TmnrReference = class;
  TCustomReportDesigner = class;
  TmnrDesignCell = class;
  TmnrDesignCells = class;
  TmnrDesignRow = class;
  TmnrDesignRows = class;
  TmnrProfiler = class;

  TmnrReportCellClass = class of TmnrReportCell;
  TmnrLayoutClass = class of TmnrLayout;
  TmnrCustomReportClass = class of TmnrCustomReport;
  TCustomReportDesignerClass = class of TCustomReportDesigner;
  TmnrProfilerClass = class of TmnrProfiler;

  TmnrRowArray = array of TmnrRow;

  TmnrSectionLoopWay = (slwSingle, slwMulti);
  TmnrFetchMode = (fmFirst, fmNext);
  TmnrAcceptMode = (acmAccept, acmSkip, acmSkipAll, acmRepeat, acmEof);
  TmnrSectionClassID = (sciReport, sciHeaderReport, sciHeaderPage, sciHeaderDetails, sciDetails, sciFooterDetails, sciFooterPage, sciFooterReport);
  TmnrSectionClassIDs = set of TmnrSectionClassID;

  TmnrFetch = record
    FetchMode: TmnrFetchMode;
    AcceptMode: TmnrAcceptMode;
    ID: Integer;
    Number: Integer;
  end;

  TOnRequest = procedure(vCell: TmnrCell) of object;
  TOnFetch = procedure(var vParams: TmnrFetch) of object;

  TmnrCell = class(TmnrLinkNode)
  private
    FRow: TmnrRow;
    FReference: TmnrReference;
    FLayout: TmnrLayout;
    function GetNext: TmnrCell;
    function GetPrior: TmnrCell;
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
    property Row: TmnrRow read FRow;
    property Next: TmnrCell read GetNext;
    property Prior: TmnrCell read GetPrior;
    property Reference: TmnrReference read FReference;
  end;

  TmnrReportCell = class(TmnrCell)
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

  TmnrRow = class(TmnrRowNode)
  private
    FReferencesRow: TmnrReferencesRow;
    FID: Int64;
    FNumber: Integer;
    function GetCells: TmnrReportCells;
    function GetNext: TmnrRow;
    function GetPrior: TmnrRow;
    procedure SetReferencesRow(const Value: TmnrReferencesRow);
  protected
    function CreateCells: TmnrRowCells; override;
  public
    function GetCellByIndex(I: Integer): TmnrCell;
    property Cells: TmnrReportCells read GetCells; //cells in row
    property Next: TmnrRow read GetNext;
    property Prior: TmnrRow read GetPrior;
    property ReferencesRow: TmnrReferencesRow read FReferencesRow write SetReferencesRow;

    property ID: Int64 read FID write FID;
    property Number: Integer read FNumber write FNumber;
  end;

  TmnrRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrRow;
    function GetLast: TmnrRow;
  public
    function Add: TmnrRow;
    property First: TmnrRow read GetFirst;
    property Last: TmnrRow read GetLast;
  end;

  TmnrLayoutInfo = record
    Name: string;
    Title: string;
    IncludeSections: TmnrSectionClassIDs;
    ExcludeSections: TmnrSectionClassIDs;
  end;

  TmnrLayout = class(TmnrLinkNode)
  private
    FOnRequest: TOnRequest;
    FReference: TmnrReference;
    FDesignerCell: TmnrDesignCell;
    function GetNext: TmnrLayout;
    function GetPrior: TmnrLayout;
  protected
    procedure DoRequest(vCell: TmnrCell); virtual;
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; virtual;
    procedure ScaleCell(vCell: TmnrCell); virtual;
    function GetTotal: Double; virtual;
  public
    Info: TmnrLayoutInfo;
    property Next: TmnrLayout read GetNext;
    property Prior: TmnrLayout read GetPrior;

    property Name: string read Info.Name;
    property Title: string read Info.Title;
    property IncludeSections: TmnrSectionClassIDs read Info.IncludeSections;
    property ExcludeSections: TmnrSectionClassIDs read Info.ExcludeSections;
    procedure Request(vCell: TmnrCell);
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    function NewCell(vRow: TmnrRow): TmnrCell;
    property Reference: TmnrReference read FReference;
    property DesignerCell: TmnrDesignCell read FDesignerCell write FDesignerCell;
    property Total: Double read GetTotal;
  end;

  TmnrLayouts = class(TmnrLinkNodes)
  private
    function GetFirst: TmnrLayout;
    function GetLast: TmnrLayout;
  public
    function Add: TmnrLayout;
    property First: TmnrLayout read GetFirst;
    property Last: TmnrLayout read GetLast;
    procedure CreateLayout(vClass: TmnrLayoutClass; const vName: string; const vTitle: string; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []); overload;
    procedure CreateLayout(vClass: TmnrLayoutClass; const vInfo: TmnrLayoutInfo); overload;
    function Find(const vName: string): TmnrLayout;
  end;

  TmnrDesignCell = class(TmnrLinkNode)
  private
    FWidth: Integer;
    FLayout: TmnrLayout;
    FName: string;
    function GetNext: TmnrDesignCell;
    function GetPrior: TmnrDesignCell;
    function GetCells: TmnrDesignCells;
    function GetReport: TmnrCustomReport;
    function GetSection: TmnrSection;
    procedure SetName(const Value: string);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(vNodes: TmnrNodes); override;
    constructor AutoCreate(vNodes: TmnrNodes; const vName: string; vWidth: Integer = 100); virtual;
    destructor Destroy; override;

    property Next: TmnrDesignCell read GetNext;
    property Prior: TmnrDesignCell read GetPrior;
    property Width: Integer read FWidth write SetWidth default 100;
    property Layout: TmnrLayout read FLayout write FLayout;
    property Cells: TmnrDesignCells read GetCells;
    property Section: TmnrSection read GetSection;
    property Report: TmnrCustomReport read GetReport;
    property Name: string read FName write SetName;
  end;

  TmnrDesignCells = class(TmnrRowCells)
  private
    function GetFirst: TmnrDesignCell;
    function GetLast: TmnrDesignCell;
    function GetRow: TmnrDesignRow;
    function GetReport: TmnrCustomReport;
    function GetSection: TmnrSection;
  public
    function Add: TmnrDesignCell;
    property First: TmnrDesignCell read GetFirst;
    property Last: TmnrDesignCell read GetLast;
    property Row: TmnrDesignRow read GetRow;
    property Section: TmnrSection read GetSection;
    property Report: TmnrCustomReport read GetReport;
  end;

  TmnrDesignRow = class(TmnrRowNode)
  private
    function GetCells: TmnrDesignCells;
    function GetNext: TmnrDesignRow;
    function GetPrior: TmnrDesignRow;
    function GetDesignRows: TmnrDesignRows;
    function GetSection: TmnrSection;
    function GetReport: TmnrCustomReport;
  protected
    function CreateCells: TmnrRowCells; override;
  public
    property Next: TmnrDesignRow read GetNext;
    property Prior: TmnrDesignRow read GetPrior;
    property Cells: TmnrDesignCells read GetCells; //cells in row
    property DesignRows: TmnrDesignRows read GetDesignRows;
    property Section: TmnrSection read GetSection;
    property Report: TmnrCustomReport read GetReport;
    function SumWidth: Integer;
  end;

  TmnrDesignRows = class(TmnrRowNodes)
  private
    FSection: TmnrSection;
    function GetFirst: TmnrDesignRow;
    function GetLast: TmnrDesignRow;
  public
    constructor Create(vSection: TmnrSection);
    function Add: TmnrDesignRow;
    property First: TmnrDesignRow read GetFirst;
    property Last: TmnrDesignRow read GetLast;
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
    FDesignRows: TmnrDesignRows;
    FClassID: TmnrSectionClassID;
    FOnFetch: TOnFetch;
    FReferencesRows: TmnrReferencesRows;
    FItems: TmnrRowReferences;
    FAppendTotals: Boolean;
    FAppendSummary: Boolean;
    function GetNext: TmnrSection;
    function GetNodes: TmnrSections;
    function GetPrior: TmnrSection;
    procedure SetNodes(const Value: TmnrSections);
    function GetReport: TmnrCustomReport;
    function GetLoopWay: TmnrSectionLoopWay;
  protected
    function DoFetch(var vParams: TmnrFetch): TmnrAcceptMode; virtual;
    procedure DoAppendTotals(vTotalSection: TmnrSection);
    procedure DoAppendSummary(vSummarySection: TmnrSection);
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
    property DesignRows: TmnrDesignRows read FDesignRows;
    property ReferencesRows: TmnrReferencesRows read FReferencesRows;
    function NewReference: TmnrReferencesRow;

    property Name: string read FName;
    property ID: integer read FID;
    property ClassID: TmnrSectionClassID read FClassID;
    property Caption: string read FCaption;
    property LoopWay: TmnrSectionLoopWay read GetLoopWay;
    property OnFetch: TOnFetch read FOnFetch write FOnFetch;

    procedure FillNow(vParams: TmnrFetch; vReference: TmnrReferencesRow);
  published
    property AppendTotals: Boolean read FAppendTotals write FAppendTotals;
    property AppendSummary: Boolean read FAppendSummary write FAppendSummary;
  end;

  TmnrSections = class(TmnrLinkNodes)
  private
    FReport: TmnrCustomReport;
    function GetFirst: TmnrSection;
    function GetLast: TmnrSection;
    function GetByName(const vName: string): TmnrSection;
    function GetReport: TmnrCustomReport;
  protected
    procedure DoAppendSummary(vSummarySection: TmnrSection);
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
    function RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer = 0; vOnFetch: TOnFetch = nil): TmnrSection;
    property ByName[const vName: string]: TmnrSection read GetByName;
    function Find(const vName: string): TmnrSection;

    property Report: TmnrCustomReport read GetReport;
    property First: TmnrSection read GetFirst;
    property Last: TmnrSection read GetLast;

    procedure Loop;
  end;

  TmnrIndex = class(TObject)
  protected
    procedure Compute(vReport: TmnrCustomReport); virtual;
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
  end;

  TmnrRowsIndex = class(TmnrIndex)
  private
    FArray: TmnrRowArray;
    function GetItems(vIndex: Integer): TmnrRow;
    function GetCount: Integer;
  protected
    procedure Compute(vReport: TmnrCustomReport); override;
  public
    property Items[vIndex: Integer]: TmnrRow read GetItems; default;
    property Count: Integer read GetCount;
  end;

  TmnrParams = class(TmnFields)
  private
    FID: Int64;
    function GetField(Index: string): TmnField;
  public
    property ID: Int64 read FID write FID;
    property Field[Index: string]: TmnField read GetField; default;
  end;

  TmnrCustomReport = class(TObject)
  private
    FCanceled: Boolean;
    FItems: TmnrRows;
    FSections: TmnrSections;
    FRowsListIndex: TmnrRowsIndex;
    FDetailTotals: TmnrSection;
    FSummary: TmnrSection;
    FLayouts: TmnrLayouts;
    //FDesignCells: TmnrDesignCells;
    FProfiler: TmnrProfiler;
    FParams: TmnrParams;

    function GetCells(Row, Column: Integer): TmnrCell;
    function GetRows(Row: Integer): TmnrRow;
  protected
    function Canceled: Boolean;
    procedure HandleNewRow(vRow: TmnrRowNode); virtual;
    procedure CreateSections(vSections: TmnrSections); virtual;
    procedure CreateLayouts(vLayouts: TmnrLayouts); virtual;
    function CreateNewRow(vSection: TmnrSection): TmnrRow; virtual;
    function CreateProfiler: TmnrProfiler; virtual;
    procedure Loop;
    //Apply param to report to use it in Queries or assign it to Variables
    procedure SetParams(vParams: TmnrParams); virtual;
    //Collect params from current record in report to send it to another report
    procedure CollectParams(vParams: TmnrParams); virtual;
    function SumString: string; virtual;

    procedure Created; virtual; //after create
    procedure Start; virtual; //after build report only in generate
    procedure Finish; virtual; //
  public
    constructor Create(vParams: TmnrParams = nil); virtual; //(vParams: TMiscParams); note: report responsible for free params
    destructor Destroy; override;
    property Params: TmnrParams read FParams;
    property Sections: TmnrSections read FSections;
    property Layouts: TmnrLayouts read FLayouts;
    property Items: TmnrRows read FItems;
    procedure Load; virtual;
    procedure Cancel;
    function FindSection(const vName: string): TmnrSection;

    procedure Prepare; virtual; //for design and generate
    procedure Generate;
    procedure Design;
    property Profiler: TmnrProfiler read FProfiler;

    procedure Fetch(vSection: TmnrSection; var vParams: TmnrFetch); virtual;
    procedure RegisterRequest(const vName: string; vOnRequest: TOnRequest); virtual;

    property RowsIndex: TmnrRowsIndex read FRowsListIndex;
    property Rows[Row: Integer]: TmnrRow read GetRows;
    property Cells[Row, Column: Integer]: TmnrCell read GetCells;

    procedure ExportCSV(const vFile: TFileName); overload; //test purpose only
    procedure ExportCSV(const vStream: TStream); overload; //test purpose only
  end;

  TmnrReportClass = class of TmnrCustomReport;

  TCustomReportDesigner = class(TComponent)
  private
    FReport: TmnrCustomReport;
    FDesignerWindow: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DesignerWindow: TComponent read FDesignerWindow;
    procedure Created; virtual;
  public
    constructor AutoCreate(vClass: TmnrCustomReportClass); overload; virtual;
    destructor Destroy; override;

    procedure DesignReport; virtual;
    function CreateDesigner: TComponent; virtual;
    property Report: TmnrCustomReport read FReport;
  end;

  TmnrProfiler = class
  private
    FCells: TmnrDesignCells;
    FReport: TmnrCustomReport;
    function GetCells: TmnrDesignCells;
  protected
    function CreateCells: TmnrDesignCells; virtual;
  public
    constructor Create; virtual;
    procedure SaveReport; virtual;
    procedure LoadReport; virtual;
    property Cells: TmnrDesignCells read GetCells;
    property Report: TmnrCustomReport read FReport;
  end;


var
  DefaultCellClass: TmnrReportCellClass = nil; //test purpose

procedure SetReportDesignerClass(vClass: TCustomReportDesignerClass);
procedure DesignReport(vClass: TmnrCustomReportClass);

function NewParams: TmnrParams; overload;
//function NewParams(Strings: TStrings): TmnrParams; overload;
//function NewParams(vText: string): TmnrParams; overload;
function NewParams(Keys: array of string; Values: array of Variant): TmnrParams; overload;

implementation

uses
  mnrNodes;

var
  ReportDesignerClass: TCustomReportDesignerClass = TCustomReportDesigner;

procedure SetReportDesignerClass(vClass: TCustomReportDesignerClass);
begin
  ReportDesignerClass := vClass;
end;

procedure DesignReport(vClass: TmnrCustomReportClass);
begin
  ReportDesignerClass.AutoCreate(vClass);
end;

function NewParams: TmnrParams;
begin
  Result := TmnrParams.Create;
end;

{function NewParams(vText: string): TmnrParams; overload;
begin
  Result := NewParams;
  Result.Text := vText;
end;}

{function NewParams(Strings: TStrings): TmnrParams; overload;
begin
  Result := NewParams;
//  Result.Assign(Strings);
end;}

function NewParams(Keys: array of string; Values: array of Variant): TmnrParams;
var
  l, i: Integer;
begin
  Result := NewParams;
  l := Length(Keys);
  if l > Length(Values) then
    l := Length(Values);
  for i := 0 to l - 1 do
    Result.Field[Keys[i]].AsVariant := Values[i];
end;

{ TmnrCustomReport }

procedure TmnrCustomReport.Load;
begin
  Profiler.LoadReport;
end;

procedure TmnrCustomReport.Cancel;
begin
  FCanceled := True;
end;

function TmnrCustomReport.Canceled: Boolean;
begin
  Result := FCanceled;
end;

procedure TmnrCustomReport.CollectParams(vParams: TmnrParams);
begin
end;

constructor TmnrCustomReport.Create(vParams: TmnrParams);
begin
  inherited Create;
  FParams := vParams;
  if FParams = nil then
    FParams := TmnrParams.Create;

  FProfiler := CreateProfiler;
  FProfiler.FReport := Self;
  FSections := TmnrSections.Create(Self);
  FLayouts := TmnrLayouts.Create;
  FItems := TmnrRows.Create;
  FRowsListIndex := nil;
  FDetailTotals := TmnrSection.Create(nil);
  FSummary := TmnrSection.Create(nil);
  CreateSections(FSections);
  CreateLayouts(FLayouts);
  Created;
end;

procedure TmnrCustomReport.CreateLayouts(vLayouts: TmnrLayouts);
begin

end;

function TmnrCustomReport.CreateNewRow(vSection: TmnrSection): TmnrRow;
begin
  Result := TmnrRow.Create(Items);
end;

function TmnrCustomReport.CreateProfiler: TmnrProfiler;
begin
  Result := TmnrProfiler.Create;
end;

procedure TmnrCustomReport.CreateSections(vSections: TmnrSections);
begin
end;

procedure TmnrCustomReport.Design;
begin
  {Prepare;
  try
    DesignReport;
  finally //handle safe finish ........
    Finish;
  end;}
end;

destructor TmnrCustomReport.Destroy;
begin
  FDetailTotals.Free;
  FSummary.Free;
  FLayouts.Free;
  FSections.Free;
  FItems.Free;
  FRowsListIndex.Free;
  FreeAndNil(FProfiler);
  FreeAndNil(FParams);
  inherited;
end;

procedure TmnrCustomReport.ExportCSV(const vStream: TStream);
  procedure WriteStr(const vStr: string);
  begin
    vStream.Write(vStr[1], Length(vStr));
  end;
var
  r: TmnrRow;
  n: TmnrCell;
begin
  r := Items.First;
  while r <> nil do
  begin
    n := r.Cells.First;
    while n <> nil do
    begin
      WriteStr(n.AsString);
      n := n.Next;
      if n <> nil then
        WriteStr(';');
    end;

    r := r.Next;
    if r <> nil then
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

procedure TmnrCustomReport.Fetch(vSection: TmnrSection; var vParams: TmnrFetch);
begin
end;

function TmnrCustomReport.FindSection(const vName: string): TmnrSection;
begin
  Result := Sections.Find(vName);
end;

procedure TmnrCustomReport.Finish;
begin
  FRowsListIndex := TmnrRowsIndex.Create(Self);
end;

procedure TmnrCustomReport.SetParams(vParams: TmnrParams);
begin
end;

procedure TmnrCustomReport.Generate;
begin
  Prepare;
  try
    SetParams(Params);
    Load;
    Start;
    Loop;
  finally //handle safe finish ........
    Finish;
  end;
end;

function TmnrCustomReport.GetCells(Row, Column: Integer): TmnrCell;
var
  r: TmnrRow;
  i: Integer;
begin
  if RowsIndex <> nil then
  begin
    r := RowsIndex.Items[Row];
    if r <> nil then
    begin
      i := 0;
      Result := r.Cells.First;
      while (Result <> nil) and (i < Column) do
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

function TmnrCustomReport.GetRows(Row: Integer): TmnrRow;
begin
  if RowsIndex <> nil then
    Result := RowsIndex.Items[Row]
  else
    Result := nil;
end;

procedure TmnrCustomReport.HandleNewRow(vRow: TmnrRowNode);
begin
end;

procedure TmnrCustomReport.Created;
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

procedure TmnrCustomReport.RegisterRequest(const vName: string; vOnRequest: TOnRequest);
var
  l: TmnrLayout;
begin
  l := Layouts.Find(vName);
  if l <> nil then
    l.OnRequest := vOnRequest;
end;

procedure TmnrCustomReport.Start;
begin

end;

function TmnrCustomReport.SumString: string;
begin
  Result := '«·„Ã„Ê⁄';
end;

{ TmnrCustomReportRowNode }

function TmnrRow.CreateCells: TmnrRowCells;
begin
  Result := TmnrReportCells.Create;
end;

{ TmnrRows }

function TmnrRows.Add: TmnrRow;
begin
  Result := TmnrRow.Create(Self);
end;

function TmnrRows.GetFirst: TmnrRow;
begin
  Result := TmnrRow(inherited First);
end;

function TmnrRows.GetLast: TmnrRow;
begin
  Result := TmnrRow(inherited Last);
end;

{ TmnrSection }

procedure TmnrSection.DoAppendTotals(vTotalSection: TmnrSection);
var
  r: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  r := DesignRows.First;
  if r <> nil then
  begin
    f := True;
    while r <> nil do
    begin
      aRow := Report.CreateNewRow(vTotalSection);
      try
        d := r.Cells.First;
        while d <> nil do
        begin
          l := d.Layout;
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow.Cells);
            c.AsString := Report.SumString;
          end
          else
          begin
            c := TmnrCurrencyReportCell.Create(aRow.Cells);
            if (l <> nil) and (l.Reference <> nil) then
              c.AsCurrency := l.Reference.Total;
          end;
          c.FRow := aRow;
          c.FLayout := l;
          if l <> nil then c.FReference := l.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      with vTotalSection.Items.Add do
      begin
        FRow := aRow;
      end;

      r := r.Next;
    end;
  end;
end;

procedure TmnrSection.DoAppendSummary(vSummarySection: TmnrSection);
var
  r: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  r := DesignRows.First;
  if r <> nil then
  begin
    f := True;
    while r <> nil do
    begin
      aRow := Report.CreateNewRow(vSummarySection);
      try
        d := r.Cells.First;
        while d <> nil do
        begin
          l := d.Layout;
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow.Cells);
            c.AsString := Report.SumString;
          end
          else
          begin
            c := TmnrCurrencyReportCell.Create(aRow.Cells);
            if l <> nil then
              c.AsCurrency := l.Total;
          end;
          c.FRow := aRow;
          c.FLayout := l;
          if l <> nil then c.FReference := l.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      with vSummarySection.Items.Add do
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
  FDesignRows := TmnrDesignRows.Create(Self);
  FReferencesRows := TmnrReferencesRows.Create;
  FItems := TmnrRowReferences.Create;
end;

destructor TmnrSection.Destroy;
begin
  FSections.Free;
  FDesignRows.Free;
  FReferencesRows.Free;
  FItems.Free;
  inherited;
end;

function TmnrSection.DoFetch(var vParams: TmnrFetch): TmnrAcceptMode;
begin
  Result := acmAccept;
  if Assigned(FOnFetch) then
    FOnFetch(vParams)
  else
    Report.Fetch(Self, vParams);
end;

procedure TmnrSection.FillNow(vParams: TmnrFetch; vReference: TmnrReferencesRow);
var
  r: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  //c: TmnrCell;
begin
  r := DesignRows.First;
  if r <> nil then
  begin
    while r <> nil do
    begin
      aRow := Report.CreateNewRow(Self);
      try
        aRow.ID := vParams.ID;
        aRow.FNumber := vParams.Number;

        d := r.Cells.First;
        while d <> nil do
        begin
          l := d.Layout;
          //c := l.NewCell(aRow);
          if l <> nil then
            l.NewCell(aRow);
          d := d.Next;
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
  if Nodes <> nil then
    Result := Nodes.Report
  else
    Result := nil;
end;

function TmnrSection.NewReference: TmnrReferencesRow;
var
  r: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
begin
  Result := ReferencesRows.Add;
  r := DesignRows.First;
  if r <> nil then
  begin
    while r <> nil do
    begin
      d := r.Cells.First;
      while d <> nil do
      begin
        l := d.Layout;
        if l <> nil then
          l.FReference := Result.Cells.Add;
        d := d.Next;
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

procedure TmnrSections.DoAppendSummary(vSummarySection: TmnrSection);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    if s.AppendSummary then
      s.DoAppendSummary(vSummarySection);
    s.Sections.DoAppendSummary(vSummarySection);
    s := s.Next;
  end;
end;

function TmnrSections.Find(const vName: string): TmnrSection;
var
  p: TmnrSection;
begin
  Result := nil;
  p := First;
  while p <> nil do
  begin
    if SameText(p.Name, vName) then
      Result := p
    else
      Result := p.Sections.Find(vName);
    if Result <> nil then
      Break
    else
      p := p.Next;
  end;
end;

function TmnrSections.GetByName(const vName: string): TmnrSection;
var
  p: TmnrSection;
begin
  Result := nil;
  p := First;
  while p <> nil do
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
  fParams: TmnrFetch;
  r: TmnrReferencesRow;
begin
  s := First;
  while s <> nil do
  begin
    fParams.ID := 0;
    fParams.Number := 0;
    case s.LoopWay of
      slwSingle:
        begin
          fparams.AcceptMode := acmAccept;
          fparams.FetchMode := fmFirst;
          s.DoFetch(fparams);
          if fparams.AcceptMode = acmAccept then
          begin
            s.FillNow(fparams, nil);
            s.Sections.Loop;
          end;
        end;
      slwMulti:
        begin
          fparams.FetchMode := fmFirst;
          fparams.AcceptMode := acmAccept;
          r := nil;
          while not Report.Canceled and (fparams.AcceptMode = acmAccept) do
          begin
            s.DoFetch(fparams);
            if (s.ClassID = sciDetails) and (fparams.FetchMode = fmFirst) then //improve add referance on first accepted ...
              r := s.NewReference;

            if fparams.AcceptMode = acmAccept then
            begin
              s.FillNow(fparams, r);
              s.Sections.Loop;
            end
            else if (fparams.AcceptMode = acmSkip) and (s.ClassID = sciHeaderDetails) then
              s.Sections.Loop;

            if (fparams.AcceptMode = acmEof) and (s.Items.Count <> 0) then
            begin
              if (r <> nil) and s.AppendTotals then
              begin
                s.DoAppendTotals(Report.FDetailTotals);
              end;
            end;

            if fparams.FetchMode = fmFirst then
              fparams.FetchMode := fmNext;
          end;

        //Summary
          if (s.ClassID = sciHeaderDetails) then
          begin
            s.Sections.DoAppendSummary(Report.FSummary);
          end;

        end; //case slwMulti:
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
  if vID = 0 then
    Result.FID := Ord(vClass)
  else
    Result.FID := vID;
  //Result.FLoopWay := vLoopWay;
end;

function TmnrRow.GetCellByIndex(I: Integer): TmnrCell;
var
  c: Integer;
begin
  Result := Cells.First;
  c := 0;
  while (Result <> nil) and (c < i) do
  begin
    Result := Result.Next;
    Inc(c);
  end;
end;

function TmnrRow.GetCells: TmnrReportCells;
begin
  Result := TmnrReportCells(inherited Cells);
end;

function TmnrRow.GetNext: TmnrRow;
begin
  Result := TmnrRow(inherited GetNext);
end;

function TmnrRow.GetPrior: TmnrRow;
begin
  Result := TmnrRow(inherited GetPrior);
end;

procedure TmnrRow.SetReferencesRow(const Value: TmnrReferencesRow);
begin
  FReferencesRow := Value;
end;

{ TmnrLayout }

function TmnrLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := nil;
end;

procedure TmnrLayout.DoRequest(vCell: TmnrCell);
begin
  if Assigned(FOnRequest) then
    FOnRequest(vCell);
end;

function TmnrLayout.GetNext: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetNext);
end;

function TmnrLayout.GetPrior: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetPrior);
end;

function TmnrLayout.GetTotal: Double;
begin
  Result := 0;
end;

function TmnrLayout.NewCell(vRow: TmnrRow): TmnrCell;
begin
  Result := CreateCell(vRow.Cells);
  if Result <> nil then
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

procedure TmnrLayout.Request(vCell: TmnrCell);
begin
  DoRequest(vCell);
end;

procedure TmnrLayout.ScaleCell(vCell: TmnrCell);
begin

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
  Result := AsString = '';
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
  if DefaultCellClass <> nil then
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

{ TmnrCell }

function TmnrCell.GetNext: TmnrCell;
begin
  Result := TmnrCell(inherited GetNext);
end;

function TmnrCell.GetPrior: TmnrCell;
begin
  Result := TmnrCell(inherited GetPrior);
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

{ TmnrRowsIndex }

procedure TmnrRowsIndex.Compute(vReport: TmnrCustomReport);
var
  i: Integer;
  r: TmnrRow;
begin
  SetLength(FArray, vReport.Items.Count);
  i := 0;
  r := vReport.Items.First;
  while r <> nil do
  begin
    FArray[i] := r;
    Inc(i);
    r := r.Next;
  end;
end;

function TmnrRowsIndex.GetCount: Integer;
begin
  Result := Length(FArray); 
end;

function TmnrRowsIndex.GetItems(vIndex: Integer): TmnrRow;
begin
  if (vIndex >= 0) and (vIndex < Length(FArray)) then
    Result := FArray[vIndex]
  else
    Result := nil;
end;

{ TmnrIndex }

procedure TmnrIndex.Compute(vReport: TmnrCustomReport);
begin

end;

constructor TmnrIndex.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  Compute(vReport);
end;

destructor TmnrIndex.Destroy;
begin

  inherited;
end;

{ TCustomReportDesigner }

constructor TCustomReportDesigner.AutoCreate(vClass: TmnrCustomReportClass);
begin
  inherited Create(nil);
  FReport := vClass.Create;
  FReport.Prepare;
  FReport.Load;
  Created;
  DesignReport;
end;

function TCustomReportDesigner.CreateDesigner: TComponent;
begin
  Result := nil;
end;

procedure TCustomReportDesigner.DesignReport;
var
  c: TComponent;
begin
  c := CreateDesigner;
  if c <> nil then
  begin
    FDesignerWindow := c;
    c.FreeNotification(Self);
    c.Tag := Integer(Report);
    c.Name := Report.ClassName;
  end
end;

destructor TCustomReportDesigner.Destroy;
begin
  FReport.Finish;
  FreeAndNil(FReport);
  inherited;
end;

procedure TCustomReportDesigner.Created;
begin

end;

procedure TCustomReportDesigner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FDesignerWindow) then
    begin
      FDesignerWindow := nil;
      Free;
    end;
  end;
end;

{ TmnrLayouts }

function TmnrLayouts.Add: TmnrLayout;
begin
  Result := TmnrLayout.Create(Self);
end;

procedure TmnrLayouts.CreateLayout(vClass: TmnrLayoutClass; const vInfo: TmnrLayoutInfo);
begin
  with vClass.Create(Self) do
  begin
    Info := vInfo;
  end;
end;

function TmnrLayouts.Find(const vName: string): TmnrLayout;
begin
  if vName = '' then
    Result := nil
  else
  begin
    Result := First;
    while Result <> nil do
      if SameText(Result.Name, vName) then
        Break
      else
        Result := Result.Next;
  end;
end;

procedure TmnrLayouts.CreateLayout(vClass: TmnrLayoutClass; const vName: string; const vTitle: string; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []);
var
  aInfo: TmnrLayoutInfo;
begin
  aInfo.Name := vName;
  aInfo.Title := vTitle;
  aInfo.IncludeSections := vIncludeSections;
  aInfo.ExcludeSections := vExcludeSections;
  CreateLayout(vClass, aInfo);
end;

function TmnrLayouts.GetFirst: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetFirst);
end;

function TmnrLayouts.GetLast: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetLast);
end;

{ TmnrDesignCell }

constructor TmnrDesignCell.AutoCreate(vNodes: TmnrNodes; const vName: string; vWidth: Integer);
begin
  Create(vNodes);
  Name := vName;
  Width := vWidth;
end;

constructor TmnrDesignCell.Create(vNodes: TmnrNodes);
begin
  inherited;
  FWidth := 100;
end;

destructor TmnrDesignCell.Destroy;
begin

  inherited;
end;

function TmnrDesignCell.GetCells: TmnrDesignCells;
begin
  Result := TmnrDesignCells(Nodes);
end;

function TmnrDesignCell.GetNext: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetNext);
end;

function TmnrDesignCell.GetPrior: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetPrior);
end;

function TmnrDesignCell.GetReport: TmnrCustomReport;
begin
  if Cells <> nil then
    Result := Cells.Report
  else
    Result := nil;
end;

function TmnrDesignCell.GetSection: TmnrSection;
begin
  if Cells <> nil then
    Result := Cells.Section
  else
    Result := nil;
end;

procedure TmnrDesignCell.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    if FLayout <> nil then FLayout.FDesignerCell := nil;
    FLayout := Report.Layouts.Find(Value);
    if FLayout <> nil then FLayout.FDesignerCell := Self
  end;
end;

procedure TmnrDesignCell.SetWidth(const Value: Integer);
begin
  if Value < 0 then
    FWidth := 100
  else
    FWidth := Value;
end;

{ TmnrDesignCells }

function TmnrDesignCells.Add: TmnrDesignCell;
begin
  Result := TmnrDesignCell.Create(Self);
end;

function TmnrDesignCells.GetFirst: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetFirst);
end;

function TmnrDesignCells.GetLast: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetLast);
end;

function TmnrDesignCells.GetReport: TmnrCustomReport;
begin
  if Row <> nil then
    Result := Row.Report
  else
    Result := nil;
end;

function TmnrDesignCells.GetRow: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetRow);
end;

function TmnrDesignCells.GetSection: TmnrSection;
begin
  if Row <> nil then
    Result := Row.Section
  else
    Result := nil;
end;

{ TmnrDesignRow }

function TmnrDesignRow.CreateCells: TmnrRowCells;
begin
  Result := TmnrDesignCells.Create;
end;

function TmnrDesignRow.GetCells: TmnrDesignCells;
begin
  Result := TmnrDesignCells(inherited Cells);
end;

function TmnrDesignRow.GetDesignRows: TmnrDesignRows;
begin
  Result := TmnrDesignRows(Nodes);
end;

function TmnrDesignRow.GetNext: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetNext);
end;

function TmnrDesignRow.GetPrior: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetPrior);
end;

function TmnrDesignRow.GetReport: TmnrCustomReport;
begin
  Result := Section.Report;
end;

function TmnrDesignRow.GetSection: TmnrSection;
begin
  Result := DesignRows.Section;
end;

function TmnrDesignRow.SumWidth: Integer;
var
  c: TmnrDesignCell;
begin
  Result := 0;
  c := Cells.First;
  while c <> nil do
  begin
    Inc(Result, c.Width);
    c := c.Next;
  end;
end;

{ TmnrDesignRows }

function TmnrDesignRows.Add: TmnrDesignRow;
begin
  Result := TmnrDesignRow.Create(Self);
end;

constructor TmnrDesignRows.Create(vSection: TmnrSection);
begin
  inherited Create;
  FSection := vSection;
end;

function TmnrDesignRows.GetFirst: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetFirst);
end;

function TmnrDesignRows.GetLast: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetLast);
end;

{ TmnrProfiler }

constructor TmnrProfiler.Create;
begin
  inherited Create;
end;

function TmnrProfiler.CreateCells: TmnrDesignCells;
begin
  Result := TmnrDesignCells.Create;
end;

function TmnrProfiler.GetCells: TmnrDesignCells;
begin
  Result := FCells;
end;

procedure TmnrProfiler.LoadReport;
begin

end;

procedure TmnrProfiler.SaveReport;
begin

end;

{ TmnrParams }

function TmnrParams.GetField(Index: string): TmnField;
begin
  Result := Find(Index);
end;

initialization

finalization

end.

