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
  ID_SECTION_FOOTERDETAILS = ID_SECTION_BASE + 8;
  ID_SECTION_LAST = ID_SECTION_BASE + 9;
  DEFAULT_CELL_WIDTH = 1000;

type

  TmnrSection = class;
  TmnrSections = class;
  TmnrCustomReport = class;
  TmnrLayout = class;
  TmnrLayouts = class;
  TmnrCell = class;
  TmnrRow = class;
  TmnrReferencesRow = class;
  TmnrReference = class;
  TCustomReportDesigner = class;
  TmnrDesignCell = class;
  TmnrDesignRow = class;
  TmnrDesignRows = class;
  TmnrProfiler = class;
  TmnrGroups = class;

  TmnrCellClass = class of TmnrCell;
  TmnrLayoutClass = class of TmnrLayout;
  TmnrCustomReportClass = class of TmnrCustomReport;
  TCustomReportDesignerClass = class of TCustomReportDesigner;
  TmnrProfilerClass = class of TmnrProfiler;
  TmnrDesignCellClass = class of TmnrDesignCell;
  //TmnrReportClass = class of TmnrCustomReport;

  TmnrRowArray = array of TmnrRow;

  TmnrSectionLoopWay = (slwAuto, slwSingle, slwMulti);
  TmnrFetchMode = (fmFirst, fmNext);
  TmnrAcceptMode = (acmAccept, acmSkip, acmSkipAll, acmRepeat, acmEof);
  TmnrSectionClassID = (sciReport, sciHeaderReport, sciHeaderPage, sciHeaderDetails, sciDetails, sciFooterDetails, sciFooterPage, sciFooterReport);
  TmnrSectionClassIDs = set of TmnrSectionClassID;

  TmnrFetch = record
    FetchMode: TmnrFetchMode;
    AcceptMode: TmnrAcceptMode;
    ID: Integer;
    Number: Integer;
    Locked: Boolean;
    Data: TObject;
  end;

  TOnRequest = procedure(vCell: TmnrCell) of object;
  TOnFetch = procedure(var vParams: TmnrFetch) of object;

  TmnrBaseCell = class(TmnrValueNode)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetAsData: Integer; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetAsData(const Value: Integer); override;
  public
    function DisplayText: string; virtual;
    function GetImageIndex: Integer; virtual;
  end;

  TmnrCell = class(TmnrBaseCell)
  private
    FReference: TmnrReference;
    FDesignCell: TmnrDesignCell;
    function GetNext: TmnrCell;
    function GetPrior: TmnrCell;
    function GetRow: TmnrRow;
    function GetLayout: TmnrLayout;
  protected
    function GetIsNull: Boolean; virtual;
    function GetDesignCell: TmnrDesignCell;
  public
    property Layout: TmnrLayout read GetLayout;
    property DesignCell: TmnrDesignCell read GetDesignCell;
    property Row: TmnrRow read GetRow;
    property Next: TmnrCell read GetNext;
    property Prior: TmnrCell read GetPrior;
    property Reference: TmnrReference read FReference;
  end;

  TmnrRow = class(TmnrRowNode)
  private
    FReferencesRow: TmnrReferencesRow;
    FDesignRow: TmnrDesignRow;
    FSection: TmnrSection;

    FID: Int64;
    FNumber: Integer;
    FRowIndex: Integer;
    FLocked: Boolean;
  protected
    function GetFirst: TmnrCell;
    function GetLast: TmnrCell;
    function GetNext: TmnrRow;
    function GetPrior: TmnrRow;

    function GetDesignRow: TmnrDesignRow;
    function GetReferencesRow: TmnrReferencesRow;
    function GetSection: TmnrSection;
    function GetByIndex(vIndex: Integer): TmnrCell;
  public
    function GetCellByIndex(I: Integer): TmnrCell;
    property Next: TmnrRow read GetNext;
    property Prior: TmnrRow read GetPrior;
    property ReferencesRow: TmnrReferencesRow read GetReferencesRow;
    property DesignRow: TmnrDesignRow read GetDesignRow;
    property Section: TmnrSection read GetSection;

    function Add: TmnrCell;
    property First: TmnrCell read GetFirst;
    property Last: TmnrCell read GetLast;

    property ID: Int64 read FID;
    property Locked: Boolean read FLocked;
    property Number: Integer read FNumber;
    property RowIndex: Integer read FRowIndex;

    property ByIndex[vIndex: Integer]: TmnrCell read GetByIndex;
  end;

  TmnrRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrRow;
    function GetLast: TmnrRow;
    function GetByIndex(vIndex: Integer): TmnrRow;
  public
    function Add: TmnrRow;
    property First: TmnrRow read GetFirst;
    property Last: TmnrRow read GetLast;
    property ByIndex[vIndex: Integer]: TmnrRow read GetByIndex;
  end;

  TmnrLayout = class(TmnrBaseCell)
  private
    FOnRequest: TOnRequest;
    FReference: TmnrReference;
    FExcludeSections: TmnrSectionClassIDs;
    FIncludeSections: TmnrSectionClassIDs;
    FName: string;
    FNumber: Integer;
    FChain: string;
    //FDesignerCell: TmnrDesignCell;
    function GetReport: TmnrCustomReport;
  protected
    function GetNext: TmnrLayout;
    function GetPrior: TmnrLayout;
    procedure DoRequest(vCell: TmnrCell); virtual;
    function CreateCell(vRow: TmnrRow): TmnrCell; virtual;
    procedure ScaleCell(vCell: TmnrCell); virtual;
    function GetTotal: Double; virtual;
    function GetPageTotal: Double; virtual;
    function DoCreateDesignCell(vRow: TmnrDesignRow): TmnrDesignCell; virtual;

    function GetExcludeSections: TmnrSectionClassIDs; virtual;
    function GetIncludeSections: TmnrSectionClassIDs; virtual;
    function GetName: string; virtual;
    function GetTitle: string; virtual;
    function GetNumber: Integer; virtual;
    function GetLayouts: TmnrLayouts;
  public
    function DisplayText: string; override;
    property Next: TmnrLayout read GetNext;
    property Prior: TmnrLayout read GetPrior;

    property Name: string read GetName;
    property Number: Integer read GetNumber;
    property Title: string read GetTitle;
    property Chain: string read FChain write FChain;
    property IncludeSections: TmnrSectionClassIDs read GetIncludeSections;
    property ExcludeSections: TmnrSectionClassIDs read GetExcludeSections;

    procedure Request(vCell: TmnrCell);
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    function NewCell(vDesignCell: TmnrDesignCell; vRow: TmnrRow): TmnrCell;
    property Reference: TmnrReference read FReference;
    //property DesignerCell: TmnrDesignCell read FDesignerCell write FDesignerCell;
    property Total: Double read GetTotal;
    property PageTotal: Double read GetPageTotal;
    function CreateDesignCell(vRow: TmnrDesignRow): TmnrDesignCell;
    property Layouts: TmnrLayouts read GetLayouts;
    property Report: TmnrCustomReport read GetReport;
  end;

  TmnrLayouts = class(TmnrLinkNodes)
  private
    FName: string;
    function GetFirst: TmnrLayout;
    function GetLast: TmnrLayout;
    procedure SetName(const Value: string);
    function GetNext: TmnrLayouts;
    function GetPrior: TmnrLayouts;
    function GetGroups: TmnrGroups;
    function GetReport: TmnrCustomReport;
  public
    function Add: TmnrLayout;
    property First: TmnrLayout read GetFirst;
    property Last: TmnrLayout read GetLast;
    property Next: TmnrLayouts read GetNext;
    property Prior: TmnrLayouts read GetPrior;

    function CreateLayout(vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest = nil; vNumber: Integer = 0; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []): TmnrLayout;
    function Find(const vName: string): TmnrLayout;
    property Name: string read FName write SetName;
    property Groups: TmnrGroups read GetGroups;
    property Report: TmnrCustomReport read GetReport;
  end;

  TmnrGroups = class(TmnrLinkNodes)
  private
    FReport: TmnrCustomReport;
    function GetFirst: TmnrLayouts;
    function GetLast: TmnrLayouts;
  public
    constructor Create(vReport: TmnrCustomReport);
    function Add: TmnrLayouts;
    property First: TmnrLayouts read GetFirst;
    property Last: TmnrLayouts read GetLast;
    function Find(const vName: string): TmnrLayouts;
    function FindLayout(const vName: string): TmnrLayout;
    property Report: TmnrCustomReport read FReport;

    function CreateLayout(const vGroup: string; vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest = nil; vNumber: Integer = 0; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []): TmnrLayout; 
  end;

  TmnrDesignCell = class(TmnrLinkNode)
  private
    FWidth: Integer;
    FLayout: TmnrLayout;
    FName: string;
    FAppendTotals: Boolean;
  protected
    function GetNext: TmnrDesignCell;
    function GetPrior: TmnrDesignCell;
    function GetRow: TmnrDesignRow;
    function GetReport: TmnrCustomReport;
    function GetSection: TmnrSection;
    procedure SetName(const Value: string);
    procedure SetWidth(const Value: Integer);
    procedure SetLayout(const Value: TmnrLayout);
    function GetLayout: TmnrLayout;
  public
    constructor Create(vNodes: TmnrNodes);
    destructor Destroy; override;

    property Next: TmnrDesignCell read GetNext;
    property Prior: TmnrDesignCell read GetPrior;
    property Layout: TmnrLayout read GetLayout write SetLayout;
    property Row: TmnrDesignRow read GetRow;
    property Section: TmnrSection read GetSection;
    property Report: TmnrCustomReport read GetReport;
    function DisplayText: string; virtual;
  published
    property Name: string read FName write SetName;
    property Width: Integer read FWidth write SetWidth default DEFAULT_CELL_WIDTH;
    property AppendTotals: Boolean read FAppendTotals write FAppendTotals default False;
  end;

  TmnrDesignRow = class(TmnrRowNode)
  protected
    function GetNext: TmnrDesignRow;
    function GetPrior: TmnrDesignRow;
    function GetDesignRows: TmnrDesignRows;
    function GetSection: TmnrSection;
    function GetReport: TmnrCustomReport;
    function GetFirst: TmnrDesignCell;
    function GetLast: TmnrDesignCell;
    function GetByIndex(vIndex: Integer): TmnrDesignCell;
  public
    property Next: TmnrDesignRow read GetNext;
    property Prior: TmnrDesignRow read GetPrior;
    property DesignRows: TmnrDesignRows read GetDesignRows;
    property Section: TmnrSection read GetSection;
    property Report: TmnrCustomReport read GetReport;
    function SumWidth: Integer;

    function Add: TmnrDesignCell;
    property First: TmnrDesignCell read GetFirst;
    property Last: TmnrDesignCell read GetLast;
    property ByIndex[vIndex: Integer]: TmnrDesignCell read GetByIndex;
    function Find(const vName: string): TmnrDesignCell;
  end;

  TmnrDesignRows = class(TmnrRowNodes)
  private
    FSection: TmnrSection;
  protected
    function GetFirst: TmnrDesignRow;
    function GetLast: TmnrDesignRow;
    function GetByIndex(vIndex: Integer): TmnrDesignRow;
    function GetSection: TmnrSection;
  public
    constructor Create(vSection: TmnrSection);
    function Add: TmnrDesignRow;
    property First: TmnrDesignRow read GetFirst;
    property Last: TmnrDesignRow read GetLast;
    property Section: TmnrSection read GetSection;
    property ByIndex[vIndex: Integer]: TmnrDesignRow read GetByIndex;
    function Find(const vName: string): TmnrDesignCell;
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
    function GetNodes: TmnrReferencesRow;
    function GetPrior: TmnrReference;
    procedure SetNodes(const Value: TmnrReferencesRow);
  public
    property Next: TmnrReference read GetNext;
    property Prior: TmnrReference read GetPrior;
    property Nodes: TmnrReferencesRow read GetNodes write SetNodes;
    property Total: Double read FTotal write FTotal;
  end;

  TmnrReferencesRow = class(TmnrRowNode)
  private
    function GetNext: TmnrReferencesRow;
    function GetPrior: TmnrReferencesRow;
    function GetFirst: TmnrReference;
    function GetLast: TmnrReference;
  protected
  public
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;

    property Next: TmnrReferencesRow read GetNext;
    property Prior: TmnrReferencesRow read GetPrior;

    function Add: TmnrReference;
    property First: TmnrReference read GetFirst;
    property Last: TmnrReference read GetLast;
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
    FAppendDetailTotals: Boolean;
    FAppendReportTotals: Boolean;
    FAppendDetailTitles: Boolean;
    FAppendReportTitles: Boolean;
    FSectionLoopWay: TmnrSectionLoopWay;
    FAppendPageTotals: Boolean;
    function GetNext: TmnrSection;
    function GetNodes: TmnrSections;
    function GetPrior: TmnrSection;
    procedure SetNodes(const Value: TmnrSections);
    function GetLoopWay: TmnrSectionLoopWay;
  protected
    function DoFetch(var vParams: TmnrFetch): TmnrAcceptMode; virtual;
    procedure DoAppendDetailTotals(vSection: TmnrSection);
    procedure DoAppendPageTotals(vSection: TmnrSection);
    procedure DoAppendReportTotals(vSection: TmnrSection);
    procedure DoAppendTitles(vSection: TmnrSection);
    function DoCreateDesignRows: TmnrDesignRows; virtual;
    function GetDesignRows: TmnrDesignRows;
    function DoCreateSections: TmnrSections;
    function GetSections: TmnrSections;

    procedure UpdateRowData(vRow: TmnrRow; vData: TObject); virtual;
    function GetCaption: string; virtual;
    function GetReport: TmnrCustomReport;
  public
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;
    property Sections: TmnrSections read GetSections;
    property Items: TmnrRowReferences read FItems;

    property Next: TmnrSection read GetNext;
    property Prior: TmnrSection read GetPrior;
    property Nodes: TmnrSections read GetNodes write SetNodes;
    property Report: TmnrCustomReport read GetReport;

    //function AddLayout
    property DesignRows: TmnrDesignRows read GetDesignRows;
    property ReferencesRows: TmnrReferencesRows read FReferencesRows;
    function NewReference: TmnrReferencesRow;

    property Name: string read FName;
    property ID: integer read FID;
    property ClassID: TmnrSectionClassID read FClassID;
    property Caption: string read GetCaption;
    property SectionLoopWay: TmnrSectionLoopWay read FSectionLoopWay write FSectionLoopWay default slwAuto;
    property LoopWay: TmnrSectionLoopWay read GetLoopWay;
    property OnFetch: TOnFetch read FOnFetch write FOnFetch;

    procedure FillNow(vParams: TmnrFetch; vIndex: Integer; vReference: TmnrReferencesRow);
    function FindDesignCell(const vName: string): TmnrDesignCell;
  published
    property AppendDetailTotals: Boolean read FAppendDetailTotals write FAppendDetailTotals default False;
    property AppendPageTotals: Boolean read FAppendPageTotals write FAppendPageTotals default False;
    property AppendReportTotals: Boolean read FAppendReportTotals write FAppendReportTotals default False;
    property AppendDetailTitles: Boolean read FAppendDetailTitles write FAppendDetailTitles default False;
    property AppendReportTitles: Boolean read FAppendReportTitles write FAppendReportTitles default False;
  end;

  TmnrSections = class(TmnrLinkNodes)
  private
    FReport: TmnrCustomReport;
  protected
    function GetFirst: TmnrSection;
    function GetLast: TmnrSection;
    function GetByName(const vName: string): TmnrSection;
    function GetReport: TmnrCustomReport;

    procedure DoAppendReportTotals(vSection: TmnrSection);
    procedure DoAppendPageTotals(vSection: TmnrSection);
    procedure DoAppendReportTitles(vSection: TmnrSection);
    function DoCreateSection: TmnrSection; virtual;

  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
    function RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer = 0; vOnFetch: TOnFetch = nil; vLoopWay: TmnrSectionLoopWay = slwAuto): TmnrSection;
    property ByName[const vName: string]: TmnrSection read GetByName;
    function Find(const vName: string): TmnrSection;
    function FindDesignCell(const vName: string): TmnrDesignCell;

    property Report: TmnrCustomReport read GetReport;
    property First: TmnrSection read GetFirst;
    property Last: TmnrSection read GetLast;

    procedure Loop;
    procedure ClearItems;
    procedure ClearDesignItems;
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

  ImnrReportDesigner = interface
    procedure DesignReport(vClass: TmnrCustomReportClass);
    procedure UpdateView(vCell: TmnrDesignCell = nil);
    procedure ProcessDrop(vNode: TmnrLayout);
  end;

  TmnrCustomReport = class(TPersistent) //belal: must be tobject but {$m+) not working need fix 
  private
    FWorking: Boolean;
    FCanceled: Boolean;
    FItems: TmnrRows;
    FSections: TmnrSections;
    FGroups: TmnrGroups;
    FRowsListIndex: TmnrRowsIndex;
    FProfiler: TmnrProfiler;
    FDesigner: ImnrReportDesigner;

    FDetailTotals: TmnrSection;
    FReportTotals: TmnrSection;
    FDetailTitles: TmnrSection;
    FReportTitles: TmnrSection;
    FHeaderPage: TmnrSection;
    FFooterPage: TmnrSection;

    function GetProfiler: TmnrProfiler;
  protected
    function Canceled: Boolean;
    procedure HandleNewRow(vRow: TmnrRowNode); virtual;
    procedure InitSections(vSections: TmnrSections); virtual;
    procedure InitLayouts(vGroups: TmnrGroups); virtual;
    procedure InitRequests; virtual;
    function CreateNewRow(vSection: TmnrSection): TmnrRow;
    procedure Loop;
    //Apply param to report to use it in Queries or assign it to Variables
    //procedure SetParams(vParams: TmnrParams); virtual;
    //Collect params from current record in report to send it to another report
    function SumString: string; virtual;

    procedure Created; virtual; //after create
    procedure Start; virtual; //after build report only in generate
    procedure Finish; virtual; //
    procedure DoPrepare; virtual;

    function DoCreateNewRow(vSection: TmnrSection): TmnrRow; virtual;
    function DoCreateProfiler: TmnrProfiler; virtual;
    function DoCreateSections: TmnrSections; virtual;
    function DoCreateGroups: TmnrGroups; virtual;
    function DoCreateItems: TmnrRows; virtual;
    procedure DoReportLoaded; virtual;
    function GetSections: TmnrSections;
    function GetGroups: TmnrGroups;
    function GetItems: TmnrRows;

    function GetCells(vRow, vCol: Integer): TmnrCell;
    function GetRows(vRow: Integer): TmnrRow;
    function GetDetailTotals: TmnrSection;
    function GetReportTotals: TmnrSection;
    function GetDetailTitles: TmnrSection;
    function GetReportTitles: TmnrSection;
    function GetFooterPage: TmnrSection;
    function GetHeaderPage: TmnrSection;
  public
  
    constructor Create;
    destructor Destroy; override;
    property Sections: TmnrSections read GetSections;
    property Groups: TmnrGroups read GetGroups;
    property Items: TmnrRows read GetItems;
    procedure Load;
    procedure Cancel;
    property Working: Boolean read FWorking;
    function Finished: Boolean;
    function FindSection(const vName: string): TmnrSection;
    property Designer: ImnrReportDesigner read FDesigner write FDesigner;

    procedure Prepare; //for design and generate
    procedure Generate;
    property Profiler: TmnrProfiler read GetProfiler;

    procedure Fetch(vSection: TmnrSection; var vParams: TmnrFetch); virtual;
    procedure RegisterRequest(const vName: string; vOnRequest: TOnRequest); virtual;

    property RowsIndex: TmnrRowsIndex read FRowsListIndex;
    property Rows[vRow: Integer]: TmnrRow read GetRows;
    property Cells[vRow, vCol: Integer]: TmnrCell read GetCells;

    procedure ExportCSV(const vFile: TFileName); overload; //test purpose only
    procedure ExportCSV(const vStream: TStream); overload; //test purpose only
    class function CreateReportDesgin: ImnrReportDesigner; virtual;
    class procedure Desgin;
    procedure Clear; virtual;

    property DetailTotals: TmnrSection read GetDetailTotals;
    property ReportTotals: TmnrSection read GetReportTotals;
    property DetailTitles: TmnrSection read GetDetailTitles;
    property ReportTitles: TmnrSection read GetReportTitles;
    property HeaderPage: TmnrSection read GetHeaderPage;
    property FooterPage: TmnrSection read GetFooterPage;
  end;


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
    FReport: TmnrCustomReport;
  protected
    function GetReport: TmnrCustomReport;
    procedure DoEnumReports(vList: TStrings); virtual;
  public
    constructor Create; virtual;
    procedure SaveReport; virtual;
    procedure LoadReport; virtual;
    procedure DeleteReport(const vName: string); virtual;

    procedure EnumReports(vList: TStrings); overload;
    function EnumReports: TStrings; overload;
    property Report: TmnrCustomReport read GetReport;
  end;


var
  DefaultCellClass: TmnrCellClass = nil; //test purpose

implementation

uses
  mnrNodes;

{ TmnrCustomReport }

procedure TmnrCustomReport.Load;
begin
  Profiler.LoadReport;
  DoReportLoaded;
end;

procedure TmnrCustomReport.Cancel;
begin
  FCanceled := True;
end;

function TmnrCustomReport.Canceled: Boolean;
begin
  Result := FCanceled;
end;

procedure TmnrCustomReport.Clear;
begin
  Items.Clear;
  Sections.ClearItems;
end;

constructor TmnrCustomReport.Create;
begin
  inherited Create;

  FProfiler := DoCreateProfiler;
  FProfiler.FReport := Self;
  FSections := DoCreateSections;
  FGroups := DoCreateGroups;
  FItems := DoCreateItems;
  FRowsListIndex := nil;

  InitSections(FSections);
  FDetailTotals := FSections.RegisterSection('DetailTotals', '„Ã«„Ì⁄ «· ›’Ì·', sciFooterDetails);
  FReportTotals := FSections.RegisterSection('ReportTotals', '„Ã«„Ì⁄ «· ﬁ—Ì—', sciFooterReport);
  FDetailTitles := FSections.RegisterSection('DetailTitles', '⁄‰«ÊÌ‰ «· ›’Ì·', sciDetails, 0, nil, slwSingle);
  FReportTitles := FSections.RegisterSection('ReportTitles', '⁄‰«ÊÌ‰ «· ﬁ—Ì—', sciHeaderReport);
  FHeaderPage   := FSections.RegisterSection('HeaderPage', '—«” «·’›Õ…', sciHeaderPage, ID_SECTION_HEADERPage);
  FFooterPage   := FSections.RegisterSection('FooterPage', '«”›· «·’›Õ…', sciFooterPage, ID_SECTION_FooterPage);

  Created;
  FWorking := True;
end;

procedure TmnrCustomReport.InitLayouts(vGroups: TmnrGroups);
begin

end;

procedure TmnrCustomReport.InitRequests;
begin

end;

function TmnrCustomReport.CreateNewRow(vSection: TmnrSection): TmnrRow;
begin
  Result := DoCreateNewRow(vSection);
  Result.FSection := vSection;
end;

function TmnrCustomReport.DoCreateGroups: TmnrGroups;
begin
  Result := TmnrGroups.Create(Self);
end;

function TmnrCustomReport.DoCreateItems: TmnrRows;
begin
  Result := TmnrRows.Create;
end;

function TmnrCustomReport.DoCreateNewRow(vSection: TmnrSection): TmnrRow;
begin
  Result := Items.Add;
end;

function TmnrCustomReport.DoCreateProfiler: TmnrProfiler;
begin
  Result := TmnrProfiler.Create;
end;

class function TmnrCustomReport.CreateReportDesgin: ImnrReportDesigner;
begin
  Result := nil;
end;

procedure TmnrCustomReport.InitSections(vSections: TmnrSections);
begin
end;

class procedure TmnrCustomReport.Desgin;
var
  aDesigner: ImnrReportDesigner;
  aClass: TmnrCustomReportClass;
begin
  aClass := Self;
  if (aClass <> nil) then
  begin
    aDesigner := CreateReportDesgin;
    if aDesigner<>nil then
      aDesigner.DesignReport(aClass);
  end;
end;

destructor TmnrCustomReport.Destroy;
begin
  FGroups.Free;
  FSections.Free;
  FItems.Free;
  FRowsListIndex.Free;
  FreeAndNil(FProfiler);
  inherited;
end;

function TmnrCustomReport.DoCreateSections: TmnrSections;
begin
  Result := TmnrSections.Create(Self);
end;

procedure TmnrCustomReport.DoPrepare;
begin

end;

procedure TmnrCustomReport.DoReportLoaded;
begin

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
    n := r.First;
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

function TmnrCustomReport.Finished: Boolean;
begin
  Result := not Working;
end;

procedure TmnrCustomReport.Generate;
begin
  Prepare;
  FWorking := True;
  try
    //SetParams(Params);
    Load;
    Start;
    Loop;
  finally //handle safe finish ........
    FWorking := False;
    Finish;
  end;
end;

function TmnrCustomReport.GetCells(vRow, vCol: Integer): TmnrCell;
var
  r: TmnrRow;
begin
  r := Rows[vRow];
  if r <> nil then
    Result := r.ByIndex[vCol]
  else
    Result := nil;
end;

function TmnrCustomReport.GetDetailTotals: TmnrSection;
begin
  Result := FDetailTotals;
end;

function TmnrCustomReport.GetFooterPage: TmnrSection;
begin
  Result := FFooterPage;
end;

function TmnrCustomReport.GetReportTotals: TmnrSection;
begin
  Result := FReportTotals;
end;

function TmnrCustomReport.GetDetailTitles: TmnrSection;
begin
  Result := FDetailTitles;
end;

function TmnrCustomReport.GetReportTitles: TmnrSection;
begin
  Result := FReportTitles;
end;

function TmnrCustomReport.GetGroups: TmnrGroups;
begin
  Result := FGroups;
end;

function TmnrCustomReport.GetHeaderPage: TmnrSection;
begin
  Result := FHeaderPage;
end;

function TmnrCustomReport.GetItems: TmnrRows;
begin
  Result := FItems;
end;

function TmnrCustomReport.GetProfiler: TmnrProfiler;
begin
  Result := FProfiler;
end;

function TmnrCustomReport.GetRows(vRow: Integer): TmnrRow;
begin
  if RowsIndex <> nil then
    Result := RowsIndex.Items[vRow]
  else
    Result := Items.ByIndex[vRow];
end;

function TmnrCustomReport.GetSections: TmnrSections;
begin
  Result := FSections;
end;

procedure TmnrCustomReport.HandleNewRow(vRow: TmnrRowNode);
begin
end;

procedure TmnrCustomReport.Created;
begin

end;

procedure TmnrCustomReport.Loop;
begin
  InitRequests; //must bge after start 
  FCanceled := False;
  try
    Sections.DoAppendReportTitles(ReportTitles);
    Sections.Loop;
    Sections.DoAppendPageTotals(FooterPage);
  except
    //Clear;
    raise;
  end;
end;

procedure TmnrCustomReport.Prepare;
begin
  DoPrepare;
  InitLayouts(Groups);
end;

procedure TmnrCustomReport.RegisterRequest(const vName: string; vOnRequest: TOnRequest);
var
  l: TmnrLayout;
begin
  l := Groups.FindLayout(vName);
  if (l <> nil) {and not Assigned(l.OnRequest) belal: need check} then
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

function TmnrRow.Add: TmnrCell;
begin
  if DefaultCellClass <> nil then
    Result := DefaultCellClass.Create(Self)
  else
    Result := nil;
end;

{ TmnrRows }

function TmnrRows.Add: TmnrRow;
begin
  Result := TmnrRow.Create(Self);
end;

function TmnrRows.GetByIndex(vIndex: Integer): TmnrRow;
begin
  Result := TmnrRow(inherited GetByIndex(vIndex));
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

function TmnrSection.DoCreateDesignRows: TmnrDesignRows;
begin
  Result := TmnrDesignRows.Create(Self);
end;

function TmnrSection.DoCreateSections: TmnrSections;
begin
  if Report<>nil then
    Result := Report.DoCreateSections
  else
    Result := TmnrSections.Create(nil);
end;

procedure TmnrSection.DoAppendDetailTotals(vSection: TmnrSection);
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
      aRow := Report.CreateNewRow(vSection);
      aRow.FDesignRow := r;
      try
        d := r.First;
        while d <> nil do
        begin
          l := d.Layout;
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString;
          end
          else
          begin
            c := TmnrCurrencyReportCell.Create(aRow);
            //c := l.CreateCell(aRow);
            if d.AppendTotals and (l <> nil) and (l.Reference <> nil) then
              c.AsCurrency := l.Reference.Total;
          end;
          c.FDesignCell := d;
          if l <> nil then c.FReference := l.Reference;

          d := d.Next;
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

procedure TmnrSection.DoAppendPageTotals(vSection: TmnrSection);
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
      aRow := Report.CreateNewRow(vSection);
      aRow.FDesignRow := r;
      try
        d := r.First;
        while d <> nil do
        begin
          l := d.Layout;
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString;
          end
          else
          begin
            c := TmnrPageTotalCell.Create(aRow);
          end;
          c.FDesignCell := d;
          if l <> nil then c.FReference := l.Reference;

          d := d.Next;
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

procedure TmnrSection.DoAppendReportTotals(vSection: TmnrSection);
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
      aRow := Report.CreateNewRow(vSection);
      aRow.FDesignRow := r;
      try
        d := r.First;
        while d <> nil do
        begin
          l := d.Layout;
          if f then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString;
          end
          else
          begin
            c := TmnrCurrencyReportCell.Create(aRow);
            if l <> nil then
              c.AsCurrency := l.Total;
          end;
          c.FDesignCell := d;
          if l <> nil then c.FReference := l.Reference;

          d := d.Next;
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

procedure TmnrSection.DoAppendTitles(vSection: TmnrSection);
var
  r: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  c: TmnrCell;
begin
  r := DesignRows.First;
  if r <> nil then
  begin
    while r <> nil do
    begin
      aRow := Report.CreateNewRow(vSection);
      aRow.FDesignRow := r;
      try
        d := r.First;
        while d <> nil do
        begin
          l := d.Layout;
          c := TmnrTextReportCell.Create(aRow);
          if l<>nil then
          begin
            c.FDesignCell := d;
            if d.DisplayText<>'' then
              c.AsString := d.DisplayText
            else
              c.AsString := l.DisplayText;
              
            c.FReference := l.Reference;
          end;
          d := d.Next;
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

constructor TmnrSection.Create(vNodes: TmnrNode);
begin
  inherited;
  FSections := DoCreateSections;
  FDesignRows := DoCreateDesignRows;
  FReferencesRows := TmnrReferencesRows.Create;
  FItems := TmnrRowReferences.Create;
  FAppendDetailTotals := False;
  FAppendReportTotals := False;
  FAppendDetailTitles := False;
  FAppendReportTitles := False;
  FAppendPageTotals := False;
  FSectionLoopWay := slwAuto;
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
  if Assigned(FOnFetch) then
    FOnFetch(vParams)
  else
    Report.Fetch(Self, vParams);

  Result := vParams.AcceptMode;
end;

procedure TmnrSection.FillNow(vParams: TmnrFetch; vIndex: Integer; vReference: TmnrReferencesRow);
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
        aRow.FID := vParams.ID;
        aRow.FNumber := vParams.Number;
        aRow.FLocked := vParams.Locked;
        aRow.FRowIndex := vIndex;
        aRow.FReferencesRow := vReference;
        aRow.FDesignRow := r;
        if vParams.Data<>nil then UpdateRowData(aRow, vParams.Data);
        

        d := r.First;
        while d <> nil do
        begin
          l := d.Layout;
          //c := l.NewCell(aRow);
          if l <> nil then
            l.NewCell(d, aRow);
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

function TmnrSection.FindDesignCell(const vName: string): TmnrDesignCell;
begin
  Result := DesignRows.Find(vName)
end;

function TmnrSection.GetCaption: string;
begin
  Result := FCaption;
end;

function TmnrSection.GetDesignRows: TmnrDesignRows;
begin
  Result := FDesignRows;
end;

function TmnrSection.GetLoopWay: TmnrSectionLoopWay;
begin
  if SectionLoopWay=slwAuto then
  begin
    if ClassID in [sciHeaderDetails, sciDetails] then
      Result := slwMulti
    else
      Result := slwSingle;
  end
  else
    Result := FSectionLoopWay;
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

function TmnrSection.GetSections: TmnrSections;
begin
  Result := FSections;
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
      d := r.First;
      while d <> nil do
      begin
        l := d.Layout;
        if l <> nil then
          l.FReference := Result.Add;
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

procedure TmnrSection.UpdateRowData(vRow: TmnrRow; vData: TObject);
begin

end;

{ TmnrSections }

procedure TmnrSections.ClearItems;
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    s.Items.Clear;
    if s.Sections<>nil then s.Sections.ClearItems;

    s := s.Next;
  end;
end;

procedure TmnrSections.ClearDesignItems;
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    s.DesignRows.Clear;
    if s.Sections<>nil then s.Sections.ClearDesignItems;

    s := s.Next;
  end;
end;

constructor TmnrSections.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  FReport := vReport;
end;

destructor TmnrSections.Destroy;
begin

  inherited;
end;

procedure TmnrSections.DoAppendReportTotals(vSection: TmnrSection);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    if s.AppendReportTotals then
      s.DoAppendReportTotals(vSection);
    s.Sections.DoAppendReportTotals(vSection);
    s := s.Next;
  end;
end;

procedure TmnrSections.DoAppendPageTotals(vSection: TmnrSection);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    if s.AppendPageTotals then
      s.DoAppendPageTotals(vSection);
    s.Sections.DoAppendPageTotals(vSection);
    s := s.Next;
  end;
end;

procedure TmnrSections.DoAppendReportTitles(vSection: TmnrSection);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    if s.AppendReportTitles then
      s.DoAppendTitles(vSection);
    s.Sections.DoAppendReportTitles(vSection);
    s := s.Next;
  end;
end;

function TmnrSections.DoCreateSection: TmnrSection;
begin
  Result := TmnrSection.Create(Self);
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

function TmnrSections.FindDesignCell(const vName: string): TmnrDesignCell;
var
  s: TmnrSection;
begin
  Result := nil;
  s := First;
  while s <> nil do
  begin
    Result := s.DesignRows.Find(vName);
    if Result<>nil then
      Break
    else
    begin
      Result := s.Sections.FindDesignCell(vName);
      if Result <> nil then
        Break
    end;
    s := s.Next;
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
  aParams: TmnrFetch;
  r: TmnrReferencesRow;
  aIdx: Integer;
begin
  s := First;
  while s <> nil do
  begin
    aIdx := 0;
    aParams.ID := 0;
    aParams.Number := 0;
    aParams.Locked := False;
    aParams.Data := nil;
    aParams.AcceptMode := acmAccept;
    aParams.FetchMode := fmFirst;

    case s.LoopWay of
      slwSingle:
      begin
        s.DoFetch(aParams);
        if aParams.AcceptMode = acmAccept then
        begin
          s.FillNow(aParams, 0, s.NewReference);
          if aParams.Data <> nil then FreeAndNil(aParams.Data);
          s.Sections.Loop;
        end;
      end;
      slwMulti:
      begin
        r := nil;
        while not Report.Canceled and (aParams.AcceptMode <> acmEof) do
        begin
          s.DoFetch(aParams);

          if (aParams.FetchMode = fmFirst) then
          begin
            if (s.ClassID = sciDetails) then //improve add referance on first accepted ...
              r := s.NewReference;
            if (s.AppendDetailTitles) then
              s.DoAppendTitles(Report.DetailTitles);
          end;

          if aParams.AcceptMode = acmAccept then
          begin
            s.FillNow(aParams, aIdx, r);
            s.Sections.Loop;
          end
          else if (aParams.AcceptMode = acmSkip) and (s.ClassID = sciHeaderDetails) then
            s.Sections.Loop;

          if (aParams.AcceptMode = acmEof) and (s.Items.Count <> 0) then
          begin
            if (r <> nil) and s.AppendDetailTotals then
            begin
              s.DoAppendDetailTotals(Report.FDetailTotals);
            end;
          end;

          aParams.ID := 0;
          aParams.Number := 0;
          aParams.Locked := False;
          if aParams.FetchMode = fmFirst then aParams.FetchMode := fmNext;
          if aParams.Data <> nil then FreeAndNil(aParams.Data);

          if aParams.AcceptMode=acmAccept then
            Inc(aIdx)
          else if aParams.AcceptMode in [acmSkip, acmSkipAll] then
            aParams.AcceptMode := acmAccept;
        end;

      //Summary
        if (s.ClassID = sciHeaderDetails) then
        begin
          s.Sections.DoAppendReportTotals(Report.FReportTotals);
        end;

      end; //case slwMulti:
    end;
    s := s.Next;
  end;
end;

function TmnrSections.RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer; vOnFetch: TOnFetch; vLoopWay: TmnrSectionLoopWay): TmnrSection;
begin
  Result := DoCreateSection;
  Result.FName := vName;
  Result.FClassID := vClass;
  Result.OnFetch := vOnFetch;
  Result.FCaption := vCaption;
  Result.SectionLoopWay := vLoopWay;
  if vID = 0 then
    Result.FID := Ord(vClass)
  else
    Result.FID := vID;
  //Result.FLoopWay := vLoopWay;
end;

function TmnrRow.GetByIndex(vIndex: Integer): TmnrCell;
begin
  Result := TmnrCell(inherited GetByIndex(vIndex));
end;

function TmnrRow.GetCellByIndex(I: Integer): TmnrCell;
var
  c: Integer;
begin
  Result := First;
  c := 0;
  while (Result <> nil) and (c < i) do
  begin
    Result := Result.Next;
    Inc(c);
  end;
end;

function TmnrRow.GetDesignRow: TmnrDesignRow;
begin
  Result := FDesignRow;
end;

function TmnrRow.GetFirst: TmnrCell;
begin
  Result := TmnrCell(inherited GetFirst);
end;

function TmnrRow.GetLast: TmnrCell;
begin
  Result := TmnrCell(inherited GetLast);
end;

function TmnrRow.GetNext: TmnrRow;
begin
  Result := TmnrRow(inherited GetNext);
end;

function TmnrRow.GetPrior: TmnrRow;
begin
  Result := TmnrRow(inherited GetPrior);
end;

function TmnrRow.GetReferencesRow: TmnrReferencesRow;
begin
  Result := FReferencesRow;
end;

function TmnrRow.GetSection: TmnrSection;
begin
  Result := FSection;  
end;

{ TmnrLayout }

function TmnrLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := nil;
end;

function TmnrLayout.CreateDesignCell(vRow: TmnrDesignRow): TmnrDesignCell;
begin
  Result := DoCreateDesignCell(vRow);
  Result.Name := Name;
  Result.Layout := Self;  
end;

function TmnrLayout.DisplayText: string;
begin
  Result := Title;
end;

function TmnrLayout.DoCreateDesignCell(vRow: TmnrDesignRow): TmnrDesignCell;
begin
  Result := TmnrDesignCell.Create(vRow);
end;

procedure TmnrLayout.DoRequest(vCell: TmnrCell);
begin
  if Assigned(FOnRequest) then
    FOnRequest(vCell);
end;

function TmnrLayout.GetExcludeSections: TmnrSectionClassIDs;
begin
  Result := FExcludeSections;
end;

function TmnrLayout.GetIncludeSections: TmnrSectionClassIDs;
begin
  Result := FIncludeSections;
end;

function TmnrLayout.GetLayouts: TmnrLayouts;
begin
  Result := TmnrLayouts(Nodes);
end;

function TmnrLayout.GetName: string;
begin
  Result := FName;
end;

function TmnrLayout.GetNext: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetNext);
end;

function TmnrLayout.GetNumber: Integer;
begin
  Result := FNumber;
end;

function TmnrLayout.GetPageTotal: Double;
begin
  Result := 0;
end;

function TmnrLayout.GetPrior: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetPrior);
end;

function TmnrLayout.GetReport: TmnrCustomReport;
begin
  if Layouts<>nil then
    Result := Layouts.Report
  else
    Result := nil;
end;

function TmnrLayout.GetTitle: string;
begin
  Result := FName;
end;

function TmnrLayout.GetTotal: Double;
begin
  Result := 0;
end;

function TmnrLayout.NewCell(vDesignCell: TmnrDesignCell; vRow: TmnrRow): TmnrCell;
begin
  Result := CreateCell(vRow);
  if Result <> nil then
  begin
    try
      Result.FReference := Reference;
      Result.FDesignCell := vDesignCell;
      DoRequest(Result);
      if not vRow.Locked then ScaleCell(Result);
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

{ TmnrCell }

function TmnrCell.GetDesignCell: TmnrDesignCell;
begin
  Result := FDesignCell;
end;

function TmnrCell.GetIsNull: Boolean;
begin
  Result := AsString = '';
end;

function TmnrCell.GetLayout: TmnrLayout;
begin
  if DesignCell<>nil then
    Result := DesignCell.Layout
  else
    Result := nil;
end;

function TmnrCell.GetNext: TmnrCell;
begin
  Result := TmnrCell(inherited GetNext);
end;

function TmnrCell.GetPrior: TmnrCell;
begin
  Result := TmnrCell(inherited GetPrior);
end;

function TmnrCell.GetRow: TmnrRow;
begin
  Result := Nodes as TmnrRow;
end;

{ TmnrReferencesRow }

function TmnrReferencesRow.Add: TmnrReference;
begin
  Result := TmnrReference.Create(Self);
end;

constructor TmnrReferencesRow.Create(vNodes: TmnrNode);
begin
  inherited;
end;

destructor TmnrReferencesRow.Destroy;
begin
  inherited;
end;

function TmnrReferencesRow.GetFirst: TmnrReference;
begin
  Result := TmnrReference(inherited First);
end;

function TmnrReferencesRow.GetLast: TmnrReference;
begin
  Result := TmnrReference(inherited First);
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

function TmnrReference.GetNodes: TmnrReferencesRow;
begin
  Result := TmnrReferencesRow(inherited GetNodes);
end;

function TmnrReference.GetPrior: TmnrReference;
begin
  Result := TmnrReference(inherited GetPrior);
end;

procedure TmnrReference.SetNodes(const Value: TmnrReferencesRow);
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

function TmnrLayouts.CreateLayout(vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest; vNumber: Integer; vIncludeSections: TmnrSectionClassIDs; vExcludeSections: TmnrSectionClassIDs): TmnrLayout;
begin
  Result := vClass.Create(Self);
  with Result do
  begin
    FName := vName;
    FNumber := vNumber;
    FIncludeSections := vIncludeSections;
    FExcludeSections := vExcludeSections;
    FOnRequest := vOnRequest;
    FOnRequest := vOnRequest;
  end;
end;

function TmnrLayouts.GetFirst: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetFirst);
end;

function TmnrLayouts.GetGroups: TmnrGroups;
begin
  Result := TmnrGroups(Nodes);
end;

function TmnrLayouts.GetLast: TmnrLayout;
begin
  Result := TmnrLayout(inherited GetLast);
end;

function TmnrLayouts.GetNext: TmnrLayouts;
begin
  Result := TmnrLayouts(inherited GetNext);
end;

function TmnrLayouts.GetPrior: TmnrLayouts;
begin
  Result := TmnrLayouts(inherited GetPrior);
end;

function TmnrLayouts.GetReport: TmnrCustomReport;
begin
  if Groups<>nil then
    Result := Groups.Report
  else
    Result := nil;
end;

procedure TmnrLayouts.SetName(const Value: string);
begin
  FName := Value;
end;

{ TmnrDesignCell }

constructor TmnrDesignCell.Create(vNodes: TmnrNodes);
begin
  inherited Create(vNodes);
  FWidth := DEFAULT_CELL_WIDTH;
  FAppendTotals := False;
end;

destructor TmnrDesignCell.Destroy;
begin

  inherited;
end;

function TmnrDesignCell.DisplayText: string;
begin
  Result := '';
end;

function TmnrDesignCell.GetRow: TmnrDesignRow;
begin
  Result := Nodes as TmnrDesignRow;
end;

function TmnrDesignCell.GetLayout: TmnrLayout;
begin
  Result := FLayout;
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
  if Row <> nil then
    Result := Row.Report
  else
    Result := nil;
end;

function TmnrDesignCell.GetSection: TmnrSection;
begin
  if Row <> nil then
    Result := Row.Section
  else
    Result := nil;
end;

procedure TmnrDesignCell.SetLayout(const Value: TmnrLayout);
begin
  FLayout := Value;
end;

procedure TmnrDesignCell.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    //if Layout <> nil then Layout.FDesignerCell := nil;
    if Report<>nil then Layout := Report.Groups.FindLayout(Value);
    //if Layout <> nil then Layout.FDesignerCell := Self
  end;
end;

procedure TmnrDesignCell.SetWidth(const Value: Integer);
begin
  if Value < 0 then
    FWidth := DEFAULT_CELL_WIDTH
  else
    FWidth := Value;
end;

{ TmnrDesignRow }

function TmnrDesignRow.Add: TmnrDesignCell;
begin
  Result := TmnrDesignCell.Create(Self);
end;

function TmnrDesignRow.Find(const vName: string): TmnrDesignCell;
begin
  Result := First;
  while Result<>nil do
  begin
    if SameText(Result.Name, vName) then
      Break
    else
      Result := Result.Next;
  end;
end;

function TmnrDesignRow.GetByIndex(vIndex: Integer): TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetByIndex(vIndex));
end;

function TmnrDesignRow.GetDesignRows: TmnrDesignRows;
begin
  Result := TmnrDesignRows(Nodes);
end;

function TmnrDesignRow.GetFirst: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetFirst);
end;

function TmnrDesignRow.GetLast: TmnrDesignCell;
begin
  Result := TmnrDesignCell(inherited GetLast);
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
  c := First;
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

function TmnrDesignRows.Find(const vName: string): TmnrDesignCell;
var
  aRow: TmnrDesignRow;
begin
  aRow := First;
  Result := nil;
  while aRow<>nil do
  begin
    Result := aRow.Find(vName);
    if Result<>nil then
      Break
    else
      aRow := aRow.Next;
  end;
end;

function TmnrDesignRows.GetByIndex(vIndex: Integer): TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetByIndex(vIndex));
end;

function TmnrDesignRows.GetFirst: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetFirst);
end;

function TmnrDesignRows.GetLast: TmnrDesignRow;
begin
  Result := TmnrDesignRow(inherited GetLast);
end;

function TmnrDesignRows.GetSection: TmnrSection;
begin
  Result := FSection;
end;

{ TmnrProfiler }

constructor TmnrProfiler.Create;
begin
  inherited Create;
end;

procedure TmnrProfiler.EnumReports(vList: TStrings);
begin
  vList.Clear;
  DoEnumReports(vList);
end;

procedure TmnrProfiler.DeleteReport(const vName: string);
begin

end;

procedure TmnrProfiler.DoEnumReports(vList: TStrings);
begin
end;

function TmnrProfiler.EnumReports: TStrings;
begin
  Result := TStringList.Create;
  try
    EnumReports(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TmnrProfiler.GetReport: TmnrCustomReport;
begin
  Result := FReport;
end;

procedure TmnrProfiler.LoadReport;
begin

end;

procedure TmnrProfiler.SaveReport;
begin

end;

{ TmnrBaseCell }

function TmnrBaseCell.DisplayText: string;
begin
  Result := AsString;
end;

function TmnrBaseCell.GetAsBoolean: Boolean;
begin
  Result := False;
end;

function TmnrBaseCell.GetAsCurrency: Currency;
begin
  Result := 0;
end;

function TmnrBaseCell.GetAsData: Integer;
begin
  Result := AsInteger;
end;

function TmnrBaseCell.GetAsDateTime: TDateTime;
begin
  Result := AsCurrency;
end;

function TmnrBaseCell.GetAsFloat: Double;
begin
  Result := AsCurrency;
end;

function TmnrBaseCell.GetAsInteger: Longint;
begin
  Result := 0;
end;

function TmnrBaseCell.GetAsString: string;
begin
  Result := '';
end;

function TmnrBaseCell.GetAsVariant: Variant;
begin
  Result := '';
end;

function TmnrBaseCell.GetImageIndex: Integer;
begin
  Result := -1;
end;

procedure TmnrBaseCell.SetAsBoolean(const Value: Boolean);
begin

end;

procedure TmnrBaseCell.SetAsCurrency(const Value: Currency);
begin

end;

procedure TmnrBaseCell.SetAsData(const Value: Integer);
begin
  AsInteger := Value;
end;

procedure TmnrBaseCell.SetAsDateTime(const Value: TDateTime);
begin

end;

procedure TmnrBaseCell.SetAsFloat(const Value: Double);
begin

end;

procedure TmnrBaseCell.SetAsInteger(const Value: Integer);
begin

end;

procedure TmnrBaseCell.SetAsString(const Value: string);
begin

end;

procedure TmnrBaseCell.SetAsVariant(const Value: Variant);
begin

end;

{ TmnrGroups }

function TmnrGroups.Add: TmnrLayouts;
begin
  Result := TmnrLayouts.Create(Self);
end;

constructor TmnrGroups.Create(vReport: TmnrCustomReport);
begin
  inherited Create(nil);
  FReport := vReport;
end;

function TmnrGroups.CreateLayout(const vGroup: string; vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest; vNumber: Integer; vIncludeSections, vExcludeSections: TmnrSectionClassIDs): TmnrLayout;
var
  aLayouts: TmnrLayouts;
begin
  aLayouts := Find(vGroup);
  if aLayouts=nil then
  begin
    aLayouts := Add;
    aLayouts.Name := vGroup;
  end;
  Result := aLayouts.CreateLayout(vClass, vName, vOnRequest, vNumber, vIncludeSections, vExcludeSections);
end;

function TmnrGroups.Find(const vName: string): TmnrLayouts;
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

function TmnrGroups.FindLayout(const vName: string): TmnrLayout;
var
  l: TmnrLayouts;
begin
  Result := nil;
  l := First;
  while l<>nil do
  begin
    Result := l.Find(vName);
    if Result<>nil then
      Break
    else
      l := l.Next;
  end;
end;

function TmnrGroups.GetFirst: TmnrLayouts;
begin
  Result := TmnrLayouts(inherited GetFirst);
end;

function TmnrGroups.GetLast: TmnrLayouts;
begin
  Result := TmnrLayouts(inherited GetLast);
end;

initialization

finalization

end.

