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
  SysUtils, Classes, Json, Generics.Collections, Types,
  mnrLists, mnUtils, mnClasses;

const
  ID_SECTION_BASE          = 0;
  ID_SECTION_REPORT        = ID_SECTION_BASE + 1;
  ID_SECTION_HEADERREPORT  = ID_SECTION_BASE + 2;
  ID_SECTION_FOOTERREPORT  = ID_SECTION_BASE + 3;
  ID_SECTION_HEADERPAGE    = ID_SECTION_BASE + 4;
  ID_SECTION_FOOTERPAGE    = ID_SECTION_BASE + 5;
  ID_SECTION_HEADERDETAILS = ID_SECTION_BASE + 6;
  ID_SECTION_DETAILS       = ID_SECTION_BASE + 7;
  ID_SECTION_FOOTERDETAILS = ID_SECTION_BASE + 8;
  ID_SECTION_LAST          = ID_SECTION_BASE + 9;
  DEFAULT_CELL_WIDTH       = 1000;

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
  TmnrDesignCell = class;
  TmnrDesignRow = class;
  TmnrDesignRows = class;
  TmnrProfiler = class;
  TmnrGroups = class;

  TmnrCellClass = class of TmnrCell;
  TmnrLayoutClass = class of TmnrLayout;
  TmnrCustomReportClass = class of TmnrCustomReport;
  TmnrProfilerClass = class of TmnrProfiler;
  TmnrDesignCellClass = class of TmnrDesignCell;
  //TmnrReportClass = class of TmnrCustomReport;

  TmnrRowArray = array of TmnrRow;

  TmnrSumStringIndex = (ssiTotal, ssiPageTotal, ssiToPageTotal);
  TmnrSectionLoopWay = (slCustom, slwAuto, slwSingle, slwMulti);
  TmnrFetchMode = (fmFirst, fmNext);
  TmnrAcceptMode = (acmAccept, acmSkip, acmSkipAll, acmRepeat, acmEof);
  TmnrSectionClassID = (sciReport, sciHeaderReport, sciHeaderPage, sciHeaderTitles, sciHeaderDetails, sciDetailTitles, sciDetails, sciFooterDetails, sciFooterPage, sciFooterReport);
  TmnrSectionClassIDs = set of TmnrSectionClassID;

  TmnrFetch = record
    FetchMode: TmnrFetchMode;
    AcceptMode: TmnrAcceptMode;
    ID: Int64;
    Locked: Boolean;
    Data: TJSONObject;
    procedure Step(var vPos: Int64); overload;
    procedure Step(var vPos: Integer); overload;
    function Step(var vPos: Int64; vMax: Int64): Boolean; overload;
    function Step(var vPos: Integer; vMax: Integer): Boolean; overload;

    procedure Reset; //call in new fetch
    procedure Clear; //call in start loop
  end;

  TOnRequest = procedure(vCell: TmnrCell) of object;
  TOnFetch = procedure(var vParams: TmnrFetch) of object;

  TmnrBaseCell = class(TmnrValueNode)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetAsData: Integer; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetAsData(const Value: Integer); override;
  public
    function DisplayText: string; virtual;
    function GetImageIndex: Integer; virtual;
  end;

  TmnrCellAction = TProc<TmnrCell>;

  TmnrCell = class(TmnrBaseCell)
  private
    FReference: TmnrReference;
    FDesignCell: TmnrDesignCell;
    FLayout: TmnrLayout;
    FIsNull: Boolean;
    FAction: TmnrCellAction;

    function GetNext: TmnrCell;
    function GetPrior: TmnrCell;
    function GetRow: TmnrRow;
    function GetLayout: TmnrLayout;
  protected
    function GetIsNull: Boolean; virtual;
    function GetDesignCell: TmnrDesignCell;
    function DoGetDisplayText: string; virtual;
    function DoCompare(vCell: TmnrCell): Integer; virtual;
    procedure DoSumCell(vCell: TmnrCell); virtual;
    procedure Created; override;
  public
    function DisplayText: string; override;
    function ExecuteAction: Boolean;
    function Compare(vCell: TmnrCell): Integer;
    procedure SumCell(vCell: TmnrCell);
    property Layout: TmnrLayout read GetLayout;
    property DesignCell: TmnrDesignCell read GetDesignCell;
    procedure Scale;
    property Row: TmnrRow read GetRow;
    property Next: TmnrCell read GetNext;
    property Prior: TmnrCell read GetPrior;
    property Reference: TmnrReference read FReference;
    property IsNull: Boolean read FIsNull write FIsNull default False;
    property Action: TmnrCellAction read FAction write FAction;
  end;

  TmnrRow = class(TmnrRowNode)
  private
    FReferencesRow: TmnrReferencesRow;
    FDesignRow: TmnrDesignRow;
    FSection: TmnrSection;

    Flocked: Boolean;
    FJData: TJsonObject;
    function GetID: Int64;
    function GetLocked: Boolean;
    function GetRowIndex: Integer;
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
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;

    function GetCellByIndex(I: Integer): TmnrCell;
    function FindCell(vName: string): TmnrCell;
    procedure ScaleCells;
    procedure DescaleCells;
    property Next: TmnrRow read GetNext;
    property Prior: TmnrRow read GetPrior;
    property ReferencesRow: TmnrReferencesRow read GetReferencesRow;
    property DesignRow: TmnrDesignRow read GetDesignRow;
    property Section: TmnrSection read GetSection;

    function Add: TmnrCell;
    property First: TmnrCell read GetFirst;
    property Last: TmnrCell read GetLast;

    property ID: Int64 read GetID;
    property Locked: Boolean read FLocked;
    property RowIndex: Integer read GetRowIndex;
    property JData: TJsonObject read FJData;

    property ByIndex[vIndex: Integer]: TmnrCell read GetByIndex;
  end;

  TmnrRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrRow;
    function GetLast: TmnrRow;
    function GetByIndex(vIndex: Integer): TmnrRow;
    procedure SetFirst(const Value: TmnrRow);
    procedure SetLast(const Value: TmnrRow);
  public
    function Add: TmnrRow;
    property First: TmnrRow read GetFirst write SetFirst;
    property Last: TmnrRow read GetLast write SetLast;
    property ByIndex[vIndex: Integer]: TmnrRow read GetByIndex;
  end;

  TmnrLayout = class(TmnrBaseCell)
  private
    FOnRequest: TOnRequest;
    //FReference: TmnrReference;
    FExcludeSections: TmnrSectionClassIDs;
    FIncludeSections: TmnrSectionClassIDs;
    FName: string;
    FNumber: Integer;
    FTag: Integer;
    FChain: string;
    FOnDataRequest: TOnRequest;
    FData: Integer;
    FTitle: string;
    FFormat: string;
    FGuid: string;
    //FDesignerCell: TmnrDesignCell;
    function GetReport: TmnrCustomReport;
    function GetTag: Integer;
  protected
    function GetNext: TmnrLayout;
    function GetPrior: TmnrLayout;
    procedure DoRequest(vCell: TmnrCell); virtual;
    function CreateCell(vRow: TmnrRow): TmnrCell; virtual;
    procedure ScaleCell(vCell: TmnrCell; Invert: Boolean); virtual;
    procedure InitDesignCell(vDesignCell: TmnrDesignCell); virtual;
    function DoCreateDesignCell(vRow: TmnrDesignRow): TmnrDesignCell; virtual;
    procedure DoCellsExchanged(vCell1, vCell2: TmnrCell); virtual;

    function GetExcludeSections: TmnrSectionClassIDs; virtual;
    function GetIncludeSections: TmnrSectionClassIDs; virtual;
    function GetName: string; virtual;
    function GetNumber: Integer; virtual;
    function GetLayouts: TmnrLayouts;

    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetAsData: Integer; override;
  public
    function DisplayText: string; override;
    property Next: TmnrLayout read GetNext;
    property Prior: TmnrLayout read GetPrior;

    property Name: string read GetName;
    property Number: Integer read GetNumber;
    property Tag: Integer read GetTag;
    property Title: string read FTitle write FTitle;
    property Format: string read FFormat write FFormat;
    property Data: Integer read FData write FData;
    property Guid: string read FGuid write FGuid;
    property Chain: string read FChain write FChain;
    property IncludeSections: TmnrSectionClassIDs read GetIncludeSections;
    property ExcludeSections: TmnrSectionClassIDs read GetExcludeSections;
    procedure CellsExchanged(vCell1, vCell2: TmnrCell);

    procedure Request(vCell: TmnrCell);
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnDataRequest: TOnRequest read FOnDataRequest write FOnDataRequest;
    function NewCell(vDesignCell: TmnrDesignCell; vRow: TmnrRow): TmnrCell;
    //property DesignerCell: TmnrDesignCell read FDesignerCell write FDesignerCell;
    //property Total: Double read GetTotal;
    //property PageTotal: Double read GetPageTotal;
    function CreateDesignCell(vRow: TmnrDesignRow; InitIt: Boolean = False): TmnrDesignCell;
    property Layouts: TmnrLayouts read GetLayouts;
    property Report: TmnrCustomReport read GetReport;
  end;

  TmnrLayoutList = class(TmnObjectList<TmnrLayout>)
  public
    constructor Create;
    function FindLayout(vNumber, vData: Integer): TmnrLayout; overload;
    function FindLayout(vNumber: Integer; const vGuid: string): TmnrLayout; overload;
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

    function CreateLayout(vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest = nil; vNumber: Integer = 0; vTag: Integer = 0; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []): TmnrLayout;
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
  protected
    procedure DoInitLayouts; virtual;

  public
    constructor Create;
    function Add: TmnrLayouts;
    property First: TmnrLayouts read GetFirst;
    property Last: TmnrLayouts read GetLast;
    function Find(const vName: string): TmnrLayouts;
    function FindLayout(const vName: string): TmnrLayout;
    property Report: TmnrCustomReport read FReport;
    procedure InitLayouts;
    function EnumLayouts: TmnrLayoutList;

    function CreateLayout(const vGroup: string; vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest = nil; const vTitle: string = ''; vNumber: Integer = 0; vTag: Integer = 0; vIncludeSections: TmnrSectionClassIDs = []; vExcludeSections: TmnrSectionClassIDs = []): TmnrLayout;
    procedure CreateRequest(const vName: string; vOnRequest: TOnRequest);
  end;

  TmnrDesignCell = class(TmnrLinkNode)
  private
    FWidth: Integer;
    FLayout: TmnrLayout;
    FName: string;
    FAppendTotals: Boolean;
    FNumber: Integer;
    FMinValue: Double;
    FSelectedTotal: Double;
    FPageTotal: Double;
    FSubTotal: Double;
    FToPageTotal: Double;
    FTotal: Double;
    FMaxValue: Double;
    FLockCount: Integer;
    FCount: Integer;
    FReference: TmnrReference;
    FHidden: Boolean;
    FAlias: string;
    //FExplodeID: Integer;
    function GetWidth: Integer;
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
    procedure AssignTo(Dest: TPersistent); override;

    procedure DoUpdateCellDisplayText(vCell: TmnrCell; var vText: string); virtual;

    function CurrencyCellClass: TmnrCellClass; virtual;
    procedure DoHasName(const vName: string; var vAccept: Boolean); virtual;
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
    procedure UpdateCellDisplayText(vCell: TmnrCell; var vText: string);

    property Total: Double read FTotal write FTotal;
    property SubTotal: Double read FSubTotal write FSubTotal;
    property PageTotal: Double read FPageTotal write FPageTotal;
    property ToPageTotal: Double read FToPageTotal write FToPageTotal;
    property SelectedTotal: Double read FSelectedTotal write FSelectedTotal;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Count: Integer read FCount write FCount;
    property Reference: TmnrReference read FReference;

    procedure Lock;
    procedure UnLock;
    function Locked: Boolean;
    procedure ScaleCell(vCell: TmnrCell); virtual;
    procedure DescaleCell(vCell: TmnrCell); virtual;
    property Hidden: Boolean read FHidden write FHidden;
    function AliasName: string;
    function HasName(const vName: string): Boolean;
    //property ExplodeID: Integer read FExplodeID write FExplodeID;
    property Number: Integer read FNumber write FNumber default 0; //used in exploded cells
  published
    property Name: string read FName write SetName;
    property Alias: string read FAlias write FAlias;
    property Width: Integer read GetWidth write SetWidth default DEFAULT_CELL_WIDTH;
    property AppendTotals: Boolean read FAppendTotals write FAppendTotals {default False};
  end;

  TmnrDesignRow = class(TmnrRowNode)
  private
    FRowReport: TmnrCustomReport;
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
    property RowReport: TmnrCustomReport read FRowReport write FRowReport; //hack for text formula :(
    property Report: TmnrCustomReport read GetReport;
    function SumWidth: Integer;

    function Add: TmnrDesignCell;
    property First: TmnrDesignCell read GetFirst;
    property Last: TmnrDesignCell read GetLast;
    property ByIndex[vIndex: Integer]: TmnrDesignCell read GetByIndex;
    function FindName(const vName: string): TmnrDesignCell;
    procedure EnumByName(List: TList; const vName: string);
    procedure EnumLayouts(List: TmnrLayoutList);
    procedure ClearSubTotals;
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
    function FindName(const vName: string): TmnrDesignCell;
    procedure EnumByName(List: TList; const vName: string);
    procedure EnumLauouts(List: TmnrLayoutList);
    procedure ClearSubTotals;
  end;

  TmnrReference = class(TmnrLinkNode)
  private
    FTotal: Currency;
    function GetNext: TmnrReference;
    function GetNodes: TmnrReferencesRow;
    function GetPrior: TmnrReference;
    procedure SetNodes(const Value: TmnrReferencesRow);
  public
    property Next: TmnrReference read GetNext;
    property Prior: TmnrReference read GetPrior;
    property Nodes: TmnrReferencesRow read GetNodes write SetNodes;
    property Total: Currency read FTotal write FTotal;
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
    FAppendDetailTotals: Boolean;
    FAppendReportTotals: Boolean;
    FAppendDetailTitles: Boolean;
    FAppendPageTitles: Boolean;
    FAppendReportTitles: Boolean;
    FSectionLoopWay: TmnrSectionLoopWay;
    FAppendPageTotals: Boolean;
    FHitAppendTitles: Boolean;
    FRows: TmnObjectList<TmnrRow>;
    FTitleRow: TmnrRow; //default row data

    function GetNext: TmnrSection;
    function GetNodes: TmnrSections;
    function GetPrior: TmnrSection;
    procedure SetNodes(const Value: TmnrSections);
    function GetLoopWay: TmnrSectionLoopWay;
  protected
    function DoFetch(var vParams: TmnrFetch): TmnrAcceptMode;

    procedure DoAppendDetailTotals(vSection: TmnrSection);
    procedure DoAppendPageTotals(vSection: TmnrSection);
    procedure DoAppendToPageTotals(vSection: TmnrSection);
    procedure DoAppendReportTotals(vSection: TmnrSection);
    procedure DoAppendTitles(vSection: TmnrSection);
    function DoCreateDesignRows: TmnrDesignRows; virtual;
    function GetDesignRows: TmnrDesignRows;
    function DoCreateSections: TmnrSections;
    function GetSections: TmnrSections;

    procedure DoUpdateRowData(vRow: TmnrRow; vData: TObject); virtual;
    function GetCaption: string; virtual;
    function GetReport: TmnrCustomReport;
    procedure DoBeginFill(vReference: TmnrReferencesRow); virtual;
    procedure DoEndFill(vReference: TmnrReferencesRow); virtual;
    procedure Registered; virtual;
    function GetAppendTitlesDefault: Boolean; virtual;
    function GetAppendTitlesStored: Boolean;
  public
    constructor Create(vNodes: TmnrNode; vClass: TmnrSectionClassID);
    destructor Destroy; override;
    property Sections: TmnrSections read GetSections;
    property Rows: TmnObjectList<TmnrRow> read FRows;
    property TitleRow: TmnrRow read FTitleRow;

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

    procedure FillNow(vParams: TmnrFetch; vIndex: Integer; vReference: TmnrReferencesRow); virtual;
    function FindDesignCellName(const vName: string): TmnrDesignCell;
    //function FindDesignCellGuid(const vGuid: string): TmnrDesignCell;

    procedure AddReportTitles;
    procedure AddDetailTitles;
    procedure ClearSubTotals;
  published
    property AppendDetailTotals: Boolean read FAppendDetailTotals write FAppendDetailTotals default False;
    property AppendPageTotals: Boolean read FAppendPageTotals write FAppendPageTotals default False;
    property AppendReportTotals: Boolean read FAppendReportTotals write FAppendReportTotals default False;
    property AppendDetailTitles: Boolean read FAppendDetailTitles write FAppendDetailTitles default False;
    property AppendPageTitles: Boolean read FAppendPageTitles write FAppendPageTitles default False;
    property AppendReportTitles: Boolean read FAppendReportTitles write FAppendReportTitles stored GetAppendTitlesStored;
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
    function DoCreateSection(vClass: TmnrSectionClassID): TmnrSection; virtual;
    procedure DoBeginLoop(vSection: TmnrSection); virtual;
    procedure DoEndLoop(vSection: TmnrSection); virtual;

  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    destructor Destroy; override;
    function RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer = 0; vOnFetch: TOnFetch = nil; vLoopWay: TmnrSectionLoopWay = slwAuto): TmnrSection;
    property ByName[const vName: string]: TmnrSection read GetByName;
    function Find(const vName: string): TmnrSection;
    function FindDesignCellName(const vName: string): TmnrDesignCell;
    function RefRowsCount: Integer;
    procedure EnumByName(List: TList; const vName: string);
    procedure EnumLayouts(List: TmnrLayoutList);

    property Report: TmnrCustomReport read GetReport;
    property First: TmnrSection read GetFirst;
    property Last: TmnrSection read GetLast;

    procedure Loop;
    procedure CustomLoop; //for custom sections
    procedure ClearItems;
    procedure ClearDesignItems;
  end;

  TmnrIndex = class(TObject)
  private
    FReport: TmnrCustomReport;
  protected
    procedure Compute(vReport: TmnrCustomReport); virtual;
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    property Report: TmnrCustomReport read FReport;
    destructor Destroy; override;
    procedure Build;
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
    procedure DesignReport(vReport: TmnrCustomReport);
    procedure UpdateView(vCell: TmnrDesignCell = nil);
    procedure ProcessDrop(vNode: TmnrLayout);
  end;

  TmnrExportOptions = set of (mnrExportDisplayText, mnrTAB, mnrBOM);

  TmnrRowList = class(TmnObjectList<TmnrRow>)
  public
    constructor Create;
    procedure AppendRows(vRows: TmnrRows);
  end;

  TRowsData = Class(TmnObjectList<TObject>)
  public
    function CreateJsonData: TJSONObject;
  end;

  TmnrCustomReport = class(TPersistent) //belal: must be tobject but {$m+) not working need fix 
  private
    FCanceled: Boolean;
    FRowsData: TRowsData;
    FItems: TmnrRows;
    FSections: TmnrSections;
    FGroups: TmnrGroups;
    FRowsListIndex: TmnrRowsIndex;
    FProfiler: TmnrProfiler;
    FDesigner: ImnrReportDesigner;

    FHeaderReport: TmnrSection;
    FDetailTotals: TmnrSection;
    FReportTotals: TmnrSection;
    FDetailTitles: TmnrSection;
    FReportTitles: TmnrSection;
    FHeaderPage: TmnrSection;
    FFooterPage: TmnrSection;
    FFooterReport: TmnrSection;
    FDesigningMode: Boolean;
    FDesignLayouts: TmnrLayoutList;

    function GetProfiler: TmnrProfiler;
    function GetReportName: string;
    function GetReportCaption: string;
  protected
    FWorking: Boolean;
    function Canceled: Boolean;
    procedure AcceptNewRow(vRow: TmnrRow; var Accepted: Boolean); virtual;
    function HandleNewRow(vRow: TmnrRowNode):Boolean; virtual;
    procedure DoInitSections(vSections: TmnrSections); virtual;
    procedure InitSections(vSections: TmnrSections); //virtual;
    procedure InitLayouts(vGroups: TmnrGroups); virtual;
    procedure InitRequests; virtual;
    function CreateNewRow(vSection: TmnrSection; vReference: TmnrReferencesRow): TmnrRow;
    procedure Loop;
    //Apply param to report to use it in Queries or assign it to Variables
    //procedure SetParams(vParams: TmnrParams); virtual;
    //Collect params from current record in report to send it to another report
    function SumString(const vIndex: TmnrSumStringIndex): string; virtual;

    procedure Created; virtual; //after create
    procedure Start; virtual; //after build report only in generate
    procedure Finish; virtual; //
    procedure DoPrepare; virtual;

    function DoGetProfilerClass: TmnrProfilerClass; virtual;
    function DoCreateNewRow(vSection: TmnrSection): TmnrRow; virtual;
    function DoCreateProfiler: TmnrProfiler; virtual;
    function DoCreateSections: TmnrSections; virtual;
    function DoCreateGroups: TmnrGroups; virtual;
    function DoCreateItems: TmnrRows; virtual;
    procedure DoNewCell(vCell: TmnrCell); virtual;
    procedure DoReportLoaded; virtual;
    function DoGetReportName: string; virtual;
    function DoGetReportCaption: string; virtual;
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
    function GetFooterReport: TmnrSection;
    function GetHeaderReport: TmnrSection;
    function GetHeaderPage: TmnrSection;
    procedure DoLoad; virtual;

    procedure DoBeforeStart; virtual;
    procedure DoAfterStart; virtual;
    procedure DoBeforeLoop; virtual;
    procedure DoAfterLoop; virtual;
    procedure DoLoopError; virtual;
    function DoCreateReportDesgin: ImnrReportDesigner; virtual;
    procedure DoEnumExportRows(vList: TmnrRowList); virtual;
    procedure DoUpdateDesignLayouts; virtual;
    procedure UpdateDesignLayouts;
  public
    constructor Create;
    destructor Destroy; override;

    property Sections: TmnrSections read GetSections;
    property Groups: TmnrGroups read GetGroups;
    property Items: TmnrRows read GetItems;
    property RowsData: TRowsData read FRowsData;
    procedure Load;
    procedure Cancel;
    property Working: Boolean read FWorking;
    function Finished: Boolean;
    function FindSection(const vName: string): TmnrSection;
    property Designer: ImnrReportDesigner read FDesigner write FDesigner;

    procedure Prepare; //for design and generate
    procedure Generate;
    procedure Desgin;
    property Profiler: TmnrProfiler read GetProfiler;
    function ProfilerClass: TmnrProfilerClass;

    procedure Fetch(vSection: TmnrSection; var vParams: TmnrFetch); virtual;
    procedure RegisterRequest(const vName: string; vOnRequest: TOnRequest); virtual;

    property RowsIndex: TmnrRowsIndex read FRowsListIndex;
    property Rows[vRow: Integer]: TmnrRow read GetRows;
    property Cells[vRow, vCol: Integer]: TmnrCell read GetCells;

    function CreateReportDesgin: ImnrReportDesigner;
    property DesigningMode: Boolean read FDesigningMode;
    procedure Clear; virtual;

    property HeaderReport: TmnrSection read GetHeaderReport;
    property HeaderPage: TmnrSection read GetHeaderPage;
    property DetailTotals: TmnrSection read GetDetailTotals;
    property ReportTitles: TmnrSection read GetReportTitles;
    property DetailTitles: TmnrSection read GetDetailTitles;
    property ReportTotals: TmnrSection read GetReportTotals;
    property FooterPage: TmnrSection read GetFooterPage;
    property FooterReport: TmnrSection read GetFooterReport;

    property ReportName: string read GetReportName;
    property ReportCaption: string read GetReportCaption;
    property DesignLayouts: TmnrLayoutList read FDesignLayouts;

    //test purpose only
    function EnumExportRows: TmnrRowList;
    procedure ExportCSV(const vFile: TFileName; ExportOptions: TmnrExportOptions = []); overload;
    procedure ExportCSV(const vStream: TStream; ExportOptions: TmnrExportOptions = []); overload; virtual;

    procedure ExportHTML(const vFile: TFileName); overload;
    procedure ExportHTML(const vStream: TStream); overload;

  end;

  TmnrProfiler = class
  private
    FReport: TmnrCustomReport;
  protected
    function GetReport: TmnrCustomReport;
    procedure DoEnumReports(vList: TStrings); virtual;
  public
    constructor Create(vReport: TmnrCustomReport); virtual;
    procedure SaveReport; virtual;
    procedure LoadReport; virtual;
    procedure NewReport(const vNewName: string); virtual;
    procedure DeleteReport(const vName: string); virtual;
    procedure CopyReport(const vName, vNewName: string); virtual;

    procedure EnumReports(vList: TStrings); overload;
    function EnumReports: TStrings; overload;
    property Report: TmnrCustomReport read GetReport;
  end;


var
  DefaultCellClass: TmnrCellClass = nil; //test purpose

procedure DesignmnReport(vClass: TmnrCustomReportClass);

implementation

uses
  mnrNodes;

procedure DesignmnReport(vClass: TmnrCustomReportClass);
var
  aReport: TmnrCustomReport;
  aDesigner: ImnrReportDesigner;
begin
  aReport := vClass.Create;
  try
    aReport.Prepare;

    aDesigner := aReport.CreateReportDesgin;
    if aDesigner<>nil then
      aDesigner.DesignReport(aReport)
    else
      aReport.Free;
  except
    FreeAndNil(aReport);
    raise;
  end;
end;

{ TmnrCustomReport }

procedure TmnrCustomReport.Load;
begin
  Clear;
  DoLoad;

  UpdateDesignLayouts;
end;

procedure TmnrCustomReport.AcceptNewRow(vRow: TmnrRow; var Accepted: Boolean);
begin
  Accepted := True;
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
  FRowsData.Clear;
  Sections.ClearItems;
  //DesignLayouts.Clear;
end;

constructor TmnrCustomReport.Create;
begin
  inherited Create;
  FRowsData := TRowsData.Create;
  FDesignLayouts := TmnrLayoutList.Create;
  FDesigningMode := False;

  FProfiler := DoCreateProfiler;
  //FProfiler.FReport := Self;
  FSections := DoCreateSections;
  FGroups := DoCreateGroups;
  FItems := DoCreateItems;
  FRowsListIndex := TmnrRowsIndex.Create(Self);

  //InitSections(FSections);
  FGroups.FReport := Self;

  FWorking := False;
  Created;
end;

procedure TmnrCustomReport.InitLayouts(vGroups: TmnrGroups);
begin

end;

procedure TmnrCustomReport.InitRequests;
begin

end;

function TmnrCustomReport.CreateNewRow(vSection: TmnrSection; vReference: TmnrReferencesRow): TmnrRow;
begin
  Result := DoCreateNewRow(vSection);
  Result.FSection := vSection;
  Result.FReferencesRow := vReference;
end;

function TmnrCustomReport.ProfilerClass: TmnrProfilerClass;
begin
  Result := DoGetProfilerClass;
end;

procedure TmnrCustomReport.DoBeforeStart;
begin

end;

procedure TmnrCustomReport.DoAfterStart;
begin

end;

procedure TmnrCustomReport.DoBeforeLoop;
begin

end;

procedure TmnrCustomReport.DoAfterLoop;
begin

end;

function TmnrCustomReport.DoCreateGroups: TmnrGroups;
begin
  Result := TmnrGroups.Create;
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
  Result := ProfilerClass.Create(Self);
end;

function TmnrCustomReport.DoCreateReportDesgin: ImnrReportDesigner;
begin
  Result := nil;
end;

function TmnrCustomReport.CreateReportDesgin: ImnrReportDesigner;
begin
  FDesigningMode := True;
  Result := DoCreateReportDesgin;
end;

procedure TmnrCustomReport.InitSections(vSections: TmnrSections);
begin
  FHeaderPage   := FSections.RegisterSection('HeaderPage', '��� ������', sciHeaderPage, ID_SECTION_HEADERPage);
  FHeaderReport := FSections.RegisterSection('HeaderReport', '��� �������', sciHeaderReport, ID_SECTION_HEADERREPORT);
  FReportTitles := FSections.RegisterSection('ReportTitles', '������ �������', sciHeaderTitles);
  FDetailTitles := FSections.RegisterSection('DetailTitles', '������ �������', sciDetailTitles, 0, nil, slwSingle);

  DoInitSections(vSections); //for header and details

  FDetailTotals := FSections.RegisterSection('DetailTotals', '������ �������', sciFooterDetails);
  FReportTotals := FSections.RegisterSection('ReportTotals', '������ �������', sciFooterReport);
  FFooterReport   := FSections.RegisterSection('FooterReport', '���� �������', sciFooterReport, ID_SECTION_FOOTERREPORT);
  FFooterPage   := FSections.RegisterSection('FooterPage', '���� ������', sciFooterPage, ID_SECTION_FOOTERPAGE);
end;

procedure TmnrCustomReport.Desgin;
var
  aDesigner: ImnrReportDesigner;
begin
  aDesigner := CreateReportDesgin;
  if aDesigner<>nil then
    aDesigner.DesignReport(Self)
  else
    Free;
end;

destructor TmnrCustomReport.Destroy;
begin
  FGroups.Free;
  FSections.Free;
  FItems.Free;
  FreeAndNil(FRowsListIndex);
  FreeAndNil(FProfiler);
  FreeAndNil(FDesignLayouts);
  FreeAndNil(FRowsData);
  inherited;
end;

function TmnrCustomReport.DoCreateSections: TmnrSections;
begin
  Result := TmnrSections.Create(Self);
end;

procedure TmnrCustomReport.DoEnumExportRows(vList: TmnrRowList);
begin
  vList.AppendRows(Items);
end;

function TmnrCustomReport.DoGetProfilerClass: TmnrProfilerClass;
begin
  Result := TmnrProfiler;
end;

function TmnrCustomReport.DoGetReportCaption: string;
begin
  Result := ReportName;
end;

function TmnrCustomReport.DoGetReportName: string;
begin
  Result := Copy(ClassName, 2, MaxInt);
end;

procedure TmnrCustomReport.DoInitSections(vSections: TmnrSections);
begin

end;

procedure TmnrCustomReport.DoLoad;
begin
  Profiler.LoadReport;
  DoReportLoaded;
end;

procedure TmnrCustomReport.DoLoopError;
begin

end;

procedure TmnrCustomReport.DoNewCell(vCell: TmnrCell);
begin

end;

procedure TmnrCustomReport.DoPrepare;
begin

end;

procedure TmnrCustomReport.DoReportLoaded;
begin

end;

procedure TmnrCustomReport.DoUpdateDesignLayouts;
begin

end;

function TmnrCustomReport.EnumExportRows: TmnrRowList;
begin
  Result := TmnrRowList.Create;
  try
    DoEnumExportRows(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TmnrCustomReport.ExportCSV(const vStream: TStream; ExportOptions: TmnrExportOptions);

const
  sEOL = #13#10;
  sLEBom:WORD = $FEFF;

  procedure WriteStr(const vStr: string);
  begin
    vStream.Write(vStr[1], ByteLength(vStr));
  end;

var
  r: TmnrRow;
  n: TmnrCell;
  l: TmnrRowList;
begin
  if (mnrBOM in ExportOptions) then
  begin
    vStream.WriteBuffer(sLEBom, SizeOf(sLEBom))
  end;

  l := EnumExportRows;
  try
    for r in l do
    begin
      n := r.First;

      while n <> nil do
      begin
        if mnrExportDisplayText in ExportOptions then
          WriteStr(n.DisplayText)
        else
          WriteStr(n.AsString);
        n := n.Next;
        if n <> nil then
        begin
          if mnrTAB in ExportOptions then
            WriteStr(#9)
          else
            WriteStr(';');
        end;
      end;

      if r <> l.Last then
        WriteStr(sEOL);
    end;
  finally
    l.Free;
  end;
end;

procedure TmnrCustomReport.ExportHTML(const vStream: TStream);

  procedure WriteStr(const vStr: UTF8String);
  begin
    vStream.Write(vStr[1], Length(vStr));
  end;

  procedure WritelnStr(const vStr: UTF8String; vIdent: Integer=0);
  begin
    if vIdent<>0 then WriteStr(RepeatString('  ', vIdent));
    WriteStr(vStr);
    WriteStr(#13#10);
  end;

var
  r: TmnrRow;
  n: TmnrCell;
  l: TmnrRowList;

begin
  l := EnumExportRows;
  try
    WritelnStr('<!doctype html>');
    WritelnStr('<html>');
    WritelnStr('<head>', 1);
    WritelnStr('<meta charset="utf-8">', 2);
    WritelnStr('<title>Invoice</title>', 2);
    WritelnStr('<meta name="description" content="Invoice demo sample">', 2);
    WritelnStr('<meta name="author" content="mnReports">', 2);
    WritelnStr('</head>', 1);


    WritelnStr('<body>', 1);
    WritelnStr('<table>', 2);
    for r in l do
    begin
      WritelnStr('<tr>', 3);
      n := r.First;
      while n <> nil do
      begin
        WritelnStr('<td>'+n.DisplayText+'<td>', 4);
        n := n.Next;
      end;
      WritelnStr('</tr>', 3);
    end;
    WritelnStr('</table>', 2);

    WritelnStr('</body>', 1);
    WritelnStr('</html>');
  finally
    l.Free;
  end;
end;

procedure TmnrCustomReport.ExportHTML(const vFile: TFileName);
var
  f: TFileStream;
begin
  f := TFileStream.Create(vFile, fmCreate);
  try
    ExportHTML(f);
  finally
    f.Free;
  end;
end;

procedure TmnrCustomReport.ExportCSV(const vFile: TFileName; ExportOptions: TmnrExportOptions);
var
  f: TFileStream;
begin
  f := TFileStream.Create(vFile, fmCreate);
  try
    ExportCSV(f, ExportOptions);
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
  FRowsListIndex.Build;
end;

function TmnrCustomReport.Finished: Boolean;
begin
  Result := not Working;
end;

procedure TmnrCustomReport.Generate;
begin
  //Prepare; call externally
  FWorking := True;
  try
    //SetParams(Params);
    //Load;
    DoBeforeStart;
    try
      Start;
    finally
      DoAfterStart;
    end;

    DoBeforeLoop;
    try
      try
        Loop;
      except
        DoLoopError;
        raise;
      end;
    finally
      DoAfterLoop;
    end;

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

function TmnrCustomReport.GetFooterReport: TmnrSection;
begin
  Result := FFooterReport;
end;

function TmnrCustomReport.GetHeaderReport: TmnrSection;
begin
  Result := FHeaderReport;
end;

function TmnrCustomReport.GetReportTotals: TmnrSection;
begin
  Result := FReportTotals;
end;

function TmnrCustomReport.GetDetailTitles: TmnrSection;
begin
  Result := FDetailTitles;
end;

function TmnrCustomReport.GetReportCaption: string;
begin
  Result := DoGetReportCaption;
end;

function TmnrCustomReport.GetReportName: string;
begin
  Result := DoGetReportName;
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

function TmnrCustomReport.HandleNewRow(vRow: TmnrRowNode): Boolean;
begin
  Result := False;
end;

procedure TmnrCustomReport.Created;
begin

end;

procedure TmnrCustomReport.Loop;
begin
  InitRequests; //must be after start
  FCanceled := False;
  try
    Sections.Loop;
    Sections.DoAppendPageTotals(FooterPage);
  except
    raise;
  end;
end;

procedure TmnrCustomReport.Prepare;
begin
  DoPrepare;
  InitSections(FSections);
  Groups.InitLayouts;
  Load;
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

function TmnrCustomReport.SumString(const vIndex: TmnrSumStringIndex): string;
begin
  case vIndex of
    ssiPageTotal: Result := '����� ������';
    ssiToPageTotal: Result := '������� ��������';
    else
      Result := '�������';
  end;
end;

procedure TmnrCustomReport.UpdateDesignLayouts;
begin
  FDesignLayouts.Clear;

  Sections.EnumLayouts(FDesignLayouts);
  DoUpdateDesignLayouts;
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

procedure TmnrRows.SetFirst(const Value: TmnrRow);
begin
  inherited SetFirst(Value);
end;

procedure TmnrRows.SetLast(const Value: TmnrRow);
begin
  inherited SetLast(Value);
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

procedure TmnrSection.DoEndFill(vReference: TmnrReferencesRow);
begin

end;

procedure TmnrSection.DoAppendDetailTotals(vSection: TmnrSection);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    f := True;
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(vSection, nil);
      aRow.FLocked := True;
      aRow.FDesignRow := aDesignRow;
      try
        d := aDesignRow.First;
        while d <> nil do
        begin
          if f and not d.AppendTotals then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString(ssiTotal);
          end
          else
          begin
            //c := TmnrReportTotalCell.Create(aRow); todo to use it instead, we need to understand why CurrencyCellClass.Create(aRow)?
            c := d.CurrencyCellClass.Create(aRow); //if we do that we will lose the special format in display text
            if d.AppendTotals  then
              c.AsCurrency := d.SubTotal;
          end;
          c.FDesignCell := d;
          c.FReference := d.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      if aRow <> nil then
        vSection.Rows.Add(aRow);

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.DoAppendPageTotals(vSection: TmnrSection);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    f := True;
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(vSection, nil);
      aRow.FLocked := True;
      aRow.FDesignRow := aDesignRow;
      try
        d := aDesignRow.First;
        while d <> nil do
        begin
          if f and not d.AppendTotals then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString(ssiPageTotal);
          end
          else
          begin
            c := TmnrPageTotalCell.Create(aRow);
          end;
          c.FDesignCell := d;
          c.FReference := d.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      vSection.Rows.Add(aRow);

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.DoAppendReportTotals(vSection: TmnrSection);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    f := True;
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(vSection, nil);
      aRow.FLocked := True;
      aRow.FDesignRow := aDesignRow;
      try
        d := aDesignRow.First;
        while d <> nil do
        begin
          if f and not d.AppendTotals then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString(ssiTotal);
          end
          else
          begin
            c := TmnrReportTotalCell.Create(aRow);
            c.AsCurrency := d.Total;
          end;
          c.FDesignCell := d;
          c.FReference := d.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      vSection.Rows.Add(aRow);

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.DoAppendTitles(vSection: TmnrSection);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  c: TmnrCell;
begin
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(vSection, nil);
      aRow.FDesignRow := aDesignRow;
      if FTitleRow=nil then FTitleRow := aRow;

      try
        d := aDesignRow.First;
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

            c.FReference := d.Reference;
          end;
          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      vSection.Rows.Add(aRow);

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.DoAppendToPageTotals(vSection: TmnrSection);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  aRow: TmnrRow;
  f: Boolean; //first
  c: TmnrCell;
begin
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    f := True;
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(vSection, nil);
      aRow.FLocked := True;
      aRow.FDesignRow := aDesignRow;
      try
        d := aDesignRow.First;
        while d <> nil do
        begin
          if f and not d.AppendTotals then
          begin
            f := False;
            c := TmnrTextReportCell.Create(aRow);
            c.AsString := Report.SumString(ssiToPageTotal);
          end
          else
          begin
            c := TmnrToPageTotalCell.Create(aRow);
          end;
          c.FDesignCell := d;
          c.FReference := d.Reference;

          d := d.Next;
        end;
      except
        aRow.Free;
        raise;
      end;
      //todo make arow pass as var and if report handle row and free it then do nothing
      Report.HandleNewRow(aRow);
      vSection.Rows.Add(aRow);

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.DoBeginFill(vReference: TmnrReferencesRow);
begin

end;

procedure TmnrSection.AddReportTitles;
begin
  if not FHitAppendTitles then
  begin
    FHitAppendTitles := True;
    if AppendPageTitles then DoAppendTitles(Report.HeaderPage);
    if AppendReportTitles then DoAppendTitles(Report.ReportTitles);
  end;
end;

procedure TmnrSection.AddDetailTitles;
begin
  if AppendDetailTitles then
  begin
    if ClassID=sciFooterReport then
    begin
      DoAppendTitles(Report.FooterReport)
    end
    else
      DoAppendTitles(Report.DetailTitles);
  end;
end;

procedure TmnrSection.ClearSubTotals;
begin
  DesignRows.ClearSubTotals;
end;

constructor TmnrSection.Create(vNodes: TmnrNode; vClass: TmnrSectionClassID);
begin
  inherited Create(vNodes);
  FClassID := vClass;
  FSections := DoCreateSections;
  FDesignRows := DoCreateDesignRows;
  FReferencesRows := TmnrReferencesRows.Create;
  FAppendDetailTotals := False;
  FAppendReportTotals := False;
  FAppendDetailTitles := False;
  FAppendPageTitles   := False;
  FAppendPageTotals   := False;
  FAppendReportTitles := GetAppendTitlesDefault;

  FSectionLoopWay     := slwAuto;
  FRows := TmnObjectList<TmnrRow>.Create;
  FRows.OwnsObjects := False;
end;

destructor TmnrSection.Destroy;
begin
  FSections.Free;
  FDesignRows.Free;
  FReferencesRows.Free;
  FreeAndNil(FRows);
  inherited;
end;

function TmnrSection.DoFetch(var vParams: TmnrFetch): TmnrAcceptMode;
begin
  vParams.Reset;
  vParams.Data := Report.RowsData.CreateJsonData; //multiline row

  if Assigned(FOnFetch) then
    FOnFetch(vParams)
  else
    Report.Fetch(Self, vParams);

  Result := vParams.AcceptMode;
end;

type
  THackJson = class(TJSONValue)
    FMembers: TList<TJSONPair>;
  end;

procedure TmnrSection.FillNow(vParams: TmnrFetch; vIndex: Integer; vReference: TmnrReferencesRow);
var
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
  l: TmnrLayout;
  aRow: TmnrRow;
  //c: TmnrCell;
  Accepted: Boolean;
begin
  //vParams.Data shared for all DesignRows
  vParams.Data.AddPair('ID', vParams.ID);
  vParams.Data.AddPair('Locked', vParams.Locked);
  vParams.Data.AddPair('Index', vIndex);

  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    while aDesignRow <> nil do
    begin
      aRow := Report.CreateNewRow(Self, vReference);
      try
        aRow.FLocked    := vParams.Locked;
        aRow.FDesignRow := aDesignRow;
        aRow.FJData     := vParams.Data;

        //note vParams.data <> nil
        DoUpdateRowData(aRow, vParams.Data);

        d := aDesignRow.First;
        while d <> nil do
        begin
          l := d.Layout;
          //c := l.NewCell(aRow);
          if l <> nil then
            l.NewCell(d, aRow);
          d := d.Next;
        end;
      except
        FreeAndNil(aRow);
        raise;
      end;

      //todo make arrow pass as var and if report handle row and free it then do nothing
      if aRow <> nil then
      begin
        Accepted := True;
        Report.AcceptNewRow(aRow, Accepted);
        if Accepted then
        begin
          Report.HandleNewRow(aRow);
          if aRow <> nil then //maybe HandleNewRow free it too
          begin
            aRow.ScaleCells;//Zaher
            Rows.Add(aRow);
          end;
        end
        else
          FreeAndNil(aRow); //no need it if not accepted
      end;

      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

function TmnrSection.FindDesignCellName(const vName: string): TmnrDesignCell;
begin
  Result := DesignRows.FindName(vName)
end;

{function TmnrSection.FindDesignCellGuid(const vGuid: string): TmnrDesignCell;
begin
  Result := DesignRows.Find(vGuid)
end;}

function TmnrSection.GetAppendTitlesDefault: Boolean;
begin
  Result := False;
end;

function TmnrSection.GetAppendTitlesStored: Boolean;
begin
  Result := FAppendReportTitles <> GetAppendTitlesDefault;
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
  aDesignRow: TmnrDesignRow;
  d: TmnrDesignCell;
begin
  Result := ReferencesRows.Add;
  aDesignRow := DesignRows.First;
  if aDesignRow <> nil then
  begin
    while aDesignRow <> nil do
    begin
      d := aDesignRow.First;
      while d <> nil do
      begin
        {l := d.Layout;
        if l <> nil then
          l.FReference := Result.Add;}
        d.FReference := Result.Add;
        d := d.Next;
      end;
      aDesignRow := aDesignRow.Next;
    end;
  end;
end;

procedure TmnrSection.Registered;
begin
end;

procedure TmnrSection.SetNodes(const Value: TmnrSections);
begin
  inherited SetNodes(Value);
end;

procedure TmnrSection.DoUpdateRowData(vRow: TmnrRow; vData: TObject);
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
    s.Rows.Clear;
    s.ReferencesRows.Clear;
    s.FHitAppendTitles := False;
    s.FTitleRow := nil;
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

procedure TmnrSections.CustomLoop;
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
    aParams.Clear;

    case s.LoopWay of
      slCustom:
      begin
        r := nil;
        while not Report.Canceled and (aParams.AcceptMode <> acmEof) do
        begin
          s.DoFetch(aParams);

          if (aParams.FetchMode = fmFirst) and (aParams.AcceptMode <> acmEof) then
          begin
            r := s.NewReference;
            s.DoBeginFill(r);
            s.AddDetailTitles;
          end;

          if aParams.AcceptMode = acmAccept then
          begin
            s.FillNow(aParams, aIdx, r);
            //s.Sections.Loop;
          end;

          if (aParams.AcceptMode = acmEof) and (s.Rows.Count <> 0) then
          begin
            if (r <> nil) then
            begin
              if s.AppendDetailTotals then s.DoAppendDetailTotals(Report.FDetailTotals);
              s.DoEndFill(r);
              s.ClearSubTotals;
            end;
          end;

          if aParams.FetchMode = fmFirst then aParams.FetchMode := fmNext;
          if aParams.AcceptMode=acmAccept then
            Inc(aIdx)
          else if aParams.AcceptMode in [acmSkip, acmSkipAll] then
            aParams.AcceptMode := acmAccept;
        end;
      end;
    end;
    s := s.Next;
  end;
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

procedure TmnrSections.DoBeginLoop(vSection: TmnrSection);
begin

end;

procedure TmnrSections.DoAppendPageTotals(vSection: TmnrSection);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    if s.AppendPageTotals then
    begin
      s.DoAppendPageTotals(vSection);
      s.DoAppendToPageTotals(vSection);
    end;
    s.Sections.DoAppendPageTotals(vSection);
    s := s.Next;
  end;
end;

function TmnrSections.DoCreateSection(vClass: TmnrSectionClassID): TmnrSection;
begin
  Result := TmnrSection.Create(Self, vClass);
end;

procedure TmnrSections.DoEndLoop(vSection: TmnrSection);
begin

end;

procedure TmnrSections.EnumByName(List: TList; const vName: string);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    s.DesignRows.EnumByName(List, vName);
    s.Sections.EnumByName(List, vName);
    s := s.Next;
  end;
end;

procedure TmnrSections.EnumLayouts(List: TmnrLayoutList);
var
  s: TmnrSection;
begin
  s := First;
  while s <> nil do
  begin
    s.DesignRows.EnumLauouts(List);
    s.Sections.EnumLayouts(List);
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

function TmnrSections.FindDesignCellName(const vName: string): TmnrDesignCell;
var
  s: TmnrSection;
begin
  Result := nil;
  s := First;
  while s <> nil do
  begin
    Result := s.DesignRows.FindName(vName);
    if Result<>nil then
      Break
    else
    begin
      Result := s.Sections.FindDesignCellName(vName);
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
    aParams.Clear;

    DoBeginLoop(s);
    try
      s.AddReportTitles;

      case s.LoopWay of
        slwSingle:
        begin
          s.DoFetch(aParams);
          if aParams.AcceptMode = acmAccept then
          begin
            r := s.NewReference;

            s.DoBeginFill(r);
            try
              s.FillNow(aParams, 0, r);
            finally
              s.DoEndFill(r);
            end;

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
              //if (s.ClassID = sciDetails) then //improve add referance on first accepted ...
              if aParams.AcceptMode <> acmSkipAll then
              begin
                r := s.NewReference;
                s.DoBeginFill(r);
                s.AddDetailTitles;
              end;
            end
            else
            begin
              if (s.ClassID <> sciDetails) and (s.ClassID <> sciFooterReport)  then //belal: need review :(
              begin
                r := s.NewReference;
                s.DoBeginFill(r);
                if aParams.AcceptMode = acmAccept then
                  s.AddDetailTitles;
              end;
            end;

            if aParams.AcceptMode = acmAccept then
            begin
              s.FillNow(aParams, aIdx, r);
              if (s.ClassID = sciHeaderDetails) then
                s.DoEndFill(r);
              s.Sections.Loop;
            end
            else if (aParams.AcceptMode = acmSkip) and (s.ClassID = sciHeaderDetails) then
              s.Sections.Loop;

            if (aParams.AcceptMode = acmEof) and (s.Rows.Count <> 0) then
            begin
              if (r <> nil) then
              begin
                if s.AppendDetailTotals then s.DoAppendDetailTotals(Report.FDetailTotals);
                s.DoEndFill(r);
                s.ClearSubTotals;
              end;
            end;

            if aParams.FetchMode = fmFirst then aParams.FetchMode := fmNext;

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
    finally
      DoEndLoop(s);
    end;
    s := s.Next;
  end;
end;

function TmnrSections.RefRowsCount: Integer;
var
  s: TmnrSection;
begin
  Result := 0;
  s := First;
  while s <> nil do
  begin
    Inc(Result, s.ReferencesRows.Count);
    Inc(Result, s.Sections.RefRowsCount);

    s := s.Next;
  end;
end;

function TmnrSections.RegisterSection(const vName, vCaption: string; const vClass: TmnrSectionClassID; const vID: Integer; vOnFetch: TOnFetch; vLoopWay: TmnrSectionLoopWay): TmnrSection;
begin
  Result := DoCreateSection(vClass);
  Result.FName := vName;
//  Result.FClassID := vClass;
  Result.OnFetch := vOnFetch;
  Result.FCaption := vCaption;
  Result.SectionLoopWay := vLoopWay;
  if vID = 0 then
    Result.FID := Ord(vClass)
  else
    Result.FID := vID;
  //Result.FLoopWay := vLoopWay;
  Result.Registered;
end;

constructor TmnrRow.Create(vNodes: TmnrNode);
begin
  inherited Create(vNodes);

end;

procedure TmnrRow.DescaleCells;
var
  c: TmnrCell;
begin
  if First <> nil then
  begin
    c := First as TmnrCell;
    repeat
      if (c.DesignCell <> nil) and not Locked then
        c.DesignCell.DescaleCell(c);
      c := c.Next as TmnrCell;
    until (c = nil);
  end;
end;

destructor TmnrRow.Destroy;
begin

  inherited;
end;

function TmnrRow.FindCell(vName: string): TmnrCell;
var
  c: TmnrCell;
  b: Boolean;
begin
  Result := nil;
  if First <> nil then
  begin
    c := First as TmnrCell;
    repeat
      b := (c.Layout<>nil) and SameText(c.Layout.ClassName, vName);
      b := b or SameText(c.DesignCell.Name, vName);
      b := b or SameText((c.DesignCell as TmnrDesignCell).AliasName, vName);

      if b then
        Result := c
      else
        c := c.Next as TmnrCell;

    until (Result <> nil) or (c = nil);
  end;
end;

procedure TmnrRow.ScaleCells;
var
  c: TmnrCell;
begin
  if not Locked then
  begin
    c := First;
    while c<>nil do
    begin
      c.Scale;
      c := c.Next;
    end;
  end;
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

function TmnrRow.GetID: Int64;
begin
  Result := JData.GetValue<Int64>('ID', 0);
end;

function TmnrRow.GetLast: TmnrCell;
begin
  Result := TmnrCell(inherited GetLast);
end;

function TmnrRow.GetLocked: Boolean;
begin
  //Result := JData.GetValue<Boolean>('Locked', False); //slow fill
  Result := Flocked;
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

function TmnrRow.GetRowIndex: Integer;
begin
  Result := JData.GetValue<Integer>('Index', 0);
end;

function TmnrRow.GetSection: TmnrSection;
begin
  Result := FSection;  
end;

{ TmnrLayout }

procedure TmnrLayout.CellsExchanged(vCell1, vCell2: TmnrCell);
begin
  DoCellsExchanged(vCell1, vCell2)
end;

function TmnrLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := nil;
end;

function TmnrLayout.CreateDesignCell(vRow: TmnrDesignRow; InitIt: Boolean): TmnrDesignCell;
begin
  Result := DoCreateDesignCell(vRow);
  Result.Layout := Self;
  Result.Name := Name;
  if InitIt then
    InitDesignCell(Result);
end;

function TmnrLayout.DisplayText: string;
begin
  if Title<>'' then
    Result := Title
  else
    Result := Name;
end;

procedure TmnrLayout.DoCellsExchanged(vCell1, vCell2: TmnrCell);
begin

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

function TmnrLayout.GetAsBoolean: Boolean;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsBoolean;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsCurrency: Currency;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsCurrency;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsDateTime: TDateTime;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsDateTime;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsDouble: Double;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsDouble;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsInteger: Longint;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsInteger;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsString: string;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsString;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsVariant: Variant;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsVariant;
  finally
    aCell.Free;
  end;
end;

function TmnrLayout.GetAsData: Integer;
var
  aCell: TmnrCell;
begin
  aCell := NewCell(nil, nil);
  try
    Result := aCell.AsData;
  finally
    aCell.Free;
  end;
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

function TmnrLayout.GetTag: Integer;
begin
  Result := FTag;
end;

procedure TmnrLayout.InitDesignCell(vDesignCell: TmnrDesignCell);
begin

end;

function TmnrLayout.NewCell(vDesignCell: TmnrDesignCell; vRow: TmnrRow): TmnrCell;
begin
  Result := CreateCell(vRow);
  if Result <> nil then
  begin
    try
      Result.FLayout := Self; //for furmula layouts
      Result.FDesignCell := vDesignCell;
      if (vDesignCell<>nil) then  Result.FReference :=  vDesignCell.Reference;
      DoRequest(Result);
{      if (vRow<>nil) and (vDesignCell<>nil) and not vRow.Locked then
        vDesignCell.ScaleCell(Result);} //moved to FillNow

      if vDesignCell<>nil then //ignore formula and layout get as (string currency .....)
        Report.DoNewCell(Result);
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        if vDesignCell<>nil then
          raise Exception.CreateFmt('%s'#13'[%s] %s', [vDesignCell.DisplayText, Name, E.Message])
        else
          raise Exception.CreateFmt('[%s] %s', [Name, E.Message]);
      end;
    end;
  end
  else
    raise Exception.CreateFmt('Error Creating Cell for %s', [Name]);
end;

procedure TmnrLayout.Request(vCell: TmnrCell);
begin
  DoRequest(vCell);
end;

procedure TmnrLayout.ScaleCell(vCell: TmnrCell; Invert: Boolean);
begin

end;

{ TmnrCell }

function TmnrCell.Compare(vCell: TmnrCell): Integer;
begin
  Result := DoCompare(vCell);
end;

procedure TmnrCell.Created;
begin
  inherited;
  FIsNull := False;
end;

function TmnrCell.DisplayText: string;
begin
  if IsNull then
    Result := ''
  else
  begin
    Result := DoGetDisplayText;
    if DesignCell<>nil then DesignCell.UpdateCellDisplayText(Self, Result);
  end;
end;

function TmnrCell.DoCompare(vCell: TmnrCell): Integer;
begin
  Result := 0;
end;

function TmnrCell.DoGetDisplayText: string;
begin
  Result := inherited DisplayText;
end;

procedure TmnrCell.DoSumCell(vCell: TmnrCell);
var
  s, c: string;
begin
  s := AsString;
  c := vCell.AsString;
  if (c<>'')or(s<>'') then
  begin
    if (c<>'') then
    begin
      if (AnsiPos(s, c)>0) then
        s := c
      else
      begin
        if s = '' then
          s := c
        else
          s := Format('%s '#150' %s', [c, s]);
      end
    end;
    AsString := s;
  end;
end;

function TmnrCell.ExecuteAction: Boolean;
begin
  Result := Assigned(FAction);
  if Result then
    FAction(Self);
end;

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
  {if DesignCell<>nil then
    Result := DesignCell.Layout
  else}
  Result := FLayout;
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

procedure TmnrCell.Scale;
begin
  if DesignCell<>nil then DesignCell.ScaleCell(Self);
end;

procedure TmnrCell.SumCell(vCell: TmnrCell);
begin
  //if not Layout.DenySumming then
    DoSumCell(vCell);
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

procedure TmnrIndex.Build;
begin
  if Report<>nil then
  begin
    Compute(Report);
  end;
end;

procedure TmnrIndex.Compute(vReport: TmnrCustomReport);
begin

end;

constructor TmnrIndex.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  FReport := vReport;
  Build;
end;

destructor TmnrIndex.Destroy;
begin

  inherited;
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

function TmnrLayouts.CreateLayout(vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest; vNumber: Integer; vTag: Integer; vIncludeSections: TmnrSectionClassIDs; vExcludeSections: TmnrSectionClassIDs): TmnrLayout;
begin
  Result := vClass.Create(Self);
  with Result do
  begin
    FName := vName;
    FNumber := vNumber;
    FTag := vTag;
    FIncludeSections := vIncludeSections;
    FExcludeSections := vExcludeSections;
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

function TmnrDesignCell.AliasName: string;
begin
  if Alias<>'' then
    Result := Alias
  else
    Result := Layout.Name;
end;

procedure TmnrDesignCell.AssignTo(Dest: TPersistent);
var
  d: TmnrDesignCell;
begin
  //inherited;
  if Dest is TmnrDesignCell then
  begin
    d := Dest as TmnrDesignCell;
    d.Width := Width;
    d.Number := Number;
    d.AppendTotals := AppendTotals;
  end;
end;

constructor TmnrDesignCell.Create(vNodes: TmnrNodes);
begin
  inherited Create(vNodes);
  FWidth := DEFAULT_CELL_WIDTH;
  FAppendTotals := False;
  FNumber := 0;
end;

function TmnrDesignCell.CurrencyCellClass: TmnrCellClass;
begin
  Result := TmnrCurrencyReportCell;
end;

procedure TmnrDesignCell.DescaleCell(vCell: TmnrCell);
begin
  if not Locked then
  begin
    Layout.ScaleCell(vCell, True);
    Dec(FCount)
  end;
end;

destructor TmnrDesignCell.Destroy;
begin

  inherited;
end;

function TmnrDesignCell.DisplayText: string;
begin
  Result := '';
end;

procedure TmnrDesignCell.DoHasName(const vName: string; var vAccept: Boolean);
begin

end;

procedure TmnrDesignCell.DoUpdateCellDisplayText(vCell: TmnrCell; var vText: string);
begin

end;

function TmnrDesignCell.GetRow: TmnrDesignRow;
begin
  Result := Nodes as TmnrDesignRow;
end;

procedure TmnrDesignCell.UpdateCellDisplayText(vCell: TmnrCell; var vText: string);
begin
  DoUpdateCellDisplayText(vCell, vText);
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

function TmnrDesignCell.GetWidth: Integer;
begin
  if Hidden then
    Result := 0
  else
    Result := FWidth;
end;

function TmnrDesignCell.HasName(const vName: string): Boolean;
begin
  Result := SameText(Name, vName) or SameText(AliasName, vName);
  DoHasName(vName, Result);
end;

procedure TmnrDesignCell.Lock;
begin
  Inc(FLockCount);
end;

procedure TmnrDesignCell.UnLock;
begin
  Dec(FLockCount);
end;

function TmnrDesignCell.Locked: Boolean;
begin
  Result := FLockCount<>0;
end;

procedure TmnrDesignCell.ScaleCell(vCell: TmnrCell);
begin
  if not Locked then
  begin
    Layout.ScaleCell(vCell, False);
    Inc(FCount)
  end;
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
    if (Layout=nil)and(Report<>nil) then Layout := Report.Groups.FindLayout(Value);
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

procedure TmnrDesignRow.ClearSubTotals;
var
  c: TmnrDesignCell;
begin
  c := First;
  while c<>nil do
  begin
    c.SubTotal := 0;
    c := c.Next;
  end;
end;

procedure TmnrDesignRow.EnumByName(List: TList; const vName: string);
var
  Cell: TmnrDesignCell;
begin
  Cell := First;
  while Cell <> nil do
  begin
    //need  use  Result.HasName(vName)

    if SameText(Cell.Name, vName) or SameText(Cell.AliasName, vName) then
      List.Add(Cell);
    Cell := Cell.Next;
  end;
end;

procedure TmnrDesignRow.EnumLayouts(List: TmnrLayoutList);
var
  Cell: TmnrDesignCell;
begin
  Cell := First;
  while Cell <> nil do
  begin
    if List.IndexOf(Cell.Layout)=-1 then
      List.Add(Cell.Layout);
    Cell := Cell.Next;
  end;
end;

function TmnrDesignRow.FindName(const vName: string): TmnrDesignCell;
begin
  Result := First;
  while Result<>nil do
  begin
    if Result.HasName(vName) then
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
  if FRowReport<>nil then
    Result := FRowReport
  else if Section<>nil then
    Result := Section.Report
  else
    Result := nil;
end;

function TmnrDesignRow.GetSection: TmnrSection;
begin
  if DesignRows<>nil then
    Result := DesignRows.Section
  else
    Result := nil;
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

procedure TmnrDesignRows.ClearSubTotals;
var
  aRow: TmnrDesignRow;
begin
  aRow := First;
  while aRow<>nil do
  begin
    aRow.ClearSubTotals;
    aRow := aRow.Next;
  end;
end;

constructor TmnrDesignRows.Create(vSection: TmnrSection);
begin
  inherited Create;
  FSection := vSection;
end;

procedure TmnrDesignRows.EnumByName(List: TList; const vName: string);
var
  aRow: TmnrDesignRow;
begin
  aRow := First;
  while aRow<>nil do
  begin
    aRow.EnumByName(List, vName);
    aRow := aRow.Next;
  end;
end;

procedure TmnrDesignRows.EnumLauouts(List: TmnrLayoutList);
var
  aRow: TmnrDesignRow;
begin
  aRow := First;
  while aRow<>nil do
  begin
    aRow.EnumLayouts(List);
    aRow := aRow.Next;
  end;
end;

function TmnrDesignRows.FindName(const vName: string): TmnrDesignCell;
var
  aRow: TmnrDesignRow;
begin
  aRow := First;
  Result := nil;
  while aRow<>nil do
  begin
    Result := aRow.FindName(vName);
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

procedure TmnrProfiler.CopyReport(const vName, vNewName: string);
begin

end;

constructor TmnrProfiler.Create(vReport: TmnrCustomReport);
begin
  inherited Create;
  FReport := vReport;
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

procedure TmnrProfiler.NewReport(const vNewName: string);
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

function TmnrBaseCell.GetAsDouble: Double;
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

procedure TmnrBaseCell.SetAsDouble(const Value: Double);
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

constructor TmnrGroups.Create;
begin
  inherited Create(nil);

end;

function TmnrGroups.CreateLayout(const vGroup: string; vClass: TmnrLayoutClass; const vName: string; vOnRequest: TOnRequest; const vTitle: string; vNumber: Integer; vTag: Integer; vIncludeSections, vExcludeSections: TmnrSectionClassIDs): TmnrLayout;
var
  aLayouts: TmnrLayouts;
begin
  aLayouts := Find(vGroup);
  if aLayouts=nil then
  begin
    aLayouts := Add;
    aLayouts.Name := vGroup;
  end;
  Result := aLayouts.CreateLayout(vClass, vName, vOnRequest, vNumber, vTag, vIncludeSections, vExcludeSections);
  Result.Title := vTitle;
end;

procedure TmnrGroups.CreateRequest(const vName: string; vOnRequest: TOnRequest);
var
  l: TmnrLayout;
begin
  l := FindLayout(vName);
  if l<>nil then
  begin
    if not Assigned(l.OnRequest) then
      l.OnRequest := vOnRequest
  end;
end;

procedure TmnrGroups.DoInitLayouts;
begin
  Report.InitLayouts(Self); //belal: to do remove (self)
end;

function TmnrGroups.EnumLayouts: TmnrLayoutList;
var
  s: TmnrLayouts;
  t: TmnrLayout;
begin
  Result := TmnrLayoutList.Create;
  try
    s := First;
    while s<>nil do
    begin
      t := s.First;
      while t<>nil do
      begin
        Result.Add(t);
        t := t.Next;
      end;
      s := s.Next;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
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
  l: TmnrLayoutList;
  y: TmnrLayout;
begin
  if vName = '' then
    Result := nil
  else
  begin
    l := EnumLayouts;
    try
      for y in l do
      begin
        if SameText(y.Name, vName) or ((Pos('.', vName)=0) and SameText(y.Name+y.Number.ToString, vName)) then //fix for adding number to name 22-11-2022 need back to sametext :)
//        if SameText(y.Name, vName) then
          Exit(y)
      end;
      Result := nil;
    finally
      l.Free;
    end;
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

procedure TmnrGroups.InitLayouts;
begin
  DoInitLayouts;
end;

{ TmnrRowList }

procedure TmnrRowList.AppendRows(vRows: TmnrRows);
var
  r: TmnrRow;
begin
  r := vRows.First;
  while r<>nil do
  begin
    Add(r);
    r := r.Next;
  end;
end;

constructor TmnrRowList.Create;
begin
  inherited Create(False);

end;

{ TmnrLayoutList }

constructor TmnrLayoutList.Create;
begin
  inherited Create(False);

end;

function TmnrLayoutList.FindLayout(vNumber, vData: Integer): TmnrLayout;
begin
  for var itm in Self do
    if (itm.Number=vNumber)and(itm.Data=vData) then
      Exit(itm);

  Result := nil;
end;

function TmnrLayoutList.FindLayout(vNumber: Integer; const vGuid: string): TmnrLayout;
begin
  for var itm in Self do
    if (itm.Number=vNumber)and(itm.Guid=vGuid) then
      Exit(itm);

  Result := nil;
end;

{ TmnrFetch }

procedure TmnrFetch.Clear;
begin
  AcceptMode := acmAccept;
  FetchMode  := fmFirst;
  Reset;
end;

procedure TmnrFetch.Reset;
begin
  ID     := 0;
  Locked := False;
  Data   := nil;
end;

procedure TmnrFetch.Step(var vPos: Integer);
begin
  if FetchMode=fmFirst then
    vPos := 0
  else
    Inc(vPos);
end;

procedure TmnrFetch.Step(var vPos: Int64);
begin
  if FetchMode=fmFirst then
    vPos := 0
  else
    Inc(vPos);
end;

function TmnrFetch.Step(var vPos: Integer; vMax: Integer): Boolean;
begin
  Step(vPos);

  Result := vPos<vMax;

  if not Result then
    AcceptMode := acmEof;
end;

function TmnrFetch.Step(var vPos: Int64; vMax: Int64): Boolean;
begin
  Step(vPos);

  Result := vPos<vMax;

  if not Result then
    AcceptMode := acmEof;
end;

{ TRowsData }

function TRowsData.CreateJsonData: TJSONObject;
begin
  Result := TJSONObject.Create;
  Add(Result);
end;

initialization

finalization

end.

