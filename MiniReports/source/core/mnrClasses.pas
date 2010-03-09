unit mnrClasses;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs, mnrLists;

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
  TmnrLayoutClass = class of TmnrLayout;
  TmnrNodesRow = class;
  TmnrLayoutsRow = class;


  TmnrSectionLoopWay = (slwSingle, slwMulti);
  TmnrFetchMode = (fmFirst, fmNext);
  TmnrAcceptMode = (acmAccept, acmSkip, acmSkipAll, acmRepeat, acmEof);
  TmnrSectionClassID = (sciReport, sciHeaderReport, sciHeaderPage, sciHeaderDetails, sciDetails, sciFooterDetails, sciFooterPage, sciFooterReport);

  TmnrFetchParams = record
    Mode: TmnrFetchMode;
    Accepted: TmnrAcceptMode;
  end;

  TOnRequest = procedure(vLayout: TmnrLayout; vCell: TmnrCustomReportCell) of object;
  TOnFetch = procedure(var vParams: TmnrFetchParams) of object;

  TmnrCustomReportCell = class(TmnrLinkNode)
  private
    FLayout: TmnrLayout;
    FRow: TmnrNodesRow;
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

  TmnrTextReportCell = class(TmnrReportCell)
  private
    FValue: string;
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
    function GetCells: TmnrReportCells;
    function GetNext: TmnrNodesRow;
    function GetPrior: TmnrNodesRow;
  protected
    function CreateCells: TmnrRowCells; override;
  public
    property Cells: TmnrReportCells read GetCells; //cells in row
    property Next: TmnrNodesRow read GetNext;
    property Prior: TmnrNodesRow read GetPrior;
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
    function GetNext: TmnrLayout;
    function GetPrior: TmnrLayout;
    function GetCells: TmnrLayouts;
  protected
    procedure DoRequest(vCell: TmnrCustomReportCell); virtual;
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; virtual;
  public
    property Next: TmnrLayout read GetNext;
    property Prior: TmnrLayout read GetPrior;

    property Name: string read FName;
    procedure Request(vCell: TmnrCustomReportCell);
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    function NewCell(vRow: TmnrNodesRow): TmnrCustomReportCell;
    property Cells: TmnrLayouts read GetCells;
  end;

  TmnrTextLayout = class(TmnrLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
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
  protected
    function CreateCells: TmnrRowCells; override;
  public
    property Next: TmnrLayoutsRow read GetNext;
    property Prior: TmnrLayoutsRow read GetPrior;
    property Cells: TmnrLayouts read GetCells; //cells in row
    procedure CreateLayout(vClass: TmnrLayoutClass; const vName: string; vRequest: TOnRequest=nil);
  end;

  TmnrLayoutsRows = class(TmnrRowNodes)
  private
    function GetFirst: TmnrLayoutsRow;
    function GetLast: TmnrLayoutsRow;
  public
    function Add: TmnrLayoutsRow;
    property First: TmnrLayoutsRow read GetFirst;
    property Last: TmnrLayoutsRow read GetLast;
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

    property Next: TmnrSection read GetNext;
    property Prior: TmnrSection read GetPrior;
    property Nodes: TmnrSections read GetNodes write SetNodes;
    property Report: TmnrCustomReport read GetReport;

    //function AddLayout
    property LayoutsRows: TmnrLayoutsRows read FLayoutsRows;

    property Name: string read FName;
    property ID: integer read FID;
    property ClassID: TmnrSectionClassID read FClassID;
    property Caption: string read FCaption;
    property LoopWay: TmnrSectionLoopWay read GetLoopWay;
    property OnFetch: TOnFetch read FOnFetch write FOnFetch; 

    procedure FillNow;
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
  
  TmnrCustomReport = class
  private
    FCanceled: Boolean;
    FItems: TmnrNodesRows;
    FSections: TmnrSections;
  protected
    function Canceled: Boolean;
    function HandleNewRow(vRow: TmnrRowNode): Boolean; virtual;
    procedure CreateSections; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Sections: TmnrSections read FSections;
    procedure AddNewRow(vRow: TmnrRowNode);
    property Items: TmnrNodesRows read FItems;
    procedure Cancel;

    procedure Init; virtual;
    procedure Prepare; virtual;
    procedure Finish; virtual;
    procedure Loop;
    procedure Fetch(vSection: TmnrSection; var vParams: TmnrFetchParams); virtual;
    procedure Generate;
    procedure ExportCSV(const vFile: TFileName); overload;
    procedure ExportCSV(const vStream: TStream); overload;
  end;

implementation


{ TmnrCustomReport }

procedure TmnrCustomReport.AddNewRow(vRow: TmnrRowNode);
begin
  if not HandleNewRow(vRow) then
    vRow.Nodes := FItems; 
end;

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
  CreateSections;
end;

procedure TmnrCustomReport.CreateSections;
begin
end;

destructor TmnrCustomReport.Destroy;
begin
  FSections.Free;
  FItems.Free;
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

end;

procedure TmnrCustomReport.Generate;
begin
  Loop;
end;

function TmnrCustomReport.HandleNewRow(vRow: TmnrRowNode): Boolean;
begin
  Result := False;
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

constructor TmnrSection.Create(vNodes: TmnrNodes);
begin
  inherited;
  FSections := TmnrSections.Create(Report);
  FLayoutsRows := TmnrLayoutsRows.Create;
end;

destructor TmnrSection.Destroy;
begin
  FSections.Free;
  FLayoutsRows.Free;
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

procedure TmnrSection.FillNow;
var
  r: TmnrLayoutsRow;
  l: TmnrLayout;
  aRow: TmnrNodesRow;
  c: TmnrCustomReportCell;
begin
  r := LayoutsRows.First;
  while r<>nil do
  begin
    aRow := TmnrNodesRow.Create(nil);
    try
      l := r.Cells.First;
      while l<>nil do
      begin
        c := l.NewCell(aRow);
        l := l.Next;
      end;
    except
      aRow.Free;
      raise;
    end;
    Report.AddNewRow(aRow);
    r := r.Next;
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
  Result := Nodes.Report;
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
  afParams: TmnrFetchParams;
begin
  s := First;
  while s<>nil do
  begin
    case s.LoopWay of
      slwSingle:
      begin
        afParams.Mode := fmFirst;
        s.DoFetch(afParams);
        if afParams.Accepted=acmAccept then
        begin
          s.FillNow;
          s.Sections.Loop;
        end;
      end;
      slwMulti: 
      begin
        afParams.Mode := fmFirst;
        afParams.Accepted := acmAccept;
        while not Report.Canceled and (afParams.Accepted=acmAccept) do
        begin
          s.DoFetch(afParams);
          if afParams.Mode=fmFirst then
            afParams.Mode := fmNext;
          if afParams.Accepted in [acmAccept, acmSkip] then
          begin
            //if s.ClassID=HeaderDetails then
            //  s.add referance row
            //link all details rows to this reference row
            s.FillNow;
            s.Sections.Loop;
          end;
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

{ TmnrLayout }

function TmnrLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := nil;
end;

procedure TmnrLayout.DoRequest(vCell: TmnrCustomReportCell);
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self, vCell);
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
      Result.FRow := vRow;
      Result.FLayout := Self;
      DoRequest(Result);
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

{ TmnrTextReportCell }

function TmnrTextReportCell.GetAsBoolean: Boolean;
begin
  Result := StrToBoolDef(AsString, False);
end;

function TmnrTextReportCell.GetAsCurrency: Currency;
begin
  Result := StrToCurrDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsFloat: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsInteger: Longint;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsString: string;
begin
  Result := FValue;
end;

function TmnrTextReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrTextReportCell.GetIsNull: Boolean;
begin
  Result := FValue<>'';
end;

procedure TmnrTextReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TmnrTextReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := CurrToStr(Value);
end;

procedure TmnrTextReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := DateTimeToStr(Value);
end;

procedure TmnrTextReportCell.SetAsFloat(const Value: Double);
begin
  FValue := FloatToStr(Value);
end;

procedure TmnrTextReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);
end;

procedure TmnrTextReportCell.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TmnrTextReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrTextLayout }

function TmnrTextLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrTextReportCell.Create(vCells);
end;

{ TmnrReportCells }

function TmnrReportCells.Add: TmnrReportCell;
begin
  Result := TmnrTextReportCell.Create(Self);
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

end.
