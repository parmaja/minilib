unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils, Math,
  mnrClasses, mnrLists, mnrNodes;
  //dluxdetails dluxdesign

const
  cMaxRows = 1000;
  cMaxCells = 10;

type

  TReportDesigner = class(TCustomReportDesigner)
  public
    function CreateDesigner: TComponent; override;

  end;

  TSimpleDetailsReport = class(TmnrCustomReport)
  protected
    BigPos, SubPos: Integer;
    HeaderDeatils, Details: TmnrSection;
    procedure CreateSections(vSections: TmnrSections); override;
    procedure CreateLayouts(vLayouts: TmnrLayouts); override;
    procedure DetailsFetch(var vParams: TmnrFetchParams);
    procedure HeadersFetch(var vParams: TmnrFetchParams);
  public
    procedure RequestMaster(vCell: TmnrCustomReportCell);
    procedure RequestNumber(vCell: TmnrCustomReportCell);
    procedure RequestDate(vCell: TmnrCustomReportCell);
    procedure RequestName(vCell: TmnrCustomReportCell);
    procedure RequestCode(vCell: TmnrCustomReportCell);
    procedure RequestValue(vCell: TmnrCustomReportCell);
  end;

  TForm1 = class(TForm)
    TestSpeedBtn: TButton;
    TestReportBtn: TButton;
    TestWriteBtn: TButton;
    DesignReportBtn: TButton;
    procedure TestSpeedBtnClick(Sender: TObject);
    procedure TestReportBtnClick(Sender: TObject);
    procedure TestWriteBtnClick(Sender: TObject);
    procedure DesignReportBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  designer;

{$R *.dfm}

procedure TForm1.DesignReportBtnClick(Sender: TObject);
begin
  DesignReport(TSimpleDetailsReport);
end;

procedure TForm1.TestReportBtnClick(Sender: TObject);
var
  r: TmnrCustomReport;
  t: Cardinal;
begin
  t := GetTickCount;
  r := TSimpleDetailsReport.Create;
  try
    r.Generate;
    r.ExportCSV('c:\1.csv');
    ShowMessage('Create in '+IntToStr(GetTickCount-t));
    t := GetTickCount;
  finally
    r.Free;
  end;
  ShowMessage('Free in '+IntToStr(GetTickCount-t));
end;

procedure TForm1.TestSpeedBtnClick(Sender: TObject);
var
  rs: TmnrRowNodes;
  r: TmnrRowNode;
  i, j: Integer;
  t: Cardinal;
  idx: TmnrLinkNodesListIndex;
begin
  rs := TmnrRowNodes.Create;
  t := GetTickCount;
  try
    for I := 0 to cMaxRows - 1 do
    begin
      r := rs.Add;
      for j:= 0 to cMaxCells - 1 do
      begin
        r.Cells.Add;
      end;
    end;
    ShowMessage(Format('Create %d node in %d ms', [cMaxRows*cMaxCells, GetTickCount-t]));

    t := GetTickCount;
    idx := TmnrLinkNodesListIndex.Create(rs);
    try
      ShowMessage(Format('Create index in %d ms', [GetTickCount-t]));
    finally
      t := GetTickCount;
      idx.Free;
      ShowMessage(Format('Free index in %d ms', [GetTickCount-t]));
    end;

  finally
    t := GetTickCount;
    rs.Free;
    ShowMessage(Format('Free %d node in %d ms', [cMaxRows*cMaxCells, GetTickCount-t]));
  end;
end;

procedure TForm1.TestWriteBtnClick(Sender: TObject);
var
  rep: TSimpleDetailsReport;
  r: TmnrRowNode;
  i, j: Integer;
  t: Cardinal;
  idx: TmnrLinkNodesListIndex;
begin
  rep := TSimpleDetailsReport.Create;
  t := GetTickCount;
  try
    for I := 0 to cMaxRows - 1 do
    begin
      r := rep.Items.Add;
      for j:= 0 to cMaxCells - 1 do
      begin
        r.Cells.Add;
      end;
    end;
    ShowMessage(Format('Create %d node in %d ms', [cMaxRows*cMaxCells, GetTickCount-t]));

    t := GetTickCount;
    idx := TmnrLinkNodesListIndex.Create(rep.Items);
    try
      ShowMessage(Format('Create index in %d ms', [GetTickCount-t]));
    finally
      t := GetTickCount;
      idx.Free;
      ShowMessage(Format('Free index in %d ms', [GetTickCount-t]));
    end;

  finally
    t := GetTickCount;
    rep.Free;
    ShowMessage(Format('Free %d node in %d ms', [cMaxRows*cMaxCells, GetTickCount-t]));
  end;
end;

{ TSimpleDetailsReport }

procedure TSimpleDetailsReport.CreateLayouts(vLayouts: TmnrLayouts);
begin
  inherited;
  with vLayouts do
  begin
    CreateLayout(TmnrIntegerLayout, 'Master', 'ÇáãÊÓáÓ');
    CreateLayout(TmnrTextLayout, 'Name', 'ÇáÇÓã');
    CreateLayout(TmnrIntegerLayout, 'Number', 'ÇáÑÞã');
    CreateLayout(TmnrDateTimeLayout, 'Date', 'ÇáÊÇÑíÎ');
    CreateLayout(TmnrTextLayout, 'Code', 'ÇáÑãÒ');
    CreateLayout(TmnrCurrencyLayout, 'Value', 'ÇáÞíãÉ');
  end;
end;

procedure TSimpleDetailsReport.CreateSections(vSections: TmnrSections);
begin
  inherited;
  HeaderDeatils := vSections.RegisterSection('HeaderDetails', 'ÑÇÓ ÇáÊÞÑíÑ', sciHeaderDetails, ID_SECTION_HEADERREPORT, HeadersFetch);
  Details := HeaderDeatils.Sections.RegisterSection('Details', 'ÇáÊÞÑíÑ', sciDetails, ID_SECTION_DETAILS, DetailsFetch);

  with HeaderDeatils.LayoutsRows.Add do
  begin
    CreateLayout(TmnrIntegerLayout, 'Master', RequestMaster);
  end;

  with Details.LayoutsRows.Add do
  begin
    //Details.AppendTotals := True;
    Details.AppendSummary := True;

    CreateLayout(TmnrTextLayout, 'Name', RequestName);
    CreateLayout(TmnrIntegerLayout, 'Number', RequestNumber);
    CreateLayout(TmnrDateTimeLayout, 'Date', RequestDate);
  //end;
  //with sec.LayoutsRows.Add do
  //begin
    CreateLayout(TmnrTextLayout, 'Code', RequestCode);
    CreateLayout(TmnrCurrencyLayout, 'Value', RequestValue);
  end;
end;

procedure TSimpleDetailsReport.DetailsFetch(var vParams: TmnrFetchParams);
begin
  with vParams do
  begin
    if Mode=fmFirst then
      SubPos := 0
    else
      Inc(SubPos);
    if SubPos>6 then
      Accepted := acmEof;
  end;
end;

procedure TSimpleDetailsReport.HeadersFetch(var vParams: TmnrFetchParams);
begin
  with vParams do
  begin
    if Mode=fmFirst then
      BigPos := 0
    else
      Inc(BigPos);
    if BigPos>3 then
      Accepted := acmEof;
  end;
end;

procedure TSimpleDetailsReport.RequestNumber(vCell: TmnrCustomReportCell);
begin
  vCell.AsInteger := SubPos;
end;

procedure TSimpleDetailsReport.RequestDate(vCell: TmnrCustomReportCell);
begin
  vCell.AsDateTime := IncDay(Now, RandomRange(-100, 100));
end;

procedure TSimpleDetailsReport.RequestMaster(vCell: TmnrCustomReportCell);
begin
  vCell.AsInteger := BigPos;
end;

procedure TSimpleDetailsReport.RequestName(vCell: TmnrCustomReportCell);
begin
  vCell.AsString := Format('Cell %d', [0]);
end;

procedure TSimpleDetailsReport.RequestCode(vCell: TmnrCustomReportCell);
begin
  vCell.AsString := Format('Row = %d    Col = %d', [vCell.Row.ID, 0]);
end;

procedure TSimpleDetailsReport.RequestValue(vCell: TmnrCustomReportCell);
begin
  vCell.AsCurrency := RandomRange(1, 1000) / RandomRange(6, 66);
end;

{ TReportDesigner }

function TReportDesigner.CreateDesigner: TComponent;
var
  f: TDesignerForm;
begin
  f := TDesignerForm.Create(nil);
  f.ReportDesigner := Self;
  f.Show;
  Result := f;
end;

initialization
  Randomize;
  SetReportDesignerClass(TReportDesigner);

end.
