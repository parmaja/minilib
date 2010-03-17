unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils, Math,
  mnrClasses, mnrLists, mnrNodes;
  //dluxdetails

const
  cMaxRows = 1000;
  cMaxCells = 10;

type

  TReport = class(TmnrCustomReport)
  protected
    SubPos: Integer;
    procedure CreateSections; override;
    procedure DetailsFetch(var vParams: TmnrFetchParams);
  public
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
    procedure TestSpeedBtnClick(Sender: TObject);
    procedure TestReportBtnClick(Sender: TObject);
    procedure TestWriteBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TestReportBtnClick(Sender: TObject);
var
  r: TmnrCustomReport;
  t: Cardinal;
begin
  t := GetTickCount;
  r := TReport.Create;
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
  rep: TReport;
  r: TmnrRowNode;
  i, j: Integer;
  t: Cardinal;
  idx: TmnrLinkNodesListIndex;
begin
  rep := TReport.Create;
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

{ TReport }

procedure TReport.CreateSections;
var
  sec: TmnrSection;
begin
  inherited;
  sec := Sections.RegisterSection('Details', 'ÇáÊÞÑíÑ', sciDetails, ID_SECTION_DETAILS, DetailsFetch);
  with sec.LayoutsRows.Add do
  begin
    sec.AppendDetailTotals := True;
    
    CreateLayout(TmnrIntegerLayout, 'Number', RequestNumber);
    CreateLayout(TmnrDateTimeLayout, 'Data', RequestDate);
    CreateLayout(TmnrTextLayout, 'Name', RequestName);
  //end;
  //with sec.LayoutsRows.Add do
  //begin
    CreateLayout(TmnrTextLayout, 'Code', RequestCode);
    CreateLayout(TmnrCurrencyLayout, 'Value', RequestValue);
  end;
end;

procedure TReport.DetailsFetch(var vParams: TmnrFetchParams);
begin
  with vParams do
  begin
    if Mode=fmFirst then
      SubPos := 0
    else
      Inc(SubPos);
    if SubPos>400 then
      Accepted := acmEof;
  end;
end;

procedure TReport.RequestNumber(vCell: TmnrCustomReportCell);
begin
  vCell.AsInteger := SubPos;
end;

procedure TReport.RequestDate(vCell: TmnrCustomReportCell);
begin
  vCell.AsDateTime := IncDay(Now, RandomRange(-100, 100));
end;

procedure TReport.RequestName(vCell: TmnrCustomReportCell);
begin
  vCell.AsString := Format('Cell %d', [0]);
end;

procedure TReport.RequestCode(vCell: TmnrCustomReportCell);
begin
  vCell.AsString := Format('Row = %d    Col = %d', [vCell.Row.ID, 0]);
end;

procedure TReport.RequestValue(vCell: TmnrCustomReportCell);
begin
  vCell.AsCurrency := RandomRange(1, 1000) / RandomRange(6, 66);
end;

initialization
  Randomize;
  
end.
