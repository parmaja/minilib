unit designer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  mnrClasses, mnrLists, mnrNodes;

type
  TDesignerForm = class(TForm)
    LayoutsList: TListBox;
    SaveBtn: TButton;
    CellsListBox: TListBox;
    SectionsListBox: TComboBox;
    DeleteBtn: TButton;
    WidthEdit: TLabeledEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveBtnClick(Sender: TObject);
    procedure SectionsListBoxClick(Sender: TObject);
    procedure LayoutsListDblClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure CellsListBoxClick(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
  private
    FReportDesigner: TCustomReportDesigner;
    procedure SetReportDesigner(const Value: TCustomReportDesigner);
    function GetReport: TmnrCustomReport;
  protected
    procedure SetName(const NewName: TComponentName); override;
    { Private declarations }
  public
    { Public declarations }
    procedure LoadInfo;
    procedure LoadSections;
    procedure FillSection;
    procedure LoadLayouts;
    function Section: TmnrSection;
    function Layout: TmnrLayout;
    function Cell: TmnrDesignCell;

    property Report: TmnrCustomReport read GetReport;
    property ReportDesigner: TCustomReportDesigner read FReportDesigner write SetReportDesigner;
  end;


implementation

{$R *.dfm}

{ TDesignerForm }

function TDesignerForm.Cell: TmnrDesignCell;
begin
  if CellsListBox.ItemIndex<>-1 then
    Result := TmnrDesignCell(CellsListBox.Items.Objects[CellsListBox.ItemIndex])
  else
    Result := nil;
end;

procedure TDesignerForm.CellsListBoxClick(Sender: TObject);
begin
  if Cell<>nil then
    WidthEdit.Text := Format('%d', [Cell.Width]);
end;

procedure TDesignerForm.DeleteBtnClick(Sender: TObject);
var
  idx: Integer;
begin
  if Cell<>nil then
  begin
    idx := CellsListBox.ItemIndex;
    Cell.Free;
    FillSection;

    if idx<=1 then
      CellsListBox.ItemIndex := 0
    else
      CellsListBox.ItemIndex := idx-1;
  end;
end;

procedure TDesignerForm.FillSection;
var
  c: TmnrDesignCell;
begin
  CellsListBox.Items.BeginUpdate;
  try
    CellsListBox.Clear;
    if (Section<>nil)and(Section.DesignRows.First<>nil) then
    begin
      c := Section.DesignRows.First.Cells.First;
      while c<>nil do
      begin
        CellsListBox.Items.AddObject(c.Layout.Title, c);
        c := c.Next;
      end;
    end;
  finally
    CellsListBox.Items.EndUpdate;
  end;
end;

procedure TDesignerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TDesignerForm.GetReport: TmnrCustomReport;
begin
  {if ReportDesigner<>nil then
    Result := ReportDesigner.Report
  else
    Result := nil;}
  Result := TmnrCustomReport(Tag);
end;

function TDesignerForm.Layout: TmnrLayout;
begin
  if LayoutsList.ItemIndex<>-1 then
    Result := TmnrLayout(LayoutsList.Items.Objects[LayoutsList.ItemIndex])
  else
    Result := nil;
end;

procedure TDesignerForm.LayoutsListDblClick(Sender: TObject);
var
  c: TmnrDesignCell;
begin
  if (Layout<>nil) and (Section<>nil) then
  begin
    if Section.DesignRows.First=nil then
      Section.DesignRows.Add;
    with Section.DesignRows.First do
    begin
      c := TmnrDesignCell.AutoCreate(Cells, Layout.Name);
      c.Layout := Layout;
    end;
    FillSection;
  end;
end;

procedure TDesignerForm.LoadInfo;
begin
  LoadSections;
  LoadLayouts;
end;

procedure TDesignerForm.LoadLayouts;
var
  l: TmnrLayout;
begin
  l := Report.Layouts.First;
  while l<>nil do
  begin
    LayoutsList.Items.AddObject(l.Title, l);
    l := l.Next;
  end;
end;

procedure TDesignerForm.LoadSections;
  procedure AddSection(s: TmnrSection);
  var
    p: TmnrSection;
  begin
    SectionsListBox.Items.AddObject(s.Caption, s);
    p := s.Sections.First;
    while p<>nil do
    begin
      AddSection(p);
      p := p.Next;
    end;
  end;

var
  p: TmnrSection;
begin
  p := Report.Sections.First;
  while p<>nil do
  begin
    AddSection(p);
    p := p.Next;
  end;
  SectionsListBox.ItemIndex := 0;
  FillSection;
end;

procedure TDesignerForm.SaveBtnClick(Sender: TObject);
begin
  Report.Profiler.SaveReport;
end;

function TDesignerForm.Section: TmnrSection;
begin
  if SectionsListBox.ItemIndex<>-1 then
    Result := TmnrSection(SectionsListBox.Items.Objects[SectionsListBox.ItemIndex])
  else
    Result := nil;
end;

procedure TDesignerForm.SectionsListBoxClick(Sender: TObject);
begin
  FillSection;
end;

procedure TDesignerForm.SetName(const NewName: TComponentName);
begin
  inherited;
  if not SameText('T'+NewName, ClassName) then
  begin
    LoadInfo;
    ShowModal;
  end;
end;

procedure TDesignerForm.SetReportDesigner(const Value: TCustomReportDesigner);
begin
  FReportDesigner := Value;
  LoadInfo;
end;

procedure TDesignerForm.WidthEditChange(Sender: TObject);
begin
  if Cell<>nil then
    Cell.Width := StrToIntDef(WidthEdit.Text, 0);
end;

end.
