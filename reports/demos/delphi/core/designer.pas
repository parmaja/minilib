unit designer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  mnrClasses, mnrLists, mnrNodes;

type
  TDesignerForm = class(TForm, ImnrReportDesigner)
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
    FReport: TmnrCustomReport;

    function GetReport: TmnrCustomReport;
  protected

    procedure DesignReport(vReport: TmnrCustomReport);
    procedure UpdateView(vCell: TmnrDesignCell = nil);
    procedure ProcessDrop(vNode: TmnrLayout);
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
    destructor Destroy; override;

    property Report: TmnrCustomReport read GetReport;

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

procedure TDesignerForm.DesignReport(vReport: TmnrCustomReport);
begin
  FReport := vReport;
  try
    //FReport.Prepare;
    FReport.Designer := Self;
    //FReport.Load;
  except
    FreeAndNil(FReport);
    raise;
  end;
  LoadInfo;
  Show;
end;

destructor TDesignerForm.Destroy;
begin
  FreeAndNil(FReport);
  inherited;
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
  Result := FReport;
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

    c := TmnrDesignCell.Create(Section.DesignRows.First);
    c.Name := Layout.Name;
    c.Layout := Layout;
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
  g: TmnrLayouts;
begin
  g := Report.Groups.First;

  while g<>nil do
  begin
    l := g.First;
    while l<>nil do
    begin
      LayoutsList.Items.AddObject(l.Title, l);
      l := l.Next;
    end;
    g := g.Next;
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

procedure TDesignerForm.FillSection;
var
  c: TmnrDesignCell;
begin
  CellsListBox.Items.BeginUpdate;
  try
    CellsListBox.Clear;
    if (Section<>nil)and(Section.DesignRows.First<>nil) then
    begin
      c := Section.DesignRows.First.First;
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

procedure TDesignerForm.ProcessDrop(vNode: TmnrLayout);
begin

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

procedure TDesignerForm.UpdateView(vCell: TmnrDesignCell);
begin

end;

procedure TDesignerForm.WidthEditChange(Sender: TObject);
begin
  if Cell<>nil then
    Cell.Width := StrToIntDef(WidthEdit.Text, 0);
end;

end.
