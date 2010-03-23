unit designer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  mnrClasses, mnrLists, mnrNodes;

type
  TDesignerForm = class(TForm)
    LayoutsList: TListBox;
    Panel1: TPanel;
    SectionsListBox: TComboBox;
    ListBox2: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FReportDesigner: TCustomReportDesigner;
    procedure SetReportDesigner(const Value: TCustomReportDesigner);
    function GetReport: TmnrCustomReport;
    { Private declarations }
  public
    { Public declarations }
    procedure LoadInfo;
    procedure LoadSections;
    procedure LoadLayouts;
    property Report: TmnrCustomReport read GetReport;
    property ReportDesigner: TCustomReportDesigner read FReportDesigner write SetReportDesigner;
  end;


implementation

{$R *.dfm}

{ TDesignerForm }

procedure TDesignerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TDesignerForm.GetReport: TmnrCustomReport;
begin
  if ReportDesigner<>nil then
    Result := ReportDesigner.Report
  else
    Result := nil;
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
end;

procedure TDesignerForm.SetReportDesigner(const Value: TCustomReportDesigner);
begin
  FReportDesigner := Value;
  LoadInfo;
end;

end.
