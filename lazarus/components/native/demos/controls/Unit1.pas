unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Alhamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, Classes, SysUtils, ComCtrls, StdCtrls, ntvPageControls, ntvTabSets, LMessages,
  ntvRegCtrls, ntvProgressBars,
  LCLType, Controls, ExtCtrls, ExtendedNotebook, DividerBevel;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    ntvPageControl1: TntvPageControl;
    ntvPageControl2: TntvPageControl;
    ntvProgressBar1: TntvProgressBar;
    ntvTabSet1: TntvTabSet;
    Panel1: TPanel;
    Panel2: TPanel;
    TestPanel: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ntvPageControl1Click(Sender: TObject);
    procedure ntvPageControl2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    FCount: Integer;
    FPageControl: TntvPageControl;
    procedure CreatePageControl;
  public
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  public
  end;

var
  Form1: TForm1; 

implementation

uses
  ntvTabs, ntvUtils;

{$r *.lfm}

{ TForm1 }

procedure TForm1.ntvPageControl1Click(Sender: TObject);
begin

end;

procedure TForm1.ntvPageControl2Click(Sender: TObject);
begin

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

procedure TForm1.CreatePageControl;
begin
  if FPageControl = nil then
  begin
    FPageControl := TntvPageControl.Create(Self);
    FPageControl.Left := 50;
    FPageControl.Top := 50;
    //FPageControl.HeaderHeight := 30;
    FPageControl.Parent := Self;
    FPageControl.Visible := True;
    FPageControl.ShowTabs := True;
    FPageControl.TabStop := True;
  end;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  inherited;
end;

procedure TForm1.EraseBackground(DC: HDC);
begin
  inherited;
end;

procedure TForm1.Paint;
begin
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FPageControl = nil then
  begin
    FPageControl := TntvPageControl.Create(Self);
    FPageControl.Left := 50;
    FPageControl.Top := 50;
    FPageControl.Parent := Self;
    FPageControl.Visible := True;
    FPageControl.ShowTabs := True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CreatePageControl;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  aPanel: TPanel;
begin
  CreatePageControl;
  aPanel:=TPanel.Create(Self);
  aPanel.Caption := 'Panel ' + IntToStr(FCount);
  aPanel.Name := 'Panel' + IntToStr(FCount);
  aPanel.Parent := FPageControl;
  FPageControl.ItemIndex := 0;
  Inc(FCount);
  aPanel:=TPanel.Create(Self);
  aPanel.Caption := 'Panel ' + IntToStr(FCount);
  aPanel.Name := 'Panel' + IntToStr(FCount);
  aPanel.Parent := FPageControl;
  Inc(FCount);
  aPanel:=TPanel.Create(Self);
  aPanel.Caption := 'Panel ' + IntToStr(FCount);
  aPanel.Name := 'Panel' + IntToStr(FCount);
  aPanel.Parent := FPageControl;
  Inc(FCount);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Tabs: TntvTabs;
  aTab: TntvTabItem;
begin
  Tabs := TntvTabs.Create(TntvTabItem);
  try
    Tabs.BeginUpdate;
    try
      aTab := Tabs.Add;
      aTab.Caption := 'Test' + IntToStr(aTab.Index);

      aTab := Tabs.Add;
      aTab.Caption := 'Test' + IntToStr(aTab.Index);
    finally
      Tabs.EndUpdate;
    end;

    Tabs.Paint(TestPanel.Canvas, TestPanel.ClientRect, []);
  finally
    Tabs.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FreeAndNil(FPageControl);
end;

end.

