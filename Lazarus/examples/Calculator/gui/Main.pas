unit Main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINCE}
  Windows,
  {$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  calculators, ExtCtrls, Menus, StdCtrls;

type

  { TMyCalculator }

  TMyCalculator = class(TCalculator)
  public
    procedure Log(const S: string); override;
    procedure Refresh; override;
  end;

  { TCalcForm }

  TCalcForm = class(TForm)
    Button1: TSpeedButton;
    Button10: TSpeedButton;
    Button11: TSpeedButton;
    Button12: TSpeedButton;
    Button13: TSpeedButton;
    Button14: TSpeedButton;
    Button15: TSpeedButton;
    Button16: TSpeedButton;
    Button17: TSpeedButton;
    Button18: TSpeedButton;
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    DisplayEdit: TPanel;
    LogList: TListBox;
    MainMenu1: TMainMenu;
    MemPnl: TPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpPnl: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure LogListClick(Sender: TObject);
    procedure LogListKeyPress(Sender: TObject; var Key: char);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
  protected

  public
    FCalculator: TMyCalculator;
    procedure Process(s: string);
  end;

var
  CalcForm: TCalcForm;

implementation

{$R *.lfm}

procedure TCalcForm.Button1Click(Sender: TObject);
begin
  Process((Sender as TSpeedButton).Caption)
end;

procedure TCalcForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TCalcForm.Process(s: string);
begin
  FCalculator.Process(s);
end;

procedure TCalcForm.FormCreate(Sender: TObject);
begin
  Caption := 'Lazarus Calculator';//for FindWindow not take the desgin time window
  FCalculator := TMyCalculator.Create;
end;

procedure TCalcForm.FormDestroy(Sender: TObject);
begin
  FCalculator.Free;
end;

procedure TCalcForm.FormHide(Sender: TObject);
begin
  Close;
end;

procedure TCalcForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Process(Key);
end;

procedure TCalcForm.FormResize(Sender: TObject);
begin
  {$IFDEF WINCE}
  ClientHeight := GetSystemMetrics(SM_CYFULLSCREEN) + GetSystemMetrics(SM_CYBORDER) * 2;
  ClientWidth := GetSystemMetrics(SM_CXFULLSCREEN) + GetSystemMetrics(SM_CXBORDER) * 2;
  {$ENDIF}
end;

procedure TCalcForm.LogListClick(Sender: TObject);
begin
end;

procedure TCalcForm.LogListKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0; //can not make the list take the keys
end;

procedure TCalcForm.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TCalcForm.MenuItem2Click(Sender: TObject);
begin
  FCalculator.Process('ON');//mean reset
end;

{ TMyCalculator }

procedure TMyCalculator.Log(const S: string);
begin
  CalcForm.LogList.Items.Add(S);
  CalcForm.LogList.ItemIndex := CalcForm.LogList.Items.Count - 1;
end;

procedure TMyCalculator.Refresh;
begin
  inherited;
  CalcForm.DisplayEdit.Caption := Sign + Number;
  if CurrentOperator = '=' then
    CalcForm.OpPnl.Caption := ''
  else
    CalcForm.OpPnl.Caption := CurrentOperator;
  if HaveMemory then
    CalcForm.MemPnl.Caption := 'M'
  else
    CalcForm.MemPnl.Caption := '';
end;

initialization
end.

