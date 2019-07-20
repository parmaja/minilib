unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, IniFiles,
  mnStreams, mnCommClasses, mnCommStreams, mnCommThreads, GPSUtils, GPSThreads;

type
  { TForm1 }
  TForm1 = class(TForm)
    PortEdit: TComboBox;
    ConvertBtn1: TButton;
    DecimalEdit: TEdit;
    DMEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    DMLbl: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    BaudRateEdit: TPanel;
    StartBtn: TButton;
    Label1: TLabel;
    ConvertBtn: TButton;
    StopBtn: TButton;
    GPSTimer: TTimer;
    procedure ConvertBtn1Click(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GPSTimerTimer(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure SendTimerTimer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FCancel: Boolean;
    FCommStream: TmnCommStream;
    FThread : TmnGPSThread;
    procedure CancelThread;
  public

  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.SendTimerTimer(Sender: TObject);
begin
end;

procedure TForm1.StopBtnClick(Sender: TObject);
begin
  CancelThread;
  GPSTimer.Enabled:= False;
end;

procedure TForm1.CancelThread;
begin
  FCancel := True;
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil;
    FCommStream := nil;
  end;
end;

procedure TForm1.StartBtnClick(Sender: TObject);
begin
  if FThread = nil then
  begin
    FCommStream := TmnCommStream.Create(True, PortEdit.Caption, StrToInt(BaudRateEdit.Caption));
    FCommStream.ReadTimeout := 10;
    FCommStream.ReadTimeoutConst := 100; //or use QueMode
//    FCommStream.QueMode := True;
    FCommStream.ConnectMode := ccmRead;
    FCommStream.Connect;
    FThread := TmnGPSThread.Create(True, FCommStream);
    FThread.EndOfLine := #13;
    FThread.FreeOnTerminate := True;
    FThread.Resume;
  end;
  GPSTimer.Enabled := True;
end;

procedure TForm1.MemoChange(Sender: TObject);
begin

end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CancelThread;
end;

procedure TForm1.GPSTimerTimer(Sender: TObject);
var
  Info:TGPSInfo;
begin
  Info := GPSInfo;
  if Info.Source <> '' then
  begin
    ListBox1.Items.Add(Info.Source);
    ListBox1.Items.Add(GPSToMAP(Info.Decimal));
    ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TForm1.ConvertBtnClick(Sender: TObject);
begin
  DMEdit.Text := GPSDecimalToDM(StrToFloat(DecimalEdit.Text));
end;

procedure TForm1.ConvertBtn1Click(Sender: TObject);
var
  Info :TGPSInfo;
begin
  GPSPrase('$GPGGA,204852.000,4038.0022,N,07401.2578,W,1,6,1.64,31.7,M,-34.3,M,,*5B', Info);
  DecimalEdit.Text := Info.Source;
  DMEdit.Text := GPSToMAP(Info.Decimal);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  IniFile:TIniFile;
begin
  IniFile:=TIniFile.Create(Application.Location + 'Options.ini');
  PortEdit.Caption:=IniFile.ReadString('Options', 'Port', 'COM6');
  BaudRateEdit.Caption:=IniFile.ReadString('Options', 'BaudRate', '4800');
end;

initialization
  {$I MainForms.lrs}
end.

