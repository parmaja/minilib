unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, IniFiles,
  mnStreams, mnCommClasses, mnCommStreams, mnCommThreads, GPSUtils, GPSThreads;

type

  { TmnGPSThread }

  TmnGPSThread = class(TmnCommThread)
  private
  protected
    procedure StringArrived(S: string); override;
  public
    FFile: TmnStream;
    destructor Destroy; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    LogLbl: TLabel;
    PortEdit: TComboBox;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    BaudEdit: TComboBox;
    StartBtn: TButton;
    Label1: TLabel;
    StopBtn: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FCancel: Boolean;
    FCommStream: TmnCommStream;
    FThread : TmnGPSThread;
    procedure CancelThread;
  public

  end;

var
  LastLogged: string;
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.StopBtnClick(Sender: TObject);
begin
  CancelThread;
  StopBtn.Enabled := False;
  StartBtn.Enabled := True;
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
    FCommStream := TmnCommStream.Create(True, PortEdit.Text, StrToInt(BaudEdit.Text));
    FCommStream.ReadTimeout := 10;
    FCommStream.ReadTimeoutConst := 100; //or use QueMode
//    FCommStream.QueMode := True;
    FCommStream.ConnectMode := ccmRead;
    FCommStream.Connect;
    FThread := TmnGPSThread.Create(True, FCommStream);
    FThread.FFile:=TmnStream.Create(TFileStream.Create(Application.Location + 'gps-' + FormatDateTime('YYYY-MM-DD-HH-NN-SS', Now) + '.txt', fmCreate));
    //FThread.EndOfLine := #13;
    FThread.FreeOnTerminate := True;
    FThread.Resume;
  end;
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CancelThread;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Application.Location + 'Options.ini');
  try
    IniFile.WriteString('Options', 'Port', PortEdit.Text);
    IniFile.ReadString('Options', 'BaudRate', BaudEdit.Text);
  finally
    IniFile.Free;
  end;
  Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Application.Location + 'Options.ini');
  try
    PortEdit.Text := IniFile.ReadString('Options', 'Port', 'COM6');
    BaudEdit.Text := IniFile.ReadString('Options', 'BaudRate', '4800');
  finally
    IniFile.Free;
  end;
end;

{ TmnGPSThread }

procedure TmnGPSThread.StringArrived(S: string);
begin
  LastLogged := S;
  FFile.WriteString(S);
end;

destructor TmnGPSThread.Destroy;
begin
  FreeAndNil(FFile);
  inherited Destroy;
end;

end.

