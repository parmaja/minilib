unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IniFiles, StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  mnOpenSSLUtils, mnOpenSSL, mnLogs, mnHttpClient, mnConnections, mnSockets, mnServers, mnWebModules, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TMainForm = class(TForm)
    StartBtn: TButton;
    Memo: TMemo;
    StopBtn: TButton;
    RootEdit: TEdit;
    RootLbl: TLabel;
    PortLbl: TLabel;
    PortEdit: TEdit;
    UseSSLChk: TCheckBox;
    procedure StartBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FMax:Integer;
    Server: TmodWebServer;
    procedure ModuleServerBeforeOpen(Sender: TObject);
    procedure ModuleServerAfterClose(Sender: TObject);
    procedure ModuleServerChanged(Listener: TmnListener);
    procedure ModuleServerLog(const S: string);
    procedure UpdateStatus;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
var
  aIni: TIniFile;
begin
  Server := TmodWebServer.Create;
  Server.OnBeforeOpen := ModuleServerBeforeOpen;
  Server.OnAfterClose := ModuleServerAfterClose;
  Server.OnChanged :=  ModuleServerChanged;
  Server.OnLog := ModuleServerLog;
  Server.Logging := True;

  if not FileExists('certificate.pem') then
    MakeCert('certificate.pem', 'privatekey.pem', 'Creative Solutions', 'Creative Solutions', 'SY', '', 2048, 0, 1);

  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    RootEdit.Text := aIni.ReadString('Options', 'root', '.\html');
    PortEdit.Text := aIni.ReadString('Options', 'port', '81');
    UseSSLChk.IsChecked := aIni.ReadBool('Options', 'ssl', false);
    //StayOnTopChk.Checked := aIni.ReadBool('Options', 'on_top', false);
    //aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aIni.Free;
  end;
end;

procedure TMainForm.StartBtnClick(Sender: TObject);
begin
  Server.Start;

end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  Server.Stop;
end;

procedure TMainForm.UpdateStatus;
begin
  ModuleServerLog(Format('Connected: %s, Max: %d, Count: %d, LastID: %d', [BoolToStr(Server.Connected, True), FMax, Server.Listener.Count, Server.Listener.LastID]))
end;

procedure TMainForm.ModuleServerBeforeOpen(Sender: TObject);
var
  aRoot:string;
begin
  aRoot := RootEdit.Text;

  if (LeftStr(aRoot, 2)='.\') or (LeftStr(aRoot, 2)='./') then
    aRoot := ExtractFilePath(ParamStr(0)) + Copy(aRoot, 3, MaxInt);

  Server.WebModule.DocumentRoot := aRoot;
  Server.Port := PortEdit.Text;
  Server.UseSSL := UseSSLChk.IsChecked;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  aIni: TIniFile;
begin
  {aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    aIni.WriteString('Options', 'root', RootEdit.Text);
    aIni.WriteString('Options', 'port', PortEdit.Text);
    aIni.WriteBool('Options', 'ssl', UseSSLChk.IsChecked);

    //StayOnTopChk.Checked := aIni.ReadBool('Options', 'on_top', false);
    //aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aIni.Free;
  end;}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkF9 then
  begin
    if Server.Active then
      Server.Stop
    else
      Server.Start;
  end;

end;

procedure TMainForm.ModuleServerAfterClose(Sender: TObject);
begin
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMainForm.ModuleServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;

  StartBtn.Enabled := not Server.Connected;
  StopBtn.Enabled := Server.Connected;
  UpdateStatus;
end;

procedure TMainForm.ModuleServerLog(const s: string);
begin
  Memo.Lines.Add(s);
end;

end.
