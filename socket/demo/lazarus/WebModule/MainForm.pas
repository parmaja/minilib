unit MainForm;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

 {
  openssl s_client -connect localhost:443
  openssl s_client -cipher RSA -connect localhost:443 -tls1 -CApath . -servername localhost
 }

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
  mnLogs,
  StdCtrls, ExtCtrls, mnSockets, mnServers, mnWebModules, mnOpenSSL,
  LResources, Buttons, Menus;

type

  { TMain }

  TMain = class(TForm)
    ChallengeSSLChk: TCheckBox;
    Label5: TLabel;
    MakeCertBtn: TButton;
    ModuleNameEdit: TEdit;
    UseSSLChk: TCheckBox;
    ExitBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LastIDLabel: TLabel;
    MainMenu: TMainMenu;
    MaxOfThreadsLabel: TLabel;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    NumberOfThreads: TLabel;
    NumberOfThreadsLbl: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PortEdit: TEdit;
    RootEdit: TEdit;
    StartBtn: TButton;
    StopBtn: TButton;
    StayOnTopChk: TCheckBox;
    procedure ExitBtnClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure MakeCertBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure NumberOfThreadsClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StayOnTopChkChange(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ChallengeServer: TmodWebServer;
    HttpServer: TmodWebServer;
    FMax:Integer;
    procedure Start;
    procedure UpdateStatus;
    procedure ChallengeServerBeforeOpen(Sender: TObject);
    procedure HttpServerBeforeOpen(Sender: TObject);
    procedure HttpServerAfterClose(Sender: TObject);
    procedure HttpServerChanged(Listener: TmnListener);
    procedure ServerLog(const S: String);
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

procedure TMain.StartBtnClick(Sender: TObject);
begin
  Start;
end;

procedure TMain.Start;
begin
  if UseSSLChk.Checked then
    ServerLog('use https://localhost:' + PortEdit.Text + '/doc/')
  else
    ServerLog('use http://localhost:' + PortEdit.Text + '/doc/');
  if ChallengeSSLChk.Checked then
    ChallengeServer.Start;
  HttpServer.Start;
end;

procedure TMain.FormHide(Sender: TObject);
begin
end;

procedure TMain.MakeCertBtnClick(Sender: TObject);
begin
  MakeCert('certificate.pem', 'privatekey.pem', 'PARMAJA', 'PARMAJA TEAM', 'SY', '', 2048, 0, 365);
end;

procedure TMain.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TMain.NumberOfThreadsClick(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TMain.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.StayOnTopChkChange(Sender: TObject);
begin
  if StayOnTopChk.Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TMain.StopBtnClick(Sender: TObject);
begin
  HttpServer.Stop;
  ChallengeServer.Stop;
  StartBtn.Enabled:=true;
  MaxOfThreadsLabel.Caption := '0';
  LastIDLabel.Caption := '0';
end;

procedure TMain.HttpServerBeforeOpen(Sender: TObject);
var
  aRoot:string;
  aWebModule: TmodWebModule;
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;

  aRoot := RootEdit.Text;
  if (LeftStr(aRoot, 2)='.\') or (LeftStr(aRoot, 2)='./') then
    aRoot := ExtractFilePath(Application.ExeName) + Copy(aRoot, 3, MaxInt);

  aWebModule := HttpServer.Modules.Find<TmodWebModule>;
  if aWebModule <> nil then
  begin
    aWebModule.AliasName := ModuleNameEdit.Text;
    aWebModule.DocumentRoot := aRoot;

  end;
  HttpServer.Port := PortEdit.Text;
  HttpServer.CertificateFile := Application.Location + 'fullchain.pem';
  HttpServer.PrivateKeyFile := Application.Location + 'privkey.pem';
  //Server.Address := '127.0.0.1';
  HttpServer.UseSSL := UseSSLChk.Checked;
end;

function FindCmdLineValue(Switch: string; var Value: string; const Chars: TSysCharSet = ['/','-']; Seprator: Char = '='): Boolean;
var
  i, l: Integer;
  s, c, w: string;
begin
  Result := False;
  l := Length(Switch);
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    c := Copy(s, l + 2, 1);
    w := Copy(s, 2, l);
    if (Chars = []) or ((s <> '') and (s[1] in Chars)) then
      if (w = Switch) and ((c = '') or (c = Seprator)) then
      begin
        Value := Copy(S, l + 3, Maxint);
        Result := True;
        break;
      end;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  aIni:TIniFile;
  function GetOption(AName, ADefault: string): string; overload;
  var
    s:string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result :=AnsiDequotedStr(s, '"')
    else
      Result := aIni.ReadString('options', AName, ADefault);
  end;

  function GetOption(AName: string; ADefault: Boolean): Boolean; overload;
  begin
    Result := aIni.ReadBool('options', AName, ADefault);
  end;

  function GetSwitch(AName, ADefault: string): string; overload; //if found in cmd mean it is true
  var
    s:string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := 'True'
    else
      Result := aIni.ReadString('options',AName, ADefault);
  end;

var
  aAutoRun:Boolean;
begin
  InstallEventLog(ServerLog);
  ChallengeServer := TmodWebServer.Create;
  ChallengeServer.OnLog := ServerLog;
  ChallengeServer.OnBeforeOpen := ChallengeServerBeforeOpen;

  HttpServer := TmodWebServer.Create;
  HttpServer.OnBeforeOpen := HttpServerBeforeOpen;
  HttpServer.OnAfterClose := HttpServerAfterClose;
  HttpServer.OnChanged :=  HttpServerChanged;
  HttpServer.OnLog := ServerLog;
  HttpServer.Logging := True;

  aIni := TIniFile.Create(Application.Location + 'config.ini');
  try
    RootEdit.Text := GetOption('root', '.\html');
    PortEdit.Text := GetOption('port', '81');
    ModuleNameEdit.Text := GetOption('module', 'doc');
    UseSSLChk.Checked := GetOption('ssl', false);
    ChallengeSSLChk.Checked := GetOption('challenge', False);
    aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aIni.Free;
  end;

  if aAutoRun then
  begin
    Start;
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aIni:TIniFile;
begin
  aIni := TIniFile.Create(Application.Location+'config.ini');
  try
    aIni.WriteString('options', 'root', RootEdit.Text);
    aIni.WriteString('options', 'port', PortEdit.Text);
    aIni.WriteString('options', 'module', ModuleNameEdit.Text);
    aIni.WriteBool('options', 'ssl', UseSSLChk.Checked);
    aIni.WriteBool('options', 'challenge', ChallengeSSLChk.Checked);
  finally
    aIni.Free;
  end;
  FreeAndNil(HttpServer);
  FreeAndNil(ChallengeServer);
  UninstallEventLog(ServerLog);
end;

procedure TMain.UpdateStatus;
begin
  NumberOfThreads.Caption := IntToStr(HttpServer.Listener.Count);
  LastIDLabel.Caption := IntToStr(HttpServer.Listener.LastID);
end;

procedure TMain.ChallengeServerBeforeOpen(Sender: TObject);
var
  aWebModule: TmodWebModule;
begin
  aWebModule := ChallengeServer.Modules.Find<TmodWebModule>;
  if aWebModule <> nil then
  begin
   //.well-known/acme-challenge/
    aWebModule.AliasName := '';//'.well-known';
    aWebModule.DocumentRoot := Application.Location + 'cert';
  end;
  ChallengeServer.UseSSL := False;
  ChallengeServer.Port := '80';
end;

procedure TMain.HttpServerAfterClose(Sender: TObject);
begin
	StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMain.HttpServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;
  MaxOfThreadsLabel.Caption:=IntToStr(FMax);
  UpdateStatus;
end;

procedure TMain.ServerLog(const S: String);
begin
  Memo.Lines.Add(s);
end;

end.
