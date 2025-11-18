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
  mnLogs, mnUtils, rtti,
  StdCtrls, ExtCtrls, mnSockets, mnServers, mnOpenSSL, mnBootstraps,
  mnModules, mnWebModules, mnWebElements, HomeModules,
  LResources, Buttons, Menus;

type

  { TMain }

  TMain = class(TForm)
    HomeAliasEdit: TEdit;
    ChallengeSSLChk: TCheckBox;
    KeepAliveChk: TCheckBox;
    Label5: TLabel;
    HomeLbl: TLabel;
    Label6: TLabel;
    MakeCertBtn: TButton;
    DocAliasEdit: TEdit;
    AutoRunChk: TCheckBox;
    BindEdit: TEdit;
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
    HomePathEdit: TEdit;
    StartBtn: TButton;
    StopBtn: TButton;
    StayOnTopChk: TCheckBox;
    CompressChk: TCheckBox;
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
    CertPassword: string;
    CertFile: string;
    PrivateKeyFile: string;
    ChallengeServer: TmodWebServer;
    HttpServer: TmodWebServer;
    FMax:Integer;
    WebServers: TWebServers;
    LogMessages: Boolean;
    procedure Start;
    procedure UpdateStatus;

    procedure HttpServerBeforeOpen(Sender: TObject);
    procedure HttpServerAfterOpen(Sender: TObject);
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
  ChallengeServer.Enabled := UseSSLChk.Checked and ChallengeSSLChk.Checked;

  WebServers.Start;
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
  WebServers.Stop;
  StartBtn.Enabled:= true;
  MaxOfThreadsLabel.Caption := '0';
  LastIDLabel.Caption := '0';
end;

procedure TMain.HttpServerBeforeOpen(Sender: TObject);
var
  aAppPath, aHomePath: string;
  aDocModule: TmodWebModule;
  aHomeModule: THomeModule;
begin
  aHomePath := IncludePathDelimiter(HomePathEdit.Text);
  if (LeftStr(aHomePath, 2)='.\') or (LeftStr(aHomePath, 2) = './') then
    aHomePath := IncludePathDelimiter(ExtractFilePath(Application.ExeName) + Copy(aHomePath, 3, MaxInt));

  HttpServer.Bind := BindEdit.Text;
  HttpServer.Port := PortEdit.Text;
  if UseSSLChk.Checked then
  begin
    //HttpServer.CertificateFile := Application.Location + '\acme\fullchain.pem';
    //HttpServer.PrivateKeyFile := Application.Location + '\acme\privkey.pem';
    HttpServer.UseSSL := True;
    HttpServer.CertificateFile := CertFile;
    HttpServer.CertPassword := CertPassword;
    HttpServer.PrivateKeyFile := PrivateKeyFile;
  end;
  //Server.Address := '127.0.0.1';

  aDocModule := HttpServer.Modules.Find<TmodWebFileModule>;
  if aDocModule <> nil then
  begin
    aDocModule.AliasName := DocAliasEdit.Text;
    aDocModule.HomePath := aHomePath;
    (aDocModule as TmodWebFileModule).ServeFiles:= [serveEnabled, serveIndex, serveDefault, serveSmart];
    //aDocModule.Use.AcceptCompressing := True;
    if CompressChk.Checked then
      aDocModule.UseCompressing := ovUndefined
    else
      aDocModule.UseCompressing := ovNo;
    aDocModule.UseKeepAlive.AsBoolean := KeepAliveChk.Checked;
    //HttpServer.SetFallbackRedirect('/'+aDocModule.AliasName+'/');
    HttpServer.SetNotfound;
  end;

  aHomeModule := HttpServer.Modules.Find<THomeModule>;
  if aHomeModule <> nil then
  begin
    aHomeModule.AliasName := HomeAliasEdit.Text;
    aAppPath := ExtractFilePath(Application.ExeName);

    //aHomeModule.IsSSL := HttpServer.UseSSL;
    //aHomeModule.Domain := 'localhost';
    //aHomeModule.Port := HttpServer.Port;
    aHomeModule.WebApp.IsLocal := True;
//    aHomeModule.AssetsURL := '/' + aHomeModule.AliasName + '/assets/';
    aHomeModule.HomePath := aHomePath;
    aHomeModule.WorkPath := aAppPath;

    aHomeModule.WebApp.IsSSL := HttpServer.UseSSL;
    aHomeModule.WebApp.AppPath := Application.Location;
    //aHomeModule.WebApp.Assets.Logo.LoadFromFile(aHomeModule.HomePath + 'cs-v2.png');
    aHomeModule.WebApp.Assets.Logo.LoadFromFile(aHomeModule.HomePath + 'cs.svg');

    ForceDirectories(aHomeModule.WorkPath + 'cache');
    ForceDirectories(aHomeModule.WorkPath + 'temp');

    if CompressChk.Checked then
      aHomeModule.UseCompressing := ovUndefined
    else
      aHomeModule.UseCompressing := ovNo;
    aHomeModule.UseKeepAlive.AsBoolean := KeepAliveChk.Checked;
  end;
end;

procedure TMain.HttpServerAfterOpen(Sender: TObject);
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
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

  function GetOption(AName: string; ADefault: Boolean = False): Boolean; overload;
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
  aBounds: TRect;
begin
  WebServers := TWebServers.Create;
  InstallEventLog(ServerLog);

  ChallengeServer := TmodWebServer.Create;
  ChallengeServer.AddChallengeAcme(ExtractFilePath(ParamStr(0)) + 'acme\.well-known\');
  ChallengeServer.AddRedirectHttps;
  ChallengeServer.Bind:= BindEdit.Text;

  ChallengeServer.OnLog := ServerLog;
  ChallengeServer.Logging := LogMessages;
  WebServers.AddServer('ChallengeServer', ChallengeServer);

  HttpServer := TmodWebServer.Create;
  HttpServer.OnBeforeOpen := HttpServerBeforeOpen;
  HttpServer.OnAfterOpen :=  HttpServerAfterOpen;
  HttpServer.OnAfterClose := HttpServerAfterClose;
  HttpServer.OnChanged :=  HttpServerChanged;
  HttpServer.OnLog := ServerLog;
  HttpServer.Logging := LogMessages;

  WebServers.AddServer('HttpServer', HttpServer);

  HttpServer.Modules.Add(TmodWebFileModule.Create('doc', 'doc'));
  HttpServer.Modules.Add(THomeModule.Create('home', 'home'));
  //HttpServer.SetFallbackRedirect('/doc/');
  HttpServer.SetNotfound;

  aIni := TIniFile.Create(Application.Location + 'config.ini');
  try
    HomePathEdit.Text := GetOption('homepath', '.\html');
    PortEdit.Text := GetOption('port', '81');
    DocAliasEdit.Text := GetOption('doc.alias', 'doc');
    HomeAliasEdit.Text := GetOption('home.alias', 'home');
    BindEdit.Text := GetOption('bind', '0.0.0.0');
    UseSSLChk.Checked := GetOption('ssl', false);
    CompressChk.Checked := GetOption('compress', false);
    KeepAliveChk.Checked := GetOption('keep-alive', false);
    ChallengeSSLChk.Checked := GetOption('challenge', False);
    CertPassword := GetOption('cert_password', '');
    CertFile := CorrectPath(ExpandToPath(GetOption('certificate', './certificate.pem'), Application.Location));
    PrivateKeyFile := CorrectPath(ExpandToPath(GetOption('privatekey', './privatekey.pem'), Application.Location));
    aBounds.Left := aIni.ReadInteger('window', 'left', Left);
    aBounds.Top := aIni.ReadInteger('window', 'top', Top);
    aBounds.Width := aIni.ReadInteger('window', 'width', Width);
    aBounds.Height := aIni.ReadInteger('window', 'height', Height);
    BoundsRect := aBounds;
    StayOnTopChk.Checked := aIni.ReadBool('window', 'ontop', StayOnTopChk.Checked);
    AutoRunChk.Checked := aIni.ReadBool('window', 'autorun', AutoRunChk.Checked);
    LogMessages := GetOption('log');
  finally
    aIni.Free;
  end;

  if AutoRunChk.Checked then
    Start;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aIni:TIniFile;
begin
  aIni := TIniFile.Create(Application.Location + 'config.ini');
  try
    aIni.WriteInteger('window', 'top', Top);
    aIni.WriteInteger('window', 'left', Left);
    aIni.WriteInteger('window', 'width', Width);
    aIni.WriteInteger('window', 'Height', Height);
    aIni.WriteBool('window', 'ontop', StayOnTopChk.Checked);
    aIni.WriteBool('window', 'autorun', AutoRunChk.Checked);

    aIni.WriteString('options', 'homepath', HomePathEdit.Text);
    aIni.WriteString('options', 'doc.alias', DocAliasEdit.Text);
    aIni.WriteString('options', 'home.alias', HomeAliasEdit.Text);
    aIni.WriteString('options', 'bind', BindEdit.Text);
    aIni.WriteString('options', 'port', PortEdit.Text);
    aIni.WriteBool('options', 'ssl', UseSSLChk.Checked);
    aIni.WriteBool('options', 'compress', CompressChk.Checked);
    aIni.WriteBool('options', 'keep-alive', KeepAliveChk.Checked);
    aIni.WriteBool('options', 'challenge', ChallengeSSLChk.Checked);
  finally
    aIni.Free;
  end;
  FreeAndNil(WebServers);
  UninstallEventLog(ServerLog);
end;

procedure TMain.UpdateStatus;
begin
  NumberOfThreads.Caption := IntToStr(HttpServer.Listener.Count);
  LastIDLabel.Caption := IntToStr(HttpServer.Listener.LastID);
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
