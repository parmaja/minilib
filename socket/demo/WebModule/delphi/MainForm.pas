unit MainForm;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI,
  mnOpenSSLUtils, mnOpenSSL, mnLogs, mnHttpClient, mnOpenSSLAPI,
  mnModules,   mnStreamUtils, mnUtils,
  Registry, IniFiles, StdCtrls, ExtCtrls, mnConnections, mnSockets, mnServers, mnWebModules,
  HomeModules;

type
  TMain = class(TForm)
    Memo: TMemo;
    StartBtn: TButton;
    HomePathEdit: TEdit;
    Label1: TLabel;
    StopBtn: TButton;
    Label2: TLabel;
    PortEdit: TEdit;
    StayOnTopChk: TCheckBox;
    Panel3: TPanel;
    LastIDLabel: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    MaxOfThreadsLabel: TLabel;
    NumberOfThreads: TLabel;
    NumberOfThreadsLbl: TLabel;
    Button1: TButton;
    UseSSLChk: TCheckBox;
    Button2: TButton;
    AliasNameEdit: TEdit;
    Label5: TLabel;
    KeepAliveChk: TCheckBox;
    CompressChk: TCheckBox;
    AutoOpenChk: TCheckBox;
    AutoRunChk: TCheckBox;
    OpenBtn: TButton;
    Button3: TButton;
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure StayOnTopChkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FMax:Integer;
    HttpServer: TmodWebServer;
    procedure OpenURL;
    procedure UpdateStatus;
    procedure HttpServerBeforeOpen(Sender: TObject);
    procedure HttpServerAfterOpen(Sender: TObject);
    procedure HttpServerAfterClose(Sender: TObject);
    procedure HttpServerChanged(Listener: TmnListener);
    procedure HttpServerLog(const S: string);
  public
  end;

var
  Main: TMain;

implementation

{$R *.DFM}

procedure TMain.StartBtnClick(Sender: TObject);
begin
  HttpServer.Start;
end;

procedure TMain.StopBtnClick(Sender: TObject);
begin
  HttpServer.Stop;
  StartBtn.Enabled := true;
end;

procedure TMain.UpdateStatus;
begin
  NumberOfThreads.Caption := IntToStr(HttpServer.Listener.Count);
  LastIDLabel.Caption := IntToStr(HttpServer.Listener.LastID);
end;

procedure TMain.HttpServerBeforeOpen(Sender: TObject);
var
  aHomePath: string;
  aDocModule: TmodWebModule;
  aHomeModule: THomeModule;
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  aHomePath := HomePathEdit.Text;
  if (LeftStr(aHomePath, 2)='.\') or (LeftStr(aHomePath, 2)='./') then
    aHomePath := ExtractFilePath(Application.ExeName) + Copy(aHomePath, 3, MaxInt);

  aHomeModule := HttpServer.Modules.Find<THomeModule>;
  if aHomeModule <> nil then
  begin
    aHomeModule.AliasName := 'home';
    aHomeModule.HomePath := aHomePath;
    aHomeModule.HostURL := 'http://localhost:' + PortEdit.Text;
    aHomeModule.HomeUrl := aHomeModule.HostURL + '/' + aHomeModule.AliasName;
    aHomeModule.CachePath := ExtractFilePath(Application.ExeName) + 'cache';
  end;

  aDocModule := HttpServer.Modules.Find<TmodWebModule>;

  if aDocModule <> nil then
  begin
    aDocModule.HomePath := aHomePath;
    aDocModule.AliasName := AliasNameEdit.Text;
    if KeepAliveChk.Checked then
      aDocModule.UseKeepAlive := klvKeepAlive
    else
      aDocModule.UseKeepAlive := klvUndefined;
    aDocModule.UseCompressing := CompressChk.Checked;
  end;
  HttpServer.Port := PortEdit.Text;

  HttpServer.UseSSL := UseSSLChk.Checked;

  HttpServer.CertificateFile := 'HttpServer.crt';
  HttpServer.PrivateKeyFile := 'HttpServer.private.key';

  //HttpServer.CertificateFile := 'certificate.pem';
  //HttpServer.PrivateKeyFile := 'key.pem';
end;

function FindCmdLineValue(Switch: string; var Value: string; const Chars: TSysCharSet = ['/', '-']; Seprator: Char = ' '; IgnoreCase: Boolean = true): Boolean;
var
  I: Integer;
  S: string;
begin
  Switch := Switch + Seprator;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (Chars = []) or CharInSet(S[1], Chars) then
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Length(Switch)), Switch) = 0) then
        begin
          Result := True;
          Value := Copy(S, Length(Switch) + 2, Maxint);
          Exit;
        end;
      end
      else
      begin
        if (AnsiCompareStr(Copy(S, 2, Length(Switch)), Switch) = 0) then
        begin
          Result := True;
          Value := Copy(S, Length(Switch) + 2, Maxint);
          Exit;
        end;
      end;
  end;
  Result := False;
end;

procedure TMain.Button1Click(Sender: TObject);
var
  s: TsslConfig;
  aCer, aCsr, aPubKey, aPrvKey: string;
begin
  //MakeCert('certificate.pem', 'privatekey.pem', 'Creative Solutions', 'Creative Solutions', 'SY', '', 2048, 0, 1);
  s := TsslConfig.Create;
  try
    s.WriteInteger('req', 'default_bits', 2048);
    s.WriteInteger('req', 'Serial', 0);
    s.WriteInteger('req', 'Days', 300);
    s.WriteString('req', 'emailAddress', 'anasbash@hotmail.com');
    s.WriteString('req', 'distinguished_name', 'dn');
    s.WriteString('req', 'req_extensions', 'req_ext');

    s.WriteString('dn', 'C', 'SA');
    s.WriteString('dn', 'OU', 'Riyad Branch');
    s.WriteString('dn', 'O', 'Contoso');
    s.WriteString('dn', 'CN', 'EA123456789');

    s.WriteString('req_ext', 'basicConstraints', 'critical,CA:TRUE');
    s.WriteString('req_ext', 'keyUsage', 'critical,cRLSign,digitalSignature,keyCertSign');

    s.WriteString('alt_names', 'serial', '123456');
    s.WriteString('alt_names', 'ID', '310122393500003');
    s.WriteString('alt_names', 'code', '9950');
    s.WriteString('alt_names', 'address', 'MyAddress');
    s.WriteString('alt_names', 'category', 'Industry');

    MakeCert2('HttpServer.crt', 'HttpServer.private.key', 'minilib', 'parmaja', 'SY', '', 2048, 0, 100);

    //MakeCert2('HttpServer', s);
    {if MakeCert(s) then
    begin
      aPubKey := s.ReadString('Result', 'PubKey', '');
      aPrvKey := s.ReadString('Result', 'PrvKey', '');
      aCer := s.ReadString('Result', 'Cer', '');
      aCsr := s.ReadString('Result', 'Csr', '');
    end;}
  finally
    s.Free;
  end;
end;

procedure TMain.Button2Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  //LogEdit.Lines.Add('Getting from URL ' + HostEdit.Text);
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.UseCompressing := True;
    //HttpClient.UserAgent := 'blalbla';
    //HttpClient.Compressing := True;
    HttpClient.GetMemoryStream('http://127.0.0.1:81/html/laz-logo.png', MemoryStream);
    //LoadFromStream(HttpClient.Response.ContentType, MemoryStream);
    MemoryStream.SaveToFile(ExtractFilePath(Application.ExeName) + 'test.png');
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  //LogEdit.Lines.Add('Finished');
end;

procedure TMain.Button3Click(Sender: TObject);
var
  ws: TWebsocketPayloadHeader;
begin
  FillChar(ws, SizeOf(TWebsocketPayloadHeader), 0);
  ws.Flags := [wsfFinish];
  ws.Opcode := wsoBinary;
  ws.InteralSize := 3;
  ws.Masked := True;
  Log.Write(IntToStr(SizeOf(TWebsocketPayloadHeader)));
  Log.Write(DataToBinStr(ws, SizeOf(ws), '-'));
end;

procedure TMain.FormCreate(Sender: TObject);
var
  aIni: TIniFile;
  function GetOption(AName, ADefault:string): string; overload;
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

  function GetSwitch(AName, ADefault: string): string;//if found in cmd mean it is true
  var
    s:string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := 'True'
    else
      Result := aIni.ReadString('options',AName, ADefault);
  end;

begin
  Memo.Font.Name := 'Consolas';
  Memo.Font.Size := 10;
  InstallEventLog(HttpServerLog);
  HttpServer := TmodWebServer.Create;
  HttpServer.OnBeforeOpen := HttpServerBeforeOpen;
  HttpServer.OnAfterOpen := HttpServerAfterOpen;
  HttpServer.OnAfterClose := HttpServerAfterClose;
  HttpServer.OnChanged :=  HttpServerChanged;
  HttpServer.OnLog := HttpServerLog;
  HttpServer.Logging := True;

  HttpServer.Modules.Add(THomeModule.Create('home', 'home', ['http/1.1']));

  aIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    HomePathEdit.Text := GetOption('homepath', '.\html');
    AliasNameEdit.Text := GetOption('alias', 'doc');
    PortEdit.Text := GetOption('port', '81');
    UseSSLChk.Checked := GetOption('ssl', false);
    StayOnTopChk.Checked := GetOption('on_top', false);
    KeepAliveChk.Checked := GetOption('keep_alive', false);
    CompressChk.Checked := GetOption('compress', false);
    AutoRunChk.Checked := StrToBoolDef(GetSwitch('autorun', ''), False);
    AutoOpenChk.Checked := StrToBoolDef(GetSwitch('autoopen', ''), False);
  finally
    aIni.Free;
  end;
  if AutoRunChk.Checked then
     HttpServer.Start;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    aIni.WriteString('options', 'homepath', HomePathEdit.Text);
    aIni.WriteString('options', 'alias', AliasNameEdit.Text);
    aIni.WriteString('options', 'Port', PortEdit.Text);
    aIni.WriteBool('options', 'ssl', UseSSLChk.Checked);
    aIni.WriteBool('options', 'on_top', StayOnTopChk.Checked);
    aIni.WriteBool('options', 'keep_alive', KeepAliveChk.Checked);
    aIni.WriteBool('options', 'compress', CompressChk.Checked);
    aIni.WriteBool('options', 'autorun', AutoRunChk.Checked);
    aIni.WriteBool('options', 'autoopen', AutoOpenChk.Checked);
  finally
    aIni.Free;
  end;
  FreeAndNil(HttpServer);
end;

procedure TMain.StayOnTopChkClick(Sender: TObject);
begin
  if StayOnTopChk.Checked then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE)
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TMain.HttpServerAfterClose(Sender: TObject);
begin
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMain.HttpServerAfterOpen(Sender: TObject);
begin
  if AutoOpenChk.Checked then
    OpenURL;
end;

procedure TMain.HttpServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;
  MaxOfThreadsLabel.Caption:=IntToStr(FMax);
  UpdateStatus;
end;

procedure TMain.HttpServerLog(const s: string);
begin
  Memo.Lines.Add(s);
end;

procedure TMain.OpenBtnClick(Sender: TObject);
begin
  OpenURL;
end;

procedure TMain.OpenURL;
begin
  if UseSSLChk.Checked then
    ShellExecute(Handle, 'Open', PWideChar('https://localhost:'+PortEdit.Text+'/'+AliasNameEdit.Text), nil, nil, 0)
  else
    ShellExecute(Handle, 'Open', PWideChar('http://localhost:'+PortEdit.Text+'/'+AliasNameEdit.Text), nil, nil, 0);
end;

end.

