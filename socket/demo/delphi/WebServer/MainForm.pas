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
  mnOpenSSLUtils, mnOpenSSL, mnLogs, mnHttpClient, osh, mnOpenSSLAPI,
  Registry, IniFiles, StdCtrls, ExtCtrls, mnConnections, mnSockets, mnServers, mnWebModules;

type
  TMain = class(TForm)
    Memo: TMemo;
    StartBtn: TButton;
    RootEdit: TEdit;
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
    ModuleNameEdit: TEdit;
    Label5: TLabel;
    KeeyAliveChk: TCheckBox;
    CompressChk: TCheckBox;
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure StayOnTopChkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FMax:Integer;
    Server: TmodWebServer;
    procedure UpdateStatus;
    procedure ModuleServerBeforeOpen(Sender: TObject);
    procedure ModuleServerAfterOpen(Sender: TObject);
    procedure ModuleServerAfterClose(Sender: TObject);
    procedure ModuleServerChanged(Listener: TmnListener);
    procedure ModuleServerLog(const S: string);
  public
  end;

var
  Main: TMain;

implementation

{$R *.DFM}

procedure TMain.StartBtnClick(Sender: TObject);
begin
  Server.Start;
end;

procedure TMain.StopBtnClick(Sender: TObject);
begin
  Server.Stop;
  StartBtn.Enabled := true;
end;

procedure TMain.UpdateStatus;
begin
  NumberOfThreads.Caption := IntToStr(Server.Listener.Count);
  LastIDLabel.Caption := IntToStr(Server.Listener.LastID);
end;

procedure TMain.ModuleServerBeforeOpen(Sender: TObject);
var
  aRoot:string;
  aWebModule: TmodWebModule;
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  aRoot := RootEdit.Text;
  if (LeftStr(aRoot, 2)='.\') or (LeftStr(aRoot, 2)='./') then
    aRoot := ExtractFilePath(Application.ExeName) + Copy(aRoot, 3, MaxInt);

  aWebModule := Server.Modules.Find<TmodWebModule>;
  if aWebModule <> nil then
  begin
    aWebModule.DocumentRoot := aRoot;
    aWebModule.AliasName := ModuleNameEdit.Text;
    aWebModule.UseKeepAlive := KeeyAliveChk.Checked;
    aWebModule.UseCompressing := CompressChk.Checked;
  end;
  Server.Port := PortEdit.Text;

  Server.UseSSL := UseSSLChk.Checked;

  Server.CertificateFile := 'server.cer';
  Server.PrivateKeyFile := 'server.private.key';

  //Server.CertificateFile := 'test.crt';
  //Server.PrivateKeyFile := 'test.private.key';
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

    MakeCert2('server.crt', 'server.privatekey.key', 'Creative Solutions', 'Creative Solutions', 'SY', '', 2048, 0, 1);

    //MakeCert2('server', s);
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
    HttpClient.Compressing := True;
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

  function GetSwitch(AName, ADefault:string): string;//if found in cmd mean it is true
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
  Server := TmodWebServer.Create;
  Server.OnBeforeOpen := ModuleServerBeforeOpen;
  Server.OnAfterOpen := ModuleServerAfterOpen;
  Server.OnAfterClose := ModuleServerAfterClose;
  Server.OnChanged :=  ModuleServerChanged;
  Server.OnLog := ModuleServerLog;
  Server.Logging := True;

  aIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    RootEdit.Text := GetOption('root', '.\html');
    ModuleNameEdit.Text := GetOption('ModuleName', 'doc');
    PortEdit.Text := GetOption('port', '81');
    UseSSLChk.Checked := GetOption('ssl', false);
    StayOnTopChk.Checked := GetOption('on_top', false);
    KeeyAliveChk.Checked := GetOption('keep_alive', false);
    CompressChk.Checked := GetOption('compress', false);
    aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aIni.Free;
  end;
  if aAutoRun then
     Server.Start;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    aIni.WriteString('options', 'DocumentRoot', RootEdit.Text);
    aIni.WriteString('options', 'ModuleName', ModuleNameEdit.Text);
    aIni.WriteString('options', 'Port', PortEdit.Text);
    aIni.WriteBool('options', 'ssl', UseSSLChk.Checked);
    aIni.WriteBool('options', 'on_top', StayOnTopChk.Checked);
    aIni.WriteBool('options', 'keep_alive', KeeyAliveChk.Checked);
    aIni.WriteBool('options', 'compress', CompressChk.Checked);
  finally
    aIni.Free;
  end;
  FreeAndNil(Server);
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

procedure TMain.ModuleServerAfterClose(Sender: TObject);
begin
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMain.ModuleServerAfterOpen(Sender: TObject);
begin
  ShellExecute(Handle, 'Open', PWideChar('http://127.0.0.1:'+PortEdit.Text+'/web'), nil, nil, 0);
end;

procedure TMain.ModuleServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;
  MaxOfThreadsLabel.Caption:=IntToStr(FMax);
  UpdateStatus;
end;

procedure TMain.ModuleServerLog(const s: string);
begin
  Memo.Lines.Add(s);
end;

end.

