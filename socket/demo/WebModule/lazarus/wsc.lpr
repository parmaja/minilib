program wsc;

{**
 *  This file is part of the "Mini Library"
 *
 *  Console version of the WebServer demo, works as a web server.
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}

{$mode delphi}{$H+}

uses
  {$ifndef WINDOWS}
  cthreads,
  {$endif}
  SysUtils, IniFiles, Classes,
  mnLogs, mnUtils,
  mnSockets, mnServers, mnOpenSSL,
  mnBootstraps,
  mnModules, mnWebModules, mnACME, mnWebElements, HomeModules;

type
  { TwscServer }

  TwscServer = class
  private
    FMax: Integer;
    AcmePath: string;
    AcmePort: string;
    AcmeDomain: string;
    AcmeEmail: string;
    CertPassword: string;
    CertFile: string;
    PrivateKeyFile: string;

    ChallengeServer: TmodWebServer;
    HttpServer: TmodWebServer;

    WebServers: TWebServers;

    UseSSL: Boolean;
    Staging: Boolean;
    KeepAlive: Boolean;
    Compress: Boolean;
    Challenge: Boolean;
    LogMessages: Boolean;
    HomePath: string;
    DocAlias: string;
    HomeAlias: string;
    Bind: string;
    Port: string;

    procedure LoadConfig;
    procedure HttpServerBeforeOpen(Sender: TObject);
    procedure ChallengeServerBeforeOpen(Sender: TObject);
    procedure ServerLog(const S: String);
    procedure Start;
    procedure RenewCert;
    procedure MakeCertCmd;
    procedure SaveRenewErrorReport(E: Exception);
    function ExecuteCommand(const ACommand: string): Boolean;
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
        Value := Copy(s, l + 3, Maxint);
        Result := True;
        break;
      end;
  end;
end;

{ TwscServer }

procedure TwscServer.ServerLog(const S: String);
begin
  Writeln(S);
end;

procedure TwscServer.ChallengeServerBeforeOpen(Sender: TObject);
begin
  ChallengeServer.Bind := Bind;
  ChallengeServer.Port := AcmePort;
end;

procedure TwscServer.HttpServerBeforeOpen(Sender: TObject);
var
  aAppFolder, aHomeFolder: string;
  aDocModule: TmodWebModule;
  aHomeModule: THomeModule;
begin
  aAppFolder := ExtractFilePath(ParamStr(0));
  aHomeFolder := IncludePathDelimiter(HomePath);
  if (LeftStr(aHomeFolder, 2)='.\') or (LeftStr(aHomeFolder, 2) = './') then
    aHomeFolder := IncludePathDelimiter(aAppFolder + Copy(aHomeFolder, 3, MaxInt));

  HttpServer.Bind := Bind;
  HttpServer.Port := Port;
  if UseSSL and FileExists(CertFile) then
  begin
    HttpServer.IsSecure := True;
    HttpServer.CertPassword := CertPassword;
    HttpServer.CertificateFile := CertFile;
    if FileExists(PrivateKeyFile) then
      HttpServer.PrivateKeyFile := PrivateKeyFile;
  end;

  aDocModule := HttpServer.Modules.Find<TmodWebFileModule>;
  if aDocModule <> nil then
  begin
    aDocModule.AliasName := DocAlias;
    aDocModule.PublicPath := aHomeFolder;
    (aDocModule as TmodWebFileModule).ServeFiles := [serveEnabled, serveIndex, serveDefault, serveSmart];
    if Compress then
      aDocModule.UseCompressing := ovUndefined
    else
      aDocModule.UseCompressing := ovNo;
    aDocModule.UseKeepAlive.AsBoolean := KeepAlive;
    HttpServer.SetNotfound;
  end;

  aHomeModule := HttpServer.Modules.Find<THomeModule>;
  if aHomeModule <> nil then
  begin
    aHomeModule.AliasName := HomeAlias;
    aHomeModule.Domain := AcmeDomain;
    aHomeModule.PublicPath := aHomeFolder;
    aHomeModule.PrivatePath := aAppFolder;

    aHomeModule.Web.IsSecure := HttpServer.IsSecure;
    aHomeModule.Web.AppPath := aAppFolder;
    aHomeModule.Web.Assets.LogoFile := aHomeModule.PublicPath + 'cs.svg';

    ForceDirectories(aHomeModule.PrivatePath + 'cache');
    ForceDirectories(aHomeModule.PrivatePath + 'temp');

    if Compress then
      aHomeModule.UseCompressing := ovUndefined
    else
      aHomeModule.UseCompressing := ovNo;
    aHomeModule.UseKeepAlive.AsBoolean := KeepAlive;
  end;
end;

procedure TwscServer.LoadConfig;
var
  aIni: TIniFile;
  AppLocation: string;

  function GetStringOption(AName, ADefault: string): string;
  var
    s: string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := AnsiDequotedStr(s, '"')
    else
      Result := aIni.ReadString('options', AName, ADefault);
  end;

  function GetBoolOption(AName: string; ADefault: Boolean = False): Boolean; //if found in cmd mean it is true
  var
    s: string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := True
    else
      Result := aIni.ReadBool('options', AName, ADefault);
  end;

begin
  AppLocation := IncludePathDelimiter(ExtractFilePath(ParamStr(0)));
  aIni := TIniFile.Create(AppLocation + 'config.ini');
  try
    HomePath := GetStringOption('homepath', '.\html');
    Port := GetStringOption('port', '81');
    DocAlias := GetStringOption('doc.alias', 'doc');
    HomeAlias := GetStringOption('home.alias', 'home');
    Bind := GetStringOption('bind', '0.0.0.0');
    UseSSL := GetBoolOption('ssl');
    Compress := GetBoolOption('compress');
    KeepAlive := GetBoolOption('keep-alive');

    Challenge := GetBoolOption('challenge');
    Staging := GetBoolOption('staging');

    AcmePath := CorrectPath(IncludePathDelimiter(ExpandToPath(aIni.ReadString('acme', 'path', AppLocation + 'acme'), AppLocation)));
    AcmePort := aIni.ReadString('acme', 'port', '80');
    AcmeDomain := aIni.ReadString('acme', 'domain', '');
    AcmeEmail := aIni.ReadString('acme', 'email', '');

    CertPassword := aIni.ReadString('cert', 'password', '');
    CertFile := CorrectPath(ExpandToPath(aIni.ReadString('cert', 'certificate', './certificate.pem'), AppLocation));
    PrivateKeyFile := CorrectPath(ExpandToPath(aIni.ReadString('cert', 'privatekey', './privatekey.pem'), AppLocation));

    LogMessages := GetBoolOption('log');
  finally
    aIni.Free;
  end;
end;

procedure TwscServer.Start;
var
  aDaysLeft: Integer;
begin
  ServerLog('');
  ServerLog('wsc - web server console');
  if UseSSL then
    ServerLog('use https://localhost:' + Port + '/doc/')
  else
    ServerLog('use http://localhost:' + Port + '/doc/');

  if UseSSL and FileExists(CertFile) then
  begin
    aDaysLeft := GetCertDaysLeft(CertFile);
    if aDaysLeft = MaxInt then
      ServerLog('certificate: cannot read expiry from ' + CertFile)
    else if aDaysLeft < 0 then
      ServerLog('WARNING: certificate EXPIRED ' + IntToStr(Abs(aDaysLeft)) + ' day(s) ago: ' + CertFile)
    else
      ServerLog('certificate expires in ' + IntToStr(aDaysLeft) + ' day(s): ' + CertFile);
  end;

  ChallengeServer.Enabled := UseSSL and Challenge;
  WebServers.Start;
end;

{Renew certificate from https://letsencrypt.org/ (ACME v2, http-01 challenge)
 Check "Staging" to test against https://acme-staging-v02.api.letsencrypt.org
 without hitting the production rate limits}
procedure TwscServer.RenewCert;
var
  aPath: string;
begin
  if (AcmeDomain = '') or (AcmeEmail = '') then
    raise Exception.Create('Domain and EMail must defined in [acme] section of config.ini');
  aPath := AcmePath + '.well-known' + PathDelim + 'acme-challenge' + PathDelim;
  ForceDirectories(aPath);
  try
    RenewCertificate(AcmeDomain, AcmeEmail, CertFile, PrivateKeyFile, aPath, Staging, ServerLog);
  except
    on E: Exception do
    begin
      SaveRenewErrorReport(E);
      raise;
    end;
  end;
end;

//Save a JSON report of the renewal error next to the certificate,
//if the exception carries an ACME error body (JSON), it is embedded as acme.error
procedure TwscServer.SaveRenewErrorReport(E: Exception);
var
  aReport, aMsg, aBody: string;
  p: Integer;

  function JsonEscape(const S: string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(S) do
      case S[i] of
        '\': Result := Result + '\\';
        '"': Result := Result + '\"';
        #10: Result := Result + '\n';
        #13: {skip};
        #9: Result := Result + '\t';
      else
        if Ord(S[i]) < 32 then
          Result := Result + '\' + IntToHex(Ord(S[i]), 4)
        else
          Result := Result + S[i];
      end;
  end;

begin
  aMsg := E.Message;
  aBody := '';
  //exception message format: "ACME error (<url>): <json body>"
  p := Pos('{', aMsg);
  if p > 0 then
  begin
    aBody := Trim(Copy(aMsg, p, MaxInt));
    aMsg := Trim(Copy(aMsg, 1, p - 1));
  end;

  ForceDirectories(ExtractFilePath(CertFile));
  aReport := '{' + sLineBreak
    + '  "type": "renew",' + sLineBreak
    + '  "datetime": "' + JsonEscape(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)) + '",' + sLineBreak
    + '  "domain": "' + JsonEscape(AcmeDomain) + '",' + sLineBreak
    + '  "staging": ' + LowerCase(BoolToStr(Staging, True)) + ',' + sLineBreak
    + '  "certificate": "' + JsonEscape(CertFile) + '",' + sLineBreak
    + '  "privatekey": "' + JsonEscape(PrivateKeyFile) + '",' + sLineBreak
    + '  "error": {' + sLineBreak
    + '    "message": "' + JsonEscape(aMsg) + '"' + sLineBreak;
  if aBody <> '' then
    aReport := aReport + '    ,"' + 'body' + '": ' + aBody + sLineBreak;
  aReport := aReport + '  }' + sLineBreak + '}' + sLineBreak;

  try
    with TStringList.Create do
    try
      Text := aReport;
      SaveToFile(ExtractFilePath(CertFile) + 'renew-report.json');
    finally
      Free;
    end;
    ServerLog('renew error report saved: ' + ExtractFilePath(CertFile) + 'renew-report.json');
  except
    on E2: Exception do
      ServerLog('cannot save renew error report: ' + E2.Message);
  end;
end;

procedure TwscServer.MakeCertCmd;
begin
  MakeCert('certificate.pem', 'privatekey.pem', 'PARMAJA', 'PARMAJA TEAM', 'SY', '', 2048, 0, 365);
end;

function TwscServer.ExecuteCommand(const ACommand: string): Boolean;
begin
  Result := True;
  if ACommand = 'renew' then
  begin
    //Challenge server must be started to serve .well-known/acme-challenge
    ChallengeServer.Enabled := True;
    WebServers.Start;
    try
      RenewCert;
    finally
      WebServers.Stop;
    end;
  end
  else if ACommand = 'makecert' then
    MakeCertCmd
  else
    Result := False;
end;

var
  aWsc: TwscServer;
  cmd, aCommand: string;

  function GetCommand: string; //first argument if it is a known command
  begin
    Result := LowerCase(Trim(ParamStr(1)));
    if (Result = 'renew') or (Result = 'makecert') then
      //known command
    else
      Result := '';
  end;

begin
  InitOpenSSL(True);
  {$ifndef WINDOWS}
  //cmem;
  {$endif}

  aWsc := TwscServer.Create;
  try
    InstallEventLog(aWsc.ServerLog);

    aWsc.LoadConfig;

    with aWsc do
    begin
      WebServers := TWebServers.Create;

      ChallengeServer := TmodWebServer.Create;
      ChallengeServer.Name := 'ChallengeServer';
      ChallengeServer.AddChallengeAcme(AcmePath + '.well-known' + PathDelim);
      ChallengeServer.AddRedirectHttps;
      ChallengeServer.OnBeforeOpen := ChallengeServerBeforeOpen;
      ChallengeServer.OnLog := ServerLog;
      WebServers.AddServer('ChallengeServer', ChallengeServer);

      HttpServer := TmodWebServer.Create;
      HttpServer.Name := 'WebServer';
      HttpServer.OnBeforeOpen := HttpServerBeforeOpen;
      HttpServer.OnLog := ServerLog;
      WebServers.AddServer('HttpServer', HttpServer);

      TmodWebFileModule.Create(HttpServer, 'doc', 'doc');
      THomeModule.Create(HttpServer, 'home', 'home');
      HttpServer.SetNotfound;

      aCommand := GetCommand;
      if aCommand <> '' then
      begin
        //run one command then exit
        if not ExecuteCommand(aCommand) then
          Writeln('Unknown command: ' + aCommand);
      end
      else
      begin
        Start;

        Writeln('');
        Writeln('Commands: renew, makecert, quit');
        repeat
          Readln(cmd);
          cmd := Trim(LowerCase(cmd));
          if cmd = 'quit' then Break;
          if not ExecuteCommand(cmd) and (cmd <> '') and (cmd <> 'exit') and (cmd <> 'q') then
            Writeln('Unknown command: ' + cmd);
        until (cmd = 'exit') or (cmd = 'q');

        WebServers.Stop;
      end;
      FreeAndNil(WebServers);
    end;
  finally
    aWsc.Free;
  end;
end.
