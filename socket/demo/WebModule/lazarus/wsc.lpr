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
  mnSockets, mnServers,
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
begin
  ServerLog('');
  ServerLog('wsc - web server console');
  if UseSSL then
    ServerLog('use https://localhost:' + Port + '/doc/')
  else
    ServerLog('use http://localhost:' + Port + '/doc/');

  ChallengeServer.Enabled := UseSSL and Staging and Challenge;
  WebServers.Start;
end;

var
  aWsc: TwscServer;
  cmd: string;

begin
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

      Start;

      Writeln('');
      Writeln('Type "quit" or press Ctrl+C to stop the server.');
      repeat
        Readln(cmd);
        cmd := Trim(LowerCase(cmd));
      until (cmd = 'quit') or (cmd = 'exit') or (cmd = 'q');

      WebServers.Stop;
      FreeAndNil(WebServers);
    end;
  finally
    aWsc.Free;
  end;
end.
