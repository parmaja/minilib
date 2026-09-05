unit mnSMTPClient;
{ **
  *  This file is part of the "Mini Library"
  *
  * @license   MIT
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    Zaher Dirkey zaherdirkey
  *
  * Ref:
  *   https://www.samlogic.net/articles/smtp-commands-reference.htm
  *   https://en.wikipedia.org/wiki/SMTP_Authentication
  * }

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

{$define OUT_Console}

interface

uses
  SysUtils, Classes, StrUtils, DateUtils,
  mnOpenSSL, mnLogs,
  mnTypes, mnUtils, mnClasses, mnFields, mnParams, mnModules, mnSockets, mnJobs, mnBase64,
  mnClients, mnStreams, mnStreamUtils;

const
  // 2xx - Success
  SMTP_SYSTEM_STATUS          = 211; // System status / help reply
  SMTP_HELP_MESSAGE           = 214; // Help message
  SMTP_SERVICE_READY          = 220; // <domain> Service ready (on connect)
  SMTP_SERVICE_CLOSING        = 221; // Service closing transmission channel (after QUIT)
  SMTP_AUTH_SUCCESS           = 235; // Authentication successful
  SMTP_OK                     = 250; // Requested mail action okay, completed
  SMTP_USER_NOT_LOCAL         = 251; // User not local; will forward
  SMTP_CANNOT_VERIFY          = 252; // Cannot VRFY user, but will accept message

  // 3xx - Intermediate (more input required)
  SMTP_AUTH_CONTINUE          = 334; // Server challenge, continue with AUTH exchange
  SMTP_START_MAIL_INPUT       = 354; // Start mail input; end with <CRLF>.<CRLF>

  // 4xx - Temporary failure (retry later)
  SMTP_SERVICE_NOT_AVAILABLE  = 421; // Service not available, closing channel
  SMTP_MAILBOX_BUSY           = 450; // Requested action not taken: mailbox busy/blocked
  SMTP_LOCAL_ERROR            = 451; // Requested action aborted: local error in processing
  SMTP_INSUFFICIENT_STORAGE   = 452; // Requested action not taken: insufficient system storage
  SMTP_TEMP_AUTH_FAILURE      = 454; // Temporary authentication failure
  SMTP_PARAMS_ERROR           = 455; // Server unable to accommodate parameters

  // 5xx - Permanent failure
  SMTP_SYNTAX_ERROR           = 500; // Syntax error, command unrecognized
  SMTP_SYNTAX_ERROR_PARAMS    = 501; // Syntax error in parameters or arguments
  SMTP_COMMAND_NOT_IMPLEMENTED= 502; // Command not implemented
  SMTP_BAD_SEQUENCE           = 503; // Bad sequence of commands
  SMTP_PARAM_NOT_IMPLEMENTED  = 504; // Command parameter not implemented
  SMTP_AUTH_REQUIRED          = 530; // Authentication required
  SMTP_AUTH_TOO_WEAK          = 534; // Authentication mechanism too weak
  SMTP_AUTH_CREDENTIALS_INVALID = 535; // Authentication credentials invalid
  SMTP_ENCRYPTION_REQUIRED    = 538; // Encryption required for requested auth mechanism
  SMTP_MAILBOX_UNAVAILABLE    = 550; // Requested action not taken: mailbox unavailable
  SMTP_USER_NOT_LOCAL_FWD     = 551; // User not local; please try <forward-path>
  SMTP_EXCEEDED_STORAGE       = 552; // Requested mail action aborted: exceeded storage allocation
  SMTP_MAILBOX_NAME_INVALID   = 553; // Requested action not taken: mailbox name not allowed
  SMTP_TRANSACTION_FAILED     = 554; // Transaction failed / no valid recipients

type

  TmnCustomSMTPClient = class;

  { TmnCustomSMTPHeader }

  TmnCustomSMTPHeader = class(TmodCommunicate)
  private
    FClient: TmnCustomSMTPClient;
  protected
  public
    constructor Create(AClient: TmnCustomSMTPClient); virtual;
    destructor Destroy; override;
    procedure Clear; override;

    property Client: TmnCustomSMTPClient read FClient;
  end;

  { TmnCustomSMTPClient }

  TmnCustomSMTPClient = class abstract(TObject)
  private
    FHost: UTF8String;
    FPort: UTF8String;
    FUserName: UTF8String;
    FPassword: UTF8String;
    FDomainName: string;
    FXMailer: string;
    FUseSSL: Boolean;

    FExtended: Boolean;
    FMaxSize: Integer;
    FAuthenticated: Boolean;
    FSSLEnabled: Boolean;

    FCapabilities: TStringList;
    FStream: TmnConnectionStream;
  protected

    procedure EnableSSL; virtual;
    function DoCreateStream: TmnConnectionStream; virtual; abstract;

    function CreateStream: TmnConnectionStream;
    procedure FreeStream; virtual;
    function GetCapability(Name: string): string;
    function CapabilityExists(Name: string): Boolean;
    function ReadRespond(Respond: TStringList = nil): Integer;
    function ReadRespondLine(out S: string): Integer;
    function ReadLine(out s: String): Boolean;
    procedure WriteLine(s: string);
    procedure WriteCommand(Command: string; s: String = '');
  public
    constructor Create;
    destructor Destroy; override;
    function Connected: Boolean;

    procedure Connect;
    function Open: Boolean;
    function Login: Boolean;
    function SendFrom(vFrom: string): Boolean;
    function SendTo(vTo: string): Boolean;
    procedure Disconnect;

    function SendMail(const vFrom, vTo, vSubject: string; vBody: TStringList): Boolean;

    property Host: UTF8String read FHost write FHost;
    property Port: UTF8String read FPort write FPort;
    property UserName: UTF8String read FUserName write FUserName;
    property Password: UTF8String read FPassword write FPassword;
    property Stream: TmnConnectionStream read FStream;
    property Capabilities: TStringList read FCapabilities;
    property XMailer: string read FXMailer write FXMailer;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
  end;

  { TmnCustomSMTPStream }

  TmnSMTPStream = class(TmnClientSocket)
  private
  protected
  public
  end;

  { TmnSMTPClient }

  TmnSMTPClient = class(TmnCustomSMTPClient)
  protected
    procedure EnableSSL; override;
    function DoCreateStream: TmnConnectionStream; override;
  end;

// WithEnclose: < >
function ParseEmail(EMailAddress: string; out Name, EMail: string; WithEnclose: Boolean = False): Boolean;

function SMTPMail(vHost, vUsername, vPassword: string; vFrom, vTo, vSubject: string; vBody: string; UseSSL: Boolean = False): Boolean; overload;
function SMTPMail(vHost, vUsername, vPassword: string; vFrom, vTo, vSubject: string; vBody: TStringList; UseSSL: Boolean = False): Boolean; overload;

implementation

const
  sUserAgent = 'mnMAIL';

function IsSMTPSuccess(ACode: Integer): Boolean;
begin
  Result := (ACode >= 200) and (ACode < 300);
end;

function IsSMTPTemporaryFailure(ACode: Integer): Boolean;
begin
  Result := (ACode >= 400) and (ACode < 500);
end;

function IsSMTPPermanentFailure(ACode: Integer): Boolean;
begin
  Result := ACode >= 500;
end;

function ScanString(S: string; out Separator: Char; out Code, Message: string): Boolean;
var
  C: Char;
  Count: Integer;
begin
  Result := False;

  if S = '' then
    Exit;

  Count := 1;
  for C in S do
  begin
    if CharInSet(C, [' ', '-']) then
    begin
      Separator := C;
      Result := True;
      Break;
    end;
    Inc(Count);
  end;

  Code := Copy(S, 1, Count-1);
  Message := Copy(S, Count+1, MaxInt);
end;

//* "Zaher Dirkey"<zaherdirkey@gmail.com>

function ParseEmail(EMailAddress: string; out Name, EMail: string; WithEnclose: Boolean = False): Boolean;
var
  Index, NextIndex, MatchCount: Integer;
  Address: string;
begin
  Result := StrScanTo(EMailAddress, 1, Name, Index , NextIndex, MatchCount, ['<']);
  Name := DequoteStr(Name);
  EMail := MidStr(EMailAddress, NextIndex-1, MaxInt);
  if EMail = '' then
  begin
    EMail := Name;
    SplitStr(Email, '@', Name, Address);
  end;

  if WithEnclose then
    EMail := EncloseStr(EMail, '<', '>')
  else
    EMail := UncloseStr(EMail, '<', '>');
end;

function SMTPMail(vHost, vUsername, vPassword: string; vFrom, vTo, vSubject: string; vBody: TStringList; UseSSL: Boolean = False): Boolean;
var
  SMTPClient: TmnSMTPClient;
  aHost, aPort, aUserName, aUserMail: string;
begin
  SplitStr(vHost, ':', aHost, aPort);
  SMTPClient := TmnSMTPClient.Create;
  try
    SMTPClient.Host := aHost;
    SMTPClient.Port := aPort;
    if SMTPClient.Port = '' then
      SMTPClient.Port := '25';
    ParseEmail(vUsername, aUserName, aUserMail);
    SMTPClient.UserName := aUserMail;
    SMTPClient.Password := vPassword;
    SMTPClient.UseSSL := UseSSL;
    Result := SMTPClient.SendMail(vFrom, vTo, vSubject, vBody);
  finally
    SMTPClient.Free;
  end;
end;

function SMTPMail(vHost, vUsername, vPassword: string; vFrom, vTo, vSubject: string; vBody: string; UseSSL: Boolean = False): Boolean;
var
  Body: TStringList;
begin
  Body := TStringList.Create;
  try
    Body.Text := vBody;
    Result := SMTPMail(vHost, vUsername, vPassword, vFrom, vTo, vSubject, Body);
  finally
    Body.Free;
  end;
end;

{ TmnCustomSMTPHeader }

constructor TmnCustomSMTPHeader.Create(AClient: TmnCustomSMTPClient);
begin
  inherited Create(nil);
  FClient := AClient;
end;

destructor TmnCustomSMTPHeader.Destroy;
begin
  inherited;
end;

procedure TmnCustomSMTPHeader.Clear;
begin
  Header.Clear;
end;

{ TmnCustomSMTPClient }

function TmnCustomSMTPClient.Connected: Boolean;
begin
  Result := FStream.Connected;
end;

function TmnCustomSMTPClient.CapabilityExists(Name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Capabilities.Count - 1 do
    if SameText(LeftStr(Capabilities[i], Length(Name)), Name)  then
    begin
      Result := True;
      Break;
    end;
end;

procedure TmnCustomSMTPClient.Connect;
begin
  if FStream = nil then
    FStream := CreateStream;
  Stream.Connect;
end;

function TmnCustomSMTPClient.CreateStream: TmnConnectionStream;
begin
  Result := DoCreateStream;
end;

function TmnCustomSMTPClient.GetCapability(Name: string): string;
var
  i: Integer;
  l: Integer;
  s: string;
begin
  Result := '';
  l := Length(Name);
  for i := 0 to Capabilities.Count - 1 do
    if SameText(LeftStr(Capabilities[i], l), Name)  then
    begin
      s := MidStr(Capabilities[i], l+1, MaxInt);
      if (s='') or (s[1]=' ') or (s[1]='=') then
      begin
        if s = '' then
          Result := ''
        else
          Result := MidStr(s, 2, MaxInt);
        Break;
      end;
    end;
end;

procedure TmnCustomSMTPClient.FreeStream;
begin
  FreeAndNil(FStream);
end;

constructor TmnCustomSMTPClient.Create;
begin
  inherited;
  FCapabilities := TStringList.Create;
  FDomainName := 'localhost';
  FXMailer := 'miniLib pascal library';
end;

destructor TmnCustomSMTPClient.Destroy;
begin
  FreeStream;
  FreeAndNil(FCapabilities);
  inherited;
end;

{ TmnCustomSMTPClient }

function TmnCustomSMTPClient.Open: Boolean;
begin
  Connect;
  Result := Stream.Connected;
end;

function TmnCustomSMTPClient.ReadLine(out s: String): Boolean;
begin
  Result := FStream.ReadUTF8Line(s);
  Log.WriteLn('=> '+S);
end;

procedure TmnCustomSMTPClient.WriteCommand(Command: string; s: String = '');
begin
  if (s <> '') then
    WriteLine(Command+' '+s)
  else
    WriteLine(Command)
end;

procedure TmnCustomSMTPClient.WriteLine(s: string);
begin
  FStream.WriteUTF8Line(s);
  Log.Writeln('<= '+s);
end;

function TmnCustomSMTPClient.ReadRespond(Respond: TStringList = nil): Integer;
var
  s: String;
  Own: Boolean;
  Separator: Char;
  Code, Msg: string;
begin
  Result := 0;

  Own := Respond = nil;
  if Own then
    Respond := TStringList.Create
  else
    Respond.Clear;

  while ReadLine(s) do
  begin
    if not FStream.Connected then
      break;

    ScanString(s, Separator, Code, Msg);
    if Separator = ' ' then //* or '-'
    begin
      Result := StrToInt(Code);
      break;
    end;
    Respond.Add(Msg);
  end;

  if Own then
    FreeAndNil(Respond);
end;

function TmnCustomSMTPClient.ReadRespondLine(out S: string): Integer;
var
  Separator: Char;
  Code, Msg: string;
begin
  ReadLine(s);
  ScanString(S, Separator, Code, Msg);
  if Separator = ' ' then
  begin
    Result := StrToInt(Code);
    S := Msg;
  end
  else
    raise Exception.Create('SMTP protcol, should read respond line with space not dash');
end;

function TmnCustomSMTPClient.Login: Boolean;
var
  Respond: TStringList;

  procedure AuthLogin;
  begin
    WriteCommand('AUTH', 'LOGIN');
    if ReadRespond <> SMTP_AUTH_CONTINUE then
      Exit;
    WriteLine(Base64Encode(Username));
    if ReadRespond <> SMTP_AUTH_CONTINUE then
      Exit;
    WriteLine(Base64Encode(Password));
    FAuthenticated := ReadRespond = SMTP_AUTH_SUCCESS;
  end;

  procedure AuthPlain;
  var
    s: UTF8String;
  begin
    s := Utf8Char(0) + Username + Utf8Char(0) + Password;
    WriteCommand('AUTH', 'PLAIN ' + Base64Encode(s));
    FAuthenticated := ReadRespond = SMTP_AUTH_SUCCESS;
  end;

var
  Code: Integer;

  procedure SendHello;
  begin
    //* Send Extended Hello
    WriteCommand('EHLO', FDomainName);
    Code := ReadRespond(FCapabilities);
    Result := (Code >= 250) and (Code <= 259);
  end;

var
  Auths: TStringList;
  s: string;
begin
  Result := False;

  Respond := TStringList.Create;
  try
    Capabilities.Clear;
    FExtended := True;
    FAuthenticated := False;

    FMaxSize := 0;

    if UseSSL and not FSSLEnabled then
    begin
      EnableSSL;
      FSSLEnabled := True;
    end;

    Result := ReadRespond = SMTP_SERVICE_READY;
    if Result then
      SendHello;
    if Result then
    begin
      if not FSSLEnabled and CapabilityExists('STARTTLS') then
      begin
        WriteCommand('STARTTLS');
        Result := ReadRespond = SMTP_SERVICE_READY;
        if Result then
        begin
          EnableSSL;
          FSSLEnabled := True;
          SendHello;
        end;
      end;
    end
    else//* Trying with none extended SMTP
    begin
      FExtended := False;
      //* Trying again with normal Hello
      WriteCommand('HELO', FDomainName);
      Code := ReadRespond(FCapabilities);
      Result := (Code >= 250) and (Code <= 259);
    end;

    if Result and FExtended then
    begin
      FMaxSize := StrToIntDef(GetCapability('SIZE'), 0);
      if (Username <> '') and (Password <> '') then
      begin
        Auths := TStringList.Create;
        try
          Auths.Delimiter := ' ';
          Auths.DelimitedText := GetCapability('AUTH');
          for s in Auths do
          begin
            if SameText(s, 'LOGIN') then
              AuthLogin
            else if SameText(s, 'PLAIN') then
              AuthPlain;
{              if SameText(s, 'CRAM-MD5') then
              AuthCramMD5
    PLAIN (Uses Base64 encoding)
    LOGIN (Uses Base64 encoding)[11] (obsoleted in favor of PLAIN)
    GSSAPI (Generic Security Services Application Program Interface)
    DIGEST-MD5 (Digest access authentication)
    MD5
    CRAM-MD5
    OAUTH10A (OAuth 1.0a HMAC-SHA1 tokens as defined in RFC 5849)
    OAUTHBEARER (OAuth 2.0 bearer tokens as defined in RFC 6750)
    XOAUTH2 [12]
              }

            if FAuthenticated then
              break;
          end;
          Result := FAuthenticated;
        finally
          Auths.Free;
        end;
      end;
    end;
  finally
    Respond.Free;
  end;
end;

procedure TmnCustomSMTPClient.Disconnect;
begin
  if FStream <> nil then
    FStream.Disconnect;
  FreeStream;
end;

procedure TmnCustomSMTPClient.EnableSSL;
begin
end;

function TmnCustomSMTPClient.SendFrom(vFrom: string): Boolean;
var
  Name, EMail: string;
begin
  ParseEmail(vFrom, Name, EMail);
  if EMail = '' then
    raise Exception.Create('SMTP Client: From EMail is Empty');
  WriteCommand('Mail From:', '<' + EMail + '>');
  Result := ReadRespond = SMTP_OK;
end;

function TmnCustomSMTPClient.SendTo(vTo: string): Boolean;
var
  Name, EMail: string;
begin
  ParseEmail(vTo, Name, EMail);
  if EMail = '' then
    raise Exception.Create('SMTP Client: To EMail is Empty');
  WriteCommand('RCPT TO:', '<' + EMail + '>');
  Result := ReadRespond = SMTP_OK;
end;

function TmnCustomSMTPClient.SendMail(const vFrom, vTo, vSubject: string; vBody: TStringList): Boolean;
const
  Terminator = '.';
var
  i: Integer;
  line: string;
begin
  Result := False;
  if Open then
  begin
    if Login then
    begin
      Result := SendFrom(vFrom);
      if not Result then
        Exit;
      Result := SendTo(vTo);
      if not Result then
        Exit;

      WriteCommand('DATA');
      Result := ReadRespond = SMTP_START_MAIL_INPUT;
      if not Result then
        Exit;

      WriteLine('From: ' + vFrom);
      WriteLine('To: ' + vTo);
      WriteLine('Date: ' + DateTimeToRFC822(now));
      WriteLine('Subject: ' + vSubject);
      WriteLine('X-mailer: ' + XMailer);
      WriteLine('');

      for i := 0 to vBody.Count - 1 do
      begin
        line := vBody[i];
        if line<>'' then
        begin
          if line[1] = Terminator then
            line := Terminator + line;
        end;
        WriteLine(Line);
      end;
      WriteLine(Terminator);
      Result := ReadRespond = SMTP_OK;

      WriteCommand('QUIT');
      Result := Result and (ReadRespond = SMTP_SERVICE_CLOSING);
    end;
  end;
end;

{ TmnSMTPClient }

function TmnSMTPClient.DoCreateStream: TmnConnectionStream;
var
  aStream: TmnSMTPStream;
begin
  aStream := TmnSMTPStream.Create;

  aStream.EndOfLine      := sWinEndOfLine;
  aStream.ReadTimeout    := 5000;
  aStream.ConnectTimeout := 5000;
  aStream.WriteTimeout   := 5000;
  aStream.Options := aStream.Options + [soWaitBeforeRead];

  aStream.Address := Host;
  aStream.Port := Port;
  if aStream.Port = '' then
    aStream.Port := '25';

  if UseSSL then
  begin
    aStream.Options := aStream.Options + [soSSL, soWaitBeforeRead];
    FSSLEnabled := True;
  end
  else
    aStream.Options := aStream.Options;// + [soNoDelay];
  Result := aStream;
end;

procedure TmnSMTPClient.EnableSSL;
begin
  (FStream as TmnSMTPStream).EnableSSL;
end;

end.
