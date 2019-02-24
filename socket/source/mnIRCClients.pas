unit mnIRCClients;
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *  @license  modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 *  @ported from SlyIRC the orignal author Steve Williams
 *  @author by Zaher Dirkey <zaher at parmaja dot com>


    https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands
    https://stackoverflow.com/questions/12747781/irc-message-format-clarification
 *}

interface

uses
  Classes, StrUtils,
  mnClasses,
  mnSockets, mnClients, mnStreams, mnConnections, mnCommands, mnUtils;

const
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }

type

  { TIRCSocketStream }

  TIRCSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  TmnIRCClient = class;

  TIRCLogType = (lgMsg, lgSend, lgReceive);

  { TIRCCommand }

  TIRCCommand = class(TObject)
  private
    FCode: string;
    FData: String;
    FName: string;

    FTime: string;
    FParams: TStringList;
    FText: string;
    FSource: string;
    FTarget: string;

    procedure SetData(const Value: String);
  protected
    property Data: String read FData write SetData;
    procedure Parse(vData: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Source: string read FSource;
    property Target: string read FTarget;

    property Name: string read FName;
    property Code: string read FCode;
    property Text: string read FText;

    property Params: TStringList read FParams;
  end;

  { TIRCHandler }

  TIRCHandler = class(TObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure DoReceive(vCommand: TIRCCommand); virtual; abstract;
    procedure Receive(vCommand: TIRCCommand); virtual;
  public
    Name: String;
    Code: String;
    constructor Create(AClient: TmnIRCClient);
    property Client: TmnIRCClient read FClient;
  end;

  TIRCHandlerClass = class of TIRCHandler;

  { TCustomIRCHandlers }

  //We should use base class TmnCommandClasses, TODO

  TCustomIRCHandlers = class(TmnNamedObjectList<TIRCHandler>)
  private
    FClient: TmnIRCClient;
  public
    constructor Create(AClient: TmnIRCClient);
    destructor Destroy; override;
    function Add(vName, vCode: String; AClass: TIRCHandlerClass): Integer;
    procedure Handle(ACommand: TIRCCommand);
    property Client: TmnIRCClient read FClient;
  end;

  { TIRCHandlers }

  //Preeminent handlers
  TIRCHandlers = class(TCustomIRCHandlers);

  { TIRCPoolHandlers }

  //Temporary commands, when received it will deleted
  TIRCPoolHandlers = class(TCustomIRCHandlers)
  end;

  TmnIRCState = (isDisconnected, isConnecting, isRegistering, isReady, isDisconnecting);

  TUserMode = (umInvisible, umOperator, umServerNotices, umWallops);
  TUserModes = set of TUserMode;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnClientConnection)
  private
    FIRC: TmnIRCClient;
  protected
    procedure DoReceive(const Data: string); virtual;
    procedure DoLog(const vData: string); virtual;
    procedure Process; override;
  public
    constructor Create(vOwner: TmnConnections; vSocket: TmnConnectionStream); override;
    destructor Destroy; override;
    procedure Connect; override;
  end;

  TOnReceive = procedure(Sender: TObject; vChannel, vMSG: String) of object;
  TOnLogData = procedure(Sender: TObject; vLogType: TIRCLogType; vMsg: String) of object;

  { TmnIRCClient }

  TmnIRCClient = class(TObject)
  private
    FOnLog: TOnLogData;
    FOnReceive: TOnReceive;
    FOnUserModeChanged: TNotifyEvent;

    FPort: String;
    FHost: String;
    FPassword: String;
    FUsername: String;
    FRealName: String;
    FAltNick: String;
    FNick: String;

    FChangeNickTo: String;
    FCurrentNick: String;
    FCurrentHost: String;
    FCurrentUserModes: TUserModes;

    FUserModes: TUserModes;

    FState: TmnIRCState;
    FConnection: TmnIRCConnection;
    FHandlers: TIRCHandlers;
    FPoolHandlers: TIRCPoolHandlers;
    procedure SetAltNick(const Value: String);
    procedure SetNick(const Value: String);
    function GetNick: String;
    procedure SetPassword(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetRealName(const Value: String);
    procedure SetHost(const Value: String);
    function GetHost: String;
    procedure Connected;
    procedure Disconnected;
    procedure SetState(const Value: TmnIRCState);
    procedure Reset;
    procedure SetUsername(const Value: String);
    function GetUserModes: TUserModes;
    procedure SetUserModes(const Value: TUserModes);
    function CreateUserModeCommand(NewModes: TUserModes): String;
  protected
    procedure ReceiveData(vData: String); virtual;
    procedure SendData(vData: String);

    procedure ChannelReceive(vChannel, vMsg: String); virtual;

    procedure UserModeChanged;
    procedure InitHandlers;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Close;

    procedure Log(vLogType: TIRCLogType; Message: String);

    procedure ChannelSend(Channel, Text: String);
    procedure Join(Channel: String);
    procedure Notice(Destination, Text: String);
    procedure Quit(Reason: String);
    property State: TmnIRCState read FState;
    property Connection: TmnIRCConnection read FConnection;
    property Handlers: TIRCHandlers read FHandlers;
    property PoolHandlers: TIRCPoolHandlers read FPoolHandlers;
  public
    property Host: String read GetHost write SetHost;
    property Port: String read FPort write SetPort;
    property Nick: String read GetNick write SetNick;
    property AltNick: String read FAltNick write SetAltNick;
    property RealName: String read FRealName write SetRealName;
    property Password: String read FPassword write SetPassword;
    property Username: String read FUsername write SetUsername;
    property UserModes: TUserModes read GetUserModes write SetUserModes;

    property OnLog: TOnLogData read FOnLog write FOnLog;
    property OnReceive: TOnReceive read FOnReceive write FOnReceive;
    property OnUserModeChanged: TNotifyEvent read FOnUserModeChanged write FOnUserModeChanged;
  end;

  { TPing_IRCHandler }

  TPing_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TPRIVMSG_IRCHandler }

  TPRIVMSG_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TMOTD_IRCHandler }

  TMOTD_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TTOPIC_IRCHandler }

  TTOPIC_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TWELCOME_IRCHandler }

  TWELCOME_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TNICK_IRCHandler }

  TNICK_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TMODE_IRCHandler }

  TMODE_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TErrNicknameInUse_IRCHandler }

  TErrNicknameInUse_IRCHandler = class(TIRCHandler)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

implementation

uses
  SysUtils;

const
  sDefaultPort = '6667';
  sNickName = 'guest';

function ExtractNickFromAddress(Address: String): String;
var
  EndOfNick: Integer;
begin
  Result := '';
  EndOfNick := Pos('!', Address);
  if EndOfNick > 0 then
    Result := Copy(Address, 1, EndOfNick - 1);
end;

{ TErrNicknameInUse_IRCHandler }

procedure TErrNicknameInUse_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  { Handle nick conflicts during the registration process. }
  with Client do
    if FState = isRegistering then
    begin
      if FChangeNickTo = FNick then
        SetNick(FAltNick)
      else
        Quit('Nick conflict');
    end;
end;

{ TMODE_IRCHandler }

procedure TMODE_IRCHandler.DoReceive(vCommand: TIRCCommand);
var
  Index: Integer;
  ModeString: String;
  AddMode: Boolean;
begin
  { Ignore channel mode changes.  Only interested in user mode changes. }
  with Client do
    if vCommand.Params[0] = FCurrentNick then
    begin
      { Copy the token for efficiency reasons. }
      ModeString := vCommand.Params[1];
      AddMode := True;
      for Index := 1 to Length(ModeString) do
      begin
        case ModeString[Index] of
          '+':
            AddMode := True;
          '-':
            AddMode := False;
          'i':
            if AddMode then
              FCurrentUserModes := FCurrentUserModes + [umInvisible]
            else
              FCurrentUserModes := FCurrentUserModes - [umInvisible];
          'o':
            if AddMode then
              FCurrentUserModes := FCurrentUserModes + [umOperator]
            else
              FCurrentUserModes := FCurrentUserModes - [umOperator];
          's':
            if AddMode then
              FCurrentUserModes := FCurrentUserModes + [umServerNotices]
            else
              FCurrentUserModes := FCurrentUserModes - [umServerNotices];
          'w':
            if AddMode then
              FCurrentUserModes := FCurrentUserModes + [umWallops]
            else
              FCurrentUserModes := FCurrentUserModes - [umWallops];
        end;
      end;
      UserModeChanged;
    end;
end;


{ TNICK_IRCHandler }

procedure TNICK_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  { If it is our nick we are changing, then record the change. }
  if UpperCase(ExtractNickFromAddress(vCommand.Source)) = UpperCase(Client.FCurrentNick) then
    Client.FCurrentNick := vCommand.Params[0];
end;

{ TWELCOME_IRCHandler }

procedure TWELCOME_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  with Client do
  begin
    { This should be the very first successful response we get, so set the Current
      host and nick from the values returned in the response. }
    FCurrentHost := vCommand.Source;
    FCurrentNick := vCommand.Params[0];
    SetState(isReady);
    { If a user mode is pre-set, then SendDirect the mode command. }
    if FUserModes <> [] then
      SendData(Format('MODE %s %s', [FCurrentNick, CreateUserModeCommand(FUserModes)]));
  end;
end;

{ TTOPIC_IRCHandler }

procedure TTOPIC_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin

end;

{ TMOTD_IRCHandler }

procedure TMOTD_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive('', vCommand.Text);
end;

{ TIRCHandler }

procedure TIRCHandler.Receive(vCommand: TIRCCommand);
begin
  DoReceive(vCommand);
  Client.Log(lgMsg, Name + ' triggered');
end;

constructor TIRCHandler.Create(AClient: TmnIRCClient);
begin
  FClient := AClient;
end;

{ TPRIVMSG_IRCHandler }

procedure TPRIVMSG_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive(vCommand.Params[0], vCommand.Params[1]);
end;

{ TPing_IRCHandler }

procedure TPing_IRCHandler.DoReceive(vCommand: TIRCCommand);
begin
  { SendDirect the PONG reply to the PING. }
  Client.SendData(Format('PONG %s', [vCommand.Params[0]]));
end;

{ TmnIRCConnection }

procedure TmnIRCConnection.DoReceive(const Data: string);
begin
  FIRC.ReceiveData(Data);
end;

procedure TmnIRCConnection.DoLog(const vData: string);
begin
  FIRC.Log(lgMsg, vData);
end;

procedure TmnIRCConnection.Process;
var
  Line: string;
begin
  inherited Process;
  if Stream.WaitToRead(Stream.Timeout) then
  begin
    Line := Trim(Stream.ReadLine);
    while Line <> '' do
    begin
      DoReceive(Line);
      Line := Trim(Stream.ReadLine);
    end;
  end;
end;

constructor TmnIRCConnection.Create(vOwner: TmnConnections; vSocket: TmnConnectionStream);
begin
  inherited Create(vOwner, vSocket);
end;

destructor TmnIRCConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TmnIRCConnection.Connect;
begin
  SetStream(TIRCSocketStream.Create(FIRC.Host, FIRC.Port, [soNoDelay, soConnectTimeout]));
  Stream.Timeout := -1;
  Stream.EndOfLine := #10;
  inherited Connect;
end;

{ TIRCCommand }

constructor TIRCCommand.Create;
begin
  inherited;
  FParams := TStringList.Create;
end;

destructor TIRCCommand.Destroy;
begin
  FreeAndNil(FParams);
end;

procedure TIRCCommand.SetData(const Value: String);
begin
  { If the string is a CTCP query, then skip the Ctrl-A characters at the start
    and end of the query. }
  FData := Value;
  if FData <> '' then
    Parse(FData);
end;

function ScanString(vStr: string; var vPos:Integer; out vCount: Integer; vChar: Char; vSkip: Boolean = False): Boolean;
var
  start: Integer;
begin
  start := vPos;
  while (vPos <= Length(vStr)) and (vStr[vPos] <> vChar) do
    Inc(vPos);
  vCount := vPos - start;
  { Remove any redundant }
  if vSkip then
    while (vPos <= Length(vStr)) and (vStr[vPos] = vChar) do
      Inc(vPos);
  Result := vCount > 0;
end;

procedure TIRCCommand.Parse(vData: string);
type
  TParseState = (prefex, command, params, message);
var
  State: TParseState;
  p: Integer;
  Start, Count: Integer;
  procedure AddIt(s: string);
  begin
    FParams.Add(s);
  end;
begin
  FParams.Clear;
  State := prefex;
  p := 1;
  while p < Length(vData) do
  begin
    { If the current token is a CTCP query, then look for the end of the
      query instead of the token separator. }
    if vData[p] = '@' then
    begin
      Inc(p); //skip it
      Start := p;
      ScanString(vData, p, Count, cTokenSeparator, True);
      if Count > 0 then
        FTime := MidStr(vData, Start, Count);
      //Inc(State); no do not increase it, we still except prefix
    end
    else if vData[p] = ':' then
    begin
      if State > prefex then
      begin
        FText := MidStr(vData, p + 1, MaxInt);
        AddIt(FText);
        State := message;
        Break;
      end
      else
      begin
        Inc(p); //skip it
        Start := p;
        ScanString(vData, p, Count, cTokenSeparator, True);
        if Count > 0 then
          FSource := MidStr(vData, Start, Count);
        Inc(State);
      end
    end
    else if vData[p] = #1 then //CTCP idk what is this
    begin
      Start := p;
      ScanString(vData, p, Count, #1);
      if Count > 0 then
        AddIt(MidStr(vData, Start, Count));
      Inc(State);
    end
    else
    begin
      Start := p;
      ScanString(vData, p, Count, cTokenSeparator, True);
      if Count > 0 then
      begin
        if State <= command then
        begin
          FName := MidStr(vData, Start, Count);
          State := command;
          Inc(State);
        end
        else
          AddIt(MidStr(vData, Start, Count));
      end
    end;
  end;
end;

{ TCustomIRCHandlers }

function TCustomIRCHandlers.Add(vName, vCode: String; AClass: TIRCHandlerClass): Integer;
var
  AHandler: TIRCHandler;
begin
  Result := IndexOfName(vName);
  if Result >= 0 then
  begin
    //not sure if we want more than one handler
  end;
  AHandler := AClass.Create(Client);
  AHandler.Name := vName;
  AHandler.Code := vCode;
  Result := inherited Add(AHandler);
end;

constructor TCustomIRCHandlers.Create(AClient: TmnIRCClient);
begin
  inherited Create(True);
  FClient := AClient;
end;

destructor TCustomIRCHandlers.Destroy;
begin
  inherited;
end;

procedure TCustomIRCHandlers.Handle(ACommand: TIRCCommand);
var
  AHandler: TIRCHandler;
begin
  for AHandler in Self do
  begin
    if SameText(ACommand.FName, AHandler.Name) or SameText(ACommand.FName, AHandler.Code)  then
      AHandler.Receive(ACommand);
  end;
end;

{ TmnIRCClient }

procedure TmnIRCClient.Close;
begin
  { Try to leave nicely if we can. }
  if FState = isReady then
  begin
    SetState(isDisconnecting);
    SendData('QUIT');
  end
  else
  begin
    SetState(isDisconnecting);
    if FConnection.Connected then
      FConnection.Close;
    Disconnected;
  end;
end;

procedure TmnIRCClient.Connect;
begin
  if (FState = isDisconnected) then
  begin
    FCurrentHost := FHost;
    FCurrentNick := FNick;
    FChangeNickTo := '';
    SetState(isConnecting);
    Connection.Connect; //move it to Process
    Connection.Start;
    Connected;
  end;
end;

constructor TmnIRCClient.Create;
begin
  inherited Create;
  FConnection := TmnIRCConnection.Create(nil, nil);
  FConnection.FIRC := Self;
  FConnection.FreeOnTerminate := False;
  FHost := 'localhost';
  FPort := sDefaultPort;
  FNick := sNickName;
  FAltNick := '';
  FRealName := '';
  FUsername := 'username';
  FPassword := '';
  FState := isDisconnected;
  FHandlers := TIRCHandlers.Create(Self);
  FPoolHandlers := TIRCPoolHandlers.Create(Self);
  InitHandlers;
end;

destructor TmnIRCClient.Destroy;
begin
  Reset;
  FreeAndNil(FConnection); //+
  FreeAndNil(FHandlers);
  FreeAndNil(FPoolHandlers);
  inherited;
end;

procedure TmnIRCClient.Reset;
begin
  SetState(isDisconnecting);
  if Assigned(FConnection) and (FConnection.Connected) then
    FConnection.Close;
end;

procedure TmnIRCClient.SendData(vData: String);
begin
  if Assigned(FConnection) and (FState in [isRegistering, isReady]) then
    FConnection.Stream.WriteLine(vData);
  Log(lgSend, vData)
end;

procedure TmnIRCClient.Log(vLogType: TIRCLogType; Message: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, vLogType, Message);
end;

procedure TmnIRCClient.ChannelSend(Channel, Text: String);
begin
  SendData(Format('PRIVMSG %s :%s', [Channel, Text]));
end;

procedure TmnIRCClient.Join(Channel: String);
begin
  SendData(Format('JOIN %s', [Channel]));
end;

procedure TmnIRCClient.Notice(Destination, Text: String);
begin
  SendData(Format('NOTICE %s :%s', [Destination, Text]));
end;

procedure TmnIRCClient.Disconnected;
begin
  SetState(isDisconnected);
end;

procedure TmnIRCClient.Connected;
begin
  SetState(isRegistering);
  { If a password exists, SendDirect it first. }
  if FPassword <> '' then
    SendData(Format('PASS %s', [FPassword]));
  { SendDirect nick. }
  SetNick(FNick);
  { SendDirect registration. }
  SendData(Format('USER %s %s %s :%s', [FUsername, FUsername, FHost, FRealName]));
end;

procedure TmnIRCClient.SetAltNick(const Value: String);
begin
  FAltNick := Value;
end;

procedure TmnIRCClient.SetNick(const Value: String);
begin
  if Value <> '' then
  begin
    if FState in [isRegistering, isReady] then
    begin
      if Value <> FChangeNickTo then
      begin
        SendData(Format('NICK %s', [Value]));
        FChangeNickTo := Value;
      end;
    end
    else
      FNick := Value;
  end;
end;

procedure TmnIRCClient.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TmnIRCClient.SetPort(const Value: String);
begin
  FPort := Value;
end;

procedure TmnIRCClient.SetRealName(const Value: String);
begin
  FRealName := Value;
end;

procedure TmnIRCClient.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TmnIRCClient.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

procedure TmnIRCClient.SetState(const Value: TmnIRCState);
begin
  if Value <> FState then
  begin
    FState := Value;
  end;
end;

function TmnIRCClient.GetUserModes: TUserModes;
begin
  if FState in [isRegistering, isReady] then
    Result := FCurrentUserModes
  else
    Result := FUserModes;
end;

procedure TmnIRCClient.SetUserModes(const Value: TUserModes);
var
  ModeString: String;
begin
  if FState in [isRegistering, isReady] then
  begin
    ModeString := CreateUserModeCommand(Value);
    if Length(ModeString) > 0 then
      SendData(Format('MODE %s %s', [FCurrentNick, ModeString]));
  end
  else
  begin
    FUserModes := Value;
  end;
end;

procedure TmnIRCClient.ReceiveData(vData: String);
var
  ACommand: TIRCCommand;
begin
  Log(lgReceive, vData);
  ACommand := TIRCCommand.Create;
  ACommand.Data := vData;
  try
    FHandlers.Handle(ACommand);
  finally
    FreeAndNil(ACommand);
  end;
end;

procedure TmnIRCClient.ChannelReceive(vChannel, vMsg: String);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self, vChannel, vMsg);
end;

procedure TmnIRCClient.UserModeChanged;
begin
  if Assigned(FOnUserModeChanged) then
    FOnUserModeChanged(Self);
end;

procedure TmnIRCClient.Quit(Reason: String);
begin
  SendData(Format('QUIT :%s', [Reason]));
end;

procedure TmnIRCClient.InitHandlers;
begin
  FHandlers.Add('PRIVMSG', 'PRIVMSG', TPRIVMSG_IRCHandler);
  FHandlers.Add('PING', 'PING', TPING_IRCHandler);
  FHandlers.Add('MOTD', '372', TMOTD_IRCHandler);
  FHandlers.Add('TOPIC', '332', TMOTD_IRCHandler);
  FHandlers.Add('NICK', 'NICK', TNICK_IRCHandler);
  FHandlers.Add('WELCOME', 'WELCOME', TWELCOME_IRCHandler);
  FHandlers.Add('MODE', 'MODE', TMODE_IRCHandler);
  FHandlers.Add('TOPIC ', '332 ', TTOPIC_IRCHandler);
  FHandlers.Add('ERR_NICKNAMEINUSE', 'ERR_NICKNAMEINUSE', TErrNicknameInUse_IRCHandler);
end;

function TmnIRCClient.GetHost: String;
begin
  if FState in [isRegistering, isReady] then
    Result := FCurrentHost
  else
    Result := FHost;
end;

function TmnIRCClient.GetNick: String;
begin
  if FState in [isRegistering, isReady] then
    Result := FCurrentNick
  else
    Result := FNick;
end;

{ Create the mode command string to SendDirect to the server to modify the user's
  mode.  For example, "+i-s" to add invisible and remove server notices. }
function TmnIRCClient.CreateUserModeCommand(NewModes: TUserModes): String;
const
  ModeChars: array [umInvisible..umWallops] of Char = ('i', 'o', 's', 'w');
var
  ModeDiff: TUserModes;
  Mode: TUserMode;
begin
  Result := '';
  { Calculate user modes to remove. }
  ModeDiff := FCurrentUserModes - NewModes;
  if ModeDiff <> [] then
  begin
    Result := Result + '-';
    for Mode := Low(TUserMode) to High(TUserMode) do
    begin
      if Mode in ModeDiff then
        Result := Result + ModeChars[Mode];
    end;
  end;
  { Calculate user modes to add. }
  ModeDiff := NewModes - FCurrentUserModes;
  if ModeDiff <> [] then
  begin
    Result := Result + '+';
    for Mode := Low(TUserMode) to High(TUserMode) do
    begin
      if Mode in ModeDiff then
        Result := Result + ModeChars[Mode];
    end;
  end;
end;

end.
