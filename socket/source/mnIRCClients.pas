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


    http://chi.cs.uchicago.edu/chirc/irc_examples.html

    https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands
    https://stackoverflow.com/questions/12747781/irc-message-format-clarification

    //Yes we can read in thread and write in main thread
    https://stackoverflow.com/questions/7418093/use-one-socket-in-two-threads
 *}

{TODO
  remove using Params[0] and use AddParam instead
}

interface

uses
  Classes, StrUtils, syncobjs,
  mnClasses, mnSockets, mnSocketStreams, mnClients, mnStreams, mnConnections, mnUtils;

const
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }

  //* https://www.alien.net.au/irc/irc2numerics.html

  IRC_RPL_WELCOME = 001;
  IRC_RPL_MYINFO = 004;
  IRC_RPL_WHOISUSER = 311;
  IRC_RPL_ENDOFWHO = 315;
  IRC_RPL_WHOISACCOUNT = 330;
  IRC_RPL_TOPIC = 332;
  IRC_RPL_TOPICWHOTIME = 333;
  IRC_RPL_NAMREPLY = 353;
  IRC_RPL_WHOSPCRPL = 354;
  IRC_RPL_ENDOFNAMES = 366;
  IRC_RPL_MOTDSTART = 375;
  IRC_RPL_INFO = 371;
  IRC_RPL_MOTD = 372;
  IRC_RPL_INFOSTART = 373;
  IRC_RPL_ENDOFINFO = 374;
  IRC_RPL_ENDOFMOTD = 376;
  IRC_ERR_NICKNAMEINUSE = 433;

  IRC_RPL_HELPSTART = 704;
  IRC_RPL_HELPTXT = 705;
  IRC_RPL_ENDOFHELP = 706;

type

  { TIRCSocketStream }

  TIRCSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  TmnIRCClient = class;

  TIRCProgress = (prgDisconnected, prgConnected, prgReady);

  TIRCState = (
    scProgress,
    scUserInfo, //Nickname and UserModes
    scChannelModes,
    scChannelNames
  );

  TIRCStates = set of TIRCState;
  TIRCMsgType = (mtLog, mtNotice, mtMessage, mtMOTD, mtSend, mtCTCPNotice, mtCTCPMessage, mtWelcome, mtTopic, mtJoin, mtPart, mtQuit);
  TIRCLogType = (lgMsg, lgSend, lgReceive);

  { TIRCCommand }

  TIRCCommand = class(TObject)
  private
    FAddress: string;
    FChannel: string;
    FCTCP: Boolean;
    FMsg: string;
    FNick: string;
    FRaw: String;
    FName: string;

    FTime: string;

    FSource: string;
    FTarget: string;

    FParams: TStringList;
    FText: string;
  protected
    property Raw: String read FRaw;
  public
    constructor Create;
    destructor Destroy; override;

    property Source: string read FSource;
    property Target: string read FTarget;

    property Channel: string read FChannel;
    property Nick: string read FNick;
    property Address: string read FAddress;

    property Name: string read FName;//Name or Code

    property Params: TStringList read FParams;
    property Msg: string read FMsg;
    property Text: string read FText; //Rest body of msg after : also added as last param
    property CTCP: Boolean read FCTCP;
  end;

  { TIRCReceiver }

  TIRCReceiver = class(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure DoReceive(vCommand: TIRCCommand); virtual; abstract;
    function Accept(S: string): Boolean; virtual;
    procedure Receive(vCommand: TIRCCommand); virtual;
    property Client: TmnIRCClient read FClient;
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); virtual;
    procedure Parse(vCommand: TIRCCommand; vData: string); virtual;
  public
    Code: Integer;
    constructor Create(AClient: TmnIRCClient); virtual;
  end;

  { TIRCUserReceiver }

  TIRCUserReceiver= class(TIRCReceiver)
  protected
    procedure Parse(vCommand: TIRCCommand; vData: string); override;
  end;

  TIRCReceiverClass = class of TIRCReceiver;

  { TCustomIRCReceivers }

  //We should use base class TmnCommandClasses, TODO

  TCustomIRCReceivers = class(TmnNamedObjectList<TIRCReceiver>)
  private
    FClient: TmnIRCClient;
  public
    constructor Create(AClient: TmnIRCClient);
    destructor Destroy; override;
    function Add(vName: String; vCode: Integer; AClass: TIRCReceiverClass): Integer;
    procedure Receive(ACommand: TIRCCommand);
    property Client: TmnIRCClient read FClient;
  end;

  { TIRCReceivers }

  TIRCReceivers = class(TCustomIRCReceivers)
  public
    procedure Parse(vData: string);
  end;

  { TIRCUserCommand }

  TIRCUserCommand = class abstract(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure Send(vChannel: string; vMsg: string); virtual; abstract;
  public
    constructor Create(AName: string; AClient: TmnIRCClient);
    property Client: TmnIRCClient read FClient;
  end;

  { TIRCUserCommands }

  TIRCUserCommands = class(TmnNamedObjectList<TIRCUserCommand>)
  private
    FClient: TmnIRCClient;
  public
    constructor Create(AClient: TmnIRCClient);
    property Client: TmnIRCClient read FClient;
  end;

  { TIRCRaw }

  TIRCRaw = class(TObject)
  public
    When: TIRCProgress;
    Raw: string;
  end;

  { TIRCQueueRaws }

  TIRCQueueRaws = class(TmnObjectList<TIRCRaw>)
  public
    procedure Add(Raw: string; When: TIRCProgress); overload;
  end;

  { TIRCUserName }

  TIRCUserLevel = set of (ulVoice, ulHalfOp, ulOp, ulAdmin, ulOwner, ulFake);

  TIRCUserMode = (umInvisible, umAway, umVoice, umHalfOp, umOp, umWallOp, umAdmin, umOwner, umFake, umServerNotices, umSSL);
  TIRCUserModes = set of TIRCUserMode;

  TIRCChannelMode = (cmKey, cmInviteOnly, cmModerated, cmNoOutside, cmRegisteredOnly, cmSecret, cmTopicProtection);
  TIRCChannelModes = set of TIRCChannelMode;

  TIRCUserName = class(TmnNamedObject)
  protected
    procedure ParseName(const AUser: string);
  public
    DisplayName: string; //without prefix
    Level: TIRCUserLevel;
    UserModes: TIRCUserModes;
  end;

  { TIRCChannel }

  TIRCChannel = class(TmnNamedObjectList<TIRCUserName>)
  private
  protected
    Adding: Boolean;
  public
    Name: string; //Channel name
    UserModes: TIRCUserModes;
    procedure Add(vUserName: string); overload;
  end;

  { TIRCChannels }

  TIRCChannels = class(TmnObjectList<TIRCChannel>)
  public
    function Find(const Name: string): TIRCChannel;
    function Found(vChannel: string): TIRCChannel;
  end;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnClientConnection)
  private
    FClient: TmnIRCClient;
    FHost: string;
    FPort: string;
  protected
    procedure Prepare; override;
    procedure Process; override;
    procedure Unprepare; override;

    procedure ReceiveRaws; //To Synch, to be out of thread
    procedure SendRaws;
    procedure SendRaw(S: string); //this called by client out of thread

    property Client: TmnIRCClient read FClient;
  public
    constructor Create(vOwner: TmnConnections; vSocket: TmnConnectionStream); override;
    destructor Destroy; override;
    procedure Connect; override;
    property Host: string read FHost;
    property Port: string read FPort;
  end;

  { TIRCSession }

  TIRCSession = record
    Channels: TIRCChannels;
    Nick: string;
    AllowedUserModes: TIRCUserModes;
    AllowedChannelModes: TIRCChannelModes;
    ServerVersion: string;
    Server: string;
    procedure Clear;
  end;

  TIRCAuth = (authNone, authPASS, authIDENTIFY);

  { TmnIRCClient }

  TmnIRCClient = class(TObject)
  private
    FAuth: TIRCAuth;
    FNicks: TStringList;
    FPort: String;
    FHost: String;
    FPassword: String;
    FQueueReceives: TIRCQueueRaws;
    FUsername: String;
    FRealName: String;
    FNick: String;
    FProgress: TIRCProgress;
    FSession: TIRCSession;
    FConnection: TmnIRCConnection;
    FReceivers: TIRCReceivers;
    FQueueSends: TIRCQueueRaws;
    FUserCommands: TIRCUserCommands;
    FLock: TCriticalSection;
    FUseUserCommands: Boolean;
    function GetActive: Boolean;
    procedure SetAuth(AValue: TIRCAuth);
    procedure SetNicks(AValue: TStringList);
    procedure SetPassword(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetRealName(const Value: String);
    procedure SetHost(const Value: String);
    procedure Connected;
    procedure Disconnected;
    procedure Close;
    procedure SetUsername(const Value: String);
    //function BuildUserModeCommand(NewModes: TIRCUserModes): String;
  protected
    NickIndex: Integer;
    procedure SetState(const Value: TIRCProgress);

    procedure Changed(vStates: TIRCStates; vChannel: string = ''; vNick: string = '');
    procedure Receive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String); virtual;

    procedure ReceiveRaw(vData: String); virtual;
    procedure SendRaw(vData: String; vQueueAt: TIRCProgress = prgDisconnected);

    procedure DoChanged(vStates: TIRCStates; vChannel: string; vNick: string); virtual;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String); virtual;

    procedure Init; virtual;
    property Connection: TmnIRCConnection read FConnection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    procedure ChannelSend(vChannel, vMsg: String);

    procedure SetMode(const Value: TIRCUserModes);
    procedure SetNick(const ANick: String);
    procedure SetTopic(AChannel: String; const ATopic: String);
    procedure Join(Channel: String; Password: string = '');
    procedure Notice(vChannel, vMsg: String);
    procedure Quit(Reason: String);

    property Progress: TIRCProgress read FProgress;
    property Receivers: TIRCReceivers read FReceivers;
    property QueueSends: TIRCQueueRaws read FQueueSends;
    property QueueReceives: TIRCQueueRaws read FQueueReceives;
    property UserCommands: TIRCUserCommands read FUserCommands;
    property Lock: TCriticalSection read FLock;
    property Active: Boolean read GetActive;
  public
    property Host: String read FHost write SetHost;
    property Port: String read FPort write SetPort;
    property Nick: String read FNick write FNick;
    property Nicks: TStringList read FNicks write SetNicks; //Alter nick names to use it if already used nick
    property RealName: String read FRealName write SetRealName;
    property Password: String read FPassword write SetPassword;
    property Username: String read FUsername write SetUsername;
    property Session: TIRCSession read FSession;
    property UseUserCommands: Boolean read FUseUserCommands write FUseUserCommands default True;
    property Auth: TIRCAuth read FAuth write SetAuth;
  end;

  { TPing_IRCReceiver }

  TPing_IRCReceiver = class(TIRCReceiver)
  protected
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); override;
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TIRCMsgReceiver }

  TIRCMsgReceiver = class abstract(TIRCUserReceiver) //just for inheritance
  protected
    procedure Parse(vCommand: TIRCCommand; vData: string); override;
  end;

  { TPRIVMSG_IRCReceiver }

  TPRIVMSG_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TNOTICE_IRCReceiver }

  TNOTICE_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TJOIN_IRCReceiver }

  TJOIN_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TPART_IRCReceiver }

  TPART_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TQUIT_IRCReceiver }

  TQUIT_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TMOTD_IRCReceiver }

  TMOTD_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TTOPIC_IRCReceiver }

  TTOPIC_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TWELCOME_IRCReceiver }

  TWELCOME_IRCReceiver = class(TIRCReceiver)
  protected
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); override;
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TMYINFO_IRCReceiver }

  TMYINFO_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TNICK_IRCReceiver }

  TNICK_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TMODE_IRCReceiver }

  TMODE_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TNAMREPLY_IRCReceiver }

  TNAMREPLY_IRCReceiver = class(TIRCReceiver)
  protected
  public
    function Accept(S: string): Boolean; override;
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { THELP_IRCReceiver }

  THELP_IRCReceiver = class(TIRCReceiver)
  protected
  public
    function Accept(S: string): Boolean; override;
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TErrNicknameInUse_IRCReceiver }

  TErrNicknameInUse_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TRAW_UserCommand }

  TRaw_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TJoin_UserCommand }

  TJoin_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TMode_UserCommand }

  TMode_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TSend_UserCommand }

  TSend_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TMe_UserCommand }

  TMe_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { THelp_UserCommand }

  THelp_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { THistory_UserCommand }

  THistory_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TNotice_UserCommand }

  TNotice_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TCNotice_UserCommand }

  TCNotice_UserCommand = class(TIRCUserCommand) //CTCP
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

implementation

uses
  SysUtils;

const
  sDefaultPort = '6667';
  sNickName = 'guest';

function ScanString(vStr: string; var vPos:Integer; out vCount: Integer; vChar: Char; vSkip: Boolean = False): Boolean;
var
  start: Integer;
begin
  start := vPos;
  while (vPos <= Length(vStr)) and (vStr[vPos] <> vChar) do
    Inc(vPos);
  vCount := vPos - start;

  if vSkip then
    while (vPos <= Length(vStr)) and (vStr[vPos] = vChar) do
      Inc(vPos);
  Result := vCount > 0;
end;

procedure ParseUserInfo(Source: string; out Nick, Address: string);
var
  p, c: Integer;
begin
  p := 1;
  if ScanString(Source, p, c, '!', true) then
  begin
    Nick := MidStr(Source, 1, c);
    Address := MidStr(Source, c + 1, MaxInt);
  end;
end;

procedure StringToUserMode(ModeStr: string; var Modes: TIRCUserModes);
var
  Index: Integer;
  Add: Boolean;
  procedure AddIt(Mode: TIRCUserMode);
  begin
    if Add then
      Modes := Modes + [Mode]
    else
      Modes := Modes - [Mode];
  end;
begin
  Add := True;
  for Index := 1 to Length(ModeStr) do
  begin
    case ModeStr[Index] of
      '+':
        Add := True;
      '-':
        Add := False;
      'i':
        AddIt(umInvisible);
      'a':
        AddIt(umAway);
      'v':
        AddIt(umVoice);
      'h':
        AddIt(umHalfOp);
      'o':
        AddIt(umOp);
      'w':
        AddIt(umWallOp);
      's':
        AddIt(umServerNotices);
      'Z':
        AddIt(umSSL);
    end;
  end;
end;

//* https://wiki.zandronum.com/IRC:Channel_Modes

procedure StringToChannelMode(ModeStr: string; var Modes: TIRCChannelModes);
var
  Index: Integer;
  Add: Boolean;
  procedure AddIt(Mode: TIRCChannelMode);
  begin
    if Add then
      Modes := Modes + [Mode]
    else
      Modes := Modes - [Mode];
  end;
begin
  Add := True;
  for Index := 1 to Length(ModeStr) do
  begin
    case ModeStr[Index] of
      '+':
        Add := True;
      '-':
        Add := False;
      't':
        AddIt(cmTopicProtection);
      'k':
        AddIt(cmKey);
      'I':
        AddIt(cmInviteOnly);
      'm':
        AddIt(cmModerated);
      'n':
        AddIt(cmNoOutside);
      'R':
        AddIt(cmRegisteredOnly);
      's':
        AddIt(cmSecret);
    end;
  end;
end;

function ExtractNickFromAddress(Address: String): String;
var
  EndOfNick: Integer;
begin
  Result := '';
  EndOfNick := Pos('!', Address);
  if EndOfNick > 0 then
    Result := Copy(Address, 1, EndOfNick - 1);
end;

{ TIRCSession }

procedure TIRCSession.Clear;
begin
  Channels.Clear;
  Nick := '';
  AllowedUserModes := [];
  AllowedChannelModes := [];
  ServerVersion := '';
  Server := '';
end;

{ TMode_UserCommand }

procedure TMode_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('MODE ' + vChannel + ' '+ vMsg, prgReady);
end;

{ THistory_UserCommand }

procedure THistory_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('HISTORY ' + vChannel + ' ' + vMsg, prgReady);
end;

{ THELP_IRCReceiver }

function THELP_IRCReceiver.Accept(S: string): Boolean;
begin
  Result := inherited Accept(S);
  if (S = IntToStr(IRC_RPL_HELPTXT))
    or (S = IntToStr(IRC_RPL_ENDOFHELP)) then
  begin
    Result := True;
  end;
end;

procedure THELP_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtWelcome, '', '', vCommand.Text);
end;

{ THelp_UserCommand }

procedure THelp_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('HELP ' + vMsg, prgReady);
end;

{ TMe_UserCommand }

procedure TMe_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('PRIVMSG ' + vChannel + ' :' + #01 + 'ACTION ' + vMsg + #01, prgReady);
end;

{ TCNotice_UserCommand }

procedure TCNotice_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('NOTICE ' + vChannel + ' :' + #01 + vMsg + #01, prgReady);
end;

{ TIRCMsgReceiver }

procedure TIRCMsgReceiver.Parse(vCommand: TIRCCommand; vData: string);
begin
  inherited Parse(vCommand, vData);
  vCommand.FMsg := vCommand.Params[0];
  vCommand.Params.Delete(0);
  if LeftStr(vCommand.FMsg, 1) = #01 then
  begin
    vCommand.FCTCP := True;
    vCommand.FMsg := DequoteStr(vCommand.FMsg, #01);
  end;
end;

{ TNotice_UserCommand }

procedure TNotice_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('NOTICE ' + vChannel + ' :' + vMsg, prgReady);
end;

{ TIRCUserName }

procedure TIRCUserName.ParseName(const AUser: string);
var
  c: Char;
  i: Integer;
begin
  Level := [];
  for i := Low(AUser) to High(AUser) do
  begin
    c := AUser[i];
    if CharInSet(c, ['&', '~', '%', '+', '@']) then
    begin
      if AUser[i] = '~' then
        Level:= Level + [ulOwner, ulOp, ulHalfOp, ulAdmin]
      else if AUser[i] = '&' then
        Level := Level + [ulOp, ulHalfOp, ulAdmin]
      else if AUser[i] = '@' then
        Level := Level + [ulOp, ulHalfOp]
      else if AUser[i] = '%' then
        Level := Level + [ulHalfOp]
      else if AUser[i] = '+' then
        Level := Level + [ulVoice];
    end
    else
    begin
      Name := MidStr(AUser, i, MaxInt);
      DisplayName := Name;
      break;
    end;
  end;
end;


{ TSend_UserCommand }

procedure TSend_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('PRIVMSG ' + vMsg, prgReady);
end;

{ TIRCUserReceiver }

procedure TIRCUserReceiver.Parse(vCommand: TIRCCommand; vData: string);
begin
  inherited Parse(vCommand, vData);
  ParseUserInfo(vCommand.Source, vCommand.FNick, vCommand.FAddress);
  vCommand.FTarget := vCommand.Params[0];
  vCommand.Params.Delete(0);
  if LeftStr(vCommand.Target, 1) = '#' then
    vCommand.FChannel := vCommand.Target
  else if vCommand.FAddress <> '' then
    vCommand.FChannel := vCommand.Nick
  else
    vCommand.FChannel := '';
end;

{ TMYINFO_IRCReceiver }

procedure TMYINFO_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.FSession.Nick := vCommand.Params[0];
  Client.FSession.Server := vCommand.Params[1];
  Client.FSession.ServerVersion := vCommand.Params[2];
  StringToUserMode(vCommand.Params[3], Client.FSession.AllowedUserModes);
  StringToChannelMode(vCommand.Params[4], Client.FSession.AllowedChannelModes);
  Client.Changed([scUserInfo]);
end;

{ TNOTICE_IRCReceiver }

procedure TNOTICE_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtNotice, vCommand.Channel, vCommand.Nick, vCommand.Msg);
end;

{ TQUIT_IRCReceiver }

procedure TQUIT_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtQuit, vCommand.Channel, vCommand.Nick, '');
end;

{ TPART_IRCReceiver }

procedure TPART_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtPart, vCommand.Channel, vCommand.Nick, vCommand.Params[0]);
end;

{ TJOIN_IRCReceiver }

procedure TJOIN_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtJoin, vCommand.Channel, vCommand.Nick, '');
end;

{ TIRCQueueRaws }

procedure TIRCQueueRaws.Add(Raw: string; When: TIRCProgress);
var
  Item: TIRCRaw;
begin
  Item := TIRCRaw.Create;
  Item.When := When;
  Item.Raw := Raw;
  Add(Item);
end;

{ TIRCChannel }

procedure TIRCChannel.Add(vUserName: string);
var
  aUserName: TIRCUserName;
begin
  aUserName := TIRCUserName.Create;
  aUserName.Name := vUserName;
  aUserName.ParseName(aUserName.Name);
  Add(aUserName);
end;

{ TIRCChannels }

function TIRCChannels.Found(vChannel: string): TIRCChannel;
begin
  Result := Find(vChannel);
  if Result = nil then
  begin
    Result := TIRCChannel.Create;
    Result.Name := vChannel;
    Add(Result);
  end;
end;

function TIRCChannels.Find(const Name: string): TIRCChannel;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

{ TIRCReceivers }

procedure TIRCReceivers.Parse(vData: string);
type
  TParseState = (prefix, command);
var
  p: Integer;
  aState: TParseState;
  Start, Count: Integer;
  aCommand: TIRCCommand;
  aReceiver: TIRCReceiver;
  ABody: string;
begin
  if vData <> '' then
  begin
    aCommand := TIRCCommand.Create;
    try
      aCommand.FRaw := vData;
      aState := prefix;
      p := 1;
      while p < Length(vData) do
      begin
        if vData[p] = '@' then
        begin
          Inc(p);
          Start := p;
          if ScanString(vData, p, Count, cTokenSeparator, True) then
            aCommand.FTime := MidStr(vData, Start, Count);
        end
        else if (p = 1) and (vData[p] = #1) then //idk what is this
        begin
          Inc(p); //skip it, what the hell is that!!! (yes talking to my self)
        end
        else if vData[p] = ':' then
        begin
          if aState < command then
          begin
            Inc(p); //skip it
            Start := p;
            if ScanString(vData, p, Count, cTokenSeparator, True) then
              aCommand.FSource := MidStr(vData, Start, Count);
            Inc(aState);
          end
          else
            raise Exception.Create('Wrong place of ":"');
        end
        else
        begin
          Start := p;
          if ScanString(vData, p, Count, cTokenSeparator, True) then
          begin
            aCommand.FName := MidStr(vData, Start, Count);
            ABody := MidStr(vData, Start + Count + 1, MaxInt);
            aState := command;
            Inc(aState);
          end;
          break;
        end;
      end;

      for aReceiver in Self do
      begin
        if aReceiver.Accept(aCommand.Name) then
        begin
          aReceiver.Parse(aCommand, ABody);
          aReceiver.Receive(aCommand);
        end;
      end;

    finally
      FreeAndNil(aCommand);
    end;
  end;
end;

{ TRaw_UserCommand }

procedure TRaw_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw(vMsg, prgConnected);
end;

{ TNAMREPLY_IRCReceiver }

function TNAMREPLY_IRCReceiver.Accept(S: string): Boolean;
begin
  Result := inherited Accept(S);
  if S = IntToStr(IRC_RPL_ENDOFNAMES) then
  begin
    Result := True;
  end;
end;

procedure TNAMREPLY_IRCReceiver.DoReceive(vCommand: TIRCCommand);
var
  aChannelName: string;
  aChannel: TIRCChannel;
  aUsers: TStringList;
  i: Integer;
begin
  // triggered by raw /NAMES
  //:server 353 zaher = #support user1
  //:server 353 zaherdirkey = ##support :user1 user2 user3

  if vCommand.Name = IntToStr(IRC_RPL_ENDOFNAMES) then
  begin
    aChannelName := vCommand.Params[1];
    aChannel := Client.Session.Channels.Found(aChannelName);
    if aChannel <> nil then
    begin
      aChannel.Adding := False;
      Client.Changed([scChannelNames], aChannel.Name);
    end;
  end
  else
  begin
    aChannelName := vCommand.Params[2];
    aChannel := Client.Session.Channels.Found(aChannelName);
    aUsers := TStringList.Create;
    try
      StrToStrings(vCommand.Params[vCommand.Params.Count - 1], aUsers, [' '], []);
      if not aChannel.Adding then
        aChannel.Clear;
      aChannel.Adding := True;
      for i := 0 to aUsers.Count -1 do
        aChannel.Add(aUsers[i]);
    finally
      aUsers.Free;
    end;
  end;
end;

{ TIRCUserCommands }

constructor TIRCUserCommands.Create(AClient: TmnIRCClient);
begin
  inherited Create(true);
  FClient := AClient;
end;

{ TIRCUserCommand }

constructor TIRCUserCommand.Create(AName: string; AClient: TmnIRCClient);
begin
  inherited Create;
  FClient := AClient;
  Name := AName;
end;

{ TJoin_UserCommand }

procedure TJoin_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('JOIN ' + vMsg, prgReady);
end;

{ TErrNicknameInUse_IRCReceiver }

procedure TErrNicknameInUse_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  with Client do
    if FProgress = prgConnected then
    begin
      if NickIndex >= Nicks.Count then
        Quit('Nick conflict')
      else
      begin
        SetNick(Nicks[NickIndex]);
        Inc(NickIndex)
      end;
    end;
end;

{ TMODE_IRCReceiver }

procedure TMODE_IRCReceiver.DoReceive(vCommand: TIRCCommand);
var
  aChannel: TIRCChannel;
begin
  vCommand.FChannel := vCommand.Params[1];
  aChannel := Client.FSession.Channels.Found(vCommand.Channel);
  StringToUserMode(vCommand.Params[1], aChannel.UserModes);
  Client.Changed([scUserInfo], vCommand.Channel);
end;

{ TNICK_IRCReceiver }

procedure TNICK_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.FSession.Nick := vCommand.Params[0];
  Client.Changed([scUserInfo]);
end;

{ TWELCOME_IRCReceiver }

procedure TWELCOME_IRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  inherited;
  case AIndex of
    0: vCommand.FNick := AValue;
    1: vCommand.FMsg := AValue;
  end;
end;

procedure TWELCOME_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  with Client do
  begin
    Receive(mtWelcome, '', vCommand.Nick, vCommand.Msg);
    SetState(prgReady);
  end;
end;

{ TTOPIC_IRCReceiver }

procedure TTOPIC_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtTopic, vCommand.Params[1], '', vCommand.Params[2]);
end;

{ TMOTD_IRCReceiver }

procedure TMOTD_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtMOTD, '', '', vCommand.Text);
end;

{ TIRCReceiver }

procedure TIRCReceiver.Receive(vCommand: TIRCCommand);
begin
  DoReceive(vCommand);
end;

procedure TIRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  vCommand.Params.Add(AValue);
end;

procedure TIRCReceiver.Parse(vCommand: TIRCCommand; vData: string);
var
  p: Integer;
  Start, Count: Integer;
  Index: Integer;
begin
  vCommand.Params.Clear;
  Index := 0;
  if vData <> '' then
  begin
    p := 1;
    while p < Length(vData) do
    begin
      if vData[p] = ':' then
      begin
        vCommand.FText := MidStr(vData, p + 1, MaxInt);
        AddParam(vCommand, Index, vCommand.Text);
        Index := Index + 1;
        Break;
      end
      else
      begin
        Start := p;
        ScanString(vData, p, Count, cTokenSeparator, True);
        if Count > 0 then
        begin
          AddParam(vCommand, Index, MidStr(vData, Start, Count));
          Index := Index + 1;
        end;
      end;
    end;
  end;
end;

function TIRCReceiver.Accept(S: string): Boolean;
begin
  Result := ((Name <> '') and SameText(S, Name)) or ((Code > 0) and (StrToIntDef(S, 0) = Code));
end;

constructor TIRCReceiver.Create(AClient: TmnIRCClient);
begin
  FClient := AClient;
end;

{ TPRIVMSG_IRCReceiver }

procedure TPRIVMSG_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtMessage, vCommand.Channel, vCommand.Nick, vCommand.Msg);
end;

{ TPing_IRCReceiver }

procedure TPing_IRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  if AIndex = 0 then
    vCommand.FMsg := AValue
  else
    inherited;
end;

procedure TPing_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.SendRaw(Format('PONG %s', [vCommand.Msg]));
end;

{ TmnIRCConnection }

procedure TmnIRCConnection.Prepare;
begin
  inherited Prepare;
  Connect;
  Queue(Client.Connected);
end;

procedure TmnIRCConnection.SendRaws;
var
  i: Integer;
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    Client.Lock.Enter;
    try
      i := 0;
      while i < Client.QueueSends.Count do
      begin
        if Client.QueueSends[i].When <= Client.Progress then
        begin
          temp.Add(Client.QueueSends[i].Raw);
          Client.QueueSends.Delete(i);
        end
        else
          inc(i);
      end;
    finally
      Client.Lock.Leave;
    end;

    for i := 0 to temp.Count -1 do
    begin
      SendRaw(temp[i]);
    end;
  finally
    FreeAndNil(temp);
  end;
end;

procedure TmnIRCConnection.Process;
var
  aLine: string;
begin
  inherited Process;
  Queue(SendRaws);

  Stream.ReadLine(aLine, True);
  aLine := Trim(aLine);

  while (aLine <> '') and Active do
  begin
    {$ifdef FPC}
      {.$IF FPC_FULLVERSION>=30101}
        //For future when Anonymous enabled in FPC
      {.$endif}
    {$endif}

    Client.Lock.Enter;
    try
      Client.QueueReceives.Add(aLine, prgConnected);
    finally
      Client.Lock.Leave;
    end;

    Queue(ReceiveRaws);
    Queue(SendRaws);

    Stream.ReadLine(aLine, True);
    aLine := Trim(aLine);
  end;
end;

procedure TmnIRCConnection.Unprepare;
begin
  inherited;
  if Connected then
    Disconnect(true);
  Synchronize(Client.Disconnected);
end;

procedure TmnIRCConnection.ReceiveRaws;
var
  i: Integer;
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    Client.Lock.Enter; //do not lock for long time
    try
      i := 0;
      while i < Client.QueueReceives.Count do
      begin
        if Client.QueueReceives[i].When <= Client.Progress then
        begin
          temp.Add(Client.QueueReceives[i].Raw);
          Client.QueueReceives.Delete(i);
        end
        else
          inc(i);
      end;
    finally
      Client.Lock.Leave;
    end;

    for i := 0 to temp.Count -1 do
    begin
      Client.ReceiveRaw(temp[i]);
    end;
  finally
    FreeAndNil(temp);
  end;
end;

procedure TmnIRCConnection.SendRaw(S: string);
begin
  if Stream.Connected then
  begin
    Stream.WriteLine(S);
    Client.Receive(mtLog, '', '', S); //it is in main thread
  end;
end;

constructor TmnIRCConnection.Create(vOwner: TmnConnections; vSocket: TmnConnectionStream);
begin
  inherited;
end;

destructor TmnIRCConnection.Destroy;
begin
  inherited;
end;

procedure TmnIRCConnection.Connect;
begin
  //Log('Connecting...');
  SetStream(TIRCSocketStream.Create(Host, Port, [soNoDelay, soSafeConnect, soKeepIfReadTimout, soSetReadTimeout, soConnectTimeout]));
  Stream.Timeout := 5 * 1000;
  //Stream.Timeout := WaitForEver;
  Stream.EndOfLine := #10;
  inherited;
  {if Connected then
    Log('Connected successed')
  else
    Log('Connected failed');}
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

{ TCustomIRCReceivers }

function TCustomIRCReceivers.Add(vName: String; vCode: Integer; AClass: TIRCReceiverClass): Integer;
var
  AReceiver: TIRCReceiver;
begin
  Result := IndexOfName(vName);
  if Result >= 0 then
  begin
    //not sure if we want more than one Receiver
  end;
  AReceiver := AClass.Create(Client);
  AReceiver.Name := vName;
  AReceiver.Code := vCode;
  Result := inherited Add(AReceiver);
end;

constructor TCustomIRCReceivers.Create(AClient: TmnIRCClient);
begin
  inherited Create(True);
  FClient := AClient;
end;

destructor TCustomIRCReceivers.Destroy;
begin
  inherited;
end;

procedure TCustomIRCReceivers.Receive(ACommand: TIRCCommand);
var
  AReceiver: TIRCReceiver;
begin
  for AReceiver in Self do
  begin
    if AReceiver.Accept(ACommand.Name) then
      AReceiver.Receive(ACommand);
  end;
end;

{ TmnIRCClient }

procedure TmnIRCClient.Disconnect;
begin
  if FProgress = prgReady then
    SendRaw('QUIT');
  Close;
end;

procedure TmnIRCClient.Connect;
begin
  if (FProgress = prgDisconnected) then
  begin
    FreeAndNil(FConnection);
    FConnection := TmnIRCConnection.Create(nil, nil);
    FConnection.FClient := Self;
    FConnection.FreeOnTerminate := false;
    Connection.FHost := Host;
    Connection.FPort := Port;
    Connection.Start;
  end;
end;

constructor TmnIRCClient.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FNicks := TStringList.Create;
  FUseUserCommands := True;

  FSession.Channels := TIRCChannels.Create(True);

  FHost := 'localhost';
  FPort := sDefaultPort;
  FNick := sNickName;
  FRealName := 'username';
  FUsername := 'username';
  FPassword := '';
  FProgress := prgDisconnected;
  FReceivers := TIRCReceivers.Create(Self);
  FUserCommands :=TIRCUserCommands.Create(Self);
  FQueueSends := TIRCQueueRaws.Create(True);
  FQueueReceives := TIRCQueueRaws.Create(True);
  Init;
end;

destructor TmnIRCClient.Destroy;
begin
  Close;
  FreeAndNil(FConnection);
  FreeAndNil(FUserCommands);
  FreeAndNil(FQueueSends);
  FreeAndNil(FQueueReceives);
  FreeAndNil(FReceivers);
  FreeAndNil(FLock);
  FreeAndNil(FNicks);
  FreeAndNil(FSession.Channels);
  inherited;
end;

procedure TmnIRCClient.Close;
begin
  if Assigned(FConnection) and (FConnection.Connected) then
  begin
    FConnection.Close;
    FConnection.WaitFor;
  end;
end;

procedure TmnIRCClient.SendRaw(vData: String; vQueueAt: TIRCProgress);
begin
  if Assigned(FConnection) then
  begin
    if (vQueueAt <= Progress) then
      FConnection.SendRaw(vData)
    else
    begin
      Lock.Enter;
      try
        QueueSends.Add(vData, vQueueAt);
      finally
        Lock.Leave;
      end;
    end;
  end;
end;

procedure TmnIRCClient.ChannelSend(vChannel, vMsg: String);
var
  aCMD: TIRCUserCommand;
  aCMDProcessed: Boolean;
  s, m: string;
  p, c: Integer;
begin
  aCMDProcessed := False;
  if FUseUserCommands and (UserCommands.Count > 0) and (LeftStr(vMsg, 1) = '/') then
  begin
    p := 2;
    if ScanString(vMsg, p, c, ' ', true) then
    begin
      m := MidStr(vMsg, 2, c);
      s := MidStr(vMsg,  3 + c, MaxInt);
      aCmd := nil;
      for aCmd in UserCommands do
      begin
        if SameText(aCmd.Name, m) then
        begin
          aCmd.Send(vChannel, s);
          aCMDProcessed := True;
        end;
      end;
      if not aCMDProcessed then
      begin
        SendRaw(Format(UpperCase(m) + ' %s :%s', [vChannel, s]), prgReady);
        aCMDProcessed := True;
      end;
    end;
  end;

  if not aCMDProcessed then
  begin
    SendRaw(Format('PRIVMSG %s :%s', [vChannel, vMsg]), prgReady);
    Receive(mtSend, vChannel, Session.Nick, vMsg);
  end;
end;

procedure TmnIRCClient.Join(Channel: String; Password: string);
begin
  SendRaw(Format('JOIN %s %s', [Channel, Password]), prgReady);
end;

procedure TmnIRCClient.Notice(vChannel, vMsg: String);
begin
  SendRaw(Format('NOTICE %s :%s', [vChannel, vMsg]), prgReady);
end;

procedure TmnIRCClient.Disconnected;
begin
  SetState(prgDisconnected);
  FSession.Nick := '';
  FSession.AllowedUserModes := [];
  FSession.AllowedChannelModes := [];
  FSession.Channels.Clear;
end;

procedure TmnIRCClient.Connected;
begin
  SetState(prgConnected);
  SetNick(FNick);
  SendRaw(Format('USER %s 0 * :%s', [FUsername, FRealName]));
  if FPassword <> '' then
  begin
    if Auth = authPass then
      SendRaw(Format('PASS %s', [FPassword]))
    else if Auth = authIDENTIFY then
      SendRaw(Format('NICKSERV IDENTIFY %s %s', [FUsername, FPassword]));
  end;
end;

procedure TmnIRCClient.SetNick(const ANick: String);
begin
  if ANick <> '' then
  begin
    if FProgress in [prgConnected, prgReady] then
      SendRaw(Format('NICK %s', [ANick]));
  end
end;

procedure TmnIRCClient.SetTopic(AChannel: String; const ATopic: String);
begin
  SendRaw(Format('TOPIC %s %s', [AChannel, ATopic]));
end;

procedure TmnIRCClient.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

function TmnIRCClient.GetActive: Boolean;
begin
  Result := (FConnection <> nil) and FConnection.Active;
end;

procedure TmnIRCClient.SetAuth(AValue: TIRCAuth);
begin
  if FAuth =AValue then Exit;
  FAuth :=AValue;
end;

procedure TmnIRCClient.SetNicks(AValue: TStringList);
begin
  FNicks.Assign(AValue);
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

procedure TmnIRCClient.SetState(const Value: TIRCProgress);
begin
  if Value <> FProgress then
  begin
    FProgress := Value;
    Changed([scProgress]);
  end;
end;

procedure TmnIRCClient.SetMode(const Value: TIRCUserModes);
var
  ModeString: String;
begin
  {ModeString := BuildUserModeCommand(Value);
  if Length(ModeString) > 0 then
    SendRaw(Format('MODE %s %s', [FSession.Nick, ModeString]), prgConnected);}
end;

procedure TmnIRCClient.ReceiveRaw(vData: String);
begin
  Receive(mtLog, '', '', '<' + vData);
  Receivers.Parse(vData);
end;

procedure TmnIRCClient.DoChanged(vStates: TIRCStates; vChannel: string; vNick: string);
begin

end;

procedure TmnIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
begin
end;

procedure TmnIRCClient.Quit(Reason: String);
begin
  SendRaw(Format('QUIT :%s', [Reason]));
end;

{function TmnIRCClient.BuildUserModeCommand(NewModes: TIRCUserModes): String;
const
  ModeChars: array [umInvisible..umWallOp] of Char = ('i', 'o', 's', 'w');
var
  ModeDiff: TIRCUserModes;
  Mode: TIRCUserMode;
begin
  Result := '';

  ModeDiff := FSession.UserModes - NewModes;
  if ModeDiff <> [] then
  begin
    Result := Result + '-';
    for Mode := Low(TIRCUserMode) to High(TIRCUserMode) do
    begin
      if Mode in ModeDiff then
        Result := Result + ModeChars[Mode];
    end;
  end;

  ModeDiff := NewModes - FSession.UserModes;
  if ModeDiff <> [] then
  begin
    Result := Result + '+';
    for Mode := Low(TIRCUserMode) to High(TIRCUserMode) do
    begin
      if Mode in ModeDiff then
        Result := Result + ModeChars[Mode];
    end;
  end;
end;}

procedure TmnIRCClient.Changed(vStates: TIRCStates; vChannel: string; vNick: string);
begin
  DoChanged(vStates, vChannel, vNick);
end;

procedure TmnIRCClient.Receive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
begin
  DoReceive(vMsgType, vChannel, vUser, vMsg)
end;

procedure TmnIRCClient.Init;
begin
  Receivers.Add('PRIVMSG', 0, TPRIVMSG_IRCReceiver);
  Receivers.Add('NOTICE', 0, TNOTICE_IRCReceiver);

  Receivers.Add('MODE', 0, TMODE_IRCReceiver);
  Receivers.Add('PING', 0, TPING_IRCReceiver);
  Receivers.Add('MOTD', IRC_RPL_MOTD, TMOTD_IRCReceiver);
  Receivers.Add('TOPIC', IRC_RPL_TOPIC, TTOPIC_IRCReceiver);
  Receivers.Add('NICK', 0, TNICK_IRCReceiver);
  Receivers.Add('WELCOME', IRC_RPL_WELCOME, TWELCOME_IRCReceiver);
  Receivers.Add('MYINFO', IRC_RPL_MYINFO, TMYINFO_IRCReceiver);
  Receivers.Add('JOIN', 0, TJOIN_IRCReceiver);
  Receivers.Add('PART', 0, TPART_IRCReceiver);
  Receivers.Add('QUIT', 0, TQUIT_IRCReceiver);
  Receivers.Add('NAMREPLY', IRC_RPL_NAMREPLY, TNAMREPLY_IRCReceiver);

  Receivers.Add('HELPSTART', IRC_RPL_HELPSTART, THELP_IRCReceiver);

  Receivers.Add('ERR_NICKNAMEINUSE', IRC_ERR_NICKNAMEINUSE, TErrNicknameInUse_IRCReceiver);

  UserCommands.Add(TRaw_UserCommand.Create('Raw', Self));
  UserCommands.Add(TRaw_UserCommand.Create('Msg', Self)); //Alias of Raw
  UserCommands.Add(TJoin_UserCommand.Create('Join', Self));
  UserCommands.Add(TJoin_UserCommand.Create('j', Self)); //alias of Join
  UserCommands.Add(TMode_UserCommand.Create('Mode', Self));
  UserCommands.Add(TSend_UserCommand.Create('Send', Self));
  UserCommands.Add(TMe_UserCommand.Create('Me', Self));
  UserCommands.Add(TNotice_UserCommand.Create('Notice', Self));
  UserCommands.Add(TCNotice_UserCommand.Create('CNotice', Self));
  UserCommands.Add(THELP_UserCommand.Create('HELP', Self));
end;

end.
