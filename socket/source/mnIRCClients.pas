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
  mnClasses, mnSockets, mnClients, mnStreams, mnConnections, mnUtils;

const
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }

  //* https://www.alien.net.au/irc/irc2numerics.html

  IRC_RPL_WELCOME = 001;
  IRC_RPL_MYINFO = 004;
  IRC_RPL_WHOISUSER = 311;
  IRC_RPL_ENDOFWHO = 315;
  IRC_RPL_CHANNEL_URL = 328;
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

  TIRCSocketStream = Class(TmnClientSocket)
  protected
    procedure DoHandleError(var Handle: Boolean; AError: Integer); override;
  end;

  TmnIRCClient = class;

  TIRCProgress = (prgDisconnected, prgConnecting, prgConnected, prgReady);

  TIRCMsgType = (
    mtMessage,
    mtNotice,
    mtMOTD,
    mtSend,
    mtCTCPNotice,
    mtCTCPMessage,
    mtWelcome,
    mtTopic,
    mtChannelInfo,
    mtUserMode,
    mtJoin,
    mtLeft, //User Part or Quit, it send when both triggered to all channels
    mtPart, //User left the channel, also we will send mtLeft after it
    mtQuit  //User left the server, but sever not send Part for all channels, so we will send mtLeft after it
  );

  TIRCReceived = record
    Channel: string;
    Target: string;
    User: string;
    Msg: string;
  end;

  { TIRCCommand }

  TIRCCommand = class(TObject)
  private
    FName: string;

    FReceived: TIRCReceived;
    FText: String; //is a last param but a text started with :
    FParams: TStringList;

    FCTCP: Boolean;

    FTime: string;
    FSender: string;
    FAddress: string;

    FSource: string; //Full Sender address, split it to Sender and Address
    FRaw: String; //Full message received

  protected
    Queue: Boolean;
    property Raw: String read FRaw;
    property Source: string read FSource;
  public
    constructor Create;
    destructor Destroy; override;

    property User: string read FReceived.User write FReceived.User;
    property Target: string read FReceived.Target write FReceived.Target; //To Whome Channel or User sent
    property Channel: string read FReceived.Channel write FReceived.Channel; //If target started with #
    property Msg: string read FReceived.Msg write FReceived.Msg;

    property Text: string read FText; //Rest body of msg after : also added as last param

    property Sender: string read FSender;
    property Address: string read FAddress;
    property Name: string read FName;//Name or Code

    function PullParam(out Param: string): Boolean; overload;
    function PullParam: String; overload;
    property Params: TStringList read FParams;
    property Received: TIRCReceived read FReceived;
    property CTCP: Boolean read FCTCP;
  end;

  { TIRCReceiver }

  TIRCReceiver = class abstract(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    When: TIRCProgress;
    property Client: TmnIRCClient read FClient;
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); virtual;

    procedure DoParse(vCommand: TIRCCommand; vData: string); virtual;
    procedure Parse(vCommand: TIRCCommand; vData: string);
    procedure Receive(vCommand: TIRCCommand); virtual; //will be executed in main thread
    procedure Execute(vCommand: TIRCCommand); virtual;

    function Accept(S: string): Boolean; virtual;
  public
    Code: Integer;
    constructor Create(AClient: TmnIRCClient); virtual;
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

  end;

  { TIRCQueueCommand }

  TIRCQueueCommand = class(TIRCCommand)
  private
    Receiver: TIRCReceiver;
    procedure Receive; //out of thread
    procedure Execute; //in the thread
  public
  end;

  { TIRCUserCommand }

  TIRCUserCommand = class abstract(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure Send(vChannel: string; vMsg: string); virtual; abstract;
    procedure PrintHelp(vChannel: string = ''); //To target channel room
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
    procedure Add(Raw: string; When: TIRCProgress = prgConnected); overload; //if MsgType log it will be log, if not it will be parsed
  end;

  { TIRCUserName }

  TIRCUserMode = (umVoice, umHalfOp, umOp, umWallOp, umAdmin, umOwner, umInvisible, umAway, umFake, umBot, umServerNotices, umSSL);
  TIRCUserModes = set of TIRCUserMode;

  TIRCChannelMode = (cmKey, cmInviteOnly, cmModerated, cmNoOutside, cmRegisteredOnly, cmSecret, cmTopicProtection);
  TIRCChannelModes = set of TIRCChannelMode;

  TIRCUser = class(TmnNamedObject)
  protected
  public
    Address: string;
    //https://freenode.net/kb/answer/usermodes
    //https://gowalker.org/github.com/oragono/oragono/irc/modes
    Mode: TIRCUserModes; //mode in the server
  end;

  { TIRCChannel }

  TIRCChannel = class(TmnNamedObjectList<TIRCUser>)
  private
  protected
    Adding: Boolean;
  public
    Name: string; //Channel name
    UserModes: TIRCUserModes;
    function Add(vUserName: string): TIRCUser; overload;
    function UpdateUser(vUser: string; UpdateMode: Boolean = False): TIRCUser;
    procedure RemoveUser(vUser: string);
  end;

  { TIRCChannels }

  TIRCChannels = class(TmnObjectList<TIRCChannel>)
  public
    function Find(const Name: string): TIRCChannel;
    function Found(vChannel: string): TIRCChannel;
    function FindUser(const vChannel, vUser: string): TIRCUser;
    procedure UpdateUser(vChannel, vUser: string; UpdateMode: Boolean = False);
    procedure RemoveUser(vChannel, vUser: string);
  end;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnLogClientConnection)
  private
    Tries: Integer;
    FOnline: Boolean;

    FClient: TmnIRCClient;
    FStream: TIRCSocketStream;

    FHost: string;
    FPort: string;
    function InitStream: Boolean;
    function CreateStream: TIRCSocketStream;
  protected
    procedure DoLog(s: string); override;
    procedure Prepare; override;
    procedure Process; override;
    procedure Unprepare; override;
  protected
    function ParseRaw(vData: string): TIRCQueueCommand;

    procedure SendRaw(S: utf8string); //this called by client out of thread
    procedure SendRaws;

    property Client: TmnIRCClient read FClient;
    function GetConnected: Boolean; override;
    property Online: Boolean read FOnline;
  public
    constructor Create(vOwner: TmnConnections);
    destructor Destroy; override;
    procedure Connect;
    procedure Stop; override;
    property Host: string read FHost;
    property Port: string read FPort;
  end;

  { TIRCSession }

  TIRCSession = record
    Channels: TIRCChannels;
    Nick: string; //Current used nickname on the server
    UserModes: TIRCUserModes; //IDK what user mode is on server
    AllowedUserModes: TIRCUserModes;
    AllowedChannelModes: TIRCChannelModes;
    ServerVersion: string;
    Server: string;
    ServerChannel: string; //After Welcome we take the server as main channel
    procedure Clear;
    procedure SetNick(ANick: string);
  end;

  TIRCAuth = (authNone, authPASS, authIDENTIFY);

  { TmnIRCClient }

  TmnIRCClient = class(TObject)
  private
    FAuth: TIRCAuth;
    FMapChannels: TStringList;
    FPort: String;
    FHost: String;
    FPassword: String;
    FReconnectTime: Integer;
    FUsername: String;
    FRealName: String;
    FProgress: TIRCProgress;
    FSession: TIRCSession;
    FConnection: TmnIRCConnection;
    FReceivers: TIRCReceivers;
    FQueueSends: TIRCQueueRaws;
    FUserCommands: TIRCUserCommands;
    FLock: TCriticalSection;
    FUseUserCommands: Boolean;
    FNicks: TStringList;
    function GetActive: Boolean;
    procedure SetAuth(AValue: TIRCAuth);
    procedure SetMapChannels(AValue: TStringList);
    procedure SetNicks(AValue: TStringList);
    procedure SetPassword(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetRealName(const Value: String);
    procedure SetHost(const Value: String);
    procedure Connecting;
    procedure Connected;
    procedure Disconnected;
    procedure Rejoin;
    procedure SetReconnectTime(AValue: Integer);
    procedure SetUsername(const Value: String);
    //function BuildUserModeCommand(NewModes: TIRCUserModes): String;
  protected
    NickIndex: Integer;
    procedure SetProgress(const Value: TIRCProgress);

    procedure ProgresChanged;

    //Maybe Channel is same of Target, but we will,for NOTICE idk
    procedure Receive(vMsgType: TIRCMsgType; vReceived: TIRCReceived); virtual;

    procedure Log(S: string);

    procedure SendRaw(vData: String; vQueueAt: TIRCProgress = prgDisconnected);

    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String); virtual;

    procedure DoMsgReceived(vChannel, vUser, vTarget, vMessage: String); virtual;
    procedure DoMsgSent(vUser, vNotice: String); virtual;
    procedure DoNotice(vChannel, vUser, vTarget, vNotice: String); virtual;
    procedure DoUserJoined(vChannel, vUser: String); virtual;
    procedure DoUserLeft(vChannel, vUser: String); virtual;
    procedure DoUserParted(vChannel, vUser: String); virtual;
    procedure DoUserQuit(vUser: String); virtual;
    procedure DoTopic(vChannel, vTopic: String); virtual;
    procedure DoLog(vMsg: String); virtual;
    procedure DoConnected; virtual;
    procedure DoDisconnected; virtual;
    procedure DoProgressChanged; virtual;
    procedure DoUsersChanged(vChannelName: string; vChannel: TIRCChannel); virtual;
    procedure DoUserChanged(vChannel: string; vUser, vNewNick: string); virtual;
    procedure DoMyInfoChanged; virtual;

    procedure GetCurrentChannel(out vChannel: string); virtual;
    function MapChannel(vChannel: string): string;

    procedure Init; virtual;
    procedure CreateConnection;
    property Connection: TmnIRCConnection read FConnection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    procedure ChannelSend(vChannel, vMsg: String); deprecated;
    procedure SendMsg(vChannel, vMsg: String);

    procedure Identify;
    procedure SetMode(const Value: TIRCUserModes);
    procedure SetChannelMode(const Value: TIRCChannelMode);
    procedure SetNick(const ANick: String);
    procedure SetTopic(AChannel: String; const ATopic: String);
    procedure Join(Channel: String; Password: string = '');
    procedure Notice(vChannel, vMsg: String);
    procedure Quit(Reason: String);

    property Progress: TIRCProgress read FProgress;
    property Receivers: TIRCReceivers read FReceivers;
    property QueueSends: TIRCQueueRaws read FQueueSends;
    property UserCommands: TIRCUserCommands read FUserCommands;
    property Active: Boolean read GetActive;
  public
    property Host: String read FHost write SetHost;
    property Port: String read FPort write SetPort;
    property Nicks: TStringList read FNicks write SetNicks; //nick names to use, dd more for if already used nick
    property RealName: String read FRealName write SetRealName;
    property Password: String read FPassword write SetPassword;
    property Username: String read FUsername write SetUsername;
    property Session: TIRCSession read FSession;
    property MapChannels: TStringList read FMapChannels write SetMapChannels;
    property UseUserCommands: Boolean read FUseUserCommands write FUseUserCommands default True;
    property ReconnectTime: Integer read FReconnectTime write SetReconnectTime;
    property Auth: TIRCAuth read FAuth write SetAuth;
  end;

  { TIRCUserReceiver }

  TIRCUserReceiver= class abstract(TIRCReceiver) //like PrivMsg
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
  end;

  { TIRCMsgReceiver }

  TIRCMsgReceiver = class abstract(TIRCUserReceiver) //just for inheritance
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
  end;

  { TPRIVMSG_IRCReceiver }

  TPRIVMSG_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TNOTICE_IRCReceiver }

  TNOTICE_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TPing_IRCReceiver }

  TPing_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); override;
    procedure Execute(vCommand: TIRCCommand); override;
  end;

  { TJOIN_IRCReceiver }

  TJOIN_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
    procedure Execute(vCommand: TIRCCommand); override;
  end;

  { TPART_IRCReceiver }

  TPART_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TQUIT_IRCReceiver }

  TQUIT_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TTOPIC_IRCReceiver }

  TTOPIC_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TWELCOME_IRCReceiver }

  TWELCOME_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string); override;
    procedure Execute(vCommand: TIRCCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TMOTD_IRCReceiver }

  TMOTD_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TMYINFO_IRCReceiver }

  TMYINFO_IRCReceiver = class(TIRCReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TNICK_IRCReceiver }

  TNICK_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TMODE_IRCReceiver }

  TMODE_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoParse(vCommand: TIRCCommand; vData: string); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TNAMREPLY_IRCReceiver }

  TNAMREPLY_IRCReceiver = class(TIRCReceiver)
  protected
  public
    function Accept(S: string): Boolean; override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { THELP_IRCReceiver }

  THELP_IRCReceiver = class(TIRCReceiver)
  protected
  public
    function Accept(S: string): Boolean; override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TErrNicknameInUse_IRCReceiver }

  TErrNicknameInUse_IRCReceiver = class(TIRCReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TRAW_UserCommand }

  TRaw_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: string; vMsg: string); override;
  public
  end;

  { TJoin_UserCommand }

  { TID_UserCommand }

  TID_UserCommand = class(TIRCUserCommand)
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

  { TPart_UserCommand }

  TPart_UserCommand = class(TIRCUserCommand)
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

procedure ParseSender(Original: string; out Nick, Address: string);
var
  p, c: Integer;
begin
  p := 1;
  if ScanString(Original, p, c, '!', true) then
  begin
    Nick := MidStr(Original, 1, c);
    Address := MidStr(Original, c + 1, MaxInt);
  end;
end;

procedure ParseUserName(const AUser: string; out NickName: string; out Mode: TIRCUserModes);
var
  c: Char;
  i: Integer;
begin
  Mode := [];
  NickName := '';
  for i := Low(AUser) to High(AUser) do
  begin
    c := AUser[i];
    if CharInSet(c, ['&', '~', '%', '+', '@']) then
    begin
      if AUser[i] = '~' then
        Mode:= Mode + [umOwner]
      else if AUser[i] = '&' then
        Mode := Mode + [umAdmin]
      else if AUser[i] = '@' then
        Mode := Mode + [umOp]
      else if AUser[i] = '%' then
        Mode := Mode + [umHalfOp]
      else if AUser[i] = '+' then
        Mode := Mode + [umVoice];
    end
    else
    begin
      NickName := MidStr(AUser, i, MaxInt);
      break;
    end;
  end;
end;

procedure StringToUserStates(ModeStr: string; var Mode: TIRCUserModes; UpdateMode: Boolean = False);
var
  Index: Integer;
  Add: Boolean;
  procedure AddIt(AMode: TIRCUserMode);
  begin
    if Add then
      Mode := Mode + [AMode]
    else
      Mode := Mode - [AMode];
  end;
begin
  if not UpdateMode then
    Mode := [];
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

procedure StringToUserMode(ModeStr: string; var Mode: TIRCUserModes; UpdateMode: Boolean = False);
var
  Index: Integer;
  Add: Boolean;
  procedure AddIt(AMode: TIRCUserMode);
  begin
    if Add then
      Mode := Mode + [AMode]
    else
      Mode := Mode - [AMode];
  end;
begin
  if not UpdateMode then
    Mode := [];
  Add := True;
  for Index := 1 to Length(ModeStr) do
  begin
    case ModeStr[Index] of
      '+':
        Add := True;
      '-':
        Add := False;
      'v':
        AddIt(umVoice);
      'h':
        AddIt(umHalfOp);
      'o':
        AddIt(umOp);
      'a':
        AddIt(umAdmin);
      'q':
        AddIt(umOwner);
    end;
  end;
end;

//* https://wiki.zandronum.com/IRC:Channel_Modes

procedure StringToChannelMode(ModeStr: string; var Mode: TIRCChannelModes; UpdateMode: Boolean = False);
var
  Index: Integer;
  Add: Boolean;
  procedure AddIt(AMode: TIRCChannelMode);
  begin
    if Add then
      Mode := Mode + [AMode]
    else
      Mode := Mode - [AMode];
  end;
begin
  if not UpdateMode then
    Mode := [];
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

{ TIRCQueueCommand }

procedure TIRCQueueCommand.Execute;
begin
  if Receiver <> nil then
    Receiver.Execute(Self);
end;

procedure TIRCQueueCommand.Receive;
begin
  if Receiver <> nil then
    Receiver.Receive(Self);
  Free;//Kamikaze
end;

{ TID_UserCommand }

procedure TID_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('NICKSERV IDENTIFY ' + vMsg, prgReady);
end;

{ TIRCSocketStream }

procedure TIRCSocketStream.DoHandleError(var Handle: Boolean; AError: Integer);
begin
  Handle := True;
end;

{ TPart_UserCommand }

procedure TPart_UserCommand.Send(vChannel: string; vMsg: string);
var
  aChannel: string;
begin
  Client.GetCurrentChannel(aChannel);
  Client.SendRaw('PART ' + aChannel + ' :' + vMsg, prgReady);
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

procedure TIRCSession.SetNick(ANick: string);
begin
  Nick := ANick;
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

procedure THELP_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtWelcome, vCommand.Received);
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

procedure TIRCMsgReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  vCommand.Msg := vCommand.PullParam;
  if LeftStr(vCommand.Msg, 1) = #01 then
  begin
    vCommand.FCTCP := True;
    vCommand.Msg := DequoteStr(vCommand.Msg, #01);
  end;
end;

{ TNotice_UserCommand }

procedure TNotice_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('NOTICE ' + vChannel + ' :' + vMsg, prgReady);
end;

{ TSend_UserCommand }

procedure TSend_UserCommand.Send(vChannel: string; vMsg: string);
begin
  Client.SendRaw('PRIVMSG ' + vMsg, prgReady);
end;

{ TIRCUserReceiver }

procedure TIRCUserReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  vCommand.User := vCommand.Sender;
  vCommand.Target := vCommand.PullParam;
  if LeftStr(vCommand.Target, 1) = '#' then
    vCommand.Channel := vCommand.Target
  {else if vCommand.FAddress <> '' then //when private chat opened , sender is the channel, right?!!!
    vCommand.Channel := vCommand.User}
  else
    vCommand.Channel := vCommand.Sender;
end;

{ TMYINFO_IRCReceiver }

procedure TMYINFO_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  vCommand.PullParam(Client.FSession.Nick);
  vCommand.PullParam(Client.FSession.Server);
  vCommand.PullParam(Client.FSession.ServerVersion);
  StringToUserStates(vCommand.PullParam, Client.FSession.AllowedUserModes);
  StringToChannelMode(vCommand.PullParam, Client.FSession.AllowedChannelModes);
  Client.DoMyInfoChanged;
end;

{ TNOTICE_IRCReceiver }

procedure TNOTICE_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  //:cs.server NOTICE zaher :You're now logged in as zaher
  Client.Receive(mtNotice, vCommand.Received);
end;

{ TQUIT_IRCReceiver }

procedure TQUIT_IRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  //:Support_1!~Zaher@myip QUIT Quit
  vCommand.User := vCommand.Sender;
  vCommand.Msg := vCommand.PullParam;
end;

procedure TQUIT_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtQuit, vCommand.Received);
  Client.Session.Channels.RemoveUser(vCommand.Channel, vCommand.User);
end;

{ TPART_IRCReceiver }

procedure TPART_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtPart, vCommand.Received);
  Client.Session.Channels.RemoveUser(vCommand.Channel, vCommand.User);
end;

{ TJOIN_IRCReceiver }

procedure TJOIN_IRCReceiver.Execute(vCommand: TIRCCommand);
begin
  inherited;
  Client.Session.Channels.UpdateUser(vCommand.Channel, vCommand.User);
end;

procedure TJOIN_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtJoin, vCommand.Received);
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

function TIRCChannel.Add(vUserName: string): TIRCUser;
begin
  Result := TIRCUser.Create;
  Result.Name := vUserName;
  Add(Result);
end;

function TIRCChannel.UpdateUser(vUser: string; UpdateMode: Boolean): TIRCUser;
var
  aNickName: string;
  aMode: TIRCUserModes;
begin
  ParseUserName(vUser, aNickName, aMode);
  Result := Find(aNickName);
  if Result = nil then
    Result := Add(aNickName);
  if UpdateMode then
    Result.Mode := aMode;
end;

procedure TIRCChannel.RemoveUser(vUser: string);
var
  aNickName: string;
  aMode: TIRCUserModes;
  aUser: TIRCUser;
begin
  ParseUserName(vUser, aNickName, aMode);
  aUser := Find(aNickName);
  if aUser = nil then
    Remove(aUser);
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

function TIRCChannels.FindUser(const vChannel, vUser: string): TIRCUser;
var
  oChannel: TIRCChannel;
begin
  oChannel := Find(vChannel);
  if oChannel <> nil then
    Result := oChannel.Find(vUser)
  else
    Result := nil;
end;

procedure TIRCChannels.RemoveUser(vChannel, vUser: string);
var
  aChannel: TIRCChannel;
begin
  aChannel := Find(vChannel);
  if aChannel <> nil then
    aChannel.RemoveUser(vUser);
end;

procedure TIRCChannels.UpdateUser(vChannel, vUser: string; UpdateMode: Boolean);
var
  aChannel: TIRCChannel;
begin
  aChannel := Found(vChannel);
  aChannel.UpdateUser(vUser, UpdateMode);
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

procedure TNAMREPLY_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aChannelName: string;
  oChannel: TIRCChannel;
  aUsers: TStringList;
  i: Integer;
begin
  // triggered by raw /NAMES
  //:server 353 zaher = #support user1
  //:server 353 zaherdirkey = ##support :user1 user2 user3

  if vCommand.Name = IntToStr(IRC_RPL_ENDOFNAMES) then
  begin
    aChannelName := vCommand.Params[1];
    oChannel := Client.Session.Channels.Found(aChannelName);
    if oChannel <> nil then
    begin
      oChannel.Adding := False;
      Client.DoUsersChanged(aChannelName, oChannel);
    end;
  end
  else
  begin
    aChannelName := vCommand.Params[2];
    oChannel := Client.Session.Channels.Found(aChannelName);
    aUsers := TStringList.Create;
    try
      StrToStrings(vCommand.Params[vCommand.Params.Count - 1], aUsers, [' '], []);
      if not oChannel.Adding then
        oChannel.Clear;
      oChannel.Adding := True;
      for i := 0 to aUsers.Count -1 do
        oChannel.UpdateUser(aUsers[i], True);
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

procedure TIRCUserCommand.PrintHelp;
begin
  //TODO
end;

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

procedure TErrNicknameInUse_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  with Client do
    if Progress = prgConnected then
    begin
      if NickIndex >= Nicks.Count then
        Quit('Nick conflict')
      else
      begin
        SetNick(Nicks[NickIndex]);
        Inc(NickIndex);
        if NickIndex > Nicks.Count then
          NickIndex := 0;
      end;
    end;
end;

{ TMODE_IRCReceiver }

procedure TMODE_IRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  vCommand.Target := vCommand.PullParam;
  if LeftStr(vCommand.Target, 1) = '#' then
    vCommand.Channel := vCommand.Target
  else
    vCommand.User := vCommand.Sender;
end;

procedure TMODE_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  oChannel: TIRCChannel;
  oUser: TIRCUser;
  aUser, aMode: string;
begin
  inherited;
  //server MODE ##channel +v username
  //:zaher MODE zaher :+i
  aMode := vCommand.PullParam;
  aUser := vCommand.PullParam;
  if vCommand.Channel = '' then
  begin
    if vCommand.User = Client.FSession.Nick then
    begin
      StringToUserMode(aMode, Client.FSession.UserModes);
      Client.Receive(mtUserMode, vCommand.Received);
    end
    else
    begin
      //IDK
    end;
  end
  else
  begin
    oChannel := Client.FSession.Channels.Found(vCommand.Channel);
    if aUser = '' then
    begin
      StringToUserMode(aMode, oChannel.UserModes);
      Client.Receive(mtChannelInfo, vCommand.Received);
    end
    else
    begin
      vCommand.FReceived.User := aUser;
      oUser := oChannel.Find(aUser);
      if oUser <> nil then
      begin
        StringToUserMode(aMode, oUser.Mode);
        Client.Receive(mtUserMode, vCommand.Received);
      end;
    end;
  end;
end;

{ TNICK_IRCReceiver }

procedure TNICK_IRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  vCommand.User := vCommand.Sender;
end;

procedure TNICK_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aNewNick: string;
  oUser: TIRCUser;
  oChannel: TIRCChannel;
begin
  //:zaherdirkey_!~zaherdirkey@myip NICK zaherdirkey
  aNewNick := vCommand.PullParam;
  if SameText(vCommand.User, Client.Session.Nick) then
  begin
    Client.Session.SetNick(aNewNick);
    Client.DoMyInfoChanged;
  end
  else
  begin
    Client.DoUserChanged('', vCommand.User, aNewNick);
    for oChannel in Client.Session.Channels do
    begin
      oUser := oChannel.Find(vCommand.User);
      if oUser <> nil then
      begin
        oUser.Name := aNewNick;
      end;
    end;
  end;
end;

{ TWELCOME_IRCReceiver }

procedure TWELCOME_IRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  inherited;
  case AIndex of
    0: vCommand.User := AValue;
    1: vCommand.Msg := AValue;
  end;
end;

procedure TWELCOME_IRCReceiver.Execute(vCommand: TIRCCommand);
begin
  inherited;
  Client.SetProgress(prgReady);
  Client.FSession.ServerChannel := vCommand.Sender;
end;

procedure TWELCOME_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  with Client do
  begin
    Receive(mtWelcome, vCommand.Received);
    Log('Ready')
  end;
end;

{ TTOPIC_IRCReceiver }

procedure TTOPIC_IRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited DoParse(vCommand, vData);
  //:server 332 zaher #support :Creative Solutions Support Channel
  //:zaherdirkey!~zaherdirkey@myip TOPIC #support :Creative Solutions Support Channel2
  vCommand.User := vCommand.PullParam;
  vCommand.Target := vCommand.PullParam;
  vCommand.Channel := vCommand.Target;
  vCommand.Msg := vCommand.PullParam;
end;

procedure TTOPIC_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtTopic, vCommand.Received);
end;

{ TMOTD_IRCReceiver }

procedure TMOTD_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtMOTD, vCommand.Received);
end;

{ TIRCReceiver }

procedure TIRCReceiver.Receive(vCommand: TIRCCommand);
begin
end;

procedure TIRCReceiver.Execute(vCommand: TIRCCommand);
begin
end;

procedure TIRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  vCommand.Params.Add(AValue);
end;

procedure TIRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
var
  p: Integer;
  Start, Count: Integer;
  Index: Integer;
begin
  Index := 0;
  p := 1;
  while p < Length(vData) do
  begin
    if vData[p] = ':' then
    begin
      vCommand.FText := MidStr(vData, p + 1, MaxInt);
      AddParam(vCommand, Index, vCommand.FText);
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

procedure TIRCReceiver.Parse(vCommand: TIRCCommand; vData: string);
begin
  vCommand.Params.Clear;
  vCommand.Queue := True; //TODO False
  if vData <> '' then
  begin
    DoParse(vCommand, vData);
  end;
end;

function TIRCReceiver.Accept(S: string): Boolean;
begin
  Result := ((Name <> '') and SameText(S, Name)) or ((Code > 0) and (StrToIntDef(S, 0) = Code));
end;

constructor TIRCReceiver.Create(AClient: TmnIRCClient);
begin
  inherited Create;
  FClient := AClient;
end;

{ TPRIVMSG_IRCReceiver }

procedure TPRIVMSG_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtMessage, vCommand.Received);
end;

{ TPing_IRCReceiver }

procedure TPing_IRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
begin
  inherited;
  vCommand.Queue := False;
end;

procedure TPing_IRCReceiver.AddParam(vCommand: TIRCCommand; AIndex: Integer; AValue: string);
begin
  if AIndex = 0 then
    vCommand.Msg := AValue
  else
    inherited;
end;

procedure TPing_IRCReceiver.Execute(vCommand: TIRCCommand);
begin
  inherited Execute(vCommand);
  Client.Connection.SendRaw(Format('PONG %s', [vCommand.Msg]));
end;

{ TmnIRCConnection }

function TmnIRCConnection.InitStream: Boolean;
var
  ReconnectTime: Integer;
begin
  if FStream = nil then
    FStream := CreateStream;

  if not FStream.Connected and not Terminated and Online then
  begin
    try
      if Tries > 0 then
      begin
        Log('Reconnecting...');
        Lock.Enter;
        try
          ReconnectTime := Client.ReconnectTime;
        finally
          Lock.Leave;
        end;
        Sleep(ReconnectTime);
      end;
      Queue(Client.Connecting);
      FStream.Connect;
      if FStream.Connected then
      begin
        Log('Connected successed');
        Queue(Client.Connected);
      end;
      Inc(Tries);
    except
      on E: Exception do
      begin
        FreeAndNil(FStream);
        Log('Connected failed:' + E.Message);
      end;
    end;
  end;

  Result := (FStream <> nil) and FStream.Connected;
end;

function TmnIRCConnection.CreateStream: TIRCSocketStream;
begin
  Result := TIRCSocketStream.Create(Host, Port, [soNoDelay, soSSL]);
  Result.ConnectTimeout := -1;
  Result.ReadTimeout := -1;//1000;
  Result.EndOfLine := #10;
end;

procedure TmnIRCConnection.DoLog(s: string);
begin
  inherited;
  Client.Log(S);
end;

procedure TmnIRCConnection.Prepare;
begin
  inherited;
end;

procedure TmnIRCConnection.SendRaws;
var
  i: Integer;
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    Lock.Enter;
    try
      i := 0;
      while i < Client.QueueSends.Count do
      begin
        if Client.QueueSends[i].When <= Client.Progress then
        begin
          if LeftStr(Client.QueueSends[i].Raw, 4) = 'JOIN' then
            nothing;
          temp.Add(Client.QueueSends[i].Raw);
          Client.QueueSends.Delete(i);
        end
        else
          inc(i);
      end;
    finally
      Lock.Leave;
    end;

    for i := 0 to temp.Count -1 do
      SendRaw(temp[i]);
  finally
    FreeAndNil(temp);
  end;
end;

procedure TmnIRCConnection.Process;
var
  aLine: utf8string;
  aCommand: TIRCQueueCommand;
begin
  inherited;
  if InitStream then
  begin
    try
      SendRaws;

      FStream.ReadLineUTF8(aLine, True);
      aLine := Trim(aLine);

      while (aLine <> '') and not Terminated do
      begin
        {$ifdef FPC}
          {.$IF FPC_FULLVERSION>=30101}
            //For future when Anonymous enabled in FPC
          {.$endif}
        {$endif}

        Log('<' + aLine);
        aCommand := ParseRaw(aLine);
        if aCommand <> nil then
        begin
          aCommand.Execute;
          if (aCommand.Receiver <> nil) and aCommand.Queue then
            Queue(aCommand.Receive)
          else
            aCommand.Receive;
        end;

        if (FStream <> nil) and (FStream.Connected) then
          SendRaws;

        if not Terminated then
        begin
          FStream.ReadLineUTF8(aLine, True);
          aLine := Trim(aLine);
        end;
      end;
    except
      on E: Exception do
      begin
        FreeAndNil(FStream);
        Log(E.Message);
      end;
    end;
  end
end;

procedure TmnIRCConnection.Unprepare;
begin
  inherited;
  if Connected then
    Stop;
  Synchronize(Client.Disconnected);
  FreeAndNil(FStream);
end;

function TmnIRCConnection.ParseRaw(vData: string): TIRCQueueCommand;
type
  TParseState = (prefix, command, message);
var
  p: Integer;
  aState: TParseState;
  Start, Count: Integer;
  aReceiver: TIRCReceiver;
  ABody: string;
begin
  //:zaherdirkey!~zaherdirkey@myip PRIVMSG channel :hi
  if vData <> '' then
  begin
    Result := TIRCQueueCommand.Create;
    Result.FRaw := vData;
    aState := prefix;
    p := 1;
    while p < Length(vData) do
    begin
      if vData[p] = '@' then
      begin
        Inc(p);
        Start := p;
        if ScanString(vData, p, Count, cTokenSeparator, True) then
          Result.FTime := MidStr(vData, Start, Count);
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
          begin
            Result.FSource := MidStr(vData, Start, Count);
            ParseSender(Result.Source, Result.FSender, Result.FAddress);
          end;
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
          Result.FName := MidStr(vData, Start, Count);
          ABody := MidStr(vData, Start + Count + 1, MaxInt);
          aState := command;
          Inc(aState);
        end;
        break;
      end;
    end;

    for aReceiver in Client.Receivers do
    begin
      if aReceiver.Accept(Result.Name) then
      begin
        Result.Receiver := aReceiver;
        aReceiver.Parse(Result, ABody);
        break;
      end;
    end;
  end
  else
    Result := nil;
end;

procedure TmnIRCConnection.SendRaw(S: utf8string);
begin
  if FStream.Connected then
  begin
    FStream.WriteLineUTF8(S);
    Log('>'+S); //it is in main thread
  end;
end;

function TmnIRCConnection.GetConnected: Boolean;
begin
  Result := not Terminated;
end;

constructor TmnIRCConnection.Create(vOwner: TmnConnections);
begin
  inherited;
end;

destructor TmnIRCConnection.Destroy;
begin
  inherited;
end;

procedure TmnIRCConnection.Connect;
begin
  inherited;
end;

procedure TmnIRCConnection.Stop;
begin
  inherited;
  if FStream <> nil then
    FStream.Disconnect;
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

function TIRCCommand.PullParam: String;
begin
  PullParam(Result);
end;

function TIRCCommand.PullParam(out Param: string): Boolean;
begin
  Result := Params.Count > 0;
  if Result then
  begin
    Param := Params[0];
    Params.Delete(0);
  end
  else
    Param := '';
end;

{ TCustomIRCReceivers }

function TCustomIRCReceivers.Add(vName: String; vCode: Integer; AClass: TIRCReceiverClass): Integer;
var
  AReceiver: TIRCReceiver;
begin
  {Result := IndexOfName(vName);
  if Result >= 0 then
  begin
    //not sure if we want more than one Receiver
  end;}
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
  if Assigned(FConnection) then
  begin
    Connection.FOnline := False;
    if (FConnection.Connected) then
      Quit('Bye');
    FConnection.Terminate;//no stop
    FConnection.WaitFor;
  end;
  NickIndex := 0;
end;

procedure TmnIRCClient.Connect;
begin
  NickIndex := 0;
  if (Progress < prgConnecting) then
  begin
    FreeAndNil(FConnection);
    CreateConnection;
    Connection.FOnline := True;
    Connection.Start;
  end;
end;

constructor TmnIRCClient.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FNicks := TStringList.Create;
  FMapChannels := TStringList.Create;
  FUseUserCommands := True;

  FSession.Channels := TIRCChannels.Create(True);

  FReconnectTime := 3000;
  FHost := 'localhost';
  FPort := sDefaultPort;

  FRealName := 'username';
  FUsername := 'username';
  FPassword := '';

  FProgress := prgDisconnected;
  FReceivers := TIRCReceivers.Create(Self);
  FUserCommands :=TIRCUserCommands.Create(Self);
  FQueueSends := TIRCQueueRaws.Create(True);
  Init;
end;

destructor TmnIRCClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FConnection);
  FreeAndNil(FUserCommands);
  FreeAndNil(FQueueSends);
  FreeAndNil(FReceivers);
  FreeAndNil(FLock);
  FreeAndNil(FMapChannels);
  FreeAndNil(FNicks);
  FreeAndNil(FSession.Channels);
  inherited;
end;

procedure TmnIRCClient.Rejoin;
var
  i: Integer;
  RejoinChannles: TStringList;
begin
  RejoinChannles := TStringList.Create;
  try
    for i := 0 to Session.Channels.Count - 1 do
      RejoinChannles.Add(Session.Channels[i].Name);

    for i := 0 to RejoinChannles.Count - 1 do
      Join(RejoinChannles[i]);
  finally
    RejoinChannles.Free;
  end;
end;

procedure TmnIRCClient.SetReconnectTime(AValue: Integer);
begin
  if FReconnectTime =AValue then
    Exit;
  if Connection <> nil then
    Connection.lock.Enter;
  try
    FReconnectTime := AValue;
  finally
    if Connection <> nil then
      Connection.lock.Leave;
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
      Connection.Lock.Enter;
      try
        QueueSends.Add(vData, vQueueAt);
      finally
        Connection.Lock.Leave;
      end;
    end;
  end;
end;

procedure TmnIRCClient.DoMsgReceived(vChannel, vUser, vTarget, vMessage: String);
begin
end;

procedure TmnIRCClient.DoMsgSent(vUser, vNotice: String);
begin
end;

procedure TmnIRCClient.DoNotice(vChannel, vUser, vTarget, vNotice: String);
begin
end;

procedure TmnIRCClient.DoUserJoined(vChannel, vUser: String);
begin
end;

procedure TmnIRCClient.DoUserLeft(vChannel, vUser: String);
begin

end;

procedure TmnIRCClient.DoUserParted(vChannel, vUser: String);
begin
end;

procedure TmnIRCClient.DoUserQuit(vUser: String);
begin
end;

procedure TmnIRCClient.DoTopic(vChannel, vTopic: String);
begin
end;

procedure TmnIRCClient.DoConnected;
begin
end;

procedure TmnIRCClient.DoDisconnected;
begin
end;

procedure TmnIRCClient.DoLog(vMsg: String);
begin
end;

procedure TmnIRCClient.ChannelSend(vChannel, vMsg: String);
begin
  SendMsg(vChannel, vMsg);
end;

procedure TmnIRCClient.SendMsg(vChannel, vMsg: String);
var
  aCMD: TIRCUserCommand;
  aCMDProcessed: Boolean;
  s, m: string;
  p, c: Integer;
  aReceived: TIRCReceived;
begin
  Initialize(aReceived);
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
    aReceived.Channel := vChannel;
    aReceived.Target := vChannel;
    aReceived.User := Session.Nick;
    aReceived.Msg := vMsg;
    Receive(mtSend, aReceived);
  end;
end;

procedure TmnIRCClient.Identify;
begin
  SendRaw(Format('NICKSERV IDENTIFY %s %s', [Username, Password]), prgConnected);
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
  Connection.FOnline := False;
  SetProgress(prgDisconnected);
  FSession.Nick := '';
  FSession.AllowedUserModes := [];
  FSession.AllowedChannelModes := [];
  FSession.Channels.Clear;
  DoDisconnected;
end;

procedure TmnIRCClient.Connected;
var
  aNick: string;
begin
  SetProgress(prgConnected);
  if Nicks.Count = 0 then
    raise Exception.Create('You should define nicknames list');
  aNick := Nicks[0];
  Inc(NickIndex);

  SendRaw(Format('USER %s 0 * :%s', [Username, RealName]));
  if FPassword <> '' then
  begin
    if Auth = authPass then
    begin
      SendRaw(Format('PASS %s:%s', [Username, FPassword]));
      SetNick(aNick);
    end
    else if Auth = authIDENTIFY then
    begin
      SetNick(aNick);
      SendRaw(Format('NICKSERV IDENTIFY %s %s', [Username, Password]));
    end
  end
  else
    SetNick(aNick);
  Rejoin;
  DoConnected;
end;

procedure TmnIRCClient.SetNick(const ANick: String);
begin
  if ANick <> '' then
    SendRaw(Format('NICK %s', [ANick]), prgConnected);
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
  Result := (FConnection <> nil) and FConnection.Connected and FConnection.Online;
end;

procedure TmnIRCClient.SetAuth(AValue: TIRCAuth);
begin
  if FAuth =AValue then Exit;
  FAuth :=AValue;
end;

procedure TmnIRCClient.SetMapChannels(AValue: TStringList);
begin
  if FMapChannels = AValue then
    Exit;
  FMapChannels.Assign(AValue);
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

procedure TmnIRCClient.Connecting;
begin
  SetProgress(prgConnecting);
end;

procedure TmnIRCClient.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

procedure TmnIRCClient.SetProgress(const Value: TIRCProgress);
begin
  if Value <> FProgress then
  begin
    try
      if Connection <> nil then
        Connection.Lock.Enter;
      FProgress := Value;
    finally
      if Connection <> nil then
        Connection.Lock.Leave;
    end;
    ProgresChanged;
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

procedure TmnIRCClient.SetChannelMode(const Value: TIRCChannelMode);
begin

end;

procedure TmnIRCClient.DoProgressChanged;
begin

end;

procedure TmnIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
begin
end;

procedure TmnIRCClient.DoUsersChanged(vChannelName: string; vChannel: TIRCChannel);
begin

end;

procedure TmnIRCClient.DoUserChanged(vChannel: string; vUser, vNewNick: string);begin

end;

procedure TmnIRCClient.DoMyInfoChanged;
begin

end;

procedure TmnIRCClient.GetCurrentChannel(out vChannel: string);
begin
  vChannel := '';
end;

function TmnIRCClient.MapChannel(vChannel: string): string;
begin
  if MapChannels.IndexOfName(vChannel) >=0 then
  begin
    Result := MapChannels.Values[vChannel];
    if Result = '' then //it is main server channel
      Result := Session.ServerChannel;
  end
  else
    Result := vChannel
end;

procedure TmnIRCClient.Quit(Reason: String);
begin
  SendRaw(Format('QUIT :%s', [Reason]), prgReady);
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

procedure TmnIRCClient.ProgresChanged;
begin
  DoProgressChanged;
end;

procedure TmnIRCClient.Receive(vMsgType: TIRCMsgType; vReceived: TIRCReceived);
var
  oChannel: TIRCChannel;
  aReceived: TIRCReceived;
begin
  vReceived.Channel := MapChannel(vReceived.Channel);
  DoReceive(vMsgType, vReceived.Channel, vReceived.User, vReceived.Msg);
  case vMsgType of
    mtJoin:
      DoUserJoined(vReceived.Channel, vReceived.User);
    mtLeft:
      DoUserLeft(vReceived.Channel, vReceived.User);
    mtPart:
    begin
      DoUserParted(vReceived.Channel, vReceived.User);
      Receive(mtLeft, vReceived);
    end;
    mtQuit:
    begin
      DoUserQuit(vReceived.User);
      if not SameText(Session.Nick, vReceived.User) then //not me
      begin
        aReceived := vReceived;
        for oChannel in Session.Channels do
        begin
          aReceived.Channel := oChannel.Name;
          Receive(mtLeft, aReceived);
          oChannel.RemoveUser(vReceived.User);
        end;
      end;
    end;
    mtNotice:
      DoNotice(vReceived.Channel, vReceived.User, vReceived.Target, vReceived.MSG);
    mtMessage:
      DoMsgReceived(vReceived.Channel, vReceived.User, vReceived.Target, vReceived.MSG);
    mtSend:
      DoMsgSent(vReceived.User, vReceived.MSG);
    mtTopic:
      DoTopic(vReceived.Channel, vReceived.MSG);
    mtUserMode:
      DoUserChanged(vReceived.Channel, vReceived.User, '');
    else;
  end;
end;

procedure TmnIRCClient.Log(S: string);
begin
  DoLog(S);
end;

procedure TmnIRCClient.Init;
begin
  Receivers.Add('PRIVMSG', 0, TPRIVMSG_IRCReceiver);
  Receivers.Add('NOTICE', 0, TNOTICE_IRCReceiver);

  Receivers.Add('MODE', 0, TMODE_IRCReceiver);
  Receivers.Add('PING', 0, TPING_IRCReceiver);
  Receivers.Add('MOTD', IRC_RPL_MOTD, TMOTD_IRCReceiver);
  Receivers.Add('', IRC_RPL_TOPIC, TTOPIC_IRCReceiver);
  Receivers.Add('TOPIC', 0, TTOPIC_IRCReceiver);//not same with IRC_RPL_TOPIC
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
  UserCommands.Add(TPart_UserCommand.Create('Part', Self));
  UserCommands.Add(TPart_UserCommand.Create('Leave', Self));
  UserCommands.Add(TMode_UserCommand.Create('Mode', Self));
  UserCommands.Add(TSend_UserCommand.Create('Send', Self));
  UserCommands.Add(TMe_UserCommand.Create('Me', Self));
  UserCommands.Add(TNotice_UserCommand.Create('Notice', Self));
  UserCommands.Add(TCNotice_UserCommand.Create('CNotice', Self));
  UserCommands.Add(TID_UserCommand.Create('ID', Self));
  UserCommands.Add(THELP_UserCommand.Create('HELP', Self));

  MapChannels.Values['ChanServ'] := ''; //To server channel
  MapChannels.Values['NickServ'] := '';
  MapChannels.Values['SaslServ'] := '';
end;

procedure TmnIRCClient.CreateConnection;
begin
  FConnection := TmnIRCConnection.Create(nil);
  FConnection.FClient := Self;
  FConnection.FreeOnTerminate := false;
  FConnection.FHost := Host;
  FConnection.FPort := Port;
end;

end.
