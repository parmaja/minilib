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
 *  @author by Zaher Dirkey <zaher, zaherdirkey>


    http://chi.cs.uchicago.edu/chirc/irc_examples.html

    https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands
    https://stackoverflow.com/questions/12747781/irc-message-format-clarification

    //Yes we can read in a thread and write in main thread
    https://stackoverflow.com/questions/7418093/use-one-socket-in-two-threads

 *}

{TODO
  remove using Params[0] and use AddParam instead

https://modern.ircdocs.horse/dcc.html
:zaherdirkey_!~zaherdirkey@4m8ei7xicz3j2.irc PRIVMSG zezo :DCC SEND 1.sql 0 4814 62894
:zaherdirkey_!~zaherdirkey@4m8ei7xicz3j2.irc PRIVMSG zezo :SHA-256 checksum for 1.sql (remote): a1a0e7c6223479a8329d73942d4c4bb35532d0932ee46c82216f50e252ee4e1d

<:freenode-connect!frigg@freenode/utility-bot/frigg NOTICE zaher :Welcome to freenode. To protect the network all new connections will be scanned for vulnerabilities. This will not harm your computer, and vulnerable hosts will be notified.
<:freenode-connect!frigg@freenode/utility-bot/frigg PRIVMSG zaher\0:VERSION\0

PREFIX=(ov)@+

}

interface

uses
  Classes, StrUtils, syncobjs,
  mnClasses, mnSockets, mnClients, mnStreams, mnConnections, mnUtils;

const
  cCTCPChar: UTF8Char = #1;
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }

  //* https://www.alien.net.au/irc/irc2numerics.html

  IRC_RPL_WELCOME = 001;
  IRC_RPL_YOURHOST = 002; //TODO
  IRC_RPL_MYINFO = 004;
  IRC_RPL_WHOISUSER = 311;
  IRC_RPL_WHOISSERVER = 312;
  IRC_RPL_WHOISOPERATOR = 313;
  IRC_RPL_WHOWASUSER = 314;
  IRC_RPL_ENDOFWHO = 315;
  IRC_RPL_WHOISIDLE = 317;
  IRC_RPL_ENDOFWHOIS = 318;
  IRC_RPL_WHOISCHANNELS = 319;
  IRC_RPL_CHANNEL_URL = 328;
  IRC_RPL_WHOISACCOUNT = 330;
  IRC_RPL_TOPIC = 332;
  IRC_RPL_TOPICWHOTIME = 333;
  IRC_RPL_NAMREPLY = 353;
  IRC_RPL_WHOREPLY = 352;
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
    mtAction, //TODO
    mtMOTD,
    mtSend,
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
    Time: TDateTime;
    Channel: utf8string;
    Target: utf8string;
    User: utf8string;
    Msg: utf8string;
  end;

  //CTCP submessage
  { TIRC_CTCP }

  TIRC_CTCP = record
    IsCTCP: Boolean;
    Command: utf8string;
    Message: utf8string; //full text after command name
    Text: utf8string; //that come after :
    Params: TArray<utf8string>;
    procedure AddParam(Value: utf8string);
    function PullParam(out Param: utf8string): Boolean; overload;
    function PullParam: utf8string; overload;
  end;

  { TIRCCommand }

  TIRCCommand = class(TObject)
  private
    FName: utf8string;

    FReceived: TIRCReceived;
    FText: utf8string; //is a last param but a text started with :
    FParams: TStringList;


    FTime: utf8string;
    FSender: utf8string;
    FAddress: utf8string;

    FSource: utf8string; //Full Sender address, split it to Sender and Address
    FRaw: utf8string;
    FCode: Integer; //Full message received

  protected
    Queue: Boolean; //Run it in the main thread, it is gui command
    CTCP: TIRC_CTCP;
    property Raw: utf8string read FRaw;
    property Source: utf8string read FSource;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParam(AValue: utf8string);

    property User: utf8string read FReceived.User write FReceived.User;
    property Target: utf8string read FReceived.Target write FReceived.Target; //To Whome Channel or User sent
    property Channel: utf8string read FReceived.Channel write FReceived.Channel; //If target started with #
    property Msg: utf8string read FReceived.Msg write FReceived.Msg;

    property Text: utf8string read FText; //Rest body of msg after : also added as last param

    property Sender: utf8string read FSender;
    property Address: utf8string read FAddress;
    property Name: utf8string read FName;//Name or Code
    property Code: Integer read FCode;// Code or 0 if it just name

    function PullParam(out Param: utf8string): Boolean; overload;
    function PullParam: utf8string; overload;
    property Params: TStringList read FParams;
    property Received: TIRCReceived read FReceived;
  end;

  TIRCQueueCommand = class;

  { TIRCReceiver }

  TIRCReceiver = class abstract(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    When: TIRCProgress;
    Echo: Boolean; //This will send internally too, not yet implemented //TODO
    CTCP: Boolean; //Only CTCP receiver
    property Client: TmnIRCClient read FClient;

    //procedure Prepare; virtual;
    procedure Receive(vCommand: TIRCCommand); virtual; //will be executed in main thread
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); virtual;
    function Execute(vCommand: TIRCCommand): TIRCQueueCommand;

    function DoAccept(aName: utf8string): Boolean; virtual; //Extending accept, useing OR
    function Accept(aName: utf8string; aSubName: utf8string = ''; aCTCP: Boolean = False): Boolean;
  public
    Codes: TArray<Integer>;
    SubName: utf8string;
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
    function Add(vName: utf8string; vCodes: TArray<Integer>; AClass: TIRCReceiverClass): Integer; overload;
    function Add(vName: utf8string; vSubName: utf8string; vCTCP: Boolean; vCodes: TArray<Integer>; AClass: TIRCReceiverClass): Integer; overload;
    function Find(aName, aSubName: utf8string; aCTCP: Boolean): TIRCReceiver;
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
    function Execute: TIRCQueueCommand; //run it in the current thread, and return the next command
    procedure Receive; //out of thread, in main thread of current thread, depend on Queue option
  public
  end;

  { TIRCUserCommand }

  TIRCUserCommand = class abstract(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); virtual; abstract;
    procedure PrintHelp(vChannel: utf8string = ''); //To target channel room
  public
    constructor Create(AName: utf8string; AClient: TmnIRCClient);
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
    Raw: utf8string;
  end;

  { TIRCQueueRaws }

  TIRCQueueRaws = class(TmnObjectList<TIRCRaw>)
  public
    procedure Add(Raw: utf8string; When: TIRCProgress = prgConnected); overload; //if MsgType log it will be log, if not it will be parsed
  end;

  { TIRCUserName }

  TIRCUserMode = (umVoice, umHalfOp, umOp, umWallOp, umAdmin, umOwner, umInvisible, umAway, umFake, umBot, umServerNotices, umSSL);
  TIRCUserModes = set of TIRCUserMode;

  TIRCChannelMode = (cmKey, cmInviteOnly, cmModerated, cmNoOutside, cmRegisteredOnly, cmSecret, cmTopicProtection);
  TIRCChannelModes = set of TIRCChannelMode;

  TIRCWhoIsInfo = record
    RealName: utf8string;
    UserName: utf8string;
    Host: utf8string;
    Account: utf8string;
    Server: utf8string;
    LastTime: utf8string;
    Channels: utf8string;
    Country: utf8string;
  end;

  TIRCUser = class(TmnNamedObject)
  protected
  public
    Address: utf8string;
    WhoIs: TIRCWhoisInfo;
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
    Name: utf8string; //Channel name
    UserModes: TIRCUserModes;
    function Add(vUserName: utf8string): TIRCUser; overload;
    function UpdateUser(vUser: utf8string; UpdateMode: Boolean = False): TIRCUser;
    procedure RemoveUser(vUser: utf8string);
  end;

  { TIRCChannels }

  TIRCChannels = class(TmnObjectList<TIRCChannel>)
  public
    function Find(const Name: utf8string): TIRCChannel;
    function Found(vChannel: utf8string): TIRCChannel;
    function FindUser(const vChannel, vUser: utf8string): TIRCUser;
    function UpdateUser(vChannel, vUser: utf8string; UpdateMode: Boolean = False): TIRCUser;
    procedure RemoveUser(vChannel, vUser: utf8string);
  end;

  TmnOnData = procedure(const Data: utf8string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnLogClientConnection)
  private
    FHost: utf8string;
    FPort: utf8string;
    FClient: TmnIRCClient;
    FStream: TIRCSocketStream;

    FDelayEvent: TEvent;
    FActive: Boolean; //End user started or stopped it
    FInternalConnected: Boolean; //True if connected, used to detected disconnected after connect

    Tries: Integer;
    function InitStream: Boolean;
    function CreateStream: TIRCSocketStream;
  protected
    procedure DoLog(s: string); override;
    procedure Prepare; override;
    procedure Process; override;
    procedure Unprepare; override;
  protected
    function ParseRaw(vData: utf8string): TIRCQueueCommand;

    procedure SendRaw(S: utf8string); //this called by client out of thread
    procedure SendRaws;

    property Client: TmnIRCClient read FClient;
    function GetConnected: Boolean; override;
    property Active: Boolean read FActive;
  public
    constructor Create(vOwner: TmnConnections);
    destructor Destroy; override;
    procedure Connect;
    procedure Stop; override;
    property Host: utf8string read FHost;
    property Port: utf8string read FPort;
  end;

  { TIRCSession }

  TIRCSession = record
    Channels: TIRCChannels;
    Nick: utf8string; //Current used nickname on the server
    UserModes: TIRCUserModes; //IDK what user mode is on server
    AllowedUserModes: TIRCUserModes;
    AllowedChannelModes: TIRCChannelModes;
    ServerVersion: utf8string;
    Server: utf8string;
    ServerChannel: utf8string; //After Welcome we take the server as main channel
    procedure Clear;
    procedure SetNick(ANick: utf8string);
  end;

  TIRCAuth = (authNone, authPASS, authIDENTIFY);

  { TmnIRCClient }

  TmnIRCClient = class(TObject)
  private
    FAuth: TIRCAuth;
    FMapChannels: TStringList;
    FPort: utf8string;
    FHost: utf8string;
    FPassword: utf8string;
    FReconnectTime: Integer;
    FUsername: utf8string;
    FRealName: utf8string;
    FProgress: TIRCProgress;
    FSession: TIRCSession;
    FConnection: TmnIRCConnection;
    FReceivers: TIRCReceivers;
    FQueueSends: TIRCQueueRaws;
    FUserCommands: TIRCUserCommands;
    FLock: TCriticalSection;
    FUseSSL: Boolean;
    FUseUserCommands: Boolean;
    FNicks: TStringList;

    function GetActive: Boolean;
    function GetOnline: Boolean;

    procedure SetAuth(AValue: TIRCAuth);
    procedure SetMapChannels(AValue: TStringList);
    procedure SetNicks(AValue: TStringList);
    procedure SetPassword(const Value: utf8string);
    procedure SetPort(const Value: utf8string);
    procedure SetRealName(const Value: utf8string);
    procedure SetHost(const Value: utf8string);

    procedure Opened; //opened by user, not really connected to the server
    procedure Connected; //connected to the server, we must check if it initial or not
    procedure Connecting; //just for notification
    procedure Disconnected;
    procedure Closed; //to clean up every thing, closed by user

    procedure SetReconnectTime(AValue: Integer);
    procedure SetUsername(const Value: utf8string);
    procedure SetUseSSL(AValue: Boolean);
    function UserModeToString(NewModes: TIRCUserModes): utf8string;
    procedure JoinChannels; //Join rooms or rejoin
  protected
    NickIndex: Integer;
    procedure SetProgress(const Value: TIRCProgress);

    procedure ProgresChanged;

    //Maybe Channel is same of Target, but we will,for NOTICE idk
    procedure Receive(vMsgType: TIRCMsgType; vReceived: TIRCReceived); virtual;

    procedure Log(S: utf8string);

    procedure SendRaw(vData: utf8string; vQueueAt: TIRCProgress = prgDisconnected);

    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: utf8string); virtual;

    procedure DoMsgReceived(vChannel, vUser, vTarget, vMessage: utf8string); virtual;
    procedure DoMsgSent(vUser, vNotice: utf8string); virtual;
    procedure DoNotice(vChannel, vUser, vTarget, vNotice: utf8string); virtual;
    procedure DoUserJoined(vChannel, vUser: utf8string); virtual;
    procedure DoUserLeft(vChannel, vUser: utf8string); virtual;
    procedure DoUserParted(vChannel, vUser: utf8string); virtual;
    procedure DoUserQuit(vUser: utf8string); virtual;
    procedure DoTopic(vChannel, vTopic: utf8string); virtual;
    procedure DoLog(vMsg: utf8string); virtual;
    procedure DoConnected; virtual;
    procedure DoDisconnected; virtual;
    procedure DoClosed; virtual;
    procedure DoOpened; virtual;
    procedure DoProgressChanged; virtual;
    procedure DoUsersChanged(vChannelName: utf8string; vChannel: TIRCChannel); virtual;
    procedure DoUserChanged(vChannel: utf8string; vUser, vNewNick: utf8string); virtual;
    procedure DoWhoIs(vUser: utf8string); virtual;
    procedure DoWho(vChannel: utf8string); virtual;
    procedure DoMyInfoChanged; virtual;

    procedure GetCurrentChannel(out vChannel: utf8string); virtual;
    function MapChannel(vChannel: utf8string): utf8string;

    procedure Init; virtual;
    procedure CreateConnection;
    property Connection: TmnIRCConnection read FConnection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure ChannelSend(vChannel, vMsg: utf8string); deprecated;
    procedure SendMsg(vChannel, vMsg: utf8string);

    procedure Identify;
    procedure SetMode(const Value: TIRCUserModes);
    procedure SetChannelMode(const Value: TIRCChannelMode); //TODO
    procedure SetNick(const ANick: utf8string);
    procedure SetTopic(AChannel: utf8string; const ATopic: utf8string);
    procedure Join(Channel: utf8string; Password: utf8string = '');
    procedure Notice(vChannel, vMsg: utf8string);
    procedure Quit(Reason: utf8string);
    procedure Kick(vChannel, vNickName: utf8string);
    procedure OpUser(vChannel, vNickName: utf8string);
    procedure DeopUser(vChannel, vNickName: utf8string);
    procedure WhoIs(vNickName: utf8string);
    procedure Who(vChannel: utf8string);

    property Progress: TIRCProgress read FProgress;
    property Receivers: TIRCReceivers read FReceivers;
    property QueueSends: TIRCQueueRaws read FQueueSends;
    property UserCommands: TIRCUserCommands read FUserCommands;
    property Active: Boolean read GetActive;
    property Online: Boolean read GetOnline;
  public
    property Host: utf8string read FHost write SetHost;
    property Port: utf8string read FPort write SetPort;
    property UseSSL: Boolean read FUseSSL write SetUseSSL;
    property Nicks: TStringList read FNicks write SetNicks; //nick names to use, dd more for if already used nick
    property RealName: utf8string read FRealName write SetRealName;
    property Password: utf8string read FPassword write SetPassword;
    property Username: utf8string read FUsername write SetUsername;
    property Session: TIRCSession read FSession;
    property MapChannels: TStringList read FMapChannels write SetMapChannels;
    property UseUserCommands: Boolean read FUseUserCommands write FUseUserCommands default True;
    property ReconnectTime: Integer read FReconnectTime write SetReconnectTime;
    property Auth: TIRCAuth read FAuth write SetAuth;
  end;

  { TIRCUserReceiver }

  TIRCUserReceiver= class abstract(TIRCReceiver) //like PrivMsg
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
  end;

  { TIRCMsgReceiver }

  TIRCMsgReceiver = class abstract(TIRCUserReceiver) //just for inheritance
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
  end;

  { TPRIVMSG_IRCReceiver }

  TPRIVMSG_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TVersion_IRCReceiver }

  TVersion_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
  end;

  { TAction_IRCReceiver }

  TAction_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TDCC_IRCReceiver }

  TDCC_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
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
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
  end;

  { TJOIN_IRCReceiver }

  TJOIN_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TPART_IRCReceiver }

  TPART_IRCReceiver = class(TIRCUserReceiver)
  protected
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TQUIT_IRCReceiver }

  TQUIT_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TTOPIC_IRCReceiver }

  TTOPIC_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TWELCOME_IRCReceiver }

  TWELCOME_IRCReceiver = class(TIRCMsgReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
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
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TMODE_IRCReceiver }

  TMODE_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand); override;
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TNAMREPLY_IRCReceiver }

  TNAMREPLY_IRCReceiver = class(TIRCReceiver)
  protected
  public
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TWHOISREPLY_IRCReceiver }

  TWHOISREPLY_IRCReceiver = class(TIRCMsgReceiver)
  protected
  public
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { TWHOREPLY_IRCReceiver }

  TWHOREPLY_IRCReceiver = class(TIRCMsgReceiver)
  protected
  public
    procedure Receive(vCommand: TIRCCommand); override;
  end;

  { THELP_IRCReceiver }

  THELP_IRCReceiver = class(TIRCReceiver)
  protected
  public
    function DoAccept(S: utf8string): Boolean; override;
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
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TID_UserCommand }

  TID_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TJoin_UserCommand }

  TJoin_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TPart_UserCommand }

  TPart_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TMode_UserCommand }

  TMode_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TSend_UserCommand }

  TSend_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TMe_UserCommand }

  TMe_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { THelp_UserCommand }

  THelp_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { THistory_UserCommand }

  THistory_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TNotice_UserCommand }

  TNotice_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

  { TCNotice_UserCommand }

  TCNotice_UserCommand = class(TIRCUserCommand) //CTCP
  protected
    procedure Send(vChannel: utf8string; vMsg: utf8string); override;
  public
  end;

implementation

uses
  mnOpenSSL,
  SysUtils;

const
  sDefaultPort = '6667';

function ScanString(vStr: utf8string; var vPos:Integer; out vCount: Integer; vChar: UTF8Char; vSkip: Boolean = False): Boolean;
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

procedure ParseSender(Original: utf8string; out Nick, Address: utf8string);
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

procedure ParseUserName(const AUser: utf8string; out NickName: utf8string; out Mode: TIRCUserModes);
var
  c: UTF8Char;
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

procedure StringToUserStates(ModeStr: utf8string; var Mode: TIRCUserModes; UpdateMode: Boolean = False);
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

procedure StringToUserMode(ModeStr: utf8string; var Mode: TIRCUserModes; UpdateMode: Boolean = False);
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

procedure StringToChannelMode(ModeStr: utf8string; var Mode: TIRCChannelModes; UpdateMode: Boolean = False);
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

function ExtractNickFromAddress(Address: utf8string): utf8string;
var
  EndOfNick: Integer;
begin
  Result := '';
  EndOfNick := Pos('!', Address);
  if EndOfNick > 0 then
    Result := Copy(Address, 1, EndOfNick - 1);
end;

{ TDCC_IRCReceiver }

procedure TDCC_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited DoExecute(vCommand, NextCommand);
end;

procedure TDCC_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  inherited Receive(vCommand);
end;

{ TIRC_CTCP }

procedure TIRC_CTCP.AddParam(Value: utf8string);
begin
  SetLength(Params, Length(Params) + 1);
  Params[Length(Params) - 1] := Value;
end;

function TIRC_CTCP.PullParam(out Param: utf8string): Boolean;
begin
  Result := Length(Params) > 0;
  if Result then
  begin
    Param := Params[0];
    Delete(Params, 0, 1);
  end
  else
    Param := '';
end;

function TIRC_CTCP.PullParam: utf8string;
begin
  PullParam(Result);
end;

{ TACTION_IRCReceiver }

procedure TAction_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.Msg := vCommand.CTCP.Message;
  Echo := True;
end;

procedure TAction_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  inherited;
  Client.Receive(mtAction, vCommand.Received);
end;

{ TVersion_IRCReceiver }

procedure TVersion_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  Client.Connection.SendRaw(Format('NOTICE %s :' + cCTCPChar + 'VERSION :miniIRC version 0.8' + cCTCPChar, [vCommand.User]));
end;

{ TWHOREPLY_IRCReceiver }

procedure TWHOREPLY_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aUser: TIRCUser;
begin
  //http://faerion.sourceforge.net/doc/irc/whox.var
  if vCommand.Code = IRC_RPL_ENDOFWHO then
  begin
    Client.DoWho(vCommand.Target);
  end
  else if vCommand.Code = IRC_RPL_WHOSPCRPL then
  begin
{    aChannelName := vCommand.PullParam;
    ParseUserName(vCommand.PullParam, aUserName, aModes);
    oChannel := Client.Session.Channels.Found(aChannelName);
    aUser := oChannel.UpdateUser(aUserName);
    aUser.WhoIs.RealName := vCommand.Text;}
  end
  else if vCommand.Code = IRC_RPL_WHOREPLY then
  begin
    //:verne.freenode.net 352 zaher #parmaja ~zaher 0.0.0.0 verne.freenode.net zaher H :0 zaher
    //:cs.server 352 zaher #parmaja ~zaher mynmr5zkhm9pe.irc cs.server zaher H@ :0 zaher
    aUser := Client.Session.Channels.UpdateUser('', vCommand.Target);
    aUser.WhoIs.Account := vCommand.PullParam;
    aUser.WhoIs.Host := vCommand.PullParam;
    aUser.WhoIs.Server := vCommand.PullParam;
    aUser.WhoIs.UserName := vCommand.PullParam;
    aUser.WhoIs.RealName := vCommand.Text;
  end;
end;

{ TWHOISREPLY_IRCReceiver }

procedure TWHOISREPLY_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aUser: TIRCUser;
begin
  if vCommand.Code = IRC_RPL_ENDOFWHOIS then
  begin
    Client.DoWhoIs(vCommand.Target);
  end
  else if vCommand.Code = IRC_RPL_WHOISCHANNELS then
  begin
    aUser := Client.Session.Channels.UpdateUser('', vCommand.Target);
    aUser.WhoIs.Channels := vCommand.Text;
  end
  else if vCommand.Code = IRC_RPL_WHOISUSER then
  begin
    aUser := Client.Session.Channels.UpdateUser('', vCommand.Target);
    aUser.WhoIs.RealName := vCommand.Text;
  end
  else if vCommand.Code = IRC_RPL_WHOISSERVER then
  begin
    aUser := Client.Session.Channels.UpdateUser('', vCommand.Target);
    aUser.WhoIs.Server := vCommand.PullParam + ' - ' + vCommand.Text;
  end
end;

{ TIRCQueueCommand }

function TIRCQueueCommand.Execute: TIRCQueueCommand;
begin
  if Receiver <> nil then
    Result := Receiver.Execute(Self)
  else
    Result := nil;
end;

procedure TIRCQueueCommand.Receive;
begin
  if Receiver <> nil then
    Receiver.Receive(Self);
  Free;//Kamikaze
end;

{ TID_UserCommand }

procedure TID_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('NICKSERV IDENTIFY ' + vMsg, prgReady);
end;

{ TIRCSocketStream }

procedure TIRCSocketStream.DoHandleError(var Handle: Boolean; AError: Integer);
begin
  Handle := True;
end;

{ TPart_UserCommand }

procedure TPart_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
var
  aChannel: utf8string;
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

procedure TIRCSession.SetNick(ANick: utf8string);
begin
  Nick := ANick;
end;

{ TMode_UserCommand }

procedure TMode_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('MODE ' + vChannel + ' '+ vMsg, prgReady);
end;

{ THistory_UserCommand }

procedure THistory_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('HISTORY ' + vChannel + ' ' + vMsg, prgReady);
end;

{ THELP_IRCReceiver }

function THELP_IRCReceiver.DoAccept(S: utf8string): Boolean;
begin
  Result := inherited DoAccept(S);
  if (S = IntToStr(IRC_RPL_HELPTXT)) or (S = IntToStr(IRC_RPL_ENDOFHELP)) then
  begin
    Result := True;
  end;
end;

procedure THELP_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtWelcome, vCommand.Received);
end;

{ THelp_UserCommand }

procedure THelp_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('HELP ' + vMsg, prgReady);
end;

{ TMe_UserCommand }

procedure TMe_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('PRIVMSG ' + vChannel + ' :' + cCTCPChar + 'ACTION ' + vMsg + cCTCPChar, prgReady);
end;

{ TCNotice_UserCommand }

procedure TCNotice_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('NOTICE ' + vChannel + ' :' + cCTCPChar + vMsg + cCTCPChar, prgReady);
end;

{ TIRCMsgReceiver }

procedure TIRCMsgReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.Msg := vCommand.PullParam;
end;

{ TNotice_UserCommand }

procedure TNotice_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('NOTICE ' + vChannel + ' :' + vMsg, prgReady);
end;

{ TSend_UserCommand }

procedure TSend_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw('PRIVMSG ' + vMsg, prgReady);
end;

{ TIRCUserReceiver }

procedure TIRCUserReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.User := vCommand.Sender;
  vCommand.Target := vCommand.PullParam;
  if LeftStr(vCommand.Target, 1) = '#' then
    vCommand.Channel := vCommand.Target
  {else if vCommand.FAddress <> '' then //when private chat Active , sender is the channel, right?!!!
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

procedure TQUIT_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtQuit, vCommand.Received);
  Client.Session.Channels.RemoveUser(vCommand.Channel, vCommand.User);
end;

procedure TQUIT_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  //:Support_1!~Zaher@myip QUIT Quit
  vCommand.User := vCommand.Sender;
  vCommand.Msg := vCommand.PullParam;
end;

{ TPART_IRCReceiver }

procedure TPART_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtPart, vCommand.Received);
  Client.Session.Channels.RemoveUser(vCommand.Channel, vCommand.User);
end;

{ TJOIN_IRCReceiver }

procedure TJOIN_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  Client.Session.Channels.UpdateUser(vCommand.Channel, vCommand.User);
end;

procedure TJOIN_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtJoin, vCommand.Received);
end;

{ TIRCQueueRaws }

procedure TIRCQueueRaws.Add(Raw: utf8string; When: TIRCProgress);
var
  Item: TIRCRaw;
begin
  Item := TIRCRaw.Create;
  Item.When := When;
  Item.Raw := Raw;
  Add(Item);
end;

{ TIRCChannel }

function TIRCChannel.Add(vUserName: utf8string): TIRCUser;
begin
  Result := TIRCUser.Create;
  Result.Name := vUserName;
  Add(Result);
end;

function TIRCChannel.UpdateUser(vUser: utf8string; UpdateMode: Boolean): TIRCUser;
var
  aNickName: utf8string;
  aMode: TIRCUserModes;
begin
  ParseUserName(vUser, aNickName, aMode);
  Result := Find(aNickName);
  if Result = nil then
    Result := Add(aNickName);
  if UpdateMode then
    Result.Mode := aMode;
end;

procedure TIRCChannel.RemoveUser(vUser: utf8string);
var
  aNickName: utf8string;
  aMode: TIRCUserModes;
  aUser: TIRCUser;
begin
  ParseUserName(vUser, aNickName, aMode);
  aUser := Find(aNickName);
  if aUser = nil then
    Remove(aUser);
end;

{ TIRCChannels }

function TIRCChannels.Found(vChannel: utf8string): TIRCChannel;
begin
  Result := Find(vChannel);
  if Result = nil then
  begin
    Result := TIRCChannel.Create;
    Result.Name := vChannel;
    Add(Result);
  end;
end;

function TIRCChannels.FindUser(const vChannel, vUser: utf8string): TIRCUser;
var
  oChannel: TIRCChannel;
begin
  oChannel := Find(vChannel);
  if oChannel <> nil then
    Result := oChannel.Find(vUser)
  else
    Result := nil;
end;

procedure TIRCChannels.RemoveUser(vChannel, vUser: utf8string);
var
  aChannel: TIRCChannel;
begin
  aChannel := Find(vChannel);
  if aChannel <> nil then
    aChannel.RemoveUser(vUser);
end;

function TIRCChannels.UpdateUser(vChannel, vUser: utf8string; UpdateMode: Boolean): TIRCUser;
var
  aChannel: TIRCChannel;
begin
  aChannel := Found(vChannel);
  Result := aChannel.UpdateUser(vUser, UpdateMode);
end;

function TIRCChannels.Find(const Name: utf8string): TIRCChannel;
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

procedure TRaw_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
begin
  Client.SendRaw(vMsg, prgConnected);
end;

{ TNAMREPLY_IRCReceiver }

procedure TNAMREPLY_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aUserName: utf8string;
  aModes: TIRCUserModes;
  aUser: TIRCUser;

  aChannelName: utf8string;
  oChannel: TIRCChannel;

  aUsers: TStringList;
  i: Integer;
begin
  // triggered by raw /NAMES
  //:server 353 zaher = #support user1
  //:server 353 zaherdirkey = ##support :user1 user2 user3

  if vCommand.Code = IRC_RPL_ENDOFNAMES then
  begin
    aChannelName := vCommand.Params[1];
    oChannel := Client.Session.Channels.Found(aChannelName);
    if oChannel <> nil then
    begin
      oChannel.Adding := False;
      Client.DoUsersChanged(aChannelName, oChannel);
    end;
  end
  else if vCommand.Code = IRC_RPL_NAMREPLY then
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

constructor TIRCUserCommand.Create(AName: utf8string; AClient: TmnIRCClient);
begin
  inherited Create;
  FClient := AClient;
  Name := AName;
end;

{ TJoin_UserCommand }

procedure TJoin_UserCommand.Send(vChannel: utf8string; vMsg: utf8string);
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

procedure TMODE_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
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
  aUser, aMode: utf8string;
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

procedure TNICK_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.User := vCommand.Sender;
end;

procedure TNICK_IRCReceiver.Receive(vCommand: TIRCCommand);
var
  aNewNick: utf8string;
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

procedure TWELCOME_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.User := vCommand.PullParam;
  vCommand.Msg := vCommand.PullParam;
  Client.SetProgress(prgReady);
  Client.FSession.ServerChannel := vCommand.Sender;
end;

procedure TWELCOME_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  with Client do
  begin
    Receive(mtWelcome, vCommand.Received);
  end;
end;

{ TTOPIC_IRCReceiver }

procedure TTOPIC_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
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

procedure TIRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
end;

function TIRCReceiver.Execute(vCommand: TIRCCommand): TIRCQueueCommand;
begin
  Result := nil;
  DoExecute(vCommand, Result);
end;

function TIRCReceiver.DoAccept(aName: utf8string): Boolean;
begin
  Result := False;
end;

function TIRCReceiver.Accept(aName: utf8string; aSubName: utf8string; aCTCP: Boolean): Boolean;
var
  aCode, i: Integer;
begin
  Result := ((Name <> '') and SameText(aName, Name));
  Result := Result and SameText(aSubName, SubName) and (CTCP = aCTCP);
  if not Result and (Length(Codes) > 0) then
  begin
    aCode := StrToIntDef(aName, 0);
    if aCode > 0 then
    begin
      for i := 0 to Length(Codes) - 1 do
      begin
        Result := (aCode = Codes[i]);
        if Result then
          Break;
      end;
    end;
  end;
  Result := Result or DoAccept(aName);
end;

constructor TIRCReceiver.Create(AClient: TmnIRCClient);
begin
  inherited Create;
  FClient := AClient;
end;


procedure TPRIVMSG_IRCReceiver.Receive(vCommand: TIRCCommand);
begin
  Client.Receive(mtMessage, vCommand.Received);
end;

{ TPing_IRCReceiver }

procedure TPing_IRCReceiver.DoExecute(vCommand: TIRCCommand; var NextCommand: TIRCQueueCommand);
begin
  inherited;
  vCommand.Queue := False;
  vCommand.Msg := vCommand.PullParam;
  Client.Connection.SendRaw(Format('PONG %s', [vCommand.Msg]));
end;

{ TmnIRCConnection }

function TmnIRCConnection.InitStream: Boolean;
var
  ReconnectTime: Integer;
begin
  if (FStream = nil) and Active then
    FStream := CreateStream;

  if not Terminated then
  begin
    if not FStream.Connected and Active then
    begin
      try
        if Tries > 0 then //delay if not first time
        begin
          Lock.Enter;
          try
            ReconnectTime := Client.ReconnectTime;
          finally
            Lock.Leave;
          end;
          Log('Reconnecting after ' + IntToStr(ReconnectTime));
          FDelayEvent.WaitFor(ReconnectTime);
          Log('Reconnecting...');
        end
        else
          Log('Connecting...');
        Queue(Client.Connecting);
        FStream.Connect;
        if FStream.Connected then
        begin
          FInternalConnected := True;
          Log('Connected successed');
          Queue(Client.Connected);
        end;
        Inc(Tries);
      except
        on E: Exception do
        begin
          Log('Connected failed:' + E.Message);
          FreeAndNil(FStream);
        end;
      end;
    end;
    if not FStream.Connected and FInternalConnected then
    begin
      FInternalConnected := False;
      Log('Disconnected');
      Queue(Client.Disconnected);
    end;
  end;

  Result := (FStream <> nil) and FStream.Connected;
end;

function TmnIRCConnection.CreateStream: TIRCSocketStream;
var
  Options: TmnsoOptions;
begin
  Options := [soNoDelay]; //To send messages immediately
  if Client.UseSSL then
    Options := Options + [soSSL, soWaitBeforeRead]; //soWaitBeforeRead to fix

  Result := TIRCSocketStream.Create(Host, Port, Options);
  Result.ConnectTimeout := -1;
  //Result.ReadTimeout := -1;
  Result.ReadTimeout := 1000;
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
  Synchronize(Client.Opened);
end;

procedure TmnIRCConnection.SendRaws;
var
  i: Integer;
  Raw: utf8string;
begin
  i := 0;
  while (FStream <> nil) and (FStream.Connected) and (i < Client.QueueSends.Count) do
  begin
    if Client.QueueSends[i].When <= Client.Progress then
    begin
      Lock.Enter;
      try
        Raw := Client.QueueSends[i].Raw;
        Client.QueueSends.Delete(i);
      finally
        Lock.Leave;
      end;
      if Connected then
        SendRaw(Raw);
    end
    else
      inc(i);
  end;
end;

procedure TmnIRCConnection.Process;
var
  aLine: utf8string;
  aCommand: TIRCQueueCommand;
  aNextCommand: TIRCQueueCommand;
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
        Log('<' + aLine);
        aNextCommand := nil;
        aCommand := ParseRaw(aLine);
        while aCommand <> nil do
        begin
          aNextCommand := aCommand.Execute;
          if (aCommand.Receiver <> nil) and aCommand.Queue then
            Queue(aCommand.Receive)
          else
            aCommand.Receive;
          //Command now freed by it self, do not use it
          aCommand := aNextCommand;
        end;

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
  Tries := 0;
  if Connected then
    Stop;
  Synchronize(Client.Closed);
  FreeAndNil(FStream);
end;

procedure ParseCTCP(var CTCP: TIRC_CTCP; vData: utf8string);
var
  p: Integer;
  Start, Count: Integer;
  s: utf8string;
  Index: Integer;
begin
  p := 1;
  Index := 0;
  while p < Length(vData) do
  begin
    if vData[p] <> ':' then
    begin
      Start := p;
      ScanString(vData, p, Count, cTokenSeparator, True);
      if Count > 0 then
      begin
        s := MidStr(vData, Start, Count);
        if Index = 0 then
        begin
          CTCP.Command := s;
          CTCP.Message := MidStr(vData, p, MaxInt);
        end
        else
          CTCP.AddParam(s);
        Index := Index + 1;
      end;
    end
    else
    begin
      CTCP.Text := MidStr(vData, p + 1, MaxInt);
      CTCP.AddParam(CTCP.Text);
      Break;
    end
  end;
end;

procedure ParseBody(vCommand: TIRCCommand; vData: utf8string); //move it to Command.Parse
var
  p: Integer;
  Start, Count: Integer;
begin
  p := 1;
  while p < Length(vData) do
  begin
    if vData[p] <> ':' then
    begin
      Start := p;
      ScanString(vData, p, Count, cTokenSeparator, True);
      if Count > 0 then
      begin
        vCommand.AddParam(MidStr(vData, Start, Count));
      end;
    end
    else
    begin
      vCommand.FText := MidStr(vData, p + 1, MaxInt);
      if LeftStr(vCommand.FText, 1) = #01 then
      begin
        vCommand.CTCP.IsCTCP := True;
        vCommand.FText := DequoteStr(vCommand.FText, #01);
        ParseCTCP(vCommand.CTCP, vCommand.Text);
      end;
      vCommand.AddParam(vCommand.FText);
      Break;
    end
  end;
end;

function TmnIRCConnection.ParseRaw(vData: utf8string): TIRCQueueCommand;
type
  TParseState = (prefix, command, message);
var
  p: Integer;
  aState: TParseState;
  Start, Count: Integer;
  ABody: utf8string;
begin
  //:zaherdirkey!~zaherdirkey@myip PRIVMSG channel :hi
  if vData <> '' then
  begin
    Result := TIRCQueueCommand.Create;
    Result.FReceived.Time := Now;
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
      else if (p = 1) and (vData[p] = cCTCPChar) then //idk what is this
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
          Result.FCode := StrToIntDef(Result.FName, 0);
          ABody := MidStr(vData, Start + Count + 1, MaxInt);
          aState := command;
          Inc(aState);
        end;
        break;
      end;
    end;

    ParseBody(Result, ABody);
    Result.Receiver := Client.Receivers.Find(Result.Name, Result.CTCP.Command, Result.CTCP.IsCTCP);
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
  FDelayEvent := TEvent.Create(nil, True, False, '');
end;

destructor TmnIRCConnection.Destroy;
begin
  FreeAndNil(FDelayEvent);
  inherited;
end;

procedure TmnIRCConnection.Connect;
begin
  inherited;
end;

procedure TmnIRCConnection.Stop;
begin
  inherited;
  if FDelayEvent <> nil then
    FDelayEvent.SetEvent;
  if FStream <> nil then
    FStream.Disconnect;
end;

{ TIRCCommand }

constructor TIRCCommand.Create;
begin
  inherited;
  FParams := TStringList.Create;
  Queue := True; //TODO make it False
end;

destructor TIRCCommand.Destroy;
begin
  FreeAndNil(FParams);
end;

procedure TIRCCommand.AddParam(AValue: utf8string);
begin
  Params.Add(AValue);
end;

function TIRCCommand.PullParam: utf8string;
begin
  PullParam(Result);
end;

function TIRCCommand.PullParam(out Param: utf8string): Boolean;
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

function TCustomIRCReceivers.Add(vName: utf8string; vCodes: TArray<Integer>; AClass: TIRCReceiverClass): Integer;
begin
  Result := Add(vName, '', False, vCodes, AClass);
end;

function TCustomIRCReceivers.Add(vName: utf8string; vSubName: utf8string; vCTCP: Boolean; vCodes: TArray<Integer>; AClass: TIRCReceiverClass): Integer;
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
  AReceiver.SubName := vSubName;
  AReceiver.CTCP := vCTCP;
  AReceiver.Codes := vCodes;
  Result := inherited Add(AReceiver);
end;

function TCustomIRCReceivers.Find(aName, aSubName: utf8string; aCTCP: Boolean): TIRCReceiver;
var
  aReceiver: TIRCReceiver;
begin
  Result := nil;
  for aReceiver in Client.Receivers do
  begin
    if aReceiver.Accept(AName, ASubName, aCTCP) then
    begin
      Result := aReceiver;
      break;
    end;
  end;
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

{ TmnIRCClient }

procedure TmnIRCClient.Stop;
begin
  if Assigned(FConnection) then
  begin
    Connection.FActive := False;
    if (FConnection.Connected) then
      Quit('Bye');

    FConnection.Terminate;//no stop
    FConnection.Stop; //should use TerminateSet, but not exists in FPC yet
    FConnection.WaitFor;
  end;
  NickIndex := 0;
end;

procedure TmnIRCClient.Start;
begin
  NickIndex := 0;
  if (Progress < prgConnecting) then
  begin
    if UseSSL then
      InitOpenSSL;
    FreeAndNil(FConnection);
    CreateConnection;
    Connection.FActive := True;
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

  FReconnectTime := 5000;
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
  Stop;
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

procedure TmnIRCClient.JoinChannels;
var
  i: Integer;
  AChannles: TStringList;
begin
  AChannles := TStringList.Create;
  try
    for i := 0 to Session.Channels.Count - 1 do
      AChannles.Add(Session.Channels[i].Name);

    for i := 0 to AChannles.Count - 1 do
      Join(AChannles[i]);
  finally
    AChannles.Free;
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

procedure TmnIRCClient.SendRaw(vData: utf8string; vQueueAt: TIRCProgress);
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

procedure TmnIRCClient.DoMsgReceived(vChannel, vUser, vTarget, vMessage: utf8string);
begin
end;

procedure TmnIRCClient.DoMsgSent(vUser, vNotice: utf8string);
begin
end;

procedure TmnIRCClient.DoNotice(vChannel, vUser, vTarget, vNotice: utf8string);
begin
end;

procedure TmnIRCClient.DoUserJoined(vChannel, vUser: utf8string);
begin
end;

procedure TmnIRCClient.DoUserLeft(vChannel, vUser: utf8string);
begin

end;

procedure TmnIRCClient.DoUserParted(vChannel, vUser: utf8string);
begin
end;

procedure TmnIRCClient.DoUserQuit(vUser: utf8string);
begin
end;

procedure TmnIRCClient.DoTopic(vChannel, vTopic: utf8string);
begin
end;

procedure TmnIRCClient.DoConnected;
begin
end;

procedure TmnIRCClient.DoDisconnected;
begin
end;

procedure TmnIRCClient.DoClosed;
begin
end;

procedure TmnIRCClient.DoOpened;
begin
end;

procedure TmnIRCClient.DoLog(vMsg: utf8string);
begin
end;

procedure TmnIRCClient.ChannelSend(vChannel, vMsg: utf8string);
begin
  SendMsg(vChannel, vMsg);
end;

procedure TmnIRCClient.SendMsg(vChannel, vMsg: utf8string);
var
  aCMD: TIRCUserCommand;
  aCMDProcessed: Boolean;
  s, m: utf8string;
  p, c: Integer;
  aReceived: TIRCReceived;
begin
  Initialize(aReceived);
  aCMDProcessed := False;
  //check if it user command, like /j
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
    aReceived.Time := Now;
    aReceived.Channel := vChannel;
    aReceived.Target := vChannel;
    aReceived.User := Session.Nick;
    aReceived.Msg := vMsg;
    Receive(mtSend, aReceived); //echo, not here //TODO
  end;
end;

procedure TmnIRCClient.Identify;
begin
  SendRaw(Format('NICKSERV IDENTIFY %s %s', [Username, Password]), prgConnected);
end;

procedure TmnIRCClient.Join(Channel: utf8string; Password: utf8string);
begin
  SendRaw(Format('JOIN %s %s', [Channel, Password]), prgReady);
end;

procedure TmnIRCClient.Kick(vChannel, vNickName: utf8string);
begin
  SendRaw(Format('KICK %s %s', [vChannel, vNickName]), prgReady);
end;

procedure TmnIRCClient.OpUser(vChannel, vNickName: utf8string);
begin
  SendRaw(Format('MODE %s +o %s', [vChannel, vNickName]), prgReady);
end;

procedure TmnIRCClient.DeopUser(vChannel, vNickName: utf8string);
begin
  SendRaw(Format('MODE %s -o %s', [vChannel, vNickName]), prgReady);
end;

procedure TmnIRCClient.WhoIs(vNickName: utf8string);
begin
  SendRaw(Format('WHOIS %s ', [vNickName]), prgReady);
end;

procedure TmnIRCClient.Who(vChannel: utf8string);
begin
  SendRaw(Format('WHO %s ', [vChannel]), prgReady);
end;

procedure TmnIRCClient.Notice(vChannel, vMsg: utf8string);
begin
  SendRaw(Format('NOTICE %s :%s', [vChannel, vMsg]), prgReady);
end;

procedure TmnIRCClient.Closed;
begin
  SetProgress(prgDisconnected);
  FSession.Nick := '';
  FSession.AllowedUserModes := [];
  FSession.AllowedChannelModes := [];
  FSession.Channels.Clear;
  DoClosed;
end;

procedure TmnIRCClient.Connected;
var
  aNick: utf8string;
begin
  //Need to check if it first time connected or reconnected after forcefully disconnected
  SetProgress(prgConnected);
  if Nicks.Count = 0 then
    raise Exception.Create('You should define nicknames list');
  aNick := Nicks[0];
  Inc(NickIndex);

  Session.SetNick(aNick);
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
  JoinChannels;
  DoConnected;
end;

procedure TmnIRCClient.Disconnected;
begin
  SetProgress(prgDisconnected);
  DoDisconnected;
end;

procedure TmnIRCClient.Opened;
begin
end;

procedure TmnIRCClient.SetNick(const ANick: utf8string);
begin
  if ANick <> '' then
  begin
    SendRaw(Format('NICK %s', [ANick]), prgConnected);
  end;
end;

procedure TmnIRCClient.SetTopic(AChannel: utf8string; const ATopic: utf8string);
begin
  SendRaw(Format('TOPIC %s %s', [AChannel, ATopic]));
end;

procedure TmnIRCClient.SetPassword(const Value: utf8string);
begin
  FPassword := Value;
end;

function TmnIRCClient.GetOnline: Boolean;
begin
  Result := (FConnection <> nil) and FConnection.Connected and FConnection.Active and (Progress > prgConnected);
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

procedure TmnIRCClient.SetPort(const Value: utf8string);
begin
  FPort := Value;
end;

procedure TmnIRCClient.SetRealName(const Value: utf8string);
begin
  FRealName := Value;
end;

procedure TmnIRCClient.SetHost(const Value: utf8string);
begin
  FHost := Value;
end;

procedure TmnIRCClient.Connecting;
begin
  SetProgress(prgConnecting);
end;

procedure TmnIRCClient.SetUsername(const Value: utf8string);
begin
  FUsername := Value;
end;

procedure TmnIRCClient.SetUseSSL(AValue: Boolean);
begin
  if FUseSSL =AValue then Exit;
  FUseSSL :=AValue;
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
  ModeString: utf8string;
begin
  ModeString := UserModeToString(Value);
  if Length(ModeString) > 0 then
    SendRaw(Format('MODE %s %s', [FSession.Nick, ModeString]), prgConnected);
end;

procedure TmnIRCClient.SetChannelMode(const Value: TIRCChannelMode);
begin

end;

procedure TmnIRCClient.DoProgressChanged;
begin

end;

procedure TmnIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: utf8string);
begin
end;

procedure TmnIRCClient.DoUsersChanged(vChannelName: utf8string; vChannel: TIRCChannel);
begin

end;

procedure TmnIRCClient.DoUserChanged(vChannel: utf8string; vUser, vNewNick: utf8string);begin

end;

procedure TmnIRCClient.DoWhoIs(vUser: utf8string);
begin

end;

procedure TmnIRCClient.DoWho(vChannel: utf8string);
begin

end;

procedure TmnIRCClient.DoMyInfoChanged;
begin

end;

procedure TmnIRCClient.GetCurrentChannel(out vChannel: utf8string);
begin
  vChannel := '';
end;

function TmnIRCClient.MapChannel(vChannel: utf8string): utf8string;
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

procedure TmnIRCClient.Quit(Reason: utf8string);
begin
  SendRaw(Format('QUIT :%s', [Reason]), prgReady);
end;

function TmnIRCClient.UserModeToString(NewModes: TIRCUserModes): utf8string;
  function GetModeChars(aMode: TIRCUserMode): utf8string;
  begin
    case aMode of
      umInvisible:
        Result := 'i';
      umAway:
        Result := 'a';
      umVoice:
        Result := 'v';
      umHalfOp:
        Result := 'h';
      umOp:
        Result := 'o';
      umWallOp:
        Result := 'w';
      umServerNotices:
        Result := 's';
      umSSL:
        Result := 'Z';
      else
        Result := '';
    end;
  end;
var
  Mode: TIRCUserMode;
begin
  Result := '';

  for Mode := Low(TIRCUserMode) to High(TIRCUserMode) do
  begin
    if Mode in NewModes then
      Result := Result + GetModeChars(Mode);
  end;

end;

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

procedure TmnIRCClient.Log(S: utf8string);
begin
  DoLog(S);
end;

procedure TmnIRCClient.Init;
begin
  Receivers.Add('PRIVMSG', [], TPRIVMSG_IRCReceiver);
  Receivers.Add('PRIVMSG', 'VERSION', True, [], TVersion_IRCReceiver);
  Receivers.Add('PRIVMSG', 'ACTION', True, [], TAction_IRCReceiver);
  Receivers.Add('PRIVMSG', 'DCC', True, [], TDCC_IRCReceiver); //TODO

  Receivers.Add('NOTICE', [], TNOTICE_IRCReceiver);

  Receivers.Add('MODE', [], TMODE_IRCReceiver);
  Receivers.Add('PING', [], TPING_IRCReceiver);

  Receivers.Add('MOTD', [IRC_RPL_MOTD], TMOTD_IRCReceiver);
  Receivers.Add('', [IRC_RPL_TOPIC], TTOPIC_IRCReceiver);
  Receivers.Add('TOPIC', [], TTOPIC_IRCReceiver);//not same with IRC_RPL_TOPIC
  Receivers.Add('NICK', [], TNICK_IRCReceiver);
  Receivers.Add('WELCOME', [IRC_RPL_WELCOME], TWELCOME_IRCReceiver);
  Receivers.Add('MYINFO', [IRC_RPL_MYINFO], TMYINFO_IRCReceiver);
  Receivers.Add('JOIN', [], TJOIN_IRCReceiver);
  Receivers.Add('PART', [], TPART_IRCReceiver);
  Receivers.Add('QUIT', [], TQUIT_IRCReceiver);
  Receivers.Add('NAMREPLY', [IRC_RPL_NAMREPLY, IRC_RPL_ENDOFNAMES], TNAMREPLY_IRCReceiver);
  Receivers.Add('WHOISREPLY', [IRC_RPL_WHOISUSER, IRC_RPL_WHOISACCOUNT, IRC_RPL_WHOISIDLE, IRC_RPL_WHOISOPERATOR, IRC_RPL_WHOISSERVER, IRC_RPL_WHOISCHANNELS, IRC_RPL_ENDOFWHOIS], TWHOISREPLY_IRCReceiver);
  Receivers.Add('WHOREPLY', [IRC_RPL_WHOREPLY, IRC_RPL_WHOSPCRPL, IRC_RPL_ENDOFWHO], TWHOREPLY_IRCReceiver);


  Receivers.Add('HELPSTART', [IRC_RPL_HELPSTART], THELP_IRCReceiver);

  Receivers.Add('ERR_NICKNAMEINUSE', [IRC_ERR_NICKNAMEINUSE], TErrNicknameInUse_IRCReceiver);

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
  //MapChannels.Values['freenode-connect] '] := ''; //Temporary here
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
