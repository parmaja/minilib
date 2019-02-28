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

    //Yes we can read in thread and write in main thread
    https://stackoverflow.com/questions/7418093/use-one-socket-in-two-threads
 *}

interface

uses
  Classes, StrUtils, syncobjs,
  mnClasses, mnSockets, mnSocketStreams, mnClients, mnStreams, mnConnections, mnUtils;

const
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }

  //https://www.alien.net.au/irc/irc2numerics.html
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

type

  { TIRCSocketStream }

  TIRCSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  TmnIRCClient = class;

  TmnIRCProgress = (prgDisconnected, prgRegistering, prgReady);

  TIRCState = (scProgress, scNick, scUserModes, scChannelModes, scChannelNames);
  TIRCStates = set of TIRCState;
  TIRCMsgType = (mtLog, mtNotice, mtWelcome, mtTopic, mtSend, mtReceive);
  TIRCLogType = (lgMsg, lgSend, lgReceive);

  { TIRCCommand }

  TIRCCommand = class(TObject)
  private
    FRaw: String;
    FName: string;

    FTime: string;
    FText: string;
    FSource: string;
    FTarget: string;
    FParams: TStringList;
  protected
    property Raw: String read FRaw;
  public
    constructor Create;
    destructor Destroy; override;

    property Source: string read FSource;
    property Target: string read FTarget;

    property Name: string read FName;//Name or Code
    property Text: string read FText; //Rest body of msg after : also added as last param

    property Params: TStringList read FParams;
  end;

  { TIRCReceiver }

  TIRCReceiver = class(TmnNamedObject)
  private
    FClient: TmnIRCClient;
    FCTCP: Boolean;
  protected
    procedure DoReceive(vCommand: TIRCCommand); virtual; abstract;
    function Accept(S: string): Boolean; virtual;
    procedure Receive(vCommand: TIRCCommand); virtual;
    property Client: TmnIRCClient read FClient;
    procedure DoParse(vCommand: TIRCCommand; vData: string); virtual;
    procedure Parse(vCommand: TIRCCommand; vData: string);
    property CTCP: Boolean read FCTCP;
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
    function Add(vName: String; vCode: Integer; AClass: TIRCReceiverClass; vCTCP: Boolean = false): Integer;
    procedure Receive(ACommand: TIRCCommand);
    property Client: TmnIRCClient read FClient;
  end;

  { TIRCReceivers }

  //Preeminent Receivers
  TIRCReceivers = class(TCustomIRCReceivers)
  public
    procedure Parse(vData: string);
  end;

  { TIRCQueueReceivers }

  //Temporary commands, when received it will deleted
  TIRCQueueReceivers = class(TCustomIRCReceivers)
  end;

  { TIRCUserCommand }

  TIRCUserCommand = class(TmnNamedObject)
  private
    FClient: TmnIRCClient;
  protected
    procedure Send(S: string); virtual; abstract;
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
    Text: string;
  end;

  { TIRCUserName }

  TIRCUserName = class(TmnNamedObject)
  public
    Mode: string;//todo
  end;

  { TIRCChannel }

  TIRCChannel = class(TmnNamedObjectList<TIRCUserName>)
  private
  protected
    Adding: Boolean;
  public
    Name: string; //Channel name
    procedure Add(vUserName: string); overload;
  end;


  { TIRCChannels }

  TIRCChannels = class(TmnObjectList<TIRCChannel>)
  public
    function Find(const Name: string): TIRCChannel;
    function Found(vChannel: string): TIRCChannel;
  end;

  { TIRCQueueRaws }

  TIRCQueueRaws = class(TmnObjectList<TIRCRaw>)
  public
    procedure Add(Raw: string); overload;
  end;

  TUserMode = (umInvisible, umOperator, umServerNotices, umWallops);
  TUserModes = set of TUserMode;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnClientConnection)
  private
    FClient: TmnIRCClient;
    FHost: string;
    FPort: string;
    _Line: string; //for sync
    _Log: string;
    procedure SendRaws;
    procedure Log(ALine: string);
    procedure ClientLog; //To Sync
  protected
    procedure Prepare; override;
    procedure Process; override;
    procedure Unprepare; override;

    procedure ReceiveRaw; //To Sync, to be out of thread
    procedure SendRaw(S: string); //this called by client out of thread

    property Client: TmnIRCClient read FClient;
  public
    constructor Create(vOwner: TmnConnections; vSocket: TmnConnectionStream); override;
    destructor Destroy; override;
    procedure Connect; override;
    property Host: string read FHost;
    property Port: string read FPort;
  end;

  TIRCSession = record
    Channels: TIRCChannels;
    Nick: string;
    Modes: TUserModes;
  end;

  { TmnIRCClient }

  TmnIRCClient = class(TObject)
  private
    FNicks: TStringList;
    FPort: String;
    FHost: String;
    FPassword: String;
    FUsername: String;
    FRealName: String;
    FNick: String;
    FModes: TUserModes;
    FProgress: TmnIRCProgress;
    FSession: TIRCSession;
    FConnection: TmnIRCConnection;
    FReceivers: TIRCReceivers;
    FQueueReceivers: TIRCQueueReceivers;
    FQueueRaws: TIRCQueueRaws;
    FUserCommands: TIRCUserCommands;
    FLock: TCriticalSection;
    FUseUserCommands: Boolean;
    function GetActive: Boolean;
    procedure SetNicks(AValue: TStringList);
    procedure SetPassword(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetRealName(const Value: String);
    procedure SetHost(const Value: String);
    procedure Connected;
    procedure Disconnected;
    procedure Close;
    procedure SetUsername(const Value: String);
    function GetUserModes: TUserModes;
    procedure SetUserModes(const Value: TUserModes);
    function BuildUserModeCommand(NewModes: TUserModes): String;
  protected
    procedure SetState(const Value: TmnIRCProgress);

    procedure Changed(vStates: TIRCStates; vChannel: string = '');
    procedure Receive(vMsgType: TIRCMsgType; vChannel, vMsg: String); virtual;

    procedure ReceiveRaw(vData: String); virtual;
    procedure SendRaw(vData: String; vQueueAt: TmnIRCProgress = prgDisconnected);
    procedure ChannelReceive(vChannel, vMsg: String); virtual;

    procedure DoChanged(vStates: TIRCStates; vChannel: string); virtual;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vMsg: String); virtual;

    procedure InitReceivers;
    procedure InitUserCommands;

    property Connection: TmnIRCConnection read FConnection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    procedure Log(Message: String);

    procedure ChannelSend(vChannel, vMsg: String);

    procedure SetNick(const ANick: String);
    procedure SetTopic(AChannel: String; const ATopic: String);
    procedure Join(Channel: String; Password: string = '');
    procedure Notice(Destination, Text: String);
    procedure Quit(Reason: String);

    property Progress: TmnIRCProgress read FProgress;
    property Receivers: TIRCReceivers read FReceivers;
    property QueueReceivers: TIRCQueueReceivers read FQueueReceivers;
    property QueueRaws: TIRCQueueRaws read FQueueRaws;
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
    property UserModes: TUserModes read GetUserModes write SetUserModes;
    property Session: TIRCSession read FSession;
    property UseUserCommands: Boolean read FUseUserCommands write FUseUserCommands default True;
  end;

  { TPing_IRCReceiver }

  TPing_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TPRIVMSG_IRCReceiver }

  TPRIVMSG_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TNOTICE_IRCReceiver }

  TNOTICE_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TJOIN_IRCReceiver }

  TJOIN_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TPART_IRCReceiver }

  TPART_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TQUIT_IRCReceiver }

  TQUIT_IRCReceiver = class(TIRCReceiver)
  protected
    procedure DoReceive(vCommand: TIRCCommand); override;
  end;

  { TCTCP_PRIVMSG_IRCReceiver }

  TCTCP_PRIVMSG_IRCReceiver = class(TIRCReceiver)
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
    constructor Create(AClient: TmnIRCClient); override;
    destructor Destroy; override;
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
    procedure Send(S: string); override;
  public
  end;

  { TJoin_UserCommand }

  TJoin_UserCommand = class(TIRCUserCommand)
  protected
    procedure Send(S: string); override;
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
  { Remove any redundant }
  if vSkip then
    while (vPos <= Length(vStr)) and (vStr[vPos] = vChar) do
      Inc(vPos);
  Result := vCount > 0;
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

{ TMYINFO_IRCReceiver }

procedure TMYINFO_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.FSession.Nick := vCommand.Params[0];
  //if FModes <> [] then
    //SendRaw(Format('MODE %s %s', [FSession.Nick, BuildUserModeCommand(FModes)]));
  Client.Changed([scNick, scUserModes]);
end;

{ TNOTICE_IRCReceiver }

procedure TNOTICE_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtReceive, vCommand.Params[0], vCommand.Params[1]);
end;

{ TQUIT_IRCReceiver }

procedure TQUIT_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive('', vCommand.Raw);
end;

{ TPART_IRCReceiver }

procedure TPART_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive('', vCommand.Raw);
end;

{ TJOIN_IRCReceiver }

procedure TJOIN_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive('', vCommand.Raw);
end;

{ TCTCP_PRIVMSG_IRCReceiver }

procedure TCTCP_PRIVMSG_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive(vCommand.Params[0], vCommand.Params[1]);
end;

{ TIRCQueueRaws }

procedure TIRCQueueRaws.Add(Raw: string);
var
  Item: TIRCRaw;
begin
  Item := TIRCRaw.Create;
  Item.Text := Raw;
  Add(Item);
end;

{ TIRCChannel }

procedure TIRCChannel.Add(vUserName: string);
var
  aUserName: TIRCUserName;
begin
  aUserName := TIRCUserName.Create;
  aUserName.Name := vUserName;
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
  IsCTCP: Boolean;
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
        { If the current token is a CTCP query, then look for the end of the query instead of the token separator }
        if vData[p] = '@' then
        begin
          Inc(p); //skip it
          Start := p;
          if ScanString(vData, p, Count, cTokenSeparator, True) then
            aCommand.FTime := MidStr(vData, Start, Count);
          //Inc(Progress); no do not increase it, we still except prefix
        end
        else if vData[p] = #1 then //CTCP idk what is this
        begin
          Inc(p); //skip it
          IsCTCP := True;
          //Start := p;
          //find CTCP command and pass the full line
//          Break;
//          Inc(Progress); //nop
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
        if (aReceiver.CTCP = IsCTCP) and aReceiver.Accept(aCommand.Name) then
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

procedure TRaw_UserCommand.Send(S: string);
begin
  Client.SendRaw(S, prgRegistering);
end;

{ TNAMREPLY_IRCReceiver }

constructor TNAMREPLY_IRCReceiver.Create(AClient: TmnIRCClient);
begin
  inherited;
end;

destructor TNAMREPLY_IRCReceiver.Destroy;
begin
  inherited;
end;

function TNAMREPLY_IRCReceiver.Accept(S: string): Boolean;
begin
  Result := inherited Accept(S);
  if S = IntToStr(IRC_RPL_ENDOFNAMES) then
  begin
    Result := True;
    //Pass the list and remove it
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
  Client.Lock.Enter;
  try
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
  finally
    Client.Lock.Leave;
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

procedure TJoin_UserCommand.Send(S: string);
begin
  Client.SendRaw('JOIN ' + S, prgReady);
end;

{ TErrNicknameInUse_IRCReceiver }

procedure TErrNicknameInUse_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  { Handle nick conflicts during the registration process. }
  with Client do
    if FProgress = prgRegistering then
    begin
//      SetNick(FAltNick);
//        Quit('Nick conflict');
    end;
end;

{ TMODE_IRCReceiver }

procedure TMODE_IRCReceiver.DoReceive(vCommand: TIRCCommand);
var
  Index: Integer;
  ModeString: String;
  AddMode: Boolean;
begin
  with Client do
    if vCommand.Params[0] = FNick then
    begin
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
              FSession.Modes := FSession.Modes + [umInvisible]
            else
              FSession.Modes := FSession.Modes - [umInvisible];
          'o':
            if AddMode then
              FSession.Modes := FSession.Modes + [umOperator]
            else
              FSession.Modes := FSession.Modes - [umOperator];
          's':
            if AddMode then
              FSession.Modes := FSession.Modes + [umServerNotices]
            else
              FSession.Modes := FSession.Modes - [umServerNotices];
          'w':
            if AddMode then
              FSession.Modes := FSession.Modes + [umWallops]
            else
              FSession.Modes := FSession.Modes - [umWallops];
        end;
      end;
      Changed([scUserModes]);//TODO need channel name
    end;
end;


{ TNICK_IRCReceiver }

procedure TNICK_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.FSession.Nick := vCommand.Params[0];
  Client.Changed([scNick]);
end;

{ TWELCOME_IRCReceiver }

procedure TWELCOME_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  with Client do
  begin
    Client.Receive(mtWelcome, vCommand.Params[0], vCommand.Params[1]);
    SetState(prgReady);
  end;
end;

{ TTOPIC_IRCReceiver }

procedure TTOPIC_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.Receive(mtTopic, vCommand.Params[1], vCommand.Params[2]);
end;

{ TMOTD_IRCReceiver }

procedure TMOTD_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  Client.ChannelReceive('', vCommand.Text);
end;

{ TIRCReceiver }

procedure TIRCReceiver.Receive(vCommand: TIRCCommand);
begin
  DoReceive(vCommand);
  //Client.Log(lgMsg, Name + ' triggered');
end;

procedure TIRCReceiver.DoParse(vCommand: TIRCCommand; vData: string);
var
  p: Integer;
  Start, Count: Integer;
  procedure AddIt(s: string);
  begin
    vCommand.Params.Add(s);
  end;
begin
  vCommand.Params.Clear;
  if vData <> '' then
  begin
    p := 1;
    while p < Length(vData) do
    begin
      if vData[p] = ':' then
      begin
        vCommand.FText := MidStr(vData, p + 1, MaxInt);
        AddIt(vCommand.Text);
        Break;
      end
      else
      begin
        Start := p;
        ScanString(vData, p, Count, cTokenSeparator, True);
        if Count > 0 then
          AddIt(MidStr(vData, Start, Count));
      end;
    end;
  end;
end;

procedure TIRCReceiver.Parse(vCommand: TIRCCommand; vData: string);
begin
  DoParse(vCommand, vData);
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
  Client.ChannelReceive(vCommand.Params[0], vCommand.Params[1]);
end;

{ TPing_IRCReceiver }

procedure TPing_IRCReceiver.DoReceive(vCommand: TIRCCommand);
begin
  { SendDirect the PONG reply to the PING. }
  Client.SendRaw(Format('PONG %s', [vCommand.Params[0]]));
end;

{ TmnIRCConnection }

procedure TmnIRCConnection.Prepare;
begin
  inherited Prepare;
  Connect;
  Synchronize(Client.Connected);
end;

procedure TmnIRCConnection.SendRaws;
var
  i: Integer;
begin
  //Client.Lock.Enter;
  try
    for i := 0 to Client.QueueRaws.Count - 1 do
    begin
      SendRaw(Client.QueueRaws[i].Text);
    end;
    Client.QueueRaws.Clear;
  finally
    //Client.Lock.Leave;
  end;
end;

procedure TmnIRCConnection.Log(ALine: string);
begin
  _Log := ALine;
  Synchronize(ClientLog);
  _Log := '';
end;

procedure TmnIRCConnection.ClientLog;
begin
  Client.Log(_Log);
end;

procedure TmnIRCConnection.Process;
begin
  inherited Process;
  Synchronize(SendRaws);
  if Stream.WaitToRead(Stream.Timeout) then
  begin
    _Line := Trim(Stream.ReadLine);
    while (_Line <> '') and Active do
    begin
      Synchronize(ReceiveRaw);
      _Line := Trim(Stream.ReadLine);
    end;
  end;
end;

procedure TmnIRCConnection.Unprepare;
begin
  inherited;
  if Connected then
    Disconnect(true);
  Synchronize(Client.Disconnected);
end;

procedure TmnIRCConnection.ReceiveRaw;
begin
  Client.ReceiveRaw(_Line);
end;

procedure TmnIRCConnection.SendRaw(S: string);
begin
  if Stream.Connected then
  begin
    Stream.WriteLine(S);
    Client.Log(S);
  end
  else
    Client.Log('not connected');
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
  Log('Connecting...');
  SetStream(TIRCSocketStream.Create(Host, Port, [soNoDelay, soSafeConnect, soKeepIfReadTimout, soConnectTimeout]));
  Stream.Timeout := 5 * 1000;
  //Stream.Timeout := WaitForEver;// 5 * 1000;
  Stream.EndOfLine := #10;
  inherited;
  if Connected then
    Log('Connected successed')
  else
    Log('Connected failed');
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

function TCustomIRCReceivers.Add(vName: String; vCode: Integer; AClass: TIRCReceiverClass; vCTCP: Boolean): Integer;
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
  AReceiver.FCTCP := vCTCP;
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

procedure TmnIRCClient.Log(Message: String);
begin
  Receive(mtLog, '', Message);
end;

procedure TmnIRCClient.Connect;
begin
  if (FProgress = prgDisconnected) or (FConnection = nil) then
  begin
    FConnection := TmnIRCConnection.Create(nil, nil);
    FConnection.FClient := Self;
    FConnection.FreeOnTerminate := true; //i will free my self
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
  FQueueReceivers := TIRCQueueReceivers.Create(Self);
  FUserCommands :=TIRCUserCommands.Create(Self);
  FQueueRaws := TIRCQueueRaws.Create(True);
  InitReceivers;
  InitUserCommands;
end;

destructor TmnIRCClient.Destroy;
begin
  Close;
  FreeAndNil(FConnection); //+
  FreeAndNil(FUserCommands);
  FreeAndNil(FQueueRaws);
  FreeAndNil(FReceivers);
  FreeAndNil(FQueueReceivers);
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
    if FConnection <> nil then
      FConnection.WaitFor;
  end;
end;

procedure TmnIRCClient.SendRaw(vData: String; vQueueAt: TmnIRCProgress);
begin
  if Assigned(FConnection) then
  begin
    if (FProgress >= vQueueAt) then
      FConnection.SendRaw(vData)
    else
      QueueRaws.Add(vData);
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
      for aCmd in UserCommands do
      begin
        if SameText(aCmd.Name, m) then
        begin
          aCmd.Send(s);
          aCMDProcessed := True;
        end;
      end;
    end;
  end;

  if not aCMDProcessed then
  begin
    SendRaw(Format('PRIVMSG %s :%s', [vChannel, vMsg]), prgReady);
    Receive(mtSend, vChannel, vMsg);
  end;
end;

procedure TmnIRCClient.Join(Channel: String; Password: string);
begin
  SendRaw(Format('JOIN %s %s', [Channel, Password]), prgReady);
end;

procedure TmnIRCClient.Notice(Destination, Text: String);
begin
  SendRaw(Format('NOTICE %s :%s', [Destination, Text]), prgReady);
end;

procedure TmnIRCClient.Disconnected;
begin
  SetState(prgDisconnected);
  FConnection := nil;//it will free self
end;

procedure TmnIRCClient.Connected;
begin
  SetState(prgRegistering);
{  if FPassword <> '' then
    SendRaw(Format('PASS %s', [FPassword]));}
  SetNick(FNick);
  SendRaw(Format('USER %s 0 * :%s', [FUsername, FRealName]));
  SendRaw(Format('NS IDENTIFY %s %s', [FUsername, FPassword]));
end;

procedure TmnIRCClient.SetNick(const ANick: String);
begin
  if ANick <> '' then
  begin
    if FProgress in [prgRegistering, prgReady] then
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

procedure TmnIRCClient.SetState(const Value: TmnIRCProgress);
begin
  if Value <> FProgress then
  begin
    FProgress := Value;
    Changed([scProgress]);
  end;
end;

function TmnIRCClient.GetUserModes: TUserModes;
begin
  Result := FModes;
end;

procedure TmnIRCClient.SetUserModes(const Value: TUserModes);
var
  ModeString: String;
begin
  if FProgress > prgDisconnected then
  begin
    ModeString := BuildUserModeCommand(Value);
    if Length(ModeString) > 0 then
      SendRaw(Format('MODE %s %s', [FSession.Nick, ModeString]));
  end;
  FModes := Value;
end;

procedure TmnIRCClient.ReceiveRaw(vData: String);
begin
  Log(vData);
  Receivers.Parse(vData);
end;

procedure TmnIRCClient.ChannelReceive(vChannel, vMsg: String);
begin
  Receive(mtReceive, vChannel, vMsg);
end;

procedure TmnIRCClient.DoChanged(vStates: TIRCStates; vChannel: string);
begin

end;

procedure TmnIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vMsg: String);
begin
end;

procedure TmnIRCClient.Quit(Reason: String);
begin
  SendRaw(Format('QUIT :%s', [Reason]));
end;

function TmnIRCClient.BuildUserModeCommand(NewModes: TUserModes): String;
const
  ModeChars: array [umInvisible..umWallops] of Char = ('i', 'o', 's', 'w');
var
  ModeDiff: TUserModes;
  Mode: TUserMode;
begin
  Result := '';

  ModeDiff := FSession.Modes - NewModes;
  if ModeDiff <> [] then
  begin
    Result := Result + '-';
    for Mode := Low(TUserMode) to High(TUserMode) do
    begin
      if Mode in ModeDiff then
        Result := Result + ModeChars[Mode];
    end;
  end;

  ModeDiff := NewModes - FSession.Modes;
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

procedure TmnIRCClient.Changed(vStates: TIRCStates; vChannel: string);
begin
  DoChanged(vStates, vChannel);
end;

procedure TmnIRCClient.Receive(vMsgType: TIRCMsgType; vChannel, vMsg: String);
begin
  DoReceive(vMsgType, vChannel, vMsg)
end;

procedure TmnIRCClient.InitReceivers;
begin
  Receivers.Add('PRIVMSG', 0, TPRIVMSG_IRCReceiver);
  Receivers.Add('NOTICE', 0, TNOTICE_IRCReceiver);
  Receivers.Add('PING', 0, TPING_IRCReceiver);
  Receivers.Add('MOTD', IRC_RPL_MOTD, TMOTD_IRCReceiver);
  Receivers.Add('TOPIC', IRC_RPL_TOPIC, TTOPIC_IRCReceiver);
  Receivers.Add('NICK', 0, TNICK_IRCReceiver);
  Receivers.Add('WELCOME', IRC_RPL_WELCOME, TWELCOME_IRCReceiver);
  Receivers.Add('MYINFO', IRC_RPL_MYINFO, TMYINFO_IRCReceiver);
  Receivers.Add('MODE', 0, TMODE_IRCReceiver);
  Receivers.Add('NAMREPLY', IRC_RPL_NAMREPLY, TNAMREPLY_IRCReceiver);
  Receivers.Add('ERR_NICKNAMEINUSE', 0, TErrNicknameInUse_IRCReceiver);

  Receivers.Add('PRIVMSG', 0, TCTCP_PRIVMSG_IRCReceiver, True);
end;

procedure TmnIRCClient.InitUserCommands;
begin
  UserCommands.Add(TRaw_UserCommand.Create('Raw', Self));
  UserCommands.Add(TJoin_UserCommand.Create('Join', Self));
end;

end.
