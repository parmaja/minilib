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
 *}

interface

uses
  Classes, StrUtils, mnSockets, mnClients, mnConnections, mnCommands, mnUtils;

const
  sDefaultPort = '6667';
  sNickName = 'unknown';
  sOtherNickName = 'othername';
  sRealName = 'Real Name';
  sChannel = 'test';

  MAX_TOKEN_LENGTH = 512;   //Defined in RFC1459 as the maximum length of a single token. }
  cTokenSeparator = ' ';    { Separates tokens, except for the following case. }
  cTokenEnclose = ':';  { If the second or higher token starts with this character, it indicates that this token is all characters to the end of the string. }

type

  { TIRCSocketStream }

  TIRCSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  TmnIRCClient = class;

  TIRCLogType = (lgMsg, lgSend, lgReceive);

  { TIRCTokens }

  TIRCTokens = class(TObject)
  private
    FData: String;
    FCount: Integer;
    FTokens: TList;
    FList: TStringList;
    FMessage: string;
    FBuffer: array [0..MAX_TOKEN_LENGTH] of Char;
    procedure SetData(const Value: String);
    function GetTokens(Index: Integer): String;
    function GetTokensFrom(Index: Integer): String;
  protected
    procedure Tokenize; virtual;
  public
    procedure Parse(vData: string); virtual;
    constructor Create;
    destructor Destroy; override;
    property Data: String read FData write SetData;
    property Tokens[Index: Integer]: String read GetTokens; default;
    property TokensFrom[Index: Integer]: String read GetTokensFrom;
    property Count: Integer read FCount;
  end;

  THandlerFunc = procedure(vTokens: TIRCTokens) of object;

  PResponseHandler = ^TResponseHandler;

  { TResponseHandler }

  TResponseHandler = record
    Name: String;
    HandlerFunc: THandlerFunc;
    PrevHandler: PResponseHandler;
  end;

  { TIRCResponseHandlers }

  TIRCResponseHandlers = class(TObject)
  private
    FHandlers: TStringList;
    FIRC: TmnIRCClient;
  public
    constructor Create(AIRC: TmnIRCClient);
    destructor Destroy; override;
    function AddHandler(vName: String; vHandlerFunc: THandlerFunc): Integer;
    procedure DeleteHandler(Index: Integer);
    procedure RemoveHandler(Response: String);
    function IndexOfHandler(vName: String): Integer;
    procedure Handle(vTokens: TIRCTokens);
  end;

  TmnIRCState = (isDisconnected, isConnecting, isRegistering, isReady, isDisconnecting);

  TUserMode = (umInvisible, umOperator, umServerNotices, umWallops);
  TUserModes = set of TUserMode;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnClientConnection)
  private
    FIRC: TmnIRCClient;
    FCommands: TmnCommands;
  protected
    procedure DoReceive(const Data: string); virtual;
    procedure DoLog(const vData: string); virtual;
    procedure Process; override;
  public
    procedure Connect; override;
    property Commands: TmnCommands read FCommands;
  end;

  TOnReceive = procedure(Sender: TObject; vChannel, vMSG: String) of object;
  TOnLogData = procedure(Sender: TObject; vLogType: TIRCLogType; vMsg: String) of object;

  { TmnIRCClient }

  TmnIRCClient = class(TObject) //TmnClientConnection
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
    FTokens: TIRCTokens;
    FCommandClasses: TmnCommandClasses;
    FHandlers: TIRCResponseHandlers;
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

    procedure Receive(vChannel, vMsg: String); virtual;

    procedure UserModeChanged;
    procedure AddHandlers;

    procedure RplPing(vTokens: TIRCTokens);
    procedure RplPrivMSG(vTokens: TIRCTokens);
    procedure RplNick(vTokens: TIRCTokens);
    procedure RplWelcome1(vTokens: TIRCTokens);
    procedure RplErrNicknameInUse(vTokens: TIRCTokens);
    procedure RplMode(vTokens: TIRCTokens);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Close;

    procedure Log(vLogType: TIRCLogType; Message: String);

    procedure Send(Channel, Text: String);
    procedure Join(Channel: String);
    procedure Notice(Destination, Text: String);
    procedure Quit(Reason: String);
    function ExtractNickFromAddress(Address: String): String;
    property State: TmnIRCState read FState;
    property Connection: TmnIRCConnection read FConnection;
    property Handlers: TIRCResponseHandlers read FHandlers;
    property CommandClasses: TmnCommandClasses read FCommandClasses;
    property Tokens: TIRCTokens read FTokens; //temp
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

implementation

uses
  SysUtils;

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

procedure TmnIRCConnection.Connect;
begin
  SetStream(TIRCSocketStream.Create(FIRC.Host, FIRC.Port, [soNoDelay, soConnectTimeout]));
  Stream.Timeout := -1;
  Stream.EndOfLine := #10;
  inherited Connect;
end;

{ TIRCTokens }

constructor TIRCTokens.Create;
begin
  inherited;
  FTokens := TList.Create;
  FList := TStringList.Create;
end;

destructor TIRCTokens.Destroy;
begin
  FreeAndNil(FTokens);
  FreeAndNil(FList);
end;

function TIRCTokens.GetTokens(Index: Integer): String;
var
  TokenStart, TokenEnd: PChar;
begin
  Result := '';
  if Index < FCount then
  begin
    TokenStart := FTokens[Index];
    if TokenStart = nil then
      Exit;
    TokenEnd := nil;
    if Index < FTokens.Count - 1 then
      TokenEnd := StrScan(TokenStart, cTokenSeparator);
    if TokenEnd = nil then
      { Use StrLCopy to protect against buffer overruns. }
      StrLCopy(FBuffer, TokenStart, High(FBuffer))
    else
      StrLCopy(FBuffer, TokenStart, TokenEnd - TokenStart);
    Result := StrPas(FBuffer);
  end;
end;

function TIRCTokens.GetTokensFrom(Index: Integer): String;
var
  TokenStart: PChar;
begin
  Result := '';
  if Index < FCount then
  begin
    TokenStart := FTokens[Index];
    if TokenStart = nil then
      Exit;
    { Use StrLCopy to protect against buffer overruns. }
    StrLCopy(FBuffer, TokenStart, High(FBuffer));
    Result := StrPas(FBuffer);
  end;
end;

procedure TIRCTokens.SetData(const Value: String);
begin
  { If the string is a CTCP query, then skip the Ctrl-A characters at the start
    and end of the query. }
  FData := Value;
  if FData <> '' then
    Tokenize;
end;

procedure TIRCTokens.Tokenize;
var
  TokenPtr: PChar;
  EndOfTokens: Boolean;
begin
  FTokens.Clear;
  FCount := 0;
  TokenPtr := PChar(FData);
  { Remove leading spaces. }
  while (TokenPtr^ <> #0) and (TokenPtr^ = cTokenSeparator) do
    Inc(TokenPtr);
  { In case we reached the end of the string. }
  if TokenPtr^ = #0 then
    Exit;
  if TokenPtr^ <> ':' then
  begin
    { No source address exists, so insert a nil string in its place. }
    FTokens.Add(nil);
    Inc(FCount);
  end
  else
  begin
    { Skip past the semi-colon in the source address. }
    Inc(TokenPtr);
  end;
  { Add the token to the list. }
  FTokens.Add(TokenPtr);
  Inc(FCount);
  while TokenPtr <> nil do
  begin
    { If the current token is a CTCP query, then look for the end of the
      query instead of the token separator. }
    if TokenPtr^ = #1 then
    begin
      TokenPtr := StrScan(TokenPtr + 1, #1);
      { Move past the Ctrl-A to the next character. }
      if TokenPtr <> nil then
        Inc(TokenPtr);
    end
    else
    begin
      TokenPtr := StrScan(TokenPtr, cTokenSeparator);
    end;
    { Remove any redundant separator characters before the token. }
    while (TokenPtr <> nil) and (TokenPtr^ <> #0) and (TokenPtr^ = cTokenSeparator) do
      Inc(TokenPtr);
    { Add it to the list if there Currently is another token. }
    if TokenPtr <> nil then
    begin
      { Skip the end-of-tokens character if it exists. }
      EndOfTokens := (TokenPtr^ = cTokenEnclose);
      if EndOfTokens then
        Inc(TokenPtr);
      if TokenPtr^ <> #0 then
      begin
        FTokens.Add(TokenPtr);
        Inc(FCount);
      end;
      { If there was an end-of-tokens character, then break out. }
      if EndOfTokens then
        Break;
    end;
  end;
end;

procedure ScanString(vStr: string; var vPos: Integer; vChar: Char; vSkip: Boolean = False);
begin
  while (vPos < Length(vStr)) and (vStr[vPos] <> vChar) do
    Inc(vPos);
  if vSkip then
    while (vPos < Length(vStr)) and (vStr[vPos] = vChar) do
      Inc(vPos);
end;

procedure TIRCTokens.Parse(vData: string);
var
  EndOfTokens: Boolean;
  s: string;
  i, j: Integer;
begin
  FList.Clear;
  i := 1;
  j := 0;
  if vData[i] <> ':' then
    FList.Add('') //No source address exists, so insert a nil string in its place.
  else
    Inc(i); //Skip past the semi-colon in the source address.

  while I < Length(vData) do
  begin
    { If the current token is a CTCP query, then look for the end of the
      query instead of the token separator. }
    j := i;
    if vData[i] = #1 then
    begin
      while (i < Length(vData)) and (vData[i] <> #1) do
        Inc(i);
    end
    else
    begin
      while (i < Length(vData)) and (vData[i] <> cTokenSeparator) do
        Inc(i);
      { Remove any redundant separator characters before the token. }
      while (i < Length(vData)) and (vData[i] = cTokenSeparator) do
        Inc(i);
    end;
    { Add it to the list if there Currently is another token. }
    if i < Length(vData) then
    begin
      { Skip the end-of-tokens character if it exists. }
      EndOfTokens := (vData[i] = cTokenEnclose);
      if EndOfTokens then
        Inc(i);
      if i < Length(vData) then
      begin
        s := MidStr(vData, j, i - 1);
        FList.Add(s);
      end;
      { If there was an end-of-tokens character, then break out. }
      if EndOfTokens then
        Break;
    end;
  end;
end;

{ TIRCResponseHandlers }

function TIRCResponseHandlers.AddHandler(vName: String; vHandlerFunc: THandlerFunc): Integer;
var
  Handler: PResponseHandler;
begin
  Result := IndexOfHandler(vName);
  if Result >= 0 then
  begin
    { Create the new handler record. }
    New(Handler);
    { Add the link to the previous handler. }
    Handler^.PrevHandler := PResponseHandler(FHandlers.Objects[Result]);
    { Replace the handler in the existing item. }
    FHandlers.Objects[Result] := TObject(Handler);
  end
  else
  begin
    { Create the handler record. }
    New(Handler);
    Handler^.PrevHandler := nil;
    { Add it to the list. }
    Result := FHandlers.AddObject(vName, TObject(Handler));
  end;
  Handler^.Name := vName;
  Handler^.HandlerFunc := vHandlerFunc;
end;

constructor TIRCResponseHandlers.Create(AIRC: TmnIRCClient);
begin
  inherited Create;
  FHandlers := TStringList.Create;
  FHandlers.Sorted := True;
  FHandlers.Duplicates := dupError;
  FIRC := AIRC;
end;

procedure TIRCResponseHandlers.DeleteHandler(Index: Integer);
var
  Handler: PResponseHandler;
begin
  if (Index >= 0) and (Index < FHandlers.Count) then
  begin
    Handler := PResponseHandler(FHandlers.Objects[Index]);
    { If chained, then simply remove the newest link in the chain. }
    if Handler^.PrevHandler <> nil then
      FHandlers.Objects[Index] := TObject(Handler^.PrevHandler)
    else
      FHandlers.Delete(Index);
    { Free the memory used by the newest link. }
    Dispose(Handler);
  end;
end;

destructor TIRCResponseHandlers.Destroy;
begin
  { Free all the handlers. }
  while FHandlers.Count > 0 do
    DeleteHandler(0);
  FHandlers.Free;
  inherited;
end;

procedure TIRCResponseHandlers.Handle(vTokens: TIRCTokens);
var
  Index: Integer;
  Handler: PResponseHandler;
begin
  Index := IndexOfHandler(vTokens[1]);
  if Index >= 0 then
  begin
    Handler := PResponseHandler(FHandlers.Objects[Index]);
    while Assigned(Handler) do
    begin
      if Assigned(Handler^.HandlerFunc) then
        Handler^.HandlerFunc(vTokens);
      Handler := Handler^.PrevHandler;
    end;
  end;
end;

function TIRCResponseHandlers.IndexOfHandler(vName: String): Integer;
begin
  Result := FHandlers.IndexOf(vName);
end;

procedure TIRCResponseHandlers.RemoveHandler(Response: String);
begin
  DeleteHandler(IndexOfHandler(Response));
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
    Connection.Connect;
    Connection.Start;
    Connected;
  end;
end;

constructor TmnIRCClient.Create;
begin
  inherited Create;
  FCommandClasses := TmnCommandClasses.Create;
  FConnection := TmnIRCConnection.Create(nil, nil);
  FConnection.FIRC := Self;
  FConnection.FreeOnTerminate := False;
  FHost := 'localhost';
  FPort := sDefaultPort;
  FNick := sNickName;
  FAltNick := sOtherNickName;
  FRealName := sRealName;
  FUsername := 'username';
  FPassword := '';
  FState := isDisconnected;
  FTokens := TIRCTokens.Create;
  FHandlers := TIRCResponseHandlers.Create(Self);
  AddHandlers;
end;

destructor TmnIRCClient.Destroy;
begin
  Reset;
  FreeAndNil(FConnection); //+
  FreeAndNil(FTokens);
  FreeAndNil(FHandlers);
  FreeAndNil(FCommandClasses);
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

procedure TmnIRCClient.Send(Channel, Text: String);
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
begin
  Log(lgReceive, vData);
  FTokens.Data := vData;
  FHandlers.Handle(FTokens);
end;

procedure TmnIRCClient.Receive(vChannel, vMsg: String);
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

procedure TmnIRCClient.AddHandlers;
begin
  FHandlers.AddHandler('PING', RplPing);
  FHandlers.AddHandler('NICK', RplNick);
  FHandlers.AddHandler('PRIVMSG', RplPrivMSG);
  FHandlers.AddHandler('WELCOME', RplWelcome1);
  FHandlers.AddHandler('ERR_NICKNAMEINUSE', RplErrNicknameInUse);
  FHandlers.AddHandler('MODE', RplMode);
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

function TmnIRCClient.ExtractNickFromAddress(Address: String): String;
var
  EndOfNick: Integer;
begin
  Result := '';
  EndOfNick := Pos('!', Address);
  if EndOfNick > 0 then
    Result := Copy(Address, 1, EndOfNick - 1);
end;

procedure TmnIRCClient.RplNick(vTokens: TIRCTokens);
begin
  { If it is our nick we are changing, then record the change. }
  if UpperCase(ExtractNickFromAddress(vTokens[0])) = UpperCase(FCurrentNick) then
    FCurrentNick := vTokens[2];
end;

procedure TmnIRCClient.RplPing(vTokens: TIRCTokens);
begin
  { SendDirect the PONG reply to the PING. }
  SendData(Format('PONG %s', [vTokens[2]]));
end;

procedure TmnIRCClient.RplPrivMSG(vTokens: TIRCTokens);
begin
  Receive(vTokens[2], vTokens[3]);
end;

procedure TmnIRCClient.RplWelcome1(vTokens: TIRCTokens);
begin
  { This should be the very first successful response we get, so set the Current
    host and nick from the values returned in the response. }
  FCurrentHost := vTokens[0];
  FCurrentNick := vTokens[2];
  SetState(isReady);
  { If a user mode is pre-set, then SendDirect the mode command. }
  if FUserModes <> [] then
    SendData(Format('MODE %s %s', [FCurrentNick, CreateUserModeCommand(FUserModes)]));
end;

procedure TmnIRCClient.RplErrNicknameInUse(vTokens: TIRCTokens);
begin
  { Handle nick conflicts during the registration process. }
  if FState = isRegistering then
  begin
    if FChangeNickTo = FNick then
      SetNick(FAltNick)
    else
      Quit('Nick conflict');
  end;
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

procedure TmnIRCClient.RplMode(vTokens: TIRCTokens);
var
  Index: Integer;
  ModeString: String;
  AddMode: Boolean;
begin
  { Ignore channel mode changes.  Only interested in user mode changes. }
  if vTokens[2] = FCurrentNick then
  begin
    { Copy the token for efficiency reasons. }
    ModeString := vTokens[3];
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

end.
