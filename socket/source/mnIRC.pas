unit mnIRC;
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
 *  @ported from orignal author Steve Williams
 *  @author by Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, mnSockets, mnClients, mnServers, mnConnections;

const
  sDefaultPort = '6667';
  sNickName = 'unknown';
  sOtherNickName = 'othername';
  sRealName = 'Real Name';
  sRoom = 'test';

  MAX_TOKEN_LENGTH = 512;   //Defined in RFC1459 as the maximum length of a single token. }
  TOKEN_SEPARATOR = ' ';    { Separates tokens, except for the following case. }
  TOKEN_ENDOFTOKENS = ':';  { If the second or higher token starts with this character, it indicates that this token is all characters to the end of the string. }

type
  TmnIRC = class;

  TTokenSyntax = (tsResponse, tsMessage, tsCTCP);

  { TIRCTokens }

  TIRCTokens = class(TObject)
  private
    FTokenString: String;
    FCount: Integer;
    FTokens: TList;
    FSyntax: TTokenSyntax;
    FBuffer: array [0..MAX_TOKEN_LENGTH] of Char;
    procedure SetTokenString(const Value: String);
    function GetTokens(Index: Integer): String;
    function GetTokensFrom(Index: Integer): String;
    procedure SetSyntax(const Value: TTokenSyntax);
  protected
    procedure Tokenize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property TokenString: String read FTokenString write SetTokenString;
    property Tokens[Index: Integer]: String read GetTokens; default;
    property TokensFrom[Index: Integer]: String read GetTokensFrom;
    property Count: Integer read FCount;
    property Syntax: TTokenSyntax read FSyntax write SetSyntax;
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
    FIRC: TmnIRC;
  public
    constructor Create(AIRC: TmnIRC);
    destructor Destroy; override;
    function AddHandler(vName: String; vHandlerFunc: THandlerFunc): Integer;
    procedure DeleteHandler(Index: Integer);
    procedure RemoveHandler(Response: String);
    function IndexOfHandler(vName: String): Integer;
    procedure Handle(vTokens: TIRCTokens);
  end;

  TmnIRCState = (isNotConnected, isResolvingHost, isConnecting, isConnected, isRegistering, isReady, isAborting, isDisconnecting);

  TUserMode = (umInvisible, umOperator, umServerNotices, umWallops);
  TUserModes = set of TUserMode;

  TmnOnData = procedure(const Data: string) of object;

  { TmnIRCConnection }

  TmnIRCConnection = class(TmnClientConnection)
  private
    FIRC: TmnIRC;
  protected
    procedure DoData(const Data: string); virtual;
    procedure DoLog(const Data: string); virtual;
    procedure Process; override;
  public
    procedure Connect; override;
  end;

  TOnResponse = procedure(Sender: TObject; vTokens: TIRCTokens; var Suppress: Boolean) of object;
  TOnReceiveData = procedure(Sender: TObject; vResponse: String) of object;

  TOnReceive = procedure(Sender: TObject; vRoom, vMSG: String) of object;
  TOnSendData = procedure(Sender: TObject; vResponse: String) of object;

  TmnIRC = class(TObject) //TmnClientConnection
  private
    FOnLog: TOnSendData;
    FRealName: String;
    FPort: String;
    FPassword: String;
    FAltNick: String;
    FHost: String;
    FNick: String;
    FChangeNickTo: String;
    FConnection: TmnIRCConnection;
    FState: TmnIRCState;
    FTokens: TIRCTokens;
    FOnBeforeStateChange: TNotifyEvent;
    FOnAfterStateChange: TNotifyEvent;
    FOnResponse: TOnResponse;
    FOnReceiveData: TOnReceiveData;
    FOnReceive: TOnReceive;
    FOnSend: TOnSendData;
    FHandlers: TIRCResponseHandlers;
    FUsername: String;
    FActualNick: String;
    FActualHost: String;
    FUserModes: TUserModes;
    FActualUserModes: TUserModes;
    FOnUserModeChanged: TNotifyEvent;
    procedure SetAltNick(const Value: String);
    procedure SetNick(const Value: String);
    function GetNick: String;
    procedure SetPassword(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetRealName(const Value: String);
    procedure SetHost(const Value: String);
    function GetHost: String;
    procedure SessionConnected;
    procedure SessionClosed;
    procedure DataAvailable(const Data: string);
    procedure SetState(const Value: TmnIRCState);
    procedure Reset;
    procedure SetUsername(const Value: String);
    function GetUserModes: TUserModes;
    procedure SetUserModes(const Value: TUserModes);
    function CreateUserModeCommand(NewModes: TUserModes): String;
  protected
    procedure ProcessResponse(vResponse: String; var Suppress: Boolean); virtual;
    procedure DirectReceive(vResponse: String); virtual;
    procedure Receive(vRoom, vMsg: String); virtual;
    procedure Response(vTokens: TIRCTokens; var Suppress: Boolean); virtual;
    procedure UserModeChanged;
    procedure AddHandlers;
    procedure RplPing(vTokens: TIRCTokens);
    procedure RplPrivMSG(vTokens: TIRCTokens);
    procedure RplNick(vTokens: TIRCTokens);
    procedure RplWelcome1(vTokens: TIRCTokens);
    procedure ErrNicknameInUse(vTokens: TIRCTokens);
    procedure RplMode(vTokens: TIRCTokens);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Close;
    procedure SendDirect(Command: String);

    procedure Log(Message: String);
    procedure Send(Room, Text: String);
    procedure Join(Room: String);
    procedure Notice(Destination, Text: String);
    procedure Quit(Reason: String);
    function ExtractNickFromAddress(Address: String): String;
    property State: TmnIRCState read FState;
    property Connection: TmnIRCConnection read FConnection;
    property Handlers: TIRCResponseHandlers read FHandlers;
  published
    property Host: String read GetHost write SetHost;
    property Port: String read FPort write SetPort;
    property Nick: String read GetNick write SetNick;
    property AltNick: String read FAltNick write SetAltNick;
    property RealName: String read FRealName write SetRealName;
    property Password: String read FPassword write SetPassword;
    property Username: String read FUsername write SetUsername;
    property UserModes: TUserModes read GetUserModes write SetUserModes;
    { Event properties. }
    property OnBeforeStateChange: TNotifyEvent read FOnBeforeStateChange write FOnBeforeStateChange;
    property OnAfterStateChange: TNotifyEvent read FOnAfterStateChange write FOnAfterStateChange;

    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
    property OnReceiveData: TOnReceiveData read FOnReceiveData write FOnReceiveData;
    property OnReceive: TOnReceive read FOnReceive write FOnReceive;
    property OnSendData: TOnSendData read FOnSend write FOnSend;
    property OnLog: TOnSendData read FOnLog write FOnLog;
    property OnUserModeChanged: TNotifyEvent read FOnUserModeChanged write FOnUserModeChanged;
  end;

implementation

uses
  SysUtils;

{ TmnIRCConnection }

procedure TmnIRCConnection.DoData(const Data: string);
begin
  FIRC.DataAvailable(Data);
end;

procedure TmnIRCConnection.DoLog(const Data: string);
begin
  FIRC.Log(Data);
end;

procedure TmnIRCConnection.Process;
var
  Line: string;
begin
  inherited Process;
  if Stream.WaitToRead(Stream.Timeout) then
  begin
    Line := Trim(Stream.ReadLine);
    while line <> '' do
    begin
      DoLog(Line);
      DoData(Line);
      Line := Trim(Stream.ReadLine);
    end;
  end;
end;

procedure TmnIRCConnection.Connect;
begin
  SetStream(TmnClientSocketStream.Create(FIRC.Host, FIRC.Port));
  Stream.Timeout := -1;
  Stream.EndOfLine := #10;
  inherited Connect;
end;

{ TIRCTokens }

constructor TIRCTokens.Create;
begin
  FTokens := TList.Create;
  FSyntax := tsResponse;
end;

destructor TIRCTokens.Destroy;
begin
  if Assigned(FTokens) then
    FTokens.Free;
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
      TokenEnd := StrScan(TokenStart, TOKEN_SEPARATOR);
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

procedure TIRCTokens.SetSyntax(const Value: TTokenSyntax);
begin
  FSyntax := Value;
end;

procedure TIRCTokens.SetTokenString(const Value: String);
begin
  { If the string is a CTCP query, then skip the Ctrl-A characters at the start
    and end of the query. }
  if FSyntax = tsCTCP then
    FTokenString := Copy(Value, 2, Length(Value) - 2)
  else
    FTokenString := Value;
  { Now break it up into its separate tokens. }
  Tokenize;
end;

procedure TIRCTokens.Tokenize;
var
  TokenPtr: PChar;
  EndOfTokens: Boolean;
begin
  FTokens.Clear;
  FCount := 0;
  if Length(FTokenString) > 0 then
  begin
    TokenPtr := PChar(FTokenString);
    { Remove leading spaces. }
    while (TokenPtr^ <> #0) and (TokenPtr^ = TOKEN_SEPARATOR) do
      Inc(TokenPtr);
    { In case we reached the end of the string. }
    if TokenPtr^ = #0 then
      Exit;
    if FSyntax = tsResponse then
    begin
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
        TokenPtr := StrScan(TokenPtr, TOKEN_SEPARATOR);
      end;
      { Remove any redundant separator characters before the token. }
      while (TokenPtr <> nil) and (TokenPtr^ <> #0) and (TokenPtr^ = TOKEN_SEPARATOR) do
        Inc(TokenPtr);
      { Add it to the list if there actually is another token. }
      if TokenPtr <> nil then
      begin
        { Skip the end-of-tokens character if it exists. }
        EndOfTokens := (FSyntax = tsResponse) and (TokenPtr^ = TOKEN_ENDOFTOKENS);
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

constructor TIRCResponseHandlers.Create(AIRC: TmnIRC);
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

{ TmnIRC }

procedure TmnIRC.Close;
begin
  { Try to leave nicely if we can. }
  if FState = isReady then
  begin
    SetState(isDisconnecting);
    SendDirect('QUIT');
  end
  else
  begin
    if Assigned(FConnection) then
    begin
      SetState(isDisconnecting);
      if FConnection.Connected then
        FConnection.Close;
      SessionClosed;
    end;
  end;
end;

procedure TmnIRC.Connect;
begin
  if Assigned(FConnection) and (FState = isNotConnected) then
  begin
    FActualHost := FHost;
    FActualNick := FNick;
    FChangeNickTo := '';
    SetState(isResolvingHost);
    //FSocket.DnsLookup(FHost);//zaher
    Connection.Connect;
    Connection.Start;
    SessionConnected;
  end;
end;

constructor TmnIRC.Create;
begin
  inherited;
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
  FState := isNotConnected;
  FTokens := TIRCTokens.Create;
  FHandlers := TIRCResponseHandlers.Create(Self);
  AddHandlers;
end;

procedure TmnIRC.DataAvailable(const Data: string);
var
  Suppress: Boolean;
begin
  { DirectReceive string and process. }
  if Length(Data) > 0 then
  begin
    { Trigger the OnReceive event. }
    DirectReceive(Data);
    { Now process the response. }
    Suppress := False;
    ProcessResponse(Data, Suppress);
  end;
end;

destructor TmnIRC.Destroy;
begin
  Reset;
  FreeAndNil(FConnection); //+
  FreeAndNil(FTokens);
  FreeAndNil(FHandlers);
  inherited;
end;

procedure TmnIRC.Reset;
begin
  SetState(isAborting);
  if Assigned(FConnection) then
    FConnection.Close;
end;

procedure TmnIRC.SendDirect(Command: String);
begin
  if Assigned(FOnSend) then
    FOnSend(Self, Command);
  if Assigned(FConnection) and (FState in [isRegistering, isReady]) then
    FConnection.Stream.WriteString(Command + #13#10);
end;

procedure TmnIRC.Log(Message: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Message);
end;

procedure TmnIRC.Send(Room, Text: String);
begin
  SendDirect(Format('PRIVMSG %s :%s', [Room, Text]));
end;

procedure TmnIRC.Join(Room: String);
begin
  SendDirect(Format('JOIN %s', [Room]));
end;

procedure TmnIRC.Notice(Destination, Text: String);
begin
  SendDirect(Format('NOTICE %s :%s', [Destination, Text]));
end;

procedure TmnIRC.SessionClosed;
begin
  SetState(isNotConnected);
end;

procedure TmnIRC.SessionConnected;
begin
  SetState(isConnected);
  SetState(isRegistering);
  { If a password exists, SendDirect it first. }
  if FPassword <> '' then
    SendDirect(Format('PASS %s', [FPassword]));
  { SendDirect nick. }
  SetNick(FNick);
  { SendDirect registration. }
  SendDirect(Format('USER %s %s %s :%s', [FUsername, FUsername, FHost, FRealName]));
end;

procedure TmnIRC.SetAltNick(const Value: String);
begin
  FAltNick := Value;
end;

procedure TmnIRC.SetNick(const Value: String);
begin
  if Length(Value) > 0 then
  begin
    if FState in [isRegistering, isReady] then
    begin
      if Value <> FChangeNickTo then
      begin
        SendDirect(Format('NICK %s', [Value]));
        FChangeNickTo := Value;
      end;
    end
    else
    begin
      FNick := Value;
    end;
  end;
end;

procedure TmnIRC.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TmnIRC.SetPort(const Value: String);
begin
  FPort := Value;
end;

procedure TmnIRC.SetRealName(const Value: String);
begin
  FRealName := Value;
end;

procedure TmnIRC.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TmnIRC.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

procedure TmnIRC.SetState(const Value: TmnIRCState);
begin
  if Value <> FState then
  begin
    { Trigger OnBeforeStateChange event. }
    if Assigned(FOnBeforeStateChange) then
      FOnBeforeStateChange(Self);
    { Change the state. }
    FState := Value;
    { Trigger OnAfterStateChange event. }
    if Assigned(FOnAfterStateChange) then
      FOnAfterStateChange(Self);
  end;
end;

function TmnIRC.GetUserModes: TUserModes;
begin
  if FState in [isRegistering, isReady] then
    Result := FActualUserModes
  else
    Result := FUserModes;
end;

procedure TmnIRC.SetUserModes(const Value: TUserModes);
var
  ModeString: String;
begin
  if FState in [isRegistering, isReady] then
  begin
    ModeString := CreateUserModeCommand(Value);
    if Length(ModeString) > 0 then
      SendDirect(Format('MODE %s %s', [FActualNick, ModeString]));
  end
  else
  begin
    FUserModes := Value;
  end;
end;

procedure TmnIRC.ProcessResponse(vResponse: String; var Suppress: Boolean);
begin
  FTokens.Syntax := tsResponse;
  FTokens.TokenString := vResponse;
  FHandlers.Handle(FTokens);
  { Trigger OnResponse event. }
  Suppress := False;
  Response(FTokens, Suppress);
end;

procedure TmnIRC.DirectReceive(vResponse: String);
begin
  if Assigned(FOnReceiveData) then
    FOnReceiveData(Self, vResponse);
end;

procedure TmnIRC.Receive(vRoom, vMsg: String);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self, vRoom, vMsg);
end;

procedure TmnIRC.Response(vTokens: TIRCTokens; var Suppress: Boolean);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, vTokens, Suppress);
end;

procedure TmnIRC.UserModeChanged;
begin
  if Assigned(FOnUserModeChanged) then
    FOnUserModeChanged(Self);
end;

procedure TmnIRC.Quit(Reason: String);
begin
  SendDirect(Format('QUIT :%s', [Reason]));
end;

procedure TmnIRC.AddHandlers;
begin
  FHandlers.AddHandler('PING', RplPing);
  FHandlers.AddHandler('NICK', RplNick);
  FHandlers.AddHandler('PRIVMSG', RplPrivMSG);
  FHandlers.AddHandler('WELCOME', RplWelcome1);
  FHandlers.AddHandler('ERR_NICKNAMEINUSE', ErrNicknameInUse);
  FHandlers.AddHandler('MODE', RplMode);
end;

function TmnIRC.GetHost: String;
begin
  if FState in [isRegistering, isReady] then
    Result := FActualHost
  else
    Result := FHost;
end;

function TmnIRC.GetNick: String;
begin
  if FState in [isRegistering, isReady] then
    Result := FActualNick
  else
    Result := FNick;
end;

function TmnIRC.ExtractNickFromAddress(Address: String): String;
var
  EndOfNick: Integer;
begin
  Result := '';
  EndOfNick := Pos('!', Address);
  if EndOfNick > 0 then
    Result := Copy(Address, 1, EndOfNick - 1);
end;

procedure TmnIRC.RplNick(vTokens: TIRCTokens);
begin
  { If it is our nick we are changing, then record the change. }
  if UpperCase(ExtractNickFromAddress(vTokens[0])) = UpperCase(FActualNick) then
    FActualNick := vTokens[2];
end;

procedure TmnIRC.RplPing(vTokens: TIRCTokens);
begin
  { SendDirect the PONG reply to the PING. }
  SendDirect(Format('PONG %s', [vTokens[2]]));
end;

procedure TmnIRC.RplPrivMSG(vTokens: TIRCTokens);
begin
  Receive(vTokens[2], vTokens[3]);
end;

procedure TmnIRC.RplWelcome1(vTokens: TIRCTokens);
begin
  { This should be the very first successful response we get, so set the actual
    host and nick from the values returned in the response. }
  FActualHost := vTokens[0];
  FActualNick := vTokens[2];
  SetState(isReady);
  { If a user mode is pre-set, then SendDirect the mode command. }
  if FUserModes <> [] then
    SendDirect(Format('MODE %s %s', [FActualNick, CreateUserModeCommand(FUserModes)]));
end;

procedure TmnIRC.ErrNicknameInUse(vTokens: TIRCTokens);
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
function TmnIRC.CreateUserModeCommand(NewModes: TUserModes): String;
const
  ModeChars: array [umInvisible..umWallops] of Char = ('i', 'o', 's', 'w');
var
  ModeDiff: TUserModes;
  Mode: TUserMode;
begin
  Result := '';
  { Calculate user modes to remove. }
  ModeDiff := FActualUserModes - NewModes;
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
  ModeDiff := NewModes - FActualUserModes;
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

procedure TmnIRC.RplMode(vTokens: TIRCTokens);
var
  Index: Integer;
  ModeString: String;
  AddMode: Boolean;
begin
  { Ignore channel mode changes.  Only interested in user mode changes. }
  if vTokens[2] = FActualNick then
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
            FActualUserModes := FActualUserModes + [umInvisible]
          else
            FActualUserModes := FActualUserModes - [umInvisible];
        'o':
          if AddMode then
            FActualUserModes := FActualUserModes + [umOperator]
          else
            FActualUserModes := FActualUserModes - [umOperator];
        's':
          if AddMode then
            FActualUserModes := FActualUserModes + [umServerNotices]
          else
            FActualUserModes := FActualUserModes - [umServerNotices];
        'w':
          if AddMode then
            FActualUserModes := FActualUserModes + [umWallops]
          else
            FActualUserModes := FActualUserModes - [umWallops];
      end;
    end;
    UserModeChanged;
  end;
end;

end.
