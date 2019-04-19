unit zktClients;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   MIT (modified of https://opensource.org/licenses/MIT)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *   Based on multiple php, py library, thanks for all
 *
 *}
{
  https://github.com/adrobinoga/zk-protocol/blob/master/protocol.md
  https://www.developpez.net/forums/d1588609/environnements-developpement/delphi/composants-vcl/composant-non-visuel-pullsdk-ZKeko/
  https://www.board4all.biz/threads/question-how-to-control-board-inbio-160-with-delphi.624395/
  https://github.com/RK-Rohan/ZK2OB/tree/master/zklib
  https://searchcode.com/file/47825527/zkem/zkem.py

  Test on ZKTeco 350 "Ver 6.60 Sep 23 2015"
}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 OFF : Parameter "$1" not used}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils,
  mnClasses, mnClients, mnSockets, mnSocketStreams;

const
  CMD_CONNECT = 1000;
  CMD_EXIT = 1001;
  CMD_ENABLEDEVICE = 1002;
  CMD_DISABLEDEVICE = 1003;
  CMD_RESTART = 1004;
  CMD_POWEROFF = 1005;
  CMD_SLEEP = 1006;
  CMD_RESUME = 1007;
  CMD_TEST_TEMP = 1011;
  CMD_TESTVOICE = 1017;
  CMD_VERSION = 1100;
  CMD_CHANGE_SPEED = 1101;

  CMD_PREPARE_DATA = 1500;
  CMD_DATA = 1501;
  CMD_FREE_DATA	=	1502;	{ Free the transfered data }
  CMD_QUERY_DATA = 1503; //CMD_DATA_WRRQ
  CMD_READ_DATA	= 1504; //CMD_DATA_RDY

  CMD_UPDATEFILE = 1700;
  CMD_READFILE = 1702;

  CMD_CHECKUDISKUPDATEPACKPAGE =1709;
  CMD_OPTIONS_DECIPHERING	=1710;
  CMD_OPTIONS_ENCRYPT	= 1711;

  CMD_ACK_OK = 2000;
  CMD_ACK_ERROR = 2001;
  CMD_ACK_DATA = 2002;
  CMD_ACK_UNKNOWN	= $FFFF;	{ Return Unknown Command }

  CMD_GET_PHOTO_COUNT = 2013;
  CMD_GET_PHOTO_BYNAME = 2014;
  CMD_CLEAR_PHOTO_BY_TIME =	2015;
  CMD_GET_PHOTONAMES_BY_TIME = 2016;

  CMD_USER_WRQ = 8;
  CMD_USERTEMP_RRQ = 9;
  CMD_USERTEMP_WRQ = 10;
  CMD_OPTIONS_RRQ = 11;
  CMD_OPTIONS_WRQ = 12;
  CMD_ATTLOG_RRQ = 13;
  CMD_CLEAR_DATA = 14;
  CMD_CLEAR_ATTLOG = 15;
  CMD_DELETE_USER = 18;
  CMD_DELETE_USERTEMP = 19;
  CMD_CLEAR_ADMIN = 20;
  CMD_ENABLE_CLOCK = 57;
  CMD_STARTVERIFY = 60;
  CMD_STARTENROLL = 61;
  CMD_CANCELCAPTURE = 62;
  CMD_STATE_RRQ = 64;
  CMD_WRITE_LCD = 66;
  CMD_CLEAR_LCD = 67;

  CMD_GET_TIME = 201;
  CMD_SET_TIME = 202;

  LEVEL_USER = 0;          // 0000 0000
  LEVEL_ENROLLER = 2;       // 0000 0010
  LEVEL_MANAGER = 12;      // 0000 1100
  LEVEL_SUPERMANAGER = 14; // 0000 1110

  {
  verification_lookup =
      0:  "Check In (Code)",
      8:  "Check In (Fingerprint)",
      32: "Check Out (Code)",
      40: "Check Out (Fingerprint)",
  }

type
  TZKHeader = packed record
    Commnad: WORD;
    CheckSum: WORD;
    SessionID: WORD; //or some data on respond
    ReplayID: WORD;
  end;

  PZKHeader = ^TZKHeader;

  { TZKPayload }

  TZKPayload = packed record
    Start: DWORD; //5050827d;
    Size: DWORD;
    Header: TZKHeader;
    function DataSize: Integer;
  end;

  PZKPayload = ^TZKPayload;

{  TZKRespond = record
    TZKPayload
    Data: Bytes;
  end;}

  TZKAttData = packed record
    Number: WORD;
    UserID: array[0..23] of char;
    Verified: Byte;
    Time: DWORD;
    State: Byte;
    WorkCode: Byte;
    Reserved: array[0..6] of char;
  end;

  TZKUserData = packed record
    ID: WORD;
    Role: Byte;
    Password: array[0..7] of char;
    Name: array[0..27] of char;
    Card: array[0..4] of char; //NotSure
    Group: Word; //NotSure
    TimeZone: Word; //NotSure
    UserID: array[0..23] of char;
  end;

  TZKAttendance = class(TObject)
  public
    Number: Integer;
    UserID: string;
    Time: TDateTime;
    State: Integer;
    WorkCode: Integer;
    Verified: Integer;
  end;

  TZKAttendances = class(TmnObjectList<TZKAttendance>)
  public
  end;

  TZKUser = class(TObject)
  public
    Number: Integer;
    UserID: string;
    Name: string;
    Password: string;
    Card: string;
    Role: Integer;
    Group: Integer;
    TimeZone: Integer;
  end;

  TZKUsers = class(TmnObjectList<TZKUser>)
  public
  end;

  TZKDataSize = record
    Size: DWORD;
    Reserved: DWORD;
  end;

  { TByteHelper }

  TByteHelper = record helper for TBytes
  public
    procedure Clear;
    procedure Add(Value: Byte); overload;
    procedure Add(Value: Word); overload;
    procedure Add(Value: AnsiString); overload;
    procedure Add(Value: TBytes); overload;
    procedure Add(Value: TZKPayload); overload;
    //Index is byte offset
    function GetWord(Index: Integer): WORD; overload;
    function Count: Integer;
    function ToString: string;
    procedure Dump;
    procedure DumpHex(BytesInLine: Integer = 0);
  end;

  TZKSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  { TZKClient }

  TZKClient = class(TObject)
  private
    FHost: string;
    FPort: string;
  protected
    FSocket: TZKSocketStream;
    FReceivedData: string;
    FStartData: Integer;
    FUserData: TBytes;
    FAttendanceData: TBytes;
    FReplyId: Integer;
    FSessionId: Integer;
    procedure Send(Buf: TBytes);
    function Recv: TBytes;
    function NewReplayID: Integer;
    function CreateSocket: TZKSocketStream; virtual;
    function CheckSum(Buf: TBytes; Offset: Integer = 0): Word;
    function CreatePacket(Command, SessionId, ReplyId: Word; CommandData: AnsiString): TBytes;
    function ExecCommand(Command, ReplyId: Word; CommandData: string; out Payload: TZKPayload; out RespondData: TBytes): Boolean; virtual; overload;
    function ExecCommand(Command, ReplyId: Word; CommandData: string; out RespondData: TBytes): Boolean; overload;
    function ExecCommand(Command, ReplyId: Word; CommandData: string): Boolean; overload;
    function ExecCommand(Command, ReplyId: Word): Boolean; overload;
    function ReceiveData(out Payload: TZKPayload; out Data: TBytes): Boolean; deprecated;
    function ReceiveBuffer(var Buffer; Size: Integer): Boolean;
    function ReceiveBytes(Size: Integer; out Data: TBytes): Boolean;
    function ReceiveHeader(out Payload: TZKPayload): Boolean;
  public
    constructor Create(vHost: string; vPort: string = '4370'); virtual;
    function Connect: Boolean;
    procedure Disconnect;
    function GetVersion: string;
    procedure EnableDevice;
    procedure DisableDevice;
    procedure TestVoice;
    function GetAddendances(Attendances: TZKAttendances): Boolean;
    function GetUsers(Users: TZKUsers): Boolean;
    property Host: string read FHost;
    property Port: string read FPort;
  end;

implementation

{ TZKPayload }

function TZKPayload.DataSize: Integer;
begin
  Result := Self.Size - SizeOf(Self.Header);
end;

{**

PC->Device

Reply ID is a flag to identify the replayed data from device. So while send data
to device, let ReplyID++ to keep every cmd have a different id.

|--------|--------|--------|--------|--------|--------|--------|--------|---
|       CMD       |    Check Sum    |    Session ID   |    Reply ID     |Data

Device->PC

|--------|--------|--------|--------|--------|--------|--------|--------|---
|       CMD       |    Check Sum    |      Data1      |     Reply ID    |Data2

*}

{ TByteHelper }

procedure TByteHelper.Clear;
begin
  SetLength(Self, 0);
end;

procedure TByteHelper.Add(Value: Byte);
var
  index: integer;
begin
  index := Length(Self);
  SetLength(Self, Index + SizeOf(Value));
  Self[index] := Value;
end;

procedure TByteHelper.Add(Value: Word);
var
  index: integer;
begin
  index := Length(Self);
  SetLength(Self, Index + SizeOf(Value));
  Self[index] := lo(Value);
  inc(index);
  Self[index] := hi(Value);
end;

procedure TByteHelper.Add(Value: AnsiString);
var
  i, index: integer;
begin
  index := Length(Self);
  SetLength(Self, Index + Length(Value));
  for i := 1 to Length(Value) do
  begin
    Self[index] := Byte(Value[i]);
    Inc(Index);
  end;
end;

procedure TByteHelper.Add(Value: TBytes);
var
  i, index: integer;
begin
  index := Length(Self);
  SetLength(Self, Index + Length(Value));
  for i := 0 to Length(Value) -1 do
  begin
    Self[index] := Value[i];
    Inc(Index);
  end;
end;

procedure TByteHelper.Add(Value: TZKPayload);
var
  index: integer;
begin
  index := Length(Self);
  SetLength(Self, Index + SizeOf(Value));
  PZKPayload(@Self[index])^ := Value;
end;

function TByteHelper.GetWord(Index: Integer): WORD;
begin
  Result := Self[Index + 1] shl 8 or Self[Index];
end;

function TByteHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TByteHelper.ToString: string;
begin
  Result := PChar(Self);
end;

procedure TByteHelper.Dump;
var
  i: Integer;
begin
  for i := 0  to Count -1 do
  begin
    if i > 0 then
      Write(',');
    Write(IntToStr(Self[i]));
  end;
end;

procedure TByteHelper.DumpHex(BytesInLine: Integer);
var
  i: Integer;
  c: Integer;
begin
  c := 0;
  for i := 0  to Count -1 do
  begin
    Write(IntToHex(Self[i], 2));
    Inc(c);
    if (BytesInLine > 0) and (c >= (BytesInLine)) then
    begin
      c := 0;
      WriteLn();
    end;
  end;
  WriteLn();
end;

{ TZKClient }

function TZKClient.CheckSum(Buf: TBytes; Offset: Integer): Word;
var
  i, c: Integer;
  w: word;
  Sum: Word;
begin
  Sum := 0;
  i := Offset;
  c := Buf.Count;
  while i < Buf.Count - 1 do //yes <= not <
  begin
    w := Buf[i + 1] shl 8 or Buf[i];
    Sum := Sum + w;
    i := i + 2;
    c := c  - 2;
  end;
  if c > 0 then //it is 1 btw
    Sum := Sum + Buf[Buf.Count - 1];
  Sum := not Sum;
  Result := Sum;
end;

function TZKClient.CreatePacket(Command, SessionId, ReplyId: Word; CommandData: AnsiString): TBytes;
var
  c: Integer;
  Packet: PZKPayload;
begin
  SetLength(Result, SizeOf(TZKPayload));
  Result.Add(CommandData);

  Packet := @Result[0];

  Packet.Start := $7d825050;
  Packet.Size := Length(Result) - (SizeOf(Packet.Start) + SizeOf(Packet.Size));

  Packet.Header.Commnad := Command;
  Packet.Header.CheckSum := 0;
  Packet.Header.ReplayID := ReplyId;
  Packet.Header.SessionID := SessionId;
  c := CheckSum(Result, SizeOf(Packet.Start) + SizeOf(Packet.Size));
  Packet.Header.CheckSum := c;
end;

function TZKClient.ExecCommand(Command, ReplyId: Word; CommandData: string; out Payload: TZKPayload; out RespondData: TBytes): Boolean;
var
  Packet: TBytes;
  CMD: Integer;
  //SizeInfo: TZKDataSize;
  aSize: DWord;
begin
  RespondData := nil;
  aSize := 0;
  Packet := CreatePacket(Command, FSessionId, ReplyId, CommandData);
  Send(Packet);
  Result := ReceiveHeader(Payload);
  if Result then
  begin
    Result := ReplyId = Payload.Header.ReplayID;
    if Result then
    begin
      CMD := Payload.Header.Commnad;
      case CMD of
        CMD_ACK_OK:
        begin
          if (FSessionId = 0) then
            FSessionId := Payload.Header.SessionID;
          ReceiveBytes(Payload.DataSize, RespondData);
        end;
        CMD_PREPARE_DATA:
        begin
          ReceiveBytes(Payload.DataSize, RespondData);
          ReceiveHeader(Payload);
          Result := Payload.Header.Commnad = CMD_DATA;
          if Result then
          begin
            ReceiveBuffer(aSize, SizeOf(aSize));
            ReceiveBytes(aSize, RespondData);
            ReceiveHeader(Payload);
            Result := (Payload.Header.ReplayID = ReplyId) and (Payload.Header.Commnad = CMD_ACK_OK);
          end;
        end;
      end;
    end;
  end;
end;

function TZKClient.ExecCommand(Command, ReplyId: Word; CommandData: string; out RespondData: TBytes): Boolean;
var
  Payload: TZKPayload;
begin
  Result := ExecCommand(Command, ReplyId, CommandData, Payload, RespondData);
end;

function TZKClient.ExecCommand(Command, ReplyId: Word; CommandData: string): Boolean;
var
  Payload: TZKPayload;
  RespondData: TBytes;
begin
  Result := ExecCommand(Command, ReplyId, CommandData, Payload, RespondData);
end;

function TZKClient.ExecCommand(Command, ReplyId: Word): Boolean;
var
  CommandData: string;
begin
  CommandData := '';
  Result := ExecCommand(Command, ReplyId, CommandData);
end;

function TZKClient.ReceiveBuffer(var Buffer; Size: Integer): Boolean;
var
  c: Integer;
begin
  c := FSocket.Read(Buffer, Size);
  Result := c > 0;
end;

function TZKClient.NewReplayID: Integer;
begin
  Inc(FReplyId);
  Result := FReplyId;
end;

function TZKClient.CreateSocket: TZKSocketStream;
begin
  Result := TZKSocketStream.Create(Host, Port, [soSetReadTimeout, soConnectTimeout, soKeepIfReadTimeout]);
  Result.Timeout := 5000;
end;

constructor TZKClient.Create(vHost: string; vPort: string);
begin
  inherited Create;
  FHost := vHost;
  FPort := vPort;
  FStartData := 8;
end;

procedure TZKClient.Send(Buf: TBytes);
begin
  FSocket.WriteBytes(Buf);
end;

function TZKClient.Recv: TBytes;
begin
  Result := FSocket.ReadBytes(4 + 8 + 1024); //Start, Header, Data buffer
end;

function TZKClient.ReceiveData(out Payload: TZKPayload; out Data: TBytes): Boolean;
var
  Respond: TBytes;
  P: PZKPayload;
begin
  Respond := Recv;
  Result := Respond.Count > 0;
  if Result then
  begin
    P := Pointer(Respond);
    Payload := P^;
    Data := Copy(Respond, SizeOf(TZKPayload), MaxInt);
  end
  else
  begin
    Finalize(Payload);
    Data := nil
  end
end;

function TZKClient.ReceiveBytes(Size: Integer; out Data: TBytes): Boolean;
var
  c: Integer;
begin
  if Size > 0 then
  begin
    SetLength(Data, Size);
    c := FSocket.Read(Data[0], Size);
    Result := c > 0;
  end
  else
    Result := True;
end;

function TZKClient.ReceiveHeader(out Payload: TZKPayload): Boolean;
var
  c: Integer;
begin
  Finalize(Payload);
  c := FSocket.Read(Payload, SizeOf(Payload));
  Result := c > 0;
end;

function TZKClient.Connect: Boolean;
begin
  FSessionId := 0;
  FReplyId := 0;
  if FSocket = nil then
    FSocket := CreateSocket;

  FSocket.Connect;

  Result := FSocket.Connected;
  if Result then
  begin
    Result := ExecCommand(CMD_CONNECT, NewReplayID);
  end;
end;

procedure TZKClient.Disconnect;
var
  Data: TBytes;
begin
  ExecCommand(CMD_EXIT, NewReplayID, '', Data);
  FSocket.Disconnect;
  FreeAndNil(FSocket);
  FSessionId := 0;
  FReplyId := 0;
end;

function TZKClient.GetVersion: string;
var
  Data: TBytes;
begin
  if ExecCommand(CMD_VERSION, NewReplayID, '', Data) then
    Result := Data.ToString
  else
    Result := '';
end;

function DecodeTime(ATime: DWORD): TDateTime;
var
  t: TSystemTime;
begin
    t.Second := ATime mod 60;
    ATime := ATime div 60;
    t.Minute := ATime mod 60;
    ATime := ATime div 60;
    t.Hour := ATime mod 24;
    ATime := ATime div 24;
    t.Day := ATime mod 31 + 1;
    ATime := ATime div 31;
    t.Month := ATime mod 12+1;
    ATime := ATime div 12;
    t.Year := ATime + 2000;
    Result := SystemTimeToDateTime(t);
end;

procedure TZKClient.DisableDevice;
begin
  ExecCommand(CMD_DISABLEDEVICE, NewReplayID, #0#0);
end;

procedure TZKClient.EnableDevice;
begin
  ExecCommand(CMD_ENABLEDEVICE, NewReplayID, #0#0);
end;

procedure TZKClient.TestVoice;
begin
  ExecCommand(CMD_TESTVOICE, NewReplayID, #0#0);
end;

function TZKClient.GetAddendances(Attendances: TZKAttendances): Boolean;
var
  Data: TBytes;
  Payload: TZKPayload;
  PAtt: ^TZKAttData;
  aAttendance: TZKAttendance;
  i: Integer;
begin
  Result := ExecCommand(CMD_ATTLOG_RRQ, NewReplayID, '', Payload, Data);
  if Result then
  begin
    //Data.DumpHex(40);
    PAtt := Pointer(Data);
    WriteLn(SizeOf(TZKAttData));
    i := Data.Count div SizeOf(TZKAttData);
    while i > 0 do
    begin
      aAttendance := TZKAttendance.Create;
      aAttendance.Number:= PAtt.Number;
      aAttendance.UserID := PAtt.UserID;
      aAttendance.Time := DecodeTime(PAtt.Time);
      aAttendance.State := PAtt.State;
      aAttendance.WorkCode := PAtt.WorkCode;
      aAttendance.Verified := PAtt.Verified;
      Attendances.Add(aAttendance);
      Inc(PAtt);
      Dec(i);
    end;
  end;
end;

function TZKClient.GetUsers(Users: TZKUsers): Boolean;
var
  Data: TBytes;
  Payload: TZKPayload;
  PUser: ^TZKUserData;
  aUser: TZKUser;
  i: Integer;
begin
  Result := ExecCommand(CMD_USERTEMP_RRQ, NewReplayID, '', Payload, Data);
  if Result then
  begin
    //Data.DumpHex(72);
    PUser := Pointer(Data);
    Writeln(SizeOf(TZKUserData));
    i := Data.Count div SizeOf(TZKUserData);
    while i > 0 do
    begin
      aUser := TZKUser.Create;
      aUser.Number := PUser.ID;
      aUser.Name := PUser.Name;
      aUser.Password := PUser.Password;
      aUser.Role := PUser.Role;
      aUser.UserID := PUser.UserID;
      aUser.Card := PUser.Card;
      aUser.Group := PUser.Group;
      aUser.TimeZone := PUser.TimeZone;
      Users.Add(aUser);
      Inc(PUser);
      Dec(i);
    end;
  end;
end;

end.

{
AttLog
CMD_ATTLOG_RRQ
40
68150000
3E00310000000000000000000000000000000000000000000000014A2DF024FF0000000000000000
3F0032000000000000000000000000000000000000000000000001C12DF024FF0000000000000000
4000320000000000000000000000000000000000000000000000012E2EF024FF0000000000000000
410032000000000000000000000000000000000000000000000001BF2EF024FF0000000000000000
420033000000000000000000000000000000000000000000000001C52EF024FF0000000000000000
4300310000000000000000000000000000000000000000000000014E2FF024FF0000000000000000

Users
CMD_USERTEMP_RRQ
72
0100 00 31323300 00000000 5A616865720000000000000000000000000000000000000000000000 0100 00 010000000000 310000000000000000000000000000000000000000000000
02000000323300000000004F6D6172000000000000000000000000000000000000000000000000010000010000000000320000000000000000000000000000000000000000000000
030000003233000000000041796100000000000000000000000000000000000000000000000000010000010000000000330000000000000000000000000000000000000000000000
04000000323300000000004C696E61000000000000000000000000000000000000000000000000010000010000000000340000000000000000000000000000000000000000000000
050000343536000000000048756461000000000000000000000000000000000000000000000000010000010000000000350000000000000000000000000000000000000000000000
}
