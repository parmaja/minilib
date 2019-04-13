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
  https://www.developpez.net/forums/d1588609/environnements-developpement/delphi/composants-vcl/composant-non-visuel-pullsdk-zkteko/
  https://www.board4all.biz/threads/question-how-to-control-board-inbio-160-with-delphi.624395/
}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils,
  mnClients, mnSockets;

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

  CMD_ACK_OK = 2000;
  CMD_ACK_ERROR = 2001;
  CMD_ACK_DATA = 2002;
  CMD_PREPARE_DATA = 1500;
  CMD_DATA = 1501;

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

  USHRT_MAX = 65535;

  LEVEL_USER = 0;          // 0000 0000
  LEVEL_ENROLLER = 2;       // 0000 0010
  LEVEL_MANAGER = 12;      // 0000 1100
  LEVEL_SUPERMANAGER = 14; // 0000 1110

type

  { TByteHelper }

  TByteHelper = record helper for TBytes
  public
    procedure Add(Value: Byte); overload;
    procedure Add(Value: Word); overload;
    procedure Add(Value: AnsiString); overload;
    procedure Add(Value: TBytes); overload;
    function Count: Integer;
    procedure Dump;
    procedure DumpHex;
  end;

  TZKTSocketStream = Class(TmnClientSocketStream)
  protected
  end;

  { TZKTClient }

  TZKTClient = class(TObject)
  private
    FHost: string;
    FPort: string;
  protected
    FSocket: TZKTSocketStream;
    FReceivedData: string;
    FStartData: Integer;
    FUserData: TBytes;
    FAttendanceData: TBytes;
    FCurrentReplyId: Integer;
    FSessionId: Integer;
    function GetReplayID: Integer;
    function CreateSocket: TZKTSocketStream; virtual;
    function CheckSum(Buf: TBytes): Word;
    function CreateHeader(command, chksum, session_id, reply_id: Word; command_string: AnsiString): TBytes;
    function ExecCommand(command: Word; CommandString: string = ''; OffsetData: Integer = 8): Boolean; virtual;
  public
    constructor Create(vHost: string; vPort: string = '4370'); virtual;
    function Connect: Boolean;
    property Host: string read FHost;
    property Port: string read FPort;
    procedure Send(Buf: TBytes);
    function Recv: TBytes;
  end;

implementation

{**

PC->Device

Reply ID is a flag to identify the replayed data from device. So while send data
to device, let ReplyID++ to keep every cmd have a different id.

|--------|--------|--------|--------|--------|--------|--------|--------|---
|       CMD       |    Check Sum    |    Session ID   |    Reply ID     |Data

Device->PC

|--------|--------|--------|--------|--------|--------|--------|--------|---
|       CMD       |    Check Sum    |      Data1      |     Reply ID    |Data2

CMD=CMD_ACK_OK,CMD_ACK_ERROR,CMD_DATA,CMD_UNKNOWN

*}

{ TByteHelper }

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

function TByteHelper.Count: Integer;
begin
  Result := Length(Self);
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

procedure TByteHelper.DumpHex;
var
  i: Integer;
begin
  for i := 0  to Count -1 do
  begin
    Write(IntToHex(Self[i], 2));
  end;
end;

{ TZKTClient }

function TZKTClient.CheckSum(Buf: TBytes): Word;
var
  i, c: Integer;
  w: word;
  Sum: Integer;
begin
  Sum := 0;
  i := 0;
  c := Buf.Count;
  while i < Buf.Count - 1 do //yes <= not < we
  begin
    w := Buf[i + 1] shl 8 or Buf[i];
    Sum := Sum + w;
    if Sum > USHRT_MAX then
        Sum := Sum - USHRT_MAX;
    i := i + 2;
    c := c  - 2;
  end;

  if c > 0 then //it is 1 btw
    Sum := Sum + Buf[Buf.Count - 1];

  while Sum > USHRT_MAX do
      Sum := Sum - USHRT_MAX;

  Sum := not Sum;

  while Sum < 0 do
      Sum := Sum + USHRT_MAX;

  Result := Sum;
end;

function TZKTClient.CreateHeader(command, chksum, session_id, reply_id: Word; command_string: AnsiString): TBytes;
var
  c: Integer;
begin
  Result.Add(command);
  Result.Add(chksum);
  Result.Add(session_id);
  Result.Add(reply_id);
  Result.Add(command_string);
  c := CheckSum(Result);
  Writeln('CheckSum: ' + IntToStr(c));
end;

function TZKTClient.ExecCommand(command: Word; CommandString: string; OffsetData: Integer): Boolean;
var
  chksum: Word;
begin
  OffsetData := OffsetData + FStartData;
  //session_id := SessionId;
end;

function TZKTClient.GetReplayID: Integer;
begin
  Inc(FCurrentReplyId);
  Result := FCurrentReplyId;
end;

function TZKTClient.CreateSocket: TZKTSocketStream;
begin
  Result := TZKTSocketStream.Create(Host, Port, [soNoDelay, soSafeConnect, soKeepIfReadTimout, soSetReadTimeout, soConnectTimeout]);
  Result.Timeout := 1000;
  //Result.Timeout := WaitForEver;
  Result.EndOfLine := #10;
end;

constructor TZKTClient.Create(vHost: string; vPort: string);
begin
  inherited Create;
//timeout_sec := 5;
//timeout_usec := 500000;
  FHost := vHost;
  FPort := vPort;
  FStartData := 8;
end;

procedure TZKTClient.Send(Buf: TBytes);
begin
  FSocket.WriteBytes(Buf);
end;

function TZKTClient.Recv: TBytes;
begin
  Result := FSocket.ReadBytes(1024);
end;

function TZKTClient.Connect: Boolean;
var
  command: Integer;
  command_string: string;
  chksum: Integer;
  session_id: Integer;
  reply_id: integer;
  Buf, Respond: TBytes;
begin
  command := CMD_CONNECT;
  command_string := '';
  chksum := 0;
  session_id := 0;
  reply_id := -1 + USHRT_MAX;
  Buf := CreateHeader(command, chksum, session_id, reply_id, command_string);

  if FSocket = nil then
    FSocket := CreateSocket;
  FSocket.Connect;
  Send(Buf);
  Respond := Recv;
  Respond.DumpHex;
  //FSocket.ReadBytes()
  //reply_id := GetReplayID;
//  Recv(received_data);
end;

end.

