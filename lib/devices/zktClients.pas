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
  https://www.developpez.net/forums/d1588609/environnements-developpement/delphi/composants-vcl/composant-non-visuel-pullsdk-zkteko/
  https://www.board4all.biz/threads/question-how-to-control-board-inbio-160-with-delphi.624395/
  https://github.com/RK-Rohan/ZK2OB/tree/master/zklib
  https://searchcode.com/file/47825527/zkem/zkem.py
}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils,
  mnClients, mnSockets, mnSocketStreams;

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
  CMD_QUERY_DATA = 1503;
  CMD_READ_DATA	= 1504;

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

  USHRT_MAX = 65535;

  LEVEL_USER = 0;          // 0000 0000
  LEVEL_ENROLLER = 2;       // 0000 0010
  LEVEL_MANAGER = 12;      // 0000 1100
  LEVEL_SUPERMANAGER = 14; // 0000 1110

  OFFSET_HEADER = 8;
  OFFSET_DATA = 8;


  INDEX_CMD = 0;
  INDEX_CHECKSUM = 2;
  INDEX_SESSION_ID = 4;
  INDEX_REPLY_ID = 6;

type
  TZKTHeader = packed record
    Commnad: WORD;
    CheckSum: WORD;
    SessionID: WORD;
    ReplayID: WORD;
  end;

  TZKTPacket = packed record
    Start: DWORD; //5050827d;
    Size: WORD;
    Reserved: WORD; //Zero
    Header: TZKTHeader;
    Data: array[0..1] of WORD;
  end;

  { TByteHelper }

  TByteHelper = record helper for TBytes
  public
    procedure Clear;
    procedure Add(Value: Byte); overload;
    procedure Add(Value: Word); overload;
    procedure Add(Value: DWord); overload;
    procedure Add(Value: AnsiString); overload;
    procedure Add(Value: TBytes); overload;
    //Index is byte offset
    function GetWord(Index: Integer): WORD; overload;
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
    function CreateHeader(Command, session_id, ReplyId: Word; CommandData: AnsiString): TBytes;
    function ExecCommand(Command, ReplyId: Word; CommandData: string; out RespondData: TBytes): Boolean; virtual;
  public
    constructor Create(vHost: string; vPort: string = '4370'); virtual;
    function Connect: Boolean;
    procedure Disconnect;
    function GetVersion: string;
    function GetSizeAddendance(out ASize: Integer): Boolean;
    function GetAddendance(out Strings: TStringList): Boolean;
    procedure DisableDevice;
    procedure EnableDevice;
    procedure TestVoice;
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

procedure TByteHelper.Add(Value: DWord);
begin
  Add(lo(Value));
  Add(hi(Value));
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

function TByteHelper.GetWord(Index: Integer): WORD;
begin
  Result := Self[Index + 1] shl 8 or Self[Index];
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
  WriteLn();
end;

{ TZKTClient }

function TZKTClient.CheckSum(Buf: TBytes): Word;
var
  i, c: Integer;
  w: word;
  Sum: Word;
begin
  Sum := 0;
  i := 0;
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

function TZKTClient.CreateHeader(Command, session_id, ReplyId: Word; CommandData: AnsiString): TBytes;
var
  c: Integer;
  l: DWord;
begin
  SetLength(Result, 0);
  Result.Add(command);
  Result.Add(Word(0));
  Result.Add(session_id);
  Result.Add(ReplyId);
  Result.Add(CommandData);
  c := CheckSum(Result);
  l := Result.Count;

  Result.Clear;
  Result.Add(Word(20560));
  Result.Add(Word(32130));
  Result.Add(l);
  Result.Add(command);
  Result.Add(Word(c));
  Result.Add(session_id);
  Result.Add(ReplyId);
  Result.Add(CommandData);

  Writeln('CheckSum: ' + IntToStr(c));
  Result.DumpHex;
end;

function TZKTClient.ExecCommand(Command, ReplyId: Word; CommandData: string; out RespondData: TBytes): Boolean;
var
  reply_id: integer;
  Buf, Respond: TBytes;
begin
  Buf := CreateHeader(Command, FSessionId, ReplyId, CommandData);

  Send(Buf);
  Respond := Recv;
  Respond.DumpHex;
  if Respond.Count > 0 then
  begin
    Result := reply_id = Respond.GetWord(OFFSET_HEADER + INDEX_REPLY_ID);
    RespondData := Copy(Respond, OFFSET_HEADER + OFFSET_DATA, Length(Respond) - OFFSET_DATA);
  end
  else
    Result := False;
end;

function TZKTClient.GetReplayID: Integer;
begin
  Inc(FCurrentReplyId);
  Result := FCurrentReplyId;
end;

function TZKTClient.CreateSocket: TZKTSocketStream;
begin
  Result := TZKTSocketStream.Create(Host, Port, [soSetReadTimeout, soKeepIfReadTimeout]);
  Result.Buffering := False;
  Result.Timeout := 1000;
  //Result.Timeout := WaitForEver;
  Result.EndOfLine := #10;
end;

constructor TZKTClient.Create(vHost: string; vPort: string);
begin
  inherited Create;
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
  Result := FSocket.ReadBytes(4 + 8 + 1024); //Start, Header, Data buffer
end;

function TZKTClient.Connect: Boolean;
var
  reply_id: integer;
  Buf, Respond: TBytes;
begin
  reply_id := 0;
  Buf := CreateHeader(CMD_CONNECT, FSessionId, reply_id, '');

  if FSocket = nil then
    FSocket := CreateSocket;

  FSocket.Connect;
  if FSocket.Connected then
  begin
    Send(Buf);
    Respond := Recv;
    Respond.DumpHex;
    FSessionId := Respond.GetWord(OFFSET_HEADER + INDEX_SESSION_ID);
    Result := reply_id = Respond.GetWord(OFFSET_HEADER + INDEX_REPLY_ID);//just for checking
  end;
end;

procedure TZKTClient.Disconnect;
var
  Data: TBytes;
begin
  ExecCommand(CMD_EXIT, GetReplayID, '', Data);
  FSocket.Disconnect;
  FreeAndNil(FSocket);
  FSessionId := 0;
  FCurrentReplyId := 0;
end;

function TZKTClient.GetVersion: string;
var
  Data: TBytes;
begin
  ExecCommand(CMD_VERSION, GetReplayID, '', Data);
  Result := StringOf(Data);
end;

function TZKTClient.GetSizeAddendance(out ASize: Integer): Boolean;
var
  Data: TBytes;
begin
  ExecCommand(CMD_PREPARE_DATA, GetReplayID, '', Data);
  if Data.Count > 0 then
    ASize := PDWord(Pointer(Data))^
  else
    ASize := 0;
  Result := False;
end;

function TZKTClient.GetAddendance(out Strings: TStringList): Boolean;
var
  ASize: Integer;
  Data: TBytes;
  S: string;
  Respond: TBytes;
begin
  GetSizeAddendance(ASize);
  ExecCommand(CMD_ATTLOG_RRQ, GetReplayID, '', Data);
  Writeln('Another:');
  //Respond := Recv;
  //Respond.DumpHex;
//  Strings.Text := StringOf(Data);
  Result := True;
end;

procedure TZKTClient.DisableDevice;
var
  Data: TBytes;
begin
  ExecCommand(CMD_DISABLEDEVICE, GetReplayID, #0#0, Data);
end;

procedure TZKTClient.EnableDevice;
var
  Data: TBytes;
begin
  ExecCommand(CMD_ENABLEDEVICE, GetReplayID, #0#0, Data);
end;

procedure TZKTClient.TestVoice;
var
  Data: TBytes;
begin
  ExecCommand(CMD_TESTVOICE, GetReplayID, #0#0, Data);
end;

end.
