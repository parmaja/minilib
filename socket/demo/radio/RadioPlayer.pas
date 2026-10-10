unit RadioPlayer;
{**
 *  This file is part of the "Mini Library" demos
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *
 *  Radio stream player:
 *   - Downloading: TmnIceCastClient from mnIceCasts.pas (network thread + Buffer)
 *   - Decoding   : MP3 -> PCM via Windows ACM (msacm32.dll, MPEG Layer-3 decoder)
 *   - Output     : Windows waveOut (winmm.dll)
 *
 *  Build for Win32 (or Win64 on Windows 10+ where the 64-bit MP3 ACM codec exists).
 *}

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Windows, SysUtils, Classes, SyncObjs, MMSystem,
  mnIceCasts;

type
  TRadioPlayerState = (
    rpsIdle,       //no stream
    rpsConnecting, //connecting to the server
    rpsBuffering,  //connected, prebuffering audio
    rpsPlaying,    //playing
    rpsStopped,    //stopped by user or server closed the stream
    rpsError       //failed, see the message
  );

  TRadioStateChanged = procedure(Sender: TObject; AState: TRadioPlayerState; const AMessage: string) of object;
  TRadioTitleChanged = procedure(Sender: TObject; const ATitle: string) of object;

  { TRadioPlayer }

  TRadioPlayer = class(TObject)
  private
    FLock: TCriticalSection; //protects FWaveOut (volume vs close)
    FClient: TmnIceCastClient;
    FThread: TObject; //TRadioPlayThread, defined in implementation
    FSession: Integer; //increased by every Play(), stale async posts are ignored
    FState: TRadioPlayerState;
    FMessage: string;
    FTitle: string;
    FStation: string;
    FBitrate: string;
    FContentType: string;
    FVolume: Integer; //0..100
    FStopping: Boolean;
    FWaveOut: THandle; //valid while the play thread has the device open
    FOnStateChanged: TRadioStateChanged;
    FOnTitleChanged: TRadioTitleChanged;
    procedure ClientStateChanged(Client: TmnIceCastClient);
    procedure ClientTitleChanged(Client: TmnIceCastClient);
    procedure DoState(AState: TRadioPlayerState; const AMessage: string);
    //called by the play thread when it failed, closes and releases the stream client
    procedure CloseClient;
    function GetState: TRadioPlayerState;
    function GetPlaying: Boolean;
    procedure SetVolume(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    //open the stream and start playing, call it in main thread
    procedure Play(const vURL: string);
    //stop and release the stream, call it in main thread
    procedure Stop;
    //called by the play thread after the audio device is opened
    procedure ApplyVolume;
    property State: TRadioPlayerState read GetState;
    property Playing: Boolean read GetPlaying;
    property Message: string read FMessage;
    property Title: string read FTitle;
    property Station: string read FStation;
    property Bitrate: string read FBitrate;
    property ContentType: string read FContentType;
    property Volume: Integer read FVolume write SetVolume; //0..100
    property IceCast: TmnIceCastClient read FClient;
    //all events are fired in the main thread
    property OnStateChanged: TRadioStateChanged read FOnStateChanged write FOnStateChanged;
    property OnTitleChanged: TRadioTitleChanged read FOnTitleChanged write FOnTitleChanged;
  end;

implementation

const
  cInChunkSize = 16 * 1024;        //MP3 bytes fed to the decoder per round
  cOutHeaderCount = 16;            //waveOut buffer pool
  cOutHeaderSize = 16 * 1024;      //PCM bytes per waveOut buffer
  cPrebufferSize = 32 * 1024;      //bytes to wait before start playing
  cMaxPendingPCM = (cOutHeaderCount * cOutHeaderSize) div 2; //max bytes queued in waveOut
  cReadyTimeout = 30000;           //ms waiting for the stream to be ready

  WAVE_FORMAT_MPEGLAYER3 = $0055;
  MPEGLAYER3_WFX_EXTRA_BYTES = 12;
  MPEGLAYER3_ID_MPEG = 1;
  MPEGLAYER3_FLAG_PADDING_ISO = $00000004;

  ACM_STREAMCONVERTF_BLOCKALIGN = $00000004;
  ACM_STREAMCONVERTF_START = $00000010;
  ACM_STREAMSIZEF_SOURCE = $00000000;

type
  TMP3FrameInfo = record
    Valid: Boolean;
    Version: Integer;   //1 = MPEG1, 2 = MPEG2, 0 = MPEG2.5
    SampleRate: Integer;
    Bitrate: Integer;   //bits per second
    Channels: Integer;
    FrameSize: Integer; //bytes of the frame with current padding
    BlockSize: Integer; //bytes of the frame without padding (for nBlockSize)
  end;

  PMPEGLAYER3WAVEFORMAT = ^TMPEGLAYER3WAVEFORMAT;
  TMPEGLAYER3WAVEFORMAT = packed record
    wfx: TWaveFormatEx;
    wID: Word;
    fdwFlags: DWORD;
    nBlockSize: Word;
    nFramesPerBlock: Word;
    nCodecDelay: Word;
  end;

  TACMStreamHeader = packed record
    cbStruct: DWORD;
    fdwStatus: DWORD;
    dwUser: NativeUInt;
    pbSrc: PByte;
    cbSrcLength: DWORD;
    cbSrcLengthUsed: DWORD;
    dwSrcUser: NativeUInt;
    pbDst: PByte;
    cbDstLength: DWORD;
    cbDstLengthUsed: DWORD;
    dwDstUser: NativeUInt;
    dwReserved: array[0..9] of DWORD;
  end;

//ACM functions, bound directly to keep the demo independent of header translations
function acmStreamOpen(out phas: THandle; had: THandle; pwfxSrc: Pointer; pwfxDst: Pointer; pwfltr: Pointer;
  dwCallback: DWORD; dwInstance: DWORD; fdwOpen: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamClose(has: THandle; fdwClose: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamReset(has: THandle; fdwReset: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamSize(has: THandle; cbInput: DWORD; var pdwOutputBytes: DWORD; fdwSize: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamPrepareHeader(has: THandle; var pash: TACMStreamHeader; fdwPrepare: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamUnprepareHeader(has: THandle; var pash: TACMStreamHeader; fdwUnprepare: DWORD): UINT; stdcall; external 'msacm32.dll';
function acmStreamConvert(has: THandle; var pash: TACMStreamHeader; fdwConvert: DWORD): UINT; stdcall; external 'msacm32.dll';

//scan the data for the first valid MPEG Layer 3 frame header
function ParseMP3Frame(AData: Pointer; ALen: Integer; out AInfo: TMP3FrameInfo): Boolean;
const
  BitratesV1: array[1..14] of Integer = (32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320);
  BitratesV2: array[1..14] of Integer = (8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160);
  RatesV1: array[0..2] of Integer = (44100, 48000, 32000);
  RatesV2: array[0..2] of Integer = (22050, 24000, 16000);
  RatesV25: array[0..2] of Integer = (11025, 12000, 8000);
var
  i: Integer;
  pb: PByte;
  b1, b2, b3: Byte;
  ver, lay, bri, sri, pad, mode: Integer;
begin
  Result := False;
  FillChar(AInfo, SizeOf(AInfo), 0);
  if ALen < 4 then
    Exit;
  pb := PByte(AData);
  for i := 0 to ALen - 4 do
  begin
    if pb^ = $FF then
    begin
      b1 := PByte(NativeUInt(pb) + 1)^;
      b2 := PByte(NativeUInt(pb) + 2)^;
      b3 := PByte(NativeUInt(pb) + 3)^;
      if (b1 and $E0) = $E0 then
      begin
        ver := (b1 shr 3) and $3;  //3 = MPEG1, 2 = MPEG2, 0 = MPEG2.5
        lay := (b1 shr 1) and $3;  //1 = Layer III
        bri := (b2 shr 4) and $F;
        sri := (b2 shr 2) and $3;
        pad := (b2 shr 1) and $1;
        mode := (b3 shr 6) and $3; //3 = mono
        if (ver <> 1) and (lay = 1) and (bri >= 1) and (bri <= 14) and (sri <= 2) then
        begin
          case ver of
            3: //MPEG1
              begin
                AInfo.Version := 1;
                AInfo.SampleRate := RatesV1[sri];
                AInfo.Bitrate := BitratesV1[bri] * 1000;
              end;
            2: //MPEG2
              begin
                AInfo.Version := 2;
                AInfo.SampleRate := RatesV2[sri];
                AInfo.Bitrate := BitratesV2[bri] * 1000;
              end;
            0: //MPEG2.5
              begin
                AInfo.Version := 0;
                AInfo.SampleRate := RatesV25[sri];
                AInfo.Bitrate := BitratesV2[bri] * 1000;
              end;
          end;
          if mode = 3 then
            AInfo.Channels := 1
          else
            AInfo.Channels := 2;
          if AInfo.Version = 1 then
            AInfo.FrameSize := (144 * AInfo.Bitrate) div AInfo.SampleRate + pad
          else
            AInfo.FrameSize := (72 * AInfo.Bitrate) div AInfo.SampleRate + pad;
          AInfo.BlockSize := AInfo.FrameSize - pad;
          AInfo.Valid := True;
          Result := True;
          Exit;
        end;
      end;
    end;
    Inc(pb);
  end;
end;

{ TRadioPlayThread }

type
  TRadioPlayThread = class(TThread)
  private
    FOwner: TRadioPlayer;
    FClient: TmnIceCastClient;
    FSession: Integer;
    FReadPos: Int64; //consumed position in the client buffer
    FInBuf: PAnsiChar; //MP3 input, partial frame kept at the head
    FInSize: Integer;
    FDstBuf: PAnsiChar; //PCM output of the decoder
    FDstSize: Integer;
    FACMStream: THandle;
    FACMHeader: TACMStreamHeader;
    FHasACM: Boolean;
    FFirstConvert: Boolean;
    FNoProgress: Integer;
    FWaveOut: HWAVEOUT;
    FHeaders: array[0..cOutHeaderCount - 1] of TWaveHdr;
    FBuffers: array[0..cOutHeaderCount - 1] of PAnsiChar;
    FWritten: array[0..cOutHeaderCount - 1] of Boolean;
    FFormat: TWaveFormatEx; //output PCM format
    FMinFrame: Integer; //smallest complete MP3 frame in bytes
  protected
    procedure Execute; override;
    function WaitForReady: Boolean;
    function ReadData(ABuf: PAnsiChar; AMax: Integer): Integer;
    function OpenDecoder: Boolean;
    procedure CloseDecoder;
    procedure DecodeLoop;
    procedure ConvertRound;
    procedure PlayPCM(ABuf: PAnsiChar; ALen: Integer);
    function PendingBytes: Integer;
    procedure PostState(AState: TRadioPlayerState; const AMessage: string);
  public
    constructor Create(AOwner: TRadioPlayer; AClient: TmnIceCastClient);
    destructor Destroy; override;
  end;

constructor TRadioPlayThread.Create(AOwner: TRadioPlayer; AClient: TmnIceCastClient);
begin
  FOwner := AOwner;
  FClient := AClient;
  FSession := AOwner.FSession;
  GetMem(FInBuf, cInChunkSize);
  inherited Create(False); //start now
end;

destructor TRadioPlayThread.Destroy;
begin
  if FInBuf <> nil then
    FreeMemory(FInBuf);
  if FDstBuf <> nil then
    FreeMemory(FDstBuf);
  inherited;
end;

procedure TRadioPlayThread.PostState(AState: TRadioPlayerState; const AMessage: string);
var
  vState: TRadioPlayerState;
  vMsg: string;
  vSession: Integer;
begin
  vState := AState;
  vMsg := AMessage;
  vSession := FSession;
  TThread.Queue(nil,
    procedure
    begin
      if (FOwner <> nil) and (FOwner.FSession = vSession) then
        FOwner.DoState(vState, vMsg);
    end);
end;

function TRadioPlayThread.WaitForReady: Boolean;
var
  t: Integer;
begin
  Result := False;
  t := 0;
  while not Terminated do
  begin
    case FClient.State of
      icReady:
        Exit(True);
      icStopped, icError:
        Exit(False); //client events already notified the UI
    end;
    Sleep(50);
    Inc(t, 50);
    if t >= cReadyTimeout then
    begin
      PostState(rpsError, 'Connection timeout');
      Exit(False);
    end;
  end;
end;

//read downloaded data, keep our own position because the client buffer only grows
function TRadioPlayThread.ReadData(ABuf: PAnsiChar; AMax: Integer): Integer;
var
  vBuf: TMemoryStream;
begin
  Result := 0;
  if AMax <= 0 then
    Exit;
  FClient.Lock.Enter;
  try
    vBuf := FClient.Buffer;
    if (vBuf = nil) or (vBuf.Size <= FReadPos) then
      Exit;
    vBuf.Position := FReadPos;
    Result := vBuf.Read(ABuf^, AMax);
    Inc(FReadPos, Result);
    if FReadPos >= vBuf.Size then
    begin //fully consumed, let the buffer release its memory
      vBuf.Size := 0;
      FReadPos := 0;
    end;
  finally
    FClient.Lock.Leave;
  end;
end;

function TRadioPlayThread.OpenDecoder: Boolean;
var
  n: Integer;
  aInfo: TMP3FrameInfo;
  aMP3: TMPEGLAYER3WAVEFORMAT;
  aSize: DWORD;
  i: Integer;
begin
  Result := False;
  //collect the first bytes to parse a frame header
  while (not Terminated) and (FClient.State = icReady) and (FInSize < 4096) do
  begin
    n := ReadData(FInBuf + FInSize, cInChunkSize - FInSize);
    if n <= 0 then
      Sleep(20)
    else
      Inc(FInSize, n);
  end;
  if Terminated then
    Exit;
  if not ParseMP3Frame(FInBuf, FInSize, aInfo) then
  begin
    PostState(rpsError, 'Cannot find MP3 frames in the stream');
    Exit;
  end;
  FMinFrame := aInfo.BlockSize + 4; //minimum bytes of a complete frame
  //source format: MPEG Layer-3
  FillChar(aMP3, SizeOf(aMP3), 0);
  aMP3.wfx.wFormatTag := WAVE_FORMAT_MPEGLAYER3;
  aMP3.wfx.nChannels := aInfo.Channels;
  aMP3.wfx.nSamplesPerSec := aInfo.SampleRate;
  aMP3.wfx.nAvgBytesPerSec := aInfo.Bitrate div 8;
  aMP3.wfx.nBlockAlign := 1;
  aMP3.wfx.wBitsPerSample := 0;
  aMP3.wfx.cbSize := MPEGLAYER3_WFX_EXTRA_BYTES;
  aMP3.wID := MPEGLAYER3_ID_MPEG;
  aMP3.fdwFlags := MPEGLAYER3_FLAG_PADDING_ISO;
  aMP3.nBlockSize := aInfo.BlockSize;
  aMP3.nFramesPerBlock := 1;
  aMP3.nCodecDelay := 0;
  //destination format: 16 bit PCM
  FillChar(FFormat, SizeOf(FFormat), 0);
  FFormat.wFormatTag := WAVE_FORMAT_PCM;
  FFormat.nChannels := aInfo.Channels;
  FFormat.nSamplesPerSec := aInfo.SampleRate;
  FFormat.wBitsPerSample := 16;
  FFormat.nBlockAlign := FFormat.nChannels * 2;
  FFormat.nAvgBytesPerSec := FFormat.nBlockAlign * FFormat.nSamplesPerSec;
  FFormat.cbSize := 0;

  if waveOutGetNumDevs = 0 then
  begin
    PostState(rpsError, 'No audio device found');
    Exit;
  end;
  if acmStreamOpen(FACMStream, 0, @aMP3, @FFormat, nil, 0, 0, 0) <> 0 then
  begin
    PostState(rpsError, 'Cannot open the MP3 decoder (ACM)');
    Exit;
  end;
  FHasACM := True;
  //how much PCM the decoder may produce from one full input chunk
  aSize := cInChunkSize;
  if acmStreamSize(FACMStream, cInChunkSize, aSize, ACM_STREAMSIZEF_SOURCE) <> 0 then
    aSize := cInChunkSize * 16;
  FDstSize := aSize + 4096;
  GetMem(FDstBuf, FDstSize);
  //open the audio device
  if waveOutOpen(@FWaveOut, WAVE_MAPPER, @FFormat, 0, 0, CALLBACK_NULL) <> MMSYSERR_NOERROR then
  begin
    PostState(rpsError, 'Cannot open the audio device (waveOut)');
    Exit;
  end;
  for i := 0 to cOutHeaderCount - 1 do
  begin
    GetMem(FBuffers[i], cOutHeaderSize);
    FillChar(FHeaders[i], SizeOf(TWaveHdr), 0);
    FHeaders[i].lpData := FBuffers[i];
    FHeaders[i].dwBufferLength := cOutHeaderSize;
    if waveOutPrepareHeader(FWaveOut, @FHeaders[i], SizeOf(TWaveHdr)) <> MMSYSERR_NOERROR then
      raise Exception.Create('waveOutPrepareHeader failed');
    FWritten[i] := False;
  end;
  FOwner.FLock.Enter;
  try
    FOwner.FWaveOut := FWaveOut;
  finally
    FOwner.FLock.Leave;
  end;
  FOwner.ApplyVolume;
  Result := True;
end;

procedure TRadioPlayThread.CloseDecoder;
var
  aWave: HWAVEOUT;
  i: Integer;
begin
  FOwner.FLock.Enter;
  try
    FOwner.FWaveOut := 0;
  finally
    FOwner.FLock.Leave;
  end;
  aWave := FWaveOut;
  FWaveOut := 0;
  if aWave <> 0 then
  begin
    waveOutReset(aWave);
    for i := 0 to cOutHeaderCount - 1 do
    begin
      if FWritten[i] then
        waveOutUnprepareHeader(aWave, @FHeaders[i], SizeOf(TWaveHdr));
      if FBuffers[i] <> nil then
        FreeMemory(FBuffers[i]);
      FBuffers[i] := nil;
    end;
    waveOutClose(aWave);
  end;
  if FHasACM then
  begin
    acmStreamReset(FACMStream, 0);
    acmStreamClose(FACMStream, 0);
    FHasACM := False;
  end;
end;

function TRadioPlayThread.PendingBytes: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to cOutHeaderCount - 1 do
    if FWritten[i] and ((FHeaders[i].dwFlags and WHDR_DONE) = 0) then
      Inc(Result, cOutHeaderSize);
end;

procedure TRadioPlayThread.PlayPCM(ABuf: PAnsiChar; ALen: Integer);
var
  i, n, aFree: Integer;
  p: PAnsiChar;
begin
  p := ABuf;
  while (ALen > 0) and (FWaveOut <> 0) and not Terminated do
  begin
    aFree := -1;
    for i := 0 to cOutHeaderCount - 1 do
      if (not FWritten[i]) or ((FHeaders[i].dwFlags and WHDR_DONE) <> 0) then
      begin
        aFree := i;
        Break;
      end;
    if aFree < 0 then
    begin
      Sleep(10); //all buffers queued, wait the device to consume
      Continue;
    end;
    n := ALen;
    if n > cOutHeaderSize then
      n := cOutHeaderSize;
    Move(p^, FBuffers[aFree]^, n);
    FHeaders[aFree].dwBufferLength := n;
    if waveOutWrite(FWaveOut, @FHeaders[aFree], SizeOf(TWaveHdr)) <> MMSYSERR_NOERROR then
      raise Exception.Create('waveOutWrite failed');
    FWritten[aFree] := True;
    Inc(p, n);
    Dec(ALen, n);
  end;
end;

procedure TRadioPlayThread.ConvertRound;
var
  Flags: DWORD;
  Used, Produced: Integer;
begin
  if FInSize <= 0 then
    Exit;
  FillChar(FACMHeader, SizeOf(FACMHeader), 0);
  FACMHeader.cbStruct := SizeOf(FACMHeader);
  FACMHeader.pbSrc := PByte(FInBuf);
  FACMHeader.cbSrcLength := FInSize;
  FACMHeader.pbDst := PByte(FDstBuf);
  FACMHeader.cbDstLength := FDstSize;
  if acmStreamPrepareHeader(FACMStream, FACMHeader, 0) <> 0 then
    raise Exception.Create('acmStreamPrepareHeader failed');
  try
    if FFirstConvert then
      Flags := ACM_STREAMCONVERTF_START or ACM_STREAMCONVERTF_BLOCKALIGN
    else
      Flags := ACM_STREAMCONVERTF_BLOCKALIGN;
    if acmStreamConvert(FACMStream, FACMHeader, Flags) <> 0 then
      raise Exception.Create('acmStreamConvert failed');
    Used := Integer(FACMHeader.cbSrcLengthUsed);
    Produced := Integer(FACMHeader.cbDstLengthUsed);
  finally
    acmStreamUnprepareHeader(FACMStream, FACMHeader, 0);
  end;
  FFirstConvert := False;
  if Produced > 0 then
    PlayPCM(FDstBuf, Produced);
  if (Used > 0) and (Used < FInSize) then
    Move((FInBuf + Used)^, FInBuf^, FInSize - Used); //keep the partial frame at the head
  if Used > 0 then
    Dec(FInSize, Used);
  if (Used = 0) and (Produced = 0) then
  begin
    Inc(FNoProgress);
    Sleep(10); //decoder buffered it, or it is a stall, avoid a tight loop
    if FNoProgress > 200 then
      raise Exception.Create('MP3 decoder is not consuming the stream');
  end
  else
    FNoProgress := 0;
end;

procedure TRadioPlayThread.DecodeLoop;
var
  n: Integer;
begin
  FFirstConvert := True;
  FNoProgress := 0;
  while not Terminated do
  begin
    if FClient.State <> icReady then
      Break; //stream ended or error, the client events notify the UI
    if PendingBytes >= cMaxPendingPCM then
    begin
      Sleep(15); //device queue is full, playback is faster than realtime
      Continue;
    end;
    n := ReadData(FInBuf + FInSize, cInChunkSize - FInSize);
    if n > 0 then
      Inc(FInSize, n)
    else if FInSize = 0 then
    begin
      Sleep(15); //wait for the network
      Continue;
    end;
    if FInSize < FMinFrame then
    begin
      Sleep(15); //not even one complete frame buffered yet
      Continue;
    end;
    ConvertRound;
  end;
end;

procedure TRadioPlayThread.Execute;
begin
  try
    try
      if not WaitForReady then
        Exit;
      //prebuffer to avoid crackling at the start
      while (not Terminated) and (FClient.State = icReady) and (FClient.BufferSize < cPrebufferSize) do
        Sleep(20);
      if Terminated or (FClient.State <> icReady) then
        Exit;
      if OpenDecoder then
      begin
        PostState(rpsPlaying, '');
        DecodeLoop;
      end;
    except
      on E: Exception do
      begin
        PostState(rpsError, E.Message);
        FOwner.CloseClient; //stop downloading too
      end;
    end;
  finally
    CloseDecoder;
  end;
end;

{ TRadioPlayer }

constructor TRadioPlayer.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FState := rpsIdle;
  FVolume := 90;
end;

destructor TRadioPlayer.Destroy;
begin
  Stop;
  FreeAndNil(FLock);
  inherited;
end;

procedure TRadioPlayer.DoState(AState: TRadioPlayerState; const AMessage: string);
begin
  FState := AState;
  FMessage := AMessage;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, AState, AMessage);
end;

procedure TRadioPlayer.ClientStateChanged(Client: TmnIceCastClient);
begin
  if Client <> FClient then
    Exit; //stale event from an old stream
  FStation := Client.StationName;
  FBitrate := Client.Bitrate;
  FContentType := Client.ContentType;
  case Client.State of
    icReady:
      DoState(rpsBuffering, '');
    icError:
      DoState(rpsError, Client.Error);
    icStopped:
      if not FStopping and (FState <> rpsError) then
        DoState(rpsStopped, 'Stream ended');
  end;
end;

procedure TRadioPlayer.ClientTitleChanged(Client: TmnIceCastClient);
begin
  if Client <> FClient then
    Exit;
  FTitle := Client.Title;
  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self, FTitle);
end;

procedure TRadioPlayer.Play(const vURL: string);
var
  aClient: TmnIceCastClient;
begin
  Stop;
  Inc(FSession);
  FStopping := False;
  FTitle := '';
  FMessage := '';
  FStation := '';
  FBitrate := '';
  FContentType := '';
  FState := rpsConnecting;
  aClient := TmnIceCastClient.Create;
  aClient.OnStateChanged := ClientStateChanged;
  aClient.OnTitleChanged := ClientTitleChanged;
  FClient := aClient;
  aClient.Open(vURL); //starts the connection thread
  FThread := TRadioPlayThread.Create(Self, aClient); //starts the play thread
end;

procedure TRadioPlayer.Stop;
var
  aThread: TRadioPlayThread;
  aClient: TmnIceCastClient;
begin
  FLock.Enter;
  try
    aThread := TRadioPlayThread(FThread);
    aClient := FClient;
    FThread := nil;
    FClient := nil;
  finally
    FLock.Leave;
  end;
  if (aThread = nil) and (aClient = nil) then
    Exit;
  FStopping := True;
  if aThread <> nil then
    aThread.Terminate;
  if aClient <> nil then
    aClient.Close; //disconnect and wait the network thread
  if aThread <> nil then
  begin
    aThread.WaitFor;
    aThread.Free;
  end;
  if aClient <> nil then
  begin
    //free it later, queued state events of the client may still wait in the main queue
    TThread.Queue(nil,
      procedure
      begin
        aClient.Free;
      end);
  end;
  DoState(rpsStopped, '');
end;

{called by the play thread when decoding failed,
 only one caller (thread or Stop) gets the client}
procedure TRadioPlayer.CloseClient;
var
  aClient: TmnIceCastClient;
begin
  FLock.Enter;
  try
    aClient := FClient;
    FClient := nil;
  finally
    FLock.Leave;
  end;
  if aClient <> nil then
  begin
    aClient.Close; //stop the download
    TThread.Queue(nil,
      procedure
      begin
        aClient.Free;
      end);
  end;
end;

procedure TRadioPlayer.ApplyVolume;
var
  aWave: THandle;
  aVol: DWORD;
begin
  if (FVolume < 0) or (FVolume > 100) then
    FVolume := 90;
  FLock.Enter;
  try
    aWave := FWaveOut;
  finally
    FLock.Leave;
  end;
  if aWave <> 0 then
  begin
    aVol := DWORD(FVolume) * $FFFF div 100;
    aVol := (aVol shl 16) or aVol;   // left channel in high word, right in low word
    waveOutSetVolume(HWAVEOUT(aWave), aVol);
  end;
end;

function TRadioPlayer.GetState: TRadioPlayerState;
begin
  Result := FState;
end;

function TRadioPlayer.GetPlaying: Boolean;
begin
  Result := FState = rpsPlaying;
end;

procedure TRadioPlayer.SetVolume(const Value: Integer);
begin
  if Value < 0 then
    FVolume := 0
  else if Value > 100 then
    FVolume := 100
  else
    FVolume := Value;
  ApplyVolume;
end;

end.
