unit mnStreamUtils;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 * @ported    Most code of deflate and inflate ported from FPC source zstream, i just wrapped it into my StreamProxy
 *}

{$M+}{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, zlib, Math,
  mnUtils, mnStreams;

const
  GzWindowBits                        = 15;
  GzipWindowBits                      = GzWindowBits + 16;
  GzipBits: array[Boolean] of integer = (GzWindowBits, GzipWindowBits);


type
  { TmnPlainStreamProxy }

  TmnPlainStreamProxy = class(TmnStreamOverProxy)
    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  end;

  { Compressing }

  TmnCompressLevel = 0..9;
  TmnStreamCompress = set of (cprsRead, cprsWrite);

  TmnCompressStreamProxy = class abstract(TmnStreamOverProxy)
  private
    FLimit: Cardinal;
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel = 9); virtual;
    class function GetCompressName: string; virtual; abstract;
    property Limit: Cardinal read FLimit write FLimit;
  end;

  TmnCompressStreamProxyClass = class of TmnCompressStreamProxy;

  { TmnDeflateStreamProxy }

  TmnDeflateStreamProxy = class(TmnCompressStreamProxy)
  private
    type
      TInflateInfo = record
        ZStream: z_stream;
        ZBuffer: PByte;
        ZEnd: Boolean;
      end;

  private
    FLevel: TmnCompressLevel;
    FGZip: Boolean;
    DeflateInfo: TInflateInfo;
    InflateInfo: TInflateInfo;
    FBufSize: Cardinal;
    FLimitRead: Cardinal;

  const
    DEF_MEM_LEVEL = 8;
    MAX_WBITS = 15;
  protected
    FCompress: TmnStreamCompress;

    procedure InitDeflate;
    procedure InitInflate;
    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel = 9); override;
    destructor Destroy; override;
    procedure CloseDeflate; //close it after finishing compress a stream or partial a stream
    procedure CloseInflate;
    class function GetCompressName: string; override;
    property BufSize: Cardinal read FBufSize write FBufSize;
  end;

  { TmnGzipStreamProxy }

  TmnGzipStreamProxy = class(TmnDeflateStreamProxy)
  private
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel); override;
    class function GetCompressName: string; override;
  end;

  { TmnChunkStreamProxy }

  TmnChunkStreamProxy = class(TmnStreamOverProxy)
  protected
    FReadSize: Integer;
    function FrameSize: Integer;

    function ReadSize: Integer;
    procedure ReadChunkEndOfLine;

    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  end;

  { WebSocket }

  TmnProtcolStreamProxy = class abstract(TmnStreamOverProxy)
  public
    class function GetProtocolName: string; virtual; abstract;
  end;

  TmnProtcolStreamProxyClass = class of TmnProtcolStreamProxy;

  { WebSocket }

//  http://livepersoninc.github.io/ws-test-page/

{******************************************************************

  +-+-+-+-+-+-+-+-+
  |7 6 5 4 3 2 1 0|
  +-+-+-+-+-------+
  |F|C|R|R| opcode|  F=Finish, R = Reserved, C=Compress flag
  +-+-------------+
  |M| Len         |  M = Masked, Len, Size of payload if <126 no need to pass the Size(16 or 64)
  +---------------+
  | Size 16 (126) |  Payload Size if Len = 126 or 127 (64 bit) LittleEndian
  | Size 16       |
  | Size 64 (127) |  Payload Size if Len = 127
  | Size 64       |
  | Size 64       |
  | Size 64       |
  | Size 64       |
  | Size 64       |
  +---------------+
  | Mask * 4 (32) |  Mask key if Mask flag set
  +---------------+
  | Payload Data  |
  +---------------+

  {$Z1}

  TWSMask = record
  public
    procedure Clear;
    case Boolean of
      True: (Key: Cardinal);
      False: (Mask: array[0..3] of Byte);
  end;

  TWSOpcode = (wsoConitnue = 0, wsoText = 1, wsoBinary = 2, wsoClose = $8, wsoPing = $9, wsoPong = $A, _wsoEnd = $F);
  TWSSizeType = (wsoSmall, wsoSize16, wsoSize64);

  TWebsocketPayloadHeader = packed record
  private
    Byte1: Byte;
    Byte2: Byte;
    function GetOpcode: TWSOpcode;
    procedure SetOpcode(const Value: TWSOpcode);
    function GetMasked: Boolean;
    procedure SetMasked(const Value: Boolean);
    function GetInteralSize: Byte;
    procedure SetInteralSize(const Value: Byte);
    function GetSizeType: TWSSizeType;
    function GetFinished: Boolean;
    procedure SetFinished(const Value: Boolean);
    function GetCompressMessages: Boolean;
    procedure SetCompressMessages(const Value: Boolean);
  public
    class operator Initialize({$ifdef FPC}var{$else}out{$endif} Dest: TWebsocketPayloadHeader);

    property Finished: Boolean read GetFinished write SetFinished;
    property CompressMessages: Boolean read GetCompressMessages write SetCompressMessages;

    property Opcode: TWSOpcode read GetOpcode write SetOpcode;
    property Masked: Boolean read GetMasked write SetMasked;
    property InteralSize: Byte read GetInteralSize write SetInteralSize;
    property SizeType: TWSSizeType read GetSizeType;
  end;

  TWebsocketPayload = record
  private
    FSize: Int64;
    FMask: TWSMask;
    function GetSize: Int64;
    procedure SetSize(const Value: Int64);
  public
    Header: TWebsocketPayloadHeader;
    property Size: Int64 read GetSize write SetSize;
    property Mask: TWSMask read FMask write FMask;
  end;

  TmnWebSocket13StreamProxy = class(TmnProtcolStreamProxy)
  private
    FSize: Integer;
    FMasked: Boolean;
    FMask: TWSMask;
    FMaskIndex: Integer;
    Header: TWebsocketPayloadHeader;
    Binary: Boolean;
  protected
    function ReadHeader: Boolean;
    property Mask: TWSMask read FMask write FMask;
    property Masked: Boolean read FMasked write FMasked;
    procedure MaskData(const FromData; var ToData; ASize: Integer); inline;
  public
    constructor Create(ABinary: Boolean = True; AMasked: Boolean = False; AMask: Cardinal = 0);
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    class function GetProtocolName: string; override;
  end;

function gzipDecompressStream(inStream, outStream: TStream; Count: Int64): Int64;

implementation

function ZDecompressCheck(code: Integer): Integer; overload;
begin
  Result := code;
  if code < 0 then
    raise EZDecompressionError.Create(string(_z_errmsg[2 - code])) at ReturnAddress;
end;

function gzipDecompressStream(inStream, outStream: TStream; Count: Int64): Int64;
const
  bufferSize = 32768;
var
  zstream: TZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of Byte;
  outBuffer: array[0..bufferSize - 1] of Byte;
  inSize: Integer;
  outSize: Integer;
  remaining: Int64;
begin
  Result := 0;
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  remaining := Count;

  // Initialize decompression
  ZDecompressCheck(inflateInit2(zstream, GzipBits[True]));

  try
    // Process data in chunks
    while (remaining > 0) do
    begin
      // Determine how much to read (don't exceed remaining or buffer size)
      inSize := inStream.Read(inBuffer, Min(bufferSize, remaining));
      if inSize <= 0 then
        Break;

      Dec(remaining, inSize);

      zstream.next_in := @inBuffer[0];
      zstream.avail_in := inSize;

      // Decompress available input
      repeat
        zstream.next_out := @outBuffer[0];
        zstream.avail_out := bufferSize;

        ZDecompressCheck(inflate(zstream, Z_NO_FLUSH));

        outSize := bufferSize - zstream.avail_out;
        if outSize > 0 then
          outStream.Write(outBuffer, outSize);
        Inc(Result, outSize);
      until (zstream.avail_in = 0) or (zstream.avail_out > 0);
    end;

    // Finalize decompression
    repeat
      zstream.next_out := @outBuffer[0];
      zstream.avail_out := bufferSize;

      zresult := inflate(zstream, Z_FINISH);
      if (zresult <> Z_STREAM_END) and (zresult <> Z_OK) then
        ZDecompressCheck(zresult);

      outSize := bufferSize - zstream.avail_out;
      if outSize > 0 then
        outStream.Write(outBuffer, outSize);
      Inc(Result, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);
  finally
    inflateEnd(zstream);
  end;
end;

{ TmnDeflateWriteStreamProxy }

function TmnDeflateStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  err: Smallint;
  HaveWrite: Longint;
begin
  if cprsWrite in FCompress then
  begin
    with DeflateInfo do
      if ZEnd then
        Result := True
      else
      begin
        InitDeflate; //init it if not initialized
        ZStream.next_in := @Buffer;
        ZStream.avail_in := Count;
        while ZStream.avail_in <> 0 do
        begin
          if ZStream.avail_out = 0 then
          begin
            { Flush the buffer to the stream and update progress }
            Over.Write(ZBuffer^, BufSize, HaveWrite, RealCount);
            { reset output buffer }
            ZStream.next_out := Pointer(ZBuffer);
            ZStream.avail_out := BufSize;
          end;
          err := deflate(ZStream, Z_NO_FLUSH);
          if err = Z_STREAM_END then
          begin
            ZEnd := True;
            break
          end
          else if err <> Z_OK then
            raise Exception.Create(String(zerror(err)));
        end;
        ResultCount := Count;
        Result := True;
      end;
  end
  else
    Result := Over.Write(Buffer, Count, ResultCount, RealCount);
end;

class function TmnDeflateStreamProxy.GetCompressName: string;
begin
  Result := 'deflate';
end;

function TmnDeflateStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  err: Smallint;
  HaveRead: Longint;
  aSize: Longint;
begin
  if cprsRead in FCompress then
  begin
    //Example https://jigsaw.w3.org/HTTP/ChunkedScript
    with InflateInfo do
    {if ZEnd then  ///* No because there is gzip stream even reach END but still feeding it, we need to reopen it
      ResultCount := 0
    else}
    begin
      InitInflate; //init it if not initialized
      ZStream.next_out := @buffer;
      ZStream.avail_out := Count;
      while ZStream.avail_out <> 0 do
      begin
        if ZStream.avail_in = 0 then
        begin
          {Refill the buffer.}
          if Limit<>0 then
          begin
            aSize := FLimit-FLimitRead;
            if aSize<=0 then Break;
            if aSize>BufSize then
              aSize := BufSize;
          end
          else
            aSize := BufSize;

          Over.Read(ZBuffer^, aSize, HaveRead, RealCount); //BufSize or count ???
          ZStream.next_in := Pointer(ZBuffer);
          ZStream.avail_in := HaveRead;
          if HaveRead=0 then //timeout or disconnected
            break;

          if Limit<>0 then
          begin
            Inc(FLimitRead, HaveRead);
          end;
        end
        else
          RealCount := 0;

        err := inflate(ZStream, Z_NO_FLUSH);
        if err = Z_STREAM_END then
        begin
          ZEnd := True;
//          CloseFragment; //* no because we can reach END of zlib but sill need to inflate the stream, weird, do not trust END
          break;
        end
        else if err <> Z_OK then
        begin
          ZEnd := True;
//          CloseFragment;
          raise Exception.Create(String(zerror(err)));
        end;
      end;
      ResultCount := Count - Integer(ZStream.avail_out);

      if FLimit<>0 then
      begin
        if FLimitRead>=FLimit then
          CloseFragment;
      end;

    end;
    Result := True;
  end
  else
    Result := Over.Read(Buffer, Count, ResultCount, RealCount);
end;

procedure TmnDeflateStreamProxy.CloseWrite;
begin
  CloseDeflate;
end;

procedure TmnDeflateStreamProxy.CloseRead;
begin
  CloseInflate;
end;

procedure TmnDeflateStreamProxy.CloseDeflate;
var
  err: Smallint;
  Written, R: Longint;
begin
  if cprsWrite in FCompress then
  begin
    with DeflateInfo do
    if ZBuffer <> nil then
    begin
      {Compress remaining data still in internal zlib data buffers.}
      repeat
        if ZStream.avail_out = 0 then
        begin
          { Flush the buffer to the stream and update progress }
          Over.Write(ZBuffer^, BufSize, Written, R);
          { reset output buffer }
          ZStream.next_out := Pointer(ZBuffer);
          ZStream.avail_out := BufSize;
        end;
        err := deflate(ZStream, Z_FINISH);
        if err = Z_STREAM_END then
          break;
        if (err <> Z_OK) then
          raise Exception.Create(String(zerror(err)));
      until False;

      if ZStream.avail_out < BufSize then
      begin
        Over.Write(ZBuffer^, BufSize - ZStream.avail_out, Written, R);
      end;

      deflateEnd(ZStream);
      FreeMem(ZBuffer);
      ZBuffer := nil;
    end;
  end;
end;

procedure TmnDeflateStreamProxy.CloseInflate;
begin
  if cprsRead in FCompress then
    with InflateInfo do
      if ZBuffer <> nil then
      begin
        InflateEnd(ZStream);
        FreeMem(ZBuffer);
        ZBuffer := nil;
      end;
end;

procedure TmnDeflateStreamProxy.InitDeflate;
var
  err: Smallint;
  WindowBits: Integer;
begin
  with DeflateInfo do
  begin
    if ZBuffer = nil then
    begin
      GetMem(ZBuffer, BufSize);

      ZStream.next_out := Pointer(ZBuffer);
      ZStream.avail_out := BufSize;

      if FGZip then
        WindowBits := MAX_WBITS + 16
      else
      begin
        WindowBits := -MAX_WBITS;
      end;
      err := deflateInit2(ZStream, FLevel, Z_DEFLATED, WindowBits, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY);
      if err <> Z_OK then
        raise Exception.Create(String(zerror(err)));
    end;
    ZEnd := False;
  end;
end;

procedure TmnDeflateStreamProxy.InitInflate;
var
  err: Smallint;
  WindowBits: Integer;
begin
  with InflateInfo do
    if ZBuffer = nil then
    begin
      GetMem(ZBuffer, BufSize);

      ZStream.next_in := Pointer(ZBuffer);
      ZStream.avail_in := 0;

      if FGZip then
        WindowBits := MAX_WBITS + 16
      else
      begin
        WindowBits := -MAX_WBITS;
      end;
      err := inflateInit2(ZStream, WindowBits);
      if err <> Z_OK then
        raise Exception.Create(String(zerror(err)));
    end;
end;

constructor TmnDeflateStreamProxy.Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel);
begin
  inherited Create(ACompress, Level);
  FBufSize := 16384;
  FLevel := Level;
  FCompress := ACompress;
  FGZip := False;
end;

destructor TmnDeflateStreamProxy.Destroy;
begin
  CloseDeflate;
  CloseInflate;
  inherited Destroy;
end;

{ TmnGzipStreamProxy }

constructor TmnGzipStreamProxy.Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel);
begin
  inherited;
  FGZip := True;
end;

class function TmnGzipStreamProxy.GetCompressName: string;
begin
  Result := 'gzip';
end;

{ TmnCompressStreamProxy }

constructor TmnCompressStreamProxy.Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel);
begin
  inherited Create;
end;

{ TmnChunkStreamProxy }

procedure TmnChunkStreamProxy.CloseRead;
begin
  inherited;
  FReadSize := 0;
end;

procedure TmnChunkStreamProxy.CloseWrite;
const
  sEOD = '0'#13#10#13#10;
var
  r, e: Longint;

begin
  inherited;
  Over.Write(PUtf8Char(sEOD)^, Length(sEOD), r, e);

end;

function TmnChunkStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  aCount: longint;
begin
  ResultCount := 0;
  RealCount := 0;
  Result := True;
  if FReadSize = 0 then
  begin
    FReadSize := ReadSize;
    if FReadSize = 0 then
    begin
      CloseFragment;
    end;
  end;

  if FReadSize > 0 then
  begin
    if FReadSize>=Count then
      aCount := Count
    else
      aCount := FReadSize;

    Over.Read(Buffer, aCount, ResultCount, RealCount);
    FReadSize := FReadSize - ResultCount;
  end;

  if FReadSize = 0 then
  begin
    ReadChunkEndOfLine;
//    CloseFragment
  end;
end;

function TmnChunkStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
const
  sEOL = #13#10;

var
  t: Integer;
  r, e: LongInt;
  b: PByte;
  s: UTF8String;
begin
  { TODO : support chunk write }

  b := @Buffer;

  ResultCount := Count;
  RealCount := Count;

  while True do
  begin
    if Count>FrameSize then
      t := FrameSize
    else
      t := Count;

    s := UTF8Encode(IntToHex(t, 1))+sEOL;

    Over.Write(PUtf8Char(s)^, Length(s), r, e);
    Result := Over.Write(b^, t, r, e);
    Over.Write(PUtf8Char(sEOL)^, Length(sEOL), r, e);
    Inc(b, t);
    //Result := Over.Write(#13#10, t, r, e);
    Count := Count-t;
    if Count<=0 then
      break;
  end;
end;

procedure TmnChunkStreamProxy.ReadChunkEndOfLine;
var
  b: Byte;
  r, c: LongInt;
begin
  Over.Read(b, 1, r, c); //skip $A
  if b = 13 then
    Over.Read(b, 1, r, c); //skip $A
end;

function TmnChunkStreamProxy.ReadSize: Integer;
var
  b: Byte;
  r, c: LongInt;
  s: string;
  t: Boolean;
begin
  Result := -1;

  s := '';
  while True do
  begin
    t := Over.Read(b, 1, r, c);
    if t and (r<>0) then
    begin
      case b of
        13, 10:
        begin
          Result := StrToIntDef('$'+s, 0);
          if b=13 then
            Over.Read(b, 1, r, c); //skip $A
          break;
        end;
        else
          s := s + Chr(b);
      end;
    end
    else
      break;
  end;
end;

function TmnChunkStreamProxy.FrameSize: Integer;
begin
  Result := 1024;
end;

{ TmnPlainStreamProxy }

procedure TmnPlainStreamProxy.CloseRead;
begin
  inherited;

end;

procedure TmnPlainStreamProxy.CloseWrite;
begin
  inherited;

end;

function TmnPlainStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
begin
  Result := Over.Read(Buffer, Count, ResultCount, RealCount);
end;

function TmnPlainStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
begin
  Result := Over.Write(Buffer, Count, ResultCount, RealCount);
end;

{ TWebsocketPayloadHeader }

function TWebsocketPayloadHeader.GetSizeType: TWSSizeType;
begin
  if InteralSize < 126 then
    Result := wsoSmall
  else if InteralSize = 126 then
    Result := wsoSize16
  else
    Result := wsoSize64;
end;

class operator TWebsocketPayloadHeader.Initialize({$ifdef FPC}var{$else}out{$endif} Dest: TWebsocketPayloadHeader);
begin
  FillChar(Dest, SizeOf(Dest), 0);
end;

{
  Opcode and Finish
   7 6 5 4 3 2 1 0
  |F|R|R|R| opcode|  F=Finish, R = Reserved
}

function TWebsocketPayloadHeader.GetFinished: Boolean;
begin
  Result := (Byte1 and $80) > 0;
end;

procedure TWebsocketPayloadHeader.SetFinished(const Value: Boolean);
begin
  Byte1 := Byte1 and (not $80) or (ord(Value) shl 7);
end;

function TWebsocketPayloadHeader.GetCompressMessages: Boolean;
begin
  Result := (Byte1 and $40) > 0;
end;

procedure TWebsocketPayloadHeader.SetCompressMessages(const Value: Boolean);
begin
  Byte1 := Byte1 and (not $40) or (ord(Value) shl 6);
end;

function TWebsocketPayloadHeader.GetOpcode: TWSOpcode;
begin
  Result := TWSOpcode(Byte((Byte1 and $0F)));
end;

procedure TWebsocketPayloadHeader.SetOpcode(const Value: TWSOpcode);
begin
  Byte1 := Byte1 and $F0 or Byte(Value);
end;

{
  Masked and Size

   7 6 5 4 3 2 1 0
  |M| Len         |  M = Masked, Len, Size of payload if <126 no need to pass the Size(16 or 64)
}
function TWebsocketPayloadHeader.GetMasked: Boolean;
begin
  Result := Byte(Byte2 and $80) > 0;
end;

procedure TWebsocketPayloadHeader.SetMasked(const Value: Boolean);
begin
  Byte2 := Byte2 and (not $80) or (ord(Value) shl 7);
end;

function TWebsocketPayloadHeader.GetInteralSize: Byte;
begin
  Result := Byte((Byte2 and not $80));
end;

procedure TWebsocketPayloadHeader.SetInteralSize(const Value: Byte);
begin
  Byte2 := Byte2 or (Value and (not $80));
end;

procedure TWSMask.Clear;
begin
  Key := 0;
end;

{ TWebsocketPayload }

function TWebsocketPayload.GetSize: Int64;
begin
  Result := Header.InteralSize;
end;

procedure TWebsocketPayload.SetSize(const Value: Int64);
begin
  Header.InteralSize := Value;
end;

{ TmnWebSocket13StreamProxy }

class function TmnWebSocket13StreamProxy.GetProtocolName: string;
begin
  Result := 'websocket.13';
end;

procedure TmnWebSocket13StreamProxy.MaskData(const FromData; var ToData; ASize: Integer);
var
  aIndex: longint;
  fp, tp: PByte;
begin
  aIndex := 0;
  fp := @FromData;
  tp := @ToData;
  while aIndex < aSize do
  begin
    tp^ := fp^ xor FMask.Mask[FMaskIndex];
    Inc(FMaskIndex);
    if FMaskIndex>=Length(FMask.Mask) then
      FMaskIndex := 0;
    Inc(aIndex);
    Inc(fp);
    Inc(tp);
  end;
end;

function TmnWebSocket13StreamProxy.ReadHeader: Boolean;
var
  c, r: LongInt;
  W: Word;
  Q: Int64;
  aSize: Int64;
begin
  Result := Over.Read(Header, SizeOf(Header), c, r);
  if Result then
  begin
    FMaskIndex := 0;
    while True do
    begin
      if c = 0 then //* not timeout
        break
      else
      begin
        if Header.InteralSize = 126 then
        begin
          Over.Read(W, SizeOf(W), c, r);
          aSize := SwapBytes(W);
        end
        else if Header.InteralSize = 127 then
        begin
          Over.Read(Q, SizeOf(Q), c, r);
          aSize := SwapBytes(Q);
        end
        else
          aSize := Header.InteralSize;

        if Header.Masked then
          Over.Read(FMask.Mask, SizeOf(FMask.Mask), c, r)
        else
          FMask.Clear;

        if Header.Opcode in [wsoConitnue, wsoText, wsoBinary] then
        begin
          FSize := aSize;
          Result := True;
          break;
        end
        else
        begin
          Result := False;
          if Header.Opcode = wsoPing then
          begin
            //Send pong
          end
          else if Header.Opcode = wsoClose then
          begin
            CloseTransmission;
            break;
            //Close;
          end;
        end;
      end;
    end;
  end;
end;

constructor TmnWebSocket13StreamProxy.Create(ABinary: Boolean; AMasked: Boolean; AMask: Cardinal);
begin
  inherited Create;
  Binary := ABinary;
  FMasked := AMasked;
  FMask.Key := AMask;
end;

function TmnWebSocket13StreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  aCount: longint;
begin
  ResultCount := 0;
  RealCount := 0;
  Result := True;
  if FSize = 0 then
  begin
    Result := ReadHeader;
    if Result then
    begin
      if FSize = 0 then
      begin
//        CloseData;
      end;
    end;
  end;

  if FSize > 0 then
  begin
    if FSize >= Count then
      aCount := Count
    else
      aCount := FSize;

    Result := Over.Read(Buffer, aCount, ResultCount, RealCount);
    if Header.Masked then
      MaskData(Buffer, Buffer, ResultCount);
    Dec(FSize, ResultCount);
    if FSize = 0 then
    begin
      if Header.Finished then
      begin
        CloseFragment;
      end;
    end;
  end;
end;

function TmnWebSocket13StreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  aHeader: TWebsocketPayloadHeader;
  Q: Int64;
  W: Word;
  aBuffer: Pointer;
begin
  aHeader.Finished := True;
  if Binary then
    aHeader.Opcode := wsoBinary
  else
    aHeader.Opcode := wsoText;

  aHeader.Masked := Masked;

  if Count > 125 then
  begin
    if Count > Word.MaxValue then
      aHeader.InteralSize := 127
    else
      aHeader.InteralSize := 126;
  end
  else
    aHeader.InteralSize := Count;

  Over.Write(aHeader, SizeOf(aHeader), ResultCount, RealCount);
  if aHeader.InteralSize > 125 then
  begin
    if aHeader.InteralSize = 126 then
    begin
      W := SwapBytes(Word(Count));
      Over.Write(W, SizeOf(W), ResultCount, RealCount);
    end
    else if aHeader.InteralSize = 127 then
    begin
      Q := SwapBytes(Int64(Count));
      Over.Write(Q, SizeOf(Q), ResultCount, RealCount);
    end;
  end;
  if Masked then
  begin
    Over.Write(Mask.Mask, SizeOf(Mask.Mask), ResultCount, RealCount);
    GetMem(aBuffer, Count);
    MaskData(Buffer, aBuffer^, Count);
    Result := Over.Write(aBuffer^, Count, ResultCount, RealCount);
    FreeMem(aBuffer);
  end
  else
    Result := Over.Write(Buffer, Count, ResultCount, RealCount);
end;

end.
