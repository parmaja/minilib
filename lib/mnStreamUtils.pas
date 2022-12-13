unit mnStreamUtils;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @ported    Most code of deflate and inflate ported from FPC source zstream, i just wrapped it into my StreamProxy
 *}

{$M+}{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, zlib,
  mnStreams;

type
  TmnCompressLevel = 0..9;
  TmnStreamCompress = set of (cprsRead, cprsWrite);

  TmnCompressStreamProxy = class abstract(TmnStreamOverProxy)
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel = 9); virtual;
    class function GetCompressName: string; virtual; abstract;
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

  const
    DEF_MEM_LEVEL = 8;
    MAX_WBITS = 15;
  protected
    FCompress: TmnStreamCompress;

    procedure InitDeflate;
    procedure InitInflate;
    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel = 9); override;
    destructor Destroy; override;
    procedure CloseDeflate; //close it after finishing compress a stream or partial a stream
    class function GetCompressName: string; override;
    procedure CloseInflate;
    property BufSize: Cardinal read FBufSize write FBufSize;
  end;

  TmnGzipStreamProxy = class(TmnDeflateStreamProxy)
  private
  public
    constructor Create(ACompress: TmnStreamCompress; Level: TmnCompressLevel); override;
    class function GetCompressName: string; override;
  end;

  TmnPlainStreamProxy = class(TmnStreamOverProxy)
    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  end;

  TmnChunkStreamProxy = class(TmnStreamOverProxy)
  protected
    FChunkEnd: Boolean;
    FReadSize: Integer;
    function ChunkSize: Integer;

    function ReadSize: Integer;
    procedure ReadChunkEndOfLine;

    procedure CloseWrite; override;
    procedure CloseRead; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean; override;
  end;

implementation

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
begin
  if cprsRead in FCompress then
  begin
    with InflateInfo do
    if ZEnd then
      ResultCount := 0
    else
    begin
      InitInflate; //init it if not initialized
      ZStream.next_out := @buffer;
      ZStream.avail_out := Count;
      while ZStream.avail_out <> 0 do
      begin
        if ZStream.avail_in = 0 then
        begin
          {Refill the buffer.}
          Over.Read(ZBuffer^, BufSize, HaveRead, RealCount);
          ZStream.next_in := Pointer(ZBuffer);
          ZStream.avail_in := HaveRead;
        end
        else
          RealCount := 0;
        err := inflate(ZStream, Z_NO_FLUSH);
        if err = Z_STREAM_END then
        begin
          ZEnd := True;
          break
        end
        else if err <> Z_OK then
          raise Exception.Create(String(zerror(err)));
      end;
      ResultCount := Count - Integer(ZStream.avail_out);
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
  FChunkEnd := False;
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
  //Result := Over.Read(Buffer, Count, ResultCount, RealCount);
  //Exit;
  ResultCount := 0;
  RealCount := 0;
  //Result := not FChunkEnd;
  Result := True;

  if Result then
  begin
    if FReadSize=0 then
    begin
      FReadSize := ReadSize;
      if FReadSize=0 then
      begin
        //FChunkEnd := True;
        CloseData;
      end;
    end;

    if FReadSize>0 then
    begin
      if FReadSize>=Count then
        aCount := Count
      else
        aCount := FReadSize;

      Over.Read(Buffer, aCount, ResultCount, RealCount);
      Dec(FReadSize, ResultCount);
    end;

    if FReadSize = 0 then
    begin
      ReadChunkEndOfLine;
    end;
  end;
end;

function TmnChunkStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
const
  sEOL = #13#10;

var
  c, i, t: Integer;
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
    if Count>ChunkSize then
      t := ChunkSize
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
      Break;
  end;
end;

procedure TmnChunkStreamProxy.ReadChunkEndOfLine;
var
  b: Byte;
  r, c: LongInt;
begin
  Over.Read(b, 1, r, c); //skip $A
  if b=13 then
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
          Break;
        end;
        else
          s := s + Chr(b);
      end;
    end
    else
      Break;
  end;
end;

function TmnChunkStreamProxy.ChunkSize: Integer;
begin
  Result := 512;
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

end.
