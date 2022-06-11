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
        ZBuffer: pointer;
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

  TmnChunkStreamProxy = class(TmnStreamOverProxy)
  protected
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
            ZStream.next_out := ZBuffer;
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
          ZStream.next_in := ZBuffer;
          Over.Read(Zbuffer^, BufSize, HaveRead, RealCount);
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
          ZStream.next_out := ZBuffer;
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

      ZStream.next_out := ZBuffer;
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

      ZStream.next_in := ZBuffer;
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

end;

procedure TmnChunkStreamProxy.CloseWrite;
begin
  inherited;

end;

function TmnChunkStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
var
  b: Byte;
  r, c: Integer;
  aCount: Integer;
  s: string;
begin
  Result := Over.Read(Buffer, Count, ResultCount, RealCount);
  Exit;

  s := '';
  aCount := 0;
  repeat
    Over.Read(b, 1, r, c);
    case b of
      13, 10:
      begin
        Over.Read(b, 1, r, c);
        aCount := StrToIntDef('$'+s, 0);
        Over.Read(Buffer, aCount, ResultCount, RealCount);

        Over.Read(b, 1, r, c);
        Over.Read(b, 1, r, c);
        b := 0;
      end;
      else
        s := s + Chr(b);
    end;

  until b=0;
end;

function TmnChunkStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: Longint): Boolean;
begin
  Result := Over.Write(Buffer, Count, ResultCount, RealCount);
end;

end.
