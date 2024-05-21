unit mnStreams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$modeswitch multihelpers}
{$ENDIF}

//TODO Change TFileSize to TStreamSize (Longint)

{
  Rules:

    3 type of reading
    Read Buffer
    Read in Loop
    Read Until

    Read Buffer: Should return true when there is data count > 0 even if disconnected/closed(data)
                 If read buffer return count = 0 and not disconnected, return false, but that mean it is timeout, or retry

    Read in Loop: Like read stream, read strings, read string, it loop until stream disconnected/closed(data)

                  Return true if count > 0 even if disconnected
                  Return false of read count = 0 (no data), if not disonnected that meant it is timeout

                  Breaking: no breaking loop when reading count = 0
                            break loop when closed data, or disconnect

    Read Until:   Like Read line, read until

                  Return true if count > 0 even if disconnected
                  Return false of read **real** count = 0 (no data), but return true if read the match, count can be 0 but maybe it is matched, match string will not count, if not disonnected that meant it is timeout or retry

                  Breaking: no breaking loop when reading count = 0
                            break loop when closed data, or disconnect


    if stream read 0 byte but it is not discoonect/closed(data) , it mean `retry` or `timeout`
    if ReadLine(s) return true but s='' that mean it is empty line
    if read count = 0 with disconnect/close after read, the stream reach end of file of disconnected

    my problem is Connected by default is false, but when socket it is True

    Passive streams is always Connected, but when read count =0 that mean disconnect/closed/eof and read buffer should mark it as disconnect
}

interface

uses
  Classes, SysUtils;

const
  cReadTimeout = 15000;
  cWriteTimeout = cReadTimeout div 8;
  cConnectTimeout = cReadTimeout div 4;

  sEndOfLine = #$0A;

  sWinEndOfLine = #$0D#$0A;
  sUnixEndOfLine = #$0A;
  sMacEndOfLine = #$0D;
  sGSEndOfLine = #$1E;

  {$ifdef MSWINDOWS}
  sNativeEndOfLine = sWinEndOfLine;
  {$else}
  sNativeEndOfLine = sUnixEndOfLine;
  {$endif}

type
  TFileSize = Longint;

  TStreamHelper = class helper for TStream
  public
    function WriteBytes(const vData: TBytes): TFileSize;
    function WriteString(const vData: string): TFileSize; overload;
    function WriteString(const vData: string; vUTF8: Boolean): TFileSize; overload;
    function WriteUtf8(const vData: UTF8String): TFileSize; deprecated;
    function WriteUTF8String(const vData: UTF8String): TFileSize; inline;
  end;

  TmnStreamClose = set of (
    cloRead, //Mark is as EOF
    cloData, //Mark is as end of data, Boundary Data or End of Part of Multipart
    cloWrite //Flush buffer
  );

  EmnStreamException = class(Exception);
  EmnStreamExceptionAbort = class(Exception); //can be ignored by ide
  TmnBufferStream = class;

  { TmnCustomStream }

  TmnCustomStream = class abstract(TStream)
  private
    FDone: TmnStreamClose;
    FPassive: Boolean;
  protected
    //If Passive like Wrapper to FileStream reading count=0 meant  close, Passive have be in Socket too if you want break reading on timeout
    property Passive: Boolean read FPassive; //Break on timeout(reading count=0)
    function GetConnected: Boolean; virtual; abstract; //Socket or COM ports have Connected and override
    function CanRead: Boolean; {$ifndef DEBUG}inline;{$endif}
    function CanWrite: Boolean; inline;
    procedure SetCloseData;
    procedure ResetCloseData;
  public
    //Count = 0 , load until eof, timeout not break the loop
    function ReadStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;

    function ReadString(out s: string; Count: TFileSize = 0): Boolean; overload; deprecated;
    function ReadUTF8String(out s: UTF8String; Count: TFileSize = 0): Boolean; overload;
    function ReadUTF8String(out s: string; Count: TFileSize = 0): Boolean; overload;
    function ReadBytes(Count: TFileSize = 0): TBytes; overload;

    //Use copy from to stream
    function WriteStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;

    function CopyToStream(AStream: TStream; Count: TFileSize = 0): TFileSize; inline; //alias
    function CopyFromStream(AStream: TStream; Count: TFileSize = 0): TFileSize; inline;

    property Connected: Boolean read GetConnected;
    property Done: TmnStreamClose read FDone;

    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
  end;

  TmnStreamOverProxy = class;

  { TmnStreamProxy }

  TmnStreamProxy = class abstract(TObject)
  private
    FOver: TmnStreamProxy;
    FEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure CloseReadAll; virtual;
    procedure CloseWriteAll; virtual;
    procedure FlushAll; virtual;
  protected
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual; abstract;
    procedure Flush; virtual;
    procedure CloseRead; virtual; abstract;
    procedure CloseWrite; virtual; abstract;
    procedure CloseData; virtual;
    property Over: TmnStreamProxy read FOver;
  public
    //RealCount passed to Original Stream to retrive the real size of write or read, do not assign or modifiy it
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual;
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual;
    procedure Enable;
    procedure Disable;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  { TmnStreamOverProxy }

  TmnStreamOverProxy = class abstract(TmnStreamProxy)
  private
    procedure CloseReadAll; override; final;
    procedure CloseWriteAll; override; final;
    procedure FlushAll; override; final;
  protected
    procedure CloseRead; override;
    procedure CloseWrite; override;
    procedure Flush; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
  public
    //Inhrite it please
    constructor Create;
    function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override; final;
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override; final;
  end;

  { TmnReadWriteBuffer }

  TmnReadWriteBuffer = class(TObject)
  protected
    function DoRead(var vBuffer; vCount: Longint): Longint; virtual;
    function DoWrite(const vBuffer; vCount: Longint): Longint; virtual;
  public
    Buffer: PByte;
    Pos: PByte;
    Stop: PByte;
    Size: Longint;
    Stream: TmnBufferStream;
    constructor Create(vStream: TmnBufferStream);
    destructor Destroy; override;

    procedure CreateBuffer;
    procedure FreeBuffer;
    function Count: Cardinal;
    function LoadBuffer: TFileSize;
    function CheckBuffer: Boolean;
    function Read(var vBuffer; vCount: Longint): Longint;
    function Write(const vBuffer; vCount: Longint): Longint;
  end;

  { TmnInitialStreamProxy }

  TmnInitialStreamProxy = class sealed(TmnStreamProxy)
  private
  protected
    FStream: TmnBufferStream;

    procedure CloseRead; override;
    procedure CloseWrite; override;
    procedure CloseData; override;

    procedure Flush; override;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
  public
    constructor Create(AStream: TmnBufferStream);
  end;

  TReadUntilCallback = procedure(vData: TObject; const Buffer; Count: Longint) of object;

  { TmnStreamControl }

  TmnStreamControl = class(TObject)
  private
    FStream :TmnBufferStream;
  public
    procedure Writing(Count: Longint); virtual;
    procedure Reading(Count: Longint); virtual;
    destructor Destroy; override;
  end;

  { TmnBufferStream }

  TmnBufferStream = class abstract(TmnCustomStream)
  protected
    FReadBuffer: TmnReadWriteBuffer;
    FWriteBuffer: TmnReadWriteBuffer;
    FControl: TmnStreamControl;
  strict private
    FEndOfLine: string;
    //procedure SaveWriteBuffer; //kinda flush
  protected

  private
    FTimeoutTries: integer;

    procedure SetReadBufferSize(AValue: TFileSize);
    procedure SetWriteBufferSize(AValue: TFileSize);
    function GetBufferSize: TFileSize;
    function GetReadBufferSize: TFileSize;
    function GetWriteBufferSize: TFileSize;

  private
    function ReadBuffer(var Buffer; Count: Longint; var ResultCount: Longint): Boolean;
    procedure SetControl(const AValue: TmnStreamControl);
    function WriteBuffer(const Buffer; Count: Longint; var ResultCount: Longint): Boolean;
  protected
    FProxy: TmnStreamProxy;

    procedure ReadError; virtual;
    //Override it but do not use it in your code, use ProxyRead or ProxyWrite
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;
    procedure DoFlush; virtual;
    procedure DoCloseRead; virtual;
    procedure DoCloseWrite; virtual;

    property Proxy: TmnStreamProxy read FProxy;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    procedure AddProxy(AProxy: TmnStreamOverProxy);

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;
    procedure Flush;

    procedure Close(ACloseWhat: TmnStreamClose = [cloRead, cloWrite]);

    //* ABuffer is created you need to free it
    //* If read count = 0 but still connected we consider it as timeout
    function ReadUntilCallback(vData: TObject; const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; Callback: TReadUntilCallback; out Matched: Boolean): Boolean;
    function ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: PByte; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;
    {$ifndef NEXTGEN}
    function ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; out Matched: Boolean): Boolean; overload;
    {$endif}

    function ReadLine(out s: UTF8String; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLine(out s: unicodestring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLine(ExcludeEOL: Boolean = True): string; overload; deprecated;
    function ReadUTF8Line(out s: UTF8String; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadUTF8Line(out s: string; ExcludeEOL: Boolean = True): Boolean; overload;
    //TODO ReadLineUTF8 to ReadUTF8Line
    function ReadLineUTF8(out s: UTF8String; ExcludeEOL: Boolean = True): Boolean; overload; deprecated;
    function ReadLineUTF8(out s: string; ExcludeEOL: Boolean = True): Boolean; overload; deprecated;
    function ReadLineUTF8(ExcludeEOL: Boolean = True): UTF8String; overload; deprecated;

    function ReadLineRawByte(out s: rawbytestring; ExcludeEOL: Boolean = True): Boolean; overload;

    {$ifndef NEXTGEN}
    function ReadLine(out s: ansistring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLine(out s: widestring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadAnsiString(vCount: Integer): AnsiString;
    {$endif}

    function WriteLine: TFileSize; overload;
    function WriteLine(const s: UTF8String): TFileSize; overload;
    function WriteLine(const s: unicodestring): TFileSize; overload;

    function WriteRawByte(const s: UTF8String): TFileSize; overload;
    function WriteUTF8Line(const s: UTF8String): TFileSize; overload;
    function WriteLineUTF8(const s: UTF8String): TFileSize; overload; deprecated;
    function WriteLineUTF8(const UTF8Bytes: TBytes): TFileSize; overload; //bytes encoded utf8  //* what the hell that <---
    {$ifndef FPC}
    function WriteLineUTF8(const s: string): TFileSize; overload; deprecated;
    function WriteUTF8Line(const s: string): TFileSize; overload;
    {$endif}

    {$ifndef NEXTGEN}
    function WriteAnsiString(const s: ansistring): TFileSize; overload;
    function WriteLineAnsiString(const s: ansistring): TFileSize; overload;
    function WriteLine(const s: ansistring): TFileSize; overload;
    function WriteLine(const s: widestring): TFileSize; overload;
    {$endif}

    function ReadBytes(vCount: Integer): TBytes;
    function WriteBytes(Buffer: TBytes): Longint;

    function ReadCommand(out Command: string; out Params: string): Boolean;

    procedure WriteCommand(const Command: string; const Format: string; const Params: array of const); overload;
    procedure WriteCommand(const Command: string; const Params: string = ''); overload;

    procedure ReadUTF8Strings(Value: TStrings); overload;
    procedure ReadStrings(Value: TStrings); overload;
    function WriteUTF8Strings(const Value: TStrings): TFileSize; overload;
    function WriteStrings(const Value: TStrings): TFileSize; overload;

    property EndOfLine: string read FEndOfLine write FEndOfLine;
    property TimeoutTries: integer read FTimeoutTries write FTimeoutTries;

    property BufferSize: TFileSize read GetBufferSize write SetReadBufferSize; //deprecated;
    property ReadBufferSize: TFileSize read GetReadBufferSize write SetReadBufferSize;
    property WriteBufferSize: TFileSize read GetWriteBufferSize write SetWriteBufferSize; //TODO not yet

    property Control: TmnStreamControl read FControl write SetControl;
  end;

  { TmnWrapperStream }

  TmnWrapperStream = class(TmnBufferStream)
  strict private
    FStreamOwned: Boolean;
    FEndOfStream: Boolean;
    FStream: TStream;
    procedure SetStream(const Value: TStream);
  protected
    function GetConnected: Boolean; override;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
  public
    constructor Create(AStream: TStream; AEndOfLine: string; Owned: Boolean = True); overload; virtual;
    constructor Create(AStream: TStream; Owned: Boolean = True); overload;
    destructor Destroy; override;
    property StreamOwned: Boolean read FStreamOwned;
    property EndOfStream: Boolean read FEndOfStream;
    property Stream: TStream read FStream write SetStream;
  end;

  TmnWrapperStreamClass = class of TmnWrapperStream;

  { TmnConnectionStream }

  TmnConnectionError = (cerSuccess, cerTimeout, cerError);

  TmnConnectionStream = class abstract(TmnBufferStream)
  private
    FReadTimeout: Integer;
    FWriteTimeout: Integer;
    FConnectTimeout: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Prepare; virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;

    function WaitToRead(Timeout: Longint): TmnConnectionError; overload; virtual; abstract;
    function WaitToWrite(Timeout: Longint): TmnConnectionError; overload; virtual; abstract;
    function WaitToRead: Boolean; overload;
    function WaitToWrite: Boolean; overload;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout; //not here

  end;

  { TmnStreamHexProxy }

  TmnHexStreamProxy = class(TmnStreamOverProxy)
  private
  protected
    type
      TmnMethodProxyEncode = (
        mpeEncode,
        mpeDecode
      );
  protected
    function HexDecode(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
    function HexEncode(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
    function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
  public

    Method: TmnMethodProxyEncode;
  end;

var
  ReadWriteBufferSize: Integer = 1024;

implementation

function TStreamHelper.WriteUtf8(const vData: UTF8String): TFileSize;
begin
  if vData<>'' then
    Result := Write(PByte(vData)^, Length(vData))
  else
    Result := 0;
end;

function TStreamHelper.WriteUTF8String(const vData: UTF8String): TFileSize;
begin
  if vData<>'' then
    Result := Write(PByte(vData)^, Length(vData))
  else
    Result := 0;
end;

procedure CopyUTF8String(out s: UTF8String; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}s := '';{$endif}
    SetLength(s, Len);
    Move(PByte(Buffer)^, PByte(s)^, Len);
  end
  else
    s := '';
end;

procedure CopyRawByteString(out s: RawByteString; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}s := '';{$endif}
    SetLength(s, Len);
    Move(PByte(Buffer)^, PByte(s)^, Len);
  end
  else
    s := '';
end;

procedure CopyUnicodeString(out s: unicodestring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}s := '';{$endif}
    {$ifdef FPC}
    SetLength(s, Len div SizeOf(unicodechar));
    {$else}
    SetLength(s, Len div SizeOf(char));
    {$endif}
    Move(PByte(Buffer)^, PByte(s)^, Len);
  end
  else
    s := '';
end;

{$ifndef NEXTGEN}
procedure CopyAnsiString(out s: ansistring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}s := '';{$endif}
    SetLength(s, Len div SizeOf(ansichar));
    Move(PByte(Buffer)^, PByte(s)^, Len);
  end
  else
    s := '';
end;

procedure CopyWideString(out s: widestring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}s := '';{$endif}
    SetLength(s, Len div SizeOf(widechar));
    Move(PByte(Buffer)^, PByte(s)^, Len);
  end
  else
    s := '';
end;
{$endif}

function ByteLength(s: unicodestring): TFileSize; overload;
begin
{$ifdef FPC}
  Result := Length(s) * SizeOf(UnicodeChar);
{$else}
  Result := Length(s) * SizeOf(WideChar);
{$endif}
end;

{$ifndef NEXTGEN}
function ByteLength(s: ansistring): TFileSize; overload;
begin
  Result := Length(s) * SizeOf(AnsiChar);
end;

function ByteLength(s: utf8string): TFileSize; overload;
begin
  Result := Length(s) * SizeOf(AnsiChar);
end;

function ByteLength(s: widestring): TFileSize; overload;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

{$endif}

{ TStreamHelper }

function TStreamHelper.WriteBytes(const vData: TBytes): TFileSize;
begin
  if Length(vData)<>0 then
    Result := Write(vData[0], Length(vData))
  else
    Result := 0;
end;

function TStreamHelper.WriteString(const vData: string): TFileSize;
begin
  if vData<>'' then
    Result := Write(PByte(vData)^, mnStreams.ByteLength(vData))
  else
    Result := 0;
end;

function TStreamHelper.WriteString(const vData: string; vUTF8: Boolean): TFileSize;
begin
  if vData<>'' then
  begin
    if vUTF8 then
      Result := WriteBytes(TEncoding.UTF8.GetBytes(vData))
    else
      Result := WriteString(vData)
  end
  else
    Result := 0;
end;

{ TmnStreamProxy }

procedure TmnStreamProxy.SetEnabled(AValue: Boolean);
begin
  if FEnabled =AValue then
    Exit;
  FEnabled := AValue;
  if not FEnabled then
  begin
    CloseRead;
    CloseWrite;
  end;
end;

procedure TmnStreamProxy.CloseData;
begin
  if FOver <> nil then
    Over.CloseData;
end;

procedure TmnStreamProxy.CloseReadAll;
begin
  CloseRead;
end;

procedure TmnStreamProxy.CloseWriteAll;
begin
  CloseWrite;
end;

procedure TmnStreamProxy.FlushAll;
begin
  Flush;
end;

procedure TmnStreamProxy.Flush;
begin
end;

function TmnStreamProxy.Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := DoRead(Buffer, Count, ResultCount, RealCount);
end;

function TmnStreamProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := DoWrite(Buffer, Count, ResultCount, RealCount);
end;

destructor TmnStreamProxy.Destroy;
begin
  if FOver <> nil then
    FreeAndNil(FOver);
  inherited;
end;

procedure TmnStreamProxy.Enable;
begin
  if Self = nil then
    raise EmnStreamException.Create('No Stream exists yet');
  Enabled := True;
end;

procedure TmnStreamProxy.Disable;
begin
  if Self <> nil then
    Enabled := False;
end;

{ TmnStreamOverProxy }

function TmnStreamOverProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FOver.Read(Buffer, Count, ResultCount, RealCount);
end;

function TmnStreamOverProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FOver.Write(Buffer, Count, ResultCount, RealCount);
end;

function TmnStreamOverProxy.Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  if Enabled then
    Result := inherited Read(Buffer, Count, ResultCount, RealCount)
  else
    Result := FOver.Read(Buffer, Count, ResultCount, RealCount);
end;

function TmnStreamOverProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  if Enabled then
    Result := inherited Write(Buffer, Count, ResultCount, RealCount)
  else
    Result := FOver.Write(Buffer, Count, ResultCount, RealCount)
end;

procedure TmnStreamOverProxy.CloseReadAll;
begin
  inherited;
  if FOver <> nil then
    FOver.CloseReadAll;
end;

procedure TmnStreamOverProxy.CloseWriteAll;
begin
  inherited;
  if FOver <> nil then
    FOver.CloseWriteAll;
end;

procedure TmnStreamOverProxy.FlushAll;
begin
  if FOver <> nil then
    FOver.FlushAll;
end;

procedure TmnStreamOverProxy.CloseRead;
begin
end;

procedure TmnStreamOverProxy.CloseWrite;
begin
end;

procedure TmnStreamOverProxy.Flush;
begin
end;

constructor TmnStreamOverProxy.Create;
begin
  inherited Create;
  FEnabled := True;
end;

{ TmnConnectionStream }

constructor TmnConnectionStream.Create;
begin
  inherited Create;
  FReadTimeout := cReadTimeout;
  FWriteTimeout := cWriteTimeout;
  FConnectTimeout := cConnectTimeout;
end;

destructor TmnConnectionStream.Destroy;
begin
  inherited;
end;

procedure TmnConnectionStream.Prepare;
begin
end;

procedure TmnConnectionStream.Connect;
begin
end;

procedure TmnConnectionStream.Disconnect;
begin
end;

function TmnConnectionStream.WaitToRead: Boolean;
begin
  if Connected then
    Result := WaitToRead(ReadTimeout) = cerSuccess
  else
    Result := False;
end;

function TmnConnectionStream.WaitToWrite: Boolean;
begin
  Result := WaitToWrite(WriteTimeout) = cerSuccess;
end;

function TmnConnectionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if TSeekOrigin(Origin) = soCurrent then
    Result := 0
  else
    raise Exception.Create('not supported and we dont want to support it')
end;

{ TmnBufferStream }

function TmnCustomStream.ReadString(out s: string; Count: TFileSize): Boolean;
var
  aBuffer: PByte;
  p: Integer;
  l, c, aSize: Integer;
begin
  //try use pointer stream
  s := '';
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    p := 0;
    while Connected do
    begin
      if (Count > 0) and (aSize < ReadWriteBufferSize) then
        l := aSize
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        SetLength(s, p + c);
        Move(aBuffer^, (PByte(s) + p)^, c);
        p := p + c;
      end;
      if ((c = 0) and Passive) or ((c=0) and not Connected) or not CanRead or ((Count > 0) and (aSize = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
  Result := s <> '';
end;

function TmnCustomStream.ReadUTF8String(out s: string; Count: TFileSize): Boolean;
var
  u8: UTF8String;
begin
  Result := ReadUTF8String(u8);
  s := UTF8ToString(u8);
end;

function TmnCustomStream.ReadUTF8String(out s: UTF8String; Count: TFileSize): Boolean;
var
  aBuffer: PByte;
  p: Integer;
  l, c, aSize: Integer;
begin
  //try use pointer stream
  s := '';
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    p := 0;
    while Connected do
    begin
      if (Count > 0) and (aSize < ReadWriteBufferSize) then
        l := aSize
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        SetLength(s, p + c);
        Move(aBuffer^, (PByte(s) + p)^, c);
        p := p + c;
      end;
      if ((c = 0) and Passive) or ((c=0) and not Connected) or not CanRead or ((Count > 0) and (aSize = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
  Result := s <> '';
end;

function TmnCustomStream.CopyToStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: Integer;
begin
  Result := ReadStream(AStream, Count, RealCount);
end;

function TmnCustomStream.ReadStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: Integer;
begin
  Result := ReadStream(AStream, Count, RealCount);
end;

function TmnCustomStream.CanRead: Boolean;
begin
  Result := not (cloData in Done) and not (cloRead in Done);
end;

function TmnCustomStream.CanWrite: Boolean;
begin
  Result := not (cloData in Done) and not (cloWrite in Done);
end;

procedure TmnCustomStream.ResetCloseData;
begin
  FDone := FDone - [cloData];
end;

procedure TmnCustomStream.SetCloseData;
begin
  FDone := FDone + [cloData];
end;

function TmnCustomStream.CopyFromStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: Integer;
begin
  Result := WriteStream(AStream, Count, RealCount);
end;

function TmnCustomStream.ReadBytes(Count: TFileSize): TBytes;
var
  aBuffer: PByte;
  p: Integer;
  l, c, aSize: Integer;
begin
  //try use pointer stream
  Result := nil;
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    p := 0;
    while Connected do
    begin
      if (Count > 0) and (aSize < ReadWriteBufferSize) then
        l := aSize
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        SetLength(Result, p + c);
        Move(aBuffer^, Result[p], c);
        p := p + c;
      end;
      if ((c = 0) and Passive) or ((c=0) and not Connected) or not CanRead or ((Count > 0) and (aSize = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.Read(var Buffer; Count: longint): longint;
begin
  ResetCloseData;
  Result := inherited Read(Buffer, Count);
end;

function TmnCustomStream.ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, aSize: Integer;
begin
  Result := 0;
  RealCount := 0;
  if not Connected or (Count=0) then
    exit(0);

  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    while True do
    begin
      if (Count > 0) and (aSize < ReadWriteBufferSize) then
        l := aSize
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        Result := Result + c;
        RealCount := RealCount + AStream.Write(aBuffer^, c);
      end;
      //(c = 0) and Passive //static file no disconnect
      //(c = 0) and not Connected //(c<>0) data in tcp buffer and server disconnected
      //not CanRead //close read for any reason
      //(Count > 0) and (aSize = 0) //we finsih count

      if ((c = 0) and Passive) or ((c = 0) and not Connected) or not CanRead or ((Count > 0) and (aSize = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.WriteStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: Integer;
begin
  Result := WriteStream(AStream, Count, RealCount);
end;

function TmnCustomStream.Write(const Buffer; Count: longint): longint;
begin
  //ResetCloseData;
  Result := inherited Write(Buffer, Count);
end;

function TmnCustomStream.WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, aSize, w: Integer;
begin
  Result := 0;
  RealCount := 0;
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    //while CanWrite do //not same as read :)
    while True do
    begin
      if (Count = 0) or (aSize > ReadWriteBufferSize) then
        l := ReadWriteBufferSize
      else
        l := aSize;

      c := AStream.Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        Result := Result + c;
        w := Write(aBuffer^, c);

        if w<>c then //c=0 or error occurred :)
        begin
          if not Connected then
            Break;
        end;

        RealCount := RealCount + w;
      end;
      if ((Count > 0) and (aSize = 0)) then //we finsih count
        break;
      if (c = 0) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

{$ifndef NEXTGEN}
function TmnBufferStream.WriteLineAnsiString(const s: ansistring): TFileSize;
var
  EOL: ansistring;
begin
  if s <> '' then
    Result := Write(Pointer(s)^, Length(s))
  else
    Result := 0;
  if EndOfLine <> '' then
  begin
    EOL := AnsiString(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, Length(EOL));
  end;
end;

function TmnBufferStream.WriteLineUTF8(const s: UTF8String): TFileSize;
begin
  Result := WriteUTF8Line(s);
end;

{$ifndef FPC}
function TmnBufferStream.WriteLineUTF8(const s: string): TFileSize;
begin
  Result := WriteUTF8Line(UTF8Encode(s));
end;

function TmnBufferStream.WriteUTF8Line(const s: string): TFileSize;
begin
  Result := WriteUTF8Line(UTF8Encode(s));
end;
{$endif}

function TmnBufferStream.WriteLineUTF8(const UTF8Bytes: TBytes): TFileSize;
begin
  if Length(Utf8Bytes)<>0 then
    Result := Write(Pointer(Utf8Bytes)^, Length(Utf8Bytes))
  else
    Result := 0;

  if EndOfLine <> '' then
  begin
    Result := Result + WriteUTF8String(UTF8String(EndOfLine));
  end;
end;

function TmnBufferStream.WriteLine(const s: ansistring): TFileSize;
begin
  Result := WriteLineAnsiString(s);
end;

function TmnBufferStream.WriteLine(const s: widestring): TFileSize;
var
  EOL: widestring;
begin
  if s <> '' then
    Result := Write(Pointer(s)^, ByteLength(s))
  else
    Result := 0;
  if EndOfLine <> '' then
  begin
    EOL := widestring(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
  end;
end;

{$endif}

function TmnBufferStream.WriteUTF8Line(const s: UTF8String): TFileSize;
begin
  if EndOfLine <> '' then
    Result := WriteUTF8String(s + UTF8String(EndOfLine))
  else
    Result := WriteUTF8String(s);
end;

function TmnBufferStream.WriteAnsiString(const s: ansistring): TFileSize;
begin
  if s <> '' then
    Result := Write(Pointer(s)^, Length(s))
  else
    Result := 0;
end;

function TmnBufferStream.WriteLine(const s: unicodestring): TFileSize;
var
  EOL: unicodestring;
begin
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(s)^, mnStreams.ByteLength(s));
  if EndOfLine <> '' then
  begin
    EOL := unicodestring(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, mnStreams.ByteLength(EOL));
  end;
end;

function TmnBufferStream.WriteRawByte(const s: UTF8String): TFileSize;
begin
  if s <> '' then
    Result := Write(Pointer(s)^, Length(s))
  else
    Result := 0;
end;

function TmnBufferStream.WriteLine(const s: UTF8String): TFileSize;
var
  EOL: UTF8String;
begin
  Result := 0;
  if s <> '' then
    Result := Write(PByte(s)^, mnStreams.ByteLength(s));
  if EndOfLine <> '' then
  begin
    EOL := UTF8String(EndOfLine);
    Result := Result + Write(PByte(EOL)^, mnStreams.ByteLength(EOL));
  end;
end;

function TmnBufferStream.WriteBuffer(const Buffer; Count: Longint; var ResultCount: Longint): Boolean;
begin
  ResultCount := FReadBuffer.Write(Buffer, Count);
  Result := (ResultCount <> 0) and Connected;
end;

function TmnBufferStream.WriteStrings(const Value: TStrings): TFileSize;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Value.Count - 1 do
  begin
    if (i < Value.Count - 1) or (Value[i] <> '') then //stupid delphi always add empty line in last of TStringList
      Result := Result + WriteLine(Value[i]);
  end;
end;

function TmnBufferStream.WriteUTF8Strings(const Value: TStrings): TFileSize;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Value.Count - 1 do
  begin
    if (i < Value.Count - 1) or (Value[i] <> '') then //stupid delphi always add empty line in last of TStringList
      Result := Result + WriteUTF8Line(Value[i]);
  end;
end;

function TmnBufferStream.ReadCommand(out Command: string; out Params: string): Boolean;
var
  s: string;
  p: Integer;
begin
  Result := ReadLine(s);
  if Result then
  begin
    p := Pos(' ', s);
    if p > 0 then
    begin
      Command := Copy(s, 1, p - 1);
      Params := Copy(s, p + 1, MaxInt);
    end
    else
    begin
      Command := s;
      Params := '';
    end;
  end
  else
  begin
    Command := '';
    Params := '';
  end;
end;

procedure TmnBufferStream.WriteCommand(const Command: string; const Format: string; const Params: array of const);
begin
  WriteCommand(Command, SysUtils.Format(Format, Params));
end;

procedure TmnBufferStream.WriteCommand(const Command: string; const Params: string);
begin
  if Params <> '' then
    WriteLine(Command + ' ' + Params)
  else
    WriteLine(Command);
end;

function TmnBufferStream.ReadLine(out s: unicodestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  EOL: unicodestring;
begin
  EOL := unicodestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], mnStreams.ByteLength(eol), ExcludeEOL, res, len, m);
  {$ifdef FPC}
  CopyUnicodeString(s, PUnicodeChar(res), len);
  {$else}
  {$ifdef NEXTGEN}
  CopyUnicodeString(s, PChar(res), len);
  {$else}
  CopyUnicodeString(s, PWideChar(res), len); //TODO check if it widechar
  {$endif}
  {$endif}
  FreeMem(res);
end;

{$ifndef NEXTGEN}
function TmnBufferStream.ReadLine(out s: widestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  EOL: widestring;
begin
  EOL := widestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyWideString(s, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(out s: ansistring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  EOL: ansistring;
begin
  EOL := ansistring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyAnsiString(s, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadAnsiString(vCount: Integer): AnsiString;
begin
  {$ifdef FPC}Result := '';{$endif}
  SetLength(Result, vCount);
  Read(PAnsichar(Result)^, vCount);
end;
{$endif}

function TmnBufferStream.ReadLine(out s: UTF8String; ExcludeEOL: Boolean): Boolean;
begin
  Result := ReadUTF8Line(s, ExcludeEOL);
end;

function TmnBufferStream.ReadLineRawByte(out s: rawbytestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  EOL: RawByteString;
begin
  EOL := RawByteString(EndOfLine);
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  CopyRawByteString(s, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(ExcludeEOL: Boolean): string;
begin
  ReadLine(Result, ExcludeEOL);
end;

{function TmnBufferStream.ReadLn: string;
begin
  ReadLine(Result);
end;}

function TmnBufferStream.ReadLineUTF8(ExcludeEOL: Boolean): UTF8String;
begin
  ReadUTF8Line(Result, ExcludeEOL);
end;

function TmnBufferStream.ReadLineUTF8(out s: UTF8String; ExcludeEOL: Boolean): Boolean;
begin
  Result := ReadUTF8Line(s, ExcludeEOL);
end;

function TmnBufferStream.ReadLineUTF8(out s: string; ExcludeEOL: Boolean): Boolean;
var
  u8: UTF8String;
begin
  Result := ReadUTF8Line(u8, ExcludeEOL);
  s := UTF8ToString(u8);
end;

function TmnBufferStream.ReadUTF8Line(out s: string; ExcludeEOL: Boolean): Boolean;
var
  u8: UTF8String;
begin
  Result := ReadUTF8Line(u8, ExcludeEOL);
  s := UTF8ToString(u8);
end;

function TmnBufferStream.WriteLine: TFileSize;
begin
  if EndOfLine <> '' then
    Result := Write(Pointer(EndOfLine)^, mnStreams.ByteLength(EndOfLine))
  else
    Result := 0;
end;

procedure TmnBufferStream.ReadStrings(Value: TStrings);
var
  s: string;
begin
  while Connected and CanRead do
  begin
    if ReadLine(s) then
      Value.Add(s);
  end;
end;

function TmnBufferStream.Write(const Buffer; Count: Longint): Longint;
var
  RealCount: longint;
begin
  if FProxy <> nil then
    FProxy.Write(Buffer, Count, Result, RealCount)
  else
    WriteBuffer(Buffer, Count, Result);
end;

procedure TmnBufferStream.Flush;
begin
  if FProxy <> nil then
    FProxy.FlushAll
  else
    DoFlush;
end;

procedure TmnBufferStream.Close(ACloseWhat: TmnStreamClose);
begin
  if not (cloRead in Done) and (cloRead in ACloseWhat) then
  begin
    if FProxy <> nil then
      FProxy.CloseReadAll
    else
      DoCloseRead;
    FDone := FDone + [cloRead];
  end;

  if not (cloWrite in Done) and (cloWrite in ACloseWhat) then
  begin
    if FProxy <> nil then
      FProxy.CloseWriteAll
    else
      DoCloseWrite;
    FDone := FDone + [cloWrite];
  end;
end;

{ TmnBufferStream }

destructor TmnBufferStream.Destroy;
begin
  Close;
  FreeAndNil(FProxy);
  FreeAndNil(FReadBuffer);
  FreeAndNil(FWriteBuffer);

  inherited;
end;

procedure TmnBufferStream.AddProxy(AProxy: TmnStreamOverProxy);
begin
  if FProxy = nil then
  begin
    FProxy := TmnInitialStreamProxy.Create(Self);
  end;

  AProxy.FOver := FProxy;
  FProxy := AProxy;
end;

constructor TmnBufferStream.Create(AEndOfLine: string);
begin
  inherited Create;
  FReadBuffer := TmnReadWriteBuffer.Create(Self);
  FWriteBuffer := TmnReadWriteBuffer.Create(Self);


  FReadBuffer.Size := ReadWriteBufferSize;
  FWriteBuffer.Size := 0;
  FEndOfLine := AEndOfLine;
end;

{procedure TmnBufferStream.SaveWriteBuffer;
var
  aSize: TFileSize;
begin
  aSize := ProxyRead(FWriteBuffer.Buffer, FWriteBuffer.Stop - FWriteBuffer.Buffer);
  FWriteBuffer.Pos := FWriteBuffer.Buffer;
  FWriteBuffer.Stop := FWriteBuffer.Buffer;
end;}

procedure TmnBufferStream.ReadError;
begin
  Close([cloRead]);
end;

procedure TmnBufferStream.DoFlush;
begin
end;

procedure TmnBufferStream.DoCloseRead;
begin

end;

procedure TmnBufferStream.DoCloseWrite;
begin

end;

function TmnBufferStream.GetBufferSize: TFileSize;
begin
  Result := FReadBuffer.Size;
end;

function TmnBufferStream.GetReadBufferSize: TFileSize;
begin
  Result := FReadBuffer.Size;
end;

function TmnBufferStream.GetWriteBufferSize: TFileSize;
begin
  Result := FWriteBuffer.Size;
end;

function TmnBufferStream.Read(var Buffer; Count: Longint): Longint;
var
  RealCount: longint;
begin
  ResetCloseData;
  Flush;//Flush write buffer
  if FProxy <> nil then
    FProxy.Read(Buffer, Count, Result, RealCount)
  else
    ReadBuffer(Buffer, Count, Result);

  //Count = 0, Connected = True = timeout, not connected = stream closed
  if (Result <= 0) and not Connected then
    Close([cloRead]);
end;

procedure TmnBufferStream.SetReadBufferSize(AValue: TFileSize);
begin
  if FReadBuffer.Size = AValue then
    Exit;
  if FReadBuffer.Buffer <> nil then
    raise Exception.Create('Change ReadBufferSize before using stream');
  FReadBuffer.Size := AValue;
end;

procedure TmnBufferStream.SetWriteBufferSize(AValue: TFileSize);
begin
  if FWriteBuffer.Size = AValue then
    Exit;
  if FWriteBuffer.Buffer <> nil then
    raise Exception.Create('change Write BufferSize before using stream');
  FWriteBuffer.Size := AValue;
end;

function TmnBufferStream.ReadBuffer(var Buffer; Count: Longint; var ResultCount: Longint): Boolean;
begin
  ResultCount := FReadBuffer.Read(Buffer, Count);
  Result := (ResultCount <> 0) and Connected;
end;

procedure TmnBufferStream.SetControl(const AValue: TmnStreamControl);
begin
  if FControl = AValue then
	  Exit;
  if FControl <> nil then
    FControl.FStream := nil;
  FControl := AValue;
  if FControl <> nil then
    FControl.FStream := Self;
end;

function TmnBufferStream.ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: PByte; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;
var
  aCount: Integer;

  function _IsMatching(vBI, vMI: Integer; out vErr: Boolean): Boolean;
  var
    b: Byte;
    t: PByte;
  begin
    if vBI >= aCount then
    begin
      vErr := (Read(b, 1) <> 1);

      if not vErr then
      begin
        if aCount = ABufferSize then
        begin
          ABufferSize := ABufferSize + ReadWriteBufferSize; { TODO : change ReadWriteBufferSize->FReadBuffer.Size }
          ReallocMem(ABuffer, ABufferSize);
        end;
        t := ABuffer;
        Inc(t, aCount);
        t^ := b;
        Inc(aCount);
      end;
    end
    else
      vErr := False;

    if not vErr then
      Result := ABuffer[vBI] = Match[vMI]
    else
      Result := False;
  end;

var
  Index: Integer;
  MatchIndex: Integer;
  aErr: Boolean;
begin
  if (Match = nil) or (MatchSize = 0) then
    raise Exception.Create('Match is empty!');

  Result := not (cloRead in Done);
  Matched := False;

  ABuffer := nil;
  ABufferSize := 0;
  aCount := 0;
  Index := 0;
  MatchIndex := 0;

  while not Matched do
  begin
    if _IsMatching(Index + MatchIndex, MatchIndex, aErr) then
    begin
      Inc(MatchIndex);
      if MatchIndex = MatchSize then
        Matched := True;
    end
    else
    begin
      MatchIndex := 0;
      Inc(Index);
      if aErr then
        Break;
      if not CanRead then
        Break;
    end;
  end;

  if ExcludeMatch and Matched then
    aCount := aCount - MatchSize;

  ReAllocMem(ABuffer, aCount);
  ABufferSize := aCount;

  //if matched but size=0 that mean we have data but it is 0, because we execluded the EOL
  if not Matched and (ABufferSize = 0) then //(cloRead in Done) and
    Result := False;
end;

function TmnBufferStream.ReadBytes(vCount: Integer): TBytes;
var
  aCount: Integer;
begin
  {$ifdef FPC}
  Result := nil;
  {$endif}
  SetLength(Result, vCount);
  aCount := Read(PByte(Result)^, vCount);
  SetLength(Result, aCount);
end;

function TmnBufferStream.WriteBytes(Buffer: TBytes): Longint;
begin
  Result := Write(Pointer(Buffer)^, Length(Buffer));
end;

{$ifndef NEXTGEN}
function TmnBufferStream.ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean;
var
  Res: PByte;
  len: TFileSize;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  CopyAnsiString(Buffer, Res, Len);
  FreeMem(Res);
end;

function TmnBufferStream.ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; out Matched: Boolean): Boolean;
var
  Res: PByte;
  len: TFileSize;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  CopyWideString(Buffer, Res, Len);
  FreeMem(Res);
end;


function TmnBufferStream.ReadUntilCallback(vData: TObject; const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; Callback: TReadUntilCallback; out Matched: Boolean): Boolean;
var
  aCount: Integer;
  aBuf: TBytes;
  aSize: Integer;

  function _IsMatching(vBI, vMI: Integer; out vErr: Boolean): Boolean;
  var
    b: Byte;
    t: PByte;
  begin
    if vBI >= aCount then
    begin
      vErr := Read(b, 1) <> 1; { TODO : find way to improve this }

      if not vErr then
      begin
        if aCount=aSize then
        begin
          aSize := aSize + ReadWriteBufferSize;
          SetLength(aBuf, aSize);
        end;
        t := PByte(aBuf);
        Inc(t, aCount);
        t^ := b;
        Inc(aCount);
      end;
    end
    else
      vErr := False;

    if not vErr then
      Result := aBuf[vBI] = Match[vMI]
    else
      Result := False;
  end;

const
  sSize = 1024;

var
  Index: Integer;
  MatchIndex: Integer;
  aErr: Boolean;
begin
  if (Match = nil) or (MatchSize = 0) then
    raise Exception.Create('Match is empty!');

  Result := not (cloRead in Done);
  Matched := False;

  aCount := 0;
  Index := 0;
  MatchIndex := 0;
  aSize := sSize;
  SetLength(aBuf, sSize);

  while not Matched do
  begin
    if _IsMatching(Index + MatchIndex, MatchIndex, aErr) then
    begin
      Inc(MatchIndex);
      if MatchIndex = MatchSize then
        Matched := True;
    end
    else
    begin
      MatchIndex := 0;
      Inc(Index);
      if aErr then
        Break
      else if (Index>=sSize) then
      begin
        Callback(vData, PByte(aBuf), aCount);
        aCount := 0;
        Index := 0;
      end;
    end;
  end;

  if ExcludeMatch and Matched then
    aCount := aCount - MatchSize;

  Callback(vData, PByte(aBuf), aCount);

  if not Matched and (cloRead in Done) and (aSize = 0) then
    Result := False;
end;

function TmnBufferStream.ReadUTF8Line(out s: UTF8String; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  EOL: UTF8String;
begin
  EOL := UTF8String(EndOfLine);
  Result := ReadBufferUntil(@eol[1], mnStreams.ByteLength(eol), ExcludeEOL, res, len, m);
  CopyUTF8String(s, res, len);
  FreeMem(res);
end;

procedure TmnBufferStream.ReadUTF8Strings(Value: TStrings);
var
  s: UTF8String;
begin
  while Connected and CanRead do
  begin
    if ReadUTF8Line(s) then
      Value.Add(UTF8ToString(s));
  end;
end;

{$endif}

procedure TmnWrapperStream.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    if (FStream <> nil) and StreamOwned then
      FreeAndNil(FStream);
    FStream := Value;
    FStreamOwned := False;
  end;
end;

function TmnWrapperStream.DoRead(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
  if Result = 0 then
    FEndOfStream := True;
end;

function TmnWrapperStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);//TODO must be buffered
end;

function TmnWrapperStream.GetConnected: Boolean;
begin
  Result := not FEndOfStream;
end;

constructor TmnWrapperStream.Create(AStream: TStream; AEndOfLine: string; Owned: Boolean = True);
begin
  inherited Create(AEndOfLine);
  if AStream = nil then
    raise EmnStreamException.Create('Stream = nil');
  FStreamOwned := Owned;
  FStream := AStream;
end;

constructor TmnWrapperStream.Create(AStream: TStream; Owned: Boolean);
begin
  Create(AStream, sEndOfLine, Owned);
end;

destructor TmnWrapperStream.Destroy;
var
  AOwned: Boolean;
  AStream: TStream;
begin
  AOwned := FStreamOwned;
  AStream := FStream;
  inherited;    //Sometime need to close into FStream
  if AOwned then
    FreeAndNil(AStream);
end;

{ TmnHexStreamProxy }

function TmnHexStreamProxy.HexEncode(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;

  function DigiToChar(c: Byte): Byte; inline;
  begin
    if c < 10 then
      Result := $30 + c //ord(AnsiChar('0')) = $30
    else
      Result := $41 + (c - 10); //ord(AnsiChar('A')) = $41
  end;
var
  BufSize: Integer;
  Buf: PByteArray;
  b, c: Byte;
  i, p: Integer;
begin
  BufSize := Count * 2;
  GetMem(Buf, BufSize);
  try
    p := 0;
    for i := 0 to Count -1 do
    begin
      b := PByteArray(@Buffer)^[i];

      c := (b shr 4) and $F;
      Buf^[p] := DigiToChar(c);
      inc(p);

      c := b and $F;
      Buf^[p] := DigiToChar(c);
      inc(p);

    end;
    Result := Over.Write(Buf^, BufSize, ResultCount, RealCount);
    ResultCount := ResultCount div 2;
  finally
    FreeMem(Buf);
  end;
end;

function TmnHexStreamProxy.HexDecode(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;

  function CharToDigi(c: Byte):Byte; inline;
  begin
    //ord(AnsiChar('0')) = $30
    //ord(AnsiChar('A')) = $41
    //ord(AnsiChar('a')) = $61
    //ord(AnsiChar('f')) = $66

    if (c >=$61) and (c <= $66) then
      Result := (c - $61) + 10
    else if (c >= $41) and (c <= $66) then
      Result := (c - $41) + 10
    else if (c >= $30) then
      Result := (c - $30)
    else
      Result := 0; //wrong char
  end;

var
  BufSize: Longint;
  Buf: PByteArray;
  b, c: Byte;
  i, p: Integer;
begin
  Result := True;
  BufSize := Count * 2;
  GetMem(Buf, BufSize);
  Over.Read(Buf^, BufSize, BufSize, RealCount);
  ResultCount := BufSize div 2;
  try
    p := 0;
    for i := 0 to ResultCount -1 do
    begin

      c := Buf^[p];
      b := (CharToDigi(c) shl 4);
      inc(p);

      c := Buf^[p];
      b := b or CharToDigi(c);
      inc(p);

      PByteArray(@Buffer)^[i] := b;
    end;
  finally
    FreeMem(Buf);
  end;
end;

function TmnHexStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  HexDecode(Buffer, Count, ResultCount, RealCount);
  Result := True;
end;

function TmnHexStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  HexEncode(Buffer, Count, ResultCount, RealCount);
  Result := True;
end;

{ TmnReadWriteBuffer }

function TmnReadWriteBuffer.CheckBuffer: Boolean;
begin
  if Buffer = nil then
    CreateBuffer;

  if (Pos >= Stop) then
    LoadBuffer;

  Result := (Pos < Stop);
end;

function TmnReadWriteBuffer.Count: Cardinal;
begin
  Result := Stop - Pos;
end;

constructor TmnReadWriteBuffer.Create(vStream: TmnBufferStream);
begin
  inherited Create;
  Stream := vStream;
end;

procedure TmnReadWriteBuffer.CreateBuffer;
begin
  if Buffer <> nil then
    raise Exception.Create('Do you want to recreate stream buffer!!!');
  GetMem(Buffer, Size);
  Pos := Buffer;
  Stop := Buffer;
end;

destructor TmnReadWriteBuffer.Destroy;
begin
  FreeBuffer;
  inherited;
end;

function TmnReadWriteBuffer.DoRead(var vBuffer; vCount: Longint): Longint;
begin
  Result := Stream.DoRead(vBuffer, vCount);
  if Stream.Control <> nil then
    Stream.Control.Reading(vCount);
end;

function TmnReadWriteBuffer.DoWrite(const vBuffer; vCount: Longint): Longint;
begin
  if Stream.Control <> nil then
    Stream.Control.Writing(vCount);
  Result := Stream.DoWrite(vBuffer, vCount);
end;

procedure TmnReadWriteBuffer.FreeBuffer;
begin
  FreeMem(Buffer);
  Buffer := nil;
  Pos := nil;
  Stop := nil;
end;

function TmnReadWriteBuffer.LoadBuffer: TFileSize;
begin
  if Pos < Stop then
    raise EmnStreamException.Create('Buffer is not empty to load');
  Pos := Buffer;
  Result := DoRead(Buffer^, Size);
  if Result > 0 then //-1 not effects here
    Stop := Pos + Result
  else
    Stop := Pos;
  {if (Result = 0) and ZeroClose then //what if we have Timeout?
    Close([cloRead]);}
end;

function TmnReadWriteBuffer.Read(var vBuffer; vCount: Longint): Longint;
var
  c, aCount, aLoaded, aTry: Longint;
  P: PByte;
begin
  if Size = 0 then
    aCount := DoRead(vBuffer, vCount)
  else
  begin
    if Buffer = nil then
      CreateBuffer;
    P := @vBuffer;
    aCount := 0;
    aTry := 0;
    while (vCount > 0) {and not (cloRead in Stream.Done)} do
    begin
      c := Stop - Pos; //size of data in buffer
      if c = 0 then //check if buffer have no data
      begin
        aLoaded := LoadBuffer;

        if (aLoaded > 0)  then
          Continue
        else if (aLoaded = 0) and not Stream.Connected then
          break
        else if aLoaded<=0 then
        begin
          Inc(aTry);
          if aTry>=Stream.TimeoutTries then
            Break
        end
      end
      else if c > vCount then // is FReadBuffer enough for Count
        c := vCount;
      vCount := vCount - c;
      aCount := aCount + c;
      System.Move(Pos^, P^, c);
      Inc(P, c);
      Inc(Pos, c);
    end;
  end;
  Result := aCount;
end;

function TmnReadWriteBuffer.Write(const vBuffer; vCount: Longint): Longint;
begin
  Result := DoWrite(vBuffer, vCount)
end;

{ TmnInitialStreamProxy }

function TmnInitialStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FStream.ReadBuffer(Buffer, Count, ResultCount);
  RealCount := ResultCount;
end;

function TmnInitialStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FStream.WriteBuffer(Buffer, Count, ResultCount);
  RealCount := ResultCount;
end;

procedure TmnInitialStreamProxy.CloseData;
begin
  FStream.SetCloseData;
end;

procedure TmnInitialStreamProxy.CloseRead;
begin
  FStream.DoCloseRead;
end;

procedure TmnInitialStreamProxy.CloseWrite;
begin
  FStream.DoCloseWrite;
end;

procedure TmnInitialStreamProxy.Flush;
begin
  FStream.DoFlush;
end;

constructor TmnInitialStreamProxy.Create(AStream: TmnBufferStream);
begin
  inherited Create;
  FStream := AStream;
end;

{ TmnStreamControl }

procedure TmnStreamControl.Writing(Count: Longint);
begin
end;

procedure TmnStreamControl.Reading(Count: Longint);
begin
end;

destructor TmnStreamControl.Destroy;
begin
  inherited;
  if (FStream <> nil) and (FStream.Control = Self) then
    FStream.FControl := nil;
end;

end.

