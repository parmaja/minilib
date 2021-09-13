unit mnStreams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
//Change TFIleSize to TStreamSize (Longint)

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

type
  TTimeoutMode = (tmConnect, tmRead, tmWrite);
  TFileSize = Longint;

  EmnStreamException = class(Exception);
  EmnStreamExceptionAbort = class(Exception); //can be ignored by ide

  { TmnCustomStream }

  TmnCustomStream = class(TStream)
  private
  protected
    function GetConnected: Boolean; virtual; //Socket or COM ports have Connected override
  public
    //Count = 0 , load until eof
    function ReadString(Count: TFileSize = 0): RawByteString; overload;
    function ReadString(Count: TFileSize; CodePage: Word): RawByteString; overload;
    function ReadBufferBytes(Count: TFileSize = 0): TBytes; overload;
    function WriteString(const Value: String): TFileSize; overload;
    //Use copy from to stream
    function ReadStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function WriteStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;
    function WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;

    function CopyToStream(AStream: TStream; Count: TFileSize = 0): TFileSize; inline; //alias
    function CopyFromStream(AStream: TStream; Count: TFileSize = 0): TFileSize; inline; //alias

    property Connected: Boolean read GetConnected;
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

  TmnStreamClose = set of (
    cloRead, //Mark is as EOF
    cloWrite //Flush buffer
  );

  { TmnBufferStream }

  TmnBufferStream = class(TmnCustomStream)
  strict private type

    { TBuffer }

    TBuffer = record
      Buffer: PByte;
      Pos: PByte;
      Stop: PByte;
      Size: Longint;
      procedure CreateBuffer;
      procedure FreeBuffer;
      function Count: Cardinal;
    end;
  protected
    FReadBuffer: TBuffer;
    FWriteBuffer: TBuffer;
  strict private
    FDone: TmnStreamClose;
    FEndOfLine: string;
    //FZeroClose: Boolean;
    function LoadReadBuffer: TFileSize;
    //procedure SaveWriteBuffer; //kinda flush
  protected
    function CheckReadBuffer: Boolean; //check it in belal job
  private
    type
      { TmnInitialStreamProxy }

      TmnInitialStreamProxy = class sealed(TmnStreamProxy)
      private
      protected
        FStream: TmnBufferStream;

        procedure CloseRead; override;
        procedure CloseWrite; override;
        procedure Flush; override;
        function DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
        function DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
      public
        constructor Create(AStream: TmnBufferStream);
      end;

    procedure SetReadBufferSize(AValue: TFileSize);
    procedure SetWriteBufferSize(AValue: TFileSize);
  protected
    FProxy: TmnStreamProxy;

    procedure ReadError; virtual;
    //Override it but do not use it in your code, use DirectRead or DirectWrite
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;
    procedure DoFlush; virtual;
    procedure DoCloseRead; virtual;
    procedure DoCloseWrite; virtual;
    function GetEndOfStream: Boolean;

    property Proxy: TmnStreamProxy read FProxy;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    procedure AddProxy(AProxy: TmnStreamOverProxy);

    function DirectRead(var Buffer; Count: Longint): Longint;
    function DirectWrite(const Buffer; Count: Longint): Longint;

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;
    procedure Flush;

    procedure Close(ACloseWhat: TmnStreamClose = [cloRead, cloWrite]);

    function ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: Pointer; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;
    {$ifndef NEXTGEN}
    function ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; out Matched: Boolean): Boolean; overload;
    {$endif}

    function ReadLine: string; overload;
    function ReadLine(out S: utf8string; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLineUTF8(out S: utf8string; ExcludeEOL: Boolean = True): Boolean;
    function ReadLine(out S: unicodestring; ExcludeEOL: Boolean = True): Boolean; overload;

    function ReadLineRawByte(out S: rawbytestring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLineRawByte: RawByteString; overload;

    {$ifndef NEXTGEN}
    function ReadLine(out S: ansistring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLine(out S: widestring; ExcludeEOL: Boolean = True): Boolean; overload;
    function ReadAnsiString(vCount: Integer): AnsiString;
    {$endif}

    //function ReadLn: string; overload; deprecated;

    function WriteLine: TFileSize; overload;
    function WriteLine(const S: utf8string): TFileSize; overload;
    function WriteLine(const S: unicodestring): TFileSize; overload;

    function WriteRawByte(const S: rawbytestring): TFileSize; overload;
    function WriteLineRawByte(const S: rawbytestring): TFileSize; overload;
    function WriteLineUTF8(const S: UTF8String): TFileSize; overload;

    {$ifndef NEXTGEN}
    function WriteAnsiString(const S: ansistring): TFileSize; overload;
    function WriteLineAnsiString(const S: ansistring): TFileSize; overload;
    function WriteLine(const S: ansistring): TFileSize; overload;
    function WriteLine(const S: widestring): TFileSize; overload;
    {$endif}

    function ReadBytes(vCount: Integer): TBytes;
    procedure WriteBytes(Buffer: TBytes);

    procedure ReadCommand(out Command: string; out Params: string);

    procedure WriteCommand(const Command: string; const Format: string; const Params: array of const); overload;
    procedure WriteCommand(const Command: string; const Params: string = ''); overload;

    procedure ReadStrings(Value: TStrings); overload;
    function WriteStrings(const Value: TStrings): TFileSize; overload;

    property Done: TmnStreamClose read FDone;
    property EOF: Boolean read GetEndOfStream; {$ifdef FPC} deprecated; {$endif} //alias of Done
    property EndOfStream: Boolean read GetEndOfStream;

    property EndOfLine: string read FEndOfLine write FEndOfLine;
    property BufferSize: TFileSize read FReadBuffer.Size write SetReadBufferSize; //deprecated;
    property ReadBufferSize: TFileSize read FReadBuffer.Size write SetReadBufferSize;
    property WriteBufferSize: TFileSize read FWriteBuffer.Size write SetWriteBufferSize; //TODO not yet
  end;

  { TmnWrapperStream }

  TmnWrapperStream = class(TmnBufferStream)
  strict private
    FStreamOwned: Boolean;
    FStream: TStream;
    procedure SetStream(const Value: TStream);
  protected
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
  public
    constructor Create(AStream: TStream; AEndOfLine:string; Owned: Boolean = True); overload; virtual;
    constructor Create(AStream: TStream; Owned: Boolean = True); overload;
    destructor Destroy; override;
    property StreamOwned: Boolean read FStreamOwned write FStreamOwned default False;
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
  public
    constructor Create;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

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

const
  ReadWriteBufferSize = 1024;

implementation

procedure CopyUTF8String(out S: utf8string; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    SetLength(S, Len);
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

procedure CopyRawByteString(out S: RawByteString; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    SetLength(S, Len);
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

procedure CopyUnicodeString(out S: unicodestring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    {$ifdef FPC}
    SetLength(S, Len div SizeOf(unicodechar));
    {$else}
    SetLength(S, Len div SizeOf(char));
    {$endif}
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

{$ifndef NEXTGEN}
procedure CopyAnsiString(out S: ansistring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    SetLength(S, Len div SizeOf(ansichar));
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

procedure CopyWideString(out S: widestring; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    SetLength(S, Len div SizeOf(widechar));
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
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

function ByteLength(s: widestring): TFileSize; overload;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

{ TmnBufferStream.TBuffer }

function TmnBufferStream.TBuffer.Count: Cardinal;
begin
  Result := Stop - Pos;
end;

procedure TmnBufferStream.TBuffer.CreateBuffer;
begin
  if Buffer <> nil then
    raise Exception.Create('Do you want to recreate stream buffer!!!');
  GetMem(Buffer, Size);
  Pos := Buffer;
  Stop := Buffer;
end;

function TmnBufferStream.CheckReadBuffer: Boolean;
begin
  if FReadBuffer.Buffer = nil then
    FReadBuffer.CreateBuffer;
  if not (FReadBuffer.Pos < FReadBuffer.Stop) then
    LoadReadBuffer;
  Result := (FReadBuffer.Pos < FReadBuffer.Stop);
end;

procedure TmnBufferStream.TBuffer.FreeBuffer;
begin
  FreeMem(Buffer);
  Buffer := nil;
  Pos := nil;
  Stop := nil;
end;

{$endif}

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
  Enabled := True;
end;

procedure TmnStreamProxy.Disable;
begin
  Enabled := False;
end;

{ TmnInitialStreamProxy }

function TmnBufferStream.TmnInitialStreamProxy.DoRead(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  ResultCount := FStream.DoRead(Buffer, Count);
  RealCount := ResultCount;
  Result := True;
end;

function TmnBufferStream.TmnInitialStreamProxy.DoWrite(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  ResultCount := FStream.DoWrite(Buffer, Count);
  RealCount := ResultCount;
  Result := True;
end;

procedure TmnBufferStream.TmnInitialStreamProxy.CloseRead;
begin
  FStream.DoCloseRead;
end;

procedure TmnBufferStream.TmnInitialStreamProxy.CloseWrite;
begin
  FStream.DoCloseWrite;
end;

procedure TmnBufferStream.TmnInitialStreamProxy.Flush;
begin
  FStream.DoFlush;
end;

constructor TmnBufferStream.TmnInitialStreamProxy.Create(AStream: TmnBufferStream);
begin
  inherited Create;
  FStream := AStream;
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

function TmnConnectionStream.WaitToRead: Boolean;
begin
  Result := WaitToRead(ReadTimeout) = cerSuccess;
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

function TmnCustomStream.WriteString(const Value: String): TFileSize;
begin
  Result := Write(Pointer(Value)^, ByteLength(Value));
end;

function TmnCustomStream.GetConnected: Boolean;
begin
  Result := False;
end;

function TmnCustomStream.ReadString(Count: TFileSize): RawByteString;
var
  aBuffer: PByte;
  p: Integer;
  l, c, Size: Integer;
begin
  Result := '';
  Size := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    p := 0;
    while Connected do
    begin
      if (Count > 0) and (Size < ReadWriteBufferSize) then
        l := Size
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          Size := Size - c;
        SetLength(Result, p + c);
        Move(aBuffer^, (PByte(Result) + p)^, c);
        p := p + c;
      end;
      if (c = 0) or ((Count > 0) and (Size = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.ReadString(Count: TFileSize; CodePage: Word): RawByteString;
begin
  Result := ReadString(Count);
  SetCodePage(Result, CodePage, False);
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

function TmnCustomStream.CopyFromStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: Integer;
begin
  Result := WriteStream(AStream, Count, RealCount);
end;

function TmnCustomStream.ReadBufferBytes(Count: TFileSize): TBytes;
var
  aBuffer: PByte;
  p: Integer;
  l, c, Size: Integer;
begin
  Result := nil;
  Size := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    p := 0;
    while Connected do
    begin
      if (Count > 0) and (Size < ReadWriteBufferSize) then
        l := Size
      else
        l := ReadWriteBufferSize;
      c := Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          Size := Size - c;
        SetLength(Result, p + c);
        Move(aBuffer^, Result[p], c);
        p := p + c;
      end;
      if (c = 0) or ((Count > 0) and (Size = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, aSize: Integer;
begin
  Result := 0;
  RealCount := 0;
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    while True do //todo use Done
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
      if ((Count > 0) and (aSize = 0)) then //we finsih count
        break;
      if ((c = 0) and not Connected) then
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

function TmnCustomStream.WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, aSize: Integer;
begin
  Result := 0;
  RealCount := 0;
  aSize := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    while true do //todo use Done
    begin
      if (Count > 0) and (aSize < ReadWriteBufferSize) then
        l := aSize
      else
        l := ReadWriteBufferSize;
      c := AStream.Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          aSize := aSize - c;
        Result := Result + c;
        RealCount := RealCount + Write(aBuffer^, c);
      end;
      if ((Count > 0) and (aSize = 0)) then //we finsih count
        break;
      if ((c = 0) and not Connected) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

{$ifndef NEXTGEN}
function TmnBufferStream.WriteLineAnsiString(const S: ansistring): TFileSize;
var
  EOL: ansistring;
begin
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S))
  else
    Result := 0;
  if EndOfLine <> '' then
  begin
    EOL := EndOfLine;
    Result := Result + Write(Pointer(EOL)^, Length(EOL));
  end;
end;

function TmnBufferStream.WriteLine(const S: ansistring): TFileSize;
begin
  Result := WriteLineAnsiString(S);
end;

function TmnBufferStream.WriteLine(const S: widestring): TFileSize;
var
  EOL: widestring;
begin
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S))
  else
    Result := 0;
  if EndOfLine <> '' then
  begin
    EOL := widestring(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
  end;
end;

{$endif}

function TmnBufferStream.WriteLineRawByte(const S: rawbytestring): TFileSize;
var
  EOL: RawByteString;
begin
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  if EndOfLine <> '' then
  begin
    EOL := RawByteString(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, Length(EOL));
  end;
end;

function TmnBufferStream.WriteLineUTF8(const S: UTF8String): TFileSize;
var
  EOL: UTF8String;
begin
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  if EndOfLine <> '' then
  begin
    EOL := EndOfLine;
    Result := Result + Write(Pointer(EOL)^, Length(EOL));
  end;
end;

function TmnBufferStream.WriteAnsiString(const S: ansistring): TFileSize;
begin
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S))
  else
    Result := 0;
end;

function TmnBufferStream.WriteLine(const S: unicodestring): TFileSize;
var
  EOL: unicodestring;
begin
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  if EndOfLine <> '' then
  begin
    EOL := unicodestring(EndOfLine);
    Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
  end;
end;

function TmnBufferStream.WriteRawByte(const S: rawbytestring): TFileSize;
begin
  if S <> '' then
    Result := Write(Pointer(S)^, Length(S))
  else
    Result := 0;
end;

function TmnBufferStream.WriteLine(const S: utf8string): TFileSize;
var
  EOL: utf8string;
begin
  Result := 0;
  if s <> '' then
    Result := Write(PByte(S)^, ByteLength(S));
  if EndOfLine <> '' then
  begin
    EOL := EndOfLine;
    Result := Result + Write(PByte(EOL)^, ByteLength(EOL));
  end;
end;

procedure TmnBufferStream.WriteBytes(Buffer: TBytes);
begin
  WriteBuffer(Pointer(Buffer)^, Length(Buffer));
end;

function TmnBufferStream.WriteStrings(const Value: TStrings): TFileSize;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Value.Count - 1 do
  begin
    if Value[i] <> '' then //stupid delphi always add empty line in last of TStringList
      Result := Result + WriteLine(Value[i]);
  end;
end;

procedure TmnBufferStream.ReadCommand(out Command: string; out Params: string);
var
  s: string;
  p: Integer;
begin
  s := ReadLine;
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

function TmnBufferStream.ReadLine(out S: unicodestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: unicodestring;
begin
  EOL := unicodestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  {$ifdef FPC}
  CopyUnicodeString(S, PUnicodeChar(res), len);
  {$else}
  {$ifdef NEXTGEN}
  CopyUnicodeString(S, PChar(res), len);
  {$else}
  CopyUnicodeString(S, PWideChar(res), len); //TODO check if it widechar
  {$endif}
  {$endif}
  FreeMem(res);
end;

{$ifndef NEXTGEN}
function TmnBufferStream.ReadLine(out S: widestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: widestring;
begin
  EOL := widestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyWideString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(out S: ansistring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: ansistring;
begin
  EOL := ansistring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyAnsiString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadAnsiString(vCount: Integer): AnsiString;
begin
  {$ifdef FPC}Result := '';{$endif}
  SetLength(Result, vCount);
  Read(PAnsichar(Result)^, vCount);
end;
{$endif}

function TmnBufferStream.ReadLine(out S: utf8string; ExcludeEOL: Boolean): Boolean;
begin
  Result := ReadLineUTF8(S, ExcludeEOL);
end;

function TmnBufferStream.ReadLineRawByte(out S: rawbytestring; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: RawByteString;
begin
  EOL := RawByteString(EndOfLine);
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  CopyRawByteString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine: string;
begin
  ReadLine(Result);
end;

{function TmnBufferStream.ReadLn: string;
begin
  ReadLine(Result);
end;}

function TmnBufferStream.ReadLineRawByte: RawByteString;
begin
  ReadLineRawByte(Result, True);
end;

function TmnBufferStream.ReadLineUTF8(out S: utf8string; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: utf8string;
begin
  EOL := utf8string(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyUTF8String(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.WriteLine: TFileSize;
begin
  if EndOfLine <> '' then
    Result := Write(Pointer(EndOfLine)^, ByteLength(EndOfLine))
  else
    Result := 0;
end;

procedure TmnBufferStream.ReadStrings(Value: TStrings);
var
  s: string;
begin
  while not (cloRead in Done) do
  begin
    if ReadLine(s) then
      Value.Add(s);
  end;
end;

function TmnBufferStream.Write(const Buffer; Count: Longint): Longint;
{var
  P, aBuffer: PByte;
  aCount, c, Size: Integer;}
begin
  if FWriteBuffer.Size = 0 then
    Result := DirectWrite(Buffer, Count)
  else
  begin
    Result := 0;
    //TODO
    {if FWriteBuffer.Buffer = nil then
      FWriteBuffer.CreateBuffer;
    P := @Buffer;
    aCount := 0;
    while (Count > 0) and not (cloWrite in Done) do
    begin
      c := FWriteBuffer.Size - (FWriteBuffer.Stop - FWriteBuffer.Buffer); //size of empty data in buffer
      if c = 0 then //check if buffer have no data
      begin
        SaveWriteBuffer;
        Continue;//new
      end;
      FWriteBuffer.Pos := FWriteBuffer.Stop;
      if c > Count then // is FReadBuffer enough for Count
        c := Count;
      Count := Count - c;
      aCount := aCount + c;
      System.Move(FWriteBuffer.Pos^, P^, c);
      Inc(P, c);
      Inc(FWriteBuffer.Pos, c);
    end;}
  end;
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
  FReadBuffer.FreeBuffer;
  FWriteBuffer.FreeBuffer;
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

function TmnBufferStream.DirectRead(var Buffer; Count: Longint): Longint;
var
  RealCount: longint;
begin
  if FProxy <> nil then
    FProxy.Read(Buffer, Count, Result, RealCount)
  else
    Result := DoRead(Buffer, Count);
end;

function TmnBufferStream.DirectWrite(const Buffer; Count: Longint): Longint;
var
  RealCount: longint;
begin
  if FProxy <> nil then
    FProxy.Write(Buffer, Count, Result, RealCount)
  else
    Result := DoWrite(Buffer, Count);
end;

constructor TmnBufferStream.Create(AEndOfLine: string);
begin
  inherited Create;
  //FZeroClose := True;
  FReadBuffer.Size := ReadWriteBufferSize;
  FWriteBuffer.Size := 0;
  FEndOfLine := AEndOfLine;
end;

function TmnBufferStream.LoadReadBuffer: TFileSize;
begin
  if FReadBuffer.Pos < FReadBuffer.Stop then
    raise EmnStreamException.Create('Buffer is not empty to load');
  FReadBuffer.Pos := FReadBuffer.Buffer;
  Result := DirectRead(FReadBuffer.Buffer^, FReadBuffer.Size);
  if Result > 0 then //-1 not effects here
    FReadBuffer.Stop := FReadBuffer.Pos + Result
  else
    FReadBuffer.Stop := FReadBuffer.Pos;
  {if (Result = 0) and ZeroClose then //what if we have Timeout?
    Close([cloRead]);}
end;

{procedure TmnBufferStream.SaveWriteBuffer;
var
  aSize: TFileSize;
begin
  aSize := DirectRead(FWriteBuffer.Buffer, FWriteBuffer.Stop - FWriteBuffer.Buffer);
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

function TmnBufferStream.GetEndOfStream: Boolean;
begin
  Result := cloRead in Done;
end;

function TmnBufferStream.Read(var Buffer; Count: Longint): Longint;
var
  c, aCount, aSize: Longint;
  P: PByte;
begin
  Flush;//Flush write buffer
  if (ReadBufferSize = 0) then
    aCount := DirectRead(Buffer, Count)
  else
  begin
    if FReadBuffer.Buffer = nil then
      FReadBuffer.CreateBuffer;
    P := @Buffer;
    aCount := 0;
    while (Count > 0) and not (cloRead in Done) do
    begin
      c := FReadBuffer.Stop - FReadBuffer.Pos; //size of data in buffer
      if c = 0 then //check if buffer have no data
      begin
        aSize := LoadReadBuffer;
        if ((aSize = 0) and not Connected) then
          break
        else
          Continue//new
      end;
      if c > Count then // is FReadBuffer enough for Count
        c := Count;
      Count := Count - c;
      aCount := aCount + c;
      System.Move(FReadBuffer.Pos^, P^, c);
      Inc(P, c);
      Inc(FReadBuffer.Pos, c);
    end;
  end;
  Result := aCount;
end;

procedure TmnBufferStream.SetReadBufferSize(AValue: TFileSize);
begin
  if FReadBuffer.Size = AValue then
    Exit;
  if FReadBuffer.Buffer <> nil then
    raise Exception.Create('change ReadBufferSize before using stream');
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

function TmnBufferStream.ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: Pointer; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;

  function CheckReadBuffer: Boolean;
  begin
    if FReadBuffer.Buffer = nil then
      FReadBuffer.CreateBuffer;
    if not (FReadBuffer.Pos < FReadBuffer.Stop) then
      LoadReadBuffer;
    Result := (FReadBuffer.Pos < FReadBuffer.Stop);
  end;

var
  P: PByte;
  mt: PByte;
  c, l: TFileSize;
  t: PByte;
begin
  if (Match = nil) or (MatchSize = 0) then
    raise Exception.Create('Match is empty!');
  Result := not (cloRead in Done);
  Matched := False;
  mt := Match;
  ABuffer := nil;
  ABufferSize := 0;
  c := 1;//TODO use start from 0
  while not Matched and CheckReadBuffer do
  begin
    P := FReadBuffer.Pos;
    while P < FReadBuffer.Stop do
    begin
      if mt^ = P^ then
      begin
        Inc(c);
        Inc(mt);
      end
      else
        mt := Match;
      Inc(P);
      if c > MatchSize then
      begin
        Matched := True;
        break;
      end;
    end;

    //Append to memory
    l := P - FReadBuffer.Pos;
    if ExcludeMatch and Matched then
      l := l - MatchSize;

    ReAllocMem(ABuffer, ABufferSize + l);
    t := ABuffer;
    Inc(t, ABufferSize);
    Move(FReadBuffer.Pos^, t^, l);
    ABufferSize := ABufferSize + l;

    FReadBuffer.Pos := PByte(P);
  end;
  if not Matched and (cloRead in Done) and (ABufferSize = 0) then
    Result := False;
end;

function TmnBufferStream.ReadBytes(vCount: Integer): TBytes;
begin
  {$ifdef FPC}
  Result := nil;
  {$endif}
  SetLength(Result, vCount);
  vCount := Read(Pointer(Result)^, vCount);
  SetLength(Result, vCount);
end;

{$ifndef NEXTGEN}
function TmnBufferStream.ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean;
var
  Res: Pointer;
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
  Res: Pointer;
  len: TFileSize;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  CopyWideString(Buffer, Res, Len);
  FreeMem(Res);
end;

{$endif}

procedure TmnWrapperStream.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    if (FStream <> nil) and FStreamOwned then
      FreeAndNil(FStream);
    FStream := Value;
    FStreamOwned := False;
  end;
end;

function TmnWrapperStream.DoRead(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
  if (Result=0) then
  begin
    if not (FStream is TmnCustomStream) or not (FStream as TmnCustomStream).Connected then
      Close([cloRead]);
  end;
end;

function TmnWrapperStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);//TODO must be buffered
end;

constructor TmnWrapperStream.Create(AStream: TStream; AEndOfLine:string; Owned: Boolean = True);
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
  Result := True;
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
    Over.Write(Buf^, BufSize, ResultCount, RealCount);
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

end.
