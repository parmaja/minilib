unit mnStreams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils;

const
  cReadTimeout = 15000;

  sEndOfLine = #$0A;

  sWinEndOfLine = #$0D#$0A;
  sUnixEndOfLine = #$0A;
  sMacEndOfLine = #$0D;
  sGSEndOfLine = #$1E;

type
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
    function ReadString(Count: TFileSize = 0): String; overload;
    function WriteString(const Value: String): TFileSize; overload;
    function ReadStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function WriteStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload;
    function ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;
    function WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize; overload;
    property Connected: Boolean read GetConnected;
  end;

  TmnStreamOverProxy = class;

  { TmnStreamProxy }

  TmnStreamProxy = class abstract(TObject)
  protected
    FSuperior: TmnStreamOverProxy;
  public
    //RealCount passed to Original Stream to retrive the real size of write or read, do not assign or modifiy it
    function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual; abstract;
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; virtual; abstract;
    procedure CloseRead; virtual; abstract;
    procedure CloseWrite; virtual; abstract;
    destructor Destroy; override;
    property Superior: TmnStreamOverProxy read FSuperior;
  end;

  { TmnStreamOverProxy }

  TmnStreamOverProxy = class(TmnStreamProxy)
  private
    FOver: TmnStreamProxy;
  protected
  public
    //Override but do not inherit it
    function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    //Override but do not inherit it
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;

    //Inhrite it please
    procedure CloseRead; override;
    procedure CloseWrite; override;
    constructor Create;
    property Over: TmnStreamProxy read FOver;
  end;

  TmnStreamClose = set of (
    cloRead, //Mark is as EOF
    cloWrite //Flush buffer
  );

  { TmnBufferStream }

  TmnBufferStream = class(TmnCustomStream)
  strict private
    FBufferSize: TFileSize;
    FReadBuffer: PByte;
    FDone: TmnStreamClose;
    FEndOfLine: string;
    FZeroClose: Boolean;
    procedure LoadBuffer;
  protected
    FPos: PByte;
    FEnd: PByte;
    procedure CreateBuffer;
    procedure FreeBuffer;
    function CheckBuffer: Boolean;

  private
    type
      { TmnInitialStreamProxy }

      TmnInitialStreamProxy = class(TmnStreamProxy)
      protected
        FStream: TmnBufferStream;
      public
        function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
        function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
        procedure CloseRead; override;
        procedure CloseWrite; override;
        constructor Create(AStream: TmnBufferStream);
      end;

    procedure SetReadBufferSize(AValue: TFileSize);
  protected
    FInternalProxy: TmnStreamProxy;
    FProxy: TmnStreamProxy;

    procedure ReadError; virtual;
    //Override it but do not use it in your code, use DirectRead or DirectWrite
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;
    procedure DoCloseRead; virtual;
    procedure DoCloseWrite; virtual;
    function GetEndOfStream: Boolean;

    property Proxy: TmnStreamProxy read FProxy;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    procedure AddProxy(AProxy: TmnStreamOverProxy; AsFirst: Boolean = False);

    function DirectRead(var Buffer; Count: Longint): Longint;
    function DirectWrite(const Buffer; Count: Longint): Longint;

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;

    procedure Close(ACloseWhat: TmnStreamClose = [cloRead, cloWrite]);

    function ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: Pointer; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;
    {$ifndef NEXTGEN}
    function ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; out Matched: Boolean): Boolean; overload;
    {$endif}

    function ReadLine: string; overload;
    function ReadLine(out S: utf8string; ExcludeEOL: Boolean = True): Boolean; overload;
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

    function WriteLineRawByte(const S: rawbytestring): TFileSize; overload;
    function WriteLineUTF8(const S: UTF8String): TFileSize; overload;

    {$ifndef NEXTGEN}
    function WriteLine(const S: ansistring): TFileSize; overload;
    function WriteLine(const S: widestring): TFileSize; overload;
    {$endif}

    //function WriteLn(const S: string): TFileSize; overload; deprecated;

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
    //if read zero length, close it, or that mean we have timeout system
    property ZeroClose: Boolean read FZeroClose write FZeroClose;
    property BufferSize: TFileSize read FBufferSize write SetReadBufferSize;
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
    FTimeout: Integer;
  protected
  public
    constructor Create;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    function WaitToRead(Timeout: Longint): TmnConnectionError; overload; virtual; abstract;
    function WaitToWrite(Timeout: Longint): TmnConnectionError; overload; virtual; abstract;
    function WaitToRead: Boolean; overload;
    function WaitToWrite: Boolean; overload;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Timeout: Integer read FTimeout write FTimeout;
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
  public

    Method: TmnMethodProxyEncode;
    function Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
    function Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean; override;
  end;

const
  ReadWriteBufferSize = 1024;

implementation

procedure CopyString(out S: utf8string; Buffer: Pointer; Len: Integer); overload;
var
  rb: RawByteString;
begin
  if Len <> 0 then
  begin
    SetLength(rb, Len);
    Move(PByte(Buffer)^, PByte(rb)^, Len);
    S := rb;
  end
  else
    S := '';
end;

procedure CopyString(out S: RawByteString; Buffer: Pointer; Len: Integer); overload;
begin
  if Len <> 0 then
  begin
    SetLength(S, Len);
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

procedure CopyString(out S: unicodestring; Buffer: Pointer; Len: Integer); overload;
begin
  if Len <> 0 then
  begin
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
procedure CopyString(out S: ansistring; Buffer: Pointer; Len: Integer); overload;
begin
  if Len <> 0 then
  begin
    SetLength(S, Len div SizeOf(ansichar));
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

procedure CopyString(out S: widestring; Buffer: Pointer; Len: Integer); overload;
begin
  if Len <> 0 then
  begin
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
{$endif}

{ TmnStreamProxy }

destructor TmnStreamProxy.Destroy;
begin
  if FSuperior <> nil then
    FSuperior.Free; //do not use FreeAndNil
  inherited;
end;

{ TmnInitialStreamProxy }

function TmnBufferStream.TmnInitialStreamProxy.Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  ResultCount := FStream.DoRead(Buffer, Count);
  RealCount := ResultCount;
  Result := True;
end;

function TmnBufferStream.TmnInitialStreamProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
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

constructor TmnBufferStream.TmnInitialStreamProxy.Create(AStream: TmnBufferStream);
begin
  inherited Create;
  FStream := AStream;
end;

{ TmnStreamOverProxy }

function TmnStreamOverProxy.Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FOver.Read(Buffer, Count, ResultCount, RealCount);
end;

function TmnStreamOverProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  Result := FOver.Write(Buffer, Count, ResultCount, RealCount);
end;

procedure TmnStreamOverProxy.CloseRead;
begin
  FOver.CloseRead;
end;

procedure TmnStreamOverProxy.CloseWrite;
begin
  FOver.CloseWrite;
end;

constructor TmnStreamOverProxy.Create;
begin
  inherited Create;
end;

{ TmnConnectionStream }

constructor TmnConnectionStream.Create;
begin
  inherited Create;
  FTimeout := cReadTimeout;
end;

function TmnConnectionStream.WaitToRead: Boolean;
begin
  Result := WaitToRead(Timeout) = cerSuccess;
end;

function TmnConnectionStream.WaitToWrite: Boolean;
begin
  Result := WaitToWrite(Timeout) = cerSuccess;
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
  Result := True;
end;

function TmnCustomStream.ReadString(Count: TFileSize): String;
var
  aBuffer: PByte;
  S: RawByteString;
  l, c, Size: Integer;
begin
  Result := '';
  Size := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
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
        SetString(S, PChar(aBuffer), c);
        Result:= Result + S
      end;
      if (c = 0) or ((Count > 0) and (Size = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.ReadStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: TFileSize;
begin
  Result := ReadStream(AStream, Count, RealCount);
end;

function TmnCustomStream.ReadStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, Size: Integer;
begin
  Result := 0;
  RealCount := 0;
  Size := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    while Connected do //todo use Done
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
        Result := Result + c;
        RealCount := RealCount + AStream.Write(aBuffer^, c);
      end;
      if (c = 0) or ((Count > 0) and (Size = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

function TmnCustomStream.WriteStream(AStream: TStream; Count: TFileSize): TFileSize;
var
  RealCount: TFileSize;
begin
  Result := WriteStream(AStream, Count, RealCount);
end;

function TmnCustomStream.WriteStream(AStream: TStream; Count: TFileSize; out RealCount: Integer): TFileSize;
var
  aBuffer: PByte;
  l, c, Size: Integer;
begin
  Result := 0;
  RealCount := 0;
  Size := Count;
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, ReadWriteBufferSize);
  try
    while Connected do //todo use Done
    begin
      if (Count > 0) and (Size < ReadWriteBufferSize) then
        l := Size
      else
        l := ReadWriteBufferSize;
      c := AStream.Read(aBuffer^, l);
      if c > 0 then
      begin
        if Count > 0 then
          Size := Size - c;
        Result := Result + c;
        RealCount := RealCount + Write(aBuffer^, c);
      end;
      if (c = 0) or ((Count > 0) and (Size = 0)) then
        break;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

{$ifndef NEXTGEN}
function TmnBufferStream.WriteLine(const S: ansistring): TFileSize;
var
  EOL: ansistring;
begin
  EOL := EndOfLine;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S))
  else
    Result := 0;
  Result := Result + Write(Pointer(EOL)^, Length(EOL));
end;

function TmnBufferStream.WriteLine(const S: widestring): TFileSize;
var
  EOL: widestring;
begin
  EOL := widestring(EndOfLine);
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S))
  else
    Result := 0;
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
end;

{$endif}

function TmnBufferStream.WriteLineRawByte(const S: rawbytestring): TFileSize;
var
  EOL: RawByteString;
begin
  EOL := RawByteString(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  Result := Result + Write(Pointer(EOL)^, Length(EOL));
end;

function TmnBufferStream.WriteLineUTF8(const S: UTF8String): TFileSize;
var
  EOL: UTF8String;
begin
  EOL := EndOfLine;
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  Result := Result + Write(Pointer(EOL)^, Length(EOL));
end;

function TmnBufferStream.WriteLine(const S: unicodestring): TFileSize;
var
  EOL: unicodestring;
begin
  EOL := unicodestring(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
end;

function TmnBufferStream.WriteLine(const S: utf8string): TFileSize;
var
  EOL: utf8string;
begin
  Result := 0;
  EOL := EndOfLine;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
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
  CopyString(S, PUnicodeChar(res), len);
  {$else}
  {$ifdef NEXTGEN}
  CopyString(S, PChar(res), len);
  {$else}
  CopyString(S, PWideChar(res), len); //TODO check if it widechat
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
  CopyString(S, res, len);
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
  CopyString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadAnsiString(vCount: Integer): AnsiString;
begin
  SetLength(Result, vCount);
  Read(PAnsichar(Result)^, vCount);
end;
{$endif}

function TmnBufferStream.ReadLine(out S: utf8string; ExcludeEOL: Boolean): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
  EOL: utf8string;
begin
  EOL := utf8string(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyString(S, res, len);
  FreeMem(res);
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
  CopyString(S, res, len);
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

function TmnBufferStream.WriteLine: TFileSize;
begin
  Result := Write(Pointer(EndOfLine)^, ByteLength(EndOfLine));
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
begin
  Result := DirectWrite(Buffer, Count);//TODO must be buffered
end;

procedure TmnBufferStream.Close(ACloseWhat: TmnStreamClose);
begin
  if not (cloRead in Done) and (cloRead in ACloseWhat) then
  begin
    if FProxy <> nil then
      FProxy.CloseRead
    else
      DoCloseRead;
    FDone := FDone + [cloRead];
  end;

  if not (cloWrite in Done) and (cloWrite in ACloseWhat) then
  begin
    if FProxy <> nil then
      FProxy.CloseWrite
    else
      DoCloseWrite;
    FDone := FDone + [cloWrite];
  end;
end;

{ TmnBufferStream }

destructor TmnBufferStream.Destroy;
begin
  Close;
  FreeAndNil(FInternalProxy);
  FreeBuffer;
  inherited;
end;

procedure TmnBufferStream.AddProxy(AProxy: TmnStreamOverProxy; AsFirst: Boolean);
begin
  if FInternalProxy = nil then
  begin
    FInternalProxy := TmnInitialStreamProxy.Create(Self);
    FProxy := FInternalProxy;
  end;

  if AsFirst then
  begin
    AProxy.FOver := FInternalProxy;
    FInternalProxy.FSuperior .FOver := AProxy;
    FInternalProxy.FSuperior := AProxy;
  end
  else
  begin
    AProxy.FOver := FProxy;
    AProxy.FOver.FSuperior := AProxy;
    FProxy := AProxy;
  end;
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
  FZeroClose := True;
  FBufferSize := ReadWriteBufferSize;
  FEndOfLine := AEndOfLine;
  //CreateBuffer;
end;

procedure TmnBufferStream.LoadBuffer;
var
  aSize: TFileSize;
begin
  if FPos < FEnd then
    raise EmnStreamException.Create('Buffer is not empty to load');
  FPos := FReadBuffer;
  aSize := DirectRead(FReadBuffer^, FBufferSize);
  if aSize > 0 then //-1 not effects here
    FEnd := FPos + aSize
  else
    FEnd := FPos;
  if (aSize = 0) and ZeroClose then //what if we have Timeout?
    Close([cloRead]);
end;

procedure TmnBufferStream.ReadError;
begin
  Close([cloRead]);
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

procedure TmnBufferStream.CreateBuffer;
begin
  if FReadBuffer <> nil then
    raise Exception.Create('Do you want to recreate stream buffer!!!');
  GetMem(FReadBuffer, FBufferSize);
  FPos := FReadBuffer;
  FEnd := FReadBuffer;
end;

procedure TmnBufferStream.FreeBuffer;
begin
  FreeMem(FReadBuffer);
  FReadBuffer := nil;
end;

function TmnBufferStream.Read(var Buffer; Count: Longint): Longint;
var
  c, aCount: Longint;
  P: PByte;
begin
  if (BufferSize = 0) then
    aCount := DirectRead(Buffer, Count)
  else
  begin
    if FReadBuffer = nil then
      CreateBuffer;
    P := @Buffer;
    aCount := 0;
    while (Count > 0) and not (cloRead in Done) do
    begin
      c := FEnd - FPos; //size of data in buffer
      if c = 0 then //check if buffer have no data
      begin
        LoadBuffer;
        Continue;//new
      end;
      if c > Count then // is FReadBuffer enough for Count
        c := Count;
      Count := Count - c;
      aCount := aCount + c;
      System.Move(FPos^, P^, c);
      Inc(P, c);
      Inc(FPos, c);
    end;
  end;
  Result := aCount;
end;

function TmnBufferStream.CheckBuffer: Boolean;
begin
  if FReadBuffer = nil then
    CreateBuffer;
  if not (FPos < FEnd) then
    LoadBuffer;
  Result := (FPos < FEnd);
end;

procedure TmnBufferStream.SetReadBufferSize(AValue: TFileSize);
begin
  if FBufferSize =AValue then Exit;
  if FReadBuffer <> nil then
    raise Exception.Create('BufferSize changed before using stream');
  FBufferSize :=AValue;
end;

function TmnBufferStream.ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out ABuffer: Pointer; out ABufferSize: TFileSize; out Matched: Boolean): Boolean;
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
  while not Matched and CheckBuffer do
  begin
    P := FPos;
    while P < FEnd do
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
    l := P - FPos;
    if ExcludeMatch and Matched then
      l := l - MatchSize;

    ReAllocMem(ABuffer, ABufferSize + l);
    t := ABuffer;
    Inc(t, ABufferSize);
    Move(FPos^, t^, l);
    ABufferSize := ABufferSize + l;

    FPos := PByte(P);
  end;
  if not Matched and (cloRead in Done) and (ABufferSize = 0) then
    Result := False;
end;

function TmnBufferStream.ReadBytes(vCount: Integer): TBytes;
begin
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
  CopyString(Buffer, Res, Len);
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
  CopyString(Buffer, Res, Len);
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
      Result := ord(AnsiChar('0')) + c
    else
      Result :=  ord(AnsiChar('A')) + (c - 10);
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
    if (c >= ord(AnsiChar('a'))) and (c <= ord(AnsiChar('f'))) then
      Result := c - ord(AnsiChar('a')) + 10
    else if (c >= ord(AnsiChar('A'))) and (c <= ord(AnsiChar('f'))) then
      Result := c - ord(AnsiChar('A')) + 10
    else if c >= ord(AnsiChar('0')) then
      Result := c - ord(AnsiChar('0'))
    else
      Result := 0; //wrong char
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

function TmnHexStreamProxy.Read(var Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  HexDecode(Buffer, Count, ResultCount, RealCount);
  Result := True;
end;

function TmnHexStreamProxy.Write(const Buffer; Count: Longint; out ResultCount, RealCount: longint): Boolean;
begin
  HexEncode(Buffer, Count, ResultCount, RealCount);
  Result := True;
end;

end.
