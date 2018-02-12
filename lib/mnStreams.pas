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
    FReadBufferSize: TFileSize;
  protected
    function IsActive: Boolean; virtual;
  public
    function ReadString(Count: Longint = 255): string;
    function WriteString(const Value: string): TFileSize;
    function ReadStream(Dest: TStream): Longint;
    function WriteStream(Source: TStream): Longint;
    property BufferSize: TFileSize read FReadBufferSize write FReadBufferSize;
  end;

  { TmnBufferStream }

  TmnBufferStream = class(TmnCustomStream)
  strict private
    FReadBuffer: PByte;
    FEOF: Boolean;
    FEndOfLine: string;
    FEOFOnError: Boolean;
    procedure LoadBuffer;
  protected
    FPos: PByte;
    FEnd: PByte;
  protected
    procedure DoError(S: string); virtual;
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;

    procedure CreateBuffer;
    procedure FreeBuffer;
    function CheckBuffer: Boolean;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;

    function ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out Buffer: Pointer; out BufferSize: TFileSize; out Matched: Boolean): Boolean;
    {$ifndef NEXTGEN}
    function ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; out Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; out Matched: Boolean): Boolean; overload;
    function ReadLine(out S: ansistring; ExcludeEOL: Boolean = True; EOL: ansistring = ''): Boolean; overload;
    function ReadLine(out S: widestring; ExcludeEOL: Boolean = True; EOL: widestring = ''): Boolean; overload;
    {$endif}
    function ReadLineRawByte(out S: rawbytestring; ExcludeEOL: Boolean = True; EOL: rawbytestring = ''): Boolean; overload;
    function ReadLine(out S: utf8string; ExcludeEOL: Boolean = True; EOL: utf8string = ''): Boolean; overload;
    function ReadLine(out S: unicodestring; ExcludeEOL: Boolean = True; EOL: unicodestring = ''): Boolean; overload;

    function ReadLine: string; overload;

    function ReadLn: string; overload; deprecated;
    {$ifndef NEXTGEN}
    function ReadAnsiString(vCount: Integer): AnsiString;
    {$endif}
    function WriteLineRawByte(const S: rawbytestring; EOL: rawbytestring = ''): TFileSize; overload;
    {$ifndef NEXTGEN}
    function WriteLine(const S: ansistring; EOL: ansistring = ''): TFileSize; overload;
    function WriteLine(const S: widestring; EOL: widestring = ''): TFileSize; overload;
    {$endif}
    function WriteLine(const S: utf8string; EOL: utf8string = ''): TFileSize; overload;
    function WriteLine(const S: unicodestring; EOL: unicodestring = ''): TFileSize; overload;

    function WriteLn(const S: string): TFileSize; overload; deprecated;

    procedure ReadCommand(out Command: string; out Params: string);

    procedure WriteCommand(const Command: string); overload;
    procedure WriteCommand(const Command: string; const Format: string; const Params: array of const); overload;
    procedure WriteCommand(const Command: string; const Params: string); overload;

    function WriteEOL: TFileSize; overload;

    procedure ReadStrings(Value: TStrings; const vEOL: string); overload;
    procedure ReadStrings(Value: TStrings); overload;
    function WriteStrings(const Value: TStrings): TFileSize; overload;

    property EOF: Boolean read FEOF;

    property EndOfLine: string read FEndOfLine write FEndOfLine;
    property EOFOnError: Boolean read FEOFOnError write FEOFOnError default False;
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

  TmnConnectionStream = class abstract(TmnBufferStream)
  private
    FTimeout: Integer;
  protected
    function GetConnected: Boolean; virtual; abstract;
    function IsActive: Boolean; override;
  public
    constructor Create;
    procedure Connect; virtual; abstract;
    procedure Drop; virtual; abstract; //Shutdown
    procedure Disconnect; virtual; abstract;
    procedure Close; //alias for Disconnect
    function WaitToRead(Timeout: Longint): Boolean; overload; virtual; abstract;
    function WaitToWrite(Timeout: Longint): Boolean; overload; virtual; abstract;
    function WaitToRead: Boolean; overload;
    function WaitToWrite: Boolean; overload;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Timeout: Integer read FTimeout write FTimeout;
    property Connected: Boolean read GetConnected;
  end;

implementation

const
  cBufferSize = 2048;

procedure CopyString(out S: utf8string; Buffer: Pointer; Len: Integer); overload;
var
  rb: rawbytestring;
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

procedure CopyString(out S: rawbytestring; Buffer: Pointer; Len: Integer); overload;
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
var
  rb: rawbytestring;
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

function ByteLength(s: utf8string): TFileSize; overload;
begin
  Result := Length(s);
end;

{ TmnConnectionStream }

function TmnConnectionStream.IsActive: Boolean;
begin
  Result := GetConnected;
end;

constructor TmnConnectionStream.Create;
begin
  inherited Create;
  FTimeout := cReadTimeout;
end;

procedure TmnConnectionStream.Close;
begin
  Disconnect;
end;

function TmnConnectionStream.WaitToRead: Boolean;
begin
  Result := WaitToRead(Timeout);
end;

function TmnConnectionStream.WaitToWrite: Boolean;
begin
  Result := WaitToWrite(Timeout);
end;

function TmnConnectionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  {$ifdef fpc}
    Result := 0;
  {$endif}
    raise Exception.Create('not supported and we dont want to support it')
end;

{ TmnBufferStream }

function TmnCustomStream.WriteString(const Value: string): TFileSize;
begin
  Result := Write(Pointer(Value)^, ByteLength(Value));
end;

function TmnCustomStream.IsActive: Boolean;
begin
  Result := True;
end;

function TmnCustomStream.ReadString(Count: Longint): string;
var
  l : Longint;
begin
  SetLength(Result, Count);
  l := Read(Pointer(Result)^, Count);
  SetLength(Result, l);
end;

function TmnCustomStream.ReadStream(Dest: TStream): Longint;
var
  aBuffer: PByte;
  n: TFileSize;
begin
  {$ifdef FPC} //less hint in fpc
  aBuffer := nil;
  {$endif}
  GetMem(aBuffer, BufferSize);
  Result := 0;
  try
    repeat
      n := Read(aBuffer^, BufferSize);
      if n > 0 then
        Dest.Write(aBuffer^, n);
      Inc(Result, n);
    until (n < BufferSize) or not IsActive;
  finally
    FreeMem(aBuffer, BufferSize);
  end;
end;

function TmnCustomStream.WriteStream(Source: TStream): Longint;
var
  aBuffer: PByte;
  n: TFileSize;
begin
  GetMem(aBuffer, BufferSize);
  Result := 0;
  try
    repeat
      n := Source.Read(aBuffer^, BufferSize);
      if n > 0 then
        Write(aBuffer^, n);
      Inc(Result, n);
    until (n < BufferSize) or not IsActive;
  finally
    FreeMem(aBuffer, BufferSize);
  end;
end;

{$ifndef NEXTGEN}
function TmnBufferStream.WriteLine(const S: ansistring; EOL: ansistring): TFileSize;
begin
  if EOL = '' then
    EOL := ansistring(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  Result := Result + Write(Pointer(EOL)^, Length(EOL));
end;

function TmnBufferStream.WriteLine(const S: widestring; EOL: widestring): TFileSize;
begin
  if EOL = '' then
    EOL := widestring(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
end;
{$endif}

function TmnBufferStream.WriteLineRawByte(const S: rawbytestring; EOL: rawbytestring): TFileSize;
begin
  if EOL = '' then
    EOL := rawbytestring(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, Length(S));
  Result := Result + Write(Pointer(EOL)^, Length(EOL));
end;

function TmnBufferStream.WriteLine(const S: unicodestring; EOL: unicodestring): TFileSize;
begin
  if EOL = '' then
    EOL := unicodestring(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
end;

function TmnBufferStream.WriteLine(const S: utf8string; EOL: utf8string): TFileSize;
begin
  if EOL = '' then
    EOL := utf8string(EndOfLine);
  Result := 0;
  if s <> '' then
    Result := Write(Pointer(S)^, ByteLength(S));
  Result := Result + Write(Pointer(EOL)^, ByteLength(EOL));
end;

function TmnBufferStream.WriteLn(const S: string): TFileSize;
begin
  Result := WriteLine(S);
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

function TmnBufferStream.ReadLine(out S: unicodestring; ExcludeEOL: Boolean; EOL: unicodestring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
begin
  if EOL = '' then
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
function TmnBufferStream.ReadLine(out S: widestring; ExcludeEOL: Boolean; EOL: widestring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
begin
  if EOL = '' then
    EOL := widestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(out S: ansistring; ExcludeEOL: Boolean; EOL: ansistring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
begin
  if EOL = '' then
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

function TmnBufferStream.ReadLine(out S: utf8string; ExcludeEOL: Boolean; EOL: utf8string): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
begin
  if EOL = '' then
    EOL := utf8string(EndOfLine);
  Result := ReadBufferUntil(@eol[1], ByteLength(eol), ExcludeEOL, res, len, m);
  CopyString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLineRawByte(out S: rawbytestring; ExcludeEOL: Boolean; EOL: rawbytestring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: TFileSize;
begin
  if EOL = '' then
    EOL := rawbytestring(EndOfLine);
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  CopyString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine: string;
begin
  ReadLine(Result);
end;

function TmnBufferStream.ReadLn: string;
begin
  ReadLine(Result);
end;

function TmnBufferStream.WriteEOL: TFileSize;
begin
  Result := Write(Pointer(EndOfLine)^, ByteLength(EndOfLine));
end;

procedure TmnBufferStream.ReadStrings(Value: TStrings; const vEOL: string);
var
  s: string;
begin
  while not EOF do
  begin
    if ReadLine(s) then
      Value.Add(s);
  end;
end;

procedure TmnBufferStream.ReadStrings(Value: TStrings);
begin
  ReadStrings(Value, EndOfLine);
end;

function TmnBufferStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := DoWrite(Buffer, Count);//TODO must be buffered
end;

procedure TmnBufferStream.WriteCommand(const Command, Params: string);
begin
  if Params <> '' then
    WriteLine(Command + ' ' + Params)
  else
    WriteLine(Command);
end;

procedure TmnBufferStream.WriteCommand(const Command: string);
begin
  WriteCommand(Command, '');
end;

procedure TmnBufferStream.WriteCommand(const Command, Format: string; const Params: array of const);
begin
  WriteCommand(Command, SysUtils.Format(Format, Params));
end;

{ TmnBufferStream }

destructor TmnBufferStream.Destroy;
begin
  FreeBuffer;
  inherited;
end;

constructor TmnBufferStream.Create(AEndOfLine: string);
begin
  inherited Create;
  FReadBufferSize := cBufferSize;
  FEndOfLine := AEndOfLine;
  CreateBuffer;
end;

procedure TmnBufferStream.LoadBuffer;
var
  aSize: TFileSize;
begin
  if FPos < FEnd then
    raise EmnStreamException.Create('Buffer is not empty to load');
  FPos := FReadBuffer;
  aSize := DoRead(FReadBuffer^, FReadBufferSize);
  FEnd := FPos + aSize;
  if aSize = 0 then
    FEOF := True;
end;

procedure TmnBufferStream.DoError(S: string);
begin
  if FEOFOnError then
    FEOF := True
  else
    raise EmnStreamExceptionAbort.Create(S);
end;

procedure TmnBufferStream.CreateBuffer;
begin
  if FReadBuffer <> nil then
    raise Exception.Create('Do you want to recreate stream buffer!!!');
  GetMem(FReadBuffer, FReadBufferSize);
  FPos := FReadBuffer;
  FEnd := FReadBuffer;
end;

procedure TmnBufferStream.FreeBuffer;
begin
  FreeMem(FReadBuffer, FReadBufferSize);
  FReadBuffer := nil;
end;

function TmnBufferStream.Read(var Buffer; Count: Longint): Longint;
var
  c, aCount: Longint;
  P: PByte;
begin
  if FReadBuffer = nil then
    CreateBuffer;
  P := @Buffer;
  aCount := 0;
  while (Count > 0) and not EOF do
  begin
    c := FEnd - FPos;
    if c = 0 then
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

function TmnBufferStream.ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out Buffer: Pointer; out BufferSize: TFileSize; out Matched: Boolean): Boolean;
var
  P: PByte;
  mt: PByte;
  c, l: TFileSize;
  t: PByte;
begin
  if (Match = nil) or (MatchSize = 0) then
    raise Exception.Create('Match is empty!');
  Result := not EOF;
  Matched := False;
  mt := Match;
  Buffer := nil;
  BufferSize := 0;
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

    ReAllocMem(Buffer, BufferSize + l);
    t := Buffer;
    Inc(t, BufferSize);
    Move(FPos^, t^, l);
    BufferSize := BufferSize + l;

    FPos := PByte(P);
  end;
  if not Matched and EOF and (BufferSize = 0) then
    Result := False;
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
begin
  if FStreamOwned then
      FStream.Free;
  inherited;
end;

end.
