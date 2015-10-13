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
  sEndOfLine = #$0A;

  sWinEndOfLine = #$0D#$0A;
  sUnixEndOfLine = #$0A;
  sMacEndOfLine = #$0D;

type
  EmnStreamException = class(Exception);

  { TmnCustomStream }

  TmnCustomStream = class(TStream)
  private
    FBufferSize: Cardinal;
  protected
    function IsActive: Boolean; virtual;
  public
    function ReadString(Count: Longint = 255): string;
    function WriteString(const Value: string): Cardinal;
    function ReadStream(Dest: TStream): Longint;
    function WriteStream(Source: TStream): Longint;
    property BufferSize: Cardinal read FBufferSize write FBufferSize;
  end;

  { TmnBufferStream }

  TmnBufferStream = class(TmnCustomStream)
  strict private
    FBuffer: PByte;
    FEOF: Boolean;
    FEndOfLine: string;
    FEOFOnError: Boolean;
    procedure LoadBuffer;
  private
  protected
    FPos: PByte;
    FEnd: PByte;
    procedure DoError(S: string); virtual;
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;

    function CheckBuffer: Boolean;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;

    function ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out Buffer: Pointer; out BufferSize: Word; out Matched: Boolean): Boolean;

    function ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; var Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; var Matched: Boolean): Boolean; overload;
    function ReadUntil(const Match: utf8string; ExcludeMatch: Boolean; out Buffer: utf8string; var Matched: Boolean): Boolean; overload;

    function ReadLine(out S: ansistring; ExcludeEOL: Boolean = True; EOL: ansistring = ''): Boolean; overload;
    function ReadLine(out S: widestring; ExcludeEOL: Boolean = True; EOL: widestring = ''): Boolean; overload;
    function ReadLine(out S: utf8string; ExcludeEOL: Boolean = True; EOL: utf8string = ''): Boolean; overload;

    function ReadLine: ansistring; overload;
    function ReadLine: widestring; overload;
    function ReadLine: utf8string; overload;

    //Do not add UTF8, fpc will conflict it with ansi
    function ReadLn: ansistring; overload;
    function ReadLn: widestring; overload;

    function WriteLine(const S: ansistring; EOL: ansistring): Cardinal; overload;
    function WriteLine(const S: widestring; EOL: widestring = ''): Cardinal; overload;
    function WriteLine(const S: utf8string; EOL: utf8string = ''): Cardinal; overload;

    function WriteLn(const S: ansistring): Cardinal; overload;
    function WriteLn(const S: widestring): Cardinal; overload;

    procedure ReadCommand(out Command: string; out Params: string);

    procedure WriteCommand(const Command: string); overload;
    procedure WriteCommand(const Command: string; const Format: string; const Params: array of const); overload;
    procedure WriteCommand(const Command: string; const Params: string); overload;

    function WriteEOL(EOL: string): Cardinal; overload;
    function WriteEOL: Cardinal; overload;

    procedure ReadStrings(Value: TStrings; const vEOL: string); overload;
    procedure ReadStrings(Value: TStrings); overload;
    function WriteStrings(const Value: TStrings): Cardinal; overload;

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

implementation

const
  cBufferSize = 10;//2048;

{ TmnBufferStream }

function TmnCustomStream.WriteString(const Value: string): Cardinal;
begin
  Result := Write(Pointer(Value)^, Length(Value) * SizeOf(Char));
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
  aBuffer: pchar;
  n: Cardinal;
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
  aBuffer: pchar;
  n: Cardinal;
begin
  aBuffer :=GetMem(BufferSize);
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

function TmnBufferStream.WriteLine(const S: ansistring; EOL: ansistring): Cardinal;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := Write(Pointer(S)^, Length(S) * SizeOf(AnsiChar));
  WriteEOL(AnsiString(EndOfLine));
end;

function TmnBufferStream.WriteLine(const S: widestring; EOL: widestring): Cardinal;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := Write(Pointer(S)^, Length(S) * SizeOf(AnsiChar));
  WriteEOL(WideString(EndOfLine));
end;

function TmnBufferStream.WriteLine(const S: utf8string; EOL: utf8string): Cardinal;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := Write(Pointer(S)^, Length(S) * SizeOf(AnsiChar));
  WriteEOL(UTF8String(EndOfLine));
end;

function TmnBufferStream.WriteLn(const S: ansistring): Cardinal;
begin
  Result := WriteLine(S);
end;

function TmnBufferStream.WriteLn(const S: widestring): Cardinal;
begin
  Result := WriteLine(S);
end;

function TmnBufferStream.WriteStrings(const Value: TStrings): Cardinal;
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

procedure TmnBufferStream.ReadCommand(out Command, Params: string);
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

function TmnBufferStream.ReadLine(out S: widestring; ExcludeEOL: Boolean; EOL: widestring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: Word;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  SetString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(out S: utf8string; ExcludeEOL: Boolean; EOL: utf8string): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: Word;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  SetString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine(out S: ansistring; ExcludeEOL: Boolean; EOL: ansistring): Boolean;
var
  m: Boolean;
  res: Pointer;
  len: Word;
begin
  if EOL = '' then
    EOL := EndOfLine;
  Result := ReadBufferUntil(@eol[1], Length(eol), ExcludeEOL, res, len, m);
  SetString(S, res, len);
  FreeMem(res);
end;

function TmnBufferStream.ReadLine: widestring;
begin
  ReadLine(Result);
end;

function TmnBufferStream.ReadLine: ansistring;
begin
  ReadLine(Result);
end;

function TmnBufferStream.ReadLine: utf8string;
begin
  ReadLine(Result);
end;

function TmnBufferStream.ReadLn: ansistring;
begin
  ReadLine(Result);
end;

function TmnBufferStream.ReadLn: widestring;
begin
  ReadLine(Result);
end;


procedure TmnBufferStream.WriteCommand(const Command, Params: string);
begin
  if Params <> '' then
    WriteLine(Command + ' ' + Params)
  else
    WriteLine(Command);
end;

function TmnBufferStream.WriteEOL(EOL: string): Cardinal;
begin
  Result := WriteString(EOL); //TODO
  //Result := Write(Pointer(Value)^, Length(Value) * SizeOf(Char));
end;

function TmnBufferStream.WriteEOL: Cardinal;
begin
  Result := WriteEOL(EndOfLine);
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

function TmnBufferStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := DoWrite(Buffer, Count);//TODO must be buffered
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
  FreeMem(FBuffer, FBufferSize);
  FBuffer := nil;
  inherited;
end;

constructor TmnBufferStream.Create(AEndOfLine: string);
begin
  inherited Create;
  FPos := FBuffer;
  FEnd := FBuffer;
  FBufferSize := cBufferSize;
  GetMem(FBuffer, FBufferSize);
  FEndOfLine := AEndOfLine;
end;

procedure TmnBufferStream.LoadBuffer;
var
  aSize: Cardinal;
begin
  if FPos < FEnd then
    raise EmnStreamException.Create('Buffer is not empty to load');
  FPos := FBuffer;
  aSize := DoRead(FBuffer^, FBufferSize);
  FEnd := FPos + aSize;
  if aSize = 0 then
    FEOF := True;
end;

procedure TmnBufferStream.DoError(S: string);
begin
  if FEOFOnError then
    FEOF := True
  else
    raise EmnStreamException.Create(S);
end;

function TmnBufferStream.Read(var Buffer; Count: Integer): Longint;
var
  c, aCount: Longint;
  P: PChar;
begin
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
    if c > Count then // is FBuffer enough for Count
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
  if not (FPos < FEnd) then
    LoadBuffer;
  Result := (FPos < FEnd);
end;

function TmnBufferStream.ReadBufferUntil(const Match: PByte; MatchSize: Word; ExcludeMatch: Boolean; out Buffer: Pointer; out BufferSize: Word; out Matched: Boolean): Boolean;
var
  P: PByte;
  mt: PByte;
  c, l: cardinal;
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
      l := l + MatchSize;

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

function TmnBufferStream.ReadUntil(const Match: ansistring; ExcludeMatch: Boolean; out Buffer: ansistring; var Matched: Boolean): Boolean;
var
  Res: Pointer;
  Len: Word;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  SetString(Buffer, Res, Len);
  FreeMem(Res);
end;

function TmnBufferStream.ReadUntil(const Match: widestring; ExcludeMatch: Boolean; out Buffer: widestring; var Matched: Boolean): Boolean;
var
  Res: Pointer;
  Len: Word;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  SetString(Buffer, Res, Len);
  FreeMem(Res);
end;

function TmnBufferStream.ReadUntil(const Match: utf8string; ExcludeMatch: Boolean; out Buffer: utf8string; var Matched: Boolean): Boolean;
var
  Res: Pointer;
  Len: Word;
begin
  if Match = '' then
    raise Exception.Create('Match is empty!');
  Result := ReadBufferUntil(@Match[1], Length(Match), ExcludeMatch, Res, Len, Matched);
  SetString(Buffer, Res, Len);
  FreeMem(Res);
end;

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


