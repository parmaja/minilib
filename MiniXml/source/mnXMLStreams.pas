unit mnXMLStreams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface
{$H+}
uses   
  Classes, SysUtils, StrUtils;

type
  EmnXMLStreamException = class(Exception);

  TmnXMLStream = class(TStream)
  private
    FStreamOwned: Boolean;
    FStream: TStream;
    FBuffer: PChar;
    FPos: PChar;
    FEnd: PChar;
    FBufferSize: Cardinal;
    FEOF: Boolean;
    FEndOfLine: string;
    procedure LoadBuffer;
  protected
  public
    constructor Create(AStream: TStream; Owned: Boolean = True); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure ReadUntil(const S: string; var Result: string; var Matched: Boolean);
    function ReadLn(var S: string): Boolean; overload;
    procedure ReadCommand(var Command: string; var Params: string);
    function WriteStream(Source: TStream): Longint;
    function WriteString(const Value: string): Cardinal;
    function WriteStrings(const Value: TStrings): Cardinal;
    function WriteLn(const Value: string): Cardinal;
    procedure WriteCommand(const Command: string; const Params: string = '');
    property EOF: Boolean read FEOF;
    property BufferSize: Cardinal read FBufferSize write FBufferSize;
    property EndOfLine: string read FEndOfLine write FEndOfLine;
  end;

implementation

const
  cBufferSize = 2048;
  sXMLEOL = #$A;

{ TmnXMLStream }

function TmnXMLStream.WriteString(const Value: string): Cardinal;
begin
  Result := FStream.Write(Pointer(Value)^, Length(Value));
end;

function TmnXMLStream.WriteStream(Source: TStream): Longint;
var
  aBuffer: pchar;
  n: cardinal;
begin
  {$IFDEF FPC}
  aBuffer := nil;
  {$ENDIF}
  GetMem(aBuffer, BufferSize);
  Result := 0;
  try
    repeat
      n := Source.Read(aBuffer^, BufferSize);
      if n > 0 then
        Write(aBuffer^, n);
      Inc(Result, n);
    until (n < BufferSize);
  finally
    FreeMem(aBuffer, BufferSize);
  end;
end;

function TmnXMLStream.WriteLn(const Value: string): Cardinal;
begin
  Result := WriteString(Value + EndOfLine);
end;

function TmnXMLStream.WriteStrings(const Value: TStrings): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Value.Count - 1 do
  begin
    if Value[i] <> '' then //stupid delphi always add empty line in last of TStringList
      Result := Result + WriteLn(Value[i]);
  end;
end;

procedure TmnXMLStream.ReadCommand(var Command, Params: string);
var
  s: string;
  p: Integer;
begin
  {$IFDEF FPC}
  S := '';
  {$ENDIF}
  ReadLn(S);
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

procedure TmnXMLStream.WriteCommand(const Command, Params: string);
begin
  if Params <> '' then
    WriteLn(Command + ' ' + Params)
  else
    WriteLn(Command);
end;

function TmnXMLStream.ReadLn(var S: string): Boolean;
var
  aMatched: Boolean;
begin
  Result := not EOF;
  if Result then
  begin
    aMatched := False;
    ReadUntil(EndOfLine, S, aMatched);
    if aMatched and (S <> '') then
      S := LeftStr(S, Length(S) - Length(EndOfLine));
  end;
end;

function TmnXMLStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

{ TmnXMLStream }

constructor TmnXMLStream.Create(AStream: TStream; Owned: Boolean = True);
begin
  inherited Create;
  if AStream = nil then
    raise EmnXMLStreamException.Create('CapStream not accept Stream = nil');
  FStreamOwned := Owned;
  FStream := AStream;
  FBufferSize := cBufferSize;
  GetMem(FBuffer, FBufferSize);
  FPos := FBuffer;
  FEnd := FBuffer;
  FEndOfLine := sXMLEOL;
end;

destructor TmnXMLStream.Destroy;
begin
  if FStreamOwned then
    FStream.Free;
  FreeMem(FBuffer, FBufferSize);
  FBuffer := nil;
  inherited;
end;

procedure TmnXMLStream.LoadBuffer;
var
  aSize: Cardinal;
begin
  if FPos < FEnd then
    raise EmnXMLStreamException.Create('Buffer is not empty to load');
  FPos := FBuffer;
  aSize := FStream.Read(FBuffer^, FBufferSize);
  FEnd := FPos + aSize;
  if aSize = 0 then
    FEOF := True;
end;

function TmnXMLStream.Read(var Buffer; Count: Integer): Longint;
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

procedure TmnXMLStream.ReadUntil(const S: string; var Result: string; var Matched: Boolean);
var
  P: PChar;
  function CheckBuffer: Boolean;
  begin
    if not (FPos < FEnd) then
      LoadBuffer;
    Result := (FPos < FEnd);
  end;
var
  idx: Integer;
  t: string;
begin
  Idx := 1;
  Matched := False;
  Result := '';
  while not Matched and CheckBuffer do
  begin
    P := FPos;
    while P < FEnd do
    begin
      if S[idx] = P^ then
        Inc(Idx)
      else
        Idx := 1;
      Inc(P);
      if Idx > Length(S) then
      begin
        Matched := True;
        break;
      end;
    end;
    SetString(t, FPos, P - FPos);
    Result := Result + t;
    FPos := P;
  end;
end;

end.

