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
  sEndOfLine = #$A;

type
  EmnStreamException = class(Exception);

  TmnCustomStream = class(TStream)
  private
    FBufferSize: Cardinal;
  published
  public
    function WriteString(const Value: string): Cardinal;
    function WriteStream(Source: TStream): Longint;
    property BufferSize: Cardinal read FBufferSize write FBufferSize;
  end;
  
  { TmnStream }

  TmnStream = class(TmnCustomStream)
  private
    FStreamOwned: Boolean;
    FStream: TStream;
    FBuffer: PChar;
    FPos: PChar;
    FEnd: PChar;
    FEOF: Boolean;
    FEndOfLine: string;
    procedure LoadBuffer;
    procedure SetStream(const Value: TStream);
  protected
  public
    constructor Create(AStream: TStream; AEndOfLine:string; Owned: Boolean = True); overload; virtual; 
    constructor Create(AStream: TStream; Owned: Boolean = True); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure ReadUntil(const UntilStr: string; var Result: string; var Matched: Boolean);
    function ReadLn(var S: string; ExcludeEOL: Boolean = True): Boolean; overload;
    procedure ReadStrings(Value: TStrings);
    procedure ReadCommand(var Command: string; var Params: string);
    function WriteStrings(const Value: TStrings): Cardinal;
    function WriteLn(const Value: string): Cardinal;
    procedure WriteCommand(const Command: string; const Params: string = '');
    property Stream: TStream read FStream write SetStream;
    property StreamOwned: Boolean read FStreamOwned write FStreamOwned default False;
    property EOF: Boolean read FEOF;
    property EndOfLine: string read FEndOfLine write FEndOfLine;
  end;

implementation

const
  cBufferSize = 2048;

{ TmnStream }

function TmnCustomStream.WriteString(const Value: string): Cardinal;
begin
  Result := Write(Pointer(Value)^, Length(Value));
end;

function TmnCustomStream.WriteStream(Source: TStream): Longint;
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

function TmnStream.WriteLn(const Value: string): Cardinal;
begin
  Result := WriteString(Value + EndOfLine);
end;

function TmnStream.WriteStrings(const Value: TStrings): Cardinal;
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

procedure TmnStream.ReadCommand(var Command, Params: string);
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

procedure TmnStream.WriteCommand(const Command, Params: string);
begin
  if Params <> '' then
    WriteLn(Command + ' ' + Params)
  else
    WriteLn(Command);
end;

function TmnStream.ReadLn(var S: string; ExcludeEOL: Boolean = True): Boolean;
var
  aMatched: Boolean;
begin
  Result := not EOF;
  if Result then
  begin
    aMatched := False;
    ReadUntil(EndOfLine, S, aMatched);
    if ExcludeEOL and aMatched and (S <> '') then
      S := LeftStr(S, Length(S) - Length(EndOfLine));
  end;
end;

procedure TmnStream.ReadStrings(Value: TStrings);
var
  s:string;
begin
  while not EOF do
  begin
    if ReadLn(S) then
      Value.Add(S);
  end;
end;

function TmnStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

{ TmnStream }

constructor TmnStream.Create(AStream: TStream; AEndOfLine:string; Owned: Boolean = True);
begin
  inherited Create;
  if AStream = nil then
    raise EmnStreamException.Create('Stream = nil');
  FStreamOwned := Owned;
  FStream := AStream;
  FBufferSize := cBufferSize;
  GetMem(FBuffer, FBufferSize);
  FPos := FBuffer;
  FEnd := FBuffer;
  FEndOfLine := AEndOfLine;
end;

constructor TmnStream.Create(AStream: TStream; Owned: Boolean);
begin
  Create(AStream, sEndOfLine, Owned);
end;

destructor TmnStream.Destroy;
begin
  if FStreamOwned then
    FStream.Free;
  FreeMem(FBuffer, FBufferSize);
  FBuffer := nil;
  inherited;
end;

procedure TmnStream.LoadBuffer;
var
  aSize: Cardinal;
begin
  if FPos < FEnd then
    raise EmnStreamException.Create('Buffer is not empty to load');
  FPos := FBuffer;
  aSize := FStream.Read(FBuffer^, FBufferSize);
  FEnd := FPos + aSize;
  if aSize = 0 then
    FEOF := True;
end;

function TmnStream.Read(var Buffer; Count: Integer): Longint;
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

procedure TmnStream.ReadUntil(const UntilStr: string; var Result: string; var Matched: Boolean);
var
  P: PChar;
  function CheckBuffer: Boolean;
  begin
    if not (FPos < FEnd) then
      LoadBuffer;
    Result := (FPos < FEnd);
  end;
var
  idx, l: Integer;
  t: string;
begin
  if UntilStr = '' then
    raise Exception.Create('UntilStr is empty!');
  Idx := 1;
  Matched := False;
  l := Length(UntilStr);
  Result := '';
  while not Matched and CheckBuffer do
  begin
    P := FPos;
    while P < FEnd do
    begin
      if UntilStr[idx] = P^ then
        Inc(Idx)
      else
        Idx := 1;
      Inc(P);
      if Idx > l then
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

procedure TmnStream.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    if (FStream <> nil) and FStreamOwned then
      FreeAndNil(FStream); 
    FStream := Value;
    FStreamOwned := False;
  end;
end;

end.


