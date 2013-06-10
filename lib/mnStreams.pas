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
    FBuffer: PChar;
    FEOF: Boolean;
    FEndOfLine: string;
    FEOFOnError: Boolean;
    procedure LoadBuffer;
  private
  protected
    FPos: PChar;
    FEnd: PChar;
    procedure DoError(S: string); virtual;
    function DoRead(var Buffer; Count: Longint): Longint; virtual; abstract;
    function DoWrite(const Buffer; Count: Longint): Longint; virtual; abstract;

    function CheckBuffer: Boolean;
  public
    constructor Create(AEndOfLine: string = sUnixEndOfLine);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer; Count: Longint): Longint; override; final;

    procedure ReadUntil(const UntilStr: string; var Result: string; var Matched: Boolean);
    function ReadLine(var S: string; const vEOL: string; vExcludeEOL: Boolean = True): Boolean; overload;
    function ReadLine(const vEOL: string): string; overload;
    function ReadLn: string; overload;
    function ReadLn(var S: string; ExcludeEOL: Boolean = True): Boolean; overload;
    function WriteLine(const S: string; const vEOL: string): Cardinal;
    function WriteLn(const S: string): Cardinal;

    procedure ReadCommand(var Command: string; var Params: string);
    procedure WriteCommand(const Command: string; const Params: string = '');

    function WriteEOL(EOL: string): Cardinal; overload;
    function WriteEOL: Cardinal; overload;

    procedure ReadStrings(Value: TStrings; const vEOL: string); overload;
    procedure ReadStrings(Value: TStrings); overload;
    function WriteStrings(const Value: TStrings; const vEOL: string): Cardinal; overload;
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
  cBufferSize = 2048;

{ TmnBufferStream }

function TmnCustomStream.WriteString(const Value: string): Cardinal;
begin
  Result := Write(Pointer(Value)^, Length(Value));
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
    until (n < BufferSize) or not IsActive;
  finally
    FreeMem(aBuffer, BufferSize);
  end;
end;

function TmnBufferStream.WriteLine(const S: string; const vEOL: string): Cardinal;
begin
  Result := WriteString(S + vEOL);
end;

function TmnBufferStream.WriteLn(const S: string): Cardinal;
begin
  Result := WriteLine(S, EndOfLine);
end;

function TmnBufferStream.WriteStrings(const Value: TStrings): Cardinal;
begin
  Result := WriteStrings(Value, EndOfLine);
end;

function TmnBufferStream.WriteStrings(const Value: TStrings; const vEOL: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Value.Count - 1 do
  begin
    if Value[i] <> '' then //stupid delphi always add empty line in last of TStringList
      Result := Result + WriteLine(Value[i], vEOL);
  end;
end;

procedure TmnBufferStream.ReadCommand(var Command, Params: string);
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

function TmnBufferStream.ReadLn: string;
begin
  {$ifdef FPC}
  Result := '';
  {$endif}
  ReadLn(Result);
end;

function TmnBufferStream.ReadLine(var S: string; const vEOL: string; vExcludeEOL: Boolean = True): Boolean;
var
  aMatched: Boolean;
begin
  Result := not EOF;
  if Result then
  begin
    aMatched := False;
    ReadUntil(vEOL, S, aMatched);
    if not aMatched and EOF and (S = '') then
      Result := False
    else if aMatched and vExcludeEOL and (S <> '') then
      S := LeftStr(S, Length(S) - Length(vEOL));
  end;
end;

function TmnBufferStream.ReadLine(const vEOL: string): string; 
begin
  {$ifdef FPC}
  Result := '';
  {$endif}
  ReadLine(Result, vEOL);
end;

procedure TmnBufferStream.WriteCommand(const Command, Params: string);
begin
  if Params <> '' then
    WriteLine(Command + ' ' + Params, EndOfLine)
  else
    WriteLine(Command, EndOfLine);
end;

function TmnBufferStream.WriteEOL(EOL: string): Cardinal;
begin
  Result := WriteString(EOL);
end;

function TmnBufferStream.WriteEOL: Cardinal;
begin
  Result := WriteEOL(EndOfLine);
end;

function TmnBufferStream.ReadLn(var S: string; ExcludeEOL: Boolean = True): Boolean;
begin
  Result := ReadLine(S, EndOfLine, ExcludeEOL);
end;

procedure TmnBufferStream.ReadStrings(Value: TStrings; const vEOL: string);
var
  s: string;
begin
  while not EOF do
  begin
    {$ifdef FPC}
    s := '';
    {$endif}
    if ReadLine(s, vEOL) then
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

procedure TmnBufferStream.ReadUntil(const UntilStr: string; var Result: string; var Matched: Boolean);
var
  P: PChar;
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


