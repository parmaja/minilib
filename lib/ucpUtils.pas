unit ucpUtils;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$H+}
{$M+}

interface

uses
  SysUtils, Variants, Classes;

type
  TUnicodeCategory = (
    ucLetter,
    ucMark,
    ucNumber,
    ucSeparator,
    ucSymbol,
    ucPunctuation,
    ucOther
  );

  TUnicodeRange = record
    First: Cardinal;
    Last: Cardinal;
    Category: TUnicodeCategory;
  end;

function UnicodeCategory(Code: Cardinal): TUnicodeCategory;
function IsLetter(Code: Cardinal): Boolean;
function IsMark(Code: Cardinal): Boolean;
function IsNumber(Code: Cardinal): Boolean;
function IsSeparator(Code: Cardinal): Boolean;
function IsSymbol(Code: Cardinal): Boolean;
function IsPunctuation(Code: Cardinal): Boolean;

function CodePointAt(const Text: UnicodeString; Pos: Integer): Cardinal;
function CodePointSize(Code: Cardinal): Integer;
function CharAt(const Text: UnicodeString; Pos: Integer): UnicodeString;
function GraphemeToUserPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
function GraphemeToNextPart(const Text: UnicodeString; Pos: Integer): UnicodeString;

type
  TMBToWC_Proc = procedure(S: AnsiChar; var R: WideChar);
  TWCToMB_Proc = procedure(S: WideChar; var R: AnsiChar);

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
function ucpAnsiToUnicode(const S: AnsiString; Proc: Tmbtowc_proc): WideString; overload;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});

implementation

uses
  ucp1250; //the default code page

const
  UnicodeRanges: array[0..2248] of TUnicodeRange = (
    {$I unicode_ranges.inc}
  );

function UnicodeCategory(Code: Cardinal): TUnicodeCategory;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := Length(UnicodeRanges) - 1;
  Result := ucOther;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Code < UnicodeRanges[Mid].First then
      Hi := Mid - 1
    else if Code > UnicodeRanges[Mid].Last then
      Lo := Mid + 1
    else
    begin
      Result := UnicodeRanges[Mid].Category;
      Break;
    end;
  end;
end;

function IsLetter(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucLetter;
end;

function IsMark(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucMark;
end;

function IsNumber(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucNumber;
end;

function IsSeparator(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucSeparator;
end;

function IsSymbol(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucSymbol;
end;

function IsPunctuation(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucPunctuation;
end;

function CodePointAt(const Text: UnicodeString; Pos: Integer): Cardinal;
var
  c1, c2: WideChar;
begin
  Result := 0;
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;
  c1 := WideChar(Text[Pos]);
  if (Ord(c1) >= $D800) and (Ord(c1) <= $DBFF) then
  begin
    if Pos + 1 <= Length(Text) then
    begin
      c2 := WideChar(Text[Pos + 1]);
      if (Ord(c2) >= $DC00) and (Ord(c2) <= $DFFF) then
      begin
        Result := ((Ord(c1) - $D800) shl 10) + (Ord(c2) - $DC00) + $10000;
        Exit;
      end;
    end;
    Result := Ord(c1);
  end
  else
    Result := Ord(c1);
end;

function CodePointSize(Code: Cardinal): Integer;
begin
  if Code > $FFFF then
    Result := 2
  else
    Result := 1;
end;

function CharAt(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  cp: Cardinal;
  n: Integer;
begin
  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);
  if n = 0 then
    Result := ''
  else
    Result := Copy(Text, Pos, n);
end;

function GraphemeToUserPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  cp, cp2: Cardinal;
  n, p: Integer;
  cat: TUnicodeCategory;
begin
  Result := '';
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;

  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);

  // CRLF pair: treat CR+LF as one unit
  if (cp = $000D) and (Pos + n <= Length(Text)) then
  begin
    cp2 := CodePointAt(Text, Pos + n);
    if cp2 = $000A then
      n := n + CodePointSize(cp2);
  end;

  Result := Copy(Text, Pos, n);
  p := Pos + n;

  // Consume combining marks, ZWJ sequences, and regional indicator pairs
  while p <= Length(Text) do
  begin
    cp := CodePointAt(Text, p);
    cat := UnicodeCategory(cp);

    // Combining marks (Mn, Mc, Me) always attach
    if cat = ucMark then
    begin
      n := CodePointSize(cp);
      Result := Result + Copy(Text, p, n);
      p := p + n;
      Continue;
    end;

    // ZWJ (U+200D): join next base + its marks
    if cp = $200D then
    begin
      n := CodePointSize(cp);
      p := p + n;
      if p <= Length(Text) then
      begin
        cp := CodePointAt(Text, p);
        n := CodePointSize(cp);
        Result := Result + WideChar($200D) + Copy(Text, p, n);
        p := p + n;
        Continue;
      end
      else
        Break;
    end;

    Break;
  end;
end;

function GraphemeToNextPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  startCat, cat: TUnicodeCategory;
  cp: Cardinal;
  n, p: Integer;
  gotBase: Boolean;
begin
  Result := '';
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;

  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);
  startCat := UnicodeCategory(cp);

  // Leading marks: standalone mark run at start of part
  if startCat = ucMark then
  begin
    p := Pos + n;
    while p <= Length(Text) do
    begin
      cp := CodePointAt(Text, p);
      if UnicodeCategory(cp) <> ucMark then
        Break;
      p := p + CodePointSize(cp);
    end;
    Result := Copy(Text, Pos, p - Pos);
    Exit;
  end;

  // Consume base characters of the same category plus attached marks
  p := Pos;
  gotBase := False;
  while p <= Length(Text) do
  begin
    cp := CodePointAt(Text, p);
    n := CodePointSize(cp);
    cat := UnicodeCategory(cp);

    // Marks always attach to preceding base
    if cat = ucMark then
    begin
      p := p + n;
      Continue;
    end;

    if not gotBase then
    begin
      // First non-mark character determines the part's category
      gotBase := True;
      startCat := cat;
      p := p + n;
      Continue;
    end;

    // Same category as the base: continue
    if cat = startCat then
    begin
      p := p + n;
      Continue;
    end;

    // Category changed: stop here
    Break;
  end;

  Result := Copy(Text, Pos, p - Pos);
end;

type
  TucpConverter = record
    MBToWCProc: procedure(S: AnsiChar; var R: WideChar);
    WCToMBProc: procedure(S: WideChar; var R: AnsiChar);
  end;

var
  FConverter: TucpConverter;

{$IFDEF FPC}
procedure Ansi2WideMove(source: PAnsiChar; cp : TSystemCodePage; var dest: WideString; len:SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Wide2AnsiMove(source: PWideChar; var dest: RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;

procedure Ansi2UnicodeMove(source: PAnsiChar; cp : TSystemCodePage; var dest: UnicodeString; len: SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Unicode2AnsiMove(source: punicodechar; var dest:RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;
{$ENDIF}

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});
{$IFDEF FPC}
var
  Manager: TUnicodeStringManager;
{$ENDIF}
begin
  FConverter.MBToWCProc := MBToWCProc;
  FConverter.WCToMBProc := WCtoMBProc;
{$IFDEF FPC}
  if Hook then
  begin
    GetWideStringManager(Manager);
    Manager.Ansi2WideMoveProc := Ansi2WideMove;
    Manager.Wide2AnsiMoveProc := Wide2AnsiMove;
    Manager.Ansi2UnicodeMoveProc := Ansi2UnicodeMove;
    Manager.Unicode2AnsiMoveProc := Unicode2AnsiMove;
    SetWideStringManager(Manager);
  end;
{$ENDIF}
end;

function ucpAnsiToUnicode(const s: AnsiString; Proc: Tmbtowc_proc): WideString; overload;
var
  i: Integer;
  r: WideChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('AnsiToUnicode: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
begin
  Result := ucpAnsiToUnicode(S, FConverter.MBToWCProc);
end;

function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;
var
  i: Integer;
  r: AnsiChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('UnicodeToAnsi: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
begin
  Result := ucpUnicodeToAnsi(S, FConverter.WCToMBProc);
end;

initialization
  FConverter.MBToWCProc := cp1250_mbtowc;
  FConverter.WCToMBProc := cp1250_wctomb;
end.

