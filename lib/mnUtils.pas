unit mnUtils;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils;

const
  sUTF8BOM: array[1..3] of Char = (#$EF, #$BB, #$BF);
{
  StrHave: test the string if it have Separators
}
function StrHave(S: string; Separators: TSysCharSet): Boolean;

function QuoteStr(Str: string; QuoteChar: string = '"'): string;

type
  TStrToStringsCallbackProc = procedure(Sender: Pointer; S: string);

function StrToStringsCallback(Sender: Pointer; Proc: TStrToStringsCallbackProc; Content: string; Separators: TSysCharSet; WhiteSpace: TSysCharSet = [#0, #13, #10]; DequoteValues: Boolean = False; Quotes: TSysCharSet = ['''', '"']): Integer;
function StrToStrings(Content: string; Strings: TStrings; Separators: TSysCharSet; WhiteSpace: TSysCharSet = [#0, #13, #10]; DequoteValues: Boolean = False; Quotes: TSysCharSet = ['''', '"']): Integer;
function CompareLeftStr(const Str: string; const WithStr: string; Start: Integer=1): Boolean;
//Index started from 1
function SubStr(const Str: String; vSeperator: Char; vIndex: Integer = 1): String; overload;
function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String; overload;

function PeriodToString(vPeriod: Double; WithSeconds:Boolean): string;
function DequoteStr(Str: string; QuoteChar: string): string; overload;
function DequoteStr(Str: string): string; overload; //deqoute use both of ' and "
function LeadLeft(const vStr: string; Count: Integer; vChar: Char): string; overload;
function LeadRight(const vStr: string; Count: integer; vChar: Char): string; overload;
function LeadLeft(const I: Integer; Count: Integer; vChar: Char): string; overload;
function LeadRight(const I: Integer; Count: integer; vChar: Char): string; overload;

{
  Break string to Strings list items at #10 or #13 or #13#10 
}

type
  TBreakToStringsCallBack = procedure(S: string; vStrings: TStrings);

procedure BreakToStrings(S: string; vStrings: TStrings; IncludeLineBreaks: Boolean = False; CallBackProc: TBreakToStringsCallBack = nil);

{
  Usfull to make your project path related
}
function ExpandToPath(FileName: string; Path: string; Root: string = ''): string;

{
  EscapeString: Example
    EscapeString(Text, '\', [#13, #10, #9 , #8, '"'], ['r', 'n', 't', 'b', '"']);

  DescapeString: Reverse of EscapeString with same params
  Both functions is case sensitive

  sBS = #8 = \b
  sTAB = #9 = \t
  sLF = #10 = \n
  sCR = #13 = \r
  sCRLF = #13#10 \r\n
}

function EscapeString(const S: string; Esc: string; Chars: array of AnsiChar; Escapes: array of string): string;
function DescapeString(const S: string; Esc: string; Chars: array of AnsiChar; Escapes: array of string): string;

//Similer to ZeroMemory
procedure InitMemory(out V; Count: {$ifdef FPC}SizeInt{$else}Longint{$endif});

{$ifndef FPC}
const
{$ifdef WINDOWS}
  DirectorySeparator: string = '\';
{$else}
  DirectorySeparator: string = '/';
{$endif}
{$endif}

implementation

function StrHave(S:string; Separators: TSysCharSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do
  begin
     if S[i] in Separators then
     begin
       Result := True;
       Break; 
     end;
  end;
end;

function QuoteStr(Str, QuoteChar: string): string;
begin
  if Str = '' then
    Result := QuoteChar + QuoteChar
  else
  begin
    if Str[1] = '"' then
    begin
      if Str[Length(Str)] <> '"' then
        Result := Result + '"'
    end
    else if Str[1] = '''' then
    begin
      if Str[Length(Str)] <> '''' then
        Result := Result + '''';
    end
    else
      Result := QuoteChar + Str + QuoteChar;
  end;
end;

function DequoteStr(Str: string): string;
begin
  if Str = '' then
    Result := ''
  else
  begin
    if Str[1] = '"' then
    begin
      if Str[Length(Str)] = '"' then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else if Str[1] = '''' then
    begin
      if Str[Length(Str)] = '''' then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else
      Result := Str;
  end;
end;

function LeadLeft(const vStr: string; Count: Integer; vChar: Char): string;
var
  l: integer;
begin
  l := Length(vStr);
  if l < Count then
    Result := vStr + StringOfChar(vChar, Count  - l)
  else
    Result := vStr;
end;

function LeadRight(const vStr: string; Count: integer; vChar: Char): string;
var
  l: integer;
begin
  l := Length(vStr);
  if l < Count then
    Result := StringOfChar(vChar, Count - l) + vStr
  else
    Result := vStr;
end;

function LeadLeft(const I: Integer; Count: Integer; vChar: Char): string;
begin
  Result := LeadLeft(IntToStr(I), Count, vChar);
end;

function LeadRight(const I: Integer; Count: integer; vChar: Char): string;
begin
  Result := LeadRight(IntToStr(I), Count, vChar);
end;

function DequoteStr(Str: string; QuoteChar: string): string;
begin
  if Str = '' then
    Result := ''
  else
  begin
    if Str[1] = QuoteChar then
    begin
      if Str[Length(Str)] = QuoteChar then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else
      Result := Str;
  end;
end;

{
for example 4 fields
f1,f2,f3,f4
f1,f2,f3,
f1,f2,,f4
,f2,f3,f4
}

function StrToStringsCallback(Sender: Pointer; Proc: TStrToStringsCallbackProc; Content: string; Separators, WhiteSpace: TSysCharSet; DequoteValues: Boolean; Quotes: TSysCharSet): Integer;
var
  Start, Cur, P: Integer;
  InQuote: Boolean;
  QuoteChar: Char;
  S: string;
begin
  Result := 0;
  if (@Proc = nil) then
    raise Exception.Create('StrToStrings: Proc is nil');
  if (Content <> '') then
  begin
    Cur := 1;
    InQuote := False;
    QuoteChar := #0;
    repeat
      //bypass white spaces
      while (Cur <= Length(Content)) and (Content[Cur] in WhiteSpace) do
        Cur := Cur + 1;

      //start from the first char
      Start := Cur - 1;
      while True do
      begin
        //seek until the separator and skip the separator if inside quoting
        while (Cur <= Length(Content)) and ((InQuote and not (Content[Cur] <> QuoteChar)) or (not (Content[Cur] in Separators))) do
          Cur := Cur + 1;
        if (Cur <= Length(Content)) and (Content[Cur] in Quotes) then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Content[Cur]) then
            QuoteChar := #0
          else if QuoteChar = #0 then
            QuoteChar := Content[Cur];
          InQuote := QuoteChar <> #0;
          Cur := Cur + 1;
        end
        else
          Break;
      end;

      if (Cur >= Start) then
      begin
        S := Copy(Content, Start + 1, Cur - Start - 1);
        if DequoteValues then
        begin
          P := Pos('=', S);
          if P > 0 then
            S := Copy(S, 1, P) + DequoteStr(Copy(S, P + 1, MaxInt));
        end;
        Proc(Sender, S);
        Inc(Result);
      end;
      Cur := Cur + 1;
    until Cur > Length(Content) + 1;
  end;
end;

procedure StrToStringsCallbackProc(Sender: Pointer; S: string);
begin
  TStrings(Sender).Add(S); //Be sure sender is TStrings
end;

function StrToStrings(Content: string; Strings: TStrings; Separators: TSysCharSet; WhiteSpace: TSysCharSet = [#0, #13, #10]; DequoteValues: Boolean = False; Quotes: TSysCharSet = ['''', '"']): Integer;
begin
  if (Strings = nil) then
    raise Exception.Create('StrToStrings: Strings is nil');
  Strings.BeginUpdate;
  try
    Result := StrToStringsCallback(Strings, @StrToStringsCallbackProc, Content, Separators, WhiteSpace, DequoteValues, Quotes);
  finally
    Strings.EndUpdate;
  end;
end;

function ExpandToPath(FileName: string; Path: string; Root: string): string;
begin
  if (FileName <> '') and ((LeftStr(FileName, 3) = '../') or (LeftStr(FileName, 3) = '..\')) then
    Result := ExpandFileName(IncludeTrailingPathDelimiter(Root) + IncludeTrailingPathDelimiter(Path) + FileName)
  else if (FileName <> '') and ((LeftStr(FileName, 2) = './') or (LeftStr(FileName, 2) = '.\')) then
    Result := IncludeTrailingPathDelimiter(Root) + IncludeTrailingPathDelimiter(Path) + RightStr(FileName, Length(FileName) - 2)
  else if (FileName <> '') and (LeftStr(FileName, 2) <> '\\') and ((LeftStr(FileName, 1) = '/') or (LeftStr(FileName, 1) = '\')) then
    Result := ExtractFileDrive(Path) + FileName
  else if ExtractFilePath(FileName) = '' then
    Result := IncludeTrailingPathDelimiter(Path) + FileName
  else
    Result := FileName;
end;

procedure cMoveStr(var Start: Integer; var Dest: string; const Source: string);
var
  i, l: Integer;
begin
  l := (Length(Dest) - Start) + 1;
  if l > Length(Source) then
    l := Length(Source);
  for i := 1 to l do
  begin
    Dest[Start] := Source[i];
    Start := Start + 1;
  end;
end;

function CompareLeftStr(const Str: string; const WithStr: string; Start: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if (Length(Str) - Start + 1) < Length(WithStr) then
  begin
    Result := False;
    exit;
  end;
  for i := 1 to Length(WithStr) do
  begin
    if Str[Start + i - 1] <> WithStr[i] then
    begin
      Result := False;
      break;
    end;
  end;
end;

function EscapeString(const S: string; Esc: string; Chars: array of AnsiChar; Escapes: array of string): string;
  function InChars(const Char: AnsiChar): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Chars) - 1 do
    begin
      if Char = Chars[i] then
      begin
        Result := i;
        break;
      end;
    end;
  end;
var
  i: Integer;
  NewLength: Integer;
  p, c: Integer;
  NeedEscape: Boolean;
begin
  if Length(Chars) <> Length(Escapes) then
    raise Exception.Create('Chars and Escapes not identical');
  NeedEscape := False;
  NewLength := Length(S);
  for i := 1 to Length(S) do
  begin
    p := InChars(S[i]);
    if p >= 0 then
    begin
      NewLength := NewLength - 1 + Length(Esc) + Length(Escapes[p]);
      NeedEscape := True;
    end;
  end;
  if not NeedEscape then
    Result := S
  else
  begin
    SetLength(Result, NewLength);
    c := 1;
    for i := 1 to Length(S) do
    begin
      p := InChars(S[i]);
      if p >= 0 then
      begin
        cMoveStr(c, Result, Esc);
        cMoveStr(c, Result, Escapes[p])
      end
      else
      begin
        Result[c] := S[i];
        c := c + 1;
      end;
    end;
  end;
end;

function DescapeString(const S: string; Esc: string; Chars: array of AnsiChar; Escapes: array of string): string;
  function InEscape(Start: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Escapes) - 1 do
    begin
      if CompareLeftStr(S, Escapes[i], Start) then
      begin
        Result := i;
        break;
      end;
    end;
  end;
var
  i: Integer;
  NewLength: Integer;
  p, c: Integer;
  NeedDescape: Boolean;
begin
  if Length(Chars) <> Length(Escapes) then
    raise Exception.Create('Chars and Escapes not identical');
  NeedDescape := False;
  NewLength := Length(S);
  i := 1;
  while i <= Length(S) do
  begin
    if CompareLeftStr(S, Esc, i) then
    begin
      i := i + Length(Esc);
      p := InEscape(i);
      if p >= 0 then
      begin
        NewLength := NewLength - Length(Esc) - Length(Escapes[p]) + 1;
        NeedDescape := True;
        i := i + Length(Escapes[p]);
      end;
    end
    else
      i := i + 1;
  end;
  if not NeedDescape then
    Result := S
  else
  begin
    SetLength(Result, NewLength);
    c := 1;
    i := 1;
    while i <= Length(S) do
    begin
      if CompareLeftStr(S, Esc, i) then
      begin
        i := i + Length(Esc);
        p := InEscape(i);
        if p >= 0 then
        begin
          cMoveStr(c, Result, Chars[p]);
          i := i + Length(Escapes[p]);
        end
        else
          cMoveStr(c, Result, Esc);
      end
      else
      begin
        Result[c] := S[i];
        c := c + 1;
        i := i + 1;
      end;
    end;
  end;
end;

procedure BreakToStrings(S: string; vStrings: TStrings; IncludeLineBreaks: Boolean; CallBackProc: TBreakToStringsCallBack);
var
  t: string;
  i, j, l: Integer;
  LB: Integer;
  procedure AddIt;
  begin
    if Assigned(CallBackProc) then
      CallBackProc(t, vStrings)
    else
      vStrings.Add(t);
  end;
begin
  l := Length(S);
  i := 1;
  j := 1;
  while (i <= l) do
  begin
    if S[i] in [#13, #10] then
    begin
      if IncludeLineBreaks then
        LB := 0
      else
        LB := 1;
      if (S[i] = #13) and (i < l) and (S[i + 1] = #10) then
      begin
        inc(i);
        if not IncludeLineBreaks then
          Inc(LB);
      end;
      t := MidStr(S, j, i - j - LB + 1);
      j := i + 1;
      AddIt;
    end;
    inc(i);
  end;
  if j < l then
  begin
    t := MidStr(S, j, l - j + 1);
    AddIt;
  end;
end;

function PeriodToString(vPeriod: Double; WithSeconds:Boolean): string;
var
  h, m, s: integer;
  d: Integer;
  g: Boolean;
  function LeadToRight(const vStr: string; Count: integer; vChar: Char): string;
  var
    l: integer;
  begin
    l := Length(vStr);
    if l < Count then
    begin
      Result := StringOfChar(vChar, Count - l) + vStr;
    end
    else
      Result := vStr;
  end;
begin
  g := vPeriod < 0;
  vPeriod := abs(vPeriod);
  d := trunc(vPeriod * SecsPerDay);
  h := d div 3600;
  d := (d  - (h *  3600));
  m := d div 60;
  s := (d  - (m *  60));
  Result := LeadToRight(IntToStr(h), 2, '0') + TimeSeparator + LeadToRight(IntToStr(m), 2, '0');
  if WithSeconds then
  begin
    if s = 0 then
      Result := Result + TimeSeparator + '00'
    else
      Result := Result + TimeSeparator + LeadToRight(IntToStr(s), 2, '0');
  end;
  if g then
    Result := '-' + Result;
end;

procedure InitMemory(out V; Count: {$ifdef FPC}SizeInt{$else}Longint{$endif});
begin
  {$ifdef FPC}
  {$PUSH}
  {$HINTS OFF}
  {$endif}
  FillChar(V, Count, #0);
  {$ifdef FPC}
  {$POP}
  {$endif}
end;

function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String;
var
  Index, B, E: Integer;
begin
  Result := '';
  Index := 0;
  B := 1;
  for E := 1 to Length(Str) do
  begin
    if Str[E] = vSeperator then
    begin
      Inc(Index);
      if Index = vFromIndex - 1 then
        B := E + 1;
    end;

    if Index = vToIndex then
    begin
      Result := Copy(Str, B, E - B);
      exit;
    end;
  end;
  Result := Copy(Str, B, MaxInt);
end;

function SubStr(const Str: String; vSeperator: Char; vIndex: Integer): String;
begin
  Result := SubStr(Str, vSeperator, vIndex, vIndex);
end;

end.

