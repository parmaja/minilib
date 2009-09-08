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
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils;

const
  sUTF8BOM: array[1..3] of Char = (#$EF, #$BB, #$BF);

function DequoteStr(Str: string; QuoteChar: string): string;
function DequoteStr(Str: string): string; //deqoute use both of ' and "
function QuoteStr(Str: string; QuoteChar: string = '"'): string;
function StrToStrings(Content: string; Strings: TStrings; Separators: TSysCharSet; WhiteSpace: TSysCharSet = [#0, #13, #10]; DequoteValues: Boolean = False; Quotes: TSysCharSet = ['''', '"']): Integer;
function ExpandToPath(FileName: string; Path: string; Root:string = ''): string;

{
  EscapeString: Example
    EscapeString(Text, '\', [#13, #10, #9 , #8, '"'], ['r', 'n', 't', 'b', '"']);

  DescapeString: Reverse of EscapeString with same params
  Both functions is case sensitive
}

function EscapeString(const S: string; Esc: string; Chars:array of AnsiChar; Escapes: array of string): string;
function DescapeString(const S: string; Esc: string; Chars:array of AnsiChar; Escapes: array of string): string;

implementation

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

function StrToStrings(Content: string; Strings: TStrings; Separators, WhiteSpace: TSysCharSet; DequoteValues: Boolean; Quotes: TSysCharSet): Integer;
var
  Head, Tail, P: Integer;
  InQuote: Boolean;
  QuoteChar: Char;
  S: string;
begin
  Result := 0;
  if (Strings = nil) then
    raise Exception.Create('StrToStrings: Strings is nil');
  if (Content <> '') then
  begin
    Tail := 1;
    InQuote := False;
    QuoteChar := #0;
    Strings.BeginUpdate;
    try
      repeat
        while (Tail <= Length(Content)) and (Content[Tail] in WhiteSpace) do
          Tail := Tail + 1;
        Head := Tail;
        while True do
        begin
          while (Tail <= Length(Content)) and ((InQuote and not (Content[Tail] <> QuoteChar)) or (not (Content[Tail] in Separators))) do
            Tail := Tail + 1;
          if (Tail <= Length(Content)) and (Content[Tail] in Quotes) then
          begin
            if (QuoteChar <> #0) and (QuoteChar = Content[Tail]) then
              QuoteChar := #0
            else if QuoteChar = #0 then
              QuoteChar := Content[Tail];
            InQuote := QuoteChar <> #0;
            Tail := Tail + 1;
          end
          else
            Break;
        end;
        if (Tail <> Head) then
        begin
          if Strings <> nil then
          begin
            S := Copy(Content, Head, Tail - Head);
            if DequoteValues then
            begin
              P := Pos('=', S);
              if P > 0 then
                S := Copy(S, 1, P) + DequoteStr(Copy(S, P + 1, MaxInt));
            end;
            Strings.Add(S);
          end;
          Inc(Result);
        end;
        Tail := Tail + 1;
      until Tail > Length(Content);
    finally
      Strings.EndUpdate;
    end;
  end;
end;

function ExpandToPath(FileName: string; Path: string; Root:string): string;
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

procedure cMoveStr(var Start:Integer; var Dest: string; const Source: string);
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

function cCompareStr(Start: Integer; const Str: string; const WithStr: string): Boolean;
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

function EscapeString(const S: string; Esc: string; Chars:array of AnsiChar; Escapes: array of string): string;
  function InChars(const Char: AnsiChar): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Chars) -1 do
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
      NeedEscape:=True;
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

function DescapeString(const S: string; Esc: string; Chars:array of AnsiChar; Escapes: array of string): string;
  function InEscape(Start: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Escapes) -1 do
    begin
      if cCompareStr(Start, S, Escapes[i]) then
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
    if cCompareStr(i, S, Esc) then
    begin
      i := i + Length(Esc);
      p := InEscape(i);
      if p >= 0 then
      begin
        NewLength := NewLength - Length(Esc) - Length(Escapes[p]) + 1;
        NeedDescape:=True;
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
      if cCompareStr(i, S, Esc) then
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

end.
