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

function DequoteStr(Str: string): string;
function QuoteStr(Str: string; QuoteChar: string = '"'): string;
function StrToStrings(Content: string; Strings: TStrings; Separators:TSysCharSet; WhiteSpace: TSysCharSet = [#0, #13, #10]; DequoteValues: Boolean = False; Quotes: TSysCharSet = ['''', '"']): Integer;
function ExpandToPath(FileName: string; Path: string; Root:string = ''): string;

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

end.
