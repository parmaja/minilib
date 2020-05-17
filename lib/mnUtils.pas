unit mnUtils;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{$ifdef mswindows}
{$define windows}
{$endif}

interface

uses
  {$ifdef windows}Windows, {$endif}
  Classes, SysUtils, StrUtils, DateUtils, Types;

const
  sUTF8BOM: array[1..3] of Char = (#$EF, #$BB, #$BF);
  {$ifdef FPC}
  {$else}
  JulianEpoch = TDateTime(-2415018.5);  //check EpochAsJulianDate
  {$endif}

{$if defined(fpc) and (fpc_fullversion<30000)} //temporary until fpc3 released
type
  PUTF8Char = PAnsiChar;
{$endif}

procedure Nothing;
{
  StrHave: test the string if it have Separators
}
function StrHave(S: string; Separators: TSysCharSet): Boolean; deprecated;

function QuoteStr(Str: string; QuoteChar: string = '"'): string;

{**
  Resume: if false then stop, default is true
*}
type
  TStrToStringsCallbackProc = procedure(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
  TArgumentsCallbackProc = procedure(Sender: Pointer; Index:Integer; Name, Value: string; var Resume: Boolean);

{**
  IgnoreInitialWhiteSpace: Ignore the first chars of this white space, not need it
*}

TStrToStringsOptions = set of (
  stsoGroupSeparators //TODO, not tested yet, if one of separators come it will ignores chars in next separators like if separators = #13, #10, now #13#10 will considered as one line break
); //

function StrToStringsCallback(Content: string; Sender: Pointer; const CallBackProc: TStrToStringsCallbackProc; Separators: TSysCharSet = [#0, #13, #10]; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']; vOptions: TStrToStringsOptions = []): Integer;
function StrToStrings(Content: string; Strings: TStrings; Separators: TSysCharSet = [#0, #13, #10]; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Integer;

procedure StrToStringsCallbackProc(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
procedure StrToStringsDeqouteCallbackProc(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);

{
  examples:
  -t -s -v: value test

  -t -s --value: "value test"

}

function ParseArgumentsCallback(Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer; Switches: array of char; WhiteSpaces: TSysCharSet = [' ', #9]; Quotes: TSysCharSet = ['''', '"'];  ValueSeperators: TSysCharSet = [':', '=']): Integer; overload;
function ParseArgumentsCallback(Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer): Integer; overload;
function ParseArguments(Content: string; Strings: TStrings; Switches: TArray<Char>; WhiteSpaces: TSysCharSet = [' ', #9]; Quotes: TSysCharSet = ['''', '"']; ValueSeperators: TSysCharSet = [':', '=']): Integer; overload;
function ParseArguments(Content: string; Strings: TStrings): Integer; overload;

function StringsToString(Strings: TStrings; LineBreak: string = sLineBreak): string;

function CompareLeftStr(const Str: string; const WithStr: string; Start: Integer = 1): Boolean;
function ContainsText(const SubStr, InStr: string): Boolean; //TODO, need one scan, not using uppercase

//Copy, MidStr but From To index
function SubStr(const AText: String; const AFromIndex, AToIndex: Integer): String; overload;
//Index started from 0
function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String; overload;
function SubStr(const Str: String; vSeperator: Char; vIndex: Integer = 0): String; overload;

function FetchStr(var AInput: string; const ADelim: string = '.'; const ADelete: Boolean = True; const ACaseSensitive: Boolean = True): string; deprecated;

function StrInArray(const Str: String; const ArrayOfString : Array of String; CaseInsensitive: Boolean = False) : Boolean;
function CharInArray(const C: string; const ArrayOfChar : array of Char; CaseInsensitive: Boolean = False) : Boolean;

//vPeriod is a datetime not tickcount
function PeriodToString(vPeriod: Double; WithSeconds: Boolean): string;
//Used by GetTickCount
function TicksToString(vTicks: Int64): string;
function DequoteStr(Str: string; QuoteChar: string = #0): string;

function RepeatString(const Str: string; Count: Integer): string;
{* VarReplace
  VarChar = '$'
  Example: VarReplace('c:\$project\$[name]';
}
function VarReplace(S: string; Values: TStrings; VarChar: Char): string;

type
  //alsCut = if the string > count we cut it as count or keep the string
  TAlignStrOptions = set of (alsLeft, alsRight, alsCut); {TODO left+right=center TODO use righttoleft}

function AlignStr(const S: string; Count: Integer; Options: TAlignStrOptions = [alsLeft]; vChar: Char = ' '): string; overload;

{
  Useful to make your project path related (Portable)
  FileName:
          ./myfile
          ../myfile
          /myfile
          \myfile
  Path:
  Root: is optional, added before Path
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

const
  cDefEscapeChars =  [#8, #9, #10, #13, '\', '"'];
  cDefEscapeStrings = ['b', 't', 'n', 'r', '\', '"'];
  cDefEscapePrefix = '\';

function EscapeString(const S: string; Esc: string; Chars: array of Char; Escapes: array of string): string;
function DescapeString(const S: string; Esc: string; Chars: array of Char; Escapes: array of string): string;

function EscapeStringC(const S: string): string;
function DescapeStringC(const S: string): string;
function ToUnixPathDelimiter(const S: string): string;

//TODO pascal
//function EscapeStringPas(const S: string): string;
//function DescapeStringPas(const S: string): string;

//IncludePathSeparator add the Delimiter when S not = ''
function IncludePathSeparator(const S: string): string;

//Similer to ZeroMemory
procedure InitMemory(out V; Count: {$ifdef FPC}SizeInt{$else}Longint{$endif});

//Rect functions
procedure CenterRect(var R1: TRect; R2: TRect);

function GetFormatSettings: TFormatSettings;


{$ifdef FPC}
{$else}
const
{$ifdef MSWINDOWS}
  DirectorySeparator: string = '\';
{$else}
  DirectorySeparator: string = '/';
{$endif}
{$endif}

//Ported from UniDates

procedure ISOStrToDate(ISODate: String; out Y, M, D, H, N, S: Word; vDateSeparator: Char = '-'; TimeDivider: Char = #0; UseDefault: Boolean = False); overload;
function ISOStrToDate(ISODate: String; vDateSeparator: Char = '-'; TimeDivider: Char = #0; UseDefault: Boolean = False): TDateTime; overload;

function ISODateToStr(DateTime: TDateTime; vDateSeparator: Char = '-'; TimeDivider: Char = ' '; WithTime: Boolean = False): String; overload;

function AnsiToUnicode(S: rawbytestring; CodePage: Integer = 0): string;
function StringAs(S: rawbytestring; CodePage: Integer = 0): utf8string;

//Zero Based
function StringOf(const Value: Array of Byte): string; overload;
function StringOf(const Value: TBytes): string; overload;

//Files Utils

type
  //If set Resume to false it will stop loop
  TEnumFilesCallback = procedure(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);

procedure EnumFiles(FileList: TStringList; Folder, Filter: string; FullPath: Boolean = False);

var
  SystemAnsiCodePage: Integer; //used to convert from Ansi string, it is the default

implementation

{$ifdef FPC}
{$else}
  {$if CompilerVersion < 22}
  var
    FFormatSettings: TFormatSettings;
  {$ifend}
{$endif}

procedure Nothing;
begin
end;

function StrHave(S: string; Separators: TSysCharSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do
  begin
     if CharInSet(S[i], Separators) then
     begin
       Result := True;
       Break; 
     end;
  end;
end;

function QuoteStr(Str: string; QuoteChar: string): string;
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

function DequoteStr(Str: string; QuoteChar: string = #0): string;
begin
  if Str = '' then
    Result := ''
  else
  begin
    if (QuoteChar > #0) and (Str[1] = QuoteChar) then
    begin
      if Str[Length(Str)] =QuoteChar then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else if Str[1] = '"' then
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

function AlignStr(const S: string; Count: Integer; Options: TAlignStrOptions; vChar: Char): string;
var
  l: integer;
begin
  l := Length(S);
  if l >= Count then
  begin
    if alsCut in Options then
      Result := LeftStr(S, Count)
    else
    begin
      Result := S;
      exit;//or cut
    end;
  end;
  if (alsLeft in Options) and (alsRight in Options) then
    Result := StringOfChar(vChar, Count div 2) + S + StringOfChar(vChar, Count - (Count div 2) - l) //the rest if div used in right
  else if alsLeft in Options then
    Result := S + StringOfChar(vChar, Count  - l)
 else if alsRight in Options then
    Result := StringOfChar(vChar, Count  - l) + S
end;

function RepeatString(const Str: string; Count: Integer): string;
begin
  Result := '';
  while Count > 0 do
  begin
    Result := Result + Str;
    Count := Count - 1;
  end;
end;

{**
*  Replace multipe variables $var for example with value in Values
*  Use name values in strings
*}

function VarReplace(S: string; Values: TStrings; VarChar: Char): string;
var
  i: Integer;
  OpenAt: Integer;
  n: string;
  l: integer;
  procedure check;
  begin
    l := i - OpenAt;
    n := MidStr(S, OpenAt + 1, l - 1);
    if Values.IndexOfName(n)>=0 then
    begin
      n := Values.Values[n];
      S := MidStr(S, 1, OpenAt - 1) + n + MidStr(S, i, MaxInt);
      i := i - l + length(n);
      OpenAt := 0;
    end;
  end;
begin
  OpenAt := 0; //or -1 in other languages
  i := 1;
  while i <= length(s) do
  begin
    if S[i] = VarChar then
      OpenAt := i
    else if (OpenAt > 0) then
    begin
      if not(CharInSet(S[i], ['0'..'9', 'a'..'z', 'A'..'Z', '_', '[', ']'])) then
      begin
        check;
      end;
    end;
    i:= i + 1;
  end;
  check;
  Result := S;
end;

{
for example 4 fields
f1,f2,f3,f4
f1,f2,f3,
f1,f2,,f4
,f2,f3,f4
}

procedure StrToStringsCallbackProc(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
begin
  TStrings(Sender).Add(S); //Be sure sender is TStrings
end;

procedure StrToStringsDeqouteCallbackProc(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  if p >= 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := DequoteStr(Copy(s, p + 1, MaxInt));
  end
  else
  begin
    Name := S;
    Value := '';
  end;
  (TObject(Sender) as TStrings).Add(Name + (TObject(Sender) as TStrings).NameValueSeparator + Value);
end;

function StrToStringsCallback(Content: string; Sender: Pointer; const CallBackProc: TStrToStringsCallbackProc; Separators: TSysCharSet; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet; vOptions: TStrToStringsOptions): Integer;
var
  Start, Cur: Integer;
  Resume: Boolean;
  InQuote: Boolean;
  QuoteChar: Char;
  S: string;
  Index: Integer;
begin
  Result := 0;
  Index := 0;
  if (@CallBackProc = nil) then
    raise Exception.Create('StrToStrings: CallBackProc is nil');
  if (Content <> '') then
  begin
    Cur := 1;
    InQuote := False;
    QuoteChar := #0;
    repeat
      //bypass white spaces
      if IgnoreInitialWhiteSpace <> [] then
        while (Cur <= Length(Content)) and CharInSet(Content[Cur], IgnoreInitialWhiteSpace) do
          Cur := Cur + 1;

      //start from the first char
      Start := Cur - 1;
      while True do
      begin
        //seek until the separator and skip the separator if inside quoting
        while (Cur <= Length(Content)) and ((InQuote and not (Content[Cur] <> QuoteChar)) or (not (CharInSet(Content[Cur], Separators)))) do
          Cur := Cur + 1;

        if (Cur <= Length(Content)) and CharInSet(Content[Cur], Quotes) then
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
        Resume := True;
        CallBackProc(Sender, Index, S, Resume);
        Index := Index + 1;
        Inc(Result);
        if not Resume then
          break;
      end;
      Cur := Cur + 1;
    until Cur > Length(Content) + 1;
  end;
end;

function StrToStrings(Content: string; Strings: TStrings; Separators: TSysCharSet; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet): Integer;
begin
  if (Strings = nil) then
    raise Exception.Create('StrToStrings: Strings is nil');
  Strings.BeginUpdate;
  try
    Result := StrToStringsCallback(Content, Strings, StrToStringsCallbackProc, Separators, IgnoreInitialWhiteSpace, Quotes);
  finally
    Strings.EndUpdate;
  end;
end;

procedure ArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; var Resume: Boolean);
begin
  (TObject(Sender) as TStrings).Add(Name + (TObject(Sender) as TStrings).NameValueSeparator + Value);
end;

//  -t   --test cmd1 cmd2 -t: value -t:value -t value
function ParseArgumentsCallback(Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer; Switches: array of char; WhiteSpaces: TSysCharSet; Quotes: TSysCharSet; ValueSeperators: TSysCharSet): Integer;
var
  Start, Cur: Integer;
  Resume: Boolean;
  Index: Integer;
  Name: string;
  Value: string;
  QuoteChar: Char;
  S: string;
  NextIsValue: Boolean;
begin
  Result := 0;
  Index := 0;
  Name := '';
  Value := '';
  NextIsValue := False;
  if (@CallBackProc = nil) then
    raise Exception.Create('ParseArguments: CallBackProc is nil');
  if (Content <> '') then
  begin
    Cur := 1;
    repeat
      //bypass white spaces
      while (Cur <= Length(Content)) and CharInSet(Content[Cur], WhiteSpaces) do
        Cur := Cur + 1;

      //start from the first char
      Start := Cur - 1;
      QuoteChar := #0;

      if (Cur <= Length(Content)) and CharInSet(Content[Cur], Quotes) then
      begin
        QuoteChar := Content[Cur];
        Cur := Cur + 1;
      end;

      while (Cur <= Length(Content)) and (not CharInSet(Content[Cur], WhiteSpaces) or (QuoteChar <> #0)) do
      begin
        if (QuoteChar <> #0) then
        begin
          if (Content[Cur] = QuoteChar) then
          begin
            Cur := Cur + 1;
            break;
          end;
        end
        else if not NextIsValue and CharInSet(Content[Cur], ValueSeperators) then
        begin
          Cur := Cur + 1;
          break;
        end;

        Cur := Cur + 1;
      end;

      if (Cur >= Start) then
      begin
        S := Copy(Content, Start + 1, Cur - Start - 1);
        if S <> '' then
        begin
          if NextIsValue then
          begin
            if CharInArray(S[1], Switches) then
              raise Exception.Create('Value excepted not a switch');
            Value := S;
            NextIsValue := False;
          end
          else
          begin
            Name := S;
            Value := '';
            if CharInArray(Name[1], Switches) and CharInSet(Name[Length(Name)], ValueSeperators) then
            begin
              Name := Copy(Name, 1, Length(Name) -1);
              NextIsValue := True;
            end
            else
              NextIsValue := False;
          end;

          if not NextIsValue then
          begin
            Resume := True;
            if CharInArray(Name[1], Switches) then
            begin
              if (Name[1] = Name[2]) then
                Name := Copy(Name, 2, Length(Name)); //change double switch to one switch
              if Name[1] <> Switches[0] then //should be first element in Switches, but i cant convert it to array right now
                Name[1] := Switches[0];
            end;
            CallBackProc(Sender, Index, DequoteStr(Name), DequoteStr(Value), Resume);
            Index := Index + 1;
            Inc(Result);
            if not Resume then
              break;
          end;
         end;
      end;
      //Cur := Cur + 1;
    until Cur > Length(Content);
  end;
end;

function ParseArgumentsCallback(Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer): Integer; overload;
begin
  Result := ParseArgumentsCallback(Content, @CallBackProc, Sender, ['-', '/']);
end;

function ParseArguments(Content: string; Strings: TStrings; Switches: TArray<Char>; WhiteSpaces: TSysCharSet; Quotes: TSysCharSet; ValueSeperators: TSysCharSet): Integer;
begin
  Result := ParseArgumentsCallback(Content, @ArgumentsCallbackProc, Strings, Switches, WhiteSpaces, Quotes, ValueSeperators);
end;

function ParseArguments(Content: string; Strings: TStrings): Integer;
begin
  Result := ParseArgumentsCallback(Content, @ArgumentsCallbackProc, Strings);
end;

function ExpandToPath(FileName: string; Path: string; Root: string): string;
begin
  if (FileName <> '') then
  begin
    if ((LeftStr(FileName, 3) = '../') or (LeftStr(FileName, 3) = '..\')) then
      Result := ExpandFileName(IncludePathSeparator(Root) + IncludePathSeparator(Path) + FileName)
    else if ((LeftStr(FileName, 2) = './') or (LeftStr(FileName, 2) = '.\')) then
      Result := IncludePathSeparator(Root) + IncludePathSeparator(Path) + RightStr(FileName, Length(FileName) - 2)
    else if (LeftStr(FileName, 2) <> '\\') and ((LeftStr(FileName, 1) = '/') or (LeftStr(FileName, 1) = '\')) then
      Result := ExtractFileDrive(Path) + FileName
    else if ExtractFileDrive(FileName) = '' then
      Result := IncludePathSeparator(Path) + FileName
    else
      Result := FileName;
  end
  else
    Result := '';
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
  end
  else
  begin
    for i := 1 to Length(WithStr) do
    begin
      if Str[Start + i - 1] <> WithStr[i] then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;

function ContainsText(const SubStr, InStr: string): Boolean;
begin
  Result := Pos(UpperCase(SubStr), UpperCase(InStr)) > 0; //Ewww
end;

function EscapeString(const S: string; Esc: string; Chars: array of Char; Escapes: array of string): string;
  function InChars(const Char: Char): Integer;
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

function DescapeString(const S: string; Esc: string; Chars: array of Char; Escapes: array of string): string;
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

function StringsToString(Strings: TStrings; LineBreak: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Strings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + LineBreak;
    Result := Result + Strings[i];
  end;
end;

function PeriodToString(vPeriod: Double; WithSeconds: Boolean): string;
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
  Result := LeadToRight(IntToStr(h), 2, '0') + GetFormatSettings.TimeSeparator + LeadToRight(IntToStr(m), 2, '0');
  if WithSeconds then
  begin
    if s = 0 then
      Result := Result + GetFormatSettings.TimeSeparator + '00'
    else
      Result := Result + GetFormatSettings.TimeSeparator + LeadToRight(IntToStr(s), 2, '0');
  end;
  if g then
    Result := '-' + Result;
end;

function TicksToString(vTicks: Int64): string;
var
  m, s, ms: Cardinal;
begin
  ms := vTicks;
  s := (ms div 1000);
  ms := (ms mod 1000);
  m := (s div 60);
  s := (s mod 60);
  Result := Format('%2d:%2d:%4d', [m, s, ms]);
  //Result := Format('%2d min  : %2d sec  : %2d msec', [m, s, ms]));
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

function SubStr(const AText: String; const AFromIndex, AToIndex: Integer): String;
begin
  Result := Copy(AText, AFromIndex, AToIndex - AFromIndex + 1);
end;

function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String;
var
  Index, B, E: Integer;
  C: Char;
begin
  Index := 0;
  B := 0;
  E := 1;
  for C in Str do
  begin
    if (B = 0) and (Index = vFromIndex) then
      B := E;

    if C = vSeperator then
      Inc(Index);

    if Index = vToIndex + 1 then
    begin
      E := E - 1;
      Break;
    end;
    Inc(E);
  end;

  if B <> 0 then
    Result := Copy(Str, B, E - B + 1)
  else
    Result := '';
end;

function SubStr(const Str: String; vSeperator: Char; vIndex: Integer): String;
begin
  Result := SubStr(Str, vSeperator, vIndex, vIndex);
end;

function FetchStr(var AInput: string; const ADelim: string; const ADelete: Boolean; const ACaseSensitive: Boolean): string;
var
  LPos: Integer;
begin
  if ADelim = #0 then begin
    // AnsiPos does not work with #0
    LPos := Pos(ADelim, AInput);
  end else begin
    LPos := Pos(ADelim, AInput);
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end
  else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then begin
      //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
  end;
end;

function StrInArray(const Str: String; const ArrayOfString: array of String; CaseInsensitive: Boolean): Boolean;
var
 itm : String;
begin
  for itm in ArrayOfString do
  begin
    if CaseInsensitive then
    begin
      if SameText(Str, itm) then
        exit(true);
    end
    else if Str = itm then
       exit(true);
  end;
  Result := false;
end;

function CharInArray(const C: string; const ArrayOfChar: array of Char; CaseInsensitive: Boolean): Boolean;
var
 itm : String;
begin
  for itm in ArrayOfChar do
  begin
    if CaseInsensitive then
    begin
      if UpperCase(C) = UpperCase(itm) then
        exit(true);
    end
    else if C = itm then
       exit(true);
  end;
  Result := false;
end;

function EscapeStringC(const S: string): string;
begin
  Result := EscapeString(s, '\', [#8, #9, #10, #13, '\', '"'], ['b', 't', 'n', 'r', '\', '"']);
end;

function DescapeStringC(const S: string): string;
begin
  Result := DescapeString(s, '\', [#8, #9, #10, #13, '\', '"'], ['b', 't', 'n', 'r', '\', '"']);
end;

function ToUnixPathDelimiter(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function IncludePathSeparator(const S: string): string;
begin
  if (s <> '') and (RightStr(S, 1) <> DirectorySeparator) then
    Result := s + PathDelim
  else
    Result := s;
end;

procedure CenterRect(var R1: TRect; R2: TRect);
begin
  OffsetRect(R1, ((R2.Right - R2.Left) div 2) - ((R1.Right - R1.Left) div 2) + (R2.Left - R1.Left), ((R2.Bottom - R2.Top) div 2) - ((R1.Bottom - R1.Top) div 2) + (R2.Top - R1.Top));
end;

function GetFormatSettings: TFormatSettings;
begin
  {$ifdef FPC}
  Result := DefaultFormatSettings;
  {$else}
    {$if CompilerVersion >= 22.0}
    Result := FormatSettings;
    {$else}
    Result := FFormatSettings;
    {$ifend}
  {$endif}
end;


procedure ISOStrToDate(ISODate: String; out Y, M, D, H, N, S: Word; vDateSeparator: Char; TimeDivider: Char; UseDefault: Boolean);
var
  Dt, Tm: String;
begin
  try
    if TimeDivider = #0 then
    begin
      if Pos('T', ISODate) > 0 then
        TimeDivider := 'T'
      else
        TimeDivider := ' ';
    end;

    if UseDefault then
      DecodeDate(Now, Y, M, D)
    else
      DecodeDate(0, Y, M, D);

    Dt := SubStr(ISODate, TimeDivider, 0);
    Tm := SubStr(ISODate, TimeDivider, 1);
    if Tm = '' then //one part
    begin
      if Pos(':', Dt) > 0 then //detect if it a time
      begin
        Tm := Dt;
        Dt := '';
      end
    end;

    if Dt <> '' then
    begin
      Y := StrToIntDef(SubStr(Dt, vDateSeparator, 0), Y);
      M := StrToIntDef(SubStr(Dt, vDateSeparator, 1), M);
      D := StrToIntDef(SubStr(Dt, vDateSeparator, 2), D);
    end;

    if Tm <> '' then
    begin
      Tm := SubStr(Tm, '+', 0);//skip time zone after plus
      H := StrToIntDef(SubStr(Tm, ':', 0), 0);
      N := StrToIntDef(SubStr(Tm, ':', 1), 0);
      //S := Round(StrToFloatDef(SubStr(Tm, ':', 2), 0)); //case 59.500s to round to 60 ->Error
      S := Trunc(StrToFloatDef(SubStr(Tm, ':', 2), 0));
    end
    else
    begin
      H := 0;
      N := 0;
      S := 0;
    end;
  except
    raise Exception.Create('Not valid DateTime');
  end;
end;

function ISOStrToDate(ISODate: String; vDateSeparator: Char; TimeDivider: Char; UseDefault: Boolean): TDateTime;
var
  Y, M, D, H, N, S: Word;
begin
  ISOStrToDate(ISODate, Y, M, D, H, N, S, vDateSeparator, TimeDivider, UseDefault);
  Result := EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0);
end;

function ISODateToStr(DateTime: TDateTime; vDateSeparator: Char; TimeDivider: Char; WithTime: Boolean): String;
var
  Y, M, D, H, N, S, O: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := AlignStr(IntToStr(Y), 4, [alsRight],'0') + vDateSeparator +  AlignStr(IntToStr(M), 2, [alsRight],'0') + vDateSeparator + AlignStr(IntToStr(D), 2, [alsRight], '0');
  if WithTime then
  begin
    DecodeTime(DateTime, H, N, S, O);
    Result := Result + TimeDivider + AlignStr(IntToStr(H), 2, [alsRight],'0') + ':' + AlignStr(IntToStr(N), 2, [alsRight],'0') + ':' + AlignStr(IntToStr(S), 2, [alsRight],'0');
  end;
end;

function AnsiToUnicode(S: rawbytestring; CodePage: Integer): string;
begin
  if CodePage = 0 then
    CodePage := SystemAnsiCodePage;
  SetCodePage(S, CodePage, False);
  Result := S;
end;

function StringAs(S: rawbytestring; CodePage: Integer = 0): utf8string;
begin
  if CodePage = 0 then
    CodePage := SystemAnsiCodePage;
  SetCodePage(S, CodePage, False);
  Result := S;
end;

function StringOf(const Value: array of Byte): string;
var
  Len:Integer;
begin
  {$ifdef FPC}
  Result := '';
  {$endif}
  Len:=Length(Value) div SizeOf(Char);
  SetLength(Result, Len);
  if Len > 0 then
    Move(Value[0], Result[1], Len * SizeOf(Char));
end;

function StringOf(const Value: TBytes): string;
var
  Len:Integer;
begin
  {$ifdef FPC}
  Result := '';
  {$endif}
  Len:=Length(Value) div SizeOf(Char);
  SetLength(Result, Len);
  if Len > 0 then
    Move(Value[0], Result[1], Len * SizeOf(Char));
end;

procedure EnumFiles(FileList: TStringList; Folder, Filter: string; FullPath: Boolean);
var
  R: integer;
  SearchRec: TSearchRec;
begin
  Folder := IncludeTrailingPathDelimiter(Folder);
  R := FindFirst(Folder + Filter, faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if FullPath then
        FileList.Add(Folder + SearchRec.Name)
      else
        FileList.Add(SearchRec.Name);
    end;
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

initialization
  {$ifdef windows}
  SystemAnsiCodePage := GetACP; //windows only
  {$else}
  SystemAnsiCodePage := 1252; //scpAnsi has no meaning in linux, you can change it in your application
  {$endif}
end.
