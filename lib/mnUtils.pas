unit mnUtils;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$modeswitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$RangeChecks+}
{$ENDIF}
{$M+}{$H+}

{$ifdef mswindows}
{$define windows}
{$endif}

interface

uses
  {$ifdef windows}Windows,{$endif}
  Classes, SysUtils, StrUtils, DateUtils, Types, Character,
  mnTypes;

procedure Nothing;

function QuoteStr(Str: string; const QuoteChar: string = '"'): string;

{**
  Resume: if false then stop, default is true
*}
type
  TStrToStringsCallbackProc = procedure(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
  TStrToStringsExCallbackProc = procedure(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);

  TArgumentsCallbackProc = procedure(Sender: Pointer; Index: Integer; Name, Value: string; IsSwitch:Boolean; var Resume: Boolean);

{**
  IgnoreInitialWhiteSpace: Ignore the first chars of this white space, not need it
*}

TStrToStringsOptions = set of (
  stsoKeepEmpty,
  stsoGroupSeparators //TODO, not tested yet, if one of separators come it will ignores chars in next separators like if separators = #13, #10, now #13#10 will considered as one line break
); //

//I want to StrToStrings to replace it with StrToStringsEx

function StrToStringsCallback(const Content: string; Sender: Pointer; const CallBackProc: TStrToStringsCallbackProc; Separators: TSysCharSet = [#0, #10, #13]; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']; vOptions: TStrToStringsOptions = []): Integer; overload;
function StrToStrings(const Content: string; Strings: TStrings; Separators: TSysCharSet = [#0, #10, #13]; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Integer; overload;

{
  Example
  StrToStringsExCallback(Memo1.Text, 1, self, @AddString, ['::', ';', #13#10, #13, #10, #0]);
  HINT: Put longest seperator first ::  befire ;
  Return the count of Parts

}

function StrToStringsExCallback(const Content: string; FromIndex: Integer; Sender: Pointer; const Separators: array of string; out MatchCount: Integer; const CallBackProc: TStrToStringsExCallbackProc; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']; vOptions: TStrToStringsOptions = []): Integer;
function StrToStringsEx(const Content: string; Strings: TStrings; Separators: Array of string; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Integer; overload;

//function StrToStringsEx(Content: string; Strings: TStrings; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Integer; overload;

procedure StrToStringsCallbackProc(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
procedure StrToStringsDequoteCallbackProc(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);

function StrScanTo(const Content: string; FromIndex: Integer; out S: string; out CharIndex, NextIndex, MatchCount: Integer; const Separators: array of string; const IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Boolean; overload;
//function StrScanTo(const Content: string; FromIndex: Integer; out S: string; NextIndex, const Separators: array of string; const IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Boolean; overload;

{
  examples:
  -t -s -v: value test

  -t -s --value: "value test"

}
type
  TParseArgumentsOptions = set of (
    pargKeepSwitch,
    pargDeqoute,
    pargTrim, //TODO
    pargValues //without name=value consider it as value
  );

//*  -t --test cmd1 cmd2 -t: value -t:value -t value
function ParseArgumentsCallback(const Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer; Switches: TArray<Char>{['-', '/']}; Options: TParseArgumentsOptions = [pargKeepSwitch, pargDeqoute]; Terminals: TSysCharSet = [' ', #9]; WhiteSpaces: TSysCharSet = [' ', #9]; Quotes: TSysCharSet = ['''', '"'];  ValueSeperators: TSysCharSet = [':', '=']): Integer; overload;

function ParseArguments(const Content: string; Strings: TStrings; Switches: TArray<Char>; Options: TParseArgumentsOptions = [pargKeepSwitch, pargDeqoute]; Terminals: TSysCharSet = [' ', #9]; WhiteSpaces: TSysCharSet = [' ', #9]; Quotes: TSysCharSet = ['''', '"'];  ValueSeperators: TSysCharSet = [':', '=']): Integer; overload;
function GetSubValue(const Content, Name: string; out Value: string; Terminals: TSysCharSet = [';']; WhiteSpaces: TSysCharSet = [' ',#9]; Quotes: TSysCharSet = ['"']; ValueSeperators: TSysCharSet = ['=']): Boolean; overload;
//*


//Parse ParamStr to Strings
//Always use KeyValues with one - not --
procedure ParseCommandArguments(CallBackProc: TArgumentsCallbackProc; Sender: Pointer; KeyValues: TArray<string> = []); overload;
procedure ParseCommandArguments(Arguments: TStrings; KeyValues: TArray<string> = []); overload;

{
  param1 param2 -s -w: value

  =param1
  =param2
  -s
  -w=value
}

//Command is not have value not a switch, it not started with - and not ended with : or =
function GetArgumentCommand(Strings: TStrings; out CommandName: string; out Index: Integer): Boolean; overload; // not deprecated 'use GetArgumentSwitch with no Value';
//SwitchName: Use switch char too, like `-demon` `-service`
//Switch started with - or -- notice -- with considered as - too
function GetArgumentSwitch(Strings: TStrings; SwitchName: string; AltSwitchName: string = ''; NoValue: Boolean = False): Boolean; overload;
//Value from name or switch both acceptable, --name:value or name:value
//Return True if name exists even if no value provided
function GetArgumentValue(Strings: TStrings; out Value: String; SwitchName: string; AltSwitchName: string = ''): Boolean; overload;

//Get Value from any thing have value by index `name1=value1 --name2=value2`
function GetArgument(Strings: TStrings; out Value: String; Index: Integer): Boolean; overload;
//Get all values
function GetArgument(Strings: TStrings; OutStrings: TStrings; AltSwitch: string = ''): Boolean; overload;
function GetArgument(Strings: TStrings; out OutStrings: TArray<String>): Boolean; overload;

function StringsToString(Strings: TStrings; LineBreak: string = sLineBreak): string;

function CompareLeftStr(const Str: string; const WithStr: string; Start: Integer = 1): Boolean;
function ContainsText(const SubStr, InStr: string): Boolean; deprecated 'Use StrUtils.ContainsText and swap params';

//Same as Copy/MidStr but From To index
//function CopyStr(const AText: String; const AFromIndex, AToIndex: Integer): String; overload; deprecated;
function SubStr(const AText: String; AFromIndex, AToIndex: Integer): String; overload;
{
  Index started from 0
}
function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String; overload;
function SubStr(const Str: String; vSeperator: Char; vIndex: Integer = 0): String; overload;

{
  StrHave: test the string if it have Separators
}
function HaveChar(S: string; Separators: TSysCharSet): Boolean;
function IndexOfChar(S: string; Separators: TSysCharSet): Integer;

//if S is same Name variabled passed, it empty it both, so i will use `var` not `out`
procedure SpliteStr(S, Separator: string; var Name:string; var Value: string); inline;

function FetchStr(var AInput: string; const ADelim: string = '.'; const ADelete: Boolean = True; const ACaseSensitive: Boolean = True): string; deprecated;

function StrInArray(const Str: String; const InArray : Array of String; CaseInsensitive: Boolean = False) : Integer; overload;
function IsStrInArray(const Str: String; const InArray : Array of String; CaseInsensitive: Boolean = False) : Boolean; overload;
function StrInArray(const Str: string; const StartIndex: Integer; const InArray: array of string; out SepLength: Integer; CaseInsensitive: Boolean = False): Integer; overload;
function IsStrInArray(const Str: string; const StartIndex: Integer; const InArray: array of string; out SepLength: Integer; CaseInsensitive: Boolean = False): Boolean; overload;
function CharInArray(const C: Char; const ArrayOfChar : array of Char; CaseInsensitive: Boolean = False) : Boolean;
function CharArrayToSet(const ArrayOfChar : TArray<Char>) : TSysCharSet;

//vPeriod is a datetime not tickcount
function PeriodToString(vPeriod: Double; WithSeconds: Boolean): string;
//Used by GetTickCount, return minuts,secons,miliseconds
function TicksToString(vTicks: Int64): string;
function DequoteStr(const Str: string; const QuoteChar: string = #0): string;

function RepeatString(const Str: string; Count: Integer): string;

function ConcatString(const S1, Delimiter: string; const S2: string = ''): string; overload;

//TODO options, to include null

type
  TCollectStringsOptions = set of (collectNulls, collectTrim);

function CollectStrings(const Strings: array of string; Delimiter: string = ','; Options: TCollectStringsOptions = [collectTrim]): string; overload;
function CollectStrings(Strings: TStrings; Delimiter: string = ','; Options: TCollectStringsOptions = [collectTrim]): string; overload;

function ReversePos(const SubStr, S : String): Integer; overload;
function ReversePos(const SubStr, S: String; const Start: Integer): Integer; overload;

{* VarReplace
  VarInit = '$'
  Example: VarReplace('c:\$project\$[name]';
}
type
  TVarOptions = set of (
    vrSmartLowerCase,
    vrAllowBrackets //TODO
  );
  TVarReplacesCallbackProc = procedure(Sender: Pointer; Name: string; var Value: string);

function VarReplace(S: string; Values: TStrings; Prefix: string; Suffix: String = ''; ExtraChar: TSysCharSet = []; VarOptions: TVarOptions = []; Sender: Pointer = nil; ReplacesCallbackProc: TVarReplacesCallbackProc = nil): string; overload;

type
  //alsCut = if the string > count we cut it as count or keep the string
  TAlignStrOptions = set of (alsLeft, alsRight, alsCut); {TODO left+right=center TODO use righttoleft}

function AlignStr(const S: string; Count: Integer; Options: TAlignStrOptions = [alsLeft]; vChar: Char = ' '): string; overload;

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

function EscapeString(const S: string; const Esc: string; Chars: array of Char; const Escapes: array of string): string;
function DescapeString(const S: string; const Esc: string; Chars: array of Char; const Escapes: array of string): string;

function EscapeStringC(const S: string; QuoteChar: UTF8Char = '"'): string;
function DescapeStringC(const S: string): string;
function ToUnixPathDelimiter(const S: string): string;

function ExpandFile(const Name: string): string;

{
  Useful to make your project path related (Portable)
  FileName:
          myfile
          mydir/myfile
          ./myfile
          ../myfile

          /myfile
          \myfile
          \\...\myfile

          c:\temp\myfile
          /mydir/myfile
  Path:
  Root: is optional, added before Path
}
function ExpandToPath(FileName: string; Path: string; Root: string = ''): string;

function CorrectPath(const Path: string): string;

//* Split at level depth of folders/directory, ignoring first \ or last one
function SplitPath(Path: string; out Right: string; Index: Integer): string; overload;
function SplitPath(Path: string; Index: Integer): string; overload;

//Remove last subdirectory
//if lasted by path delimiator remove it
function TruncPath(const Path: string; Index: Integer): string; overload;

function ExcludeTrailing(const Str: string; const TrailingChar: string = #0): string;

//IncludePathDelimiter add the Delimiter when S not = ''
function IncludePathDelimiter(const S: string; Force: Boolean = False): string;
function ExcludePathDelimiter(Path: string): string;

//This not check if S = ''
function IncludeURLDelimiter(const S: string): string; //deprecated 'AddEndURLDelimiter';
function IsURLDelimiter(const S: string): Boolean;

//If empty do not add
function AddStartURLDelimiter(const Path: string; Force: Boolean = False): string; {$ifdef D-}inline;{$endif}
function AddEndURLDelimiter(const Path: string; Force: Boolean = False): string; {$ifdef D-}inline;{$endif}

function StartsDelimiter(const vFileName: string): Boolean;
function EndsDelimiter(const vFileName: string): Boolean;

function EncloseStr(const S, Left, Right: string): string;
function UncloseStr(const S, Left, Right: string): string;

//Similer to ZeroMemory
procedure InitMemory(out V; Count: {$ifdef FPC}SizeInt{$else}Longint{$endif});

function GetFormatSettings: TFormatSettings;

//Ported from UniDates

procedure ISOStrToDate(ISODate: String; out Y, M, D, H, N, S: Word; vDateSeparator: Char = '-'; TimeDivider: Char = #0; UseDefault: Boolean = False); overload;
function ISOStrToDate(ISODate: String; vDateSeparator: Char = '-'; TimeDivider: Char = #0; UseDefault: Boolean = False): TDateTime; overload;

function ISODateToStr(DateTime: TDateTime; vDateSeparator: Char = '-'; TimeDivider: Char = ' '; TimeSeparator: Char = ':'; WithTime: Boolean = False): String; overload;
function ISODateTimeToStr(DateTime: TDateTime; vDateSeparator: Char = '-'; TimeDivider: Char = ' '): String; overload;

//  DateToStr(RFC2822ToDateTime('Thu, 26 Jun 2025 11:08:12 GMT'))
function RFC2822ToDateTime(data: string): TDateTime;
function DateTimeToRFC2822(vDate: TDateTime): string;

function DateTimeToRFC822(vDateTime: TDateTime): string;

function IsAllLowerCase(S: string): Boolean;

//Zero Based
type
  TEncodingHelper = class helper for TEncoding
  public
    function GetString(Bytes: PByte; ByteCount: Integer): String; overload; inline;
    function GetString(Bytes: PByte; Start, ByteCount: Integer): String; overload; inline;
    class function CodePageEncoding(CodePage: Word): TEncoding;
    {$ifdef FPC}
    function GetString(Bytes: array of Byte): String; overload;
    {$endif}
  end;

function StringOf(const Value: Array of Byte; CodePage: Word = CP_UTF8): string; overload; deprecated;
function StringOf(const Value: TBytes; CodePage: Word = CP_UTF8): string; overload; deprecated;
function StringOf(const Value: PByte; Size: Integer; CodePage: Word = CP_UTF8): string; overload; deprecated;
function StringOf(const Value: PByte; Start, Size: Integer; CodePage: Word = CP_UTF8): string; overload; deprecated;

function StringOfUTF8(const Value: PByte; Size: Integer): string;

//TODO fix ansi to widestring
function HexToBin(Text : PByte; Buffer: PByte; BufSize: longint): Integer; overload;
procedure BinToHex(Buffer: PByte; Text: PByte; BufSize: longint); overload;
function StringToHex(const vData: string): string; overload;
function StringToHex(const vData: PByte; vCount: Integer): string; overload;
function HexToString(const vData: string): string; overload;
function UUIDToString(Guid: TGuid; Hyphen: string = '-'): string;

function ByteToBinStr(Value: Byte): string;
function DataToBinStr(var Data; Size: Integer; Separator: string = ''): string;

//Files Utils

type
  TEnumFilesOptions = set of (efFile, efDirectory, efFullPath);
  //If set Resume to false it will stop loop
  TEnumFilesCallback = procedure(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);

procedure EnumFiles(FileList: TStrings; const Folder, Filter: string; Options: TEnumFilesOptions = [efFile]); overload;
function FirstFile(const Path, Files: string): string;
function DeleteFiles(const Path, Files: string): Integer;
function GetSizeOfFile(const vFile: string): Int64; //GetFileSize
function LoadFileString(FileName: string): string;

//mnMulDiv not using windows unit
function mnMulDiv(nNumber, nNumerator, nDenominator: Integer): Integer; overload;
function mnMulDiv(nNumber, nNumerator, nDenominator: Int64): Int64; overload;
//propblem round(10.5) -> 10
function mnRound(nNumber: Double): Int64; overload;

procedure SwapBytes(const Source; out Dest; Size: Integer); overload;
function SwapBytes(const Source: Word): Word; overload;
function SwapBytes(const Source: SmallInt): SmallInt; overload;
function SwapBytes(const Source: Cardinal): Cardinal; overload;
function SwapBytes(const Source: Int64): Int64; overload;

//Rect functions
procedure CenterRect(var R1: TRect; R2: TRect);

var
  SystemAnsiCodePage: Cardinal; //used to convert from Ansi string, it is the default
  DefFormatSettings : TFormatSettings;

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

function HaveChar(S: string; Separators: TSysCharSet): Boolean;
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

function IndexOfChar(S: string; Separators: TSysCharSet): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
  begin
     if CharInSet(S[i], Separators) then
     begin
       Result := i;
       Break;
     end;
  end;
end;

function QuoteStr(Str: string; const QuoteChar: string): string;
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

function DequoteStr(const Str: string; const QuoteChar: string = #0): string;
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

function TruncPath(const Path: string; Index: Integer): string;
var
  l, i, e: Integer;
  c: Char;
begin
  if Path = '' then
    Result := ''
  else if Index = 0 then
    Result := Path
  else
  begin
    i := 0;
    l := Length(Path);
    if Index > 0 then
    begin
      e := 1;
      if Path[1] in ['\', '/'] then
      begin
        dec(l);
        inc(e);
      end;
    end
    else
    begin
      e := l;
      if Path[l] in ['\', '/'] then
      begin
        dec(l);
        dec(e);
      end;
    end;

    while l > 0 do
    begin
      C := Path[e];
      if C in ['\', '/'] then
        Inc(i);
      if (i = Abs(Index)) then
        Break;

      if Index > 0 then
        Inc(e)
      else
        Dec(e);
      Dec(l);
    end;
    Result := Copy(Path, 1, e);
  end;
end;

function ExcludeTrailing(const Str: string; const TrailingChar: string = #0): string;
begin
  if (TrailingChar > #0) and (RightStr(Str, 1) = TrailingChar) then
    Result := MidStr(Str, 1, Length(Str) - 1)
  else
    Result := Str;
end;

function UncloseStr(const S, Left, Right: string): string;
var
  start, count: Integer;
begin
  if UpperCase(LeftStr(s, 1)) = UpperCase(Left) then
    start := Length(Left) + 1
  else
    start := 1;
  if UpperCase(RightStr(s, 1)) = UpperCase(Right) then
    count := Length(Right)
  else
    count := 0;
  count := Length(s) - start - count + 1;
  Result := MidStr(S, start, count);
end;

function EncloseStr(const S, Left, Right: string): string;
begin
  if S <> '' then
  begin
    if LeftStr(S, 1) = Left then
      Result := S
    else
      Result := Left + S;

    if RightStr(Result, 1) <> Right then
      Result := Result + Right;
  end
  else
    Result := '';
end;

function UUIDToString(Guid: TGuid; Hyphen: string): string;
begin
  Result := LowerCase(
            IntToHex(Longint(GUID.D1), 8) + Hyphen + IntToHex(GUID.D2, 4) + Hyphen + IntToHex(GUID.D3, 4)
            + Hyphen + IntToHex(GUID.D4[0], 2) + Hyphen + IntToHex(GUID.D4[1], 2) + Hyphen + IntToHex(GUID.D4[2], 2) + Hyphen + IntToHex(GUID.D4[3], 2)
            + Hyphen + IntToHex(GUID.D4[4], 2) + Hyphen + IntToHex(GUID.D4[5], 2) + Hyphen + IntToHex(GUID.D4[6], 2) + Hyphen + IntToHex(GUID.D4[7], 2)
            )
//  Result := LowerCase(RemoveEncloseStr(GUIDToString(Guid), '{', '}'));
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

function ConcatString(const S1, Delimiter: string; const S2: string): string;
begin
  Result := S1;
  if (Result <> '') and (S2 <> '') then
    Result := Result + Delimiter;
  Result := Result + S2;
end;

function CollectStrings(const Strings: array of string; Delimiter: string; Options: TCollectStringsOptions): string;
var
  s: string;
  i: Integer;
begin
  Result := '';
  for i :=0 to Length(Strings) -1 do
  begin
    if collectTrim in Options then
      s := Trim(Strings[i])
    else
      s := Strings[i];
    if (s <> '') or (collectNulls in Options) then
      Result := ConcatString(Result, Delimiter, s);
  end;
end;

function CollectStrings(Strings: TStrings; Delimiter: string; Options: TCollectStringsOptions): string;
var
  s: string;
  i: Integer;
begin
  Result := '';
  for i :=0 to Strings.Count -1 do
  begin
    if collectTrim in Options then
      s := Trim(Strings[i])
    else
      s := Strings[i];
    if (s <> '') or (collectNulls in Options) then
      Result := ConcatString(Result, Delimiter, s);
  end;
end;

function ReversePos(const SubStr, S: String; const Start: Integer): Integer;
var
  i: Integer;
  pStr: PChar;
  pSub: PChar;
begin
  pSub := Pointer(SubStr);

  for i := Start downto 1 do
  begin
    pStr := @(S[i]);
    if (pStr^ = pSub^) then
    begin
      if CompareMem(pSub, pStr, Length(SubStr)) then
      begin
        Result := i;
        exit;
      end;
    end;
  end;
  Result := 0;
end;


function ReversePos(const SubStr, S: String): Integer;
begin
  Result := ReversePos(SubStr, S, Length(S) - Length(SubStr) + 1);
end;

{**
*  Replace multipe variables $var for example with value in Values
*  Use name values in strings
*}

function VarReplace(S: string; Values: TStrings; Prefix: string; Suffix: String; ExtraChar: TSysCharSet; VarOptions: TVarOptions; Sender: Pointer; ReplacesCallbackProc: TVarReplacesCallbackProc): string;
var
  Start: Integer;
  OpenStart: Integer;
  InsideBlock: Boolean;
  InitIndex: Integer;
  procedure check(Index: Integer);
  var
    Name, Value: string;
  begin
    //* string before variable
    Result := Result + MidStr(S, Start, OpenStart - Start);
    Name := MidStr(S, OpenStart + Length(Prefix), Index - OpenStart + 1 - Length(Prefix) - Length(Suffix));
    if (LeftStr(Name, 1) = '[') and (RightStr(Name, 1) = ']') then
      Name := MidStr(Name, 2, Length(Name) - 2);
    Value := MidStr(S, OpenStart, Index - OpenStart + 1);
    if Values.IndexOfName(Name) >= 0 then
    begin
      if (vrSmartLowerCase in VarOptions) and IsAllLowerCase(Name) then //Smart idea, right ^.^
        Value := LowerCase(Values.Values[Name])
      else
        Value := Values.Values[Name];
    end
    else if Assigned(ReplacesCallbackProc) then
      ReplacesCallbackProc(Sender, Name, Value);
    Result := Result + Value;
    Start := Index + 1;
    OpenStart := 0;
  end;
var
  Current: Char;
  Len: Integer;
  Index: Integer;
begin
  if Length(Suffix) > 1 then
    raise Exception.Create('');
  InsideBlock := False;
  OpenStart := 0;
  Result := '';
  Len := Length(S);
  InitIndex := 1;
  Index := 1;
  Start := Index;
  while Index <= Len do
  begin
    Current := S[Index];
    if (OpenStart > 0) then
    begin
      if not InsideBlock and CharInSet(Current, ['[']) then
        InsideBlock := True
      else if InsideBlock and CharInSet(Current, [']']) then
        InsideBlock := False;

      if not InsideBlock then
      begin
        if ((Suffix <> '') and (Current = Suffix[1])) then
          Check(Index)
        else if ((Suffix = '') and not CharInSet(Current, ['0'..'9', 'a'..'z', 'A'..'Z', '_'] + ExtraChar)) then
        begin
          Dec(Index);
          Check(Index);
        end;
      end;
    end
    else if (Current = Prefix[InitIndex]) then
    begin
      if InitIndex = Length(Prefix) then
      begin
        OpenStart := Index - Length(Prefix) + 1;
        InitIndex := 1;
      end
      else
        Inc(InitIndex);
    end
    else
      InitIndex := 1;
    Inc(Index);
  end;
  if (OpenStart > 0) then
    Check(Index);
  Result := Result + MidStr(S, Start, MaxInt);
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

procedure StrToStringsDequoteCallbackProc(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
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

function StrToStringsCallback(const Content: string; Sender: Pointer; const CallBackProc: TStrToStringsCallbackProc; Separators: TSysCharSet; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet; vOptions: TStrToStringsOptions): Integer;
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
        while (Cur <= Length(Content)) and not (CharInSet(Content[Cur], Quotes) or (not InQuote and (CharInSet(Content[Cur], Separators)))) do
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
        if (stsoKeepEmpty in vOptions) or (s<>'') then
        begin
          Resume := True;
          CallBackProc(Sender, Index, S, Resume);
          Index := Index + 1;
          Inc(Result);
          if not Resume then
            break;
        end;
      end;
      Cur := Cur + 1;
    until Cur > Length(Content) + 1;
  end;
end;

function StrToStrings(const Content: string; Strings: TStrings; Separators: TSysCharSet; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet): Integer;
begin
  if (Strings = nil) then
    raise Exception.Create('StrToStrings: Strings is nil');
  Strings.BeginUpdate;
  try
    Result := StrToStringsCallback(Content, Strings, StrToStringsCallbackProc, Separators, IgnoreInitialWhiteSpace, Quotes, [stsoKeepEmpty]);
  finally
    Strings.EndUpdate;
  end;
end;

//Ex

function StrToStringsExCallback(const Content: string; FromIndex: Integer; Sender: Pointer; const Separators: array of string; out MatchCount: Integer; const CallBackProc: TStrToStringsExCallbackProc; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet; vOptions: TStrToStringsOptions): Integer;
var
  Start, Cur, SepLength: Integer;
  Resume: Boolean;
  InQuote: Boolean;
  QuoteChar: Char;
  S: string;
  Index: Integer;
begin
  Result := 0;
  Index := 0;
  MatchCount := 0;
  if (@CallBackProc = nil) then
    raise Exception.Create('StrToStrings: CallBackProc is nil');
  Cur := FromIndex;
  if Cur = 0 then
    Cur := 1;
  if (Content <> '') and (Cur <= Content.Length) then
  begin
    InQuote := False;
    QuoteChar := #0;
    repeat
      //bypass white spaces
      if IgnoreInitialWhiteSpace <> [] then
        while (Cur <= Length(Content)) and CharInSet(Content[Cur], IgnoreInitialWhiteSpace) do
          Cur := Cur + 1;

      //start from the first char
      Start := Cur;
      while True do
      begin
        SepLength := 1;
        //seek until the separator and skip the separator if inside quoting
        while (Cur <= Length(Content)) do
        begin
          SepLength := 1;
          if (InQuote) then
          begin
            if (Content[Cur] = QuoteChar) then
              break;
            Cur := Cur + 1;
          end
          else
          begin
            if CharInSet(Content[Cur], Quotes) then
              break
            else if (IsStrInArray(Content, Cur, Separators, SepLength)) then
            begin
              Cur := Cur + SepLength - 1;
              Inc(MatchCount);
              break;
            end
            else
            begin
              Cur := Cur + 1
            end;
          end;
        end;

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
        S := Copy(Content, Start, Cur - Start - SepLength + 1); //TODO compare it with other function
        if (stsoKeepEmpty in vOptions) or (s<>'') then
        begin
          Resume := True;
          CallBackProc(Sender, Index, Start, Cur + 1, S, Resume);
          Index := Index + 1;
          Inc(Result);
          if not Resume then
            break;
        end;
      end;
      Cur := Cur + 1;
    until Cur > Length(Content) + 1;
  end;
end;

procedure StrToStringsExCallbackProc(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
begin
  TStrings(Sender).Add(S); //Be sure sender is TStrings
end;

function StrToStringsEx(const Content: string; Strings: TStrings; Separators: array of string; IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet): Integer;
var
  MatchCount: Integer;
begin
  if (Strings = nil) then
    raise Exception.Create('StrToStrings: Strings is nil');
  Strings.BeginUpdate;
  try
    Result := StrToStringsExCallback(Content, 0, Strings, Separators, MatchCount, StrToStringsExCallbackProc, IgnoreInitialWhiteSpace, Quotes, [stsoKeepEmpty]);
  finally
    Strings.EndUpdate;
  end;
end;

type
  TResultString = record
    Found: string;
    CharIndex: Integer;
    NextIndex: Integer;
  end;

procedure StrScanToCallbackProc(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
begin
  Resume := False;
  TResultString(Sender^).Found := S;
  TResultString(Sender^).CharIndex := CharIndex;
  TResultString(Sender^).NextIndex := NextIndex;
end;

function StrScanTo(const Content: string; FromIndex: Integer; out S: string; out CharIndex, NextIndex, MatchCount: Integer; const Separators: array of string; const IgnoreInitialWhiteSpace: TSysCharSet; Quotes: TSysCharSet): Boolean;
var
  r: TResultString;
begin
  r.Found := '';
  r.CharIndex := 0;
  r.NextIndex := 0;
  Result := StrToStringsExCallback(Content, FromIndex, @r, Separators, MatchCount, StrScanToCallbackProc, IgnoreInitialWhiteSpace, Quotes) > 0;
  S := r.Found;
  CharIndex := r.CharIndex;
  NextIndex := r.NextIndex;
end;

type
  TSubStrResult = record
    Name: string;
    Value: string;
    Found: Boolean;
  end;

procedure GetSubValueCallbackProc(Sender: Pointer; Index: Integer; AName, AValue: string; IsSwitch: Boolean; var Resume: Boolean);
var
  p: ^TSubStrResult;
begin
  p := Sender;

  if (AName = p.Name) then
  begin
    p.Value := AValue;
    p.Found := True;
    Resume := False;
  end
end;

function GetSubValue(const Content, Name: string; out Value: string; Terminals: TSysCharSet; WhiteSpaces: TSysCharSet; Quotes: TSysCharSet; ValueSeperators: TSysCharSet): Boolean;
var
  r: TSubStrResult;
begin
  r.Found := False;
  r.Name := Name;
  r.Value := Name;

  ParseArgumentsCallback(Content, @GetSubValueCallbackProc, @r, [], [pargDeqoute], Terminals, WhiteSpaces, Quotes, ValueSeperators);

  Value := r.Value;
  Result := r.Found;
end;


{function StrToStringsEx(Content: string; Strings: TStrings; IgnoreInitialWhiteSpace: TSysCharSet = [' ']; Quotes: TSysCharSet = ['''', '"']): Integer; overload;
begin
  Result := StrToStringsEx(Content, Strings, [#13, #10, #0], IgnoreInitialWhiteSpace, Quotes);
end;}

function ParseArgumentsCallback(const Content: string; const CallBackProc: TArgumentsCallbackProc; Sender: Pointer; Switches: TArray<Char>; Options: TParseArgumentsOptions; Terminals: TSysCharSet; WhiteSpaces: TSysCharSet; Quotes: TSysCharSet; ValueSeperators: TSysCharSet): Integer;
type
  TParseState = (stName, stAssign, stValue);
var
  Start, Cur: Integer;
  Resume: Boolean;
  Index: Integer;
  Name: string;
  Value: string;
  QuoteChar: Char;
  S: string;
  State: TParseState;
  IsSwitch: Boolean;
  l: Integer;
begin
  if (@CallBackProc = nil) then
    raise Exception.Create('ParseArguments: CallBackProc is nil');

  IsSwitch := False;
  Result := 0;
  Index := 0;
  Name := '';
  Value := '';
  State := stName;

  if (Content = '') then
    Exit(0);

  Cur := 0;
  repeat
    Cur := Cur + 1;
    //bypass white spaces
    while (Cur <= Length(Content)) and CharInSet(Content[Cur], WhiteSpaces) do
      Cur := Cur + 1;

    //start from the first char
    Start := Cur;
    QuoteChar := #0;

    while True do
    begin
      if (Cur > Length(Content)) then
        break
      else if (QuoteChar = #0) and CharInSet(Content[Cur], Quotes) then
        QuoteChar := Content[Cur]
      else if (QuoteChar <> #0) then
      begin
        if (Content[Cur] = QuoteChar) then
          QuoteChar := #0;
      end
      else if CharInSet(Content[Cur], Terminals) then
        break
      //* if you removed -t:value will break
      else if (State = stName) and CharInSet(Content[Cur], ValueSeperators) then
      begin
        State := stAssign;
        break;
      end;
      Cur := Cur + 1;
    end;

    if (Cur >= Start) then
    begin
      l := Cur - Start;

      if l = 0 then
        S := ''
      else
      begin
        if (pargDeqoute in Options) and CharInSet(Content[Start], Quotes) and (Content[Start] = Content[Start + l - 1]) then
          S := Copy(Content, Start + 1, l - 2)
        else
          S := Copy(Content, Start, l);
      end;

      if State = stValue then
      begin
        Value := S;
        State := stName;
      end
      else
      begin
        Name := S;
        Value := '';
        if State = stAssign then
          State := stValue
        else
          State := stName;
      end;

      if State = stName then
      begin
        Resume := True;
        if (Name<>'') and CharInArray(Name[1], Switches) then
        begin
          IsSwitch := True;

          if pargKeepSwitch in Options then
          begin
            if (Length(Name)>1) and (Name[1] = Name[2]) then
              Name := Copy(Name, 2, Length(Name)); //change double switch to one switch

            if Name[1] <> Switches[0] then //should be first element in Switches, convert / to -
              Name[1] := Switches[0];
          end
          else
          begin
            if (Length(Name)>1) and (Name[1] = Name[2]) then
              Name := Copy(Name, 3, Length(Name)) //change double switch to one switch
            else
              Name := Copy(Name, 2, Length(Name));
          end;
        end;
        //run2.exe  name=value "name"=value name="value" "name=value"
        if (Value='') and not IsSwitch and (pargValues in Options) then
          CallBackProc(Sender, Index, '', Name, IsSwitch, Resume)
        else
          CallBackProc(Sender, Index, Name, Value, IsSwitch, Resume);

        IsSwitch := False;
        Index := Index + 1;
        Inc(Result);

        if not Resume then
          break;
      end;

    end
  until Cur > Length(Content);
end;

procedure ArgumentsCallbackProc(Sender: Pointer; Index: Integer; AName, AValue: string; IsSwitch: Boolean; var Resume: Boolean);
begin
  with TObject(Sender) as TStrings do
  begin
    if AValue <> '' then
      Add(AName + NameValueSeparator + AValue)
    else
      Add(AName);
  end;
end;

function ParseArguments(const Content: string; Strings: TStrings; Switches: TArray<Char>; Options: TParseArgumentsOptions; Terminals: TSysCharSet; WhiteSpaces: TSysCharSet; Quotes: TSysCharSet; ValueSeperators: TSysCharSet): Integer;
begin
  Result := ParseArgumentsCallback(Content, @ArgumentsCallbackProc, Strings, Switches, Options, Terminals, WhiteSpaces, Quotes, ValueSeperators);
end;

procedure ParseCommandArguments(CallBackProc: TArgumentsCallbackProc; Sender: Pointer; KeyValues: TArray<string>);
var
  Resume: Boolean;
  NextIsValue: Boolean;
  Name, Value: string;
  i, c, idx: Integer;
begin
  NextIsValue := False;
  c := 0;
  for i := 1 to ParamCount do
  begin
    //-----------
    if NextIsValue then
    begin
      Value := ParamStr(i);
      NextIsValue := False;
    end
    else
    begin
      Name := ParamStr(i);
      Value := '';

      if StartsText('/', Name) then
        Name := '-' + Copy(Name, 2, MaxInt)
      else if StartsText('--', Name) then
        Name := Copy(Name, 2, MaxInt);

      if EndsText('=', Name) or EndsText(':', Name) then
      begin
        Name := Copy(Name, 1, Length(Name)-1);
        NextIsValue := True;
      end
      else if IsStrInArray(Name, KeyValues) then
        NextIsValue := True
      else
      begin
        idx := IndexOfChar(Name, ['=', ':']);
        if idx > 0 then
        begin
          Value := Copy(Name, idx+1, MaxInt);
          Name := Copy(Name, 1, idx-1);
        end;
      end;
    end;

    if not NextIsValue then
    begin
      //Arguments.Add(arg);

      Resume := True;
      CallBackProc(Sender, c, Name, Value, StartsText('-', Name), Resume);
      c := c + 1;
      if not Resume then
        break;
      Name := '';
    end;
  end;

  if Name <> '' then
    CallBackProc(Sender, c, Name, Value, StartsText('-', Name), Resume)
end;

function GetArgumentCommand(Strings: TStrings; out CommandName: string; out Index: Integer): Boolean; overload;
var
  I: Integer;
  S: string;
begin
  CommandName := '';
  Index := -1;
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    if not StartsText('-', S) and not HaveChar(S, ['=', ':']) then
    begin
      CommandName := S;
      Index := I;
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure ParseCommandArguments(Arguments: TStrings; KeyValues: TArray<string>);
begin
  ParseCommandArguments(ArgumentsCallbackProc, Arguments, KeyValues);
end;

function GetArgumentValue(Strings: TStrings; out Value: String; SwitchName: string; AltSwitchName: string = ''): Boolean;
var
  I, P: Integer;
  S, N, V: string;
begin
  if StartsText('--', SwitchName) then
    SwitchName := Copy(SwitchName, 2, MaxInt);
  if StartsText('--', AltSwitchName) then
    AltSwitchName := Copy(AltSwitchName, 2, MaxInt);
  Result := False;
  Value := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    P := Pos(Strings.NameValueSeparator, S);
    if (P > 0) then
    begin
      N := Copy(S, 1, P - 1);
      V := Copy(S, p + 1, MaxInt);
    end
    else
    begin
      N := S;
      V := '';
    end;

    if (SameText(N, SwitchName)) or ((AltSwitchName <> '') and (SameText(N, AltSwitchName))) then
    begin
      Value := V;
      Exit(True);
    end;
  end;
end;

function GetArgumentSwitch(Strings: TStrings; SwitchName: string; AltSwitchName: string = ''; NoValue: Boolean = False): Boolean; overload;
var
  I, P: Integer;
  S, V: string;
begin
  Result := False;
  if StartsText('--', SwitchName) then
    SwitchName := Copy(SwitchName, 2, MaxInt);
  if StartsText('--', AltSwitchName) then
    AltSwitchName := Copy(AltSwitchName, 2, MaxInt);
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    P := Pos(Strings.NameValueSeparator, S);
    if (P > 0) then
    begin
      V := Copy(S, P + 1, MaxInt);
      S := Copy(S, 1, P - 1)
    end
    else
    begin
      V := '';
      S := Copy(S, 1, MaxInt);
    end;

    if NoValue and (V <> '') then
      Continue
    else if SameText(S, SwitchName) or ((AltSwitchName <> '') and (SameText(S, AltSwitchName))) then
      Exit(True);
  end;
end;

function GetArgument(Strings: TStrings; out Value: String; Index: Integer): Boolean;
var
  I, P: Integer;
  S, Name: string;
begin
  Result := False;
  Value := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    P := Pos(Strings.NameValueSeparator, S);
    if (P > 0) then
    begin
      Name := Copy(S, 1, P - 1);
      if (Name = '') then
      begin
        if Index = 0 then
        begin
          Value := Copy(S, P + 1, MaxInt);
          Exit(True);
        end;
        Dec(Index);
      end;
    end
    else if (P = 0) then
    begin
      Exit(True);
      Dec(Index);
    end
  end;
end;

function GetArgument(Strings: TStrings; OutStrings: TStrings; AltSwitch: string): Boolean;
  procedure AddNow(S: string);
  begin
    OutStrings.Add(S);
  end;
var
  I, P: Integer;
  S, Value: string;
begin
  Result := False;
  Value := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    P := Pos(Strings.NameValueSeparator, S);
    if (P > 0) then
    begin
      if (Copy(S, 1, P - 1) = '') then
      begin
        Value := Copy(S, P + 1, MaxInt);
        AddNow(Value);
        Result := True;
      end;
    end
    else if (P = 0) then
      AddNow(S);
  end;
end;

function GetArgument(Strings: TStrings; out OutStrings: TArray<String>): Boolean;
  procedure AddNow(S: string);
  begin
    SetLength(OutStrings, Length(OutStrings) + 1);
    OutStrings[Length(OutStrings) -1] := S;
  end;
var
  I, P: Integer;
  S, Value: string;
begin
  OutStrings := [];
  Result := False;
  Value := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    P := Pos(Strings.NameValueSeparator, S);
    if (P > 0) then
    begin
      if (Copy(S, 1, P - 1) = '') then
      begin
        Value := Copy(S, P + 1, MaxInt);
        AddNow(Value);
        Result := True;
      end;
    end
    else if (P = 0) then
      AddNow(S);
  end;
end;

function StartsDelimiter(const vFileName: string): Boolean;
begin
  Result := StartsStr('/', vFileName) or StartsStr('\', vFileName);
end;

function EndsDelimiter(const vFileName: string): Boolean;
begin
  Result := EndsStr('/', vFileName) or EndsStr('\', vFileName);
end;

function ExpandToPath(FileName: string; Path: string; Root: string): string;
begin
  if (FileName = '') then
    Exit('');

  if StartsStr('../', FileName) or StartsStr('..\', FileName) then
    Result := ExpandFileName(IncludePathDelimiter(Root) + IncludePathDelimiter(Path) + FileName)
  else if StartsStr('./', FileName) or StartsStr('.\', FileName) then
    Result := IncludePathDelimiter(Root) + IncludePathDelimiter(Path) + RightStr(FileName, Length(FileName) - 2)
  else if StartsStr('\\', FileName) then //windows network
    Result := FileName
  else if StartsDelimiter(FileName) then
    {$ifdef MSWINDOWS}
    Result := ExtractFileDrive(Path) + FileName
    {$else}
    Result := FileName
    {$endif}
  {$ifdef MSWINDOWS}
  else if (Length(FileName)>1) and (FileName[2] = ':') then
    Result := FileName
  {$endif}
  else
    Result := ExpandFileName(IncludePathDelimiter(Root) + IncludePathDelimiter(Path) + FileName);
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

function EscapeString(const S: string; const Esc: string; Chars: array of Char; const Escapes: array of string): string;
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

function DescapeString(const S: string; const Esc: string; Chars: array of Char; const Escapes: array of string): string;
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
  d := Round(vPeriod * SecsPerDay);
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
//  h := (s mod 60);
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

function SubStr(const AText: String; AFromIndex, AToIndex: Integer): String;
begin
  if AFromIndex = 0 then
    AFromIndex := 1
  else if AFromIndex < 0 then
    AFromIndex := Length(AText) + AFromIndex;

  if AToIndex <= 0 then
    AToIndex := Length(AText) + AToIndex;

  Result := Copy(AText, AFromIndex, AToIndex - AFromIndex + 1);
end;

function SubStr(const Str: String; vSeperator: Char; vFromIndex, vToIndex: Integer): String;
var
  Index, B, E: Integer;
  C: Char;
begin
  if Str='' then
    Exit('');

  Index := 0;
  B := 0;
  E := 1;
  for C in Str do
  begin
    if (B = 0) and (Index = vFromIndex) then
      B := E;

    if C = vSeperator then
      Inc(Index);

    if (Index = vToIndex + 1) then
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
  if Str = '' then
    Result := ''
  else
    Result := SubStr(Str, vSeperator, vIndex, vIndex);
end;

procedure SpliteStr(S, Separator: string; var Name:string; var Value: string);
var
  p: integer;
begin
  p := Pos(Separator, S);
  if P > 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := Copy(s, p + 1, MaxInt);
  end
  else
  begin
    Name := s;
    Value := '';
  end;
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

function StrInArray(const Str: String; const InArray: array of String; CaseInsensitive: Boolean): Integer;
var
 itm : String;
 i: Integer;
begin
  i := 0;
  for itm in InArray do
  begin
    if CaseInsensitive then
    begin
      if SameText(Str, itm) then
        exit(i);
    end
    else if Str = itm then
       exit(i);
    Inc(i);
  end;
  Result := -1;
end;

function IsStrInArray(const Str: String; const InArray: array of String; CaseInsensitive: Boolean): Boolean;
begin
  Result := (StrInArray(Str, InArray, CaseInsensitive) >= 0);
end;

function StrInArray(const Str: string; const StartIndex: Integer; const InArray: array of string; out SepLength: Integer; CaseInsensitive: Boolean): Integer;
var
 itm : String;
 i: Integer;
begin
  i := 0;
  for itm in InArray do
  begin
    if CaseInsensitive then
    begin
      if SameText(MidStr(Str, StartIndex, Length(Itm)), itm) then
      begin
        SepLength := Length(Itm);
        exit(i);
      end;
    end
    else if MidStr(Str, StartIndex, Length(Itm)) = itm then
    begin
      SepLength := Length(Itm);
      exit(i);
    end;
    Inc(i)
  end;
  SepLength := 0;
  Result := -1;
end;

function IsStrInArray(const Str: string; const StartIndex: Integer; const InArray: array of string; out SepLength: Integer; CaseInsensitive: Boolean): Boolean;
begin
  Result := (StrInArray(Str, StartIndex, InArray, SepLength, CaseInsensitive) >= 0);
end;

function CharArrayToSet(const ArrayOfChar : TArray<Char>) : TSysCharSet;
var
  itm : Char;
begin
  Result := [];
  for itm in ArrayOfChar do
    Include(Result, AnsiChar(itm));
end;

function CharInArray(const C: Char; const ArrayOfChar: array of Char; CaseInsensitive: Boolean): Boolean;
var
  itm : Char;
begin
  if CaseInsensitive then
  begin
    for itm in ArrayOfChar do
      if UpperCase(C) = UpperCase(itm) then
        exit(true);
  end
  else
  begin
    for itm in ArrayOfChar do
      if C = itm then
        exit(true);
  end;

  Result := false;
end;

function EscapeStringC(const S: string; QuoteChar: UTF8Char = '"'): string;
begin
  Result := EscapeString(s, '\', [#8, #9, #10, #13, '\', QuoteChar], ['b', 't', 'n', 'r', '\', QuoteChar]);
end;

function DescapeStringC(const S: string): string;
begin
  Result := DescapeString(s, '\', [#8, #9, #10, #13, '\', '"'], ['b', 't', 'n', 'r', '\', '"']);
end;

function ToUnixPathDelimiter(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function CorrectPath(const Path: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := StringReplace(Path, '/', PathDelim, [rfReplaceAll]);//correct it for linux
  {$else}
  Result := StringReplace(Path, '\', PathDelim, [rfReplaceAll]);//correct it for linux
  {$endif MSWINDOWS}
end;

function SplitPath(Path: string; out Right: string; Index: Integer): string; overload;
var
  l, i, e: Integer;
  c: Char;
begin
  if Path = '' then
  begin
    Result := '';
    Right := '';
  end
  else if Index = 0 then
  begin
    Result := Path;
    Right := '';
  end
  else
  begin
    i := 0;
    l := Length(Path);
    if Index > 0 then
    begin
      e := 1;
      if Path[1] in ['\', '/'] then
      begin
        dec(l);
        inc(e);
      end;
    end
    else
    begin
      e := l;
      if Path[l] in ['\', '/'] then
      begin
        dec(l);
        dec(e);
      end;
    end;

    while l > 0 do
    begin
      C := Path[e];
      if C in ['\', '/'] then
        Inc(i);
      if (i = Abs(Index)) then
        Break;

      if Index > 0 then
        Inc(e)
      else
        Dec(e);
      Dec(l);
    end;
    Result := Copy(Path, 1, e);
    Right := Copy(Path, e + 1, MaxInt);
  end;
end;

function SplitPath(Path: string; Index: Integer): string; overload;
var
  t: string;
begin
  Result := SplitPath(Path, t, Index);
end;

function ExpandFile(const Name: string): string;
var
  aEndsDelimiter: Boolean;
begin
  aEndsDelimiter := EndsDelimiter(Name);
  Result := ExpandFileName(Name);

  if aEndsDelimiter then
  begin
    //posix ExpandFileName remove last PathDelim;
    Result := IncludePathDelimiter(Result);
  end;
end;

function IncludePathDelimiter(const S: string; Force: Boolean = False): string;
begin
  if (Force or (s <> '')) and not EndsStr(PathDelim, s) then
    Result := s + PathDelim
  else
    Result := s;
end;

function IncludeURLDelimiter(const S: string): string;
begin
  if not EndsStr('/', S) then
    Result := S + '/'
  else
    Result := S;
end;

function IsURLDelimiter(const S: string): Boolean;
begin
  Result := IsStrInArray(S, ['/', '\']);
end;

function ExcludePathDelimiter(Path: string): string;
begin
  if Path = '' then
    Result := ''
  else
    Result := ExcludeTrailingPathDelimiter(Path);
end;

function AddStartURLDelimiter(const Path: string; Force: Boolean): string;
begin
  if Force or (Path <> '') then
  begin
    if (Path = '') or not StartsStr(URLPathDelim, Path) then
      Result := URLPathDelim + Path
    else
      Result := Path
  end
  else
    Result := Path
end;

function AddEndURLDelimiter(const Path: string; Force: Boolean): string;
begin
  if Force or (Path <> '') then
  begin
    if (Path = '') or not EndsStr(URLPathDelim, Path) then
      Result := Path + URLPathDelim
    else
      Result := Path
  end
  else
    Result := Path
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

function ISODateToStr(DateTime: TDateTime; vDateSeparator: Char = '-'; TimeDivider: Char = ' '; TimeSeparator: Char = ':'; WithTime: Boolean = False): String; overload;
var
  Y, M, D, H, N, S, O: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := AlignStr(IntToStr(Y), 4, [alsRight], '0') + vDateSeparator +  AlignStr(IntToStr(M), 2, [alsRight],'0') + vDateSeparator + AlignStr(IntToStr(D), 2, [alsRight], '0');
  if WithTime then
  begin
    DecodeTime(DateTime, H, N, S, O);
    Result := Result + TimeDivider + AlignStr(IntToStr(H), 2, [alsRight],'0') + TimeSeparator + AlignStr(IntToStr(N), 2, [alsRight],'0') + TimeSeparator + AlignStr(IntToStr(S), 2, [alsRight], '0');
  end;
end;

function ISODateTimeToStr(DateTime: TDateTime; vDateSeparator: Char; TimeDivider: Char): String;
begin
  Result := ISODateToStr(DateTime, vDateSeparator, TimeDivider, ':',  True);
end;

function DateTimeToRFC822(vDateTime: TDateTime): string;
const
  MonthNames: array[1..12] of String =
    (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
    );

  DayNames: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

var
  aYear, aMonth, aDay: word;
begin
  DecodeDate(vDateTime, aYear, aMonth, aDay);
  Result := DayNames[DayOfWeek(vDateTime)] +', ' + IntToStr(aDay) +' ' + MonthNames[aMonth] + ' ' + FormatDateTime('yyyy hh":"nn":"ss', vDateTime) +' ' + '+000';
end;


function RFC2822ToDateTime(data: string): TDateTime;
  const
    DayShortNames: array[0..6] of string = ('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun');

    MonthShortNames: array[0..11] of string = ('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul',
                 'aug', 'sep', 'oct', 'nov', 'dec');

  type
    TTimezoneEntry = record
      Name: string;
      Offset: Integer;
    end;

  const
    TimeZones: array[0..13] of TTimezoneEntry = (
      (Name: 'UT'; Offset: 0),
      (Name: 'UTC'; Offset: 0),
      (Name: 'GMT'; Offset: 0),
      (Name: 'Z'; Offset: 0),
      (Name: 'AST'; Offset: -400),
      (Name: 'ADT'; Offset: -300),
      (Name: 'EST'; Offset: -500),
      (Name: 'EDT'; Offset: -400),
      (Name: 'CST'; Offset: -600),
      (Name: 'CDT'; Offset: -500),
      (Name: 'MST'; Offset: -700),
      (Name: 'MDT'; Offset: -600),
      (Name: 'PST'; Offset: -800),
      (Name: 'PDT'; Offset: -700)
    );

  function IndexOfTimeZone(const tzName: string): Integer;
  var
    i: Integer;
  begin
    for i := Low(TimeZones) to High(TimeZones) do
      if SameText(tzName, TimeZones[i].Name) then
        Exit(i);
    Result := -1;
  end;

  function Split(const s: string; sep: char): TStringList;
  begin
    Result := TStringList.Create;
    Result.Delimiter := sep;
    Result.StrictDelimiter := True;
    Result.DelimitedText := s;
  end;

var
  parts: TStringList;
  i: Integer;
  dd, mm, yy, thh, tmm, tss: Integer;
  timeStr, ddStr, mmStr, yyStr, tmpStr,
  tm, tz: string;
  tzOffset: Integer;
  tzFound: Boolean;
  monthIndex: Integer;
  dateParts: TStringList;
  sign: Integer;
begin
  Result := 0;
  data := Trim(data);
  if data = '' then
    Exit;

  parts := Split(data, ' ');
  try
    if parts.Count = 0 then
      Exit;

    if (parts[0].EndsWith(',')) or (StrInArray(Copy(parts[0], 1, 3).ToLower, DayShortNames) >= 0) then
      parts.Delete(0) // There's a dayname here. Skip it
    else
    begin
      i := parts[0].LastIndexOf(',');
      if i >= 0 then
        parts[0] := Copy(parts[0], i + 2, MaxInt);
    end;

    if parts.Count < 3 then
      exit
    else if parts.Count = 3 then // RFC 850 date, deprecated
    begin
      dateParts := Split(parts[0], '-');
      try
        if dateParts.Count = 3 then
        begin
          parts.Insert(1, dateParts[1]);
          parts.Insert(1, dateParts[2]);
          parts[0] := dateParts[0];
        end;
      finally
        FreeAndNil(dateParts);
      end;
      exit;
    end
    else if parts.Count = 4 then
    begin
      timeStr := parts[3];
      i := Pos('+', timeStr);
      if i = 0 then
        i := Pos('-', timeStr);
      if i > 0 then
      begin
        parts[3] := Copy(timeStr, 1, i - 1);
        parts.Add(Copy(timeStr, i, MaxInt));
      end
      else
        parts.Add(''); // dummy tz
      exit;
    end;

    ddStr := parts[0];
    mmStr := Copy(parts[1], 1, 3).ToLower;
    yyStr := parts[2];

    tm := parts[3];
    tz := parts[4];

    if (ddStr = '') or (mmStr = '') or (yyStr = '') then
      Exit;

//    if not TryStrToInt(mmStr, monthIndex) then
    monthIndex := StrInArray(mmStr, MonthShortNames);
    if monthIndex < 0 then
    begin
      // Swap dd/mm
      tmpStr := ddStr;
      ddStr := mmStr;
      mmStr := tmpStr.ToLower;
      monthIndex := StrInArray(mmStr, MonthShortNames);
      if monthIndex < 0 then
        Exit;
    end;

    mm := monthIndex + 1;

    if ddStr.EndsWith(',') then
      Delete(ddStr, Length(ddStr), 1);

    if not TryStrToInt(ddStr, dd) then
      Exit;

    if Pos(':', yyStr) > 0 then
    begin
      // swap yy/tm
      tmpStr := tm;
      tm := yyStr;
      yyStr := tmpStr;
    end;

    if yyStr.EndsWith(',') then
    begin
      Delete(yyStr, Length(yyStr), 1);
      if yyStr = '' then
        Exit;
    end;

    //if not yyStr.StartsWith('-') and not yyStr.StartsWith('+') and not yyStr[1].IsDigit then
    if not yyStr.StartsWith('-') and not yyStr.StartsWith('+') and not IsDigit(yyStr[1]) then
    begin
      tmpStr := tz;
      tz := yyStr;
      yyStr := tmpStr;
    end;

    if not TryStrToInt(yyStr, yy) then
      Exit;

    if yy < 100 then
    begin
      if yy > 68 then
        yy := yy + 1900
      else
        yy := yy + 2000;
    end;
  finally
    parts.Free;
  end;

  try
    if tm.EndsWith(',') then
      Delete(tm, Length(tm), 1);

    parts := Split(tm, ':');
    if parts.Count = 2 then
    begin
      thh := StrToIntDef(parts[0], -1);
      tmm := StrToIntDef(parts[1], -1);
      tss := 0;
    end
    else if parts.Count = 3 then
    begin
      thh := StrToIntDef(parts[0], -1);
      tmm := StrToIntDef(parts[1], -1);
      tss := StrToIntDef(parts[2], -1);
    end
    else
      Exit;
  finally
    parts.Free;
  end;

  if (thh < 0) or (tmm < 0) or (tss < 0) then
    Exit;

  tzFound := False;
  tzOffset := 0;
  if tz <> '' then
  begin
    i := IndexOfTimeZone(tz.ToUpper);
    if i >= 0 then
    begin
      tzOffset := TimeZones[i].Offset;
      tzFound := True;
    end
    else if TryStrToInt(tz, tzOffset) then
    begin
      if (tzOffset = 0) and tz.StartsWith('-') then
        tzOffset := 0
      else
        tzFound := True;
    end;
  end;

  if tzFound then
  begin
    sign := 1;
    if tzOffset < 0 then
    begin
      sign := -1;
      tzOffset := -tzOffset;
    end;
    tzOffset := sign * ((tzOffset div 100) * 3600 + (tzOffset mod 100) * 60);
  end
  else
    tzOffset := 0;

  Result := EncodeDate(yy, mm, dd) + EncodeTime(thh, tmm, tss, 0); //tzOffset
  Result := IncHour(Result, tzOffset);
end;

function DateTimeToRFC2822(vDate: TDateTime): string;
var
  aDate: TDateTime;
begin
  {$ifdef FPC}
  aDate := NowUTC;
  {$else}
  aDate := TTimeZone.Local.ToUniversalTime(vDate);
  {$endif}
  Result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', aDate, DefFormatSettings) + ' GMT';
end;

//* thanks to https://stackoverflow.com/a/41726706/585304

{ TEncodingHelper }

class function TEncodingHelper.CodePageEncoding(CodePage: Word): TEncoding;
begin
  case CodePage of
{$IFDEF ANDROID}
    437: Result := ANSI;
{$ENDIF ANDROID}
    1200: Result := Unicode;
    1201: Result := BigEndianUnicode;
    CP_UTF7: Result := UTF7;
    CP_UTF8: Result := UTF8;
  else
    Result := ANSI;
  end;
end;

function TEncodingHelper.GetString(Bytes: PByte; ByteCount: Integer): String;
var
  aLen: Integer;
begin
  Result := '';
  if ByteCount <> 0 then
  begin
    aLen := GetCharCount(Bytes, ByteCount);
    if (aLen <>0) then
    begin
      SetLength(Result, aLen);
      {$ifdef FPC}
      GetChars(Bytes, ByteCount, PUnicodeChar(Result), aLen);
      {$else}
      GetChars(Bytes, ByteCount, PChar(Result), aLen);
      {$endif}
    end;
  end
end;

function TEncodingHelper.GetString(Bytes: PByte; Start, ByteCount: Integer): String;
begin
  if ByteCount<>0 then
    Result := GetString(@Bytes[Start], ByteCount)
  else
    Result := '';
end;


{$ifdef FPC}
function TEncodingHelper.GetString(Bytes: array of Byte): String;
var
  L, Count: Integer;
begin
  {$ifdef FPC}
  Result := '';
  {$endif}
  L := Length(Bytes);
  Count := GetCharCount(@Bytes[0], L);
  if (Count = 0) and (L > 0) then
    raise Exception.Create('Wrong encoding!');
  SetLength(Result, Count);
  GetChars(@Bytes[0], L, PUnicodeChar(Result), Count);
end;
{$endif}

function StringOf(const Value: PByte; Size: Integer; CodePage: Word = CP_UTF8): string;
begin
  Result := TEncoding.CodePageEncoding(CodePage).GetString(Value, Size);
end;

function StringOf(const Value: PByte; Start, Size: Integer; CodePage: Word = CP_UTF8): string; overload;
begin
  Result := StringOf(@Value[Start], Size, CodePage);
end;

function StringOf(const Value: array of Byte; CodePage: Word): string;
begin
  Result := TEncoding.CodePageEncoding(CodePage).GetString(Value);
end;

function StringOf(const Value: TBytes; CodePage: Word): string;
begin
  Result := TEncoding.CodePageEncoding(CodePage).GetString(Value);
end;

function StringOfUTF8(const Value: PByte; Size: Integer): string;
begin
  {$ifdef FPC}
  if Size = 0 then
    exit('');
  SetString(Result, PChar(Value), Size);
  {$else}
  Result := TEncoding.UTF8.GetString(Value, Size);
  {$endif}
end;

function HexToString(const vData: string): string; overload;
var
  b, r: TBytes;
begin
  if vData<>'' then
  begin
    b := TEncoding.ANSI.GetBytes(vData);
    SetLength(r, Length(vData) div 2);

    HexToBin(PByte(@b[0]), PByte(@r[0]), Length(r));
    Result := TEncoding.Unicode.GetString(r);
  end
  else
    Result := '';
end;

function StringToHex(const vData: string): string; overload;
begin
  if vData<>'' then
    Result := StringToHex(PByte(vData), ByteLength(vData))
  else
    Result := '';
end;

function StringToHex(const vData: PByte; vCount: Integer): string;
begin
  SetLength(Result, 2*vCount);
  BinToHex(vData, PByte(Result), vCount);
end;

const
  //H2BValidSet = ['0'..'9','A'..'F','a'..'f'];
  //H2BConvert: array['0'..'f'] of SmallInt = ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15);
  H2BValidSet = [$30..$39,$41..$46,$61..$66];
  H2BConvert: array[$30..$66] of SmallInt = ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15);

function HexToBin(Text : PByte; Buffer: PByte; BufSize: longint): Integer;
var
  I: Integer;
begin
  I := BufSize;
  while I > 0 do
  begin
    if not (Text^ in H2BValidSet) or not ((Text+1)^ in H2BValidset) then Break;
    Buffer^ := (H2BConvert[Text[0]] shl 4) + H2BConvert[Text[1]];
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

procedure BinToHex(Buffer: PByte; Text: PByte; BufSize: longint);
const
  Convert: array[0..15] of Byte = ($30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$41,$42,$43,$44,$45,$46) ; //AnsiString('0123456789ABCDEF');
var
  I: Integer;
  p: PByte;
begin
  p := Text;
  for I := 0 to BufSize - 1 do
  begin
    p^ := Convert[Buffer[I] shr 4];
    Inc(p);
    P^ := Convert[Buffer[I] and $F];
    Inc(p);
  end;
end;

function ByteToBinStr(Value: Byte): string;
var
  i: Integer;
begin
  SetLength(Result, 8);
  for i := 1 to 8 do begin
    if (Value shr (8-i)) and 1 = 0 then begin
      Result[i] := '0'
    end else begin
      Result[i] := '1';
    end;
  end;
end;

function DataToBinStr(var Data; Size: Integer; Separator: string): string;
var
  P: PByte;
begin
  Result := '';
  P := @Data;
  while Size > 0 do
  begin
    Dec(Size);
    if (Result <> '') then
      Result := Result + Separator;
    Result := Result + ByteToBinStr(P[Size]);
  end;
end;

function IsAllLowerCase(S: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(S) do
  begin
    {$ifdef FPC}
    if IsUpper(S[i]) then
    {$else}
    if S[i].IsUpper then
    {$endif}
    begin
      Result := False;
      break;
    end;
  end;
end;

function GetSizeOfFile(const vFile: string): Int64;
var
  R: TSearchRec;
begin
  if SysUtils.FindFirst(vFile, faAnyFile, R) = 0 then
  begin
    Result := R.Size;
    SysUtils.FindClose(R);
  end
  else
    Result := -1;
end;

procedure EnumFiles(FileList: TStrings; const Folder, Filter: string; Options: TEnumFilesOptions); overload;
var
  R: integer;
  SearchRec: TSearchRec;
  aFolder: string;
begin
  aFolder := IncludeTrailingPathDelimiter(Folder);
  R := FindFirst(aFolder + Filter, faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (((efDirectory in Options) and ((SearchRec.Attr and faDirectory) = faDirectory))
      or ((efFile in Options) and ((SearchRec.Attr and faDirectory) <> faDirectory)))
      and ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then
    begin
      if efFullPath in Options then
        FileList.Add(aFolder + SearchRec.Name)
      else
        FileList.Add(SearchRec.Name);
    end;
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeleteFiles(const Path, Files: string): Integer;
var
  FileList: TStringList;
  f: string;
begin
  FileList := TStringList.Create;
  try
    EnumFiles(FileList, Path, Files, [efFile, efFullPath]);
    Result := FileList.Count;
    for f in FileList do
      DeleteFile(f);
  finally
    FileList.Free;
  end;
end;

function FirstFile(const Path, Files: string): string;
var
  FileList: TStringList;
begin
  FileList := TStringList.Create;
  try
    EnumFiles(FileList, Path, Files, [efFile, efFullPath]);
    if FileList.Count > 0 then
      Result := FileList[0]
    else
      Result := ''
  finally
    FileList.Free;
  end;
end;

function LoadFileString(FileName: string): string;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('' , TUTF8Encoding.Create);
  try
    Stream.LoadFromFile(FileName);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function mnMulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
const
  Size = SizeOf(Integer);
  Half = 1 shl (Size - 1);
var
  Temp, x, r: Integer;
begin
  x := nNumber * nNumerator;
  r := x mod nDenominator; //Reaminder

  Result := x div nDenominator;

  if r <> 0 then
  begin
    if r < 0 then r := - r;
    Temp := (r shl Size) div nDenominator;
    if (Temp >= Half) then
    begin
      if Result < 0 then
        Dec(Result)
      else
        Inc(Result);
    end;
  end;
end;

function mnMulDiv(nNumber, nNumerator, nDenominator: Int64): Int64; overload;
const
  Size = SizeOf(Int64);
  Half = UInt64(1) shl (Size-1);
var
  r: Int64;
  Temp: UInt64;
  {$ifdef FPC}
  x: Int64;
  {$endif}
begin
  {$ifdef FPC}
  x := nNumber * nNumerator;
  r := x mod nDenominator; //Reaminder
  Result := x div nDenominator;
  {$else}
  Result := MulDivInt64(nNumber, nNumerator, nDenominator, r);
  {$endif}
  if r <> 0 then
  begin
    if r < 0 then r := -r;
    Temp := UInt64(r shl Size) div UInt64(nDenominator);
    if (Temp >= Half) then
    begin
      if Result < 0 then
        Dec(Result)
      else
        Inc(Result);
    end;
  end;
end;

function mnRound(nNumber: Double): Int64; overload;
begin
  Result := Trunc(nNumber);
  if Abs(Frac(nNumber))>=0.5 then
  begin
    if Result<0 then
      Dec(Result)
    else
      Inc(Result);
  end;
end;


procedure SwapBytes(const Source; out Dest; Size: Integer); overload;
var
  PSource: PByte;
  PDest: PByte;
  I: Integer;
begin
  PSource := PByte(@Source);
  PDest:= PByte(@Dest);

  for I := 0 to Size - 1 do
  begin
    PDest^ := PSource[Size - I - 1];
    Inc(PDest);
  end;
end;

function SwapBytes(const Source: Word): Word; overload;
begin
  SwapBytes(Source, Result, SizeOf(Result));
end;

function SwapBytes(const Source: SmallInt): SmallInt; overload;
begin
  SwapBytes(Source, Result, SizeOf(Result));
end;

function SwapBytes(const Source: Cardinal): Cardinal; overload;
begin
  SwapBytes(Source, Result, SizeOf(Result));
end;

function SwapBytes(const Source: Int64): Int64; overload;
begin
  SwapBytes(Source, Result, SizeOf(Result));
end;

initialization
  DefFormatSettings := TFormatSettings.Invariant;
  {$ifdef windows}
  SystemAnsiCodePage := GetACP; //windows only
  {$else}
  SystemAnsiCodePage := 1252; //scpAnsi has no meaning in linux, you can change it in your application
  {$endif}
end.

