unit mnXMLUtils;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Classes, SysUtils, Typinfo;

const
  sNotWellFormed = 'not well-formed';
  sCanotHaveDOCTYPEDeclaration = 'Cannot have a DOCTYPE declaration outside of a prolog';
  sStackIsEmpty = 'Stack is empty';

const
  OPEN_IDENTIFIER_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = OPEN_IDENTIFIER_CHARS + ['0'..'9', '-', '.'];
  sXMLAnsiOpen = '<?xml '; //with the space
  sCloseComment = '-->';
  sCDATA = 'CDATA';
  sOpenCDATA = sCDATA + '[';
  sCloseCDATA = ']]>';
  sWhitespace = [' ', #9, #13, #10];

const
  cRttiVersion = '1.0';
  cRttiAuthor = 'MiniXML';

function GetPropType(PropInfo: PPropInfo): PTypeInfo;
function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
function RepeatString(const Str: string; Count: Integer): string;
function RemoveEncloses(S, Left, Right: string): string;
function Enclose(S, Left: string; Right: string = ''): string;
function RangeStr(s: string; Start, Stop: Integer): string;
function ScanIdentifier(const s: string; Start: Integer): Integer;
function ScanQuoted(SubStr, Text: string): Integer;
function CreateAttStrings(const Attributes: string): TStrings;
procedure ReadAttStrings(Strings: TStrings; const Attributes: string);
function DequoteStr(Str: string): string;
function QuoteStr(Str: string; QuoteChar: string = '"'): string;
function CutStr(const ID, S: string; Dequote: Boolean = False): string;
function ExpandToPath(FileName: string; Path: string): string;
function StringsToString(Strings: TStrings; LineBreak: string = sLineBreak): string;
function URIToFileName(const URI: string): string;
function FileNameToURI(FileName: string): string;
function IncludeSlash(const S: string): string;
function StrToStrings(Separators, WhiteSpace: TSysCharSet; Content: string; Strings: TStrings): Integer;
function LeftSubStr(S, Separator:string):string;

implementation

uses
{$IFDEF FPC}
  mnXMLFPClasses,
{$ENDIF}
  StrUtils,
  Variants;

function RepeatString(const Str: string; Count: Integer): string;
begin
  Result := '';
  while Count > 0 do
  begin
    Result := Result + Str;
    Count := Count - 1;
  end;
end;

function RemoveEncloses(S, Left, Right: string): string;
var
  f, c: Integer;
begin
  if UpperCase(LeftStr(s, Length(Left))) = UpperCase(Left) then
    f := 6
  else
    f := 0;
  if UpperCase(RightStr(s, Length(Right))) = UpperCase(Right) then
    c := 2
  else
    c := 0;
  c := Length(s) - f - c;
  Result := MidStr(S, f, c)
end;

function Enclose(S, Left, Right: string): string;
begin
  if S <> '' then
    Result := Left + S + Right
  else
    Result := '';
end;

function RangeStr(s: string; Start, Stop: Integer): string;
begin
  Result := MidStr(s, Start, Stop - Start + 1);
end;

function ScanIdentifier(const s: string; Start: Integer): Integer;
var
  i: Integer;
begin
  i := Start;
  Result := i;
  while i <= Length(s) do
  begin
    if (s[i] in IDENTIFIER_CHARS) then
    begin
      Result := i;
      Inc(i);
    end
    else
      break;
  end;
end;

function ScanQuoted(SubStr, Text: string): Integer;
var
  r, l, c, i: integer;
  Quoted: string;
begin
  l := Length(Text);
  Result := l;
  r := Length(SubStr);
  if r = 0 then
    raise Exception.Create('You scan for what!');
  c := 1;
  i := 1;
  Quoted := '';
  while i <= l do
  begin
    if (Text[i] = Quoted) then
    begin
      Quoted := '';
      Inc(i);
    end
    else if (Text[i] = '"') or (Text[i] = '''') then
    begin
      Quoted := Text[i];
      Inc(i);
    end
    else if Quoted <> '' then
    begin
      Inc(i);
    end
    else if Text[i] = SubStr[c] then
    begin
      Inc(i);
      if c >= r then
      begin
        Result := i - c;
        break;
      end;
      Inc(c);
    end
    else
    begin
      if c = 1 then
        Inc(i);
      c := 1;
    end;
  end;
end;

procedure ReadAttStrings(Strings: TStrings; const Attributes: string); overload;
begin
  StrToStrings([' '], [], PChar(Attributes), Strings);
end;

function CreateAttStrings(const Attributes: string): TStrings; overload;
begin
  Result := TStringList.Create;
  ReadAttStrings(Result, Attributes);
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

function CutStr(const ID, S: string; Dequote: Boolean = False): string;
begin
  Result := MidStr(S, Length(ID) + 1, MaxInt);
  if Dequote then
    Result := DequoteStr(Trim(Result));
end;

function ExpandToPath(FileName: string; Path: string): string;
begin
  if (FileName <> '') and ((LeftStr(FileName, 3) = '../') or (LeftStr(FileName, 3) = '..\')) then
    Result := ExpandFileName(IncludeTrailingPathDelimiter(Path) + FileName)
  else if (FileName <> '') and ((LeftStr(FileName, 2) = './') or (LeftStr(FileName, 2) = '.\')) then
    Result := IncludeTrailingPathDelimiter(Path) + RightStr(FileName, Length(FileName) - 2)
  else if (FileName <> '') and (LeftStr(FileName, 2) <> '\\') and ((LeftStr(FileName, 1) = '/') or (LeftStr(FileName, 1) = '\')) then
    Result := ExtractFileDrive(Path) + FileName
  else if ExtractFilePath(FileName) = '' then
    Result := IncludeTrailingPathDelimiter(Path) + FileName
  else
    Result := FileName;
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

function URIToFileName(const URI: string): string;
var
  i, l, p: integer;
const
  sFP = 'file:///';
begin
  Result := '';
  l := Length(URI);
  i := 1;
  if SameText(Copy(URI, 1, Length(sFP)), sFP) then
    i := Length(sFP) + 1;
  while i <= l do
  begin
    P := PosEx('%', URI, i);
    if P > 0 then
    begin
      Result := Result + copy(URI, i, p - i) + char(StrToInt('$' + Copy(URI, p + 1, 2)));
      i := p + 3;
    end
    else
    begin
      Result := Result + copy(URI, i, MaxInt);
      break;
    end;
  end;
end;

function FileNameToURI(FileName: string): string;
const
  sChars = ['-' , '_' , '.' , '!' , '~' , '*' , '''' , '(' , ')'];
var
  i: Integer;
begin
//  FileName := StringReplace(FileName, '\', '/', [rfReplaceAll]);
  Result := '';
  for i := 1 to Length(FileName) do
  begin
    if (FileName[i] in sChars) or (FileName[i] in ['A'..'Z', 'a'..'z', '0'..'9']) then
      Result := Result + FileName[i]
    else
      Result := Result + '%'+ IntToHex(Ord(FileName[i]), 2);
  end;
  Result := 'file:///' + Result;
end;

function IncludeSlash(const S: string): string;
begin
  if (s <> '') and (RightStr(S, 1) <> '/') then
    Result := s + '/'
  else
    Result := s;
end;

function GetPropType(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType
{$ELSE}
  Result := PropInfo^.PropType^
{$ENDIF}
end;

function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    Result := Value = 0; ;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    Result := VarIsClear(Value);
  end;

  function IsDefaultClassProp: Boolean;
  var
    Value: TObject;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    Result := Value = nil;
  end;

  function IsDefaultInterfaceProp: Boolean;
  var
    Value: Pointer;
  begin
    Value := Pointer(Longword(GetOrdProp(Instance, PropInfo)));
    Result := Value = nil;
  end;
begin
  Result := True;
  if (PropInfo^.GetProc <> nil) and ((PropInfo^.SetProc <> nil) or (PropInfo^.PropType^.Kind = tkClass) or (PropInfo^.PropType^.Kind = tkInterface)) then
  begin
    PropType := GetPropType(PropInfo);
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkString, tkLString, tkWString:
        Result := IsDefaultStrProp;
      tkMethod: Result := False;
      tkVariant:
        Result := IsDefaultVariantProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
      tkClass:
        Result := IsDefaultClassProp;
      tkInterface:
        Result := IsDefaultInterfaceProp;
    end;
  end;
end;

function StrToStrings(Separators, WhiteSpace: TSysCharSet; Content: string; Strings: TStrings): Integer;
var
  Head, Tail: Integer;
  EOS, InQuote: Boolean;
  QuoteChar: Char;
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
        while Content[Tail] in WhiteSpace + [#13, #10] do
          Tail := Tail + 1;
        Head := Tail;
        while True do
        begin
          while (InQuote and not (Content[Tail] in [QuoteChar, #0])) or
            not (Content[Tail] in Separators + [#0, #13, #10, '''', '"']) do
            Tail := Tail + 1;
          if Content[Tail] in ['''', '"'] then
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
        EOS := Tail > Length(Content);                 
        if (Head <> Tail) then
        begin
          if Strings <> nil then
          begin
            Strings.Add(Copy(Content, Head, Tail - Head));
          end;
          Inc(Result);
        end;
        Tail := Tail + 1;
      until EOS;
    finally
      Strings.EndUpdate;
    end;
  end;
end;

function LeftSubStr(S, Separator:string):string;
var
  p:Integer;
begin
  p := Pos(Separator, S);
  if p > 0 then
    Result := Copy(S, 1 , P - 1)
  else
    Result := S;
end;

function RightSubStr(S, Separator:string):string;
var
  p:Integer;
begin
  p := Pos(Separator, S);
  if p > 0 then
    Result := Copy(S, P + 1 , MaxInt)
  else
    Result := '';
end;

end.

