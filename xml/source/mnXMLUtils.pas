unit mnXMLUtils;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

interface

uses
  Classes, SysUtils, Typinfo, Variants,
  {$ifdef FPC}
  uriparser,
  {$endif}
  mnUtils;

const
  sNotWellFormed = 'not well-formed';
  sCanotHaveDOCTYPEDeclaration = 'Cannot have a DOCTYPE declaration outside of a prolog';
  sStackIsEmpty = 'Stack is empty';

const
  OPEN_IDENTIFIER_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = OPEN_IDENTIFIER_CHARS + ['0'..'9', '-', '.', ':']; // : xdebug send tag like xdebug:message
  sXMLAnsiOpen = '<?xml '; //with the space
  sCloseComment = '-->';
  sCDATA = 'CDATA';
  sOpenCDATA = sCDATA + '[';
  sCloseCDATA = ']]>';
  sWhitespace = [' ', #9, #13, #10];
  sAllWhitespace = [#0] + sWhitespace;

const
  cRttiVersion = '1.0';
  cRttiAuthor = 'MiniXML';

function GetPropTypeInfo(PropInfo: PPropInfo): PTypeInfo;
Function PropType(PropInfo: PPropInfo): TTypeKind; //need to move to FPC typeinfo.pp
function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;

function RemoveEncloses(S, Left, Right: string): string;
function Enclose(S, Left: string; Right: string = ''): string;
function RangeStr(s: string; Start, Stop: Integer): string;
function ScanIdentifier(const s: string; Start: Integer): Integer;
function ScanQuoted(SubStr, Text: string): Integer;
function CreateAttStrings(const Attributes: string): TStrings;
procedure ReadAttStrings(Strings: TStrings; const Attributes: string);

function CutStr(const ID, S: string; Dequote: Boolean = False): string;
function URIToFileName(const URI: string): string;
function FileNameToURI(FileName: string): string;
function IncludeSlash(const S: string): string;
function LeftSubStr(S, Separator: string): string;
function RightSubStr(S, Separator: string): string;

implementation

uses
  StrUtils;

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
    if CharInSet(s[i], IDENTIFIER_CHARS) then
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

procedure ReadAttStrings(Strings: TStrings; const Attributes: string);
begin
  StrToStringsCallback(Attributes, Strings, @StrToStringsDeqouteCallbackProc, [' '], [#0, #13, #10]);
end;

function CreateAttStrings(const Attributes: string): TStrings;
begin
  Result := TStringList.Create;
  ReadAttStrings(Result, Attributes);
end;

function CutStr(const ID, S: string; Dequote: Boolean = False): string;
begin
  Result := MidStr(S, Length(ID) + 1, MaxInt);
  if Dequote then
    Result := DequoteStr(Trim(Result));
end;

function URIToFileName(const URI: string): string;
{$ifdef FPC}
begin
  uriparser.URIToFilename(URI, Result);
end;

{$else}
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
{$endif}

function FileNameToURI(FileName: string): string;
const
  sChars = ['-', '_', '.', '!', '~', '*', '''', '(', ')'];
var
  i: Integer;
begin
//  FileName := StringReplace(FileName, '\', '/', [rfReplaceAll]);
  Result := '';
  for i := 1 to Length(FileName) do
  begin
    if CharInSet(FileName[i], sChars) or CharInSet(FileName[i], ['A'..'Z', 'a'..'z', '0'..'9']) then
      Result := Result + FileName[i]
    else
      Result := Result + '%' + IntToHex(Ord(FileName[i]), 2);
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

function GetPropTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType
{$ELSE}
  Result := PropInfo^.PropType^
{$ENDIF}
end;

function PropType(PropInfo: PPropInfo): TTypeKind;
begin
  Result := PropInfo^.PropType^.Kind;
end;

function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Int64; //more compatible with FPC
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultBoolProp: Boolean;
  var
    Value: Int64;
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

  function IsDefaultWideStrProp: Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
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
    Value: IInterface;
  begin
    Value := GetInterfaceProp(Instance, PropInfo);
    Result := Value = nil;
  end;
begin
  Result := True; // not default for default :P
  if (PropInfo^.GetProc <> nil) and ((PropInfo^.SetProc <> nil) or (PropInfo^.PropType^.Kind = tkClass) or (PropInfo^.PropType^.Kind = tkInterface)) then
  begin
    PropType := GetPropTypeInfo(PropInfo);
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkWString, tkUString:
        Result := IsDefaultWideStrProp;
      tkString, tkLString:
        Result := IsDefaultStrProp;
      tkMethod: Result := False;
      tkVariant:
        Result := IsDefaultVariantProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
      tkClass:
        Result := IsDefaultClassProp;//TODO:BUG when published Items of collection to inherited parent items
      tkInterface:
        Result := IsDefaultInterfaceProp;
{$IFDEF FPC}
      tkAString:
        Result := IsDefaultStrProp;
      tkBool:
        Result := IsDefaultBoolProp;
{$ENDIF}
      else
      begin
      end;
    end;
  end;
end;

function LeftSubStr(S, Separator: string): string;
var
  p: Integer;
begin
  p := Pos(Separator, S);
  if p > 0 then
    Result := Copy(S, 1, P - 1)
  else
    Result := S;
end;

function RightSubStr(S, Separator: string): string;
var
  p: Integer;
begin
  p := Pos(Separator, S);
  if p > 0 then
    Result := Copy(S, P + 1, MaxInt)
  else
    Result := '';
end;

end.
