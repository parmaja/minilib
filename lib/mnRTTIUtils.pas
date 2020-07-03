unit mnRTTIUtils;
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
  Classes, SysUtils, StrUtils, DateUtils, Types, TypInfo, Variants,
  mnUtils;

function GetPropTypeInfo(PropInfo: PPropInfo): PTypeInfo;
function GetPropTypeKind(PropInfo: PPropInfo): TTypeKind;
function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
function GetVariantAs(Value:string; ValueType: string): Variant;
procedure SetPropertyValue(Instance: TObject; const PropName: string; const Value: string); overload;
procedure SetPropertyValue(Instance: TObject; PropInfo: PPropInfo; const Value: string); overload;

implementation

function GetPropTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType
{$ELSE}
  Result := PropInfo^.PropType^
{$ENDIF}
end;

function GetPropTypeKind(PropInfo: PPropInfo): TTypeKind;
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
    end;
  end;
end;

function GetVariantAs(Value:string; ValueType: string): Variant;
var
  aSingle: Single;
  aDouble: Double;
begin
  //need to improve this function
  if (ValueType = 'String') or (ValueType = 'UString') then
    Result := Value
  else if ValueType = 'OleStr' then
    Result := Value
  else if ValueType = 'Byte' then
    Result := Byte(StrToInt(Value))
  else if ValueType = 'ShortInt' then
    Result := SmallInt(StrToInt(Value))
  else if ValueType = 'Word' then
    Result := Word(StrToInt(Value))
  else if ValueType = 'SmallInt' then
    Result := SmallInt(StrToInt(Value))
  else if ValueType = 'Integer' then
    Result := StrToInt(Value)
  else if ValueType = 'Single' then
  begin
    aSingle := StrToFloat(Value);
    Result := aSingle;
  end
  else if ValueType = 'Double' then
  begin
    aDouble := StrToFloat(Value);
    Result := aDouble;
  end
  else if ValueType = 'Currency' then
    Result := StrToCurr(Value)
  else if ValueType = 'Int64' then
    Result := StrToInt64(Value)
  else if ValueType = 'LongWord' then
    Result := StrToInt64(Value)
  else if ValueType = 'Date' then
    Result := ISOStrToDate(Value)
  else if ValueType = 'Boolean' then
    Result := StrToBool(Value)
  else
    Result := Value;
end;

procedure SetPropertyValue(Instance: TObject; PropInfo: PPropInfo; const Value: string);
var
  PropType: PTypeInfo;
  TypeData: PTypeData;

  procedure ReadIntegerProp;
  var
    Data: Longint;
    IdentToInt: TIdentToInt;
  begin
    Data := 0;
    IdentToInt := FindIdentToInt(PropType);
    if not ((Assigned(IdentToInt) and IdentToInt(Value, Data))) then
    begin
      Data := StrToIntDef(Value, 0);
    end;
    SetOrdProp(Instance, PropInfo, Data);
  end;

  procedure ReadBoolProp;
  begin
    SetOrdProp(Instance, PropInfo, Ord(StrToBoolDef(Value, True)));
  end;

  procedure ReadCharProp;
  var
    Data: Longint;
  begin
    if Value <> '' then
    begin
      if (Length(Value)>1) and (Value[1]='#') then
        Data := StrToIntDef(Copy(Value, 2, MaxInt), 0)
      else
        Data := Ord(Value[1]);
    end
    else
      Data := 0;
    SetOrdProp(Instance, PropInfo, Data);
  end;

  procedure ReadEnumeration;
  begin
    SetEnumProp(Instance, PropInfo, Value);
  end;

  procedure ReadSet;
  begin
    SetSetProp(Instance, PropInfo, Value);
  end;

  procedure ReadInt64Prop;
  var
    Data: Int64;
  begin
    Data := StrToInt64(Value);
    SetInt64Prop(Instance, PropInfo, Data);
  end;

  procedure ReadFloatProp;
  var
    Data: Extended;
  begin
    Data := StrToFloat(Value);
    SetFloatProp(Instance, PropInfo, Data);
  end;

  procedure ReadCurrProp;
  var
    Data: Currency;
  begin
    Data := StrToCurr(Value);
    SetFloatProp(Instance, PropInfo, Data);
  end;

  procedure ReadWideStringProp;
  begin
    SetWideStrProp(Instance, PropInfo, widestring(Value));
  end;

  procedure ReadStringProp;
  begin
    SetStrProp(Instance, PropInfo, Value);
  end;

  procedure ReadVariantProp;
  begin
    //SetVariantProp(Instance, PropInfo, ReadVariant(Value, Attributes.Values['ValueType']));
  end;

  procedure ReadObjectProp;
  var
    aObject: TObject;
  begin
    aObject := TObject(GetOrdProp(Instance, PropInfo));
    if (aObject <> nil) and (aObject is TComponent) and not (csSubComponent in (aObject as TComponent).ComponentStyle) then
    begin
      //not now there is long story
    end
    else
    begin
      //(Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, False));
    end;
  end;

  procedure ReadInterfaceProp;
  var
    aObject: TObject;
  begin
    aObject := TObject(GetOrdProp(Instance, PropInfo));
    //(Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, True));
  end;
begin
  if Instance = nil then
    raise Exception.Create('Instance is null');
  PropType := GetPropTypeInfo(PropInfo);
  if PropType = nil then
    raise Exception.Create('PropType is null');
  TypeData := GetTypeData(PropType);
  case PropType^.Kind of
    tkInteger:
      ReadIntegerProp;
    tkChar:
      ReadCharProp;
    tkSet:
      ReadSet;
    tkEnumeration:
      ReadEnumeration;
    tkInt64:
      ReadInt64Prop;
    tkFloat:
    begin
      if (TypeData <> nil) and (TypeData^.FloatType = ftCurr) then
        ReadCurrProp
      else
        ReadFloatProp;
    end;
    tkWString, tkUString:
      ReadWideStringProp;
    tkLString, tkString:
      ReadStringProp;
    tkVariant:
      ReadVariantProp;
    tkClass:
      ReadObjectProp;
    tkMethod: ; //not yet
    tkInterface:
      ReadInterfaceProp; //not yet
    {$IFDEF FPC}
    tkAString:
      ReadStringProp;
    tkBool:
      ReadBoolProp;
    {$ENDIF}
  end;
end;

procedure SetPropertyValue(Instance: TObject; const PropName: string; const Value: string);
var
  PropInfo: PPropInfo;
begin
  if Instance = nil then
    raise Exception.Create('Instance is null');
  //need to test the speed
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if PropInfo = nil then
    raise Exception.Create('PropInfo is null');
  if (PropInfo^.GetProc <> nil) and //i removed IsStoredProp for reader must not check if stored, it is already have default value and not stored
    ((PropInfo^.SetProc <> nil) or
      ((GetOrdProp(Instance, PropInfo) <> 0) and //Must be not null when read properties or must have a SetProc
      (PropInfo^.PropType^.Kind in [tkClass, tkInterface]))) then
  begin
    {$ifdef SAFELOAD}
    try
    {$endif}
    SetPropertyValue(Instance, PropInfo, Value)
    {$ifdef SAFELOAD}
    except
    end;
    {$endif}
  end
{  else
    SkipProperty(PropName);}
end;

end.
