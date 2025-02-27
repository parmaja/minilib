unit mnXMLRttiReader;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$INTERFACES CORBA}
{$mode delphi}
{$ENDIF}

{$define SAFELOAD}

interface

uses
  Classes, SysUtils, TypInfo, 
  mnXML, mnXMLRtti;

type
  TmnXMLRttiObjectFiler = class(TmnXMLRttiFiler)
  private
    CurrentTag: string;
  protected
    function FindClass(const ClassName: string): TClass; virtual;
    function SafeFindClass(const ClassName: string): TClass; 
    function CreateObject(Instance: TObject; const ClassName, Name: string): TObject; virtual;
    procedure SkipProperty(PropName: string);
    procedure PropertyError(PropName: string);
    procedure ReadProperty(Instance: TObject; PropInfo: PPropInfo; const Value: string);
    procedure ReadPropertyValue(Instance: TObject; const PropName: string; const Value: string);
    function ReadVariant(Value:string; ValueType:string):Variant;
  public
    procedure ReadOpen(const Name: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiHeaderFiler = class(TmnXMLRttiFiler)
  public
    procedure ReadOpen(const Name: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiRootFiler = class(TmnXMLRttiFiler)
  public
    procedure ReadOpen(const Name: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiSkipFiler = class(TmnXMLRttiFiler)
  private
  public
    procedure ReadOpen(const Name: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiReader = class(TmnXMLRttiCustomRead)
  private
    function CreateFiler(const PropertyName: string; Instance: Pointer; IsInterface:Boolean): TmnXMLRttiFiler;
  protected
    procedure ReadAttributes(const Text: string); override;
    procedure ReadText(const Text: string); override; //value of property
    procedure ReadCDATA(const Text: string); override; //also value of property
    procedure ReadCloseTag(const Name: string); override;
  public                                                 
    procedure ReadRoot(Instance: TObject); overload;
    function ReadRoot: TObject; overload;
  end;

implementation

uses
  mnXMLUtils;

function StrDateToDate(Value:string): TDateTime;
var
  p:Integer;
  function Fetch(const s:string):Integer;
  var
    i:Integer;
  begin
    Result := 0;
    i := p;
    while True do
    begin
      if (p > Length(s)) or (s[p] in ['-', ':', ' ']) then
      begin
        Result := StrToIntDef(Copy(s, i, p - i), 0);
        Inc(p);
        break;
      end;
      Inc(p);
    end;
  end;
var
  y,m,d,h,n,s: Word;
begin
  p := 1;
  y := Fetch(Value);
  m := Fetch(Value);
  d := Fetch(Value);
  h := Fetch(Value);
  n := Fetch(Value);
  s := Fetch(Value);
  Result := EncodeDate(y, m, d) + EncodeTime(h, n, s, 0); 
end;

type
  THackedComponent = class(TComponent);

{ TmnXMLRttiReader }

procedure TmnXMLRttiReader.ReadAttributes(const Text: string);
begin
  inherited;
  if Stack.Current <> nil then
  begin
    Stack.Current.Attributes.AssignFrom(Text);
    Stack.Current.ReadOpen(CurrentTag);
    Inc(Stack.Current.Depth);
  end;
end;

procedure TmnXMLRttiReader.ReadCDATA(const Text: string);
begin
  inherited;
  if Trim(Text) <> '' then
    Stack.Current.ReadValue(Text);
end;

procedure TmnXMLRttiReader.ReadCloseTag(const Name: string);
begin
  inherited;
  Dec(Stack.Current.Depth);
  if Stack.Current.Depth > 0 then
  begin
    Stack.Current.ReadClose(Name);
//    Stack.Current.Attributes.Assign(nil);
  end
  else
    Stack.Pop
end;

procedure TmnXMLRttiReader.ReadText(const Text: string);
var
  t: string;
begin
  inherited;
  t:= Trim(Text);
  if t <> '' then
  begin
    if Stack.Current = nil then
      raise Exception.Create('Stack.Current is nil');
    Stack.Current.ReadValue(t);
  end;
end;

procedure TmnXMLRttiReader.ReadRoot(Instance: TObject);
begin
  try
    Stack.Push(TmnXMLRttiHeaderFiler.Create(Self, Instance));
    if not Active then
      Start;
    Stack.Pop;
    if Stack.Count > 0 then
      EmnXMLException.Create('Not all tags are closed check the xml');
  finally
    Stack.Clear;
  end;
end;

procedure TmnXMLRttiObjectFiler.ReadPropertyValue(Instance: TObject; const PropName: string; const Value: string);
var
  PropInfo: PPropInfo;
begin
  //need to test the speed
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo^.GetProc <> nil) and //i removed IsStoredProp for reader must not check if stored, it is already have default value and not stored
    ((PropInfo^.SetProc <> nil) or
      ((GetOrdProp(Instance, PropInfo) <> 0) and //Must be not null when read properties or must have a SetProc
      (PropInfo^.PropType^.Kind in [tkClass, tkInterface]))) then
  begin
    {$ifdef SAFELOAD}
    try
    {$endif}
    ReadProperty(Instance, PropInfo, Value)
    {$ifdef SAFELOAD}
    except
    end;
    {$endif}
  end
  else
    SkipProperty(PropName);
end;

function TmnXMLRttiReader.ReadRoot: TObject;
begin
  ReadRoot(nil);
  Result := Stack.Current.Instance;
end;

procedure TmnXMLRttiObjectFiler.ReadOpen(const Name: string);
var
  PropInfo: PPropInfo;
  aObject: TObject;
  aFiler: TmnXMLRttiFiler;
  aInstance: Pointer;
  {$ifdef FPC}
  aClass:string;
  {$endif}
begin
  CurrentTag := Name;

  if Name = 'Object' then
  begin
    aObject := CreateObject(Instance, Attributes['Type'], Attributes['Name']);
    if aObject <> nil then
    begin
{        if aObject is TComponent then
        Include(THackedComponent(aObject).ComponentState, csLoading);}
      (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(Name, aObject, False));
{        if aObject is TComponent then
        THackedComponent(aObject).Loaded;}//not worked
    end
    else
      PropertyError(Name);
  end
  else
  begin
    PropInfo := GetPropInfo(TObject(Instance).ClassInfo, Name);
    if PropInfo <> nil then
    begin
      if PropInfo^.PropType^.Kind in [tkClass, tkInterface] then
      begin
        if PropInfo^.PropType^.Kind = tkInterface then //use this way more safe when build in packages for delphi projects
          aInstance := Pointer(GetInterfaceProp(TObject(Instance), Name))
        else
        begin
          aInstance := Pointer(GetObjectProp(TObject(Instance), Name));
          {$ifdef FPC}
          aClass := Attributes['Class'];
          if (aClass <> '') and (aInstance = nil) and (IsStoredProp(Instance, PropInfo)) then
          begin
            aInstance := CreateObject(Instance, aClass, Attributes['Name']);
            SetObjectProp(TObject(Instance), Name, aInstance);
          end;
          {$endif}
        end;
        (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(Name, aInstance, PropInfo^.PropType^.Kind = tkInterface));
      end
    end
    else
    begin
      aFiler := RttiFilers.CreateFiler(Owner, Name, Instance, False);
      if aFiler <> nil then
        (Owner as TmnXMLRttiReader).Stack.Push(aFiler)
      else
        PropertyError(Name);
    end;
  end;
end;

procedure TmnXMLRttiObjectFiler.ReadValue(const Text: string);
begin
  ReadPropertyValue(Instance, CurrentTag, Text);
end;

function TmnXMLRttiObjectFiler.ReadVariant(Value:string; ValueType:string):Variant;
var
  aSingle:Single;
  aDouble:Double;
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
    Result := StrDateToDate(Value)
  else if ValueType = 'Boolean' then
    Result := StrToBool(Value)
  else
    Result := Value;
end;

procedure TmnXMLRttiObjectFiler.PropertyError(PropName: string);
begin
  SkipProperty(PropName);
end;

procedure TmnXMLRttiObjectFiler.ReadProperty(Instance: TObject; PropInfo: PPropInfo; const Value: string);
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
    SetVariantProp(Instance, PropInfo, ReadVariant(Value, Attributes.Values['ValueType']));
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
      (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, False));
    end;
  end;

  procedure ReadInterfaceProp;
  var
    aObject: TObject;
  begin
    aObject := TObject(GetOrdProp(Instance, PropInfo));
    (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, True));
  end;
begin
  PropType := GetPropTypeInfo(PropInfo);
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
    else
      ;
  end;
end;

procedure TmnXMLRttiObjectFiler.SkipProperty(PropName: string);
begin
  (Owner as TmnXMLRttiReader).Stack.Push(TmnXMLRttiSkipFiler.Create(Owner, Instance));
end;

function TmnXMLRttiReader.CreateFiler(const PropertyName: string; Instance: Pointer; IsInterface:Boolean): TmnXMLRttiFiler;
begin
  Result := RttiFilers.CreateFiler(Self, PropertyName, Instance, IsInterface, TmnXMLRttiObjectFiler)
end;

procedure TmnXMLRttiObjectFiler.ReadClose(const Name: string);
begin
end;

{ TmnXMLRttiRootFiler }

procedure TmnXMLRttiRootFiler.ReadClose(const Name: string);
begin
end;

procedure TmnXMLRttiRootFiler.ReadOpen(const Name: string);
begin
  if Name <> 'Object' then
    raise EmnXMLParserException.Create('Root tag <Object> not defined');
  if Instance = nil then
    Instance := FindClass(Attributes.Values['Class']).Create;
  (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler('', Instance, False));
end;

procedure TmnXMLRttiRootFiler.ReadValue(const Text: string);
begin
  raise EmnXMLException.Create('Text junk not accepted');
end;

procedure TmnXMLRttiRootFiler.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
begin
end;

{ TmnXMLRttiSkipFiler }

procedure TmnXMLRttiSkipFiler.ReadClose(const Name: string);
begin
end;

procedure TmnXMLRttiSkipFiler.ReadOpen(const Name: string);
begin
end;

procedure TmnXMLRttiSkipFiler.ReadValue(const Text: string);
begin
  //Skipped
end;

procedure TmnXMLRttiObjectFiler.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
begin
end;

procedure TmnXMLRttiSkipFiler.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
begin
end;

function TmnXMLRttiObjectFiler.FindClass(const ClassName: string): TClass;
begin
  Result := Classes.GetClass(ClassName);
end;

function TmnXMLRttiObjectFiler.CreateObject(Instance: TObject; const ClassName, Name: string): TObject;
var
  aClass: TClass;
begin
  aClass := FindClass(ClassName);
  if aClass <> nil then
  begin
    if aClass.InheritsFrom(TComponent) then
    begin
      if Instance is TComponent then
        Result := TComponentClass(aClass).Create(THackedComponent(Instance).GetChildOwner)
      else
        Result := TComponentClass(aClass).Create(nil);
      TComponent(Result).Name := Name;
    end
    {$ifdef FPC}
    else if Supports(Instance, IRttiFiler) then
    //else if Instance is IRttiFiler then //@FPC why not easy like that
    begin
      Result := nil;
      (Instance as IRttiFiler).RttiCreateObject(Result, Instance, aClass, ClassName, Name);
    end
    {$endif}
    else
      Result := nil;
      //Result := aClass.Create;//zaher: wrong create idea
  end
  else
    Result := nil;
end;

{ TmnXMLRttiHeaderFiler }

procedure TmnXMLRttiHeaderFiler.ReadClose(const Name: string);
begin
end;

procedure TmnXMLRttiHeaderFiler.ReadOpen(const Name: string);
begin
  if Name <> 'rtti' then
    raise EmnXMLParserException.Create('Header tag <rtti> not defined');
  if Attributes.Values['author'] <> cRttiAuthor then
    raise EmnXMLParserException.Create('Header author attribute must be MiniXML');
  if Attributes.Values['version'] <> cRttiVersion then
    raise EmnXMLParserException.Create('Version not compatible with ' + cRttiVersion);
  (Owner as TmnXMLRttiReader).Stack.Push(TmnXMLRttiRootFiler.Create(Owner, Instance));
end;

procedure TmnXMLRttiHeaderFiler.ReadValue(const Text: string);
begin
  raise EmnXMLException.Create('Text junk not accepted');
end;

procedure TmnXMLRttiHeaderFiler.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
begin
end;

function TmnXMLRttiObjectFiler.SafeFindClass(const ClassName: string): TClass;
begin
  Result := FindClass(ClassName);
  if Result = nil then
    raise EmnXMLParserException.Create('Class not found ' + ClassName);
end;

end.
