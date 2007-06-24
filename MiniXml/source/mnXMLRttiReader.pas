unit mnXMLRttiReader;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Classes, SysUtils, TypInfo,
  mnXML, mnXMLRtti, mnXMLFPClasses;

type
  TmnXMLRttiObjectFilerState = (ofsNone, ofsProperty);

  TmnXMLRttiObjectFiler = class(TmnXMLRttiFiler)
  private
    FReadState: TmnXMLRttiObjectFilerState;
    CurrentTag: string;
  protected
    function FindClass(const ClassName: string): TClass; virtual;
    function CreateObject(Instance: TObject; const ClassName, Name: string): TObject; virtual;
    procedure SkipProperty(PropName: string);
    procedure PropertyError(PropName: string);
    procedure ReadProperty(Instance: TObject; PropInfo: PPropInfo; const Value: string);
    procedure ReadPropertyValue(Instance: TObject; const PropName: string; const Value: string);
  public
    procedure ReadOpen(const Name, Attributes: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiHeaderFiler = class(TmnXMLRttiFiler)
  public
    procedure ReadOpen(const Name, Attributes: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiRootFiler = class(TmnXMLRttiFiler)
  public
    procedure ReadOpen(const Name, Attributes: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiSkipFiler = class(TmnXMLRttiFiler)
  private
  public
    procedure ReadOpen(const Name, Attributes: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiReader = class(TmnXMLRttiCustomRead)
  private
    function CreateFiler(const PropertyName: string; Instance: TObject; IsInterface:Boolean): TmnXMLRttiFiler;
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

type
  THackedComponent = class(TComponent);

{ TmnXMLRttiReader }

procedure TmnXMLRttiReader.ReadAttributes(const Text: string);
begin
  inherited;
  Stack.Current.ReadOpen(CurrentTag, Text);
  Inc(Stack.Current.Depth);
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
    Stack.Current.ReadClose(Name)
  else
    Stack.Pop
end;

procedure TmnXMLRttiReader.ReadText(const Text: string);
begin
  inherited;
  if Trim(Text) <> '' then
    Stack.Current.ReadValue(Text);
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

procedure TmnXMLRttiObjectFiler.ReadPropertyValue(Instance: TObject;
  const PropName: string; const Value: string);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if IsStoredProp(Instance, PropInfo) and (PropInfo^.GetProc <> nil) and
    ((PropInfo^.SetProc <> nil) or
    ((GetOrdProp(Instance, PropInfo) <> 0) and //Must be not null when read properties or must have a SetProc
    (PropInfo^.PropType^.Kind in [tkClass, tkInterface]))) then
  begin
    ReadProperty(Instance, PropInfo, Value)
  end
  else
    SkipProperty(PropName);
end;

function TmnXMLRttiReader.ReadRoot: TObject;
begin
  ReadRoot(nil);
  Result := Stack.Current.Instance;
end;

procedure TmnXMLRttiObjectFiler.ReadOpen(const Name, Attributes: string);
var
  PropInfo: PPropInfo;
  aStrings: TStrings;
  aObject: TObject;
  aFiler: TmnXMLRttiFiler;
begin
  CurrentTag := Name;
  if Name = 'Object' then
  begin
    aStrings := CreateAttStrings(Attributes);
    try
      aObject := CreateObject(Instance, DequoteStr(aStrings.Values['Type']), DequoteStr(aStrings.Values['Name']));
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
    finally
      aStrings.Free;
    end;
  end
  else
  begin
    PropInfo := GetPropInfo(TObject(Instance).ClassInfo, Name);
    if PropInfo <> nil then
    begin
      if PropInfo^.PropType^.Kind in [tkClass, tkInterface] then
      begin
        (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(Name, TObject(GetOrdProp(TObject(Instance), Name)), PropInfo^.PropType^.Kind = tkInterface));
      end
      else
        FReadState := ofsProperty;
    end
    else
    begin
      aFiler := PermanentRegister.CreateFiler(Owner, Name, Instance, False);
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

procedure TmnXMLRttiObjectFiler.PropertyError(PropName: string);
begin
  SkipProperty(PropName);
end;

procedure TmnXMLRttiObjectFiler.ReadProperty(Instance: TObject;
  PropInfo: PPropInfo; const Value: string);
var
  PropType: PTypeInfo;

  procedure ReadIntProp;
  var
    Data: Longint;
    IdentToInt: TIdentToInt;
  begin
    IdentToInt := FindIdentToInt(PropType);
    if not ((Assigned(IdentToInt) and IdentToInt(Value, Data))) then
      Data := StrToInt(Value);
    SetOrdProp(Instance, PropInfo, Data);
  end;

  procedure ReadCharProp;
  var
    Data: Longint;
  begin
    if Value <> '' then
      Data := Ord(Value[1])
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

  procedure ReadStrProp;
  begin
    SetWideStrProp(Instance, PropInfo, Value);
  end;

  procedure ReadVariantProp;
  begin
    SetVariantProp(Instance, PropInfo, Value);
  end;

  procedure ReadObjectProp;
  var
    Data: TObject;
  begin
    Data := TObject(GetOrdProp(Instance, PropInfo));
    if (Data <> nil) and (Data is TComponent) and not (csSubComponent in (Data as TComponent).ComponentStyle) then
    begin
      //not now there is long story
    end
    else
    begin
      (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, Data, False));
    end;
  end;

  procedure ReadInterfaceProp;
  var
    Data: TObject;
  begin
    Data := TObject(GetOrdProp(Instance, PropInfo));
    (Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, Data, True));
  end;
begin
  PropType := GetPropType(PropInfo);
  case PropType^.Kind of
    tkInteger:
      ReadIntProp;
    tkChar:
      ReadCharProp;
    tkSet:
      ReadSet;
    tkEnumeration:
      ReadEnumeration;
    tkInt64:
      ReadInt64Prop;
    tkFloat:
      ReadFloatProp;
    tkString, tkLString, tkWString:
      ReadStrProp;
    tkVariant:
      ReadVariantProp;
    tkClass:
      ReadObjectProp;
    tkMethod: ; //not yet
    tkInterface:
      ReadInterfaceProp; //not yet
  end;
end;

procedure TmnXMLRttiObjectFiler.SkipProperty(PropName: string);
begin
  (Owner as TmnXMLRttiReader).Stack.Push(TmnXMLRttiSkipFiler.Create(Owner, Instance));
end;

function TmnXMLRttiReader.CreateFiler(const PropertyName: string; Instance: TObject; IsInterface:Boolean): TmnXMLRttiFiler;
begin
  Result := PermanentRegister.CreateFiler(Self, PropertyName, Instance, IsInterface, TmnXMLRttiObjectFiler)
end;

procedure TmnXMLRttiObjectFiler.ReadClose(const Name: string);
begin
  FReadState := ofsNone;
end;

{ TmnXMLRttiRootFiler }

procedure TmnXMLRttiRootFiler.ReadClose(const Name: string);
begin
end;

procedure TmnXMLRttiRootFiler.ReadOpen(const Name, Attributes: string);
var
  aStrings: TStrings;
begin
  if Name <> 'Object' then
    raise EmnXMLParserException.Create('Root tag <Object> not defined');
  aStrings := CreateAttStrings(Attributes);
  try
    if Instance = nil then
      Instance := FindClass(aStrings.Values['Class']).Create;
  finally
    aStrings.Free;
  end;
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

procedure TmnXMLRttiSkipFiler.ReadOpen(const Name, Attributes: string);
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
    else
      Result := aClass.Create
  end
  else
    Result := nil;
end;

{ TmnXMLRttiHeaderFiler }

procedure TmnXMLRttiHeaderFiler.ReadClose(const Name: string);
begin
end;

procedure TmnXMLRttiHeaderFiler.ReadOpen(const Name, Attributes: string);
var
  aStrings: TStrings;
begin
  if Name <> 'rtti' then
    raise EmnXMLParserException.Create('Header tag <rtti> not defined');
  aStrings := CreateAttStrings(Attributes);
  try
    if DequoteStr(aStrings.Values['author']) <> cRttiAuthor then
      raise EmnXMLParserException.Create('Header author attribute must be MiniXML');
    if DequoteStr(aStrings.Values['version']) <> cRttiVersion then
      raise EmnXMLParserException.Create('Version not compatible with ' + cRttiVersion);
  finally
    aStrings.Free;
  end;
  (Owner as TmnXMLRttiReader).Stack.Push(TmnXMLRttiRootFiler.Create(Owner, Instance));
end;

procedure TmnXMLRttiHeaderFiler.ReadValue(const Text: string);
begin
  raise EmnXMLException.Create('Text junk not accepted');
end;

procedure TmnXMLRttiHeaderFiler.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
begin
end;

end.

