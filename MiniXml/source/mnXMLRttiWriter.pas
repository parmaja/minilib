unit mnXMLRttiWriter;
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
  mnXMLRtti, mnXMLFPClasses;

type
  TmnXMLRttiWriter = class(TmnXMLRttiCustomWriter)
  private
    FWriteTypes: Boolean;
    FRoot: TComponent;
    FWriteRefrences: Boolean;
  protected
    procedure WriteProperty(Instance: TObject; PropInfo: PPropInfo);
    procedure WriteProperties(Name:string; Instance: TObject; WithInitTag: Boolean);
    procedure WriteComponent(Component: TComponent);
    procedure DoStart; override;
  public
    constructor Create; override;
    procedure WriteObject(Instance: TObject); override;
    procedure WriteRoot(Instance: TObject); override;
    property WriteTypes: Boolean read FWriteTypes write FWriteTypes default True;
    property WriteRefrences: Boolean read FWriteRefrences write FWriteRefrences default False;
  end;

implementation

uses
  mnXMLUtils;

type
  THackComponent = class(TComponent);

{ TmnXMLRttiWriter }

//WriteObject taked from Classes.TWriter.WriteProperties

constructor TmnXMLRttiWriter.Create;
begin
  inherited;
  FWriteTypes := True;
end;

procedure TmnXMLRttiWriter.WriteComponent(Component: TComponent);
begin
  WriteProperties('', Component, True);
end;

procedure TmnXMLRttiWriter.WriteRoot(Instance: TObject);
begin
  if not Active then
    Start;
  if Instance is TComponent then
    FRoot := Instance as TComponent;
  WriteOpenTag('rtti', 'version="' + cRttiVersion + '" author="' + cRttiAuthor + '"');
  WriteProperties('', Instance, True);
  WriteCloseTag('rtti');
  FRoot := nil;
end;

procedure TmnXMLRttiWriter.WriteProperties(Name:string; Instance: TObject; WithInitTag: Boolean);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  List: TList;
begin
  if Instance is TPersistent then //TObject not support RTTI :-(
    Count := GetTypeData(Instance.ClassInfo)^.PropCount
  else
    Count := 0;
  if Count > 0 then
  begin
    List := TList.Create;
    {$IFDEF FPC}
    PropList := nil;
    {$ENDIF}
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      for I := 0 to Count - 1 do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;
        if IsStoredProp(Instance, PropInfo) and (PropInfo^.GetProc <> nil) and ((PropInfo^.SetProc <> nil) or (PropInfo^.PropType^.Kind = tkClass)) then
          List.Add(PropInfo);
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;

    Count := List.Count;
    if Count > 0 then
    begin
      if WithInitTag then
      begin
        if (Instance is TComponent) and ((Instance as TComponent).Name <> '') then
          WriteOpenTag('Object', 'Type="' + Instance.ClassName + '" Name="' + (Instance as TComponent).Name + '"')
        else
          WriteOpenTag('Object', 'Type="' + Instance.ClassName + '"');
      end;
      for i := 0 to Count - 1 do
      begin
        WriteProperty(Instance, PPropInfo(List[i]));
      end;
    end;
    List.Free;
  end;

  PermanentRegister.WriteClassProperties(Name, Self, Instance);

  if (Instance is TComponent) then
  begin
    THackComponent(Instance).GetChildren(WriteComponent, FRoot);
  end;

  if Count > 0 then
    if WithInitTag then
      WriteCloseTag('Object');
end;

procedure TmnXMLRttiWriter.WriteProperty(Instance: TObject; PropInfo: PPropInfo);
var
  PropType: PTypeInfo;
  procedure WriteIntProp;
  var
    S: string;
    IntToIdent: TIntToIdent;
    Value: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    IntToIdent := FindIntToIdent(PropType);
    S := '';
    if not ((Assigned(IntToIdent) and IntToIdent(Value, S))) then
      S := IntToStr(Value);
    WriteStopTag;
    WriteText(S);
  end;

  procedure WriteEnumeration;
  var
    Value: Longint;
    S: string;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    S := GetEnumName(PropType, Value);
    WriteStopTag;
    WriteText(S);
  end;

  procedure WriteSet;
  var
    S: string;
  begin
    S := GetSetProp(Instance, PropInfo, False);
    WriteStopTag;
    WriteText(S);
  end;

  procedure WriteChar;
  var
    Value: Char;
  begin
    Value := Char(GetOrdProp(Instance, PropInfo));
    WriteStopTag;
    WriteText(Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    WriteStopTag;
    WriteText(IntToStr(Value));
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    WriteStopTag;
    WriteText(FloatToStr(Value));
  end;

  procedure WriteStrProp;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, PropInfo);
    WriteStopTag;
    WriteText(Value);
  end;

  procedure WriteVariantProp;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    WriteStopTag;
    WriteText(Value);
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value is TComponent) and not (csSubComponent in (Value as TComponent).ComponentStyle) then
    begin
      WriteStopTag;
      if WriteRefrences then
      begin
        WriteText((Value as TComponent).Name);
      end;
    end
    else
    begin
      if (Value is TComponent) and ((Value as TComponent).Name <> '') then
        WriteAttributes('ID="' + (Value as TComponent).Name + '"');
      WriteStopTag;
      WriteProperties(PropInfo^.Name, Value, False);
    end;
  end;

  procedure WriteInterfaceProp;
  var
    Value: Pointer;
  begin
    Value := Pointer(Longword(GetOrdProp(Instance, PropInfo)));
    WriteStopTag;
    PermanentRegister.WriteInterface(PropInfo^.Name, Self, Value);
  end;

begin
  if not IsDefaultValue(Instance, PropInfo) then
  begin
    PropType := GetPropType(PropInfo);
    if PropType^.Kind in [tkInteger, tkChar, tkSet, tkEnumeration, tkInt64, tkFloat, tkString, tkLString, tkWString, tkVariant, tkClass, tkInterface] then
    begin
      WriteStartTag(PropInfo^.Name);
      if FWriteTypes then
        WriteAttributes('Type="' + PropType.Name + '"');
      case PropType^.Kind of
        tkInteger:
          WriteIntProp;
        tkChar:
          WriteChar;
        tkSet:
          WriteSet;
        tkEnumeration:
          WriteEnumeration;
        tkInt64:
          WriteInt64Prop;
        tkFloat:
          WriteFloatProp;
        tkString, tkLString, tkWString:
          WriteStrProp;
        tkVariant:
          WriteVariantProp;
        tkClass:
          WriteObjectProp;
        tkMethod: ; //not yet
        tkInterface:
          WriteInterfaceProp;
      end;
      if TagStarted then
        WriteStopTag;
      WriteCloseTag(PropInfo^.Name);
    end;
  end;
end;

procedure TmnXMLRttiWriter.WriteObject(Instance: TObject);
begin
  WriteProperties('', Instance, True);
end;

procedure TmnXMLRttiWriter.DoStart;
begin
  inherited;
end;

end.

