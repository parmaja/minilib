unit mnDON;
{ **
  *  Data Object Notication/Nodes Tree
  *
  *  @license   The MIT License (MIT)
  *
  *  @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
  *            See the file COPYING.MLGPL, included in this distribution,
  *  @author    Zaher Dirkey <zaher, zaherdirkey>
  *  @author    Belal AlHamad
  *
  *}

{$A8,C+,O+,W-,Z1}
{$STRINGCHECKS OFF}
{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch ArrayOperators}
{$ModeSwitch typehelpers}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}
{$else}
{$endif}
{$M+}{$H+}
{$ifdef mswindows}
{$define windows}
{$endif}

{$STRINGCHECKS OFF}
{$POINTERMATH ON}

interface

uses
{$IFDEF windows}Windows, {$ENDIF}
  Classes, SysUtils, StrUtils, DateUtils, Types, Character,
  mnClasses, mnUtils, mnJSON, mnFields, mnStreams;

type
  TSerializeGernerator = class;
  TSerializeGerneratorClass = class of TSerializeGernerator;

  TSerializerOption = (
    sroCompact,
    sroSmartName  //* export names without qoutaions if not have space
  );
  TSerializerOptions = set of TSerializerOption;

  { TSerializer }

  TSerializer = class abstract(TObject)
  public
    TabWidth: Integer;
    Options: TSerializerOptions;
    constructor Create;
    procedure Serialize(AGerneratorClass: TSerializeGerneratorClass; AObject: TObject);
    procedure Add(const S: string); overload; virtual; abstract;
    procedure Add(Level: Integer = 1; S: string = ''); overload;
    procedure Add(const S: string; LastOne:Boolean; Separator: string); overload;
    procedure NewLine; virtual;
    procedure Flush; virtual;
  end;

  { TStringsSerializer }

  TStringsSerializer = class(TSerializer)
  private
    FStrings: TStrings;
    FLine: string;
  public
    constructor Create(Strings: TStrings);
    destructor Destroy; override;
    procedure Flush; override;
    procedure Add(const S: string); override;
    procedure NewLine; override;
  end;

  { TStringsSerializer }

  TStreamSerializer = class(TSerializer)
  private
    FStream: TStream;
    FIsUTF8: Boolean;
  public
    constructor Create(vStream: TStream; vIsUTF8: Boolean);
    destructor Destroy; override;
    procedure Add(const S: string); override;
  end;

  //-------

  TSerializeGernerator = class abstract(TObject)
  public
    Serializer: TSerializer;
    procedure Generate(AClass: TClass; AObject: TObject; LastOne: Boolean; Level: Integer); overload; virtual; abstract;
    procedure Generate(AObject: TObject; LastOne: Boolean; Level: Integer); overload;
    constructor Create(ASerializer: TSerializer);
  end;

  TJsonSerializeGernerator = class(TSerializeGernerator)
  public
    procedure Generate(AClass: TClass; AObject: TObject; LastOne: Boolean; Level: Integer); override;
  end;

//-----------------------------------------------------------------------------
//* DON objects

type
  { TDONType }

  TDONType = (
    donObject,
    donArray,
    donString,
    donNumber,
    donBoolean,
    donIdentifier
  );

  TDON_Value = class;
  TDON_Parent = class;
  TDON_Object_Value = class;

  { TDON_Base }

  TDON_Base = class abstract(TmnObject)
  public
  end;

  { TDON_Value }

  TDON_Value = class abstract(TmnCustomField)
  private
    FParent: TDON_Parent;
    function GetValues(const Index: string): TDON_Value;
    procedure SetValues(const Index: string; const Value: TDON_Value);
  protected
    function FindItem(const Name: string): TDON_Value; virtual;
    function GetItem(Index: Integer): TDON_Value; virtual;
    function GetIsNull: Boolean; override;
  public
    constructor Create(AParent: TDON_Parent);

    function ByPath(Path: TStrings): TDON_Value; overload;
    function ByPath(const Path: string; Delimiter: Char = '.'): TDON_Value; overload;
    function ByPath(const Path: TArray<string>): TDON_Value; overload;
    function ByIndex(Index: Integer): TDON_Value;

    function AddObject: TDON_Value; overload;
    function AddObject(const Name: String): TDON_Value; overload;
    function AddArray: TDON_Value; overload;
    function AddArray(const Name: String): TDON_Value; overload;
    function AddPair(const Name: String; const Value: string): TDON_Value; overload; //Add Pair with string value with this name

    property Parent: TDON_Parent read FParent;
    property Values[const Index: string]: TDON_Value read GetValues write SetValues; default;
    {$ifndef FPC}
    property Values[const Index: TArray<string>]: TDON_Value read ByPath; default;
    property Values[Index: Integer]: TDON_Value read ByIndex; default;
    {$endif}

    property AsUtf8String;
  published
    //property IsBlob;
    //property BlobType;
    property IsNull;
    property Value;
    property AsVariant;
    property AsString;
    {$ifndef NEXTGEN}
    property AsAnsiString;
    {$endif}
    property AsTrimString;
    property AsNullString;
    property AsInteger;
    property AsInt64;
    property AsForeign;
    property AsBoolean;
    property AsCurrency;
    property AsDate;
    property AsTime;
    property AsDateTime;
    property AsText; //binary text blob convert to hex
    property AsHex;
    property AsDouble;
    property AsBytes;
  end;

  TDON_ValueClass = class of TDON_Value;

  { TDON_Custom_String_Value }

  TDON_Custom_String_Value = class abstract(TDON_Value)
  private
    FValue: string;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(AParent: TDON_Parent; const AText: string); overload;
  published
    property Value: string read FValue write FValue;
  end;

  { TDON_String_Value }

  TDON_String_Value = class(TDON_Custom_String_Value)
  public
  end;

  { TDON_Identifier_Value }

  TDON_Identifier_Value = class(TDON_Custom_String_Value)
  private
  protected
    function GetIsNull: Boolean; override;
  public
  published
  end;

  { TDON_Number_Value }

  TDON_Number_Value = class(TDON_Value)
  private
    FValue: Double;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(AParent: TDON_Parent; const ANumber: Double); overload;
  published
    property Value: Double read FValue write FValue;
  end;

  { TDON_Boolean_Value }

  TDON_Boolean_Value = class(TDON_Value)
  private
    FValue: Boolean;
  protected
    function GetValue: Variant; override;

    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;

    procedure SetValue(const Value: Variant); override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
  public
    constructor Create(AParent: TDON_Parent; AValue: Boolean); overload;
  published
    property Value: Boolean read FValue write FValue;
  end;

  TDON_Parent = class abstract(TDON_Value)
  end;

  { TDON_Pair }

  TDON_Pair = class(TDON_Parent)
  private
    FParent: TDON_Object_Value;
    FName: string;
    FValue: TDON_Value;
    procedure SetPairValue(AValue: TDON_Value);
  protected
    function FindItem(const Name: string): TDON_Value; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create(AParent: TDON_Object_Value);
    destructor Destroy; override;
    function ReleaseValue: TDON_Value;
  published
    property Value: TDON_Value read FValue write SetPairValue;
    property Name: string read FName write FName;
  end;

  { TDON_Root }

  TDON_Root = class(TDON_Pair)
  public
  end;

  { TDON_Pairs }

  TDON_Pairs = class(TmnObjectList<TDON_Pair>)
  public
  end;

  { TDON_Object_Value }

  TDON_Object_Value = class(TDON_Parent)
  private
    FPairs: TDON_Pairs;
  protected
    function FindItem(const Name: string): TDON_Value; override;
    function GetItem(Index: Integer): TDON_Value; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    function CreatePair(const PairName: string): TDON_Pair;
    procedure AcquirePair(const AName: string; out AObject: TObject);
    procedure AddPair(Value: TDON_Pair); overload;
    function AddPair(const Name: String; const Value: string): TDON_Value; overload;
    property Pairs: TDON_Pairs read FPairs;
  published
  end;

  { TDON_List }

  TDON_List = class(TmnObjectList<TDON_Value>)
  public
  end;

  { TDON_Array_Value }

  TDON_Array_Value = class(TDON_Parent)
  private
    FItems: TDON_List;
    function GetCount: Integer;
  protected
    function GetAsString: string; override;
    function FindItem(const Name: string): TDON_Value; override;
    function GetItem(Index: Integer): TDON_Value; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    function Add(Value: TDON_Value): TDON_Value; overload;
    function Add(const Value: String): TDON_Value; overload;
    procedure Add(const Values: array of const); overload;

    property Items: TDON_List read FItems;
    property Count: Integer read GetCount;
  published
  end;

function JsonParseStringPair(const S: utf8string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
//* {"value": "test1"}
function JsonParseStringValue(const S: utf8string; out Error: string; Options: TJSONParseOptions = []): TDON_Value;

function JsonParseFilePair(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
function JsonParseFileValue(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Value;

procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList);

//Used in JSON parser
procedure JsonParseAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; out AObject: TObject);

function JsonAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;

implementation

function JsonAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;

  procedure CreateValue(VT: TDONType; const s: string; out res: TObject); inline;
  begin
    res := nil;
    case VT of
      //donNumber: res := TDON_Number_Value.Create(nil, StrToFloatDef(s, 0));
      donNumber: res := TDON_String_Value.Create(nil, s);
      donIdentifier: res := TDON_Identifier_Value.Create(nil, s);
      donBoolean: res := TDON_Boolean_Value.Create(nil, StrToBoolDef(s, False));
      donString: res := TDON_String_Value.Create(nil, s);
      donObject: res := TDON_Object_Value.Create(nil);
      donArray: res := TDON_Array_Value.Create(nil);
    end;
  end;

begin
  Result := nil;
  if AParentObject = nil then
    raise Exception.Create('Can not set value to nil object');

  if (AParentObject is TDON_Pair) then
  begin
     if (AParentObject as TDON_Pair).Value <> nil then
      raise Exception.Create('Value is already set and it is not array: ' + AParentObject.ClassName);
    CreateValue(AType, AValue, Result);
    (AParentObject as TDON_Pair).Value  :=  TDON_Value(Result);
  end
  {else if (AParentObject is TDON_Object_Value) then
  begin
    Result := (AParentObject as TDON_Object_Value).CreatePair(AValue);
  end}
  else if (AParentObject is TDON_Array_Value) then
  begin
    CreateValue(AType, AValue, Result);
    (AParentObject as TDON_Array_Value).Add(TDON_Value(Result));
  end
  else
    raise Exception.Create('Value can not be set to:' + AParentObject.ClassName);
end;

procedure JsonParseAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; out AObject: TObject);
begin
  case ValueType of
    aqPair: (AParentObject as TDON_Object_Value).AcquirePair(Value, AObject);
    aqObject: AObject := JsonAcquireValue(AParentObject, Value, donObject);
    aqArray: AObject := JsonAcquireValue(AParentObject, Value, donArray);
    aqString: AObject := JsonAcquireValue(AParentObject, Value, donString);
    aqIdentifier: AObject := JsonAcquireValue(AParentObject, Value, donIdentifier);
    aqNumber: AObject := JsonAcquireValue(AParentObject, Value, donNumber);
    aqBoolean: AObject := JsonAcquireValue(AParentObject, Value, donBoolean);
  end;
end;

function JsonParseStringPair(const S: utf8string; out Error: string; Options: TJSONParseOptions): TDON_Pair;
begin
  Result := TDON_Root.Create(nil);
  try
    JsonParseCallback(s, Error, Result, JsonParseAcquireCallback, Options);
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end
end;

function JsonParseStringValue(const S: utf8string; out Error: string; Options: TJSONParseOptions): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseStringPair(S, Error, Options);
  try
    if Pair<>nil then
      Result := Pair.ReleaseValue
    else
      Result := nil;
  finally
    Pair.Free;
  end;
end;

function JsonParseFilePair(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
begin
  Result := JsonParseStringPair(Utf8Encode(LoadFileString(FileName)), Error, Options)
end;

function JsonParseFileValue(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseFilePair(FileName, Error, Options);
  Result := Pair.ReleaseValue;
end;

procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList);
var
  Serializer: TStringsSerializer;
begin
  Serializer := TStringsSerializer.Create(Strings);
  try
    Serializer.Serialize(TJsonSerializeGernerator, Pair);
    //JSon4.Serialize(Writer, True, 0);
  finally
    Serializer.Free;
  end;
end;

{ TSerializer }

procedure TSerializer.Add(const S: string; LastOne:Boolean; Separator: string);
begin
  Add(S);
  if not LastOne then
    Add(Separator);
end;

constructor TSerializer.Create;
begin
  inherited Create;
  TabWidth := 4;
end;

procedure TSerializer.Flush;
begin

end;

procedure TSerializer.NewLine;
begin
  if not (sroCompact in Options) then
    Add(sLineBreak);
end;

procedure TSerializer.Serialize(AGerneratorClass: TSerializeGerneratorClass; AObject: TObject);
var
  Gernerator: TSerializeGernerator;
begin
  Gernerator := AGerneratorClass.Create(Self);
  Gernerator.Generate(AObject, True, 0);
  Flush;
end;

procedure TSerializer.Add(Level: Integer; S: string);
begin
  if (sroCompact in Options) then
    Add(S)
  else
    Add(StringOfChar(' ', Level * TabWidth) + S);
end;

{ TStringsSerializer }

constructor TStringsSerializer.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

destructor TStringsSerializer.Destroy;
begin
  inherited;
end;

procedure TStringsSerializer.Add(const S: string);
begin
  FLine := FLine + S;
end;

procedure TStringsSerializer.Flush;
begin
  if FLine <> '' then
  begin
    FStrings.Add(FLine);
    FLine := '';
  end;
end;

procedure TStringsSerializer.NewLine;
begin
  if not (sroCompact in Options) then
  begin
    FStrings.Add(FLine);
    FLine := '';
  end;
end;

{ TDON_Number_Value }

function TDON_Number_Value.GetAsBoolean: Boolean;
begin
  Result := AsDouble <> 0;
end;

function TDON_Number_Value.GetAsCurrency: Currency;
begin
  Result := AsDouble;
end;

function TDON_Number_Value.GetAsDateTime: TDateTime;
begin
  Result := AsDouble;
end;

function TDON_Number_Value.GetAsDouble: Double;
begin
  Result := FValue;
end;

function TDON_Number_Value.GetAsInteger: Integer;
begin
  Result := Trunc(FValue);
end;

function TDON_Number_Value.GetAsString: string;
begin
  Result := FloatToStr(FValue);
end;

function TDON_Number_Value.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDON_Number_Value.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TDON_Number_Value.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TDON_Number_Value.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TDON_Number_Value.SetAsDouble(const Value: Double);
begin
  FValue := Value;

end;

procedure TDON_Number_Value.SetAsInteger(const Value: Integer);
begin
  FValue := Trunc(Value);
end;

procedure TDON_Number_Value.SetAsString(const Value: string);
begin
  FValue := StrToFloatDef(Value, 0);
end;

procedure TDON_Number_Value.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TDON_Number_Value.Create(AParent: TDON_Parent; const ANumber: Double);
begin
  inherited Create(AParent);
  FValue := ANumber;
end;

{ TDON_Value }

function TDON_Value.ByPath(Path: TStrings): TDON_Value;
var
  i: Integer;
begin
  Result := self;
  i := 0;
  while (Result <> nil) and (i < Path.Count) do
  begin
    Result := Result.FindItem(Path[i]);
    Inc(i);
  end;
end;

function TDON_Value.ByPath(const Path: string; Delimiter: Char): TDON_Value;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    //StrToStringsEx(Path, sl, [Delimiter]);//not yet we need escapes too
    sl.Delimiter := Delimiter;
    sl.DelimitedText := Path;
    Result := ByPath(sl);
  finally
    sl.Free;
  end;
end;

function TDON_Value.AddArray(const Name: String): TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object_Value) then
  begin
    aPair := (Self as TDON_Object_Value).CreatePair(Name);
    aPair.Value := TDON_Array_Value.Create(aPair);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('You cant add object here');
end;

function TDON_Value.AddObject: TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object_Value) then
  begin
    aPair := (Self as TDON_Object_Value).CreatePair('');
    aPair.Value := TDON_Object_Value.Create(aPair);
    Result := aPair.Value;
  end
  else if (Self is TDON_Array_Value) then
  begin
    Result := (Self as TDON_Array_Value).Add(TDON_Object_Value.Create(Self as TDON_Array_Value));
  end
  else if (FParent is TDON_Pair) then
  begin
    Result := TDON_Object_Value.Create(Self as TDON_Pair);
    (Self as TDON_Pair).Value := Result;
  end
  else
    raise Exception.Create('Can not add object here');
end;

function TDON_Value.AddPair(const Name, Value: string): TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object_Value) then
  begin
    aPair := (Self as TDON_Object_Value).CreatePair(Name);
    aPair.Value := TDON_String_Value.Create(aPair, Value);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('Not an object');
end;

function TDON_Value.AddObject(const Name: String): TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object_Value) then
  begin
    aPair := (Self as TDON_Object_Value).CreatePair(Name);
    aPair.Value := TDON_Object_Value.Create(aPair);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('You can add object with name here');
end;

function TDON_Value.AddArray: TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object_Value) then
  begin
    aPair := (Self as TDON_Object_Value).CreatePair('');
    aPair.Value := TDON_Array_Value.Create(aPair);
    Result := aPair.Value;
  end
  else if (Self is TDON_Array_Value) then
  begin
    Result := (Self as TDON_Array_Value).Add(TDON_Array_Value.Create(Self as TDON_Array_Value));
  end
  else if (FParent is TDON_Pair) then
  begin
    Result := TDON_Array_Value.Create(Self as TDON_Pair);
    (Self as TDON_Pair).Value := Result;
  end
  else
    raise Exception.Create('Can not add array here');
end;

function TDON_Value.ByIndex(Index: Integer): TDON_Value;
begin
  Result := GetItem(Index);
end;

function TDON_Value.ByPath(const Path: TArray<string>): TDON_Value;
var
  i: Integer;
begin
  Result := Self;
  i := 0;
  while (Result <> nil) and (i < Length(Path)) do
  begin
    Result := Result.FindItem(Path[i]);
    Inc(i);
  end;
end;

constructor TDON_Value.Create(AParent: TDON_Parent);
begin
  inherited Create;
  FParent := AParent;
end;

function TDON_Value.FindItem(const Name: string): TDON_Value;
begin
  if Name = '' then
    Result := Self
  else
    Result := nil;
end;

function TDON_Value.GetIsNull: Boolean;
begin
  Result := False;
end;

function TDON_Value.GetItem(Index: Integer): TDON_Value;
begin
  if Index = -1 then
    Result := Self
  else
    Result := nil;
end;

function TDON_Value.GetValues(const Index: string): TDON_Value;
begin
  if Self = nil then
    Result := nil
  else
    Result := FindItem(Index);
end;

procedure TDON_Value.SetValues(const Index: string; const Value: TDON_Value);
begin
  //TODO
end;

{ TDON_Array_Value }

procedure TDON_Array_Value.Add(const Values: array of const);
var
  i : Integer;
begin
  if High(Values) > 0 then
  begin
    for i := 0 to High(Values) do
    begin
      case Values[i].vType of
        vtBoolean:
          Items.Add(TDON_Boolean_Value.Create(Self, Values[i].VBoolean));
        vtChar:
          Items.Add(TDON_String_Value.Create(Self, String(Values[i].VChar)));
        vtString:
          Items.Add(TDON_String_Value.Create(Self, String(Values[i].VString^)));
        vtInteger:
          Items.Add(TDON_Number_Value.Create(Self, Values[i].VInteger));
        vtExtended:
          Items.Add(TDON_Number_Value.Create(Self, Values[i].VExtended^));
      end;
    end;
  end;
end;

function TDON_Array_Value.Add(const Value: String): TDON_Value;
begin
  Result := TDON_String_Value.Create(Self, Value);
  Add(Result);
end;

procedure TDON_Array_Value.Created;
begin
  inherited;
  FItems := TDON_List.Create;
end;

destructor TDON_Array_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TDON_Array_Value.FindItem(const Name: string): TDON_Value;
begin
  Result := nil;
end;

function TDON_Array_Value.GetAsString: string;
begin
  Result := '{Array}';
end;

function TDON_Array_Value.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TDON_Array_Value.GetItem(Index: Integer): TDON_Value;
begin
  if Index < FItems.Count then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TDON_Array_Value.GetValue: Variant;
begin
  Result := AsString;
end;

function TDON_Array_Value.Add(Value: TDON_Value): TDON_Value;
begin
  Items.Add(Value);
  Result := Value;
end;

procedure TDON_Array_Value.SetValue(const AValue: Variant);
begin
  AsString := AValue;
end;

{ TDON_Object_Value }

function TDON_Object_Value.AddPair(const Name, Value: string): TDON_Value;
var
  aPair: TDON_Pair;
begin
  aPair := CreatePair(Name);
  aPair.Value := TDON_String_Value.Create(aPair, Value);
  Result := aPair.Value;
end;

procedure TDON_Object_Value.Created;
begin
  inherited;
  FPairs := TDON_Pairs.Create;
end;

function TDON_Object_Value.CreatePair(const PairName: string): TDON_Pair;
begin
  Result := TDON_Pair.Create(Self);
  Result.FName := PairName;
  AddPair(Result);
end;

destructor TDON_Object_Value.Destroy;
begin
  FreeAndNil(FPairs);
  inherited;
end;

function TDON_Object_Value.FindItem(const Name: string): TDON_Value;
var
  i: Integer;
begin
  //for speed do not put it in FPairs.Find(Name)
  for i := 0 to FPairs.Count-1 do
    if FPairs[i].Name = Name then
    begin
      Exit(FPairs[i].Value);
    end;
  Result := nil
end;

function TDON_Object_Value.GetAsString: string;
begin
  Result := '{Object}';
end;

function TDON_Object_Value.GetItem(Index: Integer): TDON_Value;
begin
  Result := FPairs[Index].Value;
end;

function TDON_Object_Value.GetValue: Variant;
begin
  Result := AsString;
end;

procedure TDON_Object_Value.AcquirePair(const AName: string; out AObject: TObject);
begin
  AObject := TDON_Pair.Create(Self);
  (AObject as TDON_Pair).FName := AName;
  AddPair((AObject as TDON_Pair));
end;

procedure TDON_Object_Value.AddPair(Value: TDON_Pair);
begin
  FPairs.Add(Value);
end;

procedure TDON_Object_Value.SetValue(const AValue: Variant);
begin
  AsString := aValue;
end;

{ TDON_Pair }

procedure TDON_Pair.SetPairValue(AValue: TDON_Value);
begin
  if FValue <> AValue then
  begin
    if (AValue.Parent <> nil) and (AValue.Parent <> self) then
      raise Exception.Create('Value have parent we can`t move it to another parent');
    FreeAndNil(FValue);
    FValue := AValue;
    //FValue.FParent := Self;
  end;
end;

constructor TDON_Pair.Create(AParent: TDON_Object_Value);
begin
  //where is inherited zaher :)
  FParent := AParent;
end;

destructor TDON_Pair.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

function TDON_Pair.FindItem(const Name: string): TDON_Value;
begin
  if (Self = nil) or (Value = nil) then
    Result := nil
  else
    Result := Value.FindItem(Name);
end;

function TDON_Pair.GetAsString: string;
begin
  Result := '{Pair}';
end;

function TDON_Pair.GetValue: Variant;
begin
  Result := AsString;
end;

function TDON_Pair.ReleaseValue: TDON_Value;
begin
  if FValue<>nil then
  begin
    Result := FValue;
    Result.FParent := Self;
    FValue := nil;
  end
  else
    Result := nil;
end;

procedure TDON_Pair.SetValue(const AValue: Variant);
begin
  AsString := AValue;
end;

{ TDON_Boolean_Value }

constructor TDON_Boolean_Value.Create(AParent: TDON_Parent; AValue: Boolean);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

function TDON_Boolean_Value.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TDON_Boolean_Value.GetAsCurrency: Currency;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean_Value.GetAsDateTime: TDateTime;
begin
  Result := 0;
end;

function TDON_Boolean_Value.GetAsDouble: Double;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean_Value.GetAsInteger: Integer;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean_Value.GetAsString: string;
begin
  Result := BoolToStr(FValue);
end;

function TDON_Boolean_Value.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDON_Boolean_Value.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TDON_Boolean_Value.SetAsCurrency(const Value: Currency);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean_Value.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value <> 0;

end;

procedure TDON_Boolean_Value.SetAsDouble(const Value: Double);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean_Value.SetAsInteger(const Value: Integer);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean_Value.SetAsString(const Value: string);
begin
  FValue := StrToBoolDef(Value, False);
end;

procedure TDON_Boolean_Value.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TDON_Custom_String_Value }

constructor TDON_Custom_String_Value.Create(AParent: TDON_Parent; const AText: string);
begin
  inherited Create(AParent);
  FValue := AText;
end;

function TDON_Custom_String_Value.GetAsBoolean: Boolean;
begin
  Result := StrToBoolDef(AsString, False);
end;

function TDON_Custom_String_Value.GetAsCurrency: Currency;
begin
  Result := StrToCurrDef(AsString, 0);
end;

function TDON_Custom_String_Value.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(AsString, 0);
end;

function TDON_Custom_String_Value.GetAsDouble: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TDON_Custom_String_Value.GetAsInteger: Integer;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TDON_Custom_String_Value.GetAsString: string;
begin
  Result := FValue;
end;

function TDON_Custom_String_Value.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDON_Custom_String_Value.SetAsBoolean(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TDON_Custom_String_Value.SetAsCurrency(const Value: Currency);
begin
  FValue := CurrToStr(Value);
end;

procedure TDON_Custom_String_Value.SetAsDateTime(const Value: TDateTime);
begin
  FValue := DateTimeToStr(Value);
end;

procedure TDON_Custom_String_Value.SetAsDouble(const Value: Double);
begin
  FValue := FloatToStr(Value);
end;

procedure TDON_Custom_String_Value.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);

end;

procedure TDON_Custom_String_Value.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TDON_Custom_String_Value.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TDON_Identifier_Value}

function TDON_Identifier_Value.GetIsNull: Boolean;
begin
  Result := SameText(Value, 'null');
end;

{ TSerializeGernerator }

constructor TSerializeGernerator.Create(ASerializer: TSerializer);
begin
  inherited Create;
  Serializer := ASerializer;
end;

procedure TSerializeGernerator.Generate(AObject: TObject; LastOne: Boolean; Level: Integer);
begin
  Generate(AObject.ClassType, AObject, LastOne, Level);
end;

{ TJsonSerializeGernerator }

procedure TJsonSerializeGernerator.Generate(AClass: TClass; AObject: TObject; LastOne: Boolean; Level: Integer);
var
  p: TDON_Pair;
  v: TDON_Value;

  function GetName(const AName: string): string;
  begin
    if (sroSmartName in Serializer.Options) and (Pos(' ', AName) <= 0) then
      Result := AName
    else
      Result := QuoteStr(AName, '"');
    if (sroCompact in Serializer.Options) then //need fix asp
      Result := Result + ':'
    else
      Result := Result + ': ';
  end;

begin
  if AClass = TDON_Pair then
  begin
    Serializer.Add(Level, GetName((AObject as TDON_Pair).Name));

    if (AObject as TDON_Pair).Value = nil then
    begin
      Serializer.Add('null', LastOne, ',');
      Serializer.NewLine;
    end
    else
      Generate((AObject as TDON_Pair).Value, LastOne, Level);
  end
  else if AClass = TDON_Object_Value then
  begin
    with AObject as TDON_Object_Value do
    begin
      if (Pairs.Count > 0) then
      begin
        Serializer.Add('{');
        Serializer.NewLine;
        for p in Pairs do
          Generate(p, p = Pairs.Last , Level + 1);
        Serializer.Add(Level, '}');
      end
      else
        Serializer.Add('{}');
      if not LastOne then
        Serializer.Add(',');
      Serializer.NewLine;
    end;
  end
  else if AClass = TDON_Array_Value then
  begin
    with AObject as TDON_Array_Value do
    begin
      if Items.Count>0 then
      begin
        Serializer.Add('[');
        Serializer.NewLine;
        for v in Items do
        begin
          Serializer.Add(Level + 1);
          Generate(v, v = Items.Last , Level + 1);
        end;
        Serializer.Add(Level, ']');
      end
      else
        Serializer.Add('[]');
    end;
    if not LastOne then
      Serializer.Add(',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Value then
  begin
    if AObject = nil then
      Serializer.Add('null');
    if not LastOne then
      Serializer.Add(',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Root then
    Generate((AObject as TDON_Root).Value, LastOne, Level)
  else if AClass = TDON_String_Value then
  begin
    Serializer.Add(QuoteStr(EscapeStringC((AObject as TDON_String_Value).Value), '"'), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Identifier_Value then
  begin
    Serializer.Add((AObject as TDON_Identifier_Value).Value, LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Number_Value then
  begin
    Serializer.Add(FloatToStr((AObject as TDON_Number_Value).Value), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Boolean_Value then
  begin
    Serializer.Add(BoolToStr((AObject as TDON_Boolean_Value).Value, True), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass.ClassParent <> nil then //if we cant find it we take parent class
    Generate(AClass.ClassParent, AObject, LastOne, Level);
end;

{ TStreamSerializer }

procedure TStreamSerializer.Add(const S: string);
begin
  inherited;
  if FIsUTF8 then
  begin
    FStream.WriteUTF8String(UTF8Encode(s));
  end
  else
  begin
    FStream.WriteString(s);
  end;
end;

constructor TStreamSerializer.Create(vStream: TStream; vIsUTF8: Boolean);
begin
  inherited Create;
  FStream := vStream;
end;

destructor TStreamSerializer.Destroy;
begin
  inherited;
end;


initialization
end.
