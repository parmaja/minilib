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

{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch ArrayOperators}
{$ModeSwitch typehelpers}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}
{$ENDIF}
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
  mnClasses, mnUtils, mnJSON, mnFields;

type
  { TSerializer }

  TSerializer = class abstract(TObject)
  public
    TabWidth: Integer;
    constructor Create;
    procedure Add(const S: string); overload; virtual; abstract;
    procedure Add(Level: Integer = 1; S: string = ''); overload;
    procedure NewLine; virtual; abstract;
  end;

  { TStringsSerializer }

  TStringsSerializer = class(TSerializer)
  private
    FStrings: TStrings;
    FLine: string;
  public
    constructor Create(Strings: TStrings);
    destructor Destroy; override;
    procedure Flush;
    procedure Add(const S: string); override;
    procedure NewLine; override;
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

  { TDON_Base }

  TDON_Base = class abstract(TmnObject)
  public
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); virtual;
  end;

  TDON_Value = class;

  { TDON_Pair }

  TDON_Pair = class(TDON_Base)
  private
    FName: string;
    FValue: TDON_Value;
    procedure SetName(const AValue: string);
    procedure SetValue(AValue: TDON_Value);
  public
    function ReleaseValue: TDON_Value;
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); virtual;
  published
    property Value: TDON_Value read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  { TDON_Root }

  TDON_Root = class(TDON_Pair)
  public
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); override;
  end;

  { TDON_Value }

  TDON_Value = class abstract(TmnCustomField)
  private
    FParent: TDON_Pair;
    function GetValues(Index: string): TDON_Value;
    procedure SetValues(Index: string; const Value: TDON_Value);
  protected
    function FindItem(Name: string): TDON_Value; virtual;
    function GetItem(Index: Integer): TDON_Value; virtual;
    function GetIsNull: Boolean; override;
  public
    constructor Create(AParent: TDON_Pair);
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); virtual;
    property Parent: TDON_Pair read FParent;
    property Values[Index: string]: TDON_Value read GetValues write SetValues; default;
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
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;

    function GetIsEmpty: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(AParent: TDON_Pair; const AText: string); overload;
  published
    property Value: string read FValue write FValue;
  end;

  { TDON_String_Value }

  TDON_String_Value = class(TDON_Custom_String_Value)
  public
    procedure Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer); override;
  end;

  { TDON_Identifier_Value }

  TDON_Identifier_Value = class(TDON_Custom_String_Value)
  private
  protected
    function GetIsNull: Boolean; override;
  public
    procedure Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer); override;
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
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;

    function GetIsEmpty: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Variant); override;
  public
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; const ANumber: Double); overload;
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
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;

    function GetIsEmpty: Boolean; override;

    procedure SetValue(const Value: Variant); override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
  public
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; AValue: Boolean); overload;
  published
    property Value: Boolean read FValue write FValue;
  end;

  { TDON_Pairs }

  TDON_Pairs = class(TmnObjectList<TDON_Pair>)
  public
    function Find(AName: string): TDON_Pair;
  end;

  { TDON_Object_Value }

  TDON_Object_Value = class(TDON_Value)
  private
    FPairs: TDON_Pairs;
  protected
    function FindItem(Name: string): TDON_Value; override;
    function GetItem(Index: Integer): TDON_Value; override;
    function GetAsString: string; override;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure AcquirePair(const AName: string; out AObject: TObject);
    procedure Add(Value: TDON_Pair); overload;
    procedure Serialize(Serializer: TSerializer; LastOne:Boolean; Level: Integer); override;
    property Pairs: TDON_Pairs read FPairs;
  published
  end;

  { TDON_List }

  TDON_List = class(TmnObjectList<TDON_Value>)
  public
  end;

  { TDON_Array_Value }

  TDON_Array_Value = class(TDON_Value)
  private
    FItems: TDON_List;
   protected
    function GetAsString: string; override;
   public
    procedure Created; override;
    destructor Destroy; override;
    procedure Add(Value: TDON_Value); overload;
    procedure Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer); override;
    property Items: TDON_List read FItems;

  published
  end;

function JsonParseStringPair(const S: string; Options: TJSONParseOptions = []): TDON_Pair;
function JsonParseStringValue(const S: string; Options: TJSONParseOptions = []): TDON_Value;

function JsonParseFilePair(const FileName: string; Options: TJSONParseOptions = []): TDON_Pair;
function JsonParseFileValue(const FileName: string; Options: TJSONParseOptions = []): TDON_Value;

procedure JsonAcquire(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; out AObject: TObject);
function donAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;

implementation

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

function JsonParseStringPair(const S: string; Options: TJSONParseOptions): TDON_Pair;
begin
  Result := TDON_Root.Create;
  JsonParseCallback(s, Result, JsonAcquire, Options);
end;

function JsonParseStringValue(const S: string; Options: TJSONParseOptions): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseStringPair(S, Options);
  Result := Pair.ReleaseValue;
end;

function JsonParseFilePair(const FileName: string; Options: TJSONParseOptions = []): TDON_Pair;
begin
  Result := JsonParseStringPair(LoadFileString(FileName), Options)
end;

function JsonParseFileValue(const FileName: string; Options: TJSONParseOptions = []): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseFilePair(FileName, Options);
  Result := Pair.ReleaseValue;
end;

{ TSerializer }

constructor TSerializer.Create;
begin
  inherited Create;
  TabWidth := 4;
end;

procedure TSerializer.Add(Level: Integer; S: string);
begin
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
  Flush;
  inherited;
end;

procedure TStringsSerializer.Flush;
begin
  if FLine <> '' then
    NewLine;
end;

procedure TStringsSerializer.Add(const S: string);
begin
  FLine := FLine + S;
end;

procedure TStringsSerializer.NewLine;
begin
  FStrings.Add(FLine);
  FLine := '';
end;

{ TDON_Root }

procedure TDON_Root.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  Value.Serialize(Serializer, LastOne, Level);
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

function TDON_Number_Value.GetAsInteger: Longint;
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

function TDON_Number_Value.GetIsEmpty: Boolean;
begin
  Result := FValue = 0;
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

procedure TDON_Number_Value.SetAsInteger(const Value: Longint);
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

procedure TDON_Number_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  Serializer.Add(FloatToStr(Value));
  inherited;
end;

constructor TDON_Number_Value.Create(AParent: TDON_Pair; const ANumber: Double);
begin
  inherited Create(AParent);
  FValue := ANumber;
end;

{ TDON_Value }

constructor TDON_Value.Create(AParent: TDON_Pair);
begin
  inherited Create;
  FParent := AParent;
end;

function TDON_Value.FindItem(Name: string): TDON_Value;
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

function TDON_Value.GetValues(Index: string): TDON_Value;
begin
  Result := FindItem(Index);
end;

procedure TDON_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  if Self = nil then
    Serializer.Add('null');
  if not LastOne then
    Serializer.Add(',');
  Serializer.NewLine;
end;

procedure TDON_Value.SetValues(Index: string; const Value: TDON_Value);
begin
  //TODO
end;

{ TDON_Base }

procedure TDON_Base.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin

end;

{ TDON_Array_Value }

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

function TDON_Array_Value.GetAsString: string;
begin
  Result := '{Array}';
end;

procedure TDON_Array_Value.Add(Value: TDON_Value);
begin
  Items.Add(Value);
end;

procedure TDON_Array_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
var
  Itm: TDON_Value;
begin
  Serializer.Add('[');
  Serializer.NewLine;
  for Itm in Items do
  begin
    Serializer.Add(Level + 1);
    Itm.Serialize(Serializer, itm = Items.Last , Level + 1);
  end;
  Serializer.Add(Level, ']');
  inherited;
end;

{ TDON_Object_Value }

procedure TDON_Object_Value.Created;
begin
  inherited;
  FPairs := TDON_Pairs.Create;
end;

destructor TDON_Object_Value.Destroy;
begin
  FreeAndNil(FPairs);
  inherited;
end;

function TDON_Object_Value.FindItem(Name: string): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := FPairs.Find(Name);
  if Pair = nil then
    Result := nil
  else
    Result := Pair.Value;
end;

function TDON_Object_Value.GetAsString: string;
begin
  Result := '{Object}';
end;

function TDON_Object_Value.GetItem(Index: Integer): TDON_Value;
begin
  Result := FPairs[Index].Value;
end;

procedure TDON_Object_Value.AcquirePair(const AName: string; out AObject: TObject);
begin
  AObject := TDON_Pair.Create;
  (AObject as TDON_Pair).Name := AName;
  Add((AObject as TDON_Pair));
end;

procedure TDON_Object_Value.Add(Value: TDON_Pair);
begin
  FPairs.Add(Value);
end;

procedure TDON_Object_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
var
  Itm: TDON_Pair;
begin
  Serializer.Add('{');
  Serializer.NewLine;
  for Itm in Pairs do
    Itm.Serialize(Serializer, itm = Pairs.Last , Level + 1);
  Serializer.Add(Level, '}');
  inherited;
end;

{ TDON_Pair }

procedure TDON_Pair.SetValue(AValue: TDON_Value);
begin
  if FValue <> AValue then
  begin
    if (AValue.Parent <> nil) and (AValue.Parent <> self) then
      raise Exception.Create('Value have parent we can`t move it to another parent');
    FreeAndNil(FValue);
    FValue := AValue;
    FValue.FParent := Self;
  end;
end;

function TDON_Pair.ReleaseValue: TDON_Value;
begin
  Result := FValue;
  Result.FParent := Self;
  FValue := nil;
end;

procedure TDON_Pair.SetName(const AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TDON_Pair.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  Serializer.Add(Level, QuoteStr(Name, '"') + ': ');
  if Value = nil then
    raise Exception.Create('Value is null for: ' + Name)
  else
    Value.Serialize(Serializer, LastOne, Level);
end;

function donAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;
var
  v: TDON_Value;
  procedure CreateValue;
  begin
    case AType of
      donNumber: v :=  TDON_Number_Value.Create(nil, StrToFloatDef(AValue, 0));
      donIdentifier: v :=  TDON_Identifier_Value.Create(nil, AValue);
      donBoolean: v :=  TDON_Boolean_Value.Create(nil, StrToBoolDef(AValue, False));
      donString: v :=  TDON_String_Value.Create(nil, AValue);
      donObject: v := TDON_Object_Value.Create(nil);
      donArray: v := TDON_Array_Value.Create(nil);
    end;
    Result := v;
  end;
begin
  Result := nil;
  if AParentObject = nil then
    raise Exception.Create('Can not set value to nil object');

  if (AParentObject is TDON_Pair) then
  begin
     if (AParentObject as TDON_Pair).Value <> nil then
      raise Exception.Create('Value is already set and it is not array: ' + AParentObject.ClassName);
    CreateValue;
    (AParentObject as TDON_Pair).Value  :=  v;
  end
  else if (AParentObject is TDON_Array_Value) then
  begin
    CreateValue;
    (AParentObject as TDON_Array_Value).Add(v);
  end
  else
    raise Exception.Create('Value can not be set to:' + AParentObject.ClassName);
end;

procedure JsonAcquire(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; out AObject: TObject);
begin
  case ValueType of
    aqPair: (AParentObject as TDON_Object_Value).AcquirePair(Value, AObject);
    aqObject: AObject := donAcquireValue(AParentObject, Value, donObject);
    aqArray: AObject := donAcquireValue(AParentObject, Value, donArray);
    aqString: AObject := donAcquireValue(AParentObject, Value, donString);
    aqNumber: AObject := donAcquireValue(AParentObject, Value, donNumber);
    aqBoolean: AObject := donAcquireValue(AParentObject, Value, donBoolean);
    aqIdentifier: AObject := donAcquireValue(AParentObject, Value, donIdentifier);
  end;
end;

{ TDON_Boolean_Value }

procedure TDON_Boolean_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  if Value then
    Serializer.Add('true')
  else
    Serializer.Add('false');
  inherited Serialize(Serializer, LastOne, Level);
end;

constructor TDON_Boolean_Value.Create(AParent: TDON_Pair; AValue: Boolean);
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

function TDON_Boolean_Value.GetAsInteger: Longint;
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

function TDON_Boolean_Value.GetIsEmpty: Boolean;
begin
  Result := not FValue;
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

procedure TDON_Boolean_Value.SetAsInteger(const Value: Longint);
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

constructor TDON_Custom_String_Value.Create(AParent: TDON_Pair; const AText: string);
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

function TDON_Custom_String_Value.GetAsInteger: Longint;
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

function TDON_Custom_String_Value.GetIsEmpty: Boolean;
begin
  Result := FValue = '';
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

procedure TDON_Custom_String_Value.SetAsInteger(const Value: Longint);
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

procedure TDON_Identifier_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  Serializer.Add(Value);
  inherited;
end;

{ TDON_String_Value }

procedure TDON_String_Value.Serialize(Serializer: TSerializer; LastOne: Boolean; Level: Integer);
begin
  Serializer.Add(QuoteStr(EscapeStringC(Value), '"'));
  inherited;
end;

{ TDON_Pairs }

function TDON_Pairs.Find(AName: string): TDON_Pair;
var
  i: integer;
begin
  Result := nil;
  if AName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, AName) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

initialization
end.
