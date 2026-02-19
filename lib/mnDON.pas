unit mnDON;
{ **
  *  Data Object Notication/Nodes Tree
  *
  *  @license   The MIT License (MIT)
  *
  *  @author    Zaher Dirkey <zaher, zaherdirkey>
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
    sroModern  //* export names without qoutaions if not have space
  );
  TSerializerOptions = set of TSerializerOption;

  { TSerializer }

  TSerializer = class abstract(TObject)
  public
    TabWidth: Integer;
    LineTerminator: string;
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
    procedure Flush; override;
    procedure Add(const S: string); override;
    procedure NewLine; override;
  end;

  { TConsoleSerializer }

  TConsoleSerializer = class(TSerializer)
  private
    FLine: string;
  public
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
//* DON objects (Data object notations)
//-----------------------------------------------------------------------------

type

  TDON_Element = class;
  TDON_Parent = class;
  TDON_Object_Value = class;

  { TDON_Element }

  TDON_Element = class abstract(TmnCustomField)
  private
    FParent: TDON_Parent;
    function GetValues(const Index: string): TDON_Element;
    procedure SetValues(const Index: string; const Value: TDON_Element);
  protected
    function FindItem(const Name: string): TDON_Element; virtual;
    function GetItem(Index: Integer): TDON_Element; virtual;
    function GetIsNull: Boolean; override;
  public
    constructor Create(AParent: TDON_Parent);

    function ByPath(Path: TStrings): TDON_Element; overload;
    function ByPath(const Path: string; Delimiter: Char = '.'): TDON_Element; overload;
    function ByPath(const Path: TArray<string>): TDON_Element; overload;
    function ByIndex(Index: Integer): TDON_Element;

    function AddObject: TDON_Element; overload;
    function AddObject(const Name: String): TDON_Element; overload;
    function AddArray: TDON_Element; overload;
    function AddArray(const Name: String): TDON_Element; overload;
    function AddPair(const Name: String; const Value: string): TDON_Element; overload; //Add Pair with string value with this name

    property Parent: TDON_Parent read FParent;
    property Values[const Index: string]: TDON_Element read GetValues write SetValues; default;
    {$ifndef FPC}
    property Values[const Index: TArray<string>]: TDON_Element read ByPath; default;
    property Values[Index: Integer]: TDON_Element read ByIndex; default;
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

  TDON_ElementClass = class of TDON_Element;

  { TDON_Custom_String_Value }

  TDON_Custom_String_Value = class abstract(TDON_Element)
  private
    FValue: string;
    FStringOptions: TmnJsonTypeOptions;
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
    constructor Create(AParent: TDON_Parent; const AText: string; AStringOptions: TmnJsonTypeOptions = []); overload;
    property StringOptions: TmnJsonTypeOptions read FStringOptions write FStringOptions;
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

  TDON_Number_Value = class(TDON_Element)
  private
    FValue: Double;
    FIsHex: Boolean;
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
    constructor Create(AParent: TDON_Parent; const ANumber: Double; aIsHex: Boolean = False); overload;
    property IsHex: Boolean read FIsHex write FIsHex;
  published
    property Value: Double read FValue write FValue;
  end;

  { TDON_Boolean_Value }

  TDON_Boolean_Value = class(TDON_Element)
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

  { TDON_Comment }
  //* Not used

  TDON_Comment = class(TDON_Element)
  private
    FValue: string;
  public
    constructor Create(AParent: TDON_Parent; AValue: String); overload;
  published
    property Value: string read FValue write FValue;
  end;

  TDON_Parent = class abstract(TDON_Element)
  end;

  { Arrays }

  { TDON_List }

  TDON_List = class(TmnObjectList<TDON_Element>)
  public
  end;

  { TDON_Array_Value }

  TDON_Array_Value = class(TDON_Parent)
  private
    FItems: TDON_List;
    function GetCount: Integer;
  protected
    function GetAsString: string; override;
    function FindItem(const Name: string): TDON_Element; override;
    function GetItem(Index: Integer): TDON_Element; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    function Add(Value: TDON_Element): TDON_Element; overload;
    function Add(const Value: String): TDON_Element; overload;
    procedure Add(const Values: array of const); overload;

    property Items: TDON_List read FItems;
    property Count: Integer read GetCount;
  published
  end;

  { Pairs }

  { TDON_Pair }

  TDON_Pair = class(TDON_Parent)
  private
    FParent: TDON_Object_Value;
    FName: string;
    FValue: TDON_Element;
    procedure SetPairValue(AValue: TDON_Element);
  protected
    function FindItem(const Name: string): TDON_Element; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create(AParent: TDON_Object_Value);
    destructor Destroy; override;
    function ReleaseValue: TDON_Element;
  published
    property Value: TDON_Element read FValue write SetPairValue;
    property Name: string read FName write FName;
  end;

  { TDON_Pairs }

  TDON_Pairs = class(TmnObjectList<TDON_Pair>)
  public
  end;

  { Objects }

  { TDON_Object_Value }

  TDON_Object_Value = class(TDON_Parent)
  private
    FPairs: TDON_Pairs;
  protected
    function FindItem(const Name: string): TDON_Element; override;
    function GetItem(Index: Integer): TDON_Element; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    function CreatePair(const PairName: string): TDON_Pair;
    procedure AcquirePair(const AName: string; out AObject: TObject);
    procedure AddPair(Value: TDON_Pair); overload;
    function AddPair(const Name: String; const Value: string): TDON_Element; overload;
    property Pairs: TDON_Pairs read FPairs;
  published
  end;

  { TDON_Root }

  TDON_Root = class(TDON_Pair)
  public
    constructor Create(AParent: TDON_Object_Value);
  end;

//* Serializer
procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList; Options: TSerializerOptions = []);
procedure JsonConsoleSerialize(Pair: TDON_Pair; Options: TSerializerOptions = []);

function JsonParseFile(const FileName: string; Options: TJSONParseOptions = []): TDON_Root;
function JsonParseString(const Content: string; Options: TJSONParseOptions = []): TDON_Root;
//* For testing
function JsonParseChunks(const Content: string; Options: TJSONParseOptions = []; ChunkSize: Integer = 3): TDON_Root;

//Useful function to build JSON objects
function JsonParseStringPair(const S: utf8string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
//* {"value": "test1"}
function JsonParseStringValue(const S: utf8string; out Error: string; Options: TJSONParseOptions = []): TDON_Element;
function JsonParseFilePair(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
function JsonParseFileValue(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Element;

//Used in JSON parser
procedure JsonParseAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; ATypeOptions: TmnJsonTypeOptions; out AObject: TObject);

implementation

function JsonParseFile(const FileName: string; Options: TJSONParseOptions = []): TDON_Root;
var
  Parser: TmnJSONParser;
  w: TmnWrapperStream;
  fs: TFileStream;
  aLine: string;
begin
  if not FileExists(FileName) then
    raise Exception.Create('File not found ' + FileName);
  Result := TDON_Root.Create(nil);
  Parser.Init(Result, @JsonParseAcquireCallback, Options);
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    w := TmnWrapperStream.Create(fs, False);
    try
      while not (cloRead in w.State) do
      begin
        if w.ReadLine(aLine, False) then
          Parser.Parse(aLine)
      end;
      Parser.Finish;
    finally
      w.Free;
    end;
  finally
    fs.Free;
  end;
end;

function JsonParseString(const Content: string; Options: TJSONParseOptions = []): TDON_Root;
var
  Parser: TmnJSONParser;
  w: TmnWrapperStream;
  fs: TFileStream;
begin
  Result := TDON_Root.Create(nil);
  Parser.Init(Result, @JsonParseAcquireCallback, Options);
  Parser.Parse(Content);
  Parser.Finish;
end;

function JsonParseChunks(const Content: string; Options: TJSONParseOptions;
  ChunkSize: Integer): TDON_Root;
var
  Parser: TmnJSONParser;
  w: TmnWrapperStream;
  fs: TFileStream;
  s: string;
  i: Integer;
begin
  Result := TDON_Root.Create(nil);
  i:=1;
  Parser.Init(Result, @JsonParseAcquireCallback, Options);
  while i < Length(Content) do
  begin
    s := copy(Content, i, ChunkSize);
    write(s);
    Parser.Parse(s);
    i := i + ChunkSize;
  end;
  Parser.Finish;
end;

procedure JsonParseAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; ATypeOptions: TmnJsonTypeOptions; out AObject: TObject);

  function CreateObjectValue: TObject; {$Ifdef D-}inline; {$endif}
  begin
    Result := nil;
    case ValueType of
      //donComment: Result := TDON_Comment.Create(nil);
      aqNumber:
      begin
        if StartsStr('0x', Value) then
          Result := TDON_Number_Value.Create(nil, StrToIntDef('$'+Copy(Value, 3, MaxInt), 0), True)
        else
          Result := TDON_Number_Value.Create(nil, StrToFloatDef(Value, 0));
      end;
      aqIdentifier:
      begin
        if SameText('true', Value) then
          Result := TDON_Boolean_Value.Create(nil, True)
        else if SameText('false', Value) then
          Result := TDON_Boolean_Value.Create(nil, False)
        else
          Result := TDON_Identifier_Value.Create(nil, Value);
      end;
      aqBoolean: Result := TDON_Boolean_Value.Create(nil, StrToBoolDef(Value, False));
      aqString: Result := TDON_String_Value.Create(nil, Value, ATypeOptions);
      aqObject: Result := TDON_Object_Value.Create(nil);
      aqArray: Result := TDON_Array_Value.Create(nil);
    end;
  end;

begin
  case ValueType of
    aqPair:
      (AParentObject as TDON_Object_Value).AcquirePair(Value, AObject);
    else
    begin
      AObject := nil;
      if AParentObject = nil then
        raise Exception.Create('Can not set value to nil object');

      if (AParentObject is TDON_Array_Value) then
      begin
        AObject := CreateObjectValue;
        (AParentObject as TDON_Array_Value).Add(TDON_Element(AObject));
      end
      else if (AParentObject is TDON_Pair) then
      begin
         if (AParentObject as TDON_Pair).Value <> nil then
          raise Exception.Create('Value is already set and it is not array: ' + AParentObject.ClassName);
        AObject := CreateObjectValue;
        (AParentObject as TDON_Pair).Value := TDON_Element(AObject);
      end
      {else if (AParentObject is TDON_Object_Value) then
      begin
        if
        Result := (AParentObject as TDON_Object_Value).CreatePair(AValue);
      end}
      else
        raise Exception.Create('Value can not be set to:' + AParentObject.ClassName);
    end;
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

function JsonParseStringValue(const S: utf8string; out Error: string; Options: TJSONParseOptions): TDON_Element;
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

function JsonParseFileValue(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Element;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseFilePair(FileName, Error, Options);
  Result := Pair.ReleaseValue;
end;

procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList;
  Options: TSerializerOptions);
var
  Serializer: TStringsSerializer;
begin
  Serializer := TStringsSerializer.Create(Strings);
  try
    Serializer.Options := Options;
    Serializer.Serialize(TJsonSerializeGernerator, Pair);
  finally
    Serializer.Free;
  end;
end;

procedure JsonConsoleSerialize(Pair: TDON_Pair; Options: TSerializerOptions);
var
  Serializer: TConsoleSerializer;
begin
  Serializer := TConsoleSerializer.Create;
  try
    Serializer.Options := Options;
    Serializer.Serialize(TJsonSerializeGernerator, Pair);
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
  LineTerminator := #10;
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

{ TConsoleSerializer }

procedure TConsoleSerializer.Flush;
begin
  if FLine <> '' then
  begin
    WriteLn(FLine);
    FLine := '';
  end;
end;

procedure TConsoleSerializer.Add(const S: string);
begin
  FLine := FLine + S;
end;

procedure TConsoleSerializer.NewLine;
begin
  if not (sroCompact in Options) then
  begin
    WriteLn(FLine);
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

constructor TDON_Number_Value.Create(AParent: TDON_Parent;
  const ANumber: Double; aIsHex: Boolean);
begin
  inherited Create(AParent);
  FValue := ANumber;
  FIsHex := aIsHex;
end;

{ TDON_Element }

function TDON_Element.ByPath(Path: TStrings): TDON_Element;
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

function TDON_Element.ByPath(const Path: string; Delimiter: Char): TDON_Element;
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

function TDON_Element.AddArray(const Name: String): TDON_Element;
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

function TDON_Element.AddObject: TDON_Element;
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

function TDON_Element.AddPair(const Name, Value: string): TDON_Element;
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

function TDON_Element.AddObject(const Name: String): TDON_Element;
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

function TDON_Element.AddArray: TDON_Element;
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

function TDON_Element.ByIndex(Index: Integer): TDON_Element;
begin
  Result := GetItem(Index);
end;

function TDON_Element.ByPath(const Path: TArray<string>): TDON_Element;
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

constructor TDON_Element.Create(AParent: TDON_Parent);
begin
  inherited Create;
  FParent := AParent;
end;

function TDON_Element.FindItem(const Name: string): TDON_Element;
begin
  if Name = '' then
    Result := Self
  else
    Result := nil;
end;

function TDON_Element.GetIsNull: Boolean;
begin
  Result := False;
end;

function TDON_Element.GetItem(Index: Integer): TDON_Element;
begin
  if Index = -1 then
    Result := Self
  else
    Result := nil;
end;

function TDON_Element.GetValues(const Index: string): TDON_Element;
begin
  if Self = nil then
    Result := nil
  else
    Result := FindItem(Index);
end;

procedure TDON_Element.SetValues(const Index: string; const Value: TDON_Element);
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

function TDON_Array_Value.Add(const Value: String): TDON_Element;
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

function TDON_Array_Value.FindItem(const Name: string): TDON_Element;
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

function TDON_Array_Value.GetItem(Index: Integer): TDON_Element;
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

function TDON_Array_Value.Add(Value: TDON_Element): TDON_Element;
begin
  Items.Add(Value);
  Result := Value;
end;

procedure TDON_Array_Value.SetValue(const AValue: Variant);
begin
  AsString := AValue;
end;

{ TDON_Object_Value }

function TDON_Object_Value.AddPair(const Name, Value: string): TDON_Element;
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

function TDON_Object_Value.FindItem(const Name: string): TDON_Element;
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

function TDON_Object_Value.GetItem(Index: Integer): TDON_Element;
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

procedure TDON_Pair.SetPairValue(AValue: TDON_Element);
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

function TDON_Pair.FindItem(const Name: string): TDON_Element;
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

function TDON_Pair.ReleaseValue: TDON_Element;
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

{ TDON_Root }

constructor TDON_Root.Create(AParent: TDON_Object_Value);
begin
  inherited;
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

constructor TDON_Comment.Create(AParent: TDON_Parent; AValue: String);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

{ TDON_Custom_String_Value }

constructor TDON_Custom_String_Value.Create(AParent: TDON_Parent; const AText: string; AStringOptions: TmnJsonTypeOptions);
begin
  inherited Create(AParent);
  FValue := AText;
  FStringOptions := AStringOptions;
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
  v: TDON_Element;

  function GetName(const AName: string): string; {$ifndef DEBUG}inline; {$endif}
  begin
    if (sroModern in Serializer.Options) and (Pos(' ', AName) <= 0) then
      Result := AName
    else
      Result := QuoteStr(AName, '"');
    if (sroCompact in Serializer.Options) then //need fix asp
      Result := Result + ':'
    else
      Result := Result + ': ';
  end;

  function Coalesce(B: Boolean; const V1, V2: string): string; {$ifndef DEBUG}inline; {$endif} overload;
  begin
    if B then
      Result := V1
    else
      Result := V2;
  end;

  function Coalesce(B: Boolean; const V1, V2: UTF8Char): UTF8Char; {$ifndef DEBUG}inline; {$endif} overload;
  begin
    if B then
      Result := V1
    else
      Result := V2;
  end;

var
  s: string;
  QuoteChar: UTF8Char;
  Strings: TStringList;
begin
  if AClass = TDON_Comment then
  begin
    s := (AObject as TDON_Comment).Value;
    if HaveChar(s, [#13, #10]) then //* It is a multiline
      Serializer.Add('/*'+s+'*/', LastOne, '')
    else
      Serializer.Add('//' + s, LastOne, '');
    Serializer.NewLine;
  end
  else if AClass = TDON_Pair then
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
  else if AClass = TDON_Element then
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
    QuoteChar := Coalesce(jtoSingleQuote in (AObject as TDON_String_Value).StringOptions, '''', '"');

    {if (sroModern in Serializer.Options) and (jtoMultiLine in (AObject as TDON_String_Value).StringOptions) then
    begin
        Strings := TStringList.Create;
        try
          StrToStrings((AObject as TDON_String_Value).Value, Strings);
          Serializer.Add(QuoteChar);
          for s in Strings do
            Serializer.Add(EscapeStringC(s, QuoteChar) + '\'#10);
          Serializer.Add(QuoteChar, LastOne, ',');
        finally
          Strings.Free;
        end;
    end
    else}
      Serializer.Add(QuoteStr(EscapeStringC((AObject as TDON_String_Value).Value, QuoteChar), QuoteChar), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Identifier_Value then
  begin
    Serializer.Add((AObject as TDON_Identifier_Value).Value, LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Number_Value then
  begin
    if (AObject as TDON_Number_Value).IsHex then
      Serializer.Add('0x'+IntToHex(trunc((AObject as TDON_Number_Value).Value), 0), LastOne, ',')
    else
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
