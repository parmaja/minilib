unit mnDON;
{ **
  *  Data Object Notation/Nodes Tree
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
  mnTypes, mnClasses, mnUtils, mnJSON, mnFields, mnStreams;

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

  TDON_Value = class;
  TDON_Parent = class;
  TDON_Object = class;

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

  { TDON_CustomStringValue }

  TDON_CustomStringValue = class abstract(TDON_Value)
  private
    FValue: string;
    FStringType: TmnJsonStringType;
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
    constructor Create(AParent: TDON_Parent; const AText: string; AStringType: TmnJsonStringType); overload;
    constructor Create(AParent: TDON_Parent; const AText: string); overload;
    property StringType: TmnJsonStringType read FStringType write FStringType;
  published
    property Value: string read FValue write FValue;
  end;

  { TDON_String }

  TDON_String = class(TDON_CustomStringValue)
  public
  end;

  { TDON_Identifier }

  TDON_Identifier = class(TDON_CustomStringValue)
  private
  protected
    function GetIsNull: Boolean; override;
  public
  published
  end;

  { TDON_Number }

  TDON_Number = class(TDON_Value)
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

  { TDON_Boolean }

  TDON_Boolean = class(TDON_Value)
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

  TDON_Comment = class(TDON_Value)
  private
    FValue: string;
  public
    constructor Create(AParent: TDON_Parent; AValue: String); overload;
  published
    property Value: string read FValue write FValue;
  end;

  TDON_Parent = class abstract(TDON_Value)
  end;

  { Arrays }

  { TDON_Array }

  TDON_Array = class(TDON_Parent)
  protected
    { TDON_List }
    type
      TDON_List = class(TmnObjectList<TDON_Value>)
      public
      end;
  private
    FItems: TDON_List;
    function GetCount: Integer;
  protected

    function GetAsString: string; override;
    function FindItem(const Name: string): TDON_Value; override;
    function GetItem(Index: Integer): TDON_Value; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure Created; override;
  public
    constructor Create(AParent: TDON_Parent = nil); overload;
    destructor Destroy; override;
    function Add(Value: TDON_Value): TDON_Value; overload;
    function Add(const Value: String): TDON_Value; overload;
    procedure Add(const Values: array of const); overload;

    property Items: TDON_List read FItems;
    property Item[Index: Integer]: TDON_Value read GetItem; default;
    property Count: Integer read GetCount;
  published
  end;

  { Pairs }

  { TDON_Pair }

  TDON_Pair = class(TDON_Parent)
  private
    FName: string;
    FValue: TDON_Value;

    procedure SetPairValue(AValue: TDON_Value);
  protected
    function FindItem(const Name: string): TDON_Value; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create(AParent: TDON_Object);
    destructor Destroy; override;
    function ReleaseValue: TDON_Value;
  published
    property Value: TDON_Value read FValue write SetPairValue;
    property Name: string read FName write FName;
  end;

  { TDON_Pairs }

  TDON_Pairs = class(TmnObjectList<TDON_Pair>)
  public
  end;

  { Objects }

  { TDON_Object }

  TDON_Object = class(TDON_Parent)
  private
    FPairs: TDON_Pairs;
  protected
    type

      { TmnObjectListEnumerator }

      TPairsEnumerator = class(TObject)
      private
        FList: TDON_Pairs;
        FIndex: Integer;
      public
        constructor Create(AList: TDON_Pairs);
        function GetCurrent: TDON_Pair; inline;
        function MoveNext: Boolean; inline;
        property Current: TDON_Pair read GetCurrent;
      end;
          
    function FindItem(const Name: string): TDON_Value; override;
   
    function GetItem(Index: Integer): TDON_Value; override;
    function GetAsString: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure Created; override;
  public
    function GetEnumerator: TPairsEnumerator; inline;
    constructor Create(AParent: TDON_Parent = nil); overload;
    destructor Destroy; override;
    function CreatePair(const PairName: string; AValue: TDON_Value = nil): TDON_Pair;
    procedure AcquirePair(const AName: string; out AObject: TObject);
    procedure AddPair(Value: TDON_Pair); overload;
    function AddPair(const Name: String; const Value: string): TDON_Value; overload;

    function FindPair(const Name: string): TDON_Pair; overload;
    function FindByValue(const Value: string): TDON_Pair; overload;
    function FindNameByValue(const Value: string): string;  overload;
    
    property Pairs: TDON_Pairs read FPairs;    
  published
  end;

//*---------------------------------------------------------------------------------------------    
const
  cIndentSpaces = 2;

type  

  { TmnTidyWriter }

  TmnTidyWriterOptions = set of (woEndLine, woOpenIndent, woCloseIndent);

  TmnTidyWriter = class(TmnObject)
  private
    Level: Integer;
    NewLine: Boolean;
    FStream: TmnBufferStream;
  public
    Compact: Boolean;
    constructor Create(AName: string; AStream: TmnBufferStream);
    procedure WriteBOM;
    procedure Write(S: string; Options: TmnTidyWriterOptions = []); virtual;
    procedure WriteLn(const S: string = ''; Options: TmnTidyWriterOptions = []);
    procedure WriteLine(const S: string = ''; Options: TmnTidyWriterOptions = [woEndLine]);
    procedure WriteLines(const S: string = ''; Options: TmnTidyWriterOptions = []);
    function WriteStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload; inline;
    property Stream: TmnBufferStream read FStream write FStream;
  end;

  { TmnwXML_TidyWriterHelper }

  TmnwXML_TidyWriterHelper = class helper for TmnTidyWriter
  public
    procedure OpenTag(const Tag: string); overload;
    procedure OpenTag(const TagName, TagAttributes: string; TagText: string = ''); overload;
    procedure OpenTagA(const TagName, Classes: string; Attributes: string; TagText: string = ''); overload;
    procedure OpenInlineTag(const TagName:string; TagAttributes: string = ''; TagText: string = ''); overload; // keep inline, no new line
    procedure CloseTag(const Tag: string; TrailText: string = '');
    procedure AddShortTag(const TagName:string; TagAttributes: string = ''); overload; //* Self closed tag, without </tagname>
    procedure AddComment(const Comment: string);
    procedure AddInlineShortTag(const TagName:string; TagAttributes: string = ''); overload; //* Self closed tag, without </tagname>
    procedure AddTag(const TagName, TagAttributes: string); overload;
    procedure AddTag(const TagName, TagAttributes, Value: string); overload;
    procedure AddInlineTag(const TagName, TagAttributes, Value: string); overload;
    procedure ReadFromFile(FileName: string);

    procedure AddSpace;
    procedure AddLinkScript(const src: string; Integrity: string = ''; Defer: Boolean = True; Cross: Boolean = True);
    procedure AddEmbedScript(const Text: string; Defer: Boolean = True);
    procedure AddLinkStyle(const src: string; Integrity: string = ''; Cross: Boolean  = True);
    procedure AddEmbedStyle(const Text: string);
  end; 
  
//* Serializer
procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList; Options: TSerializerOptions = []); overload;
procedure JsonConsoleSerialize(AObject: TDON_Value; Options: TSerializerOptions = []); overload;

// Save
procedure JsonSaveStream(Pair: TDON_Pair; AStream: TStream; Options: TSerializerOptions = []); overload;
procedure JsonSaveFile(Pair: TDON_Pair; FileName: string; Options: TSerializerOptions = []); overload;

procedure JsonSaveStream(Obj: TDON_Object; AStream: TStream; Options: TSerializerOptions = []); overload;
procedure JsonSaveFile(Obj: TDON_Object; FileName: string; Options: TSerializerOptions = []); overload;
procedure JsonSaveString(Obj: TDON_Object; out Result: string; Options: TSerializerOptions = []); overload;

//Loading file line by line, for file not socket (timeouts)

procedure JsonLoadStream(Pair: TDON_Pair; Stream: TStream; Options: TJSONParseOptions = []); overload;

function JsonLoadPairStream(Stream: TStream; Options: TJSONParseOptions = []): TDON_Pair; overload;
function JsonLoadValueStream(Stream: TStream; Options: TJSONParseOptions = []): TDON_Value; overload;

procedure JsonLoadFile(Pair: TDON_Pair; const FileName: string; Options: TJSONParseOptions = []); overload;
function JsonLoadFile(const FileName: string; Options: TJSONParseOptions = []): TDON_Pair; overload;

// Loading from String
procedure JsonParseString(Pair: TDON_Pair; const Content: string; Options: TJSONParseOptions = []); overload;
function JsonParseString(const Content: string; Options: TJSONParseOptions = []): TDON_Pair; overload;
//* {"value": "test1"}
function JsonParseValueString(const Content: string; Options: TJSONParseOptions = []): TDON_Value; overload;

//* For testing
function JsonParseChunks(const Content: string; Options: TJSONParseOptions = []; ChunkSize: Integer = 3): TDON_Pair;
function JsonParsePairString(const S: utf8string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;

//Load file but parse as string
function JsonParsePairFile(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
function JsonParseValueFile(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Value;

//Used in JSON parser
procedure JsonParseAcquireCallback(out AObject: TObject; AParentObject: TObject; const Value: string; const ValueType: TmnJsonType; const AStringType: TmnJsonStringType);

implementation

procedure JsonSaveStream(Pair: TDON_Pair; AStream: TStream; Options: TSerializerOptions = []);
var
  Serializer: TStreamSerializer;
begin
  Serializer := TStreamSerializer.Create(AStream, True);
  try
    Serializer.Options := Options;
    Serializer.Serialize(TJsonSerializeGernerator, Pair);
  finally
    Serializer.Free;
  end;
end;

procedure JsonSaveFile(Pair: TDON_Pair; FileName: string; Options: TSerializerOptions = []);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenWrite or fmCreate);  
  try
    JsonSaveStream(Pair, AStream, Options);
  finally
    AStream.Free;
  end;
end;

procedure JsonSaveStream(Obj: TDON_Object; AStream: TStream; Options: TSerializerOptions = []);
var
  Serializer: TStreamSerializer;
begin
  Serializer := TStreamSerializer.Create(AStream, True);
  try
    Serializer.Options := Options;
    Serializer.Serialize(TJsonSerializeGernerator, Obj);
  finally
    Serializer.Free;
  end;
end;

procedure JsonSaveFile(Obj: TDON_Object; FileName: string; Options: TSerializerOptions = []);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenWrite or fmCreate);
  try
    JsonSaveStream(Obj, AStream, Options);
  finally
    AStream.Free;
  end;
end;

procedure JsonSaveString(Obj: TDON_Object; out Result: string; Options: TSerializerOptions = []); overload;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create('');
  try
    JsonSaveStream(Obj, AStream, Options);
    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

procedure JsonLoadStream(Pair: TDON_Pair; Stream: TStream; Options: TJSONParseOptions = []); overload;
var
  Parser: TmnJSONParser;
  w: TmnWrapperStream;
  aLine: string;
begin
  Parser.Init(Pair, @JsonParseAcquireCallback, Options);
  w := TmnWrapperStream.Create(Stream, False);
  try
    while w.CanRead do
    begin
      if w.ReadUTF8Line(aLine, False) then
      begin
        try
          Parser.Parse(aLine);
        except
          on E: Exception do
          begin
            E.Message := E.Message + sLineBreak + 'On line:' + sLineBreak + aLine;
            raise;
          end;
        end;
      end;
    end;
    Parser.Finish;
  finally
    w.Free;
  end;
end;

function JsonLoadPairStream(Stream: TStream; Options: TJSONParseOptions = []): TDON_Pair; overload;
begin
  Result := TDON_Pair.Create(nil);
  JsonLoadStream(Result, Stream, Options); 
end;

function JsonLoadValueStream(Stream: TStream; Options: TJSONParseOptions = []): TDON_Value; overload;
var
  Pair: TDON_Pair;
begin
  Pair := JsonLoadPairStream(Stream, Options);  
  try
    if Pair<>nil then
      Result := Pair.ReleaseValue
    else
      Result := nil;
  finally
    Pair.Free;
  end;
end;

procedure JsonLoadFile(Pair: TDON_Pair; const FileName: string; Options: TJSONParseOptions = []);
var
  fs: TFileStream;
begin
  if not FileExists(FileName) then
    raise Exception.Create('File not found ' + FileName);  
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    try
      JsonLoadStream(Pair, fs, Options);
    except
      on E: Exception do
      begin
        raise Exception.Create(E.Message + #13'file: ' + FileName);
      end;
    end;
  finally
    fs.Free;
  end;
end;

function JsonLoadFile(const FileName: string; Options: TJSONParseOptions = []): TDON_Pair; overload;
begin
  Result := TDON_Pair.Create(nil);
  JsonLoadFile(Result, FileName, Options);
end;

procedure JsonParseString(Pair: TDON_Pair; const Content: string; Options: TJSONParseOptions = []); overload;
var
  Parser: TmnJSONParser;
begin  
  Parser.Init(Pair, @JsonParseAcquireCallback, Options);
  Parser.Parse(Content);
  Parser.Finish;
end;

function JsonParseString(const Content: string; Options: TJSONParseOptions = []): TDON_Pair;
begin
  Result := TDON_Pair.Create(nil);
  JsonParseString(Result, Content, Options);
end;

function JsonParseValueString(const Content: string; Options: TJSONParseOptions = []): TDON_Value; overload;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParseString(Content, Options);  
  try
    if Pair<>nil then
      Result := Pair.ReleaseValue
    else
      Result := nil;
  finally
    Pair.Free;
  end;
end;

function JsonParseChunks(const Content: string; Options: TJSONParseOptions; ChunkSize: Integer): TDON_Pair;
var
  Parser: TmnJSONParser;
  s: string;
  i: Integer;
begin
  Result := TDON_Pair.Create(nil);
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

procedure JsonParseAcquireCallback(out AObject: TObject; AParentObject: TObject; const Value: string; const ValueType: TmnJsonType; const AStringType: TmnJsonStringType);

  function CreateObjectValue: TObject; {$Ifdef D-}inline; {$endif}
  begin
    Result := nil;
    case ValueType of
      //donComment: Result := TDON_Comment.Create(nil);
      aqNumber:
      begin
        if StartsStr('0x', Value) then
          Result := TDON_Number.Create(nil, StrToIntDef('$'+Copy(Value, 3, MaxInt), 0), True)
        else
          Result := TDON_Number.Create(nil, StrToFloatDef(Value, 0));
      end;
      aqIdentifier:
      begin
        if SameText('true', Value) then
          Result := TDON_Boolean.Create(nil, True)
        else if SameText('false', Value) then
          Result := TDON_Boolean.Create(nil, False)
        else
          Result := TDON_Identifier.Create(nil, Value);
      end;
      aqBoolean: Result := TDON_Boolean.Create(nil, StrToBoolDef(Value, False));
      aqString: Result := TDON_String.Create(nil, Value, AStringType);
      aqObject: Result := TDON_Object.Create(nil);
      aqArray: Result := TDON_Array.Create(nil);
    end;
  end;

begin
  case ValueType of
    aqPair:
      (AParentObject as TDON_Object).AcquirePair(Value, AObject);
    else
    begin
      AObject := nil;
      if AParentObject = nil then
        raise Exception.Create('Can not set value to nil object');

      if (AParentObject is TDON_Array) then
      begin
        AObject := CreateObjectValue;
        (AParentObject as TDON_Array).Add(TDON_Value(AObject));
      end
      else if (AParentObject is TDON_Pair) then
      begin
         if (AParentObject as TDON_Pair).Value <> nil then
          raise Exception.Create('Value is already set and it is not array: ' + AParentObject.ClassName);
        AObject := CreateObjectValue;
        (AParentObject as TDON_Pair).Value := TDON_Value(AObject);
      end
{      else if (AParentObject is TDON_Object) and (AParentObject.Parent = nil) then
      begin
          AObject := AParentObject;
        //  AObject := CreateObjectValue;
        //(AParentObject as TDON_Object).CreatePair('', TDON_Value(AObject));
      end}
      else
        raise Exception.Create('Value can not be set to:' + AParentObject.ClassName);
    end;
  end;
end;

function JsonParsePairString(const S: utf8string; out Error: string; Options: TJSONParseOptions): TDON_Pair;
begin
  Result := TDON_Pair.Create(nil);
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

function JsonParsePairFile(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Pair;
begin
  Result := JsonParsePairString(Utf8Encode(LoadFileString(FileName)), Error, Options)
end;

function JsonParseValueFile(const FileName: string; out Error: string; Options: TJSONParseOptions = []): TDON_Value;
var
  Pair: TDON_Pair;
begin
  Pair := JsonParsePairFile(FileName, Error, Options);
  try
    Result := Pair.ReleaseValue;
  finally
    Pair.Free;
  end;
end;

procedure JsonSerialize(Pair: TDON_Pair; Strings: TStringList; Options: TSerializerOptions);
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

procedure JsonConsoleSerialize(AObject: TDON_Value; Options: TSerializerOptions);
var
  Serializer: TConsoleSerializer;
begin
  Serializer := TConsoleSerializer.Create;
  try
    Serializer.Options := Options;
    Serializer.Serialize(TJsonSerializeGernerator, AObject);
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
  try
    Gernerator.Generate(AObject, True, 0);
    Flush;
  finally
    Gernerator.Free;
  end;
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

{ TDON_Number }

function TDON_Number.GetAsBoolean: Boolean;
begin
  Result := AsDouble <> 0;
end;

function TDON_Number.GetAsCurrency: Currency;
begin
  Result := AsDouble;
end;

function TDON_Number.GetAsDateTime: TDateTime;
begin
  Result := AsDouble;
end;

function TDON_Number.GetAsDouble: Double;
begin
  Result := FValue;
end;

function TDON_Number.GetAsInteger: Integer;
begin
  Result := Trunc(FValue);
end;

function TDON_Number.GetAsString: string;
begin
  Result := FloatToStr(FValue);
end;

function TDON_Number.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDON_Number.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TDON_Number.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TDON_Number.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TDON_Number.SetAsDouble(const Value: Double);
begin
  FValue := Value;

end;

procedure TDON_Number.SetAsInteger(const Value: Integer);
begin
  FValue := Trunc(Value);
end;

procedure TDON_Number.SetAsString(const Value: string);
begin
  FValue := StrToFloatDef(Value, 0);
end;

procedure TDON_Number.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TDON_Number.Create(AParent: TDON_Parent;
  const ANumber: Double; aIsHex: Boolean);
begin
  inherited Create(AParent);
  FValue := ANumber;
  FIsHex := aIsHex;
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
  if (Self is TDON_Object) then
  begin
    aPair := (Self as TDON_Object).CreatePair(Name);
    aPair.Value := TDON_Array.Create(aPair);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('You cant add object here');
end;

function TDON_Value.AddObject: TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object) then
  begin
    aPair := (Self as TDON_Object).CreatePair('');
    aPair.Value := TDON_Object.Create(aPair);
    Result := aPair.Value;
  end
  else if (Self is TDON_Array) then
  begin
    Result := (Self as TDON_Array).Add(TDON_Object.Create(Self as TDON_Array));
  end
  else if (FParent is TDON_Pair) then
  begin
    Result := TDON_Object.Create(Self as TDON_Pair);
    (Self as TDON_Pair).Value := Result;
  end
  else
    raise Exception.Create('Can not add object here');
end;

function TDON_Value.AddPair(const Name, Value: string): TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object) then
  begin
    aPair := (Self as TDON_Object).CreatePair(Name);
    aPair.Value := TDON_String.Create(aPair, Value);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('Not an object');
end;

function TDON_Value.AddObject(const Name: String): TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object) then
  begin
    aPair := (Self as TDON_Object).CreatePair(Name);
    aPair.Value := TDON_Object.Create(aPair);
    Result := aPair.Value;
  end
  else
    raise Exception.Create('You can add object with name here');
end;

function TDON_Value.AddArray: TDON_Value;
var
  aPair: TDON_Pair;
begin
  if (Self is TDON_Object) then
  begin
    aPair := (Self as TDON_Object).CreatePair('');
    aPair.Value := TDON_Array.Create(aPair);
    Result := aPair.Value;
  end
  else if (Self is TDON_Array) then
  begin
    Result := (Self as TDON_Array).Add(TDON_Array.Create(Self as TDON_Array));
  end
  else if (FParent is TDON_Pair) then
  begin
    Result := TDON_Array.Create(Self as TDON_Pair);
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

{ TDON_Array }

procedure TDON_Array.Add(const Values: array of const);
var
  i : Integer;
begin
  for i := 0 to High(Values) do
  begin
    case Values[i].vType of
      vtBoolean:
        Items.Add(TDON_Boolean.Create(Self, Values[i].VBoolean));
      vtChar:
        Items.Add(TDON_String.Create(Self, String(Values[i].VChar)));
      vtString:
        Items.Add(TDON_String.Create(Self, String(Values[i].VString^)));
      vtInteger:
        Items.Add(TDON_Number.Create(Self, Values[i].VInteger));
      vtExtended:
        Items.Add(TDON_Number.Create(Self, Values[i].VExtended^));
    end;
  end;
end;

function TDON_Array.Add(const Value: String): TDON_Value;
begin
  Result := TDON_String.Create(Self, Value);
  Add(Result);
end;

constructor TDON_Array.Create(AParent: TDON_Parent);
begin
  inherited Create(AParent);
end;

procedure TDON_Array.Created;
begin
  inherited;
  FItems := TDON_List.Create;
end;

destructor TDON_Array.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TDON_Array.FindItem(const Name: string): TDON_Value;
begin
  Result := nil;
end;

function TDON_Array.GetAsString: string;
begin
  Result := '{Array}';
end;

function TDON_Array.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TDON_Array.GetItem(Index: Integer): TDON_Value;
begin
  if Index < FItems.Count then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TDON_Array.GetValue: Variant;
begin
  Result := AsString;
end;

function TDON_Array.Add(Value: TDON_Value): TDON_Value;
begin
  Items.Add(Value);
  Result := Value;
end;

procedure TDON_Array.SetValue(const AValue: Variant);
begin
  AsString := AValue;
end;

{ TDON_Object }

function TDON_Object.AddPair(const Name, Value: string): TDON_Value;
var
  aPair: TDON_Pair;
begin
  aPair := CreatePair(Name);
  aPair.Value := TDON_String.Create(aPair, Value);
  Result := aPair.Value;
end;

constructor TDON_Object.Create(AParent: TDON_Parent);
begin
  inherited Create(AParent);
end;

procedure TDON_Object.Created;
begin
  inherited;
  FPairs := TDON_Pairs.Create;
end;

function TDON_Object.CreatePair(const PairName: string; AValue: TDON_Value = nil): TDON_Pair;
begin
  Result := TDON_Pair.Create(Self);
  Result.FName := PairName;
  Result.Value := AValue;
  AddPair(Result);
end;

destructor TDON_Object.Destroy;
begin
  FreeAndNil(FPairs);
  inherited;
end;

function TDON_Object.FindItem(const Name: string): TDON_Value;
var
  i: Integer;
begin
  //for speed do not put it in FPairs.Find(Name)
  for i := 0 to FPairs.Count-1 do
    //if FPairs[i].Name = Name then
    if SameText(FPairs[i].Name, Name) then
    begin
      Exit(FPairs[i].Value);
    end;
  Result := nil
end;

function TDON_Object.FindByValue(const Value: string): TDON_Pair;
var
  i: Integer;
begin
  for i := 0 to FPairs.Count-1 do
    if SameText(FPairs[i].Value.AsString, Value) then
    begin
      Exit(FPairs[i]);
    end;
  Result := nil
end;

function TDON_Object.FindNameByValue(const Value: string): string;
var
  i: Integer;
begin
  for i := 0 to FPairs.Count-1 do
    if SameText(FPairs[i].Value.AsString, Value) then
    begin
      Exit(FPairs[i].Name);
    end;
  Result := '';
end;

function TDON_Object.FindPair(const Name: string): TDON_Pair;
var
  i: Integer;
begin
  for i := 0 to FPairs.Count-1 do
    if SameText(FPairs[i].Name, Name) then
    begin
      Exit(FPairs[i]);
    end;
  Result := nil
end;

function TDON_Object.GetAsString: string;
begin
  //TODO Use Parent root to save it
  JsonSaveString(Self, Result); //TODO: What if i want it XML?
end;

function TDON_Object.GetEnumerator: TPairsEnumerator;
begin
  Result := TPairsEnumerator.Create(FPairs);
end;

function TDON_Object.GetItem(Index: Integer): TDON_Value;
begin
  Result := FPairs[Index].Value;
end;

function TDON_Object.GetValue: Variant;
begin
  Result := AsString;
end;

procedure TDON_Object.AcquirePair(const AName: string; out AObject: TObject);
begin
  AObject := TDON_Pair.Create(Self);
  (AObject as TDON_Pair).FName := AName;
  AddPair((AObject as TDON_Pair));
end;

procedure TDON_Object.AddPair(Value: TDON_Pair);
begin
  FPairs.Add(Value);
end;

procedure TDON_Object.SetValue(const AValue: Variant);
begin
  AsString := aValue;
end;

{ TDON_Pair }

procedure TDON_Pair.SetPairValue(AValue: TDON_Value);
begin
  if FValue <> AValue then
  begin
    if (AValue <> nil) and (AValue.Parent <> nil) and (AValue.Parent <> Self) then
      raise Exception.Create('Value have parent we can`t move it to another parent');
    FreeAndNil(FValue);
    FValue := AValue;
    if FValue <> nil then
      FValue.FParent := Self;
  end;
end;

constructor TDON_Pair.Create(AParent: TDON_Object);
begin
  inherited Create(AParent);
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
    Result.FParent := nil;
    FValue := nil;
  end
  else
    Result := nil;
end;

procedure TDON_Pair.SetValue(const AValue: Variant);
begin
  AsString := AValue;
end;

{ TDON_Boolean }

constructor TDON_Boolean.Create(AParent: TDON_Parent; AValue: Boolean);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

function TDON_Boolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TDON_Boolean.GetAsCurrency: Currency;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean.GetAsDateTime: TDateTime;
begin
  Result := 0;
end;

function TDON_Boolean.GetAsDouble: Double;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean.GetAsInteger: Integer;
begin
  Result := Ord(AsBoolean);
end;

function TDON_Boolean.GetAsString: string;
begin
  Result := BoolToStr(FValue);
end;

function TDON_Boolean.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TDON_Boolean.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TDON_Boolean.SetAsCurrency(const Value: Currency);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value <> 0;

end;

procedure TDON_Boolean.SetAsDouble(const Value: Double);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean.SetAsInteger(const Value: Integer);
begin
  FValue := Value <> 0;
end;

procedure TDON_Boolean.SetAsString(const Value: string);
begin
  FValue := StrToBoolDef(Value, False);
end;

procedure TDON_Boolean.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TDON_Comment.Create(AParent: TDON_Parent; AValue: String);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

{ TDON_CustomStringValue }

constructor TDON_CustomStringValue.Create(AParent: TDON_Parent; const AText: string; AStringType: TmnJsonStringType);
begin
  inherited Create(AParent);
  FValue := AText;
  FStringType := AStringType;
end;

constructor TDON_CustomStringValue.Create(AParent: TDON_Parent; const AText: string);
begin
  Create(AParent, AText, Default(TmnJsonStringType));
end;

function TDON_CustomStringValue.GetAsBoolean: Boolean;
begin
  Result := StrToBoolDef(AsString, False);
end;

function TDON_CustomStringValue.GetAsCurrency: Currency;
begin
  Result := StrToCurrDef(AsString, 0);
end;

function TDON_CustomStringValue.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(AsString, 0);
end;

function TDON_CustomStringValue.GetAsDouble: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TDON_CustomStringValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TDON_CustomStringValue.GetAsString: string;
begin
  if Self = nil then
    Result := ''
  else
    Result := FValue;
end;

function TDON_CustomStringValue.GetValue: Variant;
begin  
  Result := FValue;
end;

procedure TDON_CustomStringValue.SetAsBoolean(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TDON_CustomStringValue.SetAsCurrency(const Value: Currency);
begin
  FValue := CurrToStr(Value);
end;

procedure TDON_CustomStringValue.SetAsDateTime(const Value: TDateTime);
begin
  FValue := DateTimeToStr(Value);
end;

procedure TDON_CustomStringValue.SetAsDouble(const Value: Double);
begin
  FValue := FloatToStr(Value);
end;

procedure TDON_CustomStringValue.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);

end;

procedure TDON_CustomStringValue.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TDON_CustomStringValue.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TDON_Identifier}

function TDON_Identifier.GetIsNull: Boolean;
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

  function GetName(const AName: string): string; //{$ifndef DEBUG}inline; {$endif}
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

  function Coalesce(B: Boolean; const V1, V2: Char): Char; {$ifndef DEBUG}inline; {$endif} overload;
  begin
    if B then
      Result := V1
    else
      Result := V2;
  end;

var
  s: string;
  QuoteChar: Char;
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
  else if AClass = TDON_Object then
  begin
    with AObject as TDON_Object do
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
  else if AClass = TDON_Array then
  begin
    with AObject as TDON_Array do
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
  else if AClass = TDON_Pair then
    Generate((AObject as TDON_Pair).Value, LastOne, Level)
  else if AClass = TDON_String then
  begin
    if jtoBackQuote in (AObject as TDON_String).StringType.Options then
    begin
      if ((AObject as TDON_String).StringType.Name <> '') then
        Serializer.Add('`' + (AObject as TDON_String).StringType.Name);
      if jtoMultiLine in (AObject as TDON_String).StringType.Options then
        Serializer.NewLine
      else
        Serializer.Add(' ');
      Serializer.Add((AObject as TDON_String).Value);
      Serializer.Add('`', LastOne, ',');
    end
    else
    begin
      QuoteChar := Coalesce(jtoSingleQuote in (AObject as TDON_String).StringType.Options, '''', '"');

    {if (sroModern in Serializer.Options) and (jtoMultiLine in (AObject as TDON_String).StringOptions) then
    begin
        Strings := TStringList.Create;
        try
          StrToStrings((AObject as TDON_String).Value, Strings);
          Serializer.Add(QuoteChar);
          for s in Strings do
            Serializer.Add(EscapeStringC(s, QuoteChar) + '\'#10);
          Serializer.Add(QuoteChar, LastOne, ',');
        finally
          Strings.Free;
        end;
    end
    else}
      Serializer.Add(QuoteStr(EscapeCString((AObject as TDON_String).Value, QuoteChar), QuoteChar), LastOne, ',');
    end;
    Serializer.NewLine;
  end
  else if AClass = TDON_Identifier then
  begin
    Serializer.Add((AObject as TDON_Identifier).Value, LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Number then
  begin
    if (AObject as TDON_Number).IsHex then
      Serializer.Add('0x'+IntToHex(trunc((AObject as TDON_Number).Value), 0), LastOne, ',')
    else
      Serializer.Add(FloatToStr((AObject as TDON_Number).Value), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass = TDON_Boolean then
  begin
    Serializer.Add(BoolToStr((AObject as TDON_Boolean).Value, True), LastOne, ',');
    Serializer.NewLine;
  end
  else if AClass.ClassParent <> nil then //if we cant find it we take parent class
    Generate(AClass.ClassParent, AObject, LastOne, Level);
end;

{ TStreamSerializer }

procedure TStreamSerializer.Add(const S: string);
begin
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
  FIsUTF8 := vIsUTF8;
end;

destructor TStreamSerializer.Destroy;
begin
  inherited;
end;

(*
Source - https://stackoverflow.com/a/2971923
Posted by da-soft, modified by community. See post 'Timeline' for change history
Retrieved 2026-04-26, License - CC BY-SA 2.5
*)

{ TmnTidyWriter }

function LevelStr(vLevel: Integer): String; inline;
begin
  Result := StringOfChar(' ', vLevel * cIndentSpaces);
end;

constructor TmnTidyWriter.Create(AName: string; AStream: TmnBufferStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TmnTidyWriter.Write(S: string; Options: TmnTidyWriterOptions);
begin
  if (woCloseIndent in Options) and not (woOpenIndent in Options) then
    Dec(Level);

  if not Compact then
  begin
    if (NewLine) then
      S := LevelStr(Level) + S;
  end;

  NewLine := False;

  if (woEndLine in Options) then
  begin
    NewLine := True;
    if not Compact then
    begin
      s := S + sWinEndOfLine;
    end;
  end;

  FStream.WriteUtf8String(S);

  if (woOpenIndent in Options) and not (woCloseIndent in Options) then
    Inc(Level);
end;

procedure TmnTidyWriter.WriteBOM;
const
  sLEBom: WORD = $FEFF;
begin
  Stream.WriteBuffer(sLEBom, SizeOf(sLEBom))
end;

procedure TmnTidyWriter.WriteLn(const S: string; Options: TmnTidyWriterOptions);
begin
  Write(S, Options + [woEndLine]);
end;

procedure TmnTidyWriter.WriteLine(const S: string; Options: TmnTidyWriterOptions);
begin
  WriteLn(S, Options);
end;

procedure TmnTidyWriter.WriteLines(const S: string; Options: TmnTidyWriterOptions);
var
  I, Start: Integer;
  Line: string;
begin
  if S = '' then
  begin
    WriteLn('', Options);
    Exit;
  end;

  I := 1;
  Start := 1;
  while I <= Length(S) do
  begin
    if S[I] = #13 then
    begin
      Line := Copy(S, Start, I - Start);
      WriteLn(Line, Options);
      Inc(I);
      if (I <= Length(S)) and (S[I] = #10) then
        Inc(I);
      Start := I;
    end
    else if S[I] = #10 then
    begin
      Line := Copy(S, Start, I - Start);
      WriteLn(Line, Options);
      Inc(I);
      Start := I;
    end
    else
      Inc(I);
  end;

  if Start <= Length(S) then
  begin
    Line := Copy(S, Start, Length(S) - Start + 1);
    WriteLn(Line, Options);
  end;
end;

function TmnTidyWriter.WriteStream(AStream: TStream; Count: TFileSize): TFileSize;
begin
  Result := Stream.WriteStream(AStream, Count);
end;

{ TmnwXML_TidyWriterHelper }

procedure TmnwXML_TidyWriterHelper.OpenTag(const Tag: string);
begin
  WriteLn('<'+Tag+'>', [woOpenIndent])
end;

procedure TmnwXML_TidyWriterHelper.OpenTag(const TagName, TagAttributes: string; TagText: string);
begin
  WriteLn('<'+TagName + SpaceIf(TagAttributes) + '>' + TagText, [woOpenIndent])
end;

procedure TmnwXML_TidyWriterHelper.OpenTagA(const TagName, Classes: string; Attributes, TagText: string);
begin
  WriteLn('<'+TagName + When(Classes <> '', 'class="' + Classes + '"') + SpaceIf(Attributes) + '>' + TagText, [woOpenIndent])
end;

procedure TmnwXML_TidyWriterHelper.ReadFromFile(FileName: string);
var
  stream: TmnBufferStream;
  s: UTF8String;
begin
  stream := TmnWrapperStream.Create(TFileStream.Create(FileName, fmShareDenyWrite or fmOpenRead), True);
  try
    while not (cloRead in stream.State) do
    begin
        if stream.ReadUTF8Line(s) then
        begin
          WriteLn(UTF8ToString(s));
        end;
    end;
  finally
    stream.Free;
  end;
end;

procedure TmnwXML_TidyWriterHelper.OpenInlineTag(const TagName: string; TagAttributes: string; TagText: string);
begin
  Write('<'+TagName + SpaceIf(TagAttributes) + '>' + TagText, [woOpenIndent])
end;

procedure TmnwXML_TidyWriterHelper.CloseTag(const Tag: string; TrailText: string);
begin
  WriteLn(TrailText + '</'+Tag+'>', [woCloseIndent])
end;

procedure TmnwXML_TidyWriterHelper.AddShortTag(const TagName: string; TagAttributes: string);
begin
  WriteLn('<'+TagName + SpaceIf(TagAttributes) + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwXML_TidyWriterHelper.AddSpace;
begin
  Write('&nbsp;');
end;

procedure TmnwXML_TidyWriterHelper.AddComment(const Comment: string);
begin
  WriteLn('<!--' + Comment + '-->', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwXML_TidyWriterHelper.AddEmbedScript(const Text: string; Defer: Boolean = True);
var
  s: string;
begin
  s := '';
  if Defer then
    s := s + ' defer';  
  OpenTag('script' + s);
  WriteLines(Text);
  CloseTag('script');
end;

procedure TmnwXML_TidyWriterHelper.AddEmbedStyle(const Text: string);
begin
  OpenTag('style');
  WriteLines(Text);
  CloseTag('style');
end;

procedure TmnwXML_TidyWriterHelper.AddLinkStyle(const src: string; Integrity: string; Cross: Boolean);
var
  s: string;
begin
  s := '';
  if Integrity <> '' then
    s := s + ' integrity="' + Integrity + '"';
  if Cross then
    s :=s + ' crossorigin="anonymous"';
  AddShortTag('link', 'rel="stylesheet" href="' + src + '"' + s);
end;

procedure TmnwXML_TidyWriterHelper.AddLinkScript(const src: string; Integrity: string; Defer: Boolean; Cross: Boolean);
var
  s: string;
begin
  s := '';
  if Integrity <> '' then
    s := s + ' integrity="' + Integrity + '"';
  if Cross then
    s :=s + ' crossorigin="anonymous"';
  if Defer then
    s := s + ' defer';  
  AddTag('script', 'src="' + src + '" ' + s);
end;

procedure TmnwXML_TidyWriterHelper.AddInlineShortTag(const TagName: string; TagAttributes: string);
begin
  Write('<'+TagName + SpaceIf(TagAttributes) + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwXML_TidyWriterHelper.AddTag(const TagName, TagAttributes: string);
begin
  WriteLn('<'+TagName + SpaceIf(TagAttributes) + '></' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwXML_TidyWriterHelper.AddTag(const TagName, TagAttributes, Value: string);
begin
  WriteLn('<'+TagName + SpaceIf(TagAttributes) + '>' + Value + '</' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwXML_TidyWriterHelper.AddInlineTag(const TagName, TagAttributes, Value: string);
begin
  Write('<'+TagName + SpaceIf(TagAttributes) + '>' + Value + '</' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

{ TDON_Object.TPairsEnumerator }

constructor TDON_Object.TPairsEnumerator.Create(AList: TDON_Pairs);
begin
  inherited Create;
  FList := Alist;
  FIndex := -1;
end;

function TDON_Object.TPairsEnumerator.GetCurrent: TDON_Pair;
begin
  Result := FList[FIndex];
end;

function TDON_Object.TPairsEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

initialization
end.
