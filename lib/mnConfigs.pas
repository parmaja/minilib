unit mnConfigs;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   MIT
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @version   1.0
 *}

{$ifdef FPC}
{$mode delphi}{$H+}
{$ModeSwitch ArrayOperators}
{$endif}

{$define minilib}

interface

uses
  Classes, SysUtils, DateUtils, Variants, StrUtils,
  {$ifdef FPC}
  StreamEx,
  {$else}
  Windows,
  {$endif}
  mnUtils,
  {$ifdef minilib}
  mnClasses,
  {$else}
  fgl,
  {$endif}
  Types;

const
{$ifdef MSWINDOWS}
  cOSName = 'Windows';
{$else}
  cOSName = 'Linux';
  {$endif}

type
  TConfOption = (
    coUseSubnames,
    coInherite, //If field not exists get the value from parent
    coReturnFirst //return first item in section if item name is empty
  );
  TConfOptions = set of TConfOption;

const
  cTabSize = 4;
  sDefaultOptions = [coUseSubnames, coInherite];

type

  TConfMergeOption = (
    moWithSections,
    moOverwrite //* same as NoDublicate
  );

  TConfMergeOptions = set of TConfMergeOption;

  TConfMergeOptionsHelper = record helper for TConfMergeOptions
    function Overwrite: boolean;
    function WithSections: boolean;
  end;

  { TConfValue }

  TConfValue = class(TObject)
  private
    FValue: string;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);
    function GetAsInteger: Integer;
    procedure SetAsInteger(AValue: Integer);
    function GetAsInt64: Int64;
    procedure SetAsInt64(AValue: Int64);
  public
    property Value: string read FValue write FValue;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: String read FValue write FValue;
  end;

  { TConfField }

  TConfField = class(TConfValue)
  private
    FIsComment: Boolean;
    FName: string;
  public
    function FullString(Separator: string = '='): String; virtual;
    property IsComment: Boolean read FIsComment;
    property Name: string read FName write FName;
  end;

  TConfSection = class;

  { TConfSections }

  {$ifdef minilib}
  TConfSections = class(TmnObjectList<TConfSection>)
  {$else}
  TConfSections = class(TFPGObjectList<TConfSection>)
  {$endif}
  private
  protected
    FSection: TConfSection; //Owner
    property Section: TConfSection read FSection;
  public
    function Find(const AName: string; Def: TConfSection): TConfSection; overload;
    function Find(const AName: string): TConfSection; overload;
    function Require(const AName: string): TConfSection;
    //FallbackSection, //Only with coInherite, if not section not found in self or parent, use self
    function FindSection(ASectionName: string; AOptions: TConfOptions; FallbackSection: Boolean = False): TConfSection; overload;
    function FindSection(ASections: TArray<string>; FieldName: string; AOptions: TConfOptions; FallbackSection: Boolean = False): TConfSection; overload;

    function NewSection(const AName, ASectionType: string; AAttributes: string): TConfSection;

    function IndexOf(const AName: string): Integer;
    function ReadString(ASections: TArray<string>; AName:string; Def: String = ''; AOptions: TConfOptions = sDefaultOptions): String; overload;
    function ReadString(ASectionName, AName:string; Def: String = ''; AOptions: TConfOptions = sDefaultOptions): String; overload;
    function ReadInteger(ASectionName, AName: string; Def: Integer = 0; AOptions: TConfOptions = sDefaultOptions): Integer;
    function ReadInt64(ASectionName, AName: string; Def: Int64 = 0; AOptions: TConfOptions = sDefaultOptions): Int64; overload;
    function ReadInt64(ASections: TArray<string>; AName:string; Def: Int64 = 0; AOptions: TConfOptions = sDefaultOptions): Int64; overload;
    function ReadBool(ASectionName, AName: string; Def: Boolean = False; AOptions: TConfOptions = sDefaultOptions): Boolean;
    function ReadSwitch(ASectionName, AName: string; Def: Boolean = False; AOptions: TConfOptions = sDefaultOptions): Boolean;
    function ReadAttribute(ASectionName, AAttributeName: string): Boolean; overload;
    procedure ReadStrings(ToStrings: TStrings; SectionName: string; ValuesOnly: Boolean = True; AllowDuplicate: Boolean = False); overload;
    function ReadStrings(FromSection: string; ValuesOnly: Boolean = True; AllowDuplicate: Boolean = False): TStringList; overload;
    procedure ReadSection(ToSection: TConfSection; SectionName: string); overload;
    procedure ReadSection(FromSection: string; ToSection: string); overload;
    procedure DeleteSection(SectionName: string);
    property Item[const AName: string]: TConfSection read Find; default;
    constructor Create(AOwner: TConfSection);
  end;

  { TConfWriter }

  TConfWriter = class(TObject)
  private
    FCount: Integer;
  protected
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure WriteEndOfLine;
    procedure Write(S: string; Level: Integer); virtual;
    procedure WriteLine(S: string; Level: Integer);
    procedure Flush;
    property Count: Integer read FCount;
  end;

  { TConfSection }

  {$ifdef minilib}
  TConfSection = class(TmnObjectList<TConfField>)
  {$else}
  TConfSection = class(TFPGObjectList<TConfField>)
  {$endif}
  private
    FAttributes: TStringList;
    FAutoRemove: Boolean;
    FSeparator: string;
    FDelimiter: Char;
    FParent: TConfSection;
    FSections: TConfSections;
    function GetAsString: string;
    function GetValues(Index: string): string;
    procedure SetAsString(const Value: string);
    procedure SetValues(Index: string; AValue: string);
  protected
    function SetValue(const Index: string; const AValue: Variant): TConfField;
    function CreateField: TConfField; virtual;
    function ReplaceVariable(S: string): string; virtual;
  public
    Name: string;
    SectionType: string;
    constructor Create(AParent: TConfSection); virtual;
    destructor Destroy; override;
    function FindField(Name: String; AOptions: TConfOptions = []): TConfField; overload;
    function FindValue(AValue: String; AOptions: TConfOptions = []): TConfField; overload;
    function RequireField(const vName: string): TConfField; //find it if not exists create it
    function Add(AName, AValue: string): TConfField; overload;
    function AddItem(S: string; Separator: string; TrimIt: Boolean = False): TConfField; overload;
    function AddComment(S: string): TConfField; overload;
    function Find(const vName: string): TConfField; virtual; //no exception
    function Exists(const vName: string): Boolean;
    function EnumSectionList(ASectionName: string; AllowDuplicate:Boolean; AList: TStringList): Boolean; overload;
    function EnumSectionList(ASection: TConfSection; AllowDuplicate:Boolean; AList: TStringList): Boolean; overload;
    //function FindSection(vInSection, vSectionName: string; vOptions: TConfOptions = []): TConfSection; overload; deprecated;
    function IndexOfName(const vName: string): Integer;
    function RemoveByName(const vName: string): Boolean;

    function ReadString(AName: string; Def: String = ''; AOptions: TConfOptions = sDefaultOptions): String; overload;
    function ReadInteger(AName: string; Def: Integer = 0; AOptions: TConfOptions = sDefaultOptions): Integer; overload;
    function ReadInt64(AName: string; Def: Int64 = 0; AOptions: TConfOptions = sDefaultOptions): Int64; overload;
    //Read bool empty string return false, if not exists return default
    function ReadBool(AName: string; Def: Boolean = False; AOptions: TConfOptions = sDefaultOptions): Boolean; overload;
    //Read bool but if have no value (empty string) it return true
    function ReadSwitch(AName: string; Def: Boolean = False; AOptions: TConfOptions = sDefaultOptions): Boolean; overload;
    //Load it into strings without clear it
    procedure ReadStrings(AStrings: TStrings; ValuesOnly: Boolean = True; AllowDuplicate: Boolean = False); overload;
    procedure ReadStrings(AStrings: TStrings; AName: string; ValuesOnly: Boolean = True; AllowDuplicate: Boolean = False); overload;

    procedure ReadSection(ToSection: TConfSection); overload;

    procedure WriteString(AName: string; Value: String; DeleteIfEmpty: Boolean = False; Overwrite: Boolean = True);
    procedure WriteInteger(AName: string; Value: Integer);
    procedure WriteInt64(AName: string; Value: Int64);
    procedure WriteBool(AName: string; Value: Boolean);

    procedure Merge(FromSection: TConfSection; ToSectionName: string; MergeOptions:TConfMergeOptions = [moWithSections, moOverwrite]); overload;
    procedure Merge(FromSection: TConfSection; MergeOptions:TConfMergeOptions = [moWithSections, moOverwrite]); overload;
    procedure Merge(FromSection: string; ToSectionName: string; MergeOptions:TConfMergeOptions = [moWithSections, moOverwrite]); overload;
    function Clone(Parented: Boolean; SectionName: string = ''): TConfSection; overload;

    //collect all fields that have no name into one string //TODO with names
    function Collect(Separator: string): string; overload;
    function Collect(SectionName: string; CollectSeparator: string): string; overload;

    procedure WriteTo(Writer: TConfWriter; Level: Integer); virtual;

    //it will merge into it, You need to clear before load it
    procedure LoadFromStream(Stream: TStream; IgnoreComments: Boolean = False);
    procedure LoadFromFile(const FileName: string; IgnoreComments: Boolean = False);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);

    //AutoRemove remove field if Value = '' when use Values or SetValues
    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    property Separator: string read FSeparator write FSeparator; //value
    property Delimiter: Char read FDelimiter write FDelimiter; //eol
    property AsString: string read GetAsString write SetAsString;
    property Require[const Index: string]: TConfField read RequireField;
    //Parent can be forign ConfSecion, just linked for inheritance of values
    property Parent: TConfSection read FParent;
    property Sections: TConfSections read FSections;
    property Attributes: TStringList read FAttributes;
    function NewSection(const AName, ASectionType: string; AAttributes: string): TConfSection;
    property Values[Index: string]: string read GetValues write SetValues; default;
  end;

  { TConfFile }

  TConfFile = class(TConfSection)
  private
  protected
  public
    constructor Create(AParent: TConfSection = nil); override;
    destructor Destroy; override;
  end;

procedure ParamsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
function DequoteStr(Str: string; QuoteChar: string = #0): string;
function ConnectStr(const S1, Sep: string; S2: string = ''): string;

implementation

const
  cCommentChars: array of char = ['#', ';'];

function ConnectStr(const S1, Sep: string; S2: string): string;
begin
  Result := S1;
  if (Result <> '') and (S2 <> '') then
    Result := Result + Sep;
  Result := Result + S2;
end;

function DequoteStr(Str: string; QuoteChar: string = #0): string;
begin
  if Str = '' then
    Result := ''
  else
  begin
    if (QuoteChar > #0) and (Str[1] = QuoteChar) then
    begin
      if Str[Length(Str)] =QuoteChar then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else if Str[1] = '"' then
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

{ TConfWriter }

constructor TConfWriter.Create(AStream: TStream);
begin
  inherited Create;
  if AStream = nil then
    raise Exception.Create('Stream is nil!');
  FStream := AStream;
end;

destructor TConfWriter.Destroy;
begin
  inherited;
end;

procedure TConfWriter.Write(S: string; Level: Integer);
var
  u: Utf8String;
begin
  u := StringOfChar(' ', Level * cTabSize) + S;
  FStream.Write(u[1], Length(u));
end;

procedure TConfWriter.WriteLine(S: string; Level: Integer);
begin
  Write(S, Level);
  WriteEndOfLine;
end;

procedure TConfWriter.Flush;
begin

end;

procedure TConfWriter.WriteEndOfLine;
var
  S: UTF8String;
begin
  S := #13#10;
  FStream.Write(S[1], Length(S));
  FCount := FCount + 1;
end;

{ TConfField }

function TConfField.FullString(Separator: string): String;
begin
  if Name = '' then
  begin
    if IsComment then
      Result := Value
    else if Pos(Separator, Value) > 0 then
      Result := '"' + Value + '"'
    else
      Result := Value;
  end
  else
    Result := Name + Separator + Value;
end;

{ TConfSection }

function TConfSection.NewSection(const AName, ASectionType: string; AAttributes: string): TConfSection;
begin
  Result := Sections.NewSection(AName, ASectionType, AAttributes);
end;

{ TConfValue }

function TConfValue.GetAsBoolean: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

procedure TConfValue.SetAsBoolean(AValue: Boolean);
begin
  Value := BoolToStr(AValue);
end;

function TConfValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Value, 0);
end;

procedure TConfValue.SetAsInteger(AValue: Integer);
begin
  Value := IntToStr(AValue);
end;

function TConfValue.GetAsInt64: Int64;
begin
  Result := StrToIntDef(Value, 0);
end;

procedure TConfValue.SetAsInt64(AValue: Int64);
begin
  Value := IntToStr(AValue);
end;

{ TConfSections }

function TConfSections.FindSection(ASectionName: string; AOptions: TConfOptions; FallbackSection: Boolean): TConfSection;
begin
  Result := FindSection([ASectionName], '', AOptions, FallbackSection);
end;

function TConfSections.FindSection(ASections: TArray<string>; FieldName: string; AOptions: TConfOptions; FallbackSection: Boolean): TConfSection;
  function FindNow(ASection: TConfSection): TConfSection;
  var
    ASectionName: string;
  begin
    Result := ASection;
    for ASectionName in ASections do
    begin
      Result := Result.Sections.Find(ASectionName);
      if Result = nil then
        break;
    end;
    if (Result <> nil) and (FieldName <> '') then
      if not Result.Exists(FieldName) then
        Result := nil;
  end;
begin
  Result := FindNow(Section);

  if (Result = nil) and (coInherite in AOptions) and (Section <> nil) and (Section.Parent <> nil) then
    Result := Section.Parent.Sections.FindSection(ASections, FieldName, AOptions); //do not fallback there

  if (Result = nil) and FallbackSection then
    Result := Section;
end;

function TConfSections.Require(const AName: string): TConfSection;
begin
  Result := Find(AName);
  if Result = nil then
    Result := NewSection(AName, '', '');
end;

function TConfSections.NewSection(const AName, ASectionType: string; AAttributes: string): TConfSection;
begin
  Result := TConfSection.Create(FSection);
  Result.Name := AName;
  Result.SectionType := ASectionType;
  Result.Attributes.Delimitedtext := AAttributes;
  Add(Result);
end;

function TConfSections.Find(const AName: string; Def: TConfSection): TConfSection;
var
  i: Integer;
begin
  Result := Def;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TConfSections.Find(const AName: string): TConfSection;
begin
  Result := Find(AName, nil);
end;

function TConfSections.IndexOf(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TConfSections.ReadString(ASections: TArray<string>; AName: string; Def: String; AOptions: TConfOptions): String;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASections, AName, AOptions, True); //Find it here or in parents
  if (ASection <> nil) then
  begin
    Result := ASection.ReadString(AName, Def, AOptions);
    if Result = '' then //not sure
      Result := Def;
  end
  else
    Result := Def;
end;

procedure TConfSections.ReadSection(FromSection: string; ToSection: string);
var
  AToSection: TConfSection;
begin
  AToSection := Require(ToSection);
  ReadSection(AToSection, FromSection);
end;

function TConfSections.ReadString(ASectionName, AName: string; Def: String; AOptions: TConfOptions): String;
var
  ASection: TConfSection;
begin
  ASection := FindSection([ASectionName], AName, AOptions, True); //Find it here or in parents
  if (ASection <> nil) then
  begin
    Result := ASection.ReadString(AName, Def, AOptions);
    if Result = '' then //not sure
      Result := Def;
  end
  else
    Result := Def;
end;

function TConfSections.ReadStrings(FromSection: string; ValuesOnly, AllowDuplicate: Boolean): TStringList;
begin
  Result := TStringList.Create;
  ReadStrings(Result, FromSection, ValuesOnly, AllowDuplicate);
end;

function TConfSections.ReadBool(ASectionName, AName: string; Def: Boolean; AOptions: TConfOptions): Boolean;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASectionName, AOptions, True);

  if (ASection <> nil) then
    Result := ASection.ReadBool(AName, Def, AOptions)
  else
    Result := Def;
end;

function TConfSections.ReadAttribute(ASectionName, AAttributeName: string): Boolean;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASectionName, [], False);

  if ASection <> nil then
    Result := ASection.Attributes.IndexOf(AAttributeName) >= 0
  else
    Result := False;
end;

function TConfSections.ReadInteger(ASectionName, AName: string; Def: Integer; AOptions: TConfOptions): Integer;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASectionName, AOptions, True);

  if (ASection <> nil) then
    Result := ASection.ReadInteger(AName, Def, AOptions)
  else
    Result := Def;
end;

function TConfSections.ReadInt64(ASectionName, AName: string; Def: Int64; AOptions: TConfOptions): Int64;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASectionName, AOptions, True);

  if (ASection <> nil) then
    Result := ASection.ReadInt64(AName, Def, AOptions)
  else
    Result := Def;
end;

function TConfSections.ReadInt64(ASections: TArray<string>; AName:string; Def: Int64; AOptions: TConfOptions): Int64;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASections, AName, AOptions, True);

  if (ASection <> nil) then
    Result := ASection.ReadInt64(AName, Def, AOptions)
  else
    Result := Def;
end;

function TConfSections.ReadSwitch(ASectionName, AName: string; Def: Boolean; AOptions: TConfOptions): Boolean;
var
  ASection: TConfSection;
begin
  ASection := FindSection(ASectionName, AOptions, True);

  if (ASection <> nil) then
    Result := ASection.ReadSwitch(AName, Def, AOptions)
  else
    Result := Def;
end;

procedure TConfSections.ReadStrings(ToStrings: TStrings; SectionName: string; ValuesOnly: Boolean; AllowDuplicate: Boolean);
var
  ASection: TConfSection;
begin
  if SectionName = '' then
    ASection := Section
  else
    ASection := Find(SectionName);

  if (ASection <> nil) then
    ASection.ReadStrings(ToStrings, ValuesOnly, AllowDuplicate);
end;

procedure TConfSections.ReadSection(ToSection: TConfSection; SectionName: string);
var
  ASection: TConfSection;
begin
  if SectionName = '' then
    ASection := Section
  else
    ASection := Find(SectionName);
  if (ASection <> nil) then
    ASection.ReadSection(ToSection);
end;

procedure TConfSections.DeleteSection(SectionName: string);
var
  i: Integer;
begin
  i := IndexOf(SectionName);
  if i >=0 then
    Delete(i);
end;

constructor TConfSections.Create(AOwner: TConfSection);
begin
  inherited Create;
  FSection := AOwner;
end;

{ TConfSection }

function TConfSection.GetAsString: string;
var
  item: TConfField;
begin
  Result := '';
  for item in Self do
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + Item.Name + Separator + ' ' + Item.Value;
  end;
end;

function TConfSection.GetValues(Index: string): string;
begin
  Result := ReadString(Index, '', []);
end;

function TConfSection.SetValue(const Index: string; const AValue: Variant): TConfField;
begin
  if AutoRemove and (AValue = '') then
  begin
    RemoveByName(Index);
    Result := nil;
  end
  else
  begin
    Result := Find(Index);
    if Result = nil then
    begin
      Result := CreateField;
      Result.Name := Index;
      Add(Result);
    end;
    Result.Value := AValue;
  end;
end;

function TConfSection.CreateField: TConfField;
begin
  Result := TConfField.Create;
end;

function TConfSection.FindField(Name: String; AOptions: TConfOptions): TConfField;
begin
  Result := nil;
  if (coUseSubnames in AOptions) then
    Result := Find(Name + '.' + cOSName);
  if Result = nil then
    Result := Find(Name);
  if (Result = nil) and (Parent <> nil) and (coInherite in AOptions) then
    Result := Parent.FindField(Name, AOptions);
end;

function TConfSection.FindValue(AValue: String; AOptions: TConfOptions): TConfField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(AValue, Items[i].Value) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure ParamsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  if p >= 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := DequoteStr(Copy(s, p + 1, MaxInt));
  end
  else
  begin
    Name := S;
    Value := '';
  end;
  (TObject(Sender) as TConfSection).Add(Name, Value);
end;

procedure TConfSection.SetAsString(const Value: string);
begin
  //StrToStringsCallback(Value, Self, @ParamsCallBack, [Self.Delimiter], [' ']);
end;

procedure TConfSection.SetValues(Index: string; AValue: string);
begin
  WriteString(Index, AValue);
end;

constructor TConfSection.Create(AParent: TConfSection);
begin
  inherited Create;
  FParent := AParent;
  FSections := TConfSections.Create(Self);
  FAttributes := TStringList.Create;
  FAttributes.Delimiter := ',';
  FAttributes.StrictDelimiter := True;
  Separator := '=';
  Delimiter := #13;
  AutoRemove := False;
end;

destructor TConfSection.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FSections);
  inherited;
end;

function TConfSection.ReadInteger(AName: string; Def: Integer; AOptions: TConfOptions): Integer;
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  Field := FindField(AName, AOptions);
  if Field <> nil then
    Result := StrToIntDef(Field.Value, Def)
  else
    Result := Def;
end;

function TConfSection.ReadInt64(AName: string; Def: Int64; AOptions: TConfOptions): Int64;
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  Field := FindField(AName, AOptions);
  if Field <> nil then
    Result := StrToInt64Def(Field.Value, Def)
  else
    Result := Def;
end;

function TConfSection.ReadString(AName: string; Def: String; AOptions: TConfOptions): String;
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  Field := FindField(AName, AOptions);
  if (Field <> nil) and (Field.AsString <> '') then
    Result := Field.AsString
  else
    Result := Def;
end;

procedure TConfSection.ReadStrings(AStrings: TStrings; AName: string; ValuesOnly: Boolean; AllowDuplicate: Boolean);
var
  Field: TConfField;
  S: string;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');

  if ValuesOnly then
  begin
    for Field in Self do
      if not Field.IsComment then
        if (AName = '') or SameText(Field.Name, AName) then //sometime we have mutiple lines with same name
        begin
          S := Field.Value;
          if AllowDuplicate or (AStrings.IndexOf(S) < 0) then
            AStrings.Add(S); //TODO do we need inherited and subnames?
        end
  end
  else
  begin
    for Field in Self do
      if not Field.IsComment then
      begin
        S := Field.FullString(Separator);
        if AllowDuplicate or (AStrings.IndexOf(S) < 0) then
          AStrings.Add(S);
      end;
  end;
end;

procedure TConfSection.ReadStrings(AStrings: TStrings; ValuesOnly: Boolean; AllowDuplicate: Boolean);
begin
  ReadStrings(AStrings, '', ValuesOnly, AllowDuplicate);
end;

procedure TConfSection.ReadSection(ToSection: TConfSection);
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  if ToSection <> nil then
    ToSection.Clear;
  for Field in Self do
    ToSection.Add(Field.Name, Field.Value);
  ToSection.Attributes.AddStrings(Attributes);
end;

procedure TConfSection.WriteString(AName: string; Value: String; DeleteIfEmpty: Boolean; Overwrite: Boolean);
var
  Field: TConfField;
begin
  //TODO check if is value dublicated, pass params
  if (AName = '') then
  begin
    if Value <> '' then
    begin
      if not Overwrite or (FindValue(Value) = nil) then
        Add(AName, Value);
    end;
  end
  else
  begin
    Field := FindField(AName, []);
    if (Value = '') and DeleteIfEmpty and (Field <> nil) then
      Remove(Field)
    else
    begin
      if (Field = nil) then
        Add(AName, Value)
      else if Overwrite then
        Field.AsString := Value;
    end;
  end;
end;

procedure TConfSection.WriteInteger(AName: string; Value: Integer);
var
  Field: TConfField;
begin
  Field := RequireField(AName);
  Field.AsInteger := Value;
end;

procedure TConfSection.WriteInt64(AName: string; Value: Int64);
var
  Field: TConfField;
begin
  Field := RequireField(AName);
  Field.AsInt64 := Value;
end;

procedure TConfSection.WriteBool(AName: string; Value: Boolean);
var
  Field: TConfField;
begin
  Field := RequireField(AName);
  Field.AsBoolean := Value;
end;

function TConfSection.ReadBool(AName: string; Def: Boolean; AOptions: TConfOptions): Boolean;
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  Field := FindField(AName, AOptions);
  if Field <> nil then
    Result :=  StrToBoolDef(Field.Value, Def)
  else
    Result := Def;
end;

function TConfSection.ReadSwitch(AName: string; Def: Boolean; AOptions: TConfOptions): Boolean;
var
  Field: TConfField;
begin
  if Self = nil then
    raise Exception.Create('Section is nil, you can read from it');
  Field := FindField(AName, AOptions);
  if (Field <> nil) then
  begin
    if Field.Value <> '' then
      Result :=  StrToBoolDef(Field.Value, Def)
    else
      Result := True;
  end
  else
  begin
    Result := FindValue(AName) <> nil;
  end;
end;

procedure TConfSection.Merge(FromSection: TConfSection; MergeOptions: TConfMergeOptions);
var
  Item: TConfSection;
  aNewSection: TConfSection;
var
  Field: TConfField;
begin
  if Self = FromSection then
    raise Exception.Create('You can''t merge same section (self)');
  if FromSection <> nil then
  begin
    for Field in FromSection do
      WriteString(Field.Name, Field.Value, False, MergeOptions.Overwrite);
    Attributes.AddStrings(FromSection.Attributes);

    if MergeOptions.WithSections then
      for Item in FromSection.Sections do
      begin
        aNewSection := Sections.Find(Item.Name);
        if aNewSection = nil then
          aNewSection := NewSection(Item.Name, Item.SectionType, '');
        aNewSection.Merge(Item, MergeOptions);
      end;
  end;
end;

function TConfSection.Collect(Separator: string): string;
var
  i: Integer;
begin
  Result := '';
  if Self <> nil then
    for i := 0 to Count-1 do
    begin
      if Items[i].Name = '' then
        Result := ConnectStr(Result, Separator, Items[i].Value)
    end;
end;

function TConfSection.RequireField(const vName: string): TConfField;
begin
  Result := Find(vName);
  if Result = nil then
  begin
    Result := CreateField;
    Result.Name := vName;
    Add(Result);
  end;
end;

function TConfSection.Add(AName, AValue: string): TConfField;
begin
  Result := CreateField;
  Result.FValue := AValue;
  Result.FName := AName;
  Add(Result);
end;

function TConfSection.AddItem(S: string; Separator: string; TrimIt: Boolean): TConfField;
var
  p: Integer;
  aName: string;
  aValue: string;
begin
  if ((LeftStr(S, 1) = '''') and (RightStr(S, 1) = ''''))
    or ((LeftStr(S, 1) = '"') and (RightStr(S, 1) = '"')) then
  begin
    aName := '';
    aValue := DequoteStr(S);
  end
  else
  begin
    p := Pos(Separator, S);
    if (p > 0) then
    begin
      aName := Trim(Copy(S, 1, P - 1));
      aValue := Copy(S, P + 1, MaxInt);
    end
    else
    begin
      aName := '';
      aValue := S;
    end;
  end;
  if TrimIt then
    aValue := Trim(aValue);
  Result := Add(aName, aValue);
end;

function TConfSection.AddComment(S: string): TConfField;
begin
  Result := Add('', S);
  Result.FIsComment := True;
end;

function TConfSection.Find(const vName: string): TConfField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TConfSection.Exists(const vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

function TConfSection.EnumSectionList(ASection: TConfSection; AllowDuplicate: Boolean; AList: TStringList): Boolean;
var
  aSubSection: TConfSection;
begin
  if (ASection <> nil) and (ASection.Sections <> nil) then
  begin
    for aSubSection in ASection.Sections do
    begin
      if aSubSection.Name <> '' then
        if AllowDuplicate or (AList.IndexOfName(aSubSection.Name) < 0) then
          AList.Add(aSubSection.Name);
    end;
  end;
  Result := AList.Count > 0;
end;

function TConfSection.EnumSectionList(ASectionName: string; AllowDuplicate: Boolean; AList: TStringList): Boolean;
var
  ASection: TConfSection;
begin
  ASection := Sections.Find(ASectionName);
  if ASection <> nil then
    EnumSectionList(ASection, AllowDuplicate, AList);
  if Parent <> nil then
    Parent.EnumSectionList(ASectionName, AllowDuplicate, AList);
  Result := AList.Count > 0;
end;

function TConfSection.IndexOfName(const vName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TConfSection.RemoveByName(const vName: string): Boolean;
var
  index: Integer;
begin
  index := IndexOfName(vName);
  Result := Index > 0;
  if Result then
    Delete(Index);
end;

procedure TConfSection.Merge(FromSection: TConfSection; ToSectionName: string; MergeOptions:TConfMergeOptions);
var
  ASection: TConfSection;
begin
  if FromSection <> nil then
  begin
    if ToSectionName = '' then
      ASection := Self
    else
      ASection := Sections.Require(ToSectionName);
    if (ASection <> nil) then
      ASection.Merge(FromSection, MergeOptions);
  end;
end;

procedure TConfSection.Merge(FromSection, ToSectionName: string; MergeOptions:TConfMergeOptions);
var
  aSection: TConfSection;
begin
  aSection := Sections.Find(FromSection);
  if aSection <> nil then
    Merge(aSection, ToSectionName, MergeOptions);
end;

function TConfSection.Clone(Parented: Boolean; SectionName: string): TConfSection;
var
  ASection: TConfSection;
  AParent: TConfSection;
begin
  if SectionName = '' then
    ASection := Self
  else
    ASection := Sections.Find(SectionName);
  if (ASection <> nil) then
  begin
    if Parented then
      AParent := ASection.Parent
    else
      AParent := nil;
    Result := TConfSection.Create(AParent);
    ASection.SectionType := ASection.SectionType;
    ASection.ReadSection(Result);
  end
  else
    Result := TConfSection.Create(nil);
end;

function TConfSection.Collect(SectionName: string; CollectSeparator: string): string;
var
  ASection: TConfSection;
begin
  if SectionName = '' then
    ASection := Self
  else
    ASection := Sections.Find(SectionName);
  if (ASection <> nil) then
    Result := ASection.Collect(CollectSeparator)
  else
    Result := '';
end;

procedure TConfSection.WriteTo(Writer: TConfWriter; Level: Integer);
var
  aField: TConfField;
  aSection: TConfSection;
begin
  for aField in Self do
  begin
    Writer.WriteLine(aField.FullString(Separator), Level);
  end;

  for aSection in Sections do
  begin
    if (Writer.Count > 0) then
      Writer.WriteEndOfLine;

    if (aSection.Sections.Count > 0) then
      Writer.WriteLine('[' + aSection.Name, Level)
    else if Level = 0 then
      Writer.WriteLine('[' + aSection.Name + ']', Level)
    else
      Writer.WriteLine('\' + aSection.Name, Level);
    aSection.WriteTo(Writer, Level + 1);
    if (aSection.Sections.Count > 0) then
      Writer.WriteLine(']', Level)
  end;
end;

{ TConfFile }

constructor TConfFile.Create(AParent: TConfSection);
begin
  inherited;
  Name := ''; //ok i know it is already empty
end;

destructor TConfFile.Destroy;
begin
  inherited;
end;

function TConfSection.ReplaceVariable(S: string): string;
begin
  Result := S;//as it
end;

procedure TConfSection.LoadFromFile(const FileName: string; IgnoreComments: Boolean);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, IgnoreComments);
  finally
    Stream.Free;
  end;
end;

procedure TConfSection.SaveToStream(Stream: TStream);
var
  aWriter: TConfWriter;
begin
  aWriter := TConfWriter.Create(Stream);
  try
    WriteTo(aWriter, 0);
    aWriter.Flush;
  finally
    aWriter.Free;
  end;
end;

procedure TConfSection.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TConfSection.LoadFromStream(Stream: TStream; IgnoreComments: Boolean);
var
  aReader: TStreamReader;
  aValueSection: TConfSection;
  Line: string;

  procedure SpliteParams(var vName: string; out vType: string; out vParams: string);
  var
    i: Integer;
  begin
    vName := Trim(vName);
    vParams := '';
    vType := '';
    i := Pos(':', vName);
    if i > 0 then
    begin
      vType := Trim(MidStr(vName, i + 1, MaxInt));
      vName := Trim(MidStr(vName, 1, i - 1));
      i := Pos(' ', vType);
      if i > 0 then
      begin
        vParams := Trim(MidStr(vType, i + 1, MaxInt));
        vType := Trim(MidStr(vType, 1, i - 1));
      end;
    end
    else
    begin
      i := Pos(' ', vName);
      if i > 0 then
      begin
        vParams := Trim(MidStr(vName, i + 1, MaxInt));
        vName := Trim(MidStr(vName, 1, i - 1));
      end
    end;
  end;

  procedure AddTo(ToSection: TConfSection; vName: string);
  var
    aType: string;
    aParams: string;
  begin
    if vName = '' then
      raise Exception.Create('Can not add empty section!');
    SpliteParams(vName, aType, aParams);
    aValueSection := ToSection.Sections.Find(vName);
    if aValueSection = nil then
      aValueSection := ToSection.NewSection(vName, aType, aParams);
  end;

var
  aCurrentSection: TConfSection; //to add new sections not values, values will added to aValueSection
  aSubSection: TConfSection;
  aName: string;
  l: Char;
begin
  //Clear; merge by default
  aCurrentSection := Self;
  aValueSection := Self;
  aSubSection := Self;
  aReader := TStreamReader.Create(Stream);
  try
    {$ifdef FPC}
    while not aReader.Eof do
    {$else}
    while not aReader.EndOfStream do
    {$endif}
    begin
      Line := Trim(aReader.ReadLine);
      if Line <> '' then
      begin
        l := Line[1];
        if CharInArray(l, cCommentChars) then
        begin
          if not IgnoreComments then
            aValueSection.AddItem(Line, aCurrentSection.Separator, True);
        end
        else
        begin
          if (l = '[') and (RightStr(Line, 1) = ']') then //  [section]
          begin
            aName := MidStr(Line, 2, Length(Line) - 2);
            AddTo(aCurrentSection, aName);
            //aCurrentSection := aValueSection; //nop do not that
            aSubSection := aValueSection;
          end
          else if (l = '\') then
          begin
            aName := Trim(MidStr(Line, 2, Length(Line) - 1)); // \section
            AddTo(aSubSection, aName);
            //aCurrentSection := aValueSection; //nop do not that
            //aSubSection := aValueSection;  //nop do not that
          end
          else if (l = '[') then //opensection, because first if take it of close section
          begin
             aName := Trim(MidStr(Line, 2, Length(Line) - 2)); // [opensection]
             AddTo(aCurrentSection, aName);
             aCurrentSection := aValueSection;
             aSubSection := aValueSection;
          end
          else if (l = ']') then //close a last section opened
          begin
            if aCurrentSection.Parent = nil then
              raise Exception.Create('there is ] in wrong place!');
            aCurrentSection := aCurrentSection.Parent;
            aSubSection := aCurrentSection;
          end
          else
          begin
            Line := ReplaceVariable(Line);
            aValueSection.AddItem(Line, aCurrentSection.Separator, True);
          end
        end;
      end;
    end;
  finally
    aReader.Free;
  end;
end;

{ TConfMergeOptionsHelper }

function TConfMergeOptionsHelper.Overwrite: boolean;
begin
  Result := moOverwrite in Self;
end;

function TConfMergeOptionsHelper.WithSections: boolean;
begin
  Result := moWithSections in Self;
end;

end.
