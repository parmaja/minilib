unit mnParams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$IFDEF fpc}
{$MODE delphi}{$H+}
{$ELSE}
{$DEFINE WINDOWS}
{$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs, StrUtils,
  mnUtils, mnStreams, mnClasses, mnFields;

type

  { TmnParam }

  TmnParam = class(TmnField)
  public
  end;

  { TmnParams }

  TmnParams = class(TmnFields)
  private
    FAutoRemove: Boolean;
    FSeparator: string;
    FDelimiter: Char;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    function SetValue(const Index: string; const AValue: Variant): TmnField; override;
    function CreateField: TmnField; override;
  public
    constructor Create;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function ReadInteger(Name: string; Def: Integer = 0): Integer;
    function ReadString(Name: string; Def: String = ''): String;
    function ReadBool(Name: string; Def: Boolean = False): Boolean;
    function RequireField(const vName: string): TmnField; //find it if not exists create it

    //AutoRemove remove field if Value = '' when use Values or SetValues
    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    property Separator: string read FSeparator write FSeparator; //value
    property Delimiter: Char read FDelimiter write FDelimiter; //eol
    property AsString: string read GetAsString write SetAsString;
    property Require[const Index: string]: TmnField read RequireField;

    property Field; default;
  end;

  TmnSection = class(TmnParams)
  public
    Name: string;
  end;

  TmnConfig = class(TmnObjectList<TmnSection>)
  private
    FDefault: TmnSection;
    function GetParams(Index: string): TmnSection;
  public
    constructor Create;
    destructor Destroy; override;


    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Find(const Name: string): TmnSection;
    function ReadInteger(Section, Name: string; Def: Integer; InheriteIt: Boolean = False): Integer; overload;
    function ReadString(Section, Name: string; Def: String; InheriteIt: Boolean = False): String; overload;
    function ReadBool(Section, Name: string; Def: Boolean; InheriteIt: Boolean = False): Boolean; overload;
    function ReadBoolean(Section, Name: string; Def: Boolean; InheriteIt: Boolean = False): Boolean; overload; deprecated;

    function ReadInteger(Name: string; Def: Integer = 0): Integer; overload;
    function ReadString(Name: string; Def: String = ''): String; overload;
    function ReadBool(Name: string; Def: Boolean = False): Boolean; overload;
    function ReadBoolean(Name: string; Def: Boolean = False): Boolean; overload; deprecated;

    property Params[Index: string]: TmnSection read GetParams; default;
    property Default: TmnSection read FDefault;
  end;

  { TmnHeaderField }

  TmnHeaderField = class(TmnParam)
    function GetFullString: String; override;
  end;

  { TmnHeader }

  TmnHeader = class(TmnParams)
  private
    function GetValues(const vName: string): string;
    procedure SetValues(const vName, Value: string);
  protected
    function CreateField: TmnField; override;
  public
    constructor Create;
    procedure ReadHeader(Stream: TmnBufferStream);
    procedure WriteHeader(Stream: TmnBufferStream);
    property Values[const vName: string]: string read GetValues write SetValues; default;
  end;

  { TmnFieldHelper }

  TmnFieldHelper = class helper for TmnField
  public
    function Have(AValue: String; vSeperators: TSysCharSet = [';']): Boolean;
    function CreateSubValues(vSeperators: TSysCharSet = [';']): TStringList;
  end;

procedure FieldsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);

implementation

{ TmnParams }

function TmnParams.GetAsString: string;
var
  item: TmnField;
begin
  Result := '';
  for item in Self do
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + Item.Name + Separator + Item.AsString;
  end;
end;

function TmnParams.SetValue(const Index: string; const AValue: Variant): TmnField;
begin
  if AutoRemove and VarIsEmpty(AValue) then
  begin
    Result := nil;
    RemoveByName(Index);
  end
  else
    Result := inherited SetValue(Index, AValue);
end;

function TmnParams.CreateField: TmnField;
begin
  Result := TmnParam.Create;
end;

procedure TmnParams.LoadFromStream(Stream: TStream);
var
  Strings: TmnWrapperStream;
  Line: string;
begin
  Strings := TmnWrapperStream.Create(Stream, False);
  try
    Clear;
    while not Strings.EndOfStream do
    begin
      Line := Strings.ReadLineUTF8;
      AddItem(Line, Separator, True);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TmnParams.SaveToStream(Stream: TStream);
var
  Strings: TmnWrapperStream;
  i: Integer;
begin
  Strings := TmnWrapperStream.Create(Stream);
  try
    for i := 0 to Count - 1 do
      Strings.WriteLine(Self.Items[i].FullString);
  finally
    Strings.Free;
  end;
end;

procedure FieldsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
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
  (TObject(Sender) as TmnFields).Add(Name, Value); //params inherite fields
end;

procedure TmnParams.SetAsString(const Value: string);
begin
  StrToStringsCallback(Value, Self, @FieldsCallBack, [Self.Delimiter], [' ']);
end;

constructor TmnParams.Create;
begin
  inherited Create;
  Separator := '=';
  Delimiter := #13;
  AutoRemove := False;
end;

function TmnParams.ReadInteger(Name: string; Def: Integer): Integer;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsInteger
  else
    Result := Def;
end;

function TmnParams.ReadString(Name: string; Def: String): String;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsString
  else
    Result := Def;
end;

function TmnParams.ReadBool(Name: string; Def: Boolean): Boolean;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsBoolean
  else
    Result := Def;
end;

function TmnParams.RequireField(const vName: string): TmnField;
begin
  Result := FindField(vName);
  if Result = nil then
  begin
    Result := CreateField;
    Result.Name := vName;
    Add(Result);
  end;
end;

{ TmnSectionParams }

constructor TmnConfig.Create;
begin
  inherited Create;
  FDefault := TmnSection.Create;
  FDefault.Name := ''; //ok i know it is already empty
end;

destructor TmnConfig.Destroy;
begin
  FreeAndNil(FDefault);
  inherited;
end;

function TmnConfig.Find(const Name: string): TmnSection;
var
  i: Integer;
begin
  Result := nil;
  if Name = '' then
    Result := FDefault
  else
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnConfig.GetParams(Index: string): TmnSection;
begin
  Result := Find(Index);
end;

procedure TmnConfig.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TmnConfig.LoadFromStream(Stream: TStream);
var
  Strings: TmnWrapperStream;
  aLastSection: TmnSection;
  Line: string;
  procedure AddSection(vName: string);
  begin
    if vName = '' then
      raise Exception.Create('Can not add empty section!');
    aLastSection := Find(vName);
    if aLastSection = nil then
    begin
      aLastSection := TmnSection.Create;
      aLastSection.Name := vName;
      Add(aLastSection);
    end;
  end;
begin
  Clear;
  aLastSection := FDefault;
  Strings := TmnWrapperStream.Create(Stream, False);
  try
    while not Strings.EndOfStream do
    begin
      Line := Trim(Strings.ReadLineUTF8);
      if RightStr(Line, 1) = '.' then
        AddSection(LeftStr(Line, Length(Line) - 1))
      else if LeftStr(Line, 1) = '[' then
      begin
        if RightStr(Line, 1) = ']' then
          AddSection(MidStr(Line, 2, Length(Line) - 2))
        else
          AddSection(MidStr(Line, 1, Length(Line) - 1));
      end
      else
      begin
        aLastSection.AddItem(Line, aLastSection.Separator, True);
      end
    end;
  finally
    Strings.Free;
  end;
end;

function TmnConfig.ReadInteger(Section, Name: string; Def: Integer; InheriteIt: Boolean): Integer;
var
  ASection: TmnSection;
  Field: TmnField;
begin
  ASection := Find(Section);
  if ASection <> nil then
  begin
    Field := ASection.FindField(Name);
    if Field = nil then
    begin
      ASection := Find('');
      Field := ASection.FindField(Name);
    end;
    if Field <> nil then
      Result := Field.AsInteger
    else
      Result := Def;
  end
  else
    Result := Def;
end;

function TmnConfig.ReadString(Section, Name, Def: String; InheriteIt: Boolean): String;
var
  ASection: TmnSection;
  Field: TmnField;
begin
  ASection := Find(Section);
  if (ASection = nil) and InheriteIt then
    ASection := FDefault;

  if (ASection <> nil) then
  begin
    Field := ASection.FindField(Name);
    if Field = nil then
    begin
      ASection := Find('');
      Field := ASection.FindField(Name);
    end;
    if Field <> nil then
      Result := Field.AsString
    else
      Result := Def;
  end
  else
    Result := Def;
end;

function TmnConfig.ReadBool(Section, Name: string; Def: Boolean; InheriteIt: Boolean): Boolean;
var
  ASection: TmnSection;
  Field: TmnField;
begin
  ASection := Find(Section);
  if ASection <> nil then
  begin
    Field := ASection.FindField(Name);
    if Field = nil then
    begin
      ASection := Find('');
      Field := ASection.FindField(Name);
    end;
    if Field <> nil then
      Result := Field.AsBoolean
    else
      Result := Def;
  end
  else
    Result := Def;
end;

function TmnConfig.ReadBoolean(Section, Name: string; Def: Boolean; InheriteIt: Boolean): Boolean;
begin
  Result := ReadBool(Section, Name, Def, InheriteIt);
end;

procedure TmnConfig.SaveToFile(const FileName: string);
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

procedure TmnConfig.SaveToStream(Stream: TStream);
begin
end;

function TmnConfig.ReadBool(Name: string; Def: Boolean): Boolean;
begin
  Result := ReadBool('', Name, Def);
end;

function TmnConfig.ReadBoolean(Name: string; Def: Boolean): Boolean;
begin
  Result := ReadBool('', Name, Def);
end;

function TmnConfig.ReadInteger(Name: string; Def: Integer): Integer;
begin
  Result := ReadInteger('', Name, Def);
end;

function TmnConfig.ReadString(Name, Def: String): String;
begin
  Result := ReadString('', Name, Def);
end;

{ TmnHeaderField }

function TmnHeaderField.GetFullString: String;
begin
  Result := GetNameValue(': ');
end;

{ TmnHeader }

constructor TmnHeader.Create;
begin
  inherited Create;
  AutoRemove := True;
end;

function TmnHeader.CreateField: TmnField;
begin
  Result := TmnHeaderField.Create;
end;

function TmnHeader.GetValues(const vName: string): string;
begin
  Result := Field[vName].AsString;
end;

procedure TmnHeader.ReadHeader(Stream: TmnBufferStream);
var
  line: String;
begin
  if Stream <> nil then
  begin
    while not (cloRead in Stream.Done) do
    begin
      line := UTF8ToString(Stream.ReadLineUTF8);
      if line = '' then
        break
      else
      begin
        AddItem(line, ':', True);
      end;
    end;
  end;
end;

procedure TmnHeader.SetValues(const vName, Value: string);
begin
  SetValue(vName, Value);
end;

procedure TmnHeader.WriteHeader(Stream: TmnBufferStream);
var
  f: TmnField;
begin
  for f in Self do
    Stream.WriteLineUTF8(f.AsString);
end;

{ TmnFieldHelper }

function TmnFieldHelper.CreateSubValues(vSeperators: TSysCharSet): TStringList;
begin
  Result := TStringList.Create;
  StrToStrings(AsString, Result, vSeperators, [' ']);
end;

function TmnFieldHelper.Have(AValue: String; vSeperators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
begin
  if Self = nil then
    Result := False
  else
  begin
    SubValues := TStringList.Create;
    try
      StrToStrings(AsString, SubValues, vSeperators, [' ']);
      Result := SubValues.IndexOf(AValue) >= 0;
    finally
      SubValues.Free;
    end;
  end;
end;

end.
