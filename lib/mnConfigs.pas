unit mnConfigs;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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
  mnUtils, mnStreams, mnClasses, mnFields, mnParams;

type
  { TmodParams }

  TmnParams = class(TmnFields)
  private
    FSeparator: string;
    FDelimiter: Char;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetItem(Index: Integer): TmnField;
  public
    constructor Create;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function ReadInteger(Name: string; Def: Integer = 0): Integer;
    function ReadString(Name: string; Def: String = ''): String;
    function ReadBoolean(Name: string; Def: Boolean = False): Boolean;
    property Field; default;
    property Separator: string read FSeparator write FSeparator; //value
    property Delimiter: Char read FDelimiter write FDelimiter; //eol
    property AsString: string read GetAsString write SetAsString;
    property Items[Index: Integer]: TmnField read GetItem;
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

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Find(const Name: string): TmnSection;
    function ReadInteger(Section, Name: string; Def: Integer; InheriteIt: Boolean = False): Integer; overload;
    function ReadString(Section, Name: string; Def: String; InheriteIt: Boolean = False): String; overload;
    function ReadBoolean(Section, Name: string; Def: Boolean; InheriteIt: Boolean = False): Boolean; overload;

    function ReadInteger(Name: string; Def: Integer = 0): Integer; overload;
    function ReadString(Name: string; Def: String = ''): String; overload;
    function ReadBoolean(Name: string; Def: Boolean = False): Boolean; overload;

    property Params[Index: string]: TmnSection read GetParams; default;
    property Default: TmnSection read FDefault;
  end;

procedure ParamsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);

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
    Result := Result + Item.Name + Separator + ' ' + Item.AsString;
  end;
end;

function TmnParams.GetItem(Index: Integer): TmnField;
begin
  Result := (inherited GetItem(Index)) as TmnField;
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
      Line := Strings.ReadLineRawByte;
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
      Strings.WriteLine(Self.Items[i].GetFullString);
  finally
    Strings.Free;
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
  (TObject(Sender) as TmnParams).Add(Name, Value);
end;

procedure TmnParams.SetAsString(const Value: string);
begin
  StrToStringsCallback(Value, Self, @ParamsCallBack, [Self.Delimiter], [' ']);
end;

constructor TmnParams.Create;
begin
  inherited Create;
  Separator := '=';
  Delimiter := #13;
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

function TmnParams.ReadBoolean(Name: string; Def: Boolean): Boolean;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsBoolean
  else
    Result := Def;
end;

{ TmnSectionParams }

constructor TmnConfig.Create;
begin
  inherited Create;
  FDefault := TmnSection.Create;
  FDefault.Name := ''; //ok i know it is already empty
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
      Line := Trim(Strings.ReadLineRawByte);
      if RightStr(Line, 1) = ':' then
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

function TmnConfig.ReadBoolean(Section, Name: string; Def: Boolean; InheriteIt: Boolean): Boolean;
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

function TmnConfig.ReadBoolean(Name: string; Def: Boolean): Boolean;
begin
  Result := ReadBoolean('', Name, Def);
end;

function TmnConfig.ReadInteger(Name: string; Def: Integer): Integer;
begin
  Result := ReadInteger('', Name, Def);
end;

function TmnConfig.ReadString(Name, Def: String): String;
begin
  Result := ReadString('', Name, Def);
end;

end.
