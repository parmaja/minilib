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
    function Have(AValue: String; vSeparators: TSysCharSet = [';']): Boolean;
    function CreateSubValues(vSeparators: TSysCharSet = [';']): TStringList;
    function SubValue(const Key: string): string;
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
    while Strings.Connected do
    begin
      Strings.ReadUTF8Line(Line);
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
    while not (cloRead in Stream.State) do
    begin
      Stream.ReadUTF8Line(line);
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
    Stream.WriteUTF8Line(f.AsString);
end;

{ TmnFieldHelper }

function TmnFieldHelper.CreateSubValues(vSeparators: TSysCharSet): TStringList;
begin
  Result := TStringList.Create;
  StrToStrings(AsString, Result, vSeparators, [' ']);
end;

function TmnFieldHelper.SubValue(const Key: string): string;
begin
  GetSubValue(AsString, Key, Result);
end;

function TmnFieldHelper.Have(AValue: String; vSeparators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
begin
  if Self = nil then
    Result := False
  else
  begin
    SubValues := TStringList.Create;
    try
      StrToStrings(AsString, SubValues, vSeparators, [' ']);
      Result := SubValues.IndexOf(AValue) >= 0;
    finally
      SubValues.Free;
    end;
  end;
end;

end.
