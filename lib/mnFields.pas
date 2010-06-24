unit mnFields;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}

{$IFDEF fpc}
{$MODE delphi}{$H+}
{.$INTERFACES CORBA}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs;

type
  IField = interface(IStreamPersist)
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const Value: Currency);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  end;

  IFields = interface(IStreamPersist)
    function GetValues(Index: string): Variant;
    property Values[Index: string]: Variant read GetValues; 
  end;

  TmnCustomField = class(TInterfacedObject, IField)
  private
    FName: string;
    function GetAsHex: string;
    procedure SetAsHex(const AValue: string);
    procedure SetAsNullString(const Value: string);
    function GetAsAnsiString: ansistring;
    procedure SetAsAnsiString(const Value: ansistring);
    function GetAsWideString: widestring;
    procedure SetAsWideString(const Value: widestring);
    function GetAsUtf8String: UTF8String;
    procedure SetAsUtf8String(const Value: UTF8String);
  protected
    function GetAsText: string; virtual;
    procedure SetAsText(const AValue: string); virtual;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const Value: Integer); virtual;
    function GetAsInt64: Integer; virtual;
    procedure SetAsInt64(const Value: Integer); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(const Value: Boolean); virtual;
    function GetAsCurrency: Currency; virtual;
    procedure SetAsCurrency(const Value: Currency); virtual;
    function GetAsDate: TDateTime; virtual; //zaher must use trunc
    procedure SetAsDate(const Value: TDateTime); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
    function GetAsTime: TDateTime; virtual;
    procedure SetAsTime(const Value: TDateTime); virtual;
    function GetAsTrimString: string;
    procedure SetAsTrimString(const Value: string);

    property Value: Variant read GetValue write SetValue;
    property AsVariant: Variant read GetValue write SetValue;
    //AsAnsiString: Convert strign to utf8 it is special for Lazarus
    property AsAnsiString: ansistring read GetAsAnsiString write SetAsAnsiString;

    property AsWideString: widestring read GetAsWideString write SetAsWideString;
    property AsUtf8String: Utf8String read GetAsUtf8String write SetAsUtf8String;
    property AsString: string read GetAsString write SetAsString;
    property AsTrimString: string read GetAsTrimString write SetAsTrimString;
    property AsNullString: string read GetAsString write SetAsNullString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Integer read GetAsInt64 write SetAsInt64;
    property AsID: Integer read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsHex: string read GetAsHex write SetAsHex;
    property AsText: string read GetAsText write SetAsText; //binary text blob convert to hex

    function GetIsNull: Boolean;
    function GetIsEmpty: Boolean; virtual;
    function GetText: string; virtual;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    {
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToIStream(Stream: IStreamPersist);
    }

    property Text: string read GetText;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNull: Boolean read GetIsNull;
  public
    procedure Clear;//make value null
    procedure Empty;//make value empty
    property Name: string read FName write FName;
  end;

  TmnCustomFieldClass = class of TmnCustomField;

  { TmnCustomFields }

  TmnCustomFields = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnCustomField;
  public
    property Items[Index: Integer]: TmnCustomField read GetItem;
  end;

  TmnFields = class(TmnCustomFields)
  private
  protected
    function Find(vName: string): TmnCustomField; virtual;
  public
    function Add(AColumn: TmnCustomField): Integer; overload;
    function ByName(vName: string): TmnCustomField;
    function IsExists(vName: string): Boolean;
    procedure Clean; virtual;
  end;

implementation

{ TmnCustomField }

procedure TmnCustomField.Clear;
begin
  Value := Null;
end;

function TmnCustomField.GetAsBoolean: Boolean;
begin
  if IsEmpty then
    Result := False
  else
    Result := AsInteger <> 0;
end;

function TmnCustomField.GetAsCurrency: Currency;
begin
  if IsEmpty or not VarIsNumeric(Value) then
    Result := 0
  else
    Result := Value;
end;

function TmnCustomField.GetAsDate: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := DateOf(Result);
  end;
end;

function TmnCustomField.GetAsDateTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmnCustomField.GetAsInt64: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmnCustomField.GetAsInteger: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Value;
end;

function TmnCustomField.GetAsString: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

function TmnCustomField.GetAsTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
  begin
    Result := Value;
    Result := TimeOf(Result);
  end;
end;

function TmnCustomField.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TmnCustomField.GetIsEmpty: Boolean;
begin
  Result := VarType(Value) in [varEmpty, varNull, varUnknown];
end;

function TmnCustomField.GetIsNull: Boolean;
begin
  Result := IsEmpty;
end;

function TmnCustomField.GetText: string;
begin
  if IsEmpty then
    Result := ''
  else
    Result := Value;
end;

procedure TmnCustomField.LoadFromFile(const FileName: string);
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

procedure TmnCustomField.LoadFromStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

procedure TmnCustomField.SetAsNullString(const Value: string);
begin
  if Value = '' then
    Clear
  else
    AsString := Value;
end;

function TmnCustomField.GetAsHex: string;
var
  s: string;
begin
  s := GetAsString;
  SetLength(Result, Length(s) * 2);
  BinToHex(PChar(s), @Result[1], Length(s));
end;

function TmnCustomField.GetAsText: string;
begin
  Result := AsString;
end;

procedure TmnCustomField.SetAsHex(const AValue: string);
var
  s: string;
begin
  SetLength(s, Length(Value) div 2);
  HexToBin(PChar(AValue), @s[1], Length(s));
  AsString := s;
end;

procedure TmnCustomField.SetAsBoolean(const Value: Boolean);
begin
  AsInteger := Ord(Value);
end;

procedure TmnCustomField.SetAsCurrency(const Value: Currency);
begin
  Self.Value := Value;
end;

procedure TmnCustomField.SetAsDate(const Value: TDateTime);
begin
  Self.Value := DateOf(Value);
end;

procedure TmnCustomField.SetAsDateTime(const Value: TDateTime);
begin
  Self.Value := Value;
end;

procedure TmnCustomField.SetAsInt64(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmnCustomField.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TmnCustomField.SetAsString(const Value: string);
begin
  Self.Value := Value;
end;

procedure TmnCustomField.Empty;
begin
  Value := Unassigned;
end;

function TmnCustomField.GetAsAnsiString: ansistring;
begin
  Result := Utf8ToAnsi(GetAsString);
end;

procedure TmnCustomField.SaveToFile(const FileName: string);
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

procedure TmnCustomField.SaveToStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

procedure TmnCustomField.SetAsAnsiString(const Value: ansistring);
begin
  //fpc not auto convert because string type it same with ansistring
  SetAsString(AnsiToUtf8(Value));
end;

function TmnCustomField.GetAsWideString: widestring;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmnCustomField.SetAsText(const AValue: string);
begin
  AsString := AValue;
end;

procedure TmnCustomField.SetAsWideString(const Value: widestring);
begin
  SetAsString(Value);
end;

function TmnCustomField.GetAsUtf8String: UTF8String;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmnCustomField.SetAsUtf8String(const Value: UTF8String);
begin
  SetAsString(Value);
end;

procedure TmnCustomField.SetAsTime(const Value: TDateTime);
begin
  Self.Value := TimeOf(Value);
end;

procedure TmnCustomField.SetAsTrimString(const Value: string);
begin
  AsString := Trim(Value);
end;

{ TmnFields }

function TmnCustomFields.GetItem(Index: Integer): TmnCustomField;
begin
  Result := (inherited Items[Index]) as TmnCustomField;
end;

function TmnFields.Add(AColumn: TmnCustomField): Integer;
begin
  Result := inherited Add(AColumn);
end;

function TmnFields.ByName(vName: string): TmnCustomField;
begin
  Result := Find(vName);
  if Result = nil then
    raise Exception.Create('Field "' + vName + '" not found');
end;

function TmnFields.IsExists(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

procedure TmnFields.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Clear;
  end;
end;

function TmnFields.Find(vName: string): TmnCustomField;
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

end.

