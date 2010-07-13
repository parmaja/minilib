unit mnFields;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}

{$IFDEF fpc}
{$MODE delphi}{$H+}
{.$INTERFACES CORBA}
{$ELSE}
{$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs;

type
  IField = interface(IStreamPersist)
    function GetAsInteger: Integer;
    procedure SetAsInteger(const AValue: Integer);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const AValue: Currency);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const AValue: TDateTime);
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
  end;

  IFields = interface(IStreamPersist)
    function GetValues(Index: string): Variant;
    property Values[Index: string]: Variant read GetValues;
  end;

  TmnCustomField = class(TInterfacedObject, IField)
  private
    procedure CheckIsNil;
    function ReadAsHex: string;
    procedure WriteAsHex(const AValue: string);
    procedure WriteAsNullString(const AValue: string);
    function ReadAsAnsiString: ansistring;
    procedure WriteAsAnsiString(const AValue: ansistring);
    function ReadAsWideString: widestring;
    procedure WriteAsWideString(const AValue: widestring);
    function ReadAsUtf8String: UTF8String;
    procedure WriteAsUtf8String(const AValue: UTF8String);
    function ReadAsTrimString: string;
    procedure WriteAsTrimString(const AValue: string);

    function ReadAsText: string;
    procedure WriteAsText(const AValue: string);
    function ReadAsString: string;
    procedure WriteAsString(const AValue: string);
    function ReadAsInteger: Integer;
    procedure WriteAsInteger(const AValue: Integer);
    function ReadAsInt64: Integer;
    procedure WriteAsInt64(const AValue: Integer);
    function ReadAsBoolean: Boolean;
    procedure WriteAsBoolean(const AValue: Boolean);
    function ReadAsCurrency: Currency;
    procedure WriteAsCurrency(const AValue: Currency);
    function ReadAsDate: TDateTime;  //zaher must use trunc
    procedure WriteAsDate(const AValue: TDateTime);
    function ReadAsDateTime: TDateTime;
    procedure WriteAsDateTime(const AValue: TDateTime);
    function ReadAsTime: TDateTime;
    procedure WriteAsTime(const AValue: TDateTime);
    function ReadIsEmpty: Boolean;
    function ReadIsNull: Boolean;
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;

    function GetAsText: string; virtual;
    procedure SetAsText(const AValue: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const AValue: Integer); virtual;
    function GetAsInt64: Integer; virtual;
    procedure SetAsInt64(const AValue: Integer); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(const AValue: Boolean); virtual;
    function GetAsCurrency: Currency; virtual;
    procedure SetAsCurrency(const AValue: Currency); virtual;
    function GetAsDate: TDateTime; virtual; 
    procedure SetAsDate(const AValue: TDateTime); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const AValue: TDateTime); virtual;
    function GetAsTime: TDateTime; virtual;
    procedure SetAsTime(const AValue: TDateTime); virtual;

    function GetIsNull: Boolean; virtual;   
    function GetIsEmpty: Boolean; virtual;

    property Value: Variant read GetValue write SetValue;
    property AsVariant: Variant read GetValue write SetValue;
    //* AsAnsiString: Convert strign to utf8 it is special for Lazarus
    property AsAnsiString: ansistring read ReadAsAnsiString write WriteAsAnsiString;
    property AsWideString: widestring read ReadAsWideString write WriteAsWideString;
    property AsUtf8String: Utf8String read ReadAsUtf8String write WriteAsUtf8String;
    property AsTrimString: string read ReadAsTrimString write WriteAsTrimString;
    property AsNullString: string read ReadAsString write WriteAsNullString;
    property AsHex: string read ReadAsHex write WriteAsHex;

    property AsString: string read ReadAsString write WriteAsString;
    property AsInteger: Integer read ReadAsInteger write WriteAsInteger;
    property AsInt64: Integer read ReadAsInt64 write WriteAsInt64;
    property AsID: Integer read ReadAsInt64 write WriteAsInt64;
    property AsBoolean: Boolean read ReadAsBoolean write WriteAsBoolean;
    property AsCurrency: Currency read ReadAsCurrency write WriteAsCurrency;
    property AsDate: TDateTime read ReadAsDate write WriteAsDate;
    property AsTime: TDateTime read ReadAsTime write WriteAsTime;
    property AsDateTime: TDateTime read ReadAsDateTime write WriteAsDateTime;
    property AsText: string read ReadAsText write WriteAsText; //binary text blob convert to hex

    property IsEmpty: Boolean read ReadIsEmpty;
    property IsNull: Boolean read ReadIsNull;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    {
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToIStream(Stream: IStreamPersist);
    }
  public
    procedure Clear;//make value null
    procedure Empty;//make value empty
  end;

  TmnCustomFieldClass = class of TmnCustomField;

  { TmnCustomFields }

  TmnCustomFields = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnCustomField;
  public
    property Items[Index: Integer]: TmnCustomField read GetItem;
  end;

  TmnField = class(TmnCustomField)
  private
    FName: string;
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
  published
    property Value;
    property IsEmpty;
    property IsNull;
    
    property AsVariant;
    property AsString;
    property AsAnsiString;
    property AsTrimString;
    property AsNullString;
    property AsInteger;
    property AsInt64;
    property AsBoolean;
    property AsCurrency;
    property AsDate;
    property AsTime;
    property AsDateTime;
    property AsText; 
    property AsHex;
    property Name: string read FName write FName;
  end;

  TmnFields = class(TmnCustomFields, IFields)
  private
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure SetValues(Index: string; const AValue: Variant);
    function GetValues(Index: string): Variant;
    function Find(vName: string): TmnField; virtual;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Add(AField: TmnField): Integer; overload;
    function ByName(vName: string): TmnField;
    function IsExists(vName: string): Boolean;
    procedure Clean; virtual;
    property Values[Index: string]: Variant read GetValues write SetValues;
  end;

implementation

{ TmnCustomField }

procedure TmnCustomField.CheckIsNil;
begin
  if Self = nil then
    raise Exception.Create('Field is nil');
end;

procedure TmnCustomField.Clear;
begin
  Value := Null;
end;

function TmnCustomField.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TmnCustomField.GetAsCurrency: Currency;
begin
  Result := Value;
end;

function TmnCustomField.GetAsDate: TDateTime;
begin
  Result := Value;
  Result := DateOf(Result);
end;

function TmnCustomField.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TmnCustomField.GetAsInt64: Integer;
begin
  Result := Value;
end;

function TmnCustomField.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TmnCustomField.GetAsString: string;
begin
  Result := Value;
end;

function TmnCustomField.GetAsTime: TDateTime;
begin
  Result := Value;
  Result := TimeOf(Result);
end;

function TmnCustomField.ReadAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TmnCustomField.GetIsEmpty: Boolean;
begin
  Result := (VarType(Value) in [varEmpty, varNull, varUnknown]);
end;

function TmnCustomField.GetIsNull: Boolean;
begin
  Result := (VarType(Value) in [varNull]);
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

function TmnCustomField.ReadAsBoolean: Boolean;
begin
  if IsEmpty then
    Result := False
  else
    try
      Result := GetAsBoolean;
    except
      on E: EVariantError do
        Result := False;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsCurrency: Currency;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsCurrency;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsDate: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsDate;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsDateTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsDateTime;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsInt64: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsInt64;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsInteger: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsInteger;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsString: string;
begin
  if IsEmpty then
    Result := ''
  else
    try
      Result := GetAsString;
    except
      on E: EVariantError do
        Result := '';
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsText: string;
begin
  if IsEmpty then
    Result := ''
  else
    try
      Result := GetAsText;
    except
      on E: EVariantError do
        Result := '';
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsTime: TDateTime;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsTime;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

procedure TmnCustomField.WriteAsNullString(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    AsString := AValue;
end;

function TmnCustomField.ReadAsHex: string;
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

procedure TmnCustomField.WriteAsHex(const AValue: string);
var
  s: string;
begin
  SetLength(s, Length(AValue) div 2);
  HexToBin(PChar(AValue), @s[1], Length(s));
  AsString := s;
end;

procedure TmnCustomField.SetAsBoolean(const AValue: Boolean);
begin
  AsInteger := Ord(AValue);
end;

procedure TmnCustomField.SetAsCurrency(const AValue: Currency);
begin
  Self.Value := AValue;
end;

procedure TmnCustomField.SetAsDate(const AValue: TDateTime);
begin
  Value := DateOf(AValue);
end;

procedure TmnCustomField.SetAsDateTime(const AValue: TDateTime);
begin
  Value := AValue;
end;

procedure TmnCustomField.SetAsInt64(const AValue: Integer);
begin
  Value := AValue;
end;

procedure TmnCustomField.SetAsInteger(const AValue: Integer);
begin
  Value := AValue;
end;

procedure TmnCustomField.SetAsString(const AValue: string);
begin
  Value := AValue;
end;

procedure TmnCustomField.Empty;
begin
  Value := Unassigned;
end;

function TmnCustomField.ReadAsAnsiString: ansistring;
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

procedure TmnCustomField.WriteAsAnsiString(const AValue: ansistring);
begin
  //fpc not auto convert because string type it same with ansistring
  SetAsString(AnsiToUtf8(AValue));
end;

function TmnCustomField.ReadAsWideString: widestring;
begin
  Result := GetAsString;//the compiler will convert it
end;

function TmnCustomField.ReadIsEmpty: Boolean;
begin
  if Self <> nil then
    Result := GetIsEmpty
  else
    Result := True;
end;

function TmnCustomField.ReadIsNull: Boolean;
begin
  if Self <> nil then
    Result := GetIsNull
  else
    Result := True;
end;

procedure TmnCustomField.SetAsText(const AValue: string);
begin
  AsString := AValue;
end;

procedure TmnCustomField.WriteAsWideString(const AValue: widestring);
begin
  SetAsString(AValue);
end;

procedure TmnCustomField.WriteAsBoolean(const AValue: Boolean);
begin
  CheckIsNil;
  SetAsBoolean(AValue);
end;

procedure TmnCustomField.WriteAsCurrency(const AValue: Currency);
begin
  CheckIsNil;
  SetAsCurrency(AValue);
end;

procedure TmnCustomField.WriteAsDate(const AValue: TDateTime);
begin
  CheckIsNil;
  SetAsDate(AValue);
end;

procedure TmnCustomField.WriteAsDateTime(const AValue: TDateTime);
begin
  CheckIsNil;
  SetAsDateTime(AValue);
end;

procedure TmnCustomField.WriteAsInt64(const AValue: Integer);
begin
  CheckIsNil;
  SetAsInt64(AValue);
end;

procedure TmnCustomField.WriteAsInteger(const AValue: Integer);
begin
  CheckIsNil;
  SetAsInteger(AValue);
end;

procedure TmnCustomField.WriteAsString(const AValue: string);
begin
  CheckIsNil;
  SetAsString(AValue);
end;

procedure TmnCustomField.WriteAsText(const AValue: string);
begin
  CheckIsNil;
  SetAsText(AValue);
end;

procedure TmnCustomField.WriteAsTime(const AValue: TDateTime);
begin
  CheckIsNil;
  SetAsTime(AValue);
end;

function TmnCustomField.ReadAsUtf8String: UTF8String;
begin
  Result := GetAsString;//the compiler will convert it
end;

procedure TmnCustomField.WriteAsUtf8String(const AValue: UTF8String);
begin
  SetAsString(AValue);
end;

procedure TmnCustomField.SetAsTime(const AValue: TDateTime);
begin
  Value := TimeOf(AValue);
end;

procedure TmnCustomField.WriteAsTrimString(const AValue: string);
begin
  AsString := Trim(AValue);
end;

{ TmnFields }

function TmnCustomFields.GetItem(Index: Integer): TmnCustomField;
begin
  Result := (inherited Items[Index]) as TmnCustomField;
end;

function TmnFields.Add(AField: TmnField): Integer;
begin
  Result := inherited Add(AField);
end;

function TmnFields.ByName(vName: string): TmnField;
begin
  Result := Find(vName);
  if Result = nil then
    raise Exception.Create('Field "' + vName + '" not found');
end;

function TmnFields.IsExists(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

procedure TmnFields.LoadFromFile(const FileName: string);
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


procedure TmnFields.LoadFromStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

function TmnFields.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TmnFields.SaveToFile(const FileName: string);
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

procedure TmnFields.SaveToStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

procedure TmnFields.SetValues(Index: string; const AValue: Variant);
var
  F: TmnField;
begin
  F := Find(Index);
  if F = nil then
  begin
    F := TmnField.Create;
    F.Name := Index;
    Add(F);
  end;
  F.Value := AValue;
end;

function TmnFields._AddRef: Integer;
begin
  Result := 0;
end;

function TmnFields._Release: Integer;
begin
  Result := 0;
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

function TmnFields.Find(vName: string): TmnField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, (Items[i] as TmnField).Name) then
    begin
      Result := Items[i] as TmnField;
      break;
    end;
  end;
end;

function TmnFields.GetValues(Index: string): Variant;
var
  F: TmnField;
begin
  F := Find(Index);
  if F <> nil then
    Result := F.Value
  else
    Result := Unassigned;
end;

{ TmnField }

function TmnField.GetValue: Variant;
begin
  if Self <> nil then
    Result := FValue
  else
    Result := Unassigned;
end;

procedure TmnField.SetValue(const AValue: Variant);
begin
  if Self <> nil then
    FValue := AValue
  else
    raise Exception.Create('Can not assign value');
end;

end.

