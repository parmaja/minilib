unit mnFields;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$IFDEF fpc}
{$MODE delphi}{$H+}
{$ELSE}
{$DEFINE WINDOWS}
{$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs,
  mnClasses, mnUtils;

type

  { IField }

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
    function GetValues(const Index: string): Variant;
    property Values[const Index: string]: Variant read GetValues;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetIField(const FieldName: string): IField;
  end;

  { TmnCustomField }

  TmnCustomField = class abstract(TInterfacedPersistent, IField)
  private
    procedure CheckIsNil;
    function ReadAsHex: string;
    procedure WriteAsHex(const AValue: string);
    procedure WriteAsNullString(const AValue: string);

    {$ifndef NEXTGEN}
    function ReadAsAnsiString: ansistring;
    procedure WriteAsAnsiString(const AValue: ansistring);
    function ReadAsWideString: widestring;
    procedure WriteAsWideString(const AValue: widestring);
    function ReadAsBytes: TBytes;
    procedure WriteAsBytes(const AValue: TBytes);
    function ReadAsGuid: TGUID;
    procedure WriteAsGuid(const Value: TGUID);
    {$endif}

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
    function ReadAsInt64: Int64;
    procedure WriteAsInt64(const AValue: Int64);
    function ReadAsDouble: Double;
    procedure WriteAsDouble(const AValue: Double);
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
    function ReadIsNull: Boolean;
    procedure WriteIsNull(const AValue: Boolean);
    function ReadAsForeign: Integer;
    procedure WriteAsForeign(const Value: Integer);
    function ReadIsEmpty: Boolean;
    function ReadIsExists: Boolean;
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;

    function GetAsText: string; virtual;
    procedure SetAsText(const AValue: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
    function GetAsInteger: Integer; virtual;
    procedure SetAsInteger(const AValue: Integer); virtual;
    function GetAsInt64: Int64; virtual;
    procedure SetAsInt64(const AValue: Int64); virtual;
    function GetAsDouble: Double; virtual;
    procedure SetAsDouble(const AValue: Double); virtual;
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
    function GetAsBytes: TBytes; virtual;
    procedure SetAsBytes(const AValue: TBytes); virtual;

    function GetIsNull: Boolean; virtual; abstract;
    procedure SetIsNull(const AValue: Boolean); virtual; abstract;
    function GetIsEmpty: Boolean; virtual;
  public
    property Value: Variant read GetValue write SetValue;
    property AsVariant: Variant read GetValue write SetValue;
    //* AsAnsiString: Convert strign to utf8 it is special for Lazarus
    {$ifndef NEXTGEN}
    property AsAnsiString: ansistring read ReadAsAnsiString write WriteAsAnsiString;
    property AsWideString: widestring read ReadAsWideString write WriteAsWideString;
    {$endif}
    property AsUtf8String: Utf8String read ReadAsUtf8String write WriteAsUtf8String;
    property AsTrimString: string read ReadAsTrimString write WriteAsTrimString;
    property AsNullString: string read ReadAsString write WriteAsNullString;
    property AsHex: string read ReadAsHex write WriteAsHex;

    property AsString: string read ReadAsString write WriteAsString;
    property AsInteger: Integer read ReadAsInteger write WriteAsInteger;
    property AsInt64: Int64 read ReadAsInt64 write WriteAsInt64;
    property AsDouble: Double read ReadAsDouble write WriteAsDouble;
    property AsID: Int64 read ReadAsInt64 write WriteAsInt64;
    property AsBoolean: Boolean read ReadAsBoolean write WriteAsBoolean;
    property AsCurrency: Currency read ReadAsCurrency write WriteAsCurrency;
    property AsDate: TDateTime read ReadAsDate write WriteAsDate;
    property AsTime: TDateTime read ReadAsTime write WriteAsTime;
    property AsDateTime: TDateTime read ReadAsDateTime write WriteAsDateTime;
    property AsText: string read ReadAsText write WriteAsText; //binary text blob convert to hex
    property AsBytes: TBytes read ReadAsBytes write WriteAsBytes;
    property AsGuid: TGUID read ReadAsGuid write WriteAsGuid;
    property AsForeign: Integer read ReadAsForeign write WriteAsForeign; // alias for as integer for foreign fields

    property IsNull: Boolean read ReadIsNull write WriteIsNull;
    property IsEmpty: Boolean read ReadIsEmpty;
    property IsExists: Boolean read ReadIsExists;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    {
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToIStream(Stream: IStreamPersist);
    }
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;//make value null //should be abstract
    procedure Empty; virtual;//make value empty

    //class operator Implicit (f: TprmustomField): Integer; { TODO : when support by delphi }
  end;

  TmnCustomFieldClass = class of TmnCustomField;

  { TmnCustomFields }

  TmnCustomFields<T: TmnCustomField> = class(TmnObjectList<T>)
  public
    procedure Clear; virtual;
    procedure Clean; virtual;
  end;

  { TmnField }

  TmnField = class(TmnCustomField)
  private
    FName: string;
    FValue: Variant;
  protected
    function GetIsNull: Boolean; override;
    procedure SetIsNull(const AValue: Boolean); override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
    function GetFullString: String; virtual;
  public
    function GetNameValue(Seperator: string = '='): String;
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
    property AsDouble;
    property AsBoolean;
    property AsCurrency;
    property AsDate;
    property AsTime;
    property AsDateTime;
    property AsText; 
    property AsHex;
    property Name: string read FName write FName;
    property FullString: String read GetFullString;
  end;

  { TmnFields }

  TmnFields = class(TmnCustomFields<TmnField>, IFields)
  private
    function _AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    //function GetItem(Index: Integer): TmnField;
  protected
    function CreateField: TmnField; virtual;
    function SetValue(const Index: string; const AValue: Variant): TmnField; virtual;

    procedure SetValues(const Index: string; const AValue: Variant);
    function GetValues(const Index: string): Variant;
    function GetIField(const FieldName: string): IField;
    function GetCount: Integer;
  public
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj):HResult; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function AddItem(S: string; Separator: string; TrimIt: Boolean = False): TmnField; overload;
    function Add(AName, AValue: string): TmnField; overload;
    //This will split the name and value
    function Put(const AName, AValue: string): TmnField; overload;
    function IsExists(const vName: string): Boolean;
    function FindField(const vName: string): TmnField; virtual; //no exception
//    function ByName(const vName: string): TmnField; deprecated; //with exception if not exists
    function FindByName(const vName: string): TmnField; //with exception if not exists
    function IndexOfName(const vName: string): Integer;
    function RemoveByName(const vName: string): Boolean;
    //todo IndexOfName, IndexOf
    property FieldByName[const Index: string]: TmnField read FindByName;
    property Field[const Index: string]: TmnField read FindField;
    property Exists[const Index: string]: Boolean read IsExists;
    property Values[const Index: string]: Variant read GetValues write SetValues; default;
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
//  Value := Null;
  IsNull := True;
end;

function TmnCustomField.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TmnCustomField.GetAsBytes: TBytes;
begin
  Result := Value;
end;

function TmnCustomField.GetAsCurrency: Currency;
begin
  Result := Value;
end;

function TmnCustomField.GetAsDate: TDateTime;
begin
  Result := DateOf(AsDateTime);
end;

function TmnCustomField.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TmnCustomField.GetAsInt64: Int64;
begin
  Result := Value;
end;

function TmnCustomField.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TmnCustomField.GetAsString: string;
begin
  if VarIsNull(Value) then
    Result := ''
  else
    Result := Value;
end;

function TmnCustomField.GetAsTime: TDateTime;
begin
//  Result := TimeOf(Value);
  Result := TimeOf(AsDateTime);
end;

function TmnCustomField.ReadAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TmnCustomField.GetIsEmpty: Boolean;
begin
  Result := VarIsClear(Value) or (VarType(Value) in [varEmpty, varNull, varUnknown]);
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

function TmnCustomField.ReadAsBytes: TBytes;
begin
  if IsEmpty then
    Result := nil
  else
    try
      Result := GetAsBytes;
    except
      on E: EVariantError do
        Result := nil;
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

function TmnCustomField.ReadAsInt64: Int64;
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
begin
  Result := String2Hex(AsString);
end;

procedure TmnCustomField.WriteIsNull(const AValue: Boolean);
begin
  CheckIsNil;
  SetIsNull(AValue);
end;

function TmnCustomField.GetAsText: string;
begin
  Result := AsString;
end;

procedure TmnCustomField.WriteAsHex(const AValue: string);
begin
  AsString := Hex2String(AValue);
end;

procedure TmnCustomField.SetAsBoolean(const AValue: Boolean);
begin
  AsInteger := Ord(AValue);
end;

procedure TmnCustomField.SetAsBytes(const AValue: TBytes);
begin
  if Length(AValue) = 0 then
    Clear
  else
    Value := AValue;
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
  if AValue = 0 then
    Clear //TODO ask belal
  else
    Value := AValue;
end;

procedure TmnCustomField.SetAsInt64(const AValue: Int64);
begin
  Value := AValue;
end;

function TmnCustomField.GetAsDouble: Double;
begin
  Result := Value;
end;

procedure TmnCustomField.SetAsDouble(const AValue: Double);
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

{$ifndef NEXTGEN}
function TmnCustomField.ReadAsAnsiString: ansistring;
begin
  Result := AnsiString(GetAsString);
end;

procedure TmnCustomField.WriteAsAnsiString(const AValue: ansistring);
begin
  {$ifdef FPC}
  //fpc not auto convert because string type it same with ansistring
  SetAsString(Utf8Encode(AValue));
  {$else}
  SetAsString(AnsiToUtf8(AValue));
  {$endif}
end;

function TmnCustomField.ReadAsWideString: widestring;
begin
  Result := widestring(GetAsString);//the compiler will convert it
end;

procedure TmnCustomField.WriteAsWideString(const AValue: widestring);
begin
  SetAsString(String(AValue));
end;
{$endif}

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

constructor TmnCustomField.Create;
begin
  inherited Create;
end;

procedure TmnCustomField.Assign(Source: TPersistent);
begin
  if Source is TmnCustomField then
    Value := (Source as TmnCustomField).Value
  else
    inherited;
end;

procedure TmnCustomField.SaveToStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

function TmnCustomField.ReadIsExists: Boolean;
begin
  Result := Self <> nil;
end;

function TmnCustomField.ReadIsEmpty: Boolean;
begin
  if Self <> nil then
    Result := IsNull or GetIsEmpty
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

procedure TmnCustomField.WriteAsBoolean(const AValue: Boolean);
begin
  CheckIsNil;
  SetAsBoolean(AValue);
end;

procedure TmnCustomField.WriteAsBytes(const AValue: TBytes);
begin
  CheckIsNil;
  SetAsBytes(AValue);
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

procedure TmnCustomField.WriteAsInt64(const AValue: Int64);
begin
  CheckIsNil;
  SetAsInt64(AValue);
end;

function TmnCustomField.ReadAsDouble: Double;
begin
  if IsEmpty then
    Result := 0
  else
    try
      Result := GetAsDouble;
    except
      on E: EVariantError do
        Result := 0;
      else
        raise;
    end;
end;

function TmnCustomField.ReadAsForeign: Integer;
begin
  Result := AsInteger;
end;

function TmnCustomField.ReadAsGuid: TGUID;
begin
  Result := TGUID.Create(AsBytes);
end;

procedure TmnCustomField.WriteAsDouble(const AValue: Double);
begin
  CheckIsNil;
  SetAsDouble(AValue);
end;

procedure TmnCustomField.WriteAsForeign(const Value: Integer);
begin
  if Value = 0 then
    Clear
  else
    AsInteger := Value;
end;

procedure TmnCustomField.WriteAsGuid(const Value: TGUID);
begin
  AsBytes := Value.ToByteArray;
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
  Result := UTF8Encode(GetAsString); // the compiler will convert it
end;

procedure TmnCustomField.WriteAsUtf8String(const AValue: UTF8String);
begin
  {$ifdef FPC}
  SetAsString(UTF8Decode(AValue));
  {$else}
  SetAsString(UTF8Decode(AValue));
  {$endif}
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

{function TmnFields.ByName(vName: string): TmnField;
begin
  Result := FindField(vName);
  if Result = nil then
    raise Exception.Create('Field "' + vName + '" not found');
end;}

function TmnFields.IndexOfName(const vName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, (Items[i] as TmnField).Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TmnFields.IsExists(const vName: string): Boolean;
begin
  Result := FindField(vName) <> nil;
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

function TmnFields.QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj):HResult; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TmnFields.RemoveByName(const vName: string): Boolean;
var
  index: Integer;
begin
  index := IndexOfName(vName);
  Result := Index > 0;
  if Result then
    Delete(Index);
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

function TmnFields.Add(AName, AValue: string): TmnField;
begin
  Result := CreateField;
  Result.FValue := AValue;
  Result.FName := AName;
  Add(Result);
end;

function TmnFields.AddItem(S: string; Separator: string; TrimIt: Boolean): TmnField;
var
  p: Integer;
  aName: string;
  aValue: string;
begin
  p := Pos(Separator, S);
  if p > 0 then
  begin
    aName := Trim(Copy(S, 1, P - 1));
    aValue := Copy(S, P + 1, MaxInt);
  end
  else
  begin
    aName := '';
    aValue := S;
  end;
  if TrimIt then
    aValue := Trim(aValue);
  Result := Add(aName, aValue);
end;

procedure TmnFields.SaveToStream(Stream: TStream);
begin
  raise Exception.Create('Not implemented yet');
end;

function TmnFields.Put(const AName, AValue: string): TmnField;
begin
  Result := FindField(AName);
  if Result = nil then
  begin
    Result := CreateField;
    Add(Result);
    Result.FName := AName;
  end;
  Result.FValue := AValue;
end;

procedure TmnFields.SetValues(const Index: string; const AValue: Variant);
begin
  SetValue(Index, AValue);
end;

function TmnFields._AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  Result := 0;
end;

function TmnFields._Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  Result := 0;
end;

function TmnFields.CreateField: TmnField;
begin
  Result := TmnField.Create;
end;

function TmnFields.SetValue(const Index: string; const AValue: Variant): TmnField;
begin
  Result := FindField(Index);
  if Result = nil then
  begin
    Result := CreateField;
    Result.Name := Index;
    Add(Result);
  end;
  Result.Value := AValue;
end;

function TmnFields.FindField(const vName: string): TmnField;
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

function TmnFields.GetCount: Integer;
begin
  Result := Count;
end;

function TmnFields.FindByName(const vName: string): TmnField;
begin
  Result := FindField(vName);
  if Result = nil then
    raise Exception.Create('Field "' + vName + '" not found');
end;

function TmnFields.GetIField(const FieldName: string): IField;
begin
  Result := FindField(FieldName);
end;

{function TmnFields.GetItem(Index: Integer): TmnField;
begin
  Result := (inherited GetItem(Index)) as TmnField;
end;}

function TmnFields.GetValues(const Index: string): Variant;
var
  F: TmnField;
begin
  F := FindField(Index);
  if F <> nil then
    Result := F.Value
  else
    Result := Unassigned;
end;

{ TmnField }

function TmnField.GetIsNull: Boolean;
begin
  Result := VarIsNull(Value);
end;

procedure TmnField.SetIsNull(const AValue: Boolean);
begin
  Value := Null;
end;

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

function TmnField.GetAsString: string;
begin
  Result := FValue;
end;

procedure TmnField.SetAsString(const AValue: string);
begin
  FValue := AValue;
end;

function TmnField.GetFullString: String;
begin
  Result := GetNameValue;
end;

function TmnField.GetNameValue(Seperator: string): String;
begin
  Result := Name + Seperator + AsString;
end;

{ TmnCustomFields<T> }

procedure TmnCustomFields<T>.Clear;
begin
  inherited Create;
end;

procedure TmnCustomFields<T>.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Clear;
  end;
end;

end.
