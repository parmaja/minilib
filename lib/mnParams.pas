unit mnParams;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Contnrs, mnFields;

type

  { TIParam }

  TIParam = class(TInterfacedObject, IField)
  private
    FName: string;
    FValue: Variant;
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
    procedure SetValue(const AValue: Variant);
    function GetIsExist: Boolean;
    function GetIsEmpty: Boolean;
    function GetAsObject: TObject;
    procedure SetAsObject(const Value: TObject);
  protected
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public
    property Name: string read FName write FName;
    property Value: Variant read GetValue write SetValue;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property IsExist: Boolean read GetIsExist;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { IParams }

  IParams = interface(IFields)['{4F35798E-8B1B-4FC2-9710-AAC058122A6D}']
    function GetRefCount: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetText: string;
    procedure SetText(const Value: string);

    procedure SetChild(Value: TObject);
    function GetChild: TObject;
    function GetItems(Index: Integer): TIParam;
    function GetParam(const Index: string): TIParam;

    function IsExist(const Name: string): Boolean;
    function IsEmpty(const Name: string): Boolean;

    function Add(const Name: string; const Value: Variant): TIParam;
    procedure Merge(Source: IParams);
    procedure Assign(Source: IParams);
    procedure AssignTo(Source: IParams);
    function Find(const Name: string): TIParam;
    property Items[Index: Integer]: TIParam read GetItems;
    property Param[Index: string]: TIParam read GetParam; default;
    property Count: Integer read GetCount;
    property Name: string read GetName write SetName;
    property Text: string read GetText write SetText;
    procedure Clear;
    property Child: TObject read GetChild write SetChild;
    property RefCount: Integer read GetRefCount;
  end;

function NewParams: IParams; overload;
function NewParams(Text: string; Name: string = ''): IParams; overload;
function NewParams(Keys: array of string; Values: array of Variant; Name: string = ''): IParams; overload;
function NewParams(Strings: TStringList; Name: string = ''): IParams; overload;
function NewParams(Params: IParams; Name: string = ''): IParams; overload;

implementation

{ TIParams }

type
  TIParams = class(TInterfacedObject, IParams)
  private
    FParams: TObjectList;
    FName: string;
    FChild: TObject;
    FRefCount: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetCount: Integer;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetItems(Index: Integer): TIParam;
    procedure SetItems(Index: Integer; const Value: TIParam);
    function GetParam(const Index: string): TIParam;
    function GetIField(const FieldName: string): IField;
    function GetValues(const Index: string): Variant;
    procedure SetChild(Value: TObject);
    function GetChild: TObject;
  protected
    function GetRefCount: Integer;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

    function IsExist(const Name: string): Boolean;
    function IsEmpty(const Name: string): Boolean;

    function Add(const Name: string; const Value: Variant): TIParam;
    function Append(const Name: string; const Value: Variant): TIParam;
    procedure Merge(Source: IParams); virtual;
    procedure Assign(Source: IParams); virtual;
    procedure AssignTo(Source: IParams); virtual;
    procedure AssignStrings(Strings: TStrings);
    function IndexOf(const Name: string): Integer;
    function Find(const Name: string): TIParam;
    property Count: Integer read GetCount;
    property RefCount: Integer read FRefCount;
    //IParam can carry an object, and free it when IParam destroy
    property Child: TObject read GetChild write SetChild;
    property Items[Index: Integer]: TIParam read GetItems write SetItems;
    property Param[Index: string]: TIParam read GetParam; default;
  end;

function TIParams.Add(const Name: string; const Value: Variant): TIParam;
begin
  Result := Find(Name);
  if Result = nil then
  begin
    Result := TIParam.Create;
    Result.Name := Name;
    FParams.Add(Result);
  end;
  Result.Value := Value;
end;

function TIParams.Append(const Name: string;
  const Value: Variant): TIParam;
begin
  Result := TIParam.Create;
  Result.Name := Name;
  FParams.Add(Result);
  Result.Value := Value;
end;

procedure TIParams.Assign(Source: IParams);
begin
  Clear;
  if Source <> nil then
  begin
    Merge(Source);
  end;
end;

procedure TIParams.AssignTo(Source: IParams);
begin

end;

constructor TIParams.Create;
begin
  inherited;
  FParams := TObjectList.create(True);
end;

function TIParams.GetItems(Index: Integer): TIParam;
begin
  Result := TIParam(FParams.Items[Index]);
end;

function TIParams.GetParam(const Index: string): TIParam;
begin
  Result := Find(Index);
  if Result = nil then
    raise Exception.Create('"' + Index + '" param not found');
end;

function TIParams.GetIField(const FieldName: string): IField;
begin
  Result := Find(FieldName);
end;

function TIParams.IndexOf(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].FName, Name) = 0 then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TIParams.Find(const Name: string): TIParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].FName, Name) = 0 then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TIParams.Merge(Source: IParams);
var
  i: Integer;
  aParams: IParams;
begin
  if Supports(Source, IParams) then
  begin
    aParams := (Source as IParams);
    for i := 0 to aParams.Count - 1 do
    begin
      Param[aParams.Items[i].Name].Value := aParams.Items[i].Value
    end;
  end;
end;

procedure TIParams.SetItems(Index: Integer; const Value: TIParam);
begin
  FParams.Items[Index] := Value;
end;

function TIParams.IsExist(const Name: string): Boolean;
begin
  Result := Find(Name) <> nil;
end;

destructor TIParams.Destroy;
begin
  Child := nil;
  FParams.Free;
  inherited;
end;

{ TIParam }

function TIParam.GetAsBoolean: Boolean;
begin
  if Self <> nil then
    Result := Value
  else
    Result := False;
end;

function TIParam.GetAsCurrency: Currency;
begin
  if Self <> nil then
    Result := Value
  else
    Result := 0;
end;

function TIParam.GetAsDateTime: TDateTime;
begin
  if Self <> nil then
    Result := Value
  else
    Result := 0;
end;

function TIParam.GetAsInteger: Integer;
begin
  if Self <> nil then
    Result := Value
  else
    Result := 0;
end;

function TIParam.GetAsObject: TObject;
begin
  if Self <> nil then
    Result := TObject(Integer(Value))
  else
    Result := nil;
end;

function TIParam.GetAsString: string;
begin
  if Self <> nil then
    Result := FValue
  else
    Result := '';
end;

function TIParam.GetIsEmpty: Boolean;
begin
  Result := (Self = nil) or VarIsEmpty(Value) or VarIsClear(Value) or VarIsNull(Value) or (VarIsStr(Value) and (Value = ''));
end;

function TIParam.GetIsExist: Boolean;
begin
  Result := Self <> nil;
end;

procedure TIParam.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TIParam.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TIParam.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

function TIParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TIParam.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TIParam.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TIParam.SetAsObject(const Value: TObject);
begin
  FValue := Integer(Value);
end;

procedure TIParam.LoadFromStream(Stream: TStream);
begin
  //TODO
end;

procedure TIParam.SaveToStream(Stream: TStream);
begin
  //TODO
end;

procedure TIParam.SetAsString(const Value: string);
begin
  FValue := Value
end;

function NewParams: IParams;
begin
  Result := TIParams.Create;
end;

function NewParams(Text: string; Name: string): IParams;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  Strings.Text := Text;
  Result := NewParams(Strings);
  Strings.Free;
  Result.Name := Name;
end;

function NewParams(Keys: array of string; Values: array of Variant; Name: string = ''): IParams;
var
  l, i: Integer;
begin
  Result := NewParams;
  l := Length(Keys);
  if l > Length(Values) then
    l := Length(Values);
  for i := 0 to l - 1 do
    Result[Keys[i]].Value := Values[i];
  Result.Name := Name;
end;

function NewParams(Strings: TStringList; Name: string = ''): IParams;
var
  i, p: Integer;
  a: string;
begin
  Result := NewParams;
  for i := 0 to Strings.Count - 1 do
  begin
    a := Strings[i];
    P := AnsiPos(Strings.NameValueSeparator, a);
    Result[Copy(a, 1, P - 1)].Value := Copy(a, P + 1, MaxInt)
  end;
  Result.Name := Name;
end;

function NewParams(Params: IParams; Name: string = ''): IParams;
begin
  Result := NewParams;
  Result.Assign(Params);
  Result.Name := Name;
end;

function TIParams.GetCount: Integer;
begin
  Result := FParams.Count;
end;

function TIParams.GetText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    Result := Format('%s%s=%s'#13#10, [Result, Items[i].Name, Items[i].AsString]);
end;

function TIParams.GetValues(const Index: string): Variant;
var
  aParam: TIParam;
begin
  aParam := Param[Index];
  if aParam <> nil then
    Result := aParam.Value
  else
    Result := varEmpty;
end;

procedure TIParams.SetText(const Value: string);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := Value;
    AssignStrings(Strings);
  finally
    Strings.Free;
  end;
end;

procedure TIParams.AssignStrings(Strings: TStrings);
var
  i, p: Integer;
  a: string;
begin
  if Strings <> nil then
  begin
    for i := 0 to Strings.Count - 1 do
    begin
      a := Strings[i];
      P := AnsiPos(Strings.NameValueSeparator, a);
      Param[Copy(a, 1, P - 1)].Value := Copy(a, P + 1, MaxInt)
    end;
  end;
end;

procedure TIParams.Clear;
begin
  if FParams <> nil then
    FParams.Clear;
end;

{ TIParamsList }

function TIParams.GetName: string;
begin
  Result := FName;
end;

procedure TIParams.SetName(const Value: string);
begin
  FName := Value;
end;

function TIParams.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

procedure TIParams.LoadFromStream(Stream: TStream);
begin
  //TODO
end;

procedure TIParams.SaveToStream(Stream: TStream);
begin
  //TODO
end;

function TIParams.IsEmpty(const Name: string): Boolean;
var
  aItem: TIParam;
begin
  aItem := Find(Name);
  Result := aItem.IsEmpty;
end;

function TIParams.GetChild: TObject;
begin
  Result := FChild;
end;

procedure TIParams.SetChild(Value: TObject);
begin
  if FChild <> nil then
    FChild.Free;
  FChild := Value;
end;

initialization
finalization
end.

