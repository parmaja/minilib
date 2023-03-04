unit mnDON;
{ **
  *  Data Object Notication Tree
  *
  *  @license   The MIT License (MIT)
  *
  *  This file is part of the "Mini Library"
  *
  *  @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
  *            See the file COPYING.MLGPL, included in this distribution,
  *  @author    Zaher Dirkey <zaher, zaherdirkey>
  *}

{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch ArrayOperators}
{$ModeSwitch typehelpers}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}
{$mode delphi}
{$ENDIF}
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
  mnClasses, mnUtils,
  System.Rtti;

type
  { TSourceWriter }

  TSourceWriter = class abstract(TObject)
  public
    TabWidth: Integer;
    constructor Create;
    procedure Add(const S: string); overload; virtual; abstract;
    procedure Add(Level: Integer = 1; S: string = ''); overload;
    procedure NewLine; virtual; abstract;
  end;

  { TStringSourceWriter }

  TStringSourceWriter = class(TSourceWriter)
  private
    FStrings: TStrings;
    FLine: string;
  public
    constructor Create(Strings: TStrings);
    destructor Destroy; override;
    procedure Flush;
    procedure Add(const S: string); override;
    procedure NewLine; override;
  end;

//-----------------------------------------------------------------------------
//* DON objects

type
  { TDONType }

  TDONType = (
    donObject,
    donArray,
    donString,
    donNumber,
    donBoolean,
    donIdentifier
  );

  { TDON_Base }

  TDON_Base = class abstract(TmnObject)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); virtual;
  end;

  TDON_Value = class;

  { TDON_Pair }

  TDON_Pair = class(TDON_Base)
  private
    FName: string;
    FValue: TDON_Value;
    procedure SetName(const AValue: string);
    procedure SetValue(AValue: TDON_Value);
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  published
    property Value: TDON_Value read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  { TDON_Root }

  TDON_Root = class(TDON_Pair)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  end;

  { TDON_Value }

  TDON_Value = class abstract(TDON_Base)
  private
    FParent: TDON_Pair;
  public
    constructor Create(AParent: TDON_Pair);
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Parent: TDON_Pair read FParent;
  end;

  TDON_ValueClass = class of TDON_Value;

  { TDON_String_Value }

  TDON_String_Value = class(TDON_Value)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TDON_Identifier_Value }

  TDON_Identifier_Value = class(TDON_Value)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TDON_Number_Value }

  TDON_Number_Value = class(TDON_Value)
  private
    FNumber: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; const ANumber: string); overload;
  published
    property Number: string read FNumber write FNumber;
  end;

  { TDON_Boolean_Value }

  TDON_Boolean_Value = class(TDON_Value)
  private
    FValue: Boolean;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDON_Pair; AValue: Boolean); overload;
    constructor Create(AParent: TDON_Pair; const AValue: string); overload;
  published
    property Value: Boolean read FValue write FValue;
  end;

  { TDON_Items }

  TDON_Items = class(TmnObjectList<TDON_Pair>)
  public
  end;

  { TDON_Object_Value }

  TDON_Object_Value = class(TDON_Value)
  private
    FItems: TDON_Items;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure AcquirePair(const AName: string; out AObject: TObject);
    procedure Add(Value: TDON_Pair); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Items: TDON_Items read FItems;
  published
  end;

  { TDON_List }

  TDON_List = class(TmnObjectList<TDON_Value>)
  public
  end;

  { TDON_Array_Value }

  TDON_Array_Value = class(TDON_Value)
  private
    FItems: TDON_List;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure Add(Value: TDON_Value); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    property Items: TDON_List read FItems;
  published
  end;

function donAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;

implementation

{ TSourceWriter }

constructor TSourceWriter.Create;
begin
  inherited Create;
  TabWidth := 4;
end;

procedure TSourceWriter.Add(Level: Integer; S: string);
begin
  Add(StringOfChar(' ', Level * TabWidth) + S);
end;

{ TStringSourceWriter }

constructor TStringSourceWriter.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

destructor TStringSourceWriter.Destroy;
begin
  Flush;
  inherited;
end;

procedure TStringSourceWriter.Flush;
begin
  if FLine <> '' then
    NewLine;
end;

procedure TStringSourceWriter.Add(const S: string);
begin
  FLine := FLine + S;
end;

procedure TStringSourceWriter.NewLine;
begin
  FStrings.Add(FLine);
  FLine := '';
end;

{ TDON_Root }

procedure TDON_Root.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Value.WriteTo(Writer, LastOne, Level);
end;

{ TDON_Number_Value }

procedure TDON_Number_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Number);
  inherited;
end;

constructor TDON_Number_Value.Create(AParent: TDON_Pair; const ANumber: string);
begin
  inherited Create(AParent);
  FNumber := ANumber;
end;

{ TDON_Value }

constructor TDON_Value.Create(AParent: TDON_Pair);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TDON_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Self = nil then
    Writer.Add('null');
  if not LastOne then
    Writer.Add(',');
  Writer.NewLine;
end;

{ TDON_Base }

procedure TDON_Base.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin

end;

{ TDON_String_Value }

procedure TDON_String_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(QuoteStr(EscapeStringC(Text), '"'));
  inherited;
end;

constructor TDON_String_Value.Create(AParent: TDON_Pair; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TDON_Array_Value }

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

procedure TDON_Array_Value.Add(Value: TDON_Value);
begin
  Items.Add(Value);
end;

procedure TDON_Array_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TDON_Value;
begin
  Writer.Add('[');
  Writer.NewLine;
  for Itm in Items do
  begin
    Writer.Add(Level + 1);
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  end;
  Writer.Add(Level, ']');
  inherited;
end;

{ TDON_Object_Value }

procedure TDON_Object_Value.Created;
begin
  inherited;
  FItems := TDON_Items.Create;
end;

destructor TDON_Object_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TDON_Object_Value.AcquirePair(const AName: string; out AObject: TObject);
begin
  AObject := TDON_Pair.Create;
  (AObject as TDON_Pair).Name := AName;
  Add((AObject as TDON_Pair));
end;

procedure TDON_Object_Value.Add(Value: TDON_Pair);
begin
  Items.Add(Value);
end;

procedure TDON_Object_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TDON_Pair;
begin
  Writer.Add('{');
  Writer.NewLine;
  for Itm in Items do
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  Writer.Add(Level, '}');
  inherited;
end;

{ TDON_Pair }

procedure TDON_Pair.SetValue(AValue: TDON_Value);
begin
  if FValue <> AValue then
  begin
    if (AValue.Parent <> nil) and (AValue.Parent <> self) then
      raise Exception.Create('Value have parent we can`t move it to another parent');
    FreeAndNil(FValue);
    FValue := AValue;
    FValue.FParent := Self;
  end;
end;

procedure TDON_Pair.SetName(const AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TDON_Pair.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Level, QuoteStr(Name, '"') + ': ');
  if Value = nil then
    raise Exception.Create('Value is null for: ' + Name)
  else
    Value.WriteTo(Writer, LastOne, Level);
end;

function donAcquireValue(AParentObject: TObject; const AValue: string; AType: TDONType): TObject;
var
  v: TDON_Value;
  procedure CreateValue;
  begin
    case AType of
      donNumber: v :=  TDON_Number_Value.Create(nil, AValue);
      donIdentifier: v :=  TDON_Identifier_Value.Create(nil, AValue);
      donBoolean: v :=  TDON_Boolean_Value.Create(nil, AValue);
      donString: v :=  TDON_String_Value.Create(nil, AValue);
      donObject: v := TDON_Object_Value.Create(nil);
      donArray: v := TDON_Array_Value.Create(nil);
    end;
    Result := v;
  end;
begin
  Result := nil;
  if AParentObject = nil then
    raise Exception.Create('Can not set value to nil object');

  if (AParentObject is TDON_Array_Value) then
  begin
    CreateValue;
    (AParentObject as TDON_Array_Value).Add(v);
  end
  else if (AParentObject is TDON_Pair) then
  begin
     if (AParentObject as TDON_Pair).Value <> nil then
      raise Exception.Create('Value is already set and it is not array: ' + AParentObject.ClassName);
    CreateValue;
    (AParentObject as TDON_Pair).Value  :=  v;
  end
  else
    raise Exception.Create('Value can not set to:' + AParentObject.ClassName);
end;

{ TDON_Boolean_Value }

procedure TDON_Boolean_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Value then
    Writer.Add('true')
  else
    Writer.Add('false');
  inherited WriteTo(Writer, LastOne, Level);
end;

constructor TDON_Boolean_Value.Create(AParent: TDON_Pair; AValue: Boolean);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

constructor TDON_Boolean_Value.Create(AParent: TDON_Pair; const AValue: string);
begin
  inherited Create(AParent);
  if AValue = 'true' then
    FValue := true
  else if AValue = 'false' then
    FValue := false;
end;

{ TDON_Identifier_Value }

procedure TDON_Identifier_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Text);
  inherited;
end;

constructor TDON_Identifier_Value.Create(AParent: TDON_Pair; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

initialization
end.
