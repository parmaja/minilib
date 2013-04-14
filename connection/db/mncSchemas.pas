unit mncSchemas;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs,
  mncCommons;

type
  TschmKind = (sokNone, sokSchema, sokData,
               sokHost, sokDatabase, sokTable, sokView,
               sokProcedure, sokFunction, sokException, sokRole,
               sokTrigger, sokSequence, sokForeign, sokIndex, sokConstraint,
               sokField, sokOperator, sokProperty,
               sokType, sokDomain);

  TschmEnumOption = (ekExtra, ekAlter, ekSystem, ekSort);
  TschmEnumOptions = set of TschmEnumOption;

  { TmncSchemaParam }

  TmncSchemaParam = class(TObject)
  private
    FName: string;
    FValue: string;
  public
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  { TmncSchemaAttributes }

  TmncSchemaAttributes = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncSchemaParam;
    function GetValues(Index: string): string;
    procedure SetItem(Index: Integer; const Value: TmncSchemaParam);
  public
    constructor Create(Names, Values: array of string); overload;
    constructor Create; overload;
    function Find(const Name: string): TmncSchemaParam;
    function Add(Param: TmncSchemaParam): Integer; overload;
    function Add(Name:string; Value: string=''): TmncSchemaParam; overload;
    property Items[Index: Integer]: TmncSchemaParam read GetItem write SetItem;
    property Values[Index: string]: string read GetValues; default;
  end;

  { TmncSchemaItem }

  TmncSchemaItem = class(TObject)
  private
    FKind: TschmKind;
    FName: string;
    FAttributes: TmncSchemaAttributes;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Kind: TschmKind read FKind write FKind;
    property Attributes: TmncSchemaAttributes read FAttributes;
  end;

  { TmncSchemaItems }

  TmncSchemaItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncSchemaItem;
    procedure SetItem(Index: Integer; const Value: TmncSchemaItem);
  public
    function Find(const Name: string): TmncSchemaItem;
    function Add(vSchemaItem: TmncSchemaItem): Integer; overload;
    function Add(Name: string): TmncSchemaItem; overload;
    property Items[Index: Integer]: TmncSchemaItem read GetItem write SetItem; default;
  end;

{ TmncSchema }

  TmncSchema = class(TmncLinkObject)
  private
    FIncludeHeader: Boolean;
  protected
  public
    destructor Destroy; override;
    procedure EnumObjects(Schema: TmncSchemaItems; Kind: TschmKind; SQLName: string = ''; Options: TschmEnumOptions = []);
    //---------------------
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumDomains(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumConstraints(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumTriggers(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumIndices(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumFields(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions = []); virtual;
    //source
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TschmEnumOptions = []); virtual;
  published
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default False;
  end;

  TmncSchemaType = record
    SQLType: Integer;
    TypeName: string;
  end;

implementation

{ TmncSchemaItems }

function TmncSchemaItems.Add(Name: string): TmncSchemaItem;
begin
  Result := TmncSchemaItem.Create;
  Result.Name := Name;
  Add(Result);
end;

function TmncSchemaItems.Find(const Name: string): TmncSchemaItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Name = Items[i].Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncSchemaItems.Add(vSchemaItem: TmncSchemaItem): Integer;
begin
  Result := inherited Add(vSchemaItem);
end;

function TmncSchemaItems.GetItem(Index: Integer): TmncSchemaItem;
begin
  Result := inherited Items[Index] as TmncSchemaItem;
end;

procedure TmncSchemaItems.SetItem(Index: Integer; const Value: TmncSchemaItem);
begin
  inherited Items[Index] := Value;
end;
{TODO: delete it
const
  NEWLINE = #13#10;
  TERM = ';';
  ProcTerm = '^';

  }

{ TmncSchema }

destructor TmncSchema.Destroy;
begin
  inherited;
end;

{ TmncSchemaItem }

constructor TmncSchemaItem.Create;
begin
  inherited;
  FAttributes := TmncSchemaAttributes.Create;
end;

destructor TmncSchemaItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TmncSchema.EnumObjects(Schema: TmncSchemaItems; Kind: TschmKind; SQLName: string; Options: TschmEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokDomain: EnumDomains(Schema, Options);
    sokTable: EnumTables(Schema, Options);
    sokView: EnumViews(Schema, Options);
    sokProcedure: EnumProcedures(Schema, Options);
    sokFunction: EnumFunctions(Schema, Options);
    sokSequence: EnumSequences(Schema, Options);
    sokException: EnumExceptions(Schema, Options);
    sokRole: ;
    sokTrigger: EnumTriggers(Schema, SQLName, Options);
    sokForeign: ;
    sokField: EnumFields(Schema, SQLName, Options);
    sokIndex: EnumIndices(Schema, SQLName, Options);
    sokConstraint: EnumConstraints(Schema, SQLName, Options);
    sokData: ;
  end;
end;

procedure TmncSchema.EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumDomains(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumConstraints(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumTriggers(Schema: TmncSchemaItems;
  SQLName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumIndices(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.EnumFields(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncSchema.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions = []);
begin

end;

{ TmncSchemaAttributes }

function TmncSchemaAttributes.GetItem(Index: Integer): TmncSchemaParam;
begin
  Result := inherited Items[Index] as TmncSchemaParam;
end;

function TmncSchemaAttributes.GetValues(Index: string): string;
var
  aItem: TmncSchemaParam;
begin
  if Self = nil then
    Result := ''
  else
  begin
    aItem := Find(Index);
    if aItem = nil then
      Result := ''
    else
      Result := aItem.Value;
  end;
end;

procedure TmncSchemaAttributes.SetItem(Index: Integer; const Value: TmncSchemaParam);
begin
  inherited Items[Index] := Value;
end;

constructor TmncSchemaAttributes.Create(Names, Values: array of string);
var
  i: Integer;
  v: string;
begin
  inherited Create(True);
  for i := 0 to Length(Names) - 1 do
  begin
    if i < Length(Values) then
      v := Values[i]
    else
      v := '';
    Add(Names[i], v);
  end;
end;

constructor TmncSchemaAttributes.Create;
begin
  Create([], []);
end;

function TmncSchemaAttributes.Find(const Name: string): TmncSchemaParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncSchemaAttributes.Add(Param: TmncSchemaParam): Integer;
begin
  Result := inherited Add(Param);
end;

function TmncSchemaAttributes.Add(Name, Value: string): TmncSchemaParam;
begin
  Result := TmncSchemaParam.Create;
  Result.Name := Name;
  Result.Value := Value;
  Add(Result);
end;

end.

