unit mncSchemes;
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
  SysUtils, Classes, contnrs,
  mncSQL, mncConnections;

//todo sokGenerator to sequance
type
//  sokNone, temp dummy
  TschmKind = (sokNone, sokDatabase, sokDomain, sokTable, sokView, sokProcedure, sokFunction,
    sokGenerator, sokException, sokRole, sokTrigger, sokForeign,
    sokIndexes, sokConstraints, sokFields, sokData);

  TExtractObject = (etDomain, etTable, etRole, etTrigger, etForeign, etIndex, etData, etGrant, etCheck);
  TExtractObjects = set of TExtractObject;

  TExtractOption = (ekExtra, ekAlter, ekSystem, ekOrder);
  TschmEnumOptions = set of TExtractOption;

  { TmncSchemaItem }

  TmncSchemaItem = class(TObject)
  private
    FAttributes: TStringList;
    FName: string;
    FComment: string;
    FData: TStringList;
    function GetData: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Comment: string read FComment write FComment;
    property Data: TStringList read GetData;
    property Attributes: TStringList read FAttributes write FAttributes;
  end;

  { TmncSchemaItems }

  TmncSchemaItems = class(TObjectList)
  private
    FExtraData: TStringList;
    function GetItem(Index: Integer): TmncSchemaItem;
    procedure SetItem(Index: Integer; const Value: TmncSchemaItem);
    function GetExtraData: TStringList;
  public
    function Find(const Name: string): TmncSchemaItem;
    function Add(vSchemaItem: TmncSchemaItem): Integer; overload;
    function Add(Name: string; Comment: string = ''): Integer; overload;
    property Items[Index: Integer]: TmncSchemaItem read GetItem write SetItem; default;
    property ExtraData: TStringList read GetExtraData;
  end;

  { TmncSchema }

  TmncSchema = class(TmncLinkObject)
  private
    FIncludeHeader: Boolean;
  protected
  public
    destructor Destroy; override;
    procedure EnumObject(Schema: TmncSchemaItems; Kind: TschmKind; RelationName: string = ''; Options: TschmEnumOptions = []);
    //---------------------
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumTypes(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumConstraints(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumTriggers(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumIndexes(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumFields(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    //source
    procedure GetTriggerSource(Strings:TStringList; RelationName: string; Options: TschmEnumOptions = []); virtual;
  published
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default False;
  end;

  TSQLType = record
    SqlType: Integer;
    TypeName: string;
  end;

  TPrivTypes = record
    PrivFlag: Integer;
    PrivString: string;
  end;

  TSQLTypes = array[0..14] of TSQLType;

implementation

{ TmncSchemaItems }

function TmncSchemaItems.Add(Name, Comment: string): Integer;
var
  aItem: TmncSchemaItem;
begin
  aItem := TmncSchemaItem.Create;
  aItem.Name := Name;
  aItem.Comment := Comment;
  Result := Add(aItem);
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

function TmncSchemaItems.GetExtraData: TStringList;
begin
  if FExtraData = nil then
    FExtraData := TStringList.Create;
  Result := FExtraData;
end;

function TmncSchemaItems.GetItem(Index: Integer): TmncSchemaItem;
begin
  Result := inherited Items[Index] as TmncSchemaItem;
end;

procedure TmncSchemaItems.SetItem(Index: Integer; const Value: TmncSchemaItem);
begin
  inherited Items[Index] := Value;
end;

const
  NEWLINE = #13#10;
  TERM = ';';
  ProcTerm = '^';

{ TmncSchema }

destructor TmncSchema.Destroy;
begin
  inherited;
end;

{ TmncSchemaItem }

function TmncSchemaItem.GetData: TStringList;
begin
  if FData = nil then
    FData := TStringList.Create;
  Result := FData;
end;

constructor TmncSchemaItem.Create;
begin
  inherited;
  FAttributes := TStringList.Create;
end;

destructor TmncSchemaItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

procedure TmncSchema.EnumObject(Schema: TmncSchemaItems; Kind: TschmKind; RelationName: string; Options: TschmEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokDomain: EnumTypes(Schema, Options);
    sokTable: EnumTables(Schema, Options);
    sokView: EnumViews(Schema, Options);
    sokProcedure: EnumProcedures(Schema, Options);
    sokFunction: EnumFunctions(Schema, Options);
    sokGenerator: EnumSequences(Schema, Options);
    sokException: EnumExceptions(Schema, Options);
    sokRole: ;
    sokTrigger: EnumTriggers(Schema, RelationName, Options);
    sokForeign: ;
    sokFields: EnumFields(Schema, RelationName, Options);
    sokIndexes: EnumIndexes(Schema, RelationName, Options);
    sokConstraints: EnumConstraints(Schema, RelationName, Options);
    sokData: ;
  end;
end;

procedure TmncSchema.EnumTables(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions
  );
begin

end;

procedure TmncSchema.EnumProcedures(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumSequences(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumFunctions(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumExceptions(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumTypes(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumConstraints(Schema: TmncSchemaItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumTriggers(Schema: TmncSchemaItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumIndexes(Schema: TmncSchemaItems; RelationName: string;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumFields(Schema: TmncSchemaItems; RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.GetTriggerSource(Strings: TStringList; RelationName: string; Options: TschmEnumOptions = []);
begin

end;

end.

