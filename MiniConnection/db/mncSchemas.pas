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
  SysUtils, Classes, contnrs,
  mncSQL, mncConnections;

type
  TschmKind = (sokNone, sokDatabase, sokTypes, sokTable, sokIndex, sokView,
               sokProcedure, sokFunction, sokSequences, sokException, sokRole,
               sokTrigger, sokForeign, sokIndices, sokConstraints, sokFields,
               sokData, sokProperty, sokProperties);

  TExtractObject = (etDomain, etTable, etRole, etTrigger, etForeign,
                    etIndex, etData, etGrant, etCheck);

  TExtractObjects = set of TExtractObject;

  TExtractOption = (ekExtra, ekAlter, ekSystem, ekSort);
  TschmEnumOptions = set of TExtractOption;

  { TmncSchemaItem }

  TmncSchemaItem = class(TObject)
  private
    FName: string;
    FAttributes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Attributes: TStringList read FAttributes;
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
    procedure EnumObject(Schema: TmncSchemaItems; Kind: TschmKind; MemberName: string = ''; Options: TschmEnumOptions = []);
    //---------------------
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumTypes(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumConstraints(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumTriggers(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumIndices(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumFields(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); virtual;
    //source
    procedure GetTriggerSource(Strings:TStringList; MemberName: string; Options: TschmEnumOptions = []); virtual;
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

constructor TmncSchemaItem.Create;
begin
  inherited;
  FAttributes := TStringList.Create;
end;

destructor TmncSchemaItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TmncSchema.EnumObject(Schema: TmncSchemaItems; Kind: TschmKind; MemberName: string; Options: TschmEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokTypes: EnumTypes(Schema, Options);
    sokTable: EnumTables(Schema, Options);
    sokView: EnumViews(Schema, Options);
    sokProcedure: EnumProcedures(Schema, Options);
    sokFunction: EnumFunctions(Schema, Options);
    sokSequences: EnumSequences(Schema, Options);
    sokException: EnumExceptions(Schema, Options);
    sokRole: ;
    sokTrigger: EnumTriggers(Schema, MemberName, Options);
    sokForeign: ;
    sokFields: EnumFields(Schema, MemberName, Options);
    sokIndices: EnumIndices(Schema, MemberName, Options);
    sokConstraints: EnumConstraints(Schema, MemberName, Options);
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
  MemberName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumTriggers(Schema: TmncSchemaItems;
  MemberName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumIndices(Schema: TmncSchemaItems; MemberName: string;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.EnumFields(Schema: TmncSchemaItems; MemberName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSchema.GetTriggerSource(Strings: TStringList; MemberName: string; Options: TschmEnumOptions = []);
begin

end;

end.

