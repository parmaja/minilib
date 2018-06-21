unit mncMeta;
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
  mnFields, mncCommons, mncORM;

type
  TschmKind = (sokNone, sokMeta, sokData,
               sokHost, sokDatabase, sokTable, sokView,
               sokProcedure, sokFunction, sokException, sokRole,
               sokTrigger, sokSequence, sokForeign, sokIndex, sokConstraint,
               sokField, sokOperator, sokProperty,
               sokType, sokDomain);

  TschmEnumOption = (ekExtra, ekAlter, ekSystem, ekSort);
  TschmEnumOptions = set of TschmEnumOption;

  TmncMetaAttributes = class(TmnFields)
  private
  public
  end;

  { TmncMetaItem }

  TmncMetaItem = class(TObject)
  private
    FKind: TschmKind;
    FName: string;
    FAttributes: TmncMetaAttributes;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Kind: TschmKind read FKind write FKind;
    property Attributes: TmncMetaAttributes read FAttributes;
  end;

  { TmncMetaItems }

  TmncMetaItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TmncMetaItem;
    procedure SetItem(Index: Integer; const Value: TmncMetaItem);
  public
    function Find(const Name: string): TmncMetaItem;
    function Add(vMetaItem: TmncMetaItem): Integer; overload;
    function Add(Name: string): TmncMetaItem; overload;
    property Items[Index: Integer]: TmncMetaItem read GetItem write SetItem; default;
  end;

  TmncSQLCallback = procedure (SQL: string);

{ TmncMeta }

  TmncMeta = class(TmncLinkObject)
  private
    FIncludeHeader: Boolean;
  protected
  public
    destructor Destroy; override;
    procedure EnumObjects(Meta: TmncMetaItems; Kind: TschmKind; SQLName: string = ''; Options: TschmEnumOptions = []);
    //---------------------
    procedure EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumDomains(Meta: TmncMetaItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumConstraints(Meta: TmncMetaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumTriggers(Meta: TmncMetaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumIndices(Meta: TmncMetaItems; SQLName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions = []); virtual;
    //source
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TschmEnumOptions = []); virtual;
    procedure GetViewSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions = []); virtual;
    procedure GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions = []); virtual;

    procedure GenerateSchema(ormSchema: TormSpace; Callback: TmncSQLCallback); virtual;
  published
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default False;
  end;

  TmncMetaClass = class of TmncMeta;

  TmncMetaType = record
    SQLType: Integer;
    TypeName: string;
  end;

implementation

{ TmncMetaItems }

function TmncMetaItems.Add(Name: string): TmncMetaItem;
begin
  Result := TmncMetaItem.Create;
  Result.Name := Name;
  Add(Result);
end;

function TmncMetaItems.Find(const Name: string): TmncMetaItem;
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

function TmncMetaItems.Add(vMetaItem: TmncMetaItem): Integer;
begin
  Result := inherited Add(vMetaItem);
end;

function TmncMetaItems.GetItem(Index: Integer): TmncMetaItem;
begin
  Result := inherited Items[Index] as TmncMetaItem;
end;

procedure TmncMetaItems.SetItem(Index: Integer; const Value: TmncMetaItem);
begin
  inherited Items[Index] := Value;
end;
{TODO: delete it
const
  NEWLINE = #13#10;
  TERM = ';';
  ProcTerm = '^';

  }

{ TmncMeta }

destructor TmncMeta.Destroy;
begin
  inherited;
end;

{ TmncMetaItem }

constructor TmncMetaItem.Create;
begin
  inherited;
  FAttributes := TmncMetaAttributes.Create;
end;

destructor TmncMetaItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TmncMeta.EnumObjects(Meta: TmncMetaItems; Kind: TschmKind; SQLName: string; Options: TschmEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokDomain: EnumDomains(Meta, Options);
    sokTable: EnumTables(Meta, Options);
    sokView: EnumViews(Meta, Options);
    sokProcedure: EnumProcedures(Meta, Options);
    sokFunction: EnumFunctions(Meta, Options);
    sokSequence: EnumSequences(Meta, Options);
    sokException: EnumExceptions(Meta, Options);
    sokRole: ;
    sokTrigger: EnumTriggers(Meta, SQLName, Options);
    sokForeign: ;
    sokField: EnumFields(Meta, SQLName, Options);
    sokIndex: EnumIndices(Meta, SQLName, Options);
    sokConstraint: EnumConstraints(Meta, SQLName, Options);
    sokData: ;
  end;
end;

procedure TmncMeta.EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncMeta.EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumProcedures(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumSequences(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumFunctions(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumExceptions(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumDomains(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumConstraints(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumTriggers(Meta: TmncMetaItems;
  SQLName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncMeta.EnumIndices(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions = []);
begin

end;

procedure TmncMeta.GetViewSource(Strings: TStringList; SQLName: string;
  Options: TschmEnumOptions);
begin

end;

procedure TmncMeta.GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncMeta.GenerateSchema(ormSchema: TormSpace; Callback: TmncSQLCallback);
begin
end;

end.
