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

  { TmncSchemeItem }

  TmncSchemeItem = class(TObject)
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

  { TmncSchemeItems }

  TmncSchemeItems = class(TObjectList)
  private
    FExtraData: TStringList;
    function GetItem(Index: Integer): TmncSchemeItem;
    procedure SetItem(Index: Integer; const Value: TmncSchemeItem);
    function GetExtraData: TStringList;
  public
    function Find(const Name: string): TmncSchemeItem;
    function Add(vSchemeItem: TmncSchemeItem): Integer; overload;
    function Add(Name: string; Comment: string = ''): Integer; overload;
    property Items[Index: Integer]: TmncSchemeItem read GetItem write SetItem; default;
    property ExtraData: TStringList read GetExtraData;
  end;

  { TmncScheme }

  TmncScheme = class(TmncLinkObject)
  private
    FIncludeHeader: Boolean;
  protected
  public
    destructor Destroy; override;
    procedure EnumObject(Scheme: TmncSchemeItems; Kind: TschmKind; RelationName: string = ''; Options: TschmEnumOptions = []);
    //---------------------
    procedure EnumTables(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumViews(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumProcedures(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumSequences(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumFunctions(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumExceptions(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumTypes(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); virtual;
    procedure EnumConstraints(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumTriggers(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumIndexes(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
    procedure EnumFields(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); virtual;
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

{ TmncSchemeItems }

function TmncSchemeItems.Add(Name, Comment: string): Integer;
var
  aItem: TmncSchemeItem;
begin
  aItem := TmncSchemeItem.Create;
  aItem.Name := Name;
  aItem.Comment := Comment;
  Result := Add(aItem);
end;

function TmncSchemeItems.Find(const Name: string): TmncSchemeItem;
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

function TmncSchemeItems.Add(vSchemeItem: TmncSchemeItem): Integer;
begin
  Result := inherited Add(vSchemeItem);
end;

function TmncSchemeItems.GetExtraData: TStringList;
begin
  if FExtraData = nil then
    FExtraData := TStringList.Create;
  Result := FExtraData;
end;

function TmncSchemeItems.GetItem(Index: Integer): TmncSchemeItem;
begin
  Result := inherited Items[Index] as TmncSchemeItem;
end;

procedure TmncSchemeItems.SetItem(Index: Integer; const Value: TmncSchemeItem);
begin
  inherited Items[Index] := Value;
end;

const
  NEWLINE = #13#10;
  TERM = ';';
  ProcTerm = '^';

{ TmncScheme }

destructor TmncScheme.Destroy;
begin
  inherited;
end;

{ TmncSchemeItem }

function TmncSchemeItem.GetData: TStringList;
begin
  if FData = nil then
    FData := TStringList.Create;
  Result := FData;
end;

constructor TmncSchemeItem.Create;
begin
  inherited;
  FAttributes := TStringList.Create;
end;

destructor TmncSchemeItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

procedure TmncScheme.EnumObject(Scheme: TmncSchemeItems; Kind: TschmKind; RelationName: string; Options: TschmEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokDomain: EnumTypes(Scheme, Options);
    sokTable: EnumTables(Scheme, Options);
    sokView: EnumViews(Scheme, Options);
    sokProcedure: EnumProcedures(Scheme, Options);
    sokFunction: EnumFunctions(Scheme, Options);
    sokGenerator: EnumSequences(Scheme, Options);
    sokException: EnumExceptions(Scheme, Options);
    sokRole: ;
    sokTrigger: EnumTriggers(Scheme, RelationName, Options);
    sokForeign: ;
    sokFields: EnumFields(Scheme, RelationName, Options);
    sokIndexes: EnumIndexes(Scheme, RelationName, Options);
    sokConstraints: EnumConstraints(Scheme, RelationName, Options);
    sokData: ;
  end;
end;

procedure TmncScheme.EnumTables(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumViews(Scheme: TmncSchemeItems; Options: TschmEnumOptions
  );
begin

end;

procedure TmncScheme.EnumProcedures(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumSequences(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumFunctions(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumExceptions(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumTypes(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumConstraints(Scheme: TmncSchemeItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumTriggers(Scheme: TmncSchemeItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumIndexes(Scheme: TmncSchemeItems; RelationName: string;
  Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.EnumFields(Scheme: TmncSchemeItems; RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncScheme.GetTriggerSource(Strings: TStringList; RelationName: string; Options: TschmEnumOptions = []);
begin

end;

end.

