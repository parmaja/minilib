unit mncMeta;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs,
  mnClasses, mnFields, mncCommons, mncConnections, mncORM, mncSQL;

type
  TmetaKind = (sokNone, sokMeta, sokData,
               sokHost, sokDatabase, sokTable, sokView,
               sokProcedure, sokFunction, sokException, sokRole,
               sokTrigger, sokSequence, sokForeign, sokIndex, sokConstraint,
               sokField, sokOperator, sokProperty,
               sokType, sokDomain);

  TmetaEnumOption = (ekExtra, ekAlter, ekSystem, ekSort);
  TmetaEnumOptions = set of TmetaEnumOption;

  TmncMetaAttributes = class(TmnFields)
  private
  public
  end;

  TmncMetaHeader = class(TmnFields)
  private
  public
  end;

  { TmncMetaItem }

  TmncMetaItem = class(TmnNamedObject)
  private
    FSQLName: string; //it is the real name
    FFullName: string;
    FKind: TmetaKind;
    FAttributes: TmncMetaAttributes;
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Kind: TmetaKind read FKind write FKind;
    property Attributes: TmncMetaAttributes read FAttributes;
    procedure Clear;
    procedure Clone(AMetaItem: TmncMetaItem);
    property Value: string read FValue write FValue;
    property SQLName: string read FSQLName write FSQLName;
    property FullName: string read FFullName write FFullName; //ToDo
  end;

  { TmncMetaItems }

  TmncMetaItems = class(TmnNamedObjectList<TmncMetaItem>)
  private
    FHeader: TmncMetaHeader;
    function GetValues(Index: string): string;
    procedure SetValues(Index: string; AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Header: TmncMetaHeader read FHeader;
    function Add(Name: string; Value: string = ''): TmncMetaItem; overload;
    property Values[Index: string]: string read GetValues write SetValues;
    procedure Clone(AMetaItems: TmncMetaItems);
  end;

  TmncSQLCallback = procedure (SQL: string);

{ TmncMeta }

  TmncMeta = class(TmncLinkObject)
  private
    FIncludeHeader: Boolean;
    FServerInfo: TmncServerInfo;
    function GetSession: TmncSession;
    procedure SetSession(AValue: TmncSession);
  protected
  public
    destructor Destroy; override;
    procedure EnumObjects(Meta: TmncMetaItems; Kind: TmetaKind; SQLName: string = ''; Options: TmetaEnumOptions = []);
    //---------------------
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumDomains(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumConstraints(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); virtual;
    procedure EnumTriggers(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); virtual;
    procedure EnumIndices(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); virtual;
    procedure EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); virtual;
    //source
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TmetaEnumOptions = []); virtual;
    procedure GetViewSource(Strings: TStringList; SQLName: string; Options: TmetaEnumOptions = []); virtual;
    procedure GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); virtual;

    procedure GenerateSchema(ORM: TmncORM; Callback: TmncSQLCallback); virtual;
    property Session: TmncSession read GetSession write SetSession;//alias for FLink in base class
    property ServerInfo: TmncServerInfo read FServerInfo write FServerInfo;
  published
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default False;
  end;

  TmncMetaClass = class of TmncMeta;

  { TmncSQLMeta }

  TmncSQLMeta = class(TmncMeta)
  private
    function GetSQLSession: TmncSQLSession;
    procedure SetSQLSession(AValue: TmncSQLSession);
  protected
    function CreateCMD(SQL: string): TmncSQLCommand;
  public
    property Session: TmncSQLSession read GetSQLSession write SetSQLSession;
  end;

  { TmncMetaType }

  TmncMetaType = record
    SQLType: Integer;
    TypeName: string;
  end;

implementation

{ TmncSQLMeta }

function TmncSQLMeta.GetSQLSession: TmncSQLSession;
begin
  Result := Link as TmncSQLSession;
end;

procedure TmncSQLMeta.SetSQLSession(AValue: TmncSQLSession);
begin
  inherited Link := AValue;
end;

function TmncSQLMeta.CreateCMD(SQL: string): TmncSQLCommand;
begin
  Result := Session.CreateCommand;
  Result.SQL.Text := SQL;
end;

{ TmncMetaItems }

function TmncMetaItems.GetValues(Index: string): string;
var
  itm: TmncMetaItem;
begin
  itm := Find(Index);
  if itm <> nil then
    Result := itm.Value
  else
    Result := '';
end;

procedure TmncMetaItems.SetValues(Index: string; AValue: string);
begin

end;

constructor TmncMetaItems.Create;
begin
  inherited Create;
  FHeader := TmncMetaHeader.Create;
end;

destructor TmncMetaItems.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

function TmncMetaItems.Add(Name: string; Value: string): TmncMetaItem;
begin
  Result := TmncMetaItem.Create;
  Result.Name := Name;
  Result.Value := Value;
  Add(Result);
end;

procedure TmncMetaItems.Clone(AMetaItems: TmncMetaItems);
var
  i: Integer;
  a: TmncMetaItem;
begin
  Clear;
  if AMetaItems <> nil then
  begin
    for i := 0 to AMetaItems.Count -1 do
    begin
      a := TmncMetaItem.Create;
      a.Clone(AMetaItems.Items[i]);
      Add(a);
    end;
  end;
end;

{TODO: delete it
const
  NEWLINE = #13#10;
  TERM = ';';
  ProcTerm = '^';

  }

{ TmncMeta }

function TmncMeta.GetSession: TmncSession;
begin
  Result := Link as TmncSession;
end;

procedure TmncMeta.SetSession(AValue: TmncSession);
begin
  inherited Link := AValue;
end;

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

procedure TmncMetaItem.Clear;
begin
  Name := '';
  FSQLName := '';
  FFullName := '';
  FValue := '';
end;

procedure TmncMetaItem.Clone(AMetaItem: TmncMetaItem);
var
  i: Integer;
  aField: TmnField;
begin
  Clear;
  if AMetaItem <> nil then
  begin
    Name := AMetaItem.Name;
    FSQLName := AMetaItem.SQLName;
    FFullName := AMetaItem.FullName;
    FValue := AMetaItem.Value;

    for i := 0 to AMetaItem.Attributes.Count -1 do
    begin
      with AMetaItem.Attributes.Items[i] do
      begin
        Attributes.Add(Name, Value);
      end;
    end;
  end;
end;

procedure TmncMeta.EnumObjects(Meta: TmncMetaItems; Kind: TmetaKind; SQLName: string; Options: TmetaEnumOptions);
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
    else ;
  end;
end;

procedure TmncMeta.EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncMeta.EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncMeta.EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumDomains(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumConstraints(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumTriggers(Meta: TmncMetaItems;
  SQLName: string; Options: TmetaEnumOptions);
begin

end;

procedure TmncMeta.EnumIndices(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TmetaEnumOptions = []);
begin

end;

procedure TmncMeta.GetViewSource(Strings: TStringList; SQLName: string;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncMeta.GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMeta.GenerateSchema(ORM: TmncORM; Callback: TmncSQLCallback);
begin
end;

initialization
finalization
end.
