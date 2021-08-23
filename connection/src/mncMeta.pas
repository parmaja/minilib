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
               sokHost, sokSchema, sokDatabase, sokTable, sokView,
               sokProcedure, sokFunction, sokException, sokRole,
               sokTrigger, sokSequence, sokForeign, sokIndex, sokConstraint,
               sokField, sokOperator, sokProperty,
               sokType, sokDomain);

  TmetaEnumOption = (ekExtra, ekAlter, ekSystem, ekSort);
  TmetaEnumOptions = set of TmetaEnumOption;

  { TmncMetaAttribute }

  TmncMetaAttribute = class(TmnNameValueObject);

  TmncMetaAttributes = class(TmnNameValueObjectList<TmncMetaAttribute>)
  private
  public
  end;

  { TmncMetaItem }

  TmncMetaItem = class(TmnNamedObject)
  private
    FDefinitions: TmncMetaAttributes;
    FKind: TmetaKind;
    //FMaster: string;
    FSQLName: string; //it is the real name
    FAttributes: TmncMetaAttributes;
    FSQLType: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Kind: TmetaKind read FKind write FKind;
    procedure Clear;
    procedure Clone(AMetaItem: TmncMetaItem);
    //Copy but do not override exists one
    procedure CopyDefinitions(vMetaItem: TmncMetaItem; Force: Boolean = False);

    //property Master: string read FMaster write FMaster; //for Field it is Table for Table it is Database
    property SQLType: string read FSQLType write FSQLType; //Table, Field, Trigger ...
    property SQLName: string read FSQLName write FSQLName; //same as Name but with special for SQL like quotes, Table name, Field name etc ...
    property Attributes: TmncMetaAttributes read FAttributes; //from SQL engine
    property Definitions: TmncMetaAttributes read FDefinitions; //from SQL engine
  end;

  { TmncMetaItems }

  TmncMetaItems = class(TmnNamedObjectList<TmncMetaItem>)
  private
  public
    destructor Destroy; override;
    procedure Clone(AMetaItems: TmncMetaItems);
    function Add(Name: string): TmncMetaItem; overload;
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
    // Do not pass SQLName to SQLName param, it should qoute it at this level
    //
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); virtual;
    procedure EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); virtual;
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
    //Sources
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TmetaEnumOptions = []); virtual;
    procedure GetViewSource(Strings: TStringList; SQLName: string; Options: TmetaEnumOptions = []); virtual;
    procedure GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); virtual;

    function GetNewFieldSQL(TableName: string): string; virtual;

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
    function KindToStr(AKind: TmetaKind): string;
    function GetSQLSession: TmncSQLSession;
    procedure SetSQLSession(AValue: TmncSQLSession);
  protected
    function GetSortSQL(Options: TmetaEnumOptions; FieldName: string = 'name'): string;
    function DoCreateConnection: TmncSQLConnection; virtual;//abstract
    function CreateConnection: TmncSQLConnection;
    function QuoteIt(S: string): string; virtual;
    procedure FetchCMD(Strings: TStringList; FieldName, SQL: string);

    //FieldName the name of field contain name, some sql cant alt the fields name, like SHOW TABLES in mysql
    procedure EnumCMD(ASession: TmncSQLSession; Meta: TmncMetaItems; vKind: TmetaKind; FieldName, ItemType, SQL: string; Fields: array of string); overload; virtual;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; FieldName, SQL: string); overload;
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string); overload;
    function CreateCMD(ASession: TmncSQLSession; SQL: string): TmncSQLCommand; overload;
    function CreateCMD(SQL: string): TmncSQLCommand; overload;
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

function TmncSQLMeta.KindToStr(AKind: TmetaKind): string;
begin
  case AKind of
    sokNone: Result := 'None';
    sokMeta: Result := 'Meta';
    sokData: Result := 'Data';
    sokHost: Result := 'Host';
    sokSchema: Result := 'Schema';
    sokDatabase: Result := 'Database';
    sokTable: Result := 'Table';
    sokView: Result := 'View';
    sokProcedure: Result := 'Procedure';
    sokFunction: Result := 'Function';
    sokException: Result := 'Exception';
    sokRole: Result := 'Role';
    sokTrigger: Result := 'Trigger';
    sokSequence: Result := 'Sequence';
    sokForeign: Result := 'Foreign';
    sokIndex: Result := 'Index';
    sokConstraint: Result := 'Constraint';
    sokField: Result := 'Field';
    sokOperator: Result := 'Operator';
    sokProperty: Result := 'Property';
    sokType: Result := 'Type';
    sokDomain: Result := 'Domain';
  end;
end;

function TmncSQLMeta.GetSQLSession: TmncSQLSession;
begin
  Result := Link as TmncSQLSession;
end;

procedure TmncSQLMeta.SetSQLSession(AValue: TmncSQLSession);
begin
  inherited Link := AValue;
end;

function TmncSQLMeta.GetSortSQL(Options: TmetaEnumOptions; FieldName: string): string;
begin
  if ekSort in Options then
    Result := ' order by ' + FieldName
  else
    Result := '';
end;

function TmncSQLMeta.DoCreateConnection: TmncSQLConnection;
begin
  Result := nil;
end;

function TmncSQLMeta.CreateConnection: TmncSQLConnection;
begin
  Result := DoCreateConnection;
  if Result <> nil then
    Result.ServerInfo := ServerInfo;
end;

function TmncSQLMeta.QuoteIt(S: string): string;
begin
  Result := S;
end;

procedure TmncSQLMeta.FetchCMD(Strings: TStringList; FieldName, SQL: string);
var
  aCMD: TmncSQLCommand;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      Strings.Add(aCMD.Field[FieldName].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLMeta.EnumCMD(ASession: TmncSQLSession; Meta: TmncMetaItems; vKind: TmetaKind; FieldName, ItemType, SQL: string; Fields: array of string);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
  i: Integer;
begin
  aCMD := CreateCMD(ASession, SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := Meta.Add(aCMD.Field[FieldName].AsString);
      aItem.SQLName := QuoteIt(aItem.Name);
      aItem.Kind := vKind;

      aItem.SQLType := ItemType;

      if ItemType <> '' then
      begin
        aItem.Definitions['Type'] := ItemType;
        aItem.Definitions[ItemType] := aItem.Name;
      end;

      for i := Low(Fields) to High(Fields) do
        aItem.Attributes.Add(Fields[i], aCMD.Field[Fields[i]].AsString);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncSQLMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; FieldName, SQL: string);
begin
  EnumCMD(Session, Meta, vKind, FieldName, KindToStr(vKind), SQL, []);
end;

procedure TmncSQLMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string);
begin
  EnumCMD(Meta, vKind, 'name', SQL);
end;

function TmncSQLMeta.CreateCMD(ASession: TmncSQLSession; SQL: string): TmncSQLCommand;
begin
  Result := ASession.CreateCommand;
  Result.SQL.Text := SQL;
end;

function TmncSQLMeta.CreateCMD(SQL: string): TmncSQLCommand;
begin
  Result := CreateCMD(Session, SQL);
end;

{ TmncMetaItems }

destructor TmncMetaItems.Destroy;
begin
  inherited;
end;

function TmncMetaItems.Add(Name: string): TmncMetaItem;
begin
  Result := TmncMetaItem.Create;
  Result.Name := Name;
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
  FDefinitions := TmncMetaAttributes.Create;
end;

destructor TmncMetaItem.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FDefinitions);
  inherited;
end;

procedure TmncMetaItem.Clear;
begin
  Name := '';
  FSQLName := '';
  Attributes.Clear;
  Definitions.Clear;
end;

procedure TmncMetaItem.Clone(AMetaItem: TmncMetaItem);
var
  i: Integer;
begin
  Clear;
  if AMetaItem <> nil then
  begin
    Name := AMetaItem.Name;
    FSQLName := AMetaItem.SQLName;

    for i := 0 to AMetaItem.Attributes.Count -1 do
    begin
      with AMetaItem.Attributes.Items[i] do
      begin
        Attributes.Add(Name, Value);
      end;
    end;

    for i := 0 to AMetaItem.Definitions.Count -1 do
    begin
      with AMetaItem.Definitions.Items[i] do
      begin
        Definitions.Add(Name, Value);
      end;
    end;
  end;
end;

procedure TmncMetaItem.CopyDefinitions(vMetaItem: TmncMetaItem; Force: Boolean);
var
  i: Integer;
  aAttribute: TmncMetaAttribute;
begin
  for i := 0 to vMetaItem.Definitions.Count -1 do
  begin
    aAttribute := Definitions.Find(vMetaItem.Definitions.Items[i].Name);
    if (aAttribute = nil) then
      Definitions.Add(vMetaItem.Definitions.Items[i].Name, vMetaItem.Definitions.Items[i].Value)
    else if Force then
    begin
      aAttribute.Name := vMetaItem.Definitions.Items[i].Name;
      aAttribute.Value := vMetaItem.Definitions.Items[i].Value;
    end;
  end;
end;

procedure TmncMeta.EnumObjects(Meta: TmncMetaItems; Kind: TmetaKind; SQLName: string; Options: TmetaEnumOptions);
begin
  case Kind of
    sokDatabase: ;
    sokDomain: EnumDomains(Meta, Options);
    sokTable: EnumTables(Meta, SQLName, Options);
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

procedure TmncMeta.EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
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

function TmncMeta.GetNewFieldSQL(TableName: string): string;
begin
  //Standard
  Result := 'alter table ' + TableName + ' add ?FieldName ?FieldType';
end;

procedure TmncMeta.GenerateSchema(ORM: TmncORM; Callback: TmncSQLCallback);
begin
end;

initialization
finalization
end.
