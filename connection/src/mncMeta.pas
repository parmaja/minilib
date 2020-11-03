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
    FKind: TmetaKind;
    FAttributes: TmncMetaAttributes;
  public
    constructor Create;
    destructor Destroy; override;
    property Kind: TmetaKind read FKind write FKind;
    property Attributes: TmncMetaAttributes read FAttributes;
  end;

  { TmncMetaItems }

  TmncMetaItems = class(TmnObjectList<TmncMetaItem>)
  private
    FHeader: TmncMetaHeader;
  public
    constructor Create;
    destructor Destroy; override;
    property Header: TmncMetaHeader read FHeader;
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

function TmncMetaItems.Add(Name: string): TmncMetaItem;
begin
  Result := TmncMetaItem.Create;
  Result.Name := Name;
  Add(Result);
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
