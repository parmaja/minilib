unit mncORM;

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Contnrs,
  mnClasses,mncConnections, mncCommons;

type

  TormObject = class;

  { TormObject }

  TormObject = class(TmnNamedObjectList<TormObject>)
  private
    FComment: String;
    FName: String;
    FParent: TormObject;
    FRoot: TormObject;
    FTags: String;
  protected
    procedure Added(Item: TormObject); override;
  public
    constructor Create(AParent: TormObject; AName: String);
    property Comment: String read FComment write FComment;
    function This: TormObject; //I wish i have templates/meta programming in pascal
    property Root: TormObject read FRoot;
    property Parent: TormObject read FParent;

    property Name: String read FName write FName;
    property Tags: String read FTags write FTags; //etc: 'Key,Data'
  end;

  { TCallbackObject }

  TCallbackObjectOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TCallbackObject = class(TObject)
  private
    FParams: TStringList;
    FCallbackObject: TObject;
  public
    Index: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Add(S: string; Options: TCallbackObjectOptions = []); virtual; abstract;
    property CallbackObject: TObject read FCallbackObject write FCallbackObject;
    property Params: TStringList read FParams;
  end;

  { TmncORM }

  TmncORM = class(TormObject)
  protected
  public
    type
    TormSQLObject = class;

    { TormHelper }

    TormHelper = class(TObject)
    protected
      procedure GenerateObjects(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
      function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; virtual; abstract;
    end;

    TormHelperClass = class of TormHelper;

    { TormSQLObject }

    TormSQLObject = class(TormObject)
    protected
      HelperClass: TormHelperClass;
      procedure Created; override;
    public
      function GenerateSQL(SQL: TCallbackObject; vLevel: Integer): Boolean;
    end;

    TormObjectClass = class of TormSQLObject;

    { TDatabase }

    TDatabase = class(TormSQLObject)
    public
      constructor Create(AORM: TmncORM; AName: String);
      function This: TDatabase;
    end;

    { TormSchema }

    TSchema = class(TormSQLObject)
    public
      constructor Create(ADatabase: TDatabase; AName: String);
      function This: TSchema;
    end;

    TFields = class;

    { TTable }

    TTable = class(TormSQLObject)
    protected
      procedure Added(Item: TormObject); override;
    public
      Fields: TFields;
      constructor Create(ASchema: TSchema; AName: String);
      function This: TTable;
    end;

    { TFields }

    TFields = class(TormSQLObject)
    public
      constructor Create(ATable: TTable);
      function This: TFields;
    end;

    TormFieldOption = (
      foReferenced,
      foInternal, //Do not display for end user
      foSummed, //
      foPrimary,
      foSequenced, //or AutoInc
      foNotNull, //or required
      foIndexed
    );

    TormFieldOptions = set of TormFieldOption;
    TormFieldType = (ftString, ftBoolean, ftInteger, ftCurrency, ftFloat, ftDate, ftTime, ftDateTime, ftMemo, ftBlob);

    { TField }

    TField = class(TormSQLObject)
    private
      FDefaultValue: Variant;
      FFieldSize: Integer;
      FFieldType: TormFieldType;
      FOptions: TormFieldOptions;
      FRefTableName: String;
      FRefFieldName: String;
      FRefField: TField;
      function GetIndexed: Boolean;
      function GetPrimary: Boolean;
      function GetSequenced: Boolean;
      procedure SetIndexed(AValue: Boolean);
      procedure SetPrimary(AValue: Boolean);
      procedure SetSequenced(AValue: Boolean);
    public
      constructor Create(AFields: TFields; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions = []);
      function Parent: TTable;
      procedure Reference(ATableName: String; AFieldName: String);
      property Options: TormFieldOptions read FOptions write FOptions;
      property DefaultValue: Variant read FDefaultValue write FDefaultValue;

      property FieldType: TormFieldType read FFieldType write FFieldType;
      property FieldSize: Integer read FFieldSize write FFieldSize;

      property Indexed: Boolean read GetIndexed write SetIndexed;
      property Primary: Boolean read GetPrimary write SetPrimary;
      property Sequenced: Boolean read GetSequenced write SetSequenced;
    end;

    { StoredProcedure }

    TStoredProcedure = class(TormSQLObject)
    private
      FCode: String;
    public
      property Code: String read FCode write FCode;
    end;

    { Trigger }

    TTrigger = class(TormSQLObject)
    private
      FCode: String;
    public
      property Code: String read FCode write FCode;
    end;

  public
    type

    TRegObject = class(TObject)
    public
      ObjectClass: TormObjectClass;
      HelperClass: TormHelperClass;
    end;

    { TRegObjects }

    TRegObjects = class(TmnObjectList<TRegObject>)
    public
      function FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
      function FindHelper(AObjectClass: TormObjectClass): TormHelperClass;
    end;

  private
    FObjectClasses: TRegObjects;

  protected

  public
    constructor Create(AName: String); virtual;
    destructor Destroy; override;
    function This: TmncORM;

    function CreateDatabase(AName: String): TDatabase;
    function CreateSchema(ADatabase: TDatabase; AName: String): TSchema;
    function CreateTable(ASchema: TSchema; AName: String): TTable;
    function CreateField(ATable: TTable; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions = []): TField;

    function GenerateSQL(Callback: TCallbackObject): Boolean; overload;
    function GenerateSQL(vSQL: TStrings): Boolean; overload;

    procedure Register(AObjectClass: TormObjectClass; AHelperClass: TormHelperClass);
    property ObjectClasses: TRegObjects read FObjectClasses;
  end;

  TmncORMClass = class of TmncORM;

function LevelStr(vLevel: Integer): String;

implementation

{ TCallbackObject }

constructor TCallbackObject.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TCallbackObject.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

{ TmncORM.TormHelper }

procedure TmncORM.TormHelper.GenerateObjects(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
var
  o: TormObject;
begin
  for o in AObject do
    (o as TormSQLObject).GenerateSQL(SQL, vLevel);
end;

{ TmncORM.TormSQLObject }

procedure TmncORM.TormSQLObject.Created;
begin
  inherited Created;
  if (FRoot <> nil) then
    HelperClass := (Root as TmncORM).ObjectClasses.FindHelper(TormObjectClass(ClassType));
end;

function TmncORM.TormSQLObject.GenerateSQL(SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  helper: TormHelper;
begin
  if HelperClass <> nil then
  begin
    helper := HelperClass.Create;
    helper.ProduceSQL(self, SQL, vLevel);
  end;
end;

{ TmncORM.TormHelper }

{ TmncORM.TRegObjects }

function TmncORM.TRegObjects.FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
var
  o: TRegObject;
begin
  for o in Self do
  begin
    if o.ObjectClass.ClassParent = AObjectClass then
    begin
      Result := o.ObjectClass;
      break;
    end;
  end;
end;

function TmncORM.TRegObjects.FindHelper(AObjectClass: TormObjectClass): TormHelperClass;
var
  o: TRegObject;
begin
  for o in Self do
  begin
    if o.ObjectClass = AObjectClass then
    begin
      Result := o.HelperClass;
      break;
    end;
  end;
end;

{ TmncORM.TFields }

constructor TmncORM.TFields.Create(ATable: TTable);
begin
  inherited Create(ATable, '');
  ATable.Fields := Self;
end;

function TmncORM.TFields.This: TFields;
begin
  Result := Self;
end;

{ TmncORM }

constructor TmncORM.Create(AName: String);
begin
  inherited Create(nil, AName);
  FObjectClasses := TRegObjects.Create;
end;

destructor TmncORM.Destroy;
begin
  inherited Destroy;
end;

function TmncORM.This: TmncORM;
begin
  Result := Self;
end;

function TmncORM.CreateDatabase(AName: String): TDatabase;
begin
  Result := TDatabase.Create(Self, AName);
end;

function TmncORM.CreateSchema(ADatabase: TDatabase; AName: String): TSchema;
begin
  Result := TSchema.Create(ADatabase, AName);
end;

function TmncORM.CreateTable(ASchema: TSchema; AName: String): TTable;
begin
  Result := TTable.Create(ASchema, AName);
  TFields.Create(Result); //will be assigned to Fields in TFields.Create
end;

function TmncORM.CreateField(ATable: TTable; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions): TField;
begin
  Result := TField.Create(ATable.Fields, AName, AFieldType, AOptions);
end;

procedure TmncORM.Register(AObjectClass: TormObjectClass; AHelperClass: TormHelperClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.HelperClass := AHelperClass;
  ObjectClasses.Add(aRegObject);
end;

{ TField }

function TmncORM.TField.GetIndexed: Boolean;
begin
  Result := foIndexed in Options;
end;

function TmncORM.TField.GetPrimary: Boolean;
begin
  Result := foPrimary in Options;
end;

function TmncORM.TField.GetSequenced: Boolean;
begin
  Result := foSequenced in Options;
end;

procedure TmncORM.TField.SetIndexed(AValue: Boolean);
begin
  if AValue then
    Options := Options + [foIndexed]
  else
    Options := Options - [foIndexed];
end;

procedure TmncORM.TField.SetPrimary(AValue: Boolean);
begin
  if AValue then
    Options := Options + [foPrimary]
  else
    Options := Options - [foPrimary];
end;

procedure TmncORM.TField.SetSequenced(AValue: Boolean);
begin
  if AValue then
    Options := Options + [foSequenced]
  else
    Options := Options - [foSequenced];
end;

constructor TmncORM.TField.Create(AFields: TFields; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions);
begin
  inherited Create(AFields, AName);
  FOptions := AOptions;
  FFieldType := AFieldType;
end;

function TmncORM.TField.Parent: TTable;
begin
  Result := inherited Parent as TTable;
end;

procedure TmncORM.TField.Reference(ATableName: String; AFieldName: String);
begin
end;

{ TTable }

function TmncORM.TTable.This: TTable;
begin
  Result := Self;
end;

procedure TmncORM.TTable.Added(Item: TormObject);
begin
  inherited Added(Item);
  if Item is TFields then
  begin
    if Fields = nil then
      Fields := Item as TFields
    else
      raise Exception.Create('You cannot Send Fields twice');
  end;
end;

constructor TmncORM.TTable.Create(ASchema: TSchema; AName: String);
begin
  inherited Create(ASchema, AName);
  //TFields := TFields.Create(Self); //hmmmmmm :J
end;

{ TSchema }

function TmncORM.TSchema.This: TSchema;
begin
  Result := Self;
end;

constructor TmncORM.TSchema.Create(ADatabase: TDatabase; AName: String);
begin
  inherited Create(ADatabase, AName);
end;

{ TormObject }

function TormObject.This: TormObject;
begin
  Result := Self;
end;

procedure TormObject.Added(Item: TormObject);
begin
  inherited Added(Item);
end;

function LevelStr(vLevel: Integer): String;
begin
  Result := StringOfChar(' ', vLevel * 4);
end;

constructor TormObject.Create(AParent: TormObject; AName: String);
begin
  inherited Create;
  FName := AName;
  if AParent <> nil then
  begin
    FParent := AParent;
    AParent.Add(Self);
    FRoot := AParent.Root;
  end
  else
    FRoot := Self;
end;

{ TDatabase }

function TmncORM.TDatabase.This: TDatabase;
begin
  Result := Self;
end;

constructor TmncORM.TDatabase.Create(AORM: TmncORM; AName: String);
begin
  inherited Create(AORM, AName);
end;

type

  { TSQLCallbackObject }

  TSQLCallbackObject = class(TCallbackObject)
  public
    Buffer: string;
    SQL: TStrings;
    destructor Destroy; override;
    procedure Add(S: string; Options: TCallbackObjectOptions = []); override;
  end;

{ TSQLCallbackObject }

destructor TSQLCallbackObject.Destroy;
begin
  if Buffer <> '' then
    Add('', [cboEndLine]);
  inherited Destroy;
end;

procedure TSQLCallbackObject.Add(S: string; Options: TCallbackObjectOptions);
begin
  Buffer := Buffer + S;
  if (cboEndLine in Options) or (cboEndChunk in Options) then
  begin
    if Buffer <> '' then
      SQL.Add(Buffer);
    Buffer := '';
  end;
  if (cboEndChunk in Options) and (SQL.Count > 0) then
  begin
    SQL.Add('^');
    SQL.Add(' ');
  end;
end;

function TmncORM.GenerateSQL(vSQL: TStrings): Boolean;
var
  SQLCB: TSQLCallbackObject;
begin
  SQLCB := TSQLCallbackObject.Create;
  SQLCB.SQL := vSQL;
  SQLCB.Add('--SQL Generated By ORM object', [cboEndChunk]);
  SQLCB.Add('');
  GenerateSQL(SQLCB);
  FreeAndNil(SQLCB);
end;

function TmncORM.GenerateSQL(Callback: TCallbackObject): Boolean;
var
  AParams: TStringList;
  o: TormSQLObject;
  helper: TormHelper;
begin
  AParams := TStringList.Create;
  try
    for o in Self do
    begin
      if o.HelperClass <> nil then
      begin
        helper := o.HelperClass.Create;
        helper.ProduceSQL(o, Callback, 0);
      end;
    end;
  finally
    FreeAndNil(AParams);
  end;
  Result := True;
end;

end.
