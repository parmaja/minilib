unit mncORM;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, Contnrs, Variants,
  mnClasses;

type

  TmncORM = class;
  TormObject = class;
  TormObjectClass = class of TormObject;

  { TormObject }

  TormObject = class(TmnObjectList<TormObject>)
  private
    FComment: String;
    FName: String;
    FParent: TormObject;
    FRoot: TmncORM;
    FTags: String;
  protected
    procedure Added(Item: TormObject); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TormObjectClass; AName: string; RaiseException: Boolean = false): TormObject;
  public
    constructor Create(AParent: TormObject; AName: String);

    function Find(const Name: string): TormObject;
    function IndexOfName(vName: string): Integer;

    property Comment: String read FComment write FComment;
    function This: TormObject; //I wish i have templates/meta programming in pascal
    property Root: TmncORM read FRoot;
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
      TormSQLObjectClass = class of TormSQLObject;

      TFieldFilter = set of (
        ffSelect,
        ffView, //show it to end user
        ffData
      );

      TTable = class;

      { TormGenerator }

      TormGenerator = class(TObject)
      private
      protected
        procedure DefaultGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; virtual;
      public
        procedure GenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
        constructor Create; virtual; //usfull for creating it by GeneratorClass.Create
      end;

      TormGeneratorClass = class of TormGenerator;

      TormTableGenerator = class(TormGenerator)
      protected
        //temporary here, must moved to Table Generator
        function SelectSQL(AObject: TTable; AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string; virtual;
        function InsertSQL(AObject: TTable; AFilter: TFieldFilter; ExtraFields: array of string): string; virtual;
        function UpdateSQL(AObject: TTable; AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string; virtual;
        function SaveSQL(AObject: TTable; AFilter: TFieldFilter; Updating, Returning: Boolean; Keys: array of string): string; virtual;
      end;

      { TormSQLObject }

      TormSQLObject = class(TormObject)
      private
        FGeneratorClass: TormGeneratorClass;
      protected
        procedure SetGeneratorClass(AValue: TormGeneratorClass); virtual;
        property GeneratorClass: TormGeneratorClass read FGeneratorClass write SetGeneratorClass;
        procedure Created; override;
      public
        function SQLName: string; virtual;
        function QuotedSQLName: string; virtual;
        procedure GenSQL(SQL: TCallbackObject; vLevel: Integer);
      end;

      { TDatabase }

      TDatabase = class(TormSQLObject)
      private
        FVersion: integer;
      public
        constructor Create(AORM: TmncORM; AName: String);
        function This: TDatabase;
        property Version: integer read FVersion write FVersion;
      end;

      { TormSchema }

      TSchema = class(TormSQLObject)
      public
        constructor Create(ADatabase: TDatabase; AName: String);
        function This: TSchema;
      end;

      TField = class;
      TFields = class;
      TIndexes = class;
      TReferences = class;

      { TTable }

      TTable = class(TormSQLObject)
      protected
        procedure Added(Item: TormObject); override;
        procedure SetGeneratorClass(AValue: TormGeneratorClass); override;
      public
        Prefix: string; //used to added to generated field name, need more tests
        Fields: TFields;
        VirtualFields: TFields;
        Indexes: TIndexes;
        References: TReferences;
        constructor Create(ASchema: TSchema; AName: String; APrefix: string = '');
        function SelectSQL(AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string;
        function SaveSQL(Updating, Returning: Boolean; AFilter: TFieldFilter; Keys: array of string): string;
        procedure GenAliases(Strings: TStringList);
        function This: TTable;
      end;

      TFields = class(TormSQLObject)
      public
        constructor Create(ATable: TTable);
        function This: TFields;
      end;

      { TVirtualFields }

      TVirtualFields = class(TFields)
      public
        function This: TVirtualFields;
      end;

      { TIndexes }

      TIndexes = class(TormSQLObject)
      public
        constructor Create(ATable: TTable);
        function This: TIndexes;
      end;

      { TReferences }

      TReferences = class(TormSQLObject)
      public
        constructor Create(ATable: TTable);
        function This: TReferences;
      end;

      TormFieldOption = (
        foReferenced,
        foInternal, //Do not display for end user
        foPrimary,
        foSequenced, //or AutoInc
        foNotNull, //or required
        foUnique,
        foIndexed
      );
      TormFieldOptions = set of TormFieldOption;

      TormFieldType = (ftString, ftBoolean, ftSmallInteger, ftInteger, ftBigInteger, ftCurrency, ftFloat, ftDate, ftTime, ftDateTime, ftText, ftBlob);

      TormReferenceOption = (
        rfoNothing,
        rfoRestrict,
        rfoCascade,
        rfoSetNull
      );

      TReferenceInfoStr = record
        Table: string;
        Field: string;
        UpdateOptions: TormReferenceOption;
        DeleteOptions: TormReferenceOption;
      end;

      TReferenceInfoLink = record
        Table: TTable;
        Field: TField;
        UpdateOptions: TormReferenceOption;
        DeleteOptions: TormReferenceOption;
      end;

      { TField }

      TField = class(TormSQLObject)
      private
        FDefaultValue: Variant;
        FFieldSize: Integer;
        FFieldType: TormFieldType;
        FFilter: TFieldFilter;
        FIndex: string;
        FOptions: TormFieldOptions;
        FTitle: string;
        function GetIndexed: Boolean;
        function GetPrimary: Boolean;
        function GetSequenced: Boolean;
        procedure SetIndexed(AValue: Boolean);
        procedure SetPrimary(AValue: Boolean);
        procedure SetSequenced(AValue: Boolean);
      protected
        ReferenceInfoStr: TReferenceInfoStr;
        procedure Check; override;
        function Table: TTable;
      public
        ReferenceInfo: TReferenceInfoLink;
        constructor Create(AFields: TFields; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions = []);
        function SQLName: string; override;
        function Parent: TFields;
        property Options: TormFieldOptions read FOptions write FOptions;
        property Filter: TFieldFilter read FFilter write FFilter;
        property DefaultValue: Variant read FDefaultValue write FDefaultValue;
        procedure ReferenceTo(TableName, FieldName: string; UpdateOptions, DeleteOptions: TormReferenceOption);

        property Title: string read FTitle write FTitle;

        property FieldType: TormFieldType read FFieldType write FFieldType;
        property FieldSize: Integer read FFieldSize write FFieldSize;

        property Indexed: Boolean read GetIndexed write SetIndexed;
        property Primary: Boolean read GetPrimary write SetPrimary;
        property Sequenced: Boolean read GetSequenced write SetSequenced;

        property Index: string read FIndex write FIndex;// this will group this field have same values into one index with that index name
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

      TFieldValue = class(TormObject)
      public
        Value: Variant;
      end;

      { TInsertData }

      TInsertData = class(TormSQLObject)
      private
      protected
        FTable: TTable;
        procedure Check; override;
      public
        constructor Create(ADatabase: TDatabase; ATableName: string);
        procedure AddValue(FieldName, FieldValue: Variant);
        property Table: TTable read FTable;
      end;

    public
      type

        TRegObject = class(TObject)
        public
          ObjectClass: TormObjectClass;
          GeneratorClass: TormGeneratorClass;
        end;

        { TRegObjects }

        TRegObjects = class(TmnObjectList<TRegObject>)
        public
          function FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
          function FindGenerator(AObjectClass: TormObjectClass): TormGeneratorClass;
        end;

  private
    FImpact: Boolean;
    FObjectClasses: TRegObjects;
    FQuoteChar: string;
    FUsePrefexes: Boolean;

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

    procedure RegisterGenerator(AObjectClass: TormObjectClass; AGeneratorClass: TormGeneratorClass);
    property ObjectClasses: TRegObjects read FObjectClasses;
    property QuoteChar: string read FQuoteChar write FQuoteChar; //Empty, it will be used with SQLName
    property UsePrefexes: Boolean read FUsePrefexes write FUsePrefexes; //option to use Prefex in Field names
    property Impact: Boolean read FImpact write FImpact; //use inline peroperty of members
  end;

  TmncORMClass = class of TmncORM;

//Common fields

  TNameField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String = '');
  end;

  TStringField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions = []);
  end;

  TDateTimeField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions = []);
  end;

  TIntegerField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions = []);
  end;

  TBooleanField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String);
  end;

  TCurrencyField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String);
  end;

  TCommentField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String = '');
  end;

  TIDField = class(TmncORM.TField)
  public
    constructor Create(AFields: TmncORM.TFields; AName: String = '');
  end;

  TRefIDField = class(TmncORM.TField) //Abort it it used in detail table
  public
    constructor Create(AFields: TmncORM.TFields; AName, AMasterTable: String; AMasterID: string = ''; AOptions: TmncORM.TormFieldOptions = []);
  end;

  TRefStringField = class(TmncORM.TField) //Abort it it used in detail table
  public
    constructor Create(AFields: TmncORM.TFields; AName, AMasterTable: String; AMasterField: string; AOptions: TmncORM.TormFieldOptions = []);
  end;

  TRefDetailField = class(TmncORM.TField) //Delete record when master deleted
  public
    constructor Create(AFields: TmncORM.TFields; AName, AMasterTable: String; AMasterID: string = '');
  end;

function LevelStr(vLevel: Integer): String;
function ValueToStr(vValue: Variant): string;

implementation

{ TmncORM.TFields }

constructor TmncORM.TFields.Create(ATable: TTable);
begin
  inherited Create(ATable, '');
  if (ATable <> nil) then
  begin
    if (Self is TVirtualFields) then
      ATable.VirtualFields := Self
    else
      ATable.Fields := Self;
  end;
end;

function TmncORM.TFields.This: TFields;
begin
  Result := Self;
end;

{ TmncORM.TVirtualFields }

function TmncORM.TVirtualFields.This: TVirtualFields;
begin
  Result := Self;
end;

{ TmncORM.TInsertData }

procedure TmncORM.TInsertData.Check;
begin
  inherited Check;
  if FTable = nil then
    FTable := Root.FindObject(TTable, Name, true) as TTable;
end;

constructor TmncORM.TInsertData.Create(ADatabase: TDatabase; ATableName: string);
begin
  inherited Create(ADatabase, ATableName);
end;

procedure TmncORM.TInsertData.AddValue(FieldName, FieldValue: Variant);
begin
  with TFieldValue.Create(Self, FieldName) do
  begin
    Value := FieldValue;
  end;
end;

{ TmncORM.TReferences }

constructor TmncORM.TReferences.Create(ATable: TTable);
begin
  inherited Create(ATable, '');
  if ATable <> nil then
    ATable.References := Self;
end;

function TmncORM.TReferences.This: TReferences;
begin
  Result := Self;
end;

{ TmncORM.TIndexes }

constructor TmncORM.TIndexes.Create(ATable: TTable);
begin
  inherited Create(ATable, '');
  if ATable <> nil then
    ATable.Indexes := Self;
end;

function TmncORM.TIndexes.This: TIndexes;
begin
  Result := Self;
end;

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

{ TmncORM.TormGenerator }

procedure TmncORM.TormGenerator.DefaultGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
var
  o: TormObject;
begin
  for o in AObject do
    (o as TormSQLObject).GenSQL(SQL, vLevel);
end;

function TmncORM.TormGenerator.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  Result := False;
end;

procedure TmncORM.TormGenerator.GenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer);
begin
  if not DoGenerateSQL(AObject, SQL, vLevel) then
    DefaultGenerateSQL(AObject, SQL, vLevel);
end;

constructor TmncORM.TormGenerator.Create;
begin
  inherited Create;
end;

function TmncORM.TormTableGenerator.SelectSQL(AObject: TTable; AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string;
var
  o: TormObject;
  i: Integer;
  b: Boolean;
begin
  with AObject  do
  begin
    Result := 'select ';
    b := False;
    for i := 0 to Length(ExtraFields) - 1 do
    begin
      if b then
        Result := Result + ', '
      else
        b := True;
      Result := Result + ExtraFields[i];
    end;

    for o in Fields do
    begin
      if (AFilter = []) or (AFilter <= (o as TField).Filter) then
      begin
        if b then
          Result := Result + ', '
        else
          b := True;
        Result := Result + (o as TField).SQLName;
      end;
    end;

    Result := Result + ' from ' + This.SQLName + ' ';
    for i := 0 to Length(Keys) - 1 do
    begin
      if i = 0 then
        Result := Result + ' where '
      else
        Result := Result + ' and ';
      Result := Result + Keys[i] + '=?' + Keys[i];
    end;
  end;

end;

function TmncORM.TormTableGenerator.UpdateSQL(AObject: TTable; AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string;
var
  o: TormObject;
  i: Integer;
  b: Boolean;
begin
  with AObject  do
  begin
    with AObject as TTable do
    begin
      Result := 'update ' + SQLName + ' set '#13;
      b := False;
      for i := 0 to Length(ExtraFields) - 1 do
      begin
        if b then
          Result := Result + ', '
        else
          b := True;
        Result := Result + ExtraFields[i] + '=?' + ExtraFields[i];
      end;

      for o in Fields do
      begin
        if (AFilter = []) or (AFilter <= (o as TField).Filter) then
        begin
          if b then
            Result := Result + ', '
          else
            b := True;
          Result := Result + (o as TField).SQLName + '=?' + (o as TField).SQLName;
        end;
      end;

      for i := 0 to Length(Keys) - 1 do
      begin
        if i = 0 then
          Result := Result + #13'where '
        else
          Result := Result + ' and ';
        Result := Result + Keys[i] + '=?' + Keys[i];
      end;
    end;
  end;
end;

function TmncORM.TormTableGenerator.InsertSQL(AObject: TTable; AFilter: TFieldFilter; ExtraFields: array of string): string;
var
  o: TormObject;
  i: Integer;
  b: Boolean;
begin
  with AObject do
  begin
    Result := 'insert into ' + SQLName + ' (';
    b := False;
    for i := 0 to Length(ExtraFields) - 1 do
    begin
      if b then
        Result := Result + ', '
      else
        b := True;
      Result := Result + ExtraFields[i];
    end;

    for o in Fields do
    begin
      if (AFilter = []) or (AFilter <= (o as TField).Filter) then
      begin
        if b then
            Result := Result + ', '
          else
            b := True;
          Result := Result + (o as TField).SQLName;
       end;
    end;

    b := False;
    Result := Result + ') '#13'values (';
    for i := 0 to Length(ExtraFields) - 1 do
    begin
      if b then
        Result := Result + ', '
      else
        b := True;
      Result := Result + '?' + ExtraFields[i];
    end;

    for o in Fields do
    begin
      if b then
        Result := Result + ', '
      else
        b := True;
      Result := Result + '?' + (o as TField).SQLName;
    end;
    Result := Result + ')';
  end;
end;

function TmncORM.TormTableGenerator.SaveSQL(AObject: TTable; AFilter: TFieldFilter; Updating, Returning: Boolean; Keys: array of string): string;
var
  i:Integer;
  b:Boolean;
begin
  with AObject do
  begin
    if Updating then
      Result := UpdateSQL(AObject, AFilter, Keys, [])
    else
    begin
      if Returning then
        Result := InsertSQL(AObject, AFilter, []) //not sure how to work with this
      else
        Result := InsertSQL(AObject, AFilter, []);
    end;

    if not Updating and Returning and (Length(Keys) <> 0) then
    begin
      Result := Result + #13 + 'returning ';
      b := False;
      for i := 0 to Length(Keys) - 1 do
      begin
        if b then
          Result := Result + ', '
        else
          b := True;
        Result := Result + Keys[i];
      end;
    end;
  end;
end;

{ TmncORM.TormSQLObject }

procedure TmncORM.TormSQLObject.SetGeneratorClass(AValue: TormGeneratorClass);
begin
  if FGeneratorClass =AValue then Exit;
  FGeneratorClass :=AValue;
end;

procedure TmncORM.TormSQLObject.Created;
begin
  inherited Created;
  if (FRoot <> nil) then
    GeneratorClass := (Root as TmncORM).ObjectClasses.FindGenerator(TormObjectClass(ClassType));
end;

function TmncORM.TormSQLObject.SQLName: string;
begin
  Result := Name;
end;

procedure TmncORM.TormSQLObject.GenSQL(SQL: TCallbackObject; vLevel: Integer);
var
  Generator: TormGenerator;
begin
  if GeneratorClass <> nil then
  begin
    Generator := GeneratorClass.Create;
    try
      Generator.GenerateSQL(self, SQL, vLevel);
    finally
      Generator.Free;
    end;
  end;
end;

function TmncORM.TormSQLObject.QuotedSQLName: string;
begin
  Result := SQLName;
  if Root.QuoteChar <> '' then
    Result := Root.QuoteChar + Result + Root.QuoteChar;
end;

{ TmncORM.TRegObjects }

function TmncORM.TRegObjects.FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
var
  o: TRegObject;
begin
  Result := nil;
  for o in Self do
  begin
    if o.ObjectClass.ClassParent = AObjectClass then
    begin
      Result := o.ObjectClass;
      break;
    end;
  end;
end;

function TmncORM.TRegObjects.FindGenerator(AObjectClass: TormObjectClass): TormGeneratorClass;
var
  o: TRegObject;
begin
  Result := TormGenerator;
  for o in Self do
  begin
    if AObjectClass.InheritsFrom(o.ObjectClass) then
    begin
      Result := o.GeneratorClass;
      break;
    end;
  end;
end;

{ TmncORM }

constructor TmncORM.Create(AName: String);
begin
  inherited Create(nil, AName);
  FRoot := Self;
  FUsePrefexes := True;
  FObjectClasses := TRegObjects.Create;
end;

destructor TmncORM.Destroy;
begin
  FreeAndNil(FObjectClasses);
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

procedure TmncORM.RegisterGenerator(AObjectClass: TormObjectClass; AGeneratorClass: TormGeneratorClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.GeneratorClass := AGeneratorClass;
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

procedure TmncORM.TField.Check;
begin
  if ReferenceInfoStr.Table <> '' then
  begin
    ReferenceInfo.Table := Root.FindObject(TTable, ReferenceInfoStr.Table, true) as TTable;
    if ReferenceInfo.Table = nil then
      raise Exception.Create(ReferenceInfoStr.Table + ' not exists');
    if ReferenceInfoStr.Field <> '' then
      ReferenceInfo.Field := ReferenceInfo.Table.FindObject(TField, ReferenceInfoStr.Field, true) as TField;
  end;
  ReferenceInfo.UpdateOptions := ReferenceInfoStr.UpdateOptions;
  ReferenceInfo.DeleteOptions := ReferenceInfoStr.DeleteOptions;
  inherited Check;
end;

function TmncORM.TField.Table: TTable;
begin
  Result := Parent.Parent as TTable;
end;

constructor TmncORM.TField.Create(AFields: TFields; AName: String; AFieldType: TormFieldType; AOptions: TormFieldOptions);
begin
  inherited Create(AFields, AName);
  FTitle := AName;
  FOptions := AOptions;
  FFieldType := AFieldType;
  FFilter := [ffSelect, ffView, ffData];
  if AFieldType = ftString then
    FFieldSize := 60;
end;

function TmncORM.TField.SQLName: string;
begin
  Result := inherited SQLName;
  if (Table <> nil) then
  begin
    if Root.UsePrefexes then
      Result := Table.Prefix + Result;
  end;
end;

function TmncORM.TField.Parent: TFields;
begin
  Result := inherited Parent as TFields;
end;

procedure TmncORM.TField.ReferenceTo(TableName, FieldName: string; UpdateOptions, DeleteOptions: TormReferenceOption);
begin
  ReferenceInfoStr.Table := TableName;
  ReferenceInfoStr.Field := FieldName;
  ReferenceInfoStr.UpdateOptions := UpdateOptions;
  ReferenceInfoStr.DeleteOptions := DeleteOptions;
end;

{ TTable }

function TmncORM.TTable.This: TTable;
begin
  Result := Self;
end;

procedure TmncORM.TTable.Added(Item: TormObject);
begin
  inherited Added(Item);
end;

procedure TmncORM.TTable.SetGeneratorClass(AValue: TormGeneratorClass);
begin
  if not (AValue.InheritsFrom(TormTableGenerator)) then
    raise Exception.Create('Generator should be TableGenerator');

  inherited SetGeneratorClass(AValue);
end;

constructor TmncORM.TTable.Create(ASchema: TSchema; AName: String; APrefix: string);
begin
  inherited Create(ASchema, AName);
  Prefix := APrefix;
  //TFields := TFields.Create(Self); //hmmmmmm :J
end;

function TmncORM.TTable.SelectSQL(AFilter: TFieldFilter; Keys: array of string; ExtraFields: array of string): string;
var
  Generator: TormGenerator;
begin
  if GeneratorClass <> nil then
  begin
    Generator := GeneratorClass.Create;
    Result := (Generator as TormTableGenerator).SelectSQL(Self, AFilter, Keys, ExtraFields);
  end
  else
    Result := '';
end;

function TmncORM.TTable.SaveSQL(Updating, Returning: Boolean; AFilter: TFieldFilter; Keys: array of string): string;
var
  Generator: TormGenerator;
begin
  if GeneratorClass <> nil then
  begin
    Generator := GeneratorClass.Create;
    Result := (Generator as TormTableGenerator).SaveSQL(Self, AFilter, Updating, Returning, Keys);
  end
  else
    Result := '';
end;

procedure TmncORM.TTable.GenAliases(Strings: TStringList);
var
  o: TormObject;
begin
  for o in Fields do
  begin
    Strings.Add((o as TField).SQLName + '=' + (o as TField).Title);
  end;
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

procedure TormObject.Check;
var
  o: TormObject;
begin
  for o in Self do
    o.Check;
end;

function TormObject.Find(const Name: string): TormObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TormObject.FindObject(ObjectClass: TormObjectClass; AName: string; RaiseException: Boolean): TormObject;
var
  o: TormObject;
begin
  Result := nil;
  for o in Self do
  begin
    if (o.InheritsFrom(ObjectClass) and (SameText(o.Name, AName))) then
    begin
      Result := o;
      exit;
    end;
  end;
  for o in Self do
  begin
    Result := o.FindObject(ObjectClass, AName);
    if Result <> nil then
      exit;
  end;
  if RaiseException and (Result = nil) then
    raise Exception.Create(ObjectClass.ClassName + ': ' + AName +  ' not exists in ' + Name);
end;

function TormObject.IndexOfName(vName: string): Integer;
var
  i: integer;
begin
  Result := -1;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := i;
        break;
      end;
    end;
end;

function LevelStr(vLevel: Integer): String;
begin
  Result := StringOfChar(' ', vLevel * 4);
end;

function ValueToStr(vValue: Variant): string;
begin
  if not VarIsEmpty(vValue) then
  begin
    if VarType(vValue) = varString then
      Result := '''' + vValue + ''''
    else
      Result := VarToStr(vValue);
  end
  else
    Result := '''''';
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
  end;
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
  private
    Buffer: string;
  public
    SQL: TStrings; //Reference to SQL
    constructor Create(ASQL: TStrings);
    destructor Destroy; override;
    procedure Add(S: string; Options: TCallbackObjectOptions = []); override;
  end;

{ TSQLCallbackObject }

constructor TSQLCallbackObject.Create(ASQL: TStrings);
begin
  inherited Create;
  SQL := ASQL;
end;

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
  SQLCB := TSQLCallbackObject.Create(vSQL);
  try
    GenerateSQL(SQLCB);
  finally
    FreeAndNil(SQLCB);
  end;
  Result := True;
end;

function TmncORM.GenerateSQL(Callback: TCallbackObject): Boolean;
var
  AParams: TStringList;
  o: TormObject;
  sqlObject: TormSQLObject;
  Generator: TormGenerator;
begin
  Check;
  AParams := TStringList.Create;
  try
    for o in Self do
    begin
      sqlObject := (o as TormSQLObject);
      if sqlObject.GeneratorClass <> nil then
      begin
        Generator := sqlObject.GeneratorClass.Create;
        try
          Generator.GenerateSQL(sqlObject, Callback, 0);
        finally
          Generator.Free;
        end;
      end;
    end;
  finally
    FreeAndNil(AParams);
  end;
  Result := True;
end;

// Common Fields

constructor TIDField.Create(AFields: TmncORM.TFields; AName: String);
begin
  if AName = '' then
    AName := 'ID';
  inherited Create(AFields, AName, ftInteger, [foIndexed, foSequenced, foPrimary, foInternal, foNotNull]);
end;

{ TCommentField }

constructor TCommentField.Create(AFields: TmncORM.TFields; AName: String);
begin
  if AName = '' then
    AName := 'Comment';
  inherited Create(AFields, AName, ftString, []);
  FieldSize := 250;
end;

{ TStringField }

constructor TStringField.Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions);
begin
  inherited Create(AFields, AName, ftString, AOptions);
  FieldSize := 60;
end;

{ TNameField }

constructor TNameField.Create(AFields: TmncORM.TFields; AName: String);
begin
  if AName = '' then
    AName := 'Name';
  inherited Create(AFields, AName, ftString, [foIndexed, foNotNull]);
  FieldSize := 60;
end;

{ TBooleanField }

constructor TBooleanField.Create(AFields: TmncORM.TFields; AName: String);
begin
  inherited Create(AFields, AName, ftBoolean, [foNotNull]);
  DefaultValue := 0;
end;

{ TRefIDField }

constructor TRefIDField.Create(AFields: TmncORM.TFields; AName, AMasterTable: String; AMasterID: string; AOptions: TmncORM.TormFieldOptions);
begin
  if AMasterID = '' then
    AMasterID := 'ID';
  inherited Create(AFields, AName, ftInteger, AOptions + [foReferenced]);
  ReferenceTo(AMasterTable, AMasterID, rfoRestrict, rfoRestrict);
end;

{ TRefDetailField }

constructor TRefDetailField.Create(AFields: TmncORM.TFields; AName, AMasterTable, AMasterID: string);
begin
  if AMasterID = '' then
    AMasterID := 'ID';
  inherited Create(AFields, AName, ftInteger, [foReferenced]);
  ReferenceTo(AMasterTable, AMasterID, rfoRestrict, rfoCascade);
end;

{ TIntegerField }

constructor TIntegerField.Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions);
begin
  inherited Create(AFields, AName, ftInteger, AOptions);
end;

{ TDateTimeField }

constructor TDateTimeField.Create(AFields: TmncORM.TFields; AName: String; AOptions: TmncORM.TormFieldOptions);
begin
  inherited Create(AFields, AName, ftDateTime, AOptions);
end;

{ TRefStringField }

constructor TRefStringField.Create(AFields: TmncORM.TFields; AName, AMasterTable, AMasterField: string; AOptions: TmncORM.TormFieldOptions);
begin
  inherited Create(AFields, AName, ftString, AOptions + [foReferenced]);
  ReferenceTo(AMasterTable, AMasterField, rfoRestrict, rfoRestrict);
end;

{ TCurrencyField }

constructor TCurrencyField.Create(AFields: TmncORM.TFields; AName: String);
begin
  inherited Create(AFields, AName, ftCurrency, [foNotNull]);
  DefaultValue := 0;
end;

end.
