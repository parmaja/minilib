unit mncORM;
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
  Classes, SysUtils, Contnrs,
  mnClasses, rttiutils,
  mncConnections, mncCommons;

type

   TormObject = class;

   { TormObject }

   TormObject = class(TmnNamedObjectList<TormObject>)
   private
     FComment: string;
     FName: string;
     FParent: TormObject;
     FRoot: TormObject;
     FTags: string;
   protected
     procedure Added(Item: TormObject); override;
   public
     constructor Create(AParent: TormObject; AName: string);
     property Comment: string read FComment write FComment;
     function DoSQL(vSQL: TStrings; Params: TStringList): Boolean; virtual;
     function GenerateSQL(vSQL: TStrings; Params: TStringList): Boolean; virtual;
     function This: TormObject; //I wish i have templates/meta programming in pascal
     property Root: TormObject read FRoot;
     property Parent: TormObject read FParent;

     property Name: string read FName write FName;
     property Tags: string read FTags write FTags; //etc: 'Key,Data'
   end;

  { TmncORM }

  TmncORM = class(TormObject)
  protected
  public
    type
      TormObjectClass = class of TormObject;

      { TDatabase }

      TDatabase = class(TormObject)
      public
        constructor Create(AORM:TmncORM; AName: string);
        function This: TDatabase;
      end;

      { TormSchema }

      TSchema = class(TormObject)
      public
        constructor Create(ADatabase: TDatabase; AName: string);
        function This: TSchema;
      end;

      TFields = class;

      { TTable }

      TTable = class(TormObject)
      protected
        procedure Added(Item: TormObject); override;
      public
        Fields: TFields;
        constructor Create(ASchema: TSchema; AName: string);
        function This: TTable;
      end;

      { TFields }

      TFields = class(TormObject)
      public
        constructor Create(ATable: TTable);
        function This: TFields;
      end;

      TormFieldOption = (
        foID, //PRIMARY and AUTO
        foReferenced,
        foInternal, //Do not display for end user
        foSummed, //
        foIndexed
      );

      TormFieldOptions = set of TormFieldOption;
      TormFieldType = (ftString, ftBoolean, ftInteger, ftCurrency, ftFloat, ftDate, ftTime, ftDateTime, ftMemo, ftBlob);

      { TField }

      TField = class(TormObject)
      private
        FFieldSize: Integer;
        FFieldType: TormFieldType;
        FOptions: TormFieldOptions;
        FRefTableName: string;
        FRefFieldName: string;
        FRefField: TField;
      public
        constructor Create(AFields: TFields; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []);
        function Parent: TTable;
        procedure Reference(ATableName: string; AFieldName: string);
        property Options: TormFieldOptions read FOptions write FOptions;
        property FieldType: TormFieldType read FFieldType write FFieldType;
        property FieldSize: Integer read FFieldSize write FFieldSize;
      end;

      { StoredProcedure }

      TStoredProcedure = class(TormObject)
      private
        FCode: string;
      public
        property Code: string read FCode write FCode;
      end;

      { Trigger }

      TTrigger = class(TormObject)
      private
        FCode: string;
      public
        property Code: string read FCode write FCode;
      end;

  public
    type
      TRegObject = class(TObject)
      public
         ObjectClass: TormObjectClass;
      end;

      { TRegObjects }

      TRegObjects = class(TmnObjectList<TRegObject>)
      public
        function FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
      end;
   private
      FObjectClasses: TRegObjects;

   public
    constructor Create(AName: string); virtual;
    function This: TmncORM;

    function CreateDatabase(AName: string): TDatabase; virtual; abstract;
    function CreateSchema(ADatabase: TDatabase; AName: string): TSchema; virtual; abstract;
    function CreateTable(ASchema: TSchema; AName: string): TTable; virtual; abstract;
    function CreateField(ATable: TTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []): TField; virtual; abstract;

    procedure Register(AObjectClass: TormObjectClass);
    property ObjectClasses: TRegObjects read FObjectClasses;
  end;

  TmncORMClass = class of TmncORM;

implementation

{ TmncORM.TRegObjects }

function TmncORM.TRegObjects.FindDerived(AObjectClass: TormObjectClass): TormObjectClass;
var
  o: TRegObject;
begin
  for o in Self do
  begin
    if AObjectClass.ClassParent = o.ObjectClass then
    begin
      Result := o.ObjectClass;
      break;
    end;
  end;
end;

{ TmncORM.TFields }

constructor TmncORM.TFields.Create(ATable: TTable);
begin
  inherited Create(ATable, '');
end;

function TmncORM.TFields.This: TFields;
begin
  Result := Self;
end;

{ TmncORM }

constructor TmncORM.Create(AName: string);
begin
  inherited Create(nil, AName);
  FObjectClasses := TRegObjects.Create;
end;

function TmncORM.This: TmncORM;
begin
  Result := Self;
end;

procedure TmncORM.Register(AObjectClass: TormObjectClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  ObjectClasses.Add(aRegObject);
end;

{ TField }

constructor TmncORM.TField.Create(AFields: TFields; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions);
begin
  inherited Create(AFields, AName);
  FOptions := AOptions;
  FFieldType := AFieldType;
end;

function TmncORM.TField.Parent: TTable;
begin
  Result := inherited Parent as TTable;
end;

procedure TmncORM.TField.Reference(ATableName: string; AFieldName: string);
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
      raise Exception.Create('You cannot add Fields twice')
  end;
end;

constructor TmncORM.TTable.Create(ASchema: TSchema; AName: string);
begin
  inherited Create(ASchema, AName);
  //TFields := TFields.Create(Self);
end;

{ TSchema }

function TmncORM.TSchema.This: TSchema;
begin
  Result := Self;
end;

constructor TmncORM.TSchema.Create(ADatabase: TDatabase; AName: string);
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

constructor TormObject.Create(AParent: TormObject; AName: string);
begin
  inherited Create;
  if AParent <> nil then
  begin
    FParent := AParent;
    AParent.Add(Self);
    FRoot := AParent.Root;
  end
  else
    FRoot := Self;
end;

function TormObject.DoSQL(vSQL: TStrings; Params: TStringList): Boolean;
begin
  Result := False;
end;

function TormObject.GenerateSQL(vSQL: TStrings; Params: TStringList): Boolean;
var
  o: TormObject;
begin
  Result := DoSQL(vSQL, Params);
  for o in Self do
  begin
    o.GenerateSQL(vSQL, Params);
  end;
  //Result := False; //TODO raise exception
end;

{ TDatabase }

function TmncORM.TDatabase.This: TDatabase;
begin
  Result := Self;
end;

constructor TmncORM.TDatabase.Create(AORM: TmncORM; AName: string);
begin
  inherited Create(AORM, AName);
end;

end.
