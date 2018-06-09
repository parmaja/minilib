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
  mnClasses,
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
  public
    constructor Create(AParent: TormObject; AName: string);
    property Name: string read FName write FName;
    property Comment: string read FComment write FComment;
    function This: TormObject; //I wish i have templates/meta programming in pascal
    property Parent: TormObject read FParent;
    property Root: TormObject read FRoot;
  end;

  { TormSchema }

  TormSchema = class(TormObject)
  public
    constructor Create(AName: string);
    function This: TormSchema;
  end;

  { TormDatabase }

  TormDatabase = class(TormObject)
  public
    constructor Create(ASchema: TormSchema; AName: string);
    function This: TormDatabase;
  end;

  { TormTable }

  TormTable = class(TormObject)
  public
    constructor Create(ADatabase: TormDatabase; AName: string);
    function This: TormTable;
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

  { TormField }

  TormField = class(TormObject)
  private
    FFieldType: TormFieldType;
    FOptions: TormFieldOptions;
    FRefTableName: string;
    FRefFieldName: string;
    FRefField: TormField;
  public
    constructor Create(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []);
    function Parent: TormTable;
    procedure Reference(ATableName: string; AFieldName: string);
    property Options: TormFieldOptions read FOptions write FOptions;
    property FieldType: TormFieldType read FFieldType write FFieldType;
  end;

  { TormInsert }

  TormInsert = class(TormObject)
  public
    function This: TormInsert;
    constructor Create(AFields, AValues: Array of String);
  end;

implementation

{ TormInsert }

function TormInsert.This: TormInsert;
begin
  Result := Self;
end;

constructor TormInsert.Create(AFields, AValues: array of String);
begin

end;

{ TormField }

constructor TormField.Create(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions);
begin
  inherited Create(ATable, AName);
  FOptions := AOptions;
  FFieldType := AFieldType;
end;

function TormField.Parent: TormTable;
begin
  Result := inherited Parent as TormTable;
end;

procedure TormField.Reference(ATableName: string; AFieldName: string);
begin

end;

{ TormTable }

function TormTable.This: TormTable;
begin
  Result := Self;
end;

constructor TormTable.Create(ADatabase: TormDatabase; AName: string);
begin
  inherited Create(ADatabase, AName);
end;

{ TormSchema }

function TormSchema.This: TormSchema;
begin
  Result := Self;
end;

constructor TormSchema.Create(AName: string);
begin
  inherited Create(nil, AName);
end;

{ TormObject }

function TormObject.This: TormObject;
begin
  Result := Self;
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

{ TormDatabase }

function TormDatabase.This: TormDatabase;
begin
  Result := Self;
end;

constructor TormDatabase.Create(ASchema: TormSchema; AName: string);
begin
  inherited Create(ASchema, AName);
end;

end.
