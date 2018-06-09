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
  mncConnections, mncCommons, mncMetas;

type
  TormObject = class;

  { TormObject }

  TormObject = class(TmnNamedObjectList<TormObject>)
  private
    FName: string;
  public
    function This: TormObject; //I wish i have templates/meta programming in pascal
    constructor Create(AName: string);
    property Name: string read FName write FName;
  end;

  { TormSchema }

  TormSchema = class(TormObject)
  public
    function This: TormSchema;
    constructor Create(AName: string);
  end;

  { TormDatabase }

  TormDatabase = class(TormObject)
  public
    function This: TormDatabase;
    constructor Create(ASchema: TormSchema; AName: string);
  end;

  { TormTable }

  TormTable = class(TormObject)
  public
    function This: TormTable;
    constructor Create(ADatabase: TormDatabase; AName: string);
  end;

  TormFieldOption = (
    foID, //PRIMARY and AUTO
    foIndexed
  );

  TormFieldOptions = set of TormFieldOption;

  { TormField }

  TormField = class(TormObject)
  private
    FDataType: TmncDataType;
    FOptions: TormFieldOptions;
  public
    constructor Create(ATable: TormTable; AName: string; ADataType: TmncDataType; AOptions: TormFieldOptions);
    property Options: TormFieldOptions read FOptions write FOptions;
    property DataType: TmncDataType read FDataType write FDataType;
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

constructor TormField.Create(ATable: TormTable; AName: string; ADataType: TmncDataType; AOptions: TormFieldOptions);
begin
  inherited Create(AName);
  ATable.Add(Self);
  FOptions := AOptions;
  FDataType := ADataType;
end;

{ TormTable }

function TormTable.This: TormTable;
begin
  Result := Self;
end;

constructor TormTable.Create(ADatabase: TormDatabase; AName: string);
begin
  inherited Create(AName);
  ADatabase.Add(Self);
end;

{ TormSchema }

function TormSchema.This: TormSchema;
begin
  Result := Self;
end;

constructor TormSchema.Create(AName: string);
begin
  inherited Create(AName);
end;

{ TormObject }

function TormObject.This: TormObject;
begin
  Result := Self;
end;

constructor TormObject.Create(AName: string);
begin

end;

{ TormDatabase }

function TormDatabase.This: TormDatabase;
begin
  Result := Self;
end;

constructor TormDatabase.Create(ASchema: TormSchema; AName: string);
begin
  inherited Create(AName);
  ASchema.Add(Self);

end;

end.
