unit appSchema;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
interface

{$mode delphi}
{$H+}

uses
  SysUtils, StrUtils, Classes,
  mncORM;

function CreateSchema1(ORMClass: TmncORMClass): TmncORM;
function CreateSchema2(ORMClass: TmncORMClass): TmncORM;

implementation

function CreateSchema1(ORMClass: TmncORMClass): TmncORM;
begin
  Result := ORMClass.Create('ORM');

  with Result do
    with TDatabase.Create(This, 'TDatabase') do
      with TSchema.Create(This, 'TSchema') do
      begin
        with TTable.Create(This, 'Employees') do
        begin
          with TFields.Create(This) do
          begin
            with TField.Create(This, 'Name', ftString) do
            begin
              FieldSize := 60;
              Options := [foIndexed];
            end;
            with TField.Create(This, 'Salary', ftCurrency) do
            begin
              Options := [foIndexed];
            end;
          end;
        end;
      end;
end;

function CreateSchema2(ORMClass: TmncORMClass): TmncORM;
begin
  Result := ORMClass.Create('ORM');

  with Result do
    with CreateDatabase('TDatabase') do
      with CreateSchema(This, 'TSchema') do
      begin
        with CreateTable(This, 'Employees') do
        begin
          with CreateField(This, 'Name', ftString) do
          begin
            FieldSize := 60;
            Options := [foIndexed];
          end;
          with CreateField(This, 'Salary', ftCurrency) do
          begin
            Options := [foIndexed];
          end;
        end;
      end;
end;

end.


