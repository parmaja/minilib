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
    with TDatabase.Create(This, 'Database') do
      with TSchema.Create(This, 'Schema') do
      begin
        with TTable.Create(This, 'Employees') do
        begin
          with TFields.Create(This) do
          begin
            with TField.Create(This, 'ID', ftInteger) do
            begin
              Options := [foIndexed, foPrimary, foSequenced];
            end;
            with TField.Create(This, 'Name', ftString) do
            begin
              FieldSize := 60;
              Options := [foIndexed];
            end;
            with TField.Create(This, 'MonthSalary', ftCurrency) do
            begin
              DefaultValue := 0;
            end;
          end;
        end;

        with TTable.Create(This, 'Entries') do
        begin
          with TFields.Create(This) do
          begin
            with TField.Create(This, 'ID', ftInteger) do
            begin
              Options := [foIndexed, foPrimary, foSequenced];
            end;
            with TField.Create(This, 'EmployeeID', ftInteger) do
            begin
              ReferenceTo('Employees', 'ID', [rfoDelete, rfoUpdate]);
              FieldSize := 60;
              Options := [foIndexed, foReferenced];
            end;
            with TField.Create(This, 'Value', ftCurrency) do
            begin
              Options := [foSummed];
            end;
          end;
      end;
    end;
end;

function CreateSchema2(ORMClass: TmncORMClass): TmncORM;
begin
  Result := ORMClass.Create('ORM');

  with Result do
    with CreateDatabase('Database') do
      with CreateSchema(This, 'Schema') do
      begin
        with CreateTable(This, 'Employees') do
        begin
          with CreateField(This, 'ID', ftString) do
          begin
            FieldSize := 60;
            Options := [foIndexed, foPrimary, foSequenced];
          end;
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


