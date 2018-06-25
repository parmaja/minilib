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
  Result := ORMClass.Create;

  with Result do
    with TormDatabase.Create('Database') do
      with TormSchema.Create(This, 'Schema') do
      begin
        with TormTable.Create(This, 'Employees') do
        begin
          with TormField.Create(This, 'Name', ftString) do
          begin
            FieldSize := 60;
            Options := [foIndexed];
          end;
          with TormField.Create(This, 'Salary', ftCurrency) do
          begin
            Options := [foIndexed];
          end;
        end;
      end;
end;

function CreateSchema2(ORMClass: TmncORMClass): TmncORM;
begin
  Result := ORMClass.Create;

  with Result do
    with CreateDatabase('Database') do
      with CreateSchema(This, 'Schema') do
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


