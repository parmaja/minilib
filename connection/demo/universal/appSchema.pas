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

function CreateORM(ORMClass: TmncORMClass): TmncORM;

implementation

function CreateORM(ORMClass: TmncORMClass): TmncORM;
begin
  Result := ORMClass.Create('ORM');

  with Result do
  begin
    UsePrefexes := True;
    with TDatabase.Create(This, 'Database') do
      with TSchema.Create(This, 'Schema') do begin

        with TTable.Create(This, 'Companies') do begin
          with TFields.Create(This) do begin
            with TField.Create(This, 'ID', ftInteger) do begin
              Options := [foIndexed, foPrimary, foSequenced];
            end;
            with TField.Create(This, 'Name', ftString) do begin
              FieldSize := 60;
              Options := [foIndexed];
            end;
            with TField.Create(This, 'Address', ftCurrency) do begin
              DefaultValue := 0;
            end;
          end;
        end;

        with TTable.Create(This, 'Employees') do begin
          with TFields.Create(This) do begin
            with TIDField.Create(This, 'ID') do begin
            end;
            with TNameField.Create(This, 'Name') do begin
              Options := [foIndexed];
            end;

            with TRefIDField.Create(This, 'Company', 'Companies') do begin
            end;

            with TCurrencyField.Create(This, 'MonthSalary') do begin
            end;
          end;
        end;

        with TTable.Create(This, 'Entries') do
        begin
          Prefix := 'Ent';
          with TFields.Create(This) do begin
            with TField.Create(This, 'ID', ftInteger) do begin
              Options := [foIndexed, foPrimary, foSequenced];
            end;

            with TRefIDField.Create(This, 'Employee', 'Employees', 'ID') do begin
            end;

            with TCurrencyField.Create(This, 'Value') do begin
              //Options := Options + [foTotal];
            end;
          end;
      end;
    end;
  end;
end;

end.
