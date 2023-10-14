program project1;
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

uses
  mnWebElements;

function CreateORM(ORMClass: TmncORMClass): TmnwElement;
begin
  if ORMClass = nil then
    Result := nil
  else
  begin
    Result := TmnwElement.Create('App');
    with Result do
    begin
      with TDatabase.Create(This, 'Document') do
        with TSchema.Create(This, '') do begin

          with TTable.Create(This, 'Companies') do begin

    end;
  end;
end;

begin

end.

