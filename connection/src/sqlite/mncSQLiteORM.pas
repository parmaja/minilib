unit mncSQLiteORM;
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
  SysUtils, Classes,
  mncMeta, mncORM, mncSQLite;

type

  { TmncORMSQLite }

  TmncORMSQLite = class(TmncORM)
  protected

  public
    function CreateSchema(AName: string): TormSchema; override;
    function CreateDatabase(ASchema: TormSchema; AName: string): TormDatabase; override;
    function CreateTable(ADatabase: TormDatabase; AName: string): TormTable; override;
    function CreateField(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []): TormField; override;
  end;

implementation

{ TmncORMSQLite }

function TmncORMSQLite.CreateSchema(AName: string): TormSchema;
begin

end;

function TmncORMSQLite.CreateDatabase(ASchema: TormSchema; AName: string): TormDatabase;
begin

end;

function TmncORMSQLite.CreateTable(ADatabase: TormDatabase; AName: string): TormTable;
begin

end;

function TmncORMSQLite.CreateField(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions): TormField;
begin

end;

end.
