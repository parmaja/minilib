unit mncPGORM;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL modified of http://www.gnu.org/licenses/lgpl.html
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{
  TODO:
    use foUnique
}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  SysUtils, Classes, Variants,
  mncORM;

type

  { TmncPGORM }

  TmncPGORM = class(TmncStdORM)
  protected
    type

      { TDatabasePG }

      TDatabasePG = class(TormGenerator)
      public
      end;

      { TSchemaPG }

      TSchemaPG = class(TormGenerator)
      public
      end;

      { TTablePG }

      TTablePG = class(TTableStd)
      public
        constructor Create; override;
        function GenForignKey(Field: TField; AExternal: Boolean): string; override;
      end;

      { TFieldsPG }

      TFieldsPG = class(TFieldsStd)
      public
      end;

      { TFieldPG }

      TFieldPG = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TInsertDataPG }

      TInsertDataPG = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

  protected
    class function FieldTypeToString(FieldType: TmncORM.TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
  end;

implementation

uses
  mncDB;

{ TmncPGORM.TInsertDataPG }

function TmncPGORM.TInsertDataPG.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  o: TormObject;
  i: Integer;
begin
  i := 0;
  with AObject as TInsertData do
  begin
    SQL.Add('insert into ' + Table.QuotedSQLName + '(' );
    for o in this do
    begin
      if i > 0 then
        SQL.Add(',', []);
      SQL.Add(Table.Prefix + (o as TFieldValue).Name); //todo need to use SQLName
      Inc(i);
    end;
    SQL.Add(') values (');

    i := 0;
    for o in this do
    begin
      if i > 0 then
        SQL.Add(',', []);
      SQL.Add(ValueToStr((o as TFieldValue).Value));
      Inc(i);
    end;
    SQL.Add(')', [cboEndChunk, cboEndLine]);
  end;
  Result := True;
end;

{ TmncPGORM.TFieldPG }

function TmncPGORM.TFieldPG.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  aType: string;
begin
//  vSQL.Add(vLevel, Name + ' as Integer'); bug in fpc needs to reproduce but i can
  with AObject as TField do
  begin
    if foSequenced in Options then
    begin
      case FieldType of
        ftSmallInteger:
          aType := 'smallserial';
        ftInteger:
          aType := 'serial';
        ftBigInteger:
          aType := 'bigserial';
        else
          aType := FieldTypeToString(FieldType, FieldSize);
      end;
    end
    else
      aType := FieldTypeToString(FieldType, FieldSize);
    SQL.Add(vLevel, QuotedSQLName + ' '+ aType);
    if (foNotNull in Options) or (foPrimary in Options) then
      SQL.Add(' not null');
    if not VarIsEmpty(DefaultValue) then
    begin
      if VarType(DefaultValue) = varString then
        SQL.Add(' default ''' + DefaultValue + '''')
      else if VarType(DefaultValue) = varBoolean then
        SQL.Add(' default ' + VarBoolToStr(DefaultValue))
      else
        SQL.Add(' default ' + VarToStr(DefaultValue));
    end;
  end;
  Result := True;
end;

{ TmncPGORM.TTablePG }

constructor TmncPGORM.TTablePG.Create;
begin
  inherited Create;
  GenerateOptions.PK := gnpInternal;
  GenerateOptions.FK := gnpInternal;
  GenerateOptions.Indexes := gnpExternal;
end;

function TmncPGORM.TTablePG.GenForignKey(Field: TField; AExternal: Boolean): string;
begin
  Result := 'constraint RF_' + Field.Table.Name + '_' +  Field.ReferenceInfo.Table.Name + Field.ReferenceInfo.Field.Name + ' foreign key (' + Field.QuotedSQLName + ')'
          +' references ' + Field.ReferenceInfo.Table.QuotedSQLName + '(' + Field.ReferenceInfo.Field.QuotedSQLName + ')';
end;

{ TmncPGORM }

class function TmncPGORM.FieldTypeToString(FieldType: TmncORM.TormFieldType; FieldSize: Integer): String;
begin
  case FieldType of
    ftString: Result := 'varchar('+IntToStr(FieldSize)+')';
    ftBoolean: Result := 'boolean';
    ftSmallInteger: Result := 'smllint';
    ftInteger: Result := 'integer';
    ftBigInteger: Result := 'bigint';
    ftCurrency: Result := 'decimal(12, 4)';
    ftFloat: Result := 'float';
    ftDate: Result := 'date';
    ftTime: Result := 'time';
    ftDateTime: Result := 'datetime';
    ftText: Result := 'text';
    ftBlob: Result := 'text';
  end;
end;

procedure TmncPGORM.Created;
begin
  inherited Created;
  RegisterGenerator(TDatabase, TDatabasePG);
  RegisterGenerator(TSchema, TSchemaPG);
  RegisterGenerator(TTable, TTablePG);
  RegisterGenerator(TFields, TFieldsPG);
  RegisterGenerator(TField, TFieldPG);
  RegisterGenerator(TInsertData, TInsertDataPG);
end;

initialization
  Engines.RegisterORM('PostgreSQL', TmncPGORM);
end.
