unit mncFBORM;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL modified of http://www.gnu.org/licenses/lgpl.html
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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
  SysUtils, Classes, mnUtils, Variants,
  mncORM, mncConnections, mncFirebird;

type

  { TmncFBORM }

  TmncFBORM = class(TmncStdORM)
  protected
    type

      { TDatabaseFB }

      TDatabaseFB = class(TormGenerator)
      public
      end;

      { TSchemaFB }

      TSchemaFB = class(TormGenerator)
      public
      end;

      { TTableFB }

      TTableFB = class(TTableStd)
      public
        constructor Create; override;
        procedure GenExternalIndexes(Table: TTable; IndexList: TIndexGroups; SQL: TCallbackObject; vLevel: Integer); override;
        function GenForignKey(Field: TField; AExternal: Boolean): string; override;
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldsFB }

      TFieldsFB = class(TFieldsStd)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldFB }

      TFieldFB = class(TFieldStd)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TInsertDataFB }

      TInsertDataFB = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

  protected
    class function FieldTypeToString(FieldType:TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
  end;

implementation

uses
  mncDB;

{ TmncFBORM.TFieldsFB }

function TmncFBORM.TFieldsFB.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  Result := inherited DoGenerateSQL(AObject, SQL, vLevel);
end;

{ TmncFBORM.TInsertDataFB }

function TmncFBORM.TInsertDataFB.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
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
      SQL.Add(Table.Prefix + (o as TFieldValue).Name); //todo need to use genname
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

{ TmncFBORM.TFieldFB }

function TmncFBORM.TFieldFB.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  with AObject as TField do
  begin
    SQL.Add(vLevel, QuotedSQLName + ' '+ FieldTypeToString(FieldType, FieldSize));
{    if (foSequenced in Options) then
      SQL.Add(' identity');} // identity in ver 3
    if not VarIsEmpty(DefaultValue) then
    begin
      if VarType(DefaultValue) = varString then
        SQL.Add(' default ''' + DefaultValue + '''')
      else if VarType(DefaultValue) = varBoolean then
        SQL.Add(' default ' + IntToStr(Ord(Boolean(DefaultValue))))
      else
        SQL.Add(' default ' + VarToStr(DefaultValue));
    end;
    if (foNotNull in Options) or (foPrimary in Options) then
      SQL.Add(' not null');

    if (foPrimary in Options) and (Table.Fields.PrimaryKeys = 1) then //one PK we will use it here
      SQL.Add(' primary key');
  end;
  Result := True;
end;

constructor TmncFBORM.TTableFB.Create;
begin
  inherited Create;
  GenerateOptions.PK := gnpAttribute;
  GenerateOptions.FK := gnpInternal;
  GenerateOptions.Indexes := gnpExternal;
end;

procedure TmncFBORM.TTableFB.GenExternalIndexes(Table: TTable; IndexList: TIndexGroups; SQL: TCallbackObject; vLevel: Integer);
var
  o: TormObject;
  Field: TField;
  Seq: string;
begin

  inherited GenExternalIndexes(Table, IndexList, SQL, vLevel);

  for o in Table.Fields do
  begin
    Field := o as TField;
    if Field.Sequenced then
    begin
      Seq := 'SQ_' + Field.FullPathName;
      SQL.Add(vLevel, 'create sequence ' + Seq, [cboEndChunk]);
      SQL.Add(vLevel, 'create trigger ' + 'SQ_TR_' + Field.FullPathName + ' for ' + Field.Table.QuotedSQLName, [cboEndLine]);
      SQL.Add(vLevel, 'active before insert position 0', [cboEndLine]);
      SQL.Add(vLevel, 'as', [cboEndLine]);
      SQL.Add(vLevel, 'begin', [cboEndLine]);
      SQL.Add(vLevel + 1, 'if (new.' + Field.QuotedSQLName + ' is null) then', [cboEndLine]);
      SQL.Add(vLevel + 2, 'new.' + Field.QuotedSQLName + ' =  next value for ' + Seq + ';', [cboEndLine]);
      SQL.Add(vLevel, 'end', [cboEndLine, cboEndChunk]);
    end;
  end;
end;

function TmncFBORM.TTableFB.GenForignKey(Field: TField; AExternal: Boolean): string;
begin
  Result := 'constraint RF_' + Field.Table.Name + '_' +  Field.ReferenceInfo.Table.Name + Field.ReferenceInfo.Field.Name + ' foreign key (' + Field.QuotedSQLName + ')'
          +' references ' + Field.ReferenceInfo.Table.QuotedSQLName + '(' + Field.ReferenceInfo.Field.QuotedSQLName + ')';
end;

function TmncFBORM.TTableFB.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  Result :=inherited DoGenerateSQL(AObject, SQL, vLevel);
end;

{ TmncFBORM }

class function TmncFBORM.FieldTypeToString(FieldType: TormFieldType; FieldSize: Integer): String;
begin
  case FieldType of
    ftString: Result := 'varchar('+IntToStr(FieldSize)+')';
    ftBoolean: Result := 'smallint'; //Boolean in FB 3
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

procedure TmncFBORM.Created;
begin
  inherited Created;
  RegisterGenerator(TDatabase ,TDatabaseFB);
  RegisterGenerator(TSchema, TSchemaFB);
  RegisterGenerator(TTable, TTableFB);
  RegisterGenerator(TFields, TFieldsFB);
  RegisterGenerator(TField, TFieldFB);
  RegisterGenerator(TInsertData, TInsertDataFB);
end;

initialization
  Engines.RegisterORM('FirebirdSQL', TmncFBORM);
end.

