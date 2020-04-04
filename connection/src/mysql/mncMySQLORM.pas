unit mncMySQLORM;
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
  SysUtils, Classes, Variants, mncORM;

type

  { TmncORMMySQL }

  TmncORMMySQL = class(TmncORM)
  protected
    type

      { TDatabaseMySQL }

      TDatabaseMySQL = class(TormGenerator)
      public
      end;

      { TSchemaMySQL }

      TSchemaMySQL = class(TormGenerator)
      public
      end;

      { TTableMySQL }

      TTableMySQL = class(TormTableGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldsMySQL }

      TFieldsMySQL = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldMySQL }

      TFieldMySQL = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TInsertDataMySQL }

      TInsertDataMySQL = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;
  protected
    class function FieldTypeToString(FieldType: TmncORM.TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
  end;

implementation

{ TmncORMMySQL.TInsertDataMySQL }

function TmncORMMySQL.TInsertDataMySQL.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
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

{ TmncORMMySQL.TFieldsMySQL }

function TmncORMMySQL.TFieldsMySQL.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  o: TormObject;
  i: Integer;
begin
  i := 0;
  for o in AObject do
  begin
    if i > 0 then //not first line
      SQL.Add(',', [cboEndLine]);
    (o as TormSQLObject).GenSQL(SQL, vLevel);
    Inc(i);
  end;
  Result := True;
end;

{ TmncORMMySQL.TFieldMySQL }

function TmncORMMySQL.TFieldMySQL.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  fs: Integer;
begin
//  vSQL.Add(LevelStr(vLevel) + Name + ' as Integer'); bug in fpc needs to reproduce but i can
  with AObject as TField do
  begin
    fs := FieldSize;
    if fs = 0 then
      fs := 60;
    SQL.Add(LevelStr(vLevel) + QuotedSQLName + ' '+ FieldTypeToString(FieldType, fs));
    if (foNotNull in Options) or (foPrimary in Options) then
      SQL.Add(' not null');
    if foSequenced in Options then
      SQL.Add(' auto_increment');
    if not VarIsEmpty(DefaultValue) then
    begin
      if VarType(DefaultValue) = varString then
        SQL.Add(' default ''' + DefaultValue + '''')
      else
        SQL.Add(' default ' + VarToStr(DefaultValue));
    end;
  end;
  Result := True;
end;

{ TmncORMMySQL.TTableMySQL }

function TmncORMMySQL.TTableMySQL.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  i: integer;
  o: TormObject;
  Field: TField;
  Keys: string;
  IndexList: TStringList;
  aIndexName, aIndexFields: string;
  S: string;
begin
  Result := True;
  with AObject as TTable do
  begin

    SQL.Add(LevelStr(vLevel) + 'create table ' + QuotedSQLName);
    SQL.Add('(', [cboEndLine]);
    SQL.Params.Values['Table'] := SQLName;
    Fields.GenSQL(SQL, vLevel + 1);

    IndexList := TStringList.Create;
    try
      //collect primary keys and indexes
      Keys := '';
      for o in Fields do
      begin
        Field := o as TField;
        if Field.Primary then
        begin
          if Keys <> '' then
            Keys := Keys + ', ';
          Keys := Keys + Field.QuotedSQLName;
        end
        else if (Field.Indexed) or (Field.IndexName <> '') then
        begin
          if (Field.IndexName <> '') then
            aIndexName := Field.IndexName
          else
            if not Root.UsePrefexes then //Yes, if not using prefix we will force use it here
              aIndexName :=  'Idx_' + Prefix + Field.SQLName
            else
              aIndexName :=  'Idx_' + Field.SQLName;

          aIndexFields := IndexList.Values[aIndexName];
          if aIndexFields <> '' then
            aIndexFields := aIndexFields + ' ,';
          aIndexFields := aIndexFields + Field.SQLName;
          IndexList.Values[aIndexName] := aIndexFields;
        end;
      end;

      if Keys <> '' then
      begin
        SQL.Add(',', [cboEndLine]);
        SQL.Add(LevelStr(vLevel + 1) + 'primary key (' + Keys + ')', []);
      end;

      if IndexList.Count > 0 then
      begin
        for i := 0 to IndexList.Count -1 do
        begin
          aIndexName := IndexList.Names[i];
          aIndexFields := IndexList.ValueFromIndex[i];
          SQL.Add(',', [cboEndLine]);
          SQL.Add(LevelStr(vLevel + 1) + 'index ' + aIndexName + '(' + aIndexFields + ')');
        end;
      end;

      for o in Fields do
      begin
        Field := o as TField;
        if Field.ReferenceInfo.Table <> nil then
        begin
          SQL.Add(',', [cboEndLine]);
          S := 'foreign key Ref_' + SQLName + Field.ReferenceInfo.Table.Name + Field.ReferenceInfo.Field.Name + '(' + Field.QuotedSQLName + ')'
                  +' references ' + Field.ReferenceInfo.Table.QuotedSQLName + '(' + Field.ReferenceInfo.Field.QuotedSQLName + ')';

          if rfoReject = Field.ReferenceInfo.DeleteOption then
            S := S + ' on delete restrict'
          else if rfoCascade = Field.ReferenceInfo.DeleteOption then
            S := S + ' on delete cascade'
          else if rfoSetNull = Field.ReferenceInfo.DeleteOption then
            S := S + ' on delete set null';

          if rfoReject = Field.ReferenceInfo.UpdateOption then
            S := S + ' on update restrict'
          else if rfoCascade = Field.ReferenceInfo.UpdateOption then
            S := S + ' on update cascade'
          else if rfoSetNull = Field.ReferenceInfo.UpdateOption then
            S := S + ' on update set null';

          SQL.Add(LevelStr(vLevel + 1) + S , []);
        end;
      end;
      SQL.Add('', [cboEndLine]);
      SQL.Add(')', [cboEndLine]);
      SQL.Add('', [cboEndChunk]);
      SQL.Params.Values['Table'] := '';
    finally
      FreeAndNil(IndexList);
    end;
  end;
end;

{ TmncORMMySQL }

class function TmncORMMySQL.FieldTypeToString(FieldType: TmncORM.TormFieldType; FieldSize: Integer): String;
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

procedure TmncORMMySQL.Created;
begin
  inherited Created;
  RegisterGenerator(TDatabase, TDatabaseMySQL);
  RegisterGenerator(TSchema, TSchemaMySQL);
  RegisterGenerator(TTable, TTableMySQL);
  RegisterGenerator(TFields, TFieldsMySQL);
  RegisterGenerator(TField, TFieldMySQL);
  RegisterGenerator(TInsertData, TInsertDataMySQL);
end;

end.
