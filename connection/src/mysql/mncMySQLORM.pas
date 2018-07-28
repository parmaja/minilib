unit mncMySQLORM;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  SysUtils, Classes, mnUtils, Variants, mncORM;

type

  { TmncORMMySQL }

  TmncORMMySQL = class(TmncORM)
  protected
    type

      { TDatabaseMySQL }

      TDatabaseMySQL = class(TormHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TSchemaMySQL }

      TSchemaMySQL = class(TormHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TTableMySQL }

      TTableMySQL = class(TormTableHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldsMySQL }

      TFieldsMySQL = class(TormHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldMySQL }

      TFieldMySQL = class(TormHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TInsertDataMySQL }

      TInsertDataMySQL = class(TormHelper)
      public
        function CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;
  protected
    class function FieldTypeToString(FieldType:TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
  end;

implementation

{ TmncORMMySQL.TInsertDataMySQL }

function TmncORMMySQL.TInsertDataMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  o: TormObject;
  i: Integer;
begin
  i := 0;
  with AObject as TInsertData do
  begin
    SQL.Add('insert into ' + Table.GenName + '(' );
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

{ TmncORMMySQL.TFieldsMySQL }

function TmncORMMySQL.TFieldsMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  o: TormObject;
  i: Integer;
begin
  i := 0;
  for o in AObject do
  begin
    if i > 0 then //not first line
      SQL.Add(',', [cboEndLine]);
    (o as TormSQLObject).GenerateSQL(SQL, vLevel);
    Inc(i);
  end;
  Result := True;
end;

{ TmncORMMySQL.TFieldMySQL }

function TmncORMMySQL.TFieldMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  fs: Integer;
begin
//  vSQL.Add(LevelStr(vLevel) + Name + ' as Integer'); bug in fpc needs to reproduce but i can
  with AObject as TField do
  begin
    fs := FieldSize;
    if fs = 0 then
      fs := 60;
    SQL.Add(LevelStr(vLevel) + GenName + ' '+ FieldTypeToString(FieldType, fs));
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

function TmncORMMySQL.TTableMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  i: integer;
  Field: TField;
  Keys: string;
  IndexList: TStringList;
  IndexName, IndexFields: string;
  S: string;
begin
  Result := True;
  with AObject as TTable do
  begin

    SQL.Add(LevelStr(vLevel) + 'create table ' + GenName);
    SQL.Add('(', [cboEndLine]);
    SQL.Params.Values['Table'] := GenName;
    Fields.GenerateSQL(SQL, vLevel + 1);

    IndexList := TStringList.Create;
    //collect primary keys and indexes
    Keys := '';
    for Field in Fields do
    begin
      if Field.Primary then
      begin
        if Keys <> '' then
          Keys := Keys + ', ';
        Keys := Keys + Field.GenName;
      end
      else if (Field.Indexed) or (Field.Index <> '') then
      begin
        if (Field.Index <> '') then
          IndexName := Field.Index
        else
          IndexName :=  'Idx_' + Field.GenName;

        IndexFields := IndexList.Values[IndexName];
        if IndexFields <> '' then
          IndexFields := IndexFields + ' ,';
        IndexFields := IndexFields + Field.GenName;
        IndexList.Values[IndexName] := IndexFields;
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
        IndexName := IndexList.Names[i];
        IndexFields := IndexList.ValueFromIndex[i];
        SQL.Add(',', [cboEndLine]);
        SQL.Add(LevelStr(vLevel + 1) + 'index ' + IndexName + '(' + IndexFields + ')');
      end;
    end;

    FreeAndNil(Indexes);

    for Field in Fields do
    begin
      if Field.ReferenceInfo.Table <> nil then
      begin
        SQL.Add(',', [cboEndLine]);
        S := 'foreign key Ref_' + GenName + Field.ReferenceInfo.Table.GenName + Field.ReferenceInfo.Field.GenName + '(' + Field.GenName + ')'
                +' references ' + Field.ReferenceInfo.Table.GenName + '(' + Field.ReferenceInfo.Field.GenName + ')';

        if rfoDelete in Field.ReferenceInfo.Options then
        begin
          if rfoRestrict in Field.ReferenceInfo.Options then
            S := S + ' on delete restrict'
          else
            S := S + ' on delete cascade'
        end
        else
          S := S + ' on delete set null';

        if rfoUpdate in Field.ReferenceInfo.Options then
        begin
          if rfoRestrict in Field.ReferenceInfo.Options then
            S := S + ' on update restrict'
          else
            S := S + ' on update cascade';
        end
        else
          S := S + ' on update set null';

        SQL.Add(LevelStr(vLevel + 1) + S , []);
      end;
    end;
    SQL.Add('', [cboEndLine]);
    SQL.Add(')', [cboEndLine]);
    SQL.Add('', [cboEndChunk]);
    SQL.Params.Values['Table'] := '';
  end;
end;

{ TmncORMMySQL.TDatabaseMySQL }

function TmncORMMySQL.TDatabaseMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  GenerateObjects(AObject, SQL, vLevel);
  Result := True;
end;

{ TmncORMMySQL.SchemaMySQL }

function TmncORMMySQL.TSchemaMySQL.CreateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  GenerateObjects(AObject, SQL, vLevel);
  Result := True;
end;

{ TmncORMMySQL }

class function TmncORMMySQL.FieldTypeToString(FieldType: TormFieldType; FieldSize: Integer): String;
begin
  case FieldType of
    ftString: Result := 'varchar('+IntToStr(FieldSize)+')';
    ftBoolean: Result := 'boolean';
    ftInteger: Result := 'integer';
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
  Register(TDatabase ,TDatabaseMySQL);
  Register(TSchema, TSchemaMySQL);
  Register(TTable, TTableMySQL);
  Register(TFields, TFieldsMySQL);
  Register(TField, TFieldMySQL);
  Register(TInsertData, TInsertDataMySQL);
end;

end.
