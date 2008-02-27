unit mncSqlUtils;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
interface

uses
  Windows, SysUtils, StrUtils, Classes, StdCtrls;

type
  TSQL_Engine = (sqlePlan, sqleParadox, sqleFirebird, sqleMySQL, sqlePostgres, sqleSQLite);
  TArrayFieldNames = array of string;

const
  SQL_Engine_Quoted: array[TSQL_Engine] of string = (
    '', //sqlePlan
    '', //sqlParadox
    '"', //sqlFirebird
    '`', //sqlMySQL
    '"', //sqlPostgres
    '"' //sqlSQLite
    );

  SQL_Engine_Bool_Values: array[TSQL_Engine] of array[Boolean] of string = (
    ('False', 'True'), //sqlePlan
    ('False', 'True'), //sqlParadox
    ('0', 'True'), //sqlFirebird
    ('false', 'true'), //sqlMySQL
    ('false', 'true'), //sqlPostgres
    ('false', 'true') //sqlSQLite
    );

var
  DefaultSQLEngine: TSQL_Engine = sqlePlan;
  DefaultSQLQuoted : Boolean = False;

function DBName(const Name: string; Engine: TSQL_Engine = sqlePlan): string;
function DBBool(Value: Boolean; Engine: TSQL_Engine = sqlePlan): string;
function BoolToSqlText(Value: Boolean): string;
function DateToSqlDate(vDate: TDateTime): string;
procedure AppendAndNotLike(var vSql: string; const vFieldName, Value: string; vAndOr: string = 'and');
procedure AppendAndLike(var vSql: string; const vFieldName, Value: string; vAndOr: string = 'and');
procedure AppendAndLeftLike(var vSql: string; const vFieldName, Value: string; vAndOr: string = 'and');
procedure AppendAndText(var vSql: string; const vFieldName, Value: string; vAndOr: string = 'and');
procedure AppendAndDate(var vSql: string; const vFieldName: string; vDate: TDateTime; vEqual: string = '='; vAndOr: string = 'and');
procedure AppendBetweenDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string = 'and');
procedure AppendNotBetweenDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string = 'and');
procedure AppendBetweenData(var vSql: string; const vFieldName: string; vFrom, vTo: Integer; vAndOr: string = 'and');
procedure AppendAndNumber(var vSql: string; const vFieldName: string; vNumber: Integer; vEqual: string = '='; vAndOr: string = 'and');
procedure AppendAndData(var vSql: string; const vFieldName: string; Value: Integer; vEqual: string = '='; vAndOr: string = 'and');
procedure AddData(var vSql: string; const vAndOr, vFieldName: string; Value: Integer);
procedure AppendRangeDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime);
procedure AppendRangeNumber(var vSql: string; const vFieldName: string; vFromNumber, vToNumber: Integer);
procedure AppendRangeData(var vSql: string; const vFieldName: string; vFromNumber, vToNumber: Integer);
procedure AppendAndStr(var vSql: string; const vFieldName: string; Value: string; vEqual: string = '='; vAndOr: string = 'and');
procedure AppendAndVariant(var vSql: string; const vFieldName: string; Value: Variant; vEqual: string = '='; vAndOr: string = 'and');
procedure AppendAndCustom(var vSql: string; const vCustStr: string; vAndOr: string = 'and');
procedure AppendAndBool(var vSql: string; const vFieldName: string; Value: Boolean; vEqual: string = '='; vAndOr: string = 'and');


function ForSelect(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string; overload;
function ForSelect(Table: string; Fields: array of string; Keys: array of string): string; overload;
function ForUpdate(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string; overload;
function ForUpdate(Table: string; Fields: array of string; Keys: array of string): string; overload;
function ForInsert(Table: string; Fields: array of string; ExtraFields: array of string): string; overload;
function ForInsert(Table: string; Fields: array of string): string; overload;
function ForUpdateOrInsert(Updating, Returning:Boolean; Table: string; Fields: array of string; Keys: array of string): string; overload;
function ForDelete(Table: string; Keys: array of string): string; overload;
function MergeArray(S: TArrayFieldNames; A: array of string): TArrayFieldNames;

procedure AppendWhere(var vSql: string); overload;
procedure AppendWhere(var vSql: string; const vWhere: string); overload;
procedure AddWhere(var vSql: string; vWhere: string; Brackets: Boolean = True);
function ExtractOperator(var Value: string): string;

implementation

uses
  Variants;

function DateToSqlDate(vDate: TDateTime): string;
begin
  Result := '''' + FormatDateTime('mm"/"dd"/"yyyy', vDate) + '''';
end;

function QuotedName(const Name: string; const Quote: string): string;
begin
  Result := Name;
  if LeftStr(Name, Length(Quote)) <> Quote then
    Result := Quote + Result;
  if RightStr(Name, Length(Quote)) <> Quote then
    Result := Result + Quote;
end;

function DBName(const Name: string; Engine: TSQL_Engine = sqlePlan): string;
begin
  if Name = '*' then
    Result := Name
  else
  begin
    if DefaultSQLQuoted then
      Result := QuotedName(Name, SQL_Engine_Quoted[DefaultSQLEngine])
    else
      Result := QuotedName(Name, '');
  end;
end;

function DBBool(Value: Boolean; Engine: TSQL_Engine): string;
begin
  Result := SQL_Engine_Bool_Values[DefaultSQLEngine][Value]
end;

function BoolToSqlText(Value: Boolean): string;
begin
  if Value then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

procedure AppendAndText(var vSql: string; const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + DBName(vFieldName) + ' = ''' + Value + ''')'#13;
  end;
end;

procedure AppendAndLike(var vSql: string; const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + DBName(vFieldName) + ' like ''%' + Value + '%'')'#13;
  end;
end;

procedure AppendAndLeftLike(var vSql: string; const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + DBName(vFieldName) + ' like ''' + Value + '%'')'#13;
  end;
end;

procedure AppendAndNotLike(var vSql: string; const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + DBName(vFieldName) + ' not like ''%' + Value + '%'')'#13;
  end;
end;

procedure AppendAndDate(var vSql: string; const vFieldName: string; vDate: TDateTime; vEqual: string; vAndOr: string);
begin
  if (vDate <> 0) then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + DBName(vFieldName) + vEqual + DateToSqlDate(vDate) + ')'#13;
  end;
end;

procedure AppendRangeDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime);
begin
  AppendAndDate(vSql, vFieldName, vFromDate, '>=');
  AppendAndDate(vSql, vFieldName, vToDate, '<=');
end;

procedure AppendBetweenData(var vSql: string; const vFieldName: string; vFrom, vTo: Integer; vAndOr: string = 'and');
var
  S: string;
begin
  if (vFrom <> 0) and (vTo <> 0) then
    S := '(' + DBName(vFieldName) + ' between ' + IntToStr(vFrom) + ' and ' + IntToStr(vTo) + ')'#13
  else if (vFrom <> 0) then
    S := '(' + DBName(vFieldName) + '>=' + IntToStr(vFrom) + ')'#13
  else if (vTo <> 0) then
    S := '(' + DBName(vFieldName) + '<=' + IntToStr(vTo) + ')'#13;
  if (S <> '') then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' ' + S;
  end;
end;

procedure AppendBetweenDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string);
var
  S: string;
begin
  if (vFromDate <> 0) and (vToDate <> 0) then
    S := '(' + DBName(vFieldName) + ' between ' + DateToSqlDate(vFromDate) + ' and ' + DateToSqlDate(vToDate) + ')'#13
  else if (vFromDate <> 0) then
    S := '(' + DBName(vFieldName) + '>=' + DateToSqlDate(vFromDate) + ')'#13
  else if (vToDate <> 0) then
    S := '(' + DBName(vFieldName) + '<=' + DateToSqlDate(vToDate) + ')'#13;
  if (S <> '') then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' ' + S;
  end;
end;

procedure AppendNotBetweenDate(var vSql: string; const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string);
var
  S: string;
begin
  if (vFromDate <> 0) and (vToDate <> 0) then
    S := '(' + DBName(vFieldName) + ' not between ' + DateToSqlDate(vFromDate) + ' and ' + DateToSqlDate(vToDate) + ')'#13
  else if (vFromDate <> 0) then
    S := '(' + DBName(vFieldName) + '<' + DateToSqlDate(vFromDate) + ')'#13
  else if (vToDate <> 0) then
    S := '(' + DBName(vFieldName) + '>' + DateToSqlDate(vToDate) + ')'#13;
  if (S <> '') then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' ' + S;
  end;
end;

procedure AppendAndNumber(var vSql: string; const vFieldName: string; vNumber: Integer; vEqual: string; vAndOr: string);
var
  Str: string;
begin
  if (vSql <> '') then
    vSql := vSql + ' ' + vAndOr;
  if vEqual = '' then
    vEqual := '=';
  Str := IntToStr(vNumber);
  vSql := vSql + ' (' + DBName(vFieldName) + vEqual + Str + ')'#13;
end;

procedure AppendRangeNumber(var vSql: string; const vFieldName: string; vFromNumber, vToNumber: Integer);
begin
  AppendAndNumber(vSql, vFieldName, vFromNumber, '>=');
  AppendAndNumber(vSql, vFieldName, vToNumber, '<=');
end;

procedure AppendRangeData(var vSql: string; const vFieldName: string; vFromNumber, vToNumber: Integer);
begin
  AppendAndData(vSql, vFieldName, vFromNumber, '>=');
  AppendAndData(vSql, vFieldName, vToNumber, '<=');
end;

procedure AppendAndCustom(var vSql: string; const vCustStr: string; vAndOr: string);
begin
  if vCustStr <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    vSql := vSql + ' (' + vCustStr + ')' + #13;
  end;
end;

procedure AppendAndStr(var vSql: string; const vFieldName: string; Value: string; vEqual: string; vAndOr: string);
begin
  if Value > '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    vSql := vSql + ' (' + DBName(vFieldName) + vEqual + Value + ')' + #13;
  end;
end;

procedure AppendAndVariant(var vSql: string; const vFieldName: string; Value: Variant; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  aStr := VarToStr(Value);
  if aStr <> '' then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    vSql := vSql + ' (' + DBName(vFieldName) + vEqual + aStr + ')' + #13;
  end;
end;

procedure AppendAndData(var vSql: string; const vFieldName: string; Value: Integer; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  if (Value <> 0) then
  begin
    if (vSql <> '') then
      vSql := vSql + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    aStr := IntToStr(Value);
    vSql := vSql + ' (' + DBName(vFieldName) + vEqual + aStr + ')'#13;
  end;
end;

procedure AppendAndBool(var vSql: string; const vFieldName: string; Value: Boolean; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  if (vSql <> '') then
    vSql := vSql + ' ' + vAndOr;
  aStr := DBBool(Value);
  vSql := vSql + ' (' + DBName(vFieldName) + vEqual + aStr + ')' + #13;
end;

procedure AddWhere(var vSql: string; vWhere: string; Brackets: Boolean);
begin
  if vWhere <> '' then
  begin
    if vSql <> '' then
      vSql := vSql + ' and'
    else
      vSql := 'where';
    if not (vWhere[1] in [' ', #13]) then
      vSql := vSql + ' ';
    if Brackets then
      vSql := vSql + '(' + vWhere + ')'
    else
      vSql := vSql + vWhere;
  end;
end;

function MakeWhere(const vSql: string): string;
begin

end;

procedure AppendWhere(var vSql: string; const vWhere: string); overload;
begin
  if vWhere <> '' then
    vSql := vSql + #13'where ' + vWhere;
end;

procedure AppendWhere(var vSql: string);
begin
  if vSql <> '' then
    vSql := #13'where ' + vSql;
end;

function ExtractOperator(var Value: string): string;
begin
  if Value <> '' then
  begin
    if Value[1] in ['=', '<', '>'] then
    begin
      Result := Value[1];
      if (Length(Value) > 1) and ((Result = '<') or (Result = '>')) and (Value[2] in ['=', '<', '>']) then
      begin
        Result := Result + Value[2];
      end;
      if Result = '><' then
        Result := '<>';
      Value := Copy(Value, Length(Result) + 1, MaxInt);
    end
    else
      Result := '=';
  end
  else
    Result := '=';
end;

procedure AddAndOr(var vSQL: string; const S: string; vAndOr: string);
begin
  if S <> '' then
  begin
    if vSQL <> '' then
      vSQL := vSQL + ' ' + vAndOr;
    vSQL := vSQL + '(' + S + ')';
  end;
end;

procedure AddLike(var vSql: string; const vAndOr, vFieldName: string; Value: string);
var
  e: string;
begin
  if (vAndOr <> '') and (vSql <> '') then
    vSql := vSql + ' ' + vAndOr + ' ';
  e := ExtractOperator(Value);
  if e = '=' then
    vSql := vSql + '(' + DBName(vFieldName) + ' like ''%' + Value + '%'')'
  else if e = '<>' then
    vSql := vSql + '( not ' + DBName(vFieldName) + 'like ''%' + Value + '%'')';
end;

procedure AddData(var vSql: string; const vAndOr, vFieldName: string; Value: Integer);
begin
  if (vAndOr <> '') and (vSql <> '') then
    vSql := vSql + ' ' + vAndOr + ' ';
  vSql := vSql + '(' + DBName(vFieldName) + '=' + IntToStr(Value) + ')';
end;

function ForSelect(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string;
var
  i: Integer;
  b: Boolean;
begin
  Result := 'select ';
  b := False;
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(Fields[i]);
  end;
  Result := Result + ' from ' + DBName(Table) + ' ';
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + ' where '
    else
      Result := Result + ' and ';
    Result := Result + DBName(Keys[i]) + '=?' + DBName(Keys[i]);
  end;
end;

function ForSelect(Table: string; Fields: array of string; Keys: array of string): string;
begin
  Result := ForSelect(Table, Fields, Keys, []);
end;

function ForUpdate(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string;
var
  i: Integer;
  b: Boolean;
begin
  Result := 'update ' + DBName(Table) + ' set '#13;
  b := False;
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(ExtraFields[i]) + '=?' + DBName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(Fields[i]) + '=?' + DBName(Fields[i]);
  end;
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + #13'where '
    else
      Result := Result + ' and ';
    Result := Result + DBName(Keys[i]) + '=?' + DBName(Keys[i]);
  end;
end;

function ForUpdate(Table: string; Fields: array of string; Keys: array of string): string; overload;
begin
  Result := ForUpdate(Table, Fields, Keys, []);
end;

function ForInsert(Table: string; Fields: array of string; ExtraFields: array of string): string;
var
  i: Integer;
  b: Boolean;
begin
  Result := 'insert into ' + DBName(Table) + ' (';
  b := False;
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + DBName(Fields[i]);
  end;
  b := False;
  Result := Result + ') '#13'values (';
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + '?' + DBName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + '?' + DBName(Fields[i]);
  end;
  Result := Result + ')';
end;

function ForInsert(Table: string; Fields: array of string): string; overload;
begin
  Result := ForInsert(Table, Fields, []);
end;

function ForUpdateOrInsert(Updating, Returning:Boolean; Table: string; Fields: array of string; Keys: array of string): string; overload;
var
  i:Integer;
  b:Boolean;
begin
  if Updating then
    Result := ForUpdate(Table, Fields, Keys)
  else
  begin
    if Returning then
      Result := ForInsert(Table, Fields, [])
    else
      Result := ForInsert(Table, Fields, Keys);
  end;
  if not Updating and Returning and (Length(Keys) <> 0) then
  begin
    Result := Result + #13 + 'returning ';
    b := False;
    for i := 0 to Length(Keys) - 1 do
    begin
      if b then
        Result := Result + ', '
      else
        b := True;
      Result := Result + DBName(Keys[i]);
    end;
  end;
end;

function ForDelete(Table: string; Keys: array of string): string; overload;
var
  i: Integer;
begin
  Result := 'delete from ' + DBName(Table) + #13;
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + #13'where '
    else
      Result := Result + ' and ';
    Result := Result + DBName(Keys[i]) + '=?' + DBName(Keys[i]);
  end;
end;

function MergeArray(S: TArrayFieldNames; A: array of string): TArrayFieldNames;
var
  i: Integer;
  c: Integer;
begin
  Result := S;
  c := Length(Result);
  for i := 0 to Length(A) - 1 do
  begin
    SetLength(Result, c + i + 1);
    Result[c + i] := A[i];
  end;
  Result := Result;
end;

end.

