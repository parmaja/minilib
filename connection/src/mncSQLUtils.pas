unit mncSQLUtils deprecated;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils; 

type
  TArrayFieldNames = array of string;
//  TSQLModes = (sqlmPlan, sqlmSQLite, sqlmFirebird, sqlmPostgres);

  TSQLMode = class abstract(TObject)
  public
    class function QuoteName(const S: string): string; virtual; abstract;
    class function BoolValue(const B: Boolean): string; virtual; abstract;
  end;

  TSQLStandard = class(TSQLMode)
  public
    class function QuoteName(const S: string): string; override;
    class function BoolValue(const B: Boolean): string; override;
  end;

  TSQLiteMode = class(TSQLMode)
  public
    class function QuoteName(const S: string): string; override;
    class function BoolValue(const B: Boolean): string; override;
  end;

  TFirebirdMode = class(TSQLMode)
  public
    class function QuoteName(const S: string): string; override;
    class function BoolValue(const B: Boolean): string; override;
  end;

  TPostgreMode = class(TSQLMode)
  public
    class function QuoteName(const S: string): string; override;
    class function BoolValue(const B: Boolean): string; override;
  end;

  { TSQLObject }

  TSQLObject<T: TSQLMode> = record
  private
    FQuoteNames: Boolean;
    FText: string;
  public
    //constructor Init(AMode: TSQLModes = sqlmPlan; AQuoteNames: Boolean = False);
    //destructor Done; virtual;
    function SQLName(const Name: string): string;
    function SQLBool(Value: Boolean): string;
    function SQLDate(vDate: TDateTime): string;

    procedure AddAndOr(const S: string; vAndOr: string);
    procedure AddLike(const vFieldName: string; Value: string; vAndOr: string = 'and'); overload;
    procedure AddNotLike(const vFieldName, Value: string; vAndOr: string = 'and');
    procedure AddLeftLike(const vFieldName, Value: string; vAndOr: string = 'and');
    procedure AddText(const vFieldName, Value: string; vAndOr: string = 'and');
    procedure AddDate(const vFieldName: string; vDate: TDateTime; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddBetweenDate(const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string = 'and');
    procedure AddNotBetweenDate(const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string = 'and');
    procedure AddBetweenData(const vFieldName: string; vFrom, vTo: Integer; vAndOr: string = 'and');
    procedure AddNumber(const vFieldName: string; vNumber: Int64; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddData(const vFieldName: string; Value: Integer; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddRangeDate(const vFieldName: string; vFromDate, vToDate: TDateTime);
    procedure AddRangeNumber(const vFieldName: string; vFromNumber, vToNumber: Integer);
    procedure AddRangeData(const vFieldName: string; vFromNumber, vToNumber: Integer);
    procedure AddStr(const vFieldName: string; Value: string; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddVariant(const vFieldName: string; Value: Variant; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddCustom(const vCustStr: string; vAndOr: string = 'and'; vCondition: Boolean = True);
    procedure AddBool(const vFieldName: string; Value: Boolean; vEqual: string = '='; vAndOr: string = 'and');
    procedure AddIntBool(const vFieldName: string; Value: Boolean; vEqual: string = '='; vAndOr: string = 'and');


    function ForSelect(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string; overload;
    function ForSelect(Table: string; Fields: array of string; Keys: array of string): string; overload;
    function ForUpdate(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string; overload;
    function ForUpdate(Table: string; Fields: array of string; Keys: array of string): string; overload;
    function ForInsert(Table: string; Fields: array of string; ExtraFields: array of string): string; overload;
    function ForInsert(Table: string; Fields: array of string): string; overload;
    function ForUpdateOrInsert(Updating, Returning:Boolean; Table: string; Fields: array of string; Keys: array of string): string; overload;
    function ForDelete(Table: string; Keys: array of string): string; overload;

    function MergeArray(S: TArrayFieldNames; A: array of string): TArrayFieldNames;

    procedure AddWhere; overload;
    procedure AddWhere(vWhere: string; Brackets: Boolean = True); overload;
    function ExtractOperator(var Value: string): string;
    property QuoteNames: Boolean read FQuoteNames write FQuoteNames;
    property Text: string read FText write FText;
  end;

implementation

uses
  StrUtils, Variants;

{
  SQLMode_Bool_Values: array[TSQLModes] of array[Boolean] of string = (
      ('False', 'True'), //sqlePlan
      ('false', 'true'), //sqlSQLite
      ('0', '1'), //sqlFirebird
      ('false', 'true') //sqlPostgres
    );
}

function QuotedName(const Name: string; const Quote: string): string;
begin
  Result := Name;
  if LeftStr(Name, Length(Quote)) <> Quote then
    Result := Quote + Result;
  if RightStr(Name, Length(Quote)) <> Quote then
    Result := Result + Quote;
end;


{ TSQLObject }

function TSQLObject<T>.SQLDate(vDate: TDateTime): string;
begin
  Result := '''' + FormatDateTime('mm"/"dd"/"yyyy', vDate) + '''';
end;

function TSQLObject<T>.SQLName(const Name: string): string;
begin
  if Name = '*' then
    Result := Name
  else
  begin
    if QuoteNames then
      Result := T.QuoteName(Name)
//      Result := QuotedName(Name, '"')
    else
      Result := Name;
  end;
end;

function TSQLObject<T>.SQLBool(Value: Boolean): string;
begin
  Result := T.BoolValue(Value);
end;

procedure TSQLObject<T>.AddText(const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' (' + SQLName(vFieldName) + ' = ''' + Value + ''')'#13;
  end;
end;

procedure TSQLObject<T>.AddLike(const vFieldName: string; Value: string; vAndOr: string);
var
  e: string;
begin
  if (vAndOr <> '') and (FText <> '') then
    FText := FText + ' ' + vAndOr + ' ';
  e := ExtractOperator(Value);
  if e = '=' then
    FText := FText + '(' + SQLName(vFieldName) + ' like ''%' + Value + '%'')'
  else if e = '<>' then
    FText := FText + '( not ' + SQLName(vFieldName) + 'like ''%' + Value + '%'')';
end;

procedure TSQLObject<T>.AddLeftLike(const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' (' + SQLName(vFieldName) + ' like ''' + Value + '%'')'#13;
  end;
end;

procedure TSQLObject<T>.AddNotLike(const vFieldName, Value: string; vAndOr: string);
begin
  if Value <> '' then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' (' + SQLName(vFieldName) + ' not like ''%' + Value + '%'')'#13;
  end;
end;

procedure TSQLObject<T>.AddDate(const vFieldName: string; vDate: TDateTime; vEqual: string; vAndOr: string);
begin
  if (vDate <> 0) then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' (' + SQLName(vFieldName) + vEqual + SQLDate(vDate) + ')'#13;
  end;
end;

procedure TSQLObject<T>.AddRangeDate(const vFieldName: string; vFromDate, vToDate: TDateTime);
begin
  AddDate(vFieldName, vFromDate, '>=');
  AddDate(vFieldName, vToDate, '<=');
end;

procedure TSQLObject<T>.AddBetweenData(const vFieldName: string; vFrom, vTo: Integer; vAndOr: string = 'and');
var
  S: string;
begin
  if (vFrom <> 0) and (vTo <> 0) then
    S := '(' + SQLName(vFieldName) + ' between ' + IntToStr(vFrom) + ' and ' + IntToStr(vTo) + ')'#13
  else if (vFrom <> 0) then
    S := '(' + SQLName(vFieldName) + '>=' + IntToStr(vFrom) + ')'#13
  else if (vTo <> 0) then
    S := '(' + SQLName(vFieldName) + '<=' + IntToStr(vTo) + ')'#13;
  if (S <> '') then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' ' + S;
  end;
end;

procedure TSQLObject<T>.AddBetweenDate(const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string);
var
  S: string;
begin
  if (vFromDate <> 0) and (vToDate <> 0) then
    S := '(' + SQLName(vFieldName) + ' between ' + SQLDate(vFromDate) + ' and ' + SQLDate(vToDate) + ')'#13
  else if (vFromDate <> 0) then
    S := '(' + SQLName(vFieldName) + '>=' + SQLDate(vFromDate) + ')'#13
  else if (vToDate <> 0) then
    S := '(' + SQLName(vFieldName) + '<=' + SQLDate(vToDate) + ')'#13;
  if (S <> '') then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' ' + S;
  end;
end;

procedure TSQLObject<T>.AddNotBetweenDate(const vFieldName: string; vFromDate, vToDate: TDateTime; vAndOr: string);
var
  S: string;
begin
  if (vFromDate <> 0) and (vToDate <> 0) then
    S := '(' + SQLName(vFieldName) + ' not between ' + SQLDate(vFromDate) + ' and ' + SQLDate(vToDate) + ')'#13
  else if (vFromDate <> 0) then
    S := '(' + SQLName(vFieldName) + '<' + SQLDate(vFromDate) + ')'#13
  else if (vToDate <> 0) then
    S := '(' + SQLName(vFieldName) + '>' + SQLDate(vToDate) + ')'#13;
  if (S <> '') then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' ' + S;
  end;
end;

procedure TSQLObject<T>.AddNumber(const vFieldName: string; vNumber: Int64; vEqual: string; vAndOr: string);
var
  Str: string;
begin
  if (vAndOr <> '') and (FText <> '') then
    FText := FText + ' ' + vAndOr;
  if vEqual = '' then
    vEqual := '=';
  Str := IntToStr(vNumber);
  FText := FText + ' (' + SQLName(vFieldName) + vEqual + Str + ')'#13;
end;

procedure TSQLObject<T>.AddRangeNumber(const vFieldName: string; vFromNumber, vToNumber: Integer);
begin
  AddNumber(vFieldName, vFromNumber, '>=');
  AddNumber(vFieldName, vToNumber, '<=');
end;

procedure TSQLObject<T>.AddRangeData(const vFieldName: string; vFromNumber, vToNumber: Integer);
begin
  AddData(vFieldName, vFromNumber, '>=');
  AddData(vFieldName, vToNumber, '<=');
end;

procedure TSQLObject<T>.AddCustom(const vCustStr: string; vAndOr: string; vCondition: Boolean);
begin
  if vCondition and (vCustStr <> '') then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    FText := FText + ' (' + vCustStr + ')' + #13;
  end;
end;

procedure TSQLObject<T>.AddStr(const vFieldName: string; Value: string; vEqual: string; vAndOr: string);
begin
  if Value > '' then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    FText := FText + ' (' + SQLName(vFieldName) + vEqual + Value + ')' + #13;
  end;
end;

procedure TSQLObject<T>.AddVariant(const vFieldName: string; Value: Variant; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  aStr := VarToStr(Value);
  if aStr <> '' then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    FText := FText + ' (' + SQLName(vFieldName) + vEqual + aStr + ')' + #13;
  end;
end;

procedure TSQLObject<T>.AddData(const vFieldName: string; Value: Integer; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  if (Value <> 0) then
  begin
    if (FText <> '') then
      FText := FText + ' ' + vAndOr;
    if vEqual = '' then
      vEqual := '=';
    aStr := IntToStr(Value);
    FText := FText + ' (' + SQLName(vFieldName) + vEqual + aStr + ')'#13;
  end;
end;

procedure TSQLObject<T>.AddBool(const vFieldName: string; Value: Boolean; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  if (FText <> '') then
    FText := FText + ' ' + vAndOr;
  aStr := SQLBool(Value);
  FText := FText + ' (' + SQLName(vFieldName) + vEqual + aStr + ')' + #13;
end;

procedure TSQLObject<T>.AddIntBool(const vFieldName: string; Value: Boolean; vEqual: string; vAndOr: string);
var
  aStr: string;
begin
  if (FText <> '') then
    FText := FText + ' ' + vAndOr;
  if Value then
    aStr := '1'
  else
    aStr := '0';
  FText := FText + ' (' + SQLName(vFieldName) + vEqual + aStr + ')' + #13;
end;

procedure TSQLObject<T>.AddWhere(vWhere: string; Brackets: Boolean);
begin
  if vWhere <> '' then
  begin
    if FText <> '' then
      FText := FText + ' and'
    else
      FText := 'where';
    if not (vWhere[1] in [' ', #13]) then
      FText := FText + ' ';
    if Brackets then
      FText := FText + '(' + vWhere + ')'
    else
      FText := FText + vWhere;
  end;
end;

procedure TSQLObject<T>.AddWhere;
begin
  if FText <> '' then
    FText := #13'where ' + FText;
end;

function TSQLObject<T>.ExtractOperator(var Value: string): string;
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

procedure TSQLObject<T>.AddAndOr(const S: string; vAndOr: string);
begin
  if S <> '' then
  begin
    if FText <> '' then
      FText := FText + ' ' + vAndOr;
    FText := FText + '(' + S + ')';
  end;
end;

function TSQLObject<T>.ForSelect(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string;
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
    Result := Result + SQLName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + SQLName(Fields[i]);
  end;
  Result := Result + ' from ' + SQLName(Table) + ' ';
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + ' where '
    else
      Result := Result + ' and ';
    Result := Result + SQLName(Keys[i]) + '=?' + SQLName(Keys[i]);
  end;
end;

function TSQLObject<T>.ForSelect(Table: string; Fields: array of string; Keys: array of string): string;
begin
  Result := ForSelect(Table, Fields, Keys, []);
end;

function TSQLObject<T>.ForUpdate(Table: string; Fields: array of string; Keys: array of string; ExtraFields: array of string): string;
var
  i: Integer;
  b: Boolean;
begin
  Result := 'update ' + SQLName(Table) + ' set '#13;
  b := False;
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + SQLName(ExtraFields[i]) + '=?' + SQLName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + SQLName(Fields[i]) + '=?' + SQLName(Fields[i]);
  end;
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + #13'where '
    else
      Result := Result + ' and ';
    Result := Result + SQLName(Keys[i]) + '=?' + SQLName(Keys[i]);
  end;
end;

function TSQLObject<T>.ForUpdate(Table: string; Fields: array of string; Keys: array of string): string;
begin
  Result := ForUpdate(Table, Fields, Keys, []);
end;

function TSQLObject<T>.ForInsert(Table: string; Fields: array of string; ExtraFields: array of string): string;
var
  i: Integer;
  b: Boolean;
begin
  Result := 'insert into ' + SQLName(Table) + ' (';
  b := False;
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + SQLName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + SQLName(Fields[i]);
  end;
  b := False;
  Result := Result + ') '#13'values (';
  for i := 0 to Length(ExtraFields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + '?' + SQLName(ExtraFields[i]);
  end;
  for i := 0 to Length(Fields) - 1 do
  begin
    if b then
      Result := Result + ', '
    else
      b := True;
    Result := Result + '?' + SQLName(Fields[i]);
  end;
  Result := Result + ')';
end;

function TSQLObject<T>.ForInsert(Table: string; Fields: array of string): string;
begin
  Result := ForInsert(Table, Fields, []);
end;

function TSQLObject<T>.ForUpdateOrInsert(Updating, Returning:Boolean; Table: string; Fields: array of string; Keys: array of string): string;
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
      Result := Result + SQLName(Keys[i]);
    end;
  end;
end;

function TSQLObject<T>.ForDelete(Table: string; Keys: array of string): string;
var
  i: Integer;
begin
  Result := 'delete from ' + SQLName(Table) + #13;
  for i := 0 to Length(Keys) - 1 do
  begin
    if i = 0 then
      Result := Result + #13'where '
    else
      Result := Result + ' and ';
    Result := Result + SQLName(Keys[i]) + '=?' + SQLName(Keys[i]);
  end;
end;

function TSQLObject<T>.MergeArray(S: TArrayFieldNames; A: array of string): TArrayFieldNames;
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

{ TSQLiteMode }

class function TSQLiteMode.BoolValue(const B: Boolean): string;
const
  cBoolStrs: array [Boolean] of String = ('False', 'True');
begin
  Result := cBoolStrs[B]
end;

class function TSQLiteMode.QuoteName(const S: string): string;
begin
  Result := QuotedName(S, '"');
end;

{ TSQLStandard }

class function TSQLStandard.BoolValue(const B: Boolean): string;
const
  cBoolStrs: array [Boolean] of String = ('false', 'true');
begin
  Result := cBoolStrs[B]
end;

class function TSQLStandard.QuoteName(const S: string): string;
begin
  Result := '';
end;

{ TFirebirdMode }

class function TFirebirdMode.BoolValue(const B: Boolean): string;
const
  cBoolStrs: array [Boolean] of String = ('0', '1');
begin
  Result := cBoolStrs[B]
end;

class function TFirebirdMode.QuoteName(const S: string): string;
begin
  Result := QuotedName(S, '"');
end;

{ TPostgreMode }

class function TPostgreMode.BoolValue(const B: Boolean): string;
const
  cBoolStrs: array [Boolean] of String = ('false', 'true');
begin
  Result := cBoolStrs[B]
end;

class function TPostgreMode.QuoteName(const S: string): string;
begin
  Result := QuotedName(S, '"');
end;

end.

