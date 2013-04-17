unit mncFBSchemas;
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
  mncSchemas, mncConnections, mncFBUtils, mncFirebird;

type

  { TmncFBSchema }

  TmncFBSchema = class(TmncSchema)
  private
    function GetSession: TmncSession;
    procedure SetSession(AValue: TmncSession);
  protected
    function CreateCMD(SQL: string): TmncFBCommand;
    procedure EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TschmEnumOptions; ByField: string = 'name'): string;
  public
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumDomains(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumConstraints(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndices(Schema: TmncSchemaItems; SQLName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions = []); override;
    procedure GetViewSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions = []); override;
    procedure GetIndexInfo(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions = []); override;
    property Session: TmncSession read GetSession write SetSession;//alias for FLink in base class
  end;

implementation

{ TmncSchemaItems }

function TmncFBSchema.GetSession: TmncSession;
begin
  Result := Link as TmncSession;
end;

procedure TmncFBSchema.SetSession(AValue: TmncSession);
begin
  inherited Link := AValue;
end;

function TmncFBSchema.CreateCMD(SQL: string): TmncFBCommand;
begin
  Result := TmncFBCommand.CreateBy(Session);
  Result.SQL.Text := SQL;
end;

procedure TmncFBSchema.EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string; Fields: array of string);
var
  aCMD: TmncFBCommand;
  aItem: TmncSchemaItem;
  i: Integer;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := Schema.Add(aCMD.Field['name'].AsTrimString);
      aItem.Kind := vKind;
      for i := Low(Fields) to High(Fields) do
        aItem.Attributes.Add(Fields[i], aCMD.Field[Fields[i]].AsTrimString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncFBSchema.EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string);
begin
  EnumCMD(Schema, vKind, SQL, []);
end;

procedure TmncFBSchema.FetchCMD(Strings: TStringList; SQL: string);
var
  aCMD: TmncFBCommand;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      Strings.Add(aCMD.Field['name'].AsTrimString);
      aCMD.Next;
    end;
  finally
  end;
end;

function TmncFBSchema.GetSortSQL(Options: TschmEnumOptions; ByField: string): string;
begin
  if ekSort in Options then
    Result := ' order by ' + ByField
  else
    Result := '';
end;

procedure TmncFBSchema.EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions);
const
  sSQL =
    'select rdb$relation_name name from rdb$relations ' +
    'where ' +
    '  (rdb$system_flag <> 1 or rdb$system_flag is null) ' +
    //'and rdb$flags <> 1'+//System ekSystem in Options
    ' and rdb$view_blr is null ';

begin
  EnumCMD(Schema, sokTable, sSQL + GetSortSQL(Options));
end;

procedure TmncFBSchema.EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
  EnumCMD(Schema, sokView, 'select rdb$relation_name as name, rdb$description as description from rdb$relations where (rdb$system_flag <> 1 or rdb$system_flag is null) and (rdb$flags <> 0) and rdb$view_blr is not null '+ GetSortSQL(Options));
end;

procedure TmncFBSchema.EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumDomains(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumConstraints(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncFBSchema.EnumTriggers(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
const
  sSQL =
    'select rdb$trigger_name name from rdb$triggers trg join rdb$relations rel on  trg.rdb$relation_name = rel.rdb$relation_name ' +
    'where ' +
    '(rel.rdb$system_flag <> 1 or rel.rdb$system_flag is null) and ' +
    'not exists (select * from rdb$check_constraints chk where trg.rdb$trigger_name = chk.rdb$trigger_name) ';
var
  s: string;
begin
  s := sSQL;
  if SQLName <> '' then
    s := s + ' and trg.rdb$relation_name = ''' + SQLName + '''';
  EnumCMD(Schema, sokIndex, s);
end;

procedure TmncFBSchema.EnumIndices(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
const
sSQL =
  'select idx.rdb$index_name as name, idx.rdb$relation_name as relname, idx.rdb$unique_flag as "Unique", idx.rdb$index_type as type ' + LineEnding +
  'from rdb$indices idx '+ LineEnding +
  'join rdb$relations relc on idx.rdb$relation_name = relc.rdb$relation_name ' + LineEnding +
  'where '+ LineEnding +
  '    (relc.rdb$system_flag <> 1 or relc.rdb$system_flag is null) ' + LineEnding +
  '    and not exists (select * from rdb$relation_constraints rc ' + LineEnding +
  '                     where rc.rdb$index_name = idx.rdb$index_name)' + LineEnding;
var
  s: string;
begin
  s := sSQL;
  if SQLName <> '' then
  begin
    s := s + ' and idx.rdb$relation_name = ''' + SQLName + '''';
    EnumCMD(Schema, sokIndex, s, ['Unique']);
  end
  else
    EnumCMD(Schema, sokIndex, s);
end;

function GetTriggerType(TriggerType: Integer): string;
var
  i, t: Integer;
const
  sTypes: array[0..2] of string = ('insert', 'update', 'delete');
begin
    // this function taked from trigger.cpp in project FlameRobin
    // For explanation: read README.universal_triggers file in Firebird's
    //                  doc/sql.extensions directory
  i := TriggerType mod 2;
  if i > 0 then
    Result := 'before '
  else
    Result := 'after ';
  TriggerType := TriggerType + 1;
  TriggerType := TriggerType shr 1;
  for i := 0 to 2 do
  begin
    t := TriggerType mod 4;
    if t > 0 then
    begin
      if (i > 0) then
        Result := Result + ' or ';
      Result := Result + sTypes[t - 1];
      TriggerType := TriggerType shr 2;
    end;
  end;
end;

procedure TmncFBSchema.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions);
const
  sSQL =
    'select trg.rdb$trigger_name name, trg.rdb$trigger_source source, trg.rdb$trigger_sequence as sequence, trg.rdb$trigger_type as type, trg.rdb$trigger_inactive as inactive, trg.rdb$flags as flags, trg.rdb$relation_name relname '+
    'from rdb$triggers trg join rdb$relations rel on  trg.rdb$relation_name = rel.rdb$relation_name ' +
    'where ';
//    '(rel.rdb$system_flag <> 1 or rel.rdb$system_flag is null) and ' +
//    'not exists (select * from rdb$check_constraints chk where trg.rdb$trigger_name = chk.rdb$trigger_name) ';
var
  s: string;
  aName: string;
  aRelationName: string;
  aCMD: TmncFBCommand;
  ActiveStr: string;
begin
  s := sSQL + ' trg.rdb$trigger_name = ''' + SQLName + '''';
  aCMD := CreateCMD(s);
  try
    if aCMD.Execute then
    begin
      aName := aCMD.Field['name'].AsTrimString;
      aRelationName := aCMD.Field['relname'].AsTrimString;
      if aCMD.Field['inactive'].IsNull then
        ActiveStr := 'inactive'
      else if aCMD.Field['inactive'].AsInteger = 1 then
        ActiveStr := 'inactive'
      else
        ActiveStr := 'active';

      if aCMD.Field['flags'].AsInteger <> 1 then
        Strings.Add('/* ');

      Strings.Add(Format('create or alter trigger %s for %s', [FBQuoteName(aName), FBQuoteName(aRelationName)]));

      Strings.Add(Format('%s %s position %d', [ActiveStr, GetTriggerType(aCMD.Field['type'].AsInteger),
          aCMD.Field['sequence'].AsInteger]));

      if not aCMD.Field['source'].IsNull then
        Strings.Text := Strings.Text + aCMD.Field['source'].AsTrimString;

      if aCMD.Field['flags'].AsInteger <> 1 then
        Strings.Add(' */');
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncFBSchema.GetViewSource(Strings: TStringList; SQLName: string;
  Options: TschmEnumOptions);
const
  sSQL =
    'select rdb$relation_name relname, rdb$view_source source'+ LineEnding +
    'from rdb$relations ' + LineEnding +
    'where ' + LineEnding +
    '  (rdb$system_flag <> 1 or rdb$system_flag is null) ' + LineEnding +
    '  and rdb$flags = 1 ' +
    '  and  not rdb$view_blr is null ' + LineEnding +
    '  and rdb$relation_name = ?name';

  sSQLColumns =
      'select rdb$field_name as name, rdb$relation_name relname from rdb$relation_fields ' + LineEnding +
      'where ' + LineEnding +
      '  rdb$relation_name = ?name ' + LineEnding +
      'order by rdb$field_position';
var
  C, S, aName: string;
  aCMD: TmncFBCommand;
begin
  aCMD := CreateCMD('');
  try
    aCMD.SQL.Text := sSQL;
    aCMD.Param['name'].AsTrimString := SQLName;
    S := '';
    if aCMD.Execute then
    begin
      S := aCMD.Field['source'].AsString;
      aName := FBQuoteName(aCMD.Field['relname'].AsTrimString);
      aCMD.Close;

      aCMD.SQL.Text := sSQLColumns;
      aCMD.Param['name'].AsTrimString := SQLName;
      aCMD.Execute;
      C := '';
      while not aCMD.Eof do
      begin
        if C <> '' then
          C := C + ', ';
        C := C + FBQuoteName(aCMD.Field['name'].AsTrimString);
        aCMD.Next;
      end;
      Strings.Text := 'recreate view ' + aName + '(' + C + ')' + LineEnding + 'as ' + LineEnding + Trim(S);
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncFBSchema.GetIndexInfo(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
var
  aCMD: TmncFBCommand;
  aItem: TmncSchemaItem;
begin
  aCMD := CreateCMD('');
  try
    if aCMD.Execute then
    begin
      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Name';
      aItem.Attributes.Add('name', SQLName);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Field';
      aItem.Attributes.Add('field', aCMD.Field['name'].AsTrimString);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'CID';
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsTrimString);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Sequence NO';
      aItem.Attributes.Add('seqno',  aCMD.Field['seqno'].AsTrimString);
      Schema.Add(aItem);
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncFBSchema.EnumFields(Schema: TmncSchemaItems; SQLName: string; Options: TschmEnumOptions);
var
  aCMD: TmncFBCommand;
  aItem: TmncSchemaItem;
begin
  aCMD := CreateCMD('select rdb$field_name as name, rdb$description as description from rdb$relation_fields');
  aCMD.SQL.Add('where rdb$relation_name = ''' + SQLName + '''');
  if not (ekSystem in Options) then
     aCMD.SQL.Add('and rdb$system_flag <> 1 ');
  if (ekSort in Options) then
    aCMD.SQL.Add('order by name')
  else
    aCMD.SQL.Add('order by rdb$field_position');

  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := TmncSchemaItem.Create;
      aItem.Name := aCMD.Field['name'].AsTrimString;
  {    aItem.Attributes.Add('type', aCMD.Field['type'].AsString);
      aItem.Attributes.Add('pk', IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add('notnull', IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add('dflt_value', aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsString);}
      Schema.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

end.

