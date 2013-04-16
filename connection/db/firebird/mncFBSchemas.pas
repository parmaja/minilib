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
  mncSchemas, mncConnections, mncFirebird;

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
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TschmEnumOptions = []); override;
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

procedure TmncFBSchema.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TschmEnumOptions);
const
  sSQL =
    'select rdb$trigger_name name, rdb$trigger_source source, rdb$trigger_inactive as inactive, rdb$flags as flags from rdb$triggers trg join rdb$relations rel on  trg.rdb$relation_name = rel.rdb$relation_name ' +
    'where ' +
    '(rel.rdb$system_flag <> 1 or rel.rdb$system_flag is null) and ' +
    'not exists (select * from rdb$check_constraints chk where trg.rdb$trigger_name = chk.rdb$trigger_name) ';
var
  s: string;
begin
  s := sSQL + ' and trg.rdb$relation_name = ''' + SQLName + '''';

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

