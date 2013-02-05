unit sqlvStdClasses;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

{
  Schema --------------------------
     |                 |               |
     |                 |               |
     |                 |               |
   Group             Group           Group
     |---------------
     |       |       |
   Member  Member  Member

   Open Schema = List the groups and open the first (Default) group
   Open Group = List the members of this group in the members list
   Open Member = Make the member as Schema and open it as Schema
}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  mncSchemas, mncSQLiteSchemas,
  sqlvSessions, sqlvClasses;

type

  { TsqlvDatabase }

  TsqlvDatabase = class(TsqlvNode)
  private
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvMembers }

  TsqlvMembers = class(TsqlvNode)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvTables }

  TsqlvTables = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  TsqlvTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvIndex }

  TsqlvIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvProcedures }

  TsqlvProcedures = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvProcedure }

  TsqlvProcedure = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvViews }

  TsqlvViews = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvView }

  TsqlvView = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvSequences }

  TsqlvSequences = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvDomains }

  TsqlvDomains = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvExceptions }

  TsqlvExceptions = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvFunctions }

  TsqlvFunctions = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvTriggers }

  TsqlvTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvTableTriggers }

  TsqlvTableTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  TsqlvTrigger = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvIndices }

  TsqlvIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvTableIndices }

  TsqlvTableIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvDropIndex }

  TsqlvDropIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvTableFields }

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvDropTable }

  TsqlvDropTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvSelectTable }

  TsqlvSelectTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvInsertTable }

  TsqlvInsertTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvDropField }

  TsqlvDropField = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvNewField }

  TsqlvNewField = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvEmptyTable }

  TsqlvEmptyTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvExportSQL }

  TsqlvExportSQL = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  { TsqlvImportSQL }

  TsqlvImportSQL = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vParams: TmncSchemaParams); override;
  end;

  TsqlvExecuteType = (execNormal, execExport, execImport);

  TsqlvGui = class(TObject)
  public
    procedure ExecuteScript(ExecuteType: TsqlvExecuteType); virtual; abstract;

    procedure LoadEditor(vNode: TsqlvNode; vStrings:TStringList);
    procedure LoadEditor(vNode: TsqlvNode; S: string); virtual; abstract;

    procedure EnumGroups(vNode: TsqlvNode; vGroup, vMemberName: string; Params: TmncSchemaParams; vSelectDefault: Boolean); virtual; abstract;
    procedure EnumMembers(vNode: TsqlvNode; const vMemberName: string); virtual; abstract;

    procedure LoadSchema(vNode: TsqlvNode; vNodes: TsqlvNodes; vMemberName: string; Params: TmncSchemaParams; vSelectDefault: Boolean); virtual; abstract;
    procedure LoadHeader(vNode: TsqlvNode; vHeader: TStringList); virtual; abstract;
    procedure LoadMembers(vNode: TsqlvNode; vSchemaName: string; vNodes: TsqlvNodes); virtual; abstract;
  end;

var
  sqlvGui: TsqlvGui = nil;

implementation

uses
  Contnrs;

{ TsqlvGui }

procedure TsqlvGui.LoadEditor(vNode: TsqlvNode; vStrings: TStringList);
begin
  LoadEditor(vNode, vStrings.Text);
end;

{ TsqlvDatabase }

constructor TsqlvDatabase.Create;
begin
  inherited;
  Group := 'Databases';
  Name := 'Database';
  Title := 'Database';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASE;
end;

procedure TsqlvDatabase.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.EnumGroups(Self, 'Database', Value, vParams, True);
end;

{ TsqlvTables }

constructor TsqlvTables.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Tables';
  Title := 'Tables';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTables.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName := 'Table';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumTables(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvProcedures }

constructor TsqlvProcedures.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Procedures';
  Title := 'Procedures';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedures.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Procedure';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumProcedures(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvViews }

constructor TsqlvViews.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Views';
  Title := 'Views';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvViews.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='View';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumViews(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvSequences }

constructor TsqlvSequences.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Sequences';
  Title := 'Sequences';
  Kind := sokSequence;
  ImageIndex := IMG_GENERATOR;
end;

procedure TsqlvSequences.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Sequences';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumSequences(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvDomains }

constructor TsqlvDomains.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Domains';
  Title := 'Domains';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TsqlvDomains.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Domain';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumDomains(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvExceptions }

constructor TsqlvExceptions.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Exceptions';
  Title := 'Exceptions';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TsqlvExceptions.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Exception';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumExceptions(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvFunctions }

constructor TsqlvFunctions.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Functions';
  Title := 'Functions';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TsqlvFunctions.EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Function';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumFunctions(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvMembers }

constructor TsqlvMembers.Create;
begin
  inherited;
end;

procedure TsqlvMembers.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.EnumMembers(Self, Value);
end;

function TsqlvMembers.GetCanExecute: Boolean;
begin
  Result := True;
end;

{ TsqlvProcedure }

constructor TsqlvProcedure.Create;
begin
  inherited;
  Group := 'Procedure';
  Name := 'ProcedureSource';
  Title := 'Procedure Source';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedure.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aSchema: TmncSQLiteSchema;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aSchema := TmncSQLiteSchema.Create;
    try
      aSchema.Session := sqlvEngine.Session.DBSession;
      //aSchema.ExtractObject(aStrings, sokProcedure, MemberName, [ekAlter]);
    finally
      aSchema.Free;
    end;
    sqlvGui.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvView }

constructor TsqlvView.Create;
begin
  inherited;
  Group := 'View';
  Name := 'ViewSource';
  Title := 'View Source';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvView.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aSchema: TmncSQLiteSchema;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aSchema := TmncSQLiteSchema.Create;
    try
      aSchema.Session := sqlvEngine.Session.DBSession;
      //aSchema.ExtractObject(aStrings, sokView, MemberName, [ekAlter]);
    finally
      aSchema.Free;
    end;
    sqlvGui.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTableTriggers }

constructor TsqlvTableTriggers.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTableTriggers.EnumSchema(var SchemaName: string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  inherited;
  SchemaName := 'Trigger';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumTriggers(SchemaItems, MemberName);
  finally
    aSchema.Free;
  end;
end;

{ TsqlvTrigger }

constructor TsqlvTrigger.Create;
begin
  inherited;
  Group := 'Triggers';
  Name := 'Trigger';
  Title := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTrigger.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aSchema: TmncSQLiteSchema;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aSchema := TmncSQLiteSchema.Create;
    try
      aSchema.Session := sqlvEngine.Session.DBSession;
      aSchema.GetTriggerSource(aStrings, Value, [ekAlter]);
    finally
      aSchema.Free;
    end;
    sqlvGui.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTableIndices }

constructor TsqlvTableIndices.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvTableIndices.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Unique');
end;

procedure TsqlvTableIndices.EnumSchema(var SchemaName: string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName :='Index';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumIndices(SchemaItems, MemberName);
  finally
    aSchema.Free
  end;
end;

{ TsqlvDropIndex }

constructor TsqlvDropIndex.Create;
begin
  inherited;
  Group := 'Index';
  Name := 'DropIndex';
  Title := 'Drop Index';
  Kind := sokIndex;
  Style := Style + [nsCommand];
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvDropIndex.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;

end;

{ TsqlvTableFields }

constructor TsqlvTableFields.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Fields';
  Title := 'Fields';
  Kind := sokField;
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvTableFields.EnumHeader(Header: TStringList);
begin
  inherited;
  Header.Add('Type');
  Header.Add('PK');//primiry key
  Header.Add('NN');//not null
  Header.Add('Default');
end;

procedure TsqlvTableFields.EnumSchema(var SchemaName: string;
  SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Field';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumFields(SchemaItems, MemberName);
  finally
    aSchema.Free
  end;
end;

{ TsqlvNewField }

constructor TsqlvNewField.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'NewField';
  Title := 'New Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvNewField.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aStrings: TStringList;
  aFieldName: string;
begin
  inherited;
  {if InputQuery('New', 'New Field for ' + MemberName, aFieldName) then
  begin
    aStrings := TStringList.Create;
    aStrings.Text := 'alter table ' + FBQuoteName(MemberName) + ' add ' + FBQuoteName(aFieldName) + ' smallint';
    Open(vSession, aStrings);
    aStrings.Free;
  end;}
end;

{ TsqlvEmptyTable }

constructor TsqlvEmptyTable.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'EmptyTable';
  Title := 'Empty';
  Kind := sokNone;
  Style := [nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvEmptyTable.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + Value;
  sqlvGui.LoadEditor(Self, aStrings);
  aStrings.Free;
end;

{ TsqlvDropField }

constructor TsqlvDropField.Create;
begin
  inherited;
  Group := 'Field';
  Name := 'DropField';
  Title := 'Drop Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvDropField.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + vParams['Table'] + ' drop column ' + Value;
    sqlvGui.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTriggers }

constructor TsqlvTriggers.Create;
begin
  inherited Create;
  Group := 'Database';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTriggers.EnumSchema(var SchemaName: string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName := 'Trigger';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumTriggers(SchemaItems);
  finally
    aSchema.Free
  end;
end;

{ TsqlvTable }

constructor TsqlvTable.Create;
begin
  inherited Create;
  Group := 'Tables';
  Name := 'Table';
  Title := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTable.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.EnumGroups(Self, Name, Value, vParams, True);
end;

{ TsqlvIndex }

constructor TsqlvIndex.Create;
begin
  inherited Create;
  Group := 'Indices';
  Name := 'Index';
  Title := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvIndex.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.EnumMembers(Self, Value);
end;

procedure TsqlvIndex.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Value');
end;

procedure TsqlvIndex.EnumSchema(var SchemaName: string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName := 'Property';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.GetIndexInfo(SchemaItems, MemberName);
  finally
    aSchema.Free
  end;
end;

{ TsqlvIndices }

constructor TsqlvIndices.Create;
begin
  inherited Create;
  Group := 'Database';
  Name := 'Indices';
  Title := 'Indices';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvIndices.EnumHeader(Header: TStringList);
begin
  inherited;
end;

procedure TsqlvIndices.EnumSchema(var SchemaName: string; SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
  SchemaName:='Index';
  aSchema := TmncSQLiteSchema.Create;
  try
    aSchema.Session := sqlvEngine.Session.DBSession;
    aSchema.EnumIndices(SchemaItems, '');
  finally
    aSchema.Free
  end;
end;

{ TsqlvDropTable }

constructor TsqlvDropTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'DropTable';
  Title := 'Drop';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvDropTable.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.LoadEditor(Self, 'drop table ' + Value);
end;

{ TsqlvSelectTable }

constructor TsqlvSelectTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'SelectTable';
  Title := 'Select';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvSelectTable.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  inherited;
  sqlvGui.LoadEditor(Self, 'select * from ' + Value);
end;

{ TsqlvInsertTable }

constructor TsqlvInsertTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'InsertTable';
  Title := 'Insert';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvInsertTable.DoExecute(const Value: string; vParams: TmncSchemaParams);
var
  aSchema: TmncSQLiteSchema;
  aItems: TmncSchemaItems;
  s1, s2: string;
  i: Integer;
begin
  inherited DoExecute(Value, vParams);
  aItems := TmncSchemaItems.Create;
  try
    aSchema := TmncSQLiteSchema.Create;
    try
      aSchema.Session := sqlvEngine.Session.DBSession;
      aSchema.EnumFields(aItems, Value);
    finally
      aSchema.Free
    end;
    s1 := '';
    s2 := '';
    for i := 0 to aItems.Count - 1 do
    begin
      if i > 0 then
      begin
        s1 := s1 + ' ,';
        s2 := s2 + ' ,';
      end;
      s1 := s1+ aItems[i].Name;
      s2 := s2 + '?' + aItems[i].Name;
    end;
  finally
    aItems.Free;
  end;
  sqlvGui.LoadEditor(Self, 'insert into ' + Value + '(' + s1 + ') values (' + s2 +')');
end;

{ TsqlvExportSQL }

constructor TsqlvExportSQL.Create;
begin
  inherited Create;
  Group := 'GUI.SQL';
  Name := 'ExportSQL';
  Title := 'Export';
  Kind := sokNone;
  Style := [nsNeedSession, nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvExportSQL.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  sqlvGui.ExecuteScript(execExport);
end;

{ TsqlvImportSQL }

constructor TsqlvImportSQL.Create;
begin
  inherited;
  Group := 'GUI.SQL';
  Name := 'ImportSQL';
  Title := 'Import';
  Kind := sokNone;
  Style := [nsNeedSession, nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvImportSQL.DoExecute(const Value: string; vParams: TmncSchemaParams);
begin
  sqlvGui.ExecuteScript(execImport);
end;

initialization
  sqlvEngine.RegisterViewer([TsqlvDatabase]);
  sqlvEngine.RegisterViewer([TsqlvTables, TsqlvTable, TsqlvTableFields, TsqlvExportSQL, TsqlvImportSQL]);
  sqlvEngine.RegisterViewer([TsqlvSelectTable, TsqlvInsertTable, TsqlvEmptyTable, TsqlvDropTable]);
  sqlvEngine.RegisterViewer([TsqlvIndices, TsqlvTableIndices, TsqlvIndex, TsqlvDropIndex]);
  sqlvEngine.RegisterViewer([TsqlvDropField{, TsqlvNewField}]);
  sqlvEngine.RegisterViewer([TsqlvViews, TsqlvView]);
  sqlvEngine.RegisterViewer([TsqlvTriggers, TsqlvTrigger, TsqlvTableTriggers]);
  //sqlvEngine.RegisterViewer([TsqlvDomains, TsqlvExceptions, TsqlvFunctions]);
  //sqlvEngine.RegisterViewer([TsqlvProcedures, TsqlvProcedure]);
  //sqlvEngine.RegisterViewer([TsqlvSequences]);
end.

