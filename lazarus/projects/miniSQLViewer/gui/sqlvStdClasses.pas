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
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvMembers }

  TsqlvMembers = class(TsqlvNode)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvTables }

  TsqlvTables = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  TsqlvTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvIndex }

  TsqlvIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvProcedures }

  TsqlvProcedures = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvProcedure }

  TsqlvProcedure = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvViews }

  TsqlvViews = class(TsqlvMembers)
  public
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvView }

  TsqlvView = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvSequences }

  TsqlvSequences = class(TsqlvMembers)
  public
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvDomains }

  TsqlvDomains = class(TsqlvMembers)
  public
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvExceptions }

  TsqlvExceptions = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvFunctions }

  TsqlvFunctions = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvTriggers }

  TsqlvTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvTableTriggers }

  TsqlvTableTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  TsqlvTrigger = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvIndices }

  TsqlvIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvTableIndices }

  TsqlvTableIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvDropIndex }

  TsqlvDropIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvTableFields }

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvDropTable }

  TsqlvDropTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvSelectTable }

  TsqlvSelectTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvInsertTable }

  TsqlvInsertTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvDropField }

  TsqlvDropField = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvNewField }

  TsqlvNewField = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvEmptyTable }

  TsqlvEmptyTable = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvExportSQL }

  TsqlvExportSQL = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  { TsqlvImportSQL }

  TsqlvImportSQL = class(TsqlvNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; vStack: TsqlvStack); override;
  end;

  TsqlvExecuteType = (execNormal, execExport, execImport);

  TsqlvGui = class(TObject)
  public
    procedure ExecuteScript(ExecuteType: TsqlvExecuteType); virtual; abstract;

    procedure LoadEditor(vNode: TsqlvNode; vStrings:TStringList);
    procedure LoadEditor(vNode: TsqlvNode; S: string); virtual; abstract;
    procedure ShowSchema(vNode: TsqlvNode; vStack: TsqlvStack; vSelectDefault: Boolean); virtual; abstract;
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

procedure TsqlvDatabase.DoExecute(const Value: string; vStack: TsqlvStack);
begin
  inherited;
  sqlvGui.ShowSchema(Self, vStack, True);
end;

{ TsqlvTables }

constructor TsqlvTables.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Tables';
  Title := 'Tables';
  Item := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTables.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Procedure';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedures.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'View';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvViews.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Sequence';
  Kind := sokSequence;
  ImageIndex := IMG_GENERATOR;
end;

procedure TsqlvSequences.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Domain';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TsqlvDomains.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Exception';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TsqlvExceptions.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Function';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TsqlvFunctions.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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

procedure TsqlvMembers.DoExecute(const Value: string; vStack: TsqlvStack);
begin
  inherited;
  //sqlvGui.ShowSchema(Self, Value);
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
  Item := 'Source';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedure.DoExecute(const Value: string; vStack: TsqlvStack);
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
  Item := 'Source';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvView.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvTableTriggers.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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

procedure TsqlvTrigger.DoExecute(const Value: string; vStack: TsqlvStack);
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
  Item := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvTableIndices.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Unique');
end;

procedure TsqlvTableIndices.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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

procedure TsqlvDropIndex.DoExecute(const Value: string; vStack: TsqlvStack);
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
  Item := 'Field';
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

procedure TsqlvTableFields.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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

procedure TsqlvNewField.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvEmptyTable.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvDropField.DoExecute(const Value: string; vStack: TsqlvStack);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + vStack['Table'] + ' drop column ' + Value;
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
  Item := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTriggers.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  //Item := 'Field'; nop it has a child groups
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTable.DoExecute(const Value: string; vStack: TsqlvStack);
begin
  inherited;
  sqlvGui.ShowSchema(Self, vStack, True);
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

procedure TsqlvIndex.DoExecute(const Value: string; vStack: TsqlvStack);
begin
  inherited;
  //sqlvGui.EnumMembers(Self, Value);
end;

procedure TsqlvIndex.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Value');
end;

procedure TsqlvIndex.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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
  Item := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvIndices.EnumHeader(Header: TStringList);
begin
  inherited;
end;

procedure TsqlvIndices.EnumSchema(SchemaItems: TmncSchemaItems; const MemberName: string);
var
  aSchema: TmncSQLiteSchema;
begin
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

procedure TsqlvDropTable.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvSelectTable.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvInsertTable.DoExecute(const Value: string; vStack: TsqlvStack);
var
  aSchema: TmncSQLiteSchema;
  aItems: TmncSchemaItems;
  s1, s2: string;
  i: Integer;
begin
  inherited DoExecute(Value, vStack);
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

procedure TsqlvExportSQL.DoExecute(const Value: string; vStack: TsqlvStack);
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

procedure TsqlvImportSQL.DoExecute(const Value: string; vStack: TsqlvStack);
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

