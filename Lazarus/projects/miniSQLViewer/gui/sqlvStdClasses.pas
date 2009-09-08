unit sqlvStdClasses;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  mncSchemas, mncSQLiteSchemas,
  sqlvSessions, sqlvClasses;

type

  { TsqlvGuiNode }

  TsqlvGuiNode = class(TsqlvNode)
  public
    procedure LoadEditor(vStrings:TStringList);
    procedure LoadEditor(S: string);

    procedure EnumGroups(vGroup, vMemberName: string; Params: TmncParams; vSelectDefault: Boolean);
    procedure EnumMembers(const vMemberName: string);
    procedure LoadHeader(vHeader:TStringList);
    procedure LoadGroup(vNodes:TsqlvNodes; vMemberName:string; Params: TmncParams; vSelectDefault: Boolean);
    procedure LoadMembers(vSchemaName:string; vNodes:TsqlvNodes);
  end;

  { TsqlvDatabase }

  TsqlvDatabase = class(TsqlvGuiNode)
  private
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvMembers }

  TsqlvMembers = class(TsqlvGuiNode)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvTables }

  TsqlvTables = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  TsqlvTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvIndex }

  TsqlvIndex = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
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

  TsqlvProcedure = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvViews }

  TsqlvViews = class(TsqlvMembers)
  public
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvView }

  TsqlvView = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
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

  TsqlvTrigger = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
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
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvTableFields }

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumSchema(var SchemaName:string; SchemaItems: TmncSchemaItems; const MemberName: string); override;
  end;

  { TsqlvDropTable }

  TsqlvDropTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvSelectTable }

  TsqlvSelectTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvDropTable }

  { TsqlvInsertTable }

  TsqlvInsertTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvDropField }

  TsqlvDropField = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvNewField }

  TsqlvNewField = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvEmptyTable }

  TsqlvEmptyTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvExportSQL }

  TsqlvExportSQL = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

  { TsqlvImportSQL }

  TsqlvImportSQL = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure DoExecute(const Value: string; Params: TmncParams = nil); override;
  end;

implementation

uses
  Main, Contnrs;

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

procedure TsqlvDatabase.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  EnumGroups('Database', Value, Params, True);
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
  Kind := sokSequences;
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
  Kind := sokDomains;
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

procedure TsqlvMembers.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  EnumMembers(Value);
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

procedure TsqlvProcedure.DoExecute(const Value: string; Params: TmncParams);
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
    LoadEditor(aStrings);
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

procedure TsqlvView.DoExecute(const Value: string; Params: TmncParams);
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
    LoadEditor(aStrings);
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

procedure TsqlvTrigger.DoExecute(const Value: string; Params: TmncParams);
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
    LoadEditor(aStrings);
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
  Kind := sokIndices;
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
  Kind := sokIndices;
  Style := Style + [nsCommand];
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvDropIndex.DoExecute(const Value: string; Params: TmncParams);
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
  Kind := sokFields;
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
  Kind := sokFields;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvNewField.DoExecute(const Value: string; Params: TmncParams);
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
  Title := 'Empty Table';
  Kind := sokNone;
  Style := [nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvEmptyTable.DoExecute(const Value: string; Params: TmncParams);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + Value;
  LoadEditor(aStrings);
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

procedure TsqlvDropField.DoExecute(const Value: string; Params: TmncParams);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + Params['Table'] + ' drop column ' + Value;
    LoadEditor(aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvGuiNode }

procedure TsqlvGuiNode.EnumGroups(vGroup, vMemberName: string; Params: TmncParams; vSelectDefault: Boolean);
var
  aNodes: TsqlvNodes;
begin
  aNodes := TsqlvNodes.Create;
  try
    sqlvEngine.Enum(vGroup, aNodes, sqlvEngine.Session.IsActive);
    LoadGroup(aNodes, vMemberName, Params, vSelectDefault);
  finally
    aNodes.Free;
  end;
end;

procedure TsqlvGuiNode.EnumMembers(const vMemberName: string);
var
  i: Integer;
  aItems: TmncSchemaItems;
  aSchemaName: string;
  aNodes: TsqlvNodes;
  aNode: TsqlvNode;
begin
  inherited;
  aNodes := TsqlvNodes.Create;
  aItems := TmncSchemaItems.Create;
  try
    EnumSchema(aSchemaName, aItems, vMemberName);
    for i := 0 to aItems.Count -1 do
    begin
      aNode := TsqlvNode.Create;
      aNode.Name := aItems[i].Name;
      aNode.Attributes.Assign(aItems[i].Attributes);
      aNodes.Add(aNode);
    end;
    LoadMembers(aSchemaName, aNodes);
  finally
    aItems.Free;
    aNodes.Free;
  end;
end;

procedure TsqlvGuiNode.LoadHeader(vHeader: TStringList);
var
  i: Integer;
begin
  with MainForm do
  begin
    MembersGrid.ColCount := vHeader.Count;
    for i := 0 to vHeader.Count -1 do
    begin
      MembersGrid.Cells[i, 0] := vHeader[i];
//      if i = 0 then
//        MembersGrid.Columns[i].SizePriority := 50;
    end;
  end;
end;

procedure TsqlvGuiNode.LoadEditor(vStrings: TStringList);
begin
  LoadEditor(vStrings.Text);
end;

procedure TsqlvGuiNode.LoadEditor(S: string);
begin
  with MainForm do
  begin
    AddRecentSQL;
    SQLEdit.Lines.BeginUpdate;
    SQLEdit.Lines.Text := S;
    SQLEdit.Lines.EndUpdate;
    State := sqlsSQL;
    SQLEdit.SetFocus;
  end;
end;

procedure TsqlvGuiNode.LoadGroup(vNodes: TsqlvNodes; vMemberName:string; Params: TmncParams; vSelectDefault: Boolean);
var
  i, c: Integer;
  d: Integer;
begin
  with MainForm do
  begin
    d := -1;
    c := 0;
    GroupsList.Items.BeginUpdate;
    try
      GroupsList.Clear;
      GroupsNames.Clear;
      for i := 0 to vNodes.Count -1 do
      begin
        if not (nsCommand in vNodes[i].Style) then
        begin
          GroupsList.Items.Add(vNodes[i].Title);
          GroupsNames.Add(vNodes[i].Name);
          if (d < 0) and (nsDefault in vNodes[i].Style) then
            d := c;
          c := c + 1;
        end;
      end;
    finally
      GroupsList.Items.EndUpdate;
    end;
    if d < 0 then
      d := 0;
    if vNodes.Count > 0 then
      GroupsList.ItemIndex := d;
    GroupInfo.Name := Self.Name;
    GroupInfo.Value := vMemberName;
    TitleLbl.Caption := Self.Title + ': ' + vMemberName;
    if vSelectDefault then
      OpenGroup;
    State := sqlsMembers;
  end;
end;

procedure TsqlvGuiNode.LoadMembers(vSchemaName:string; vNodes: TsqlvNodes);
var
  i, j, c: Integer;
  d: Integer;
  aHeader: TStringList;
  aCols: Integer;
begin
  aHeader:=TStringList.Create;
  try
    EnumHeader(aHeader);
    LoadHeader(aHeader);
    aCols := aHeader.Count;
  finally
    aHeader.Free;
  end;
  with MainForm do
  begin
    d := -1;
    c := 0;
    MembersGrid.BeginUpdate;
    try
      MembersGrid.RowCount := 1;//fixed only
      MembersGrid.Cells[0, 0] := vSchemaName;
      for i := 0 to vNodes.Count -1 do
      begin
        if not (nsCommand in vNodes[i].Style) then
        begin
          MembersGrid.RowCount := c + 2;
          for j := 0 to aCols - 1 do
          begin
            if j = 0 then
              MembersGrid.Cells[j, c + 1] := vNodes[i].Name
            else if (j - 1) < vNodes[i].Attributes.Count then //maybe Attributes not have all data
              MembersGrid.Cells[j, c + 1] := vNodes[i].Attributes[j - 1];
          end;
          if (d < 0) and (nsDefault in vNodes[i].Style) then
            d := c;
          c := c + 1;
        end;
      end;
    finally
      MembersGrid.EndUpdate;
    end;
    if d < 0 then
      d := 0;
    MembersGrid.Row := d;
    MembersGrid.AutoSizeColumns;
    SchemaName := vSchemaName;
    State := sqlsMembers;
    UpdateToolActions(vSchemaName);
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

procedure TsqlvTable.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  EnumGroups(Name, Value, Params, True);
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

procedure TsqlvIndex.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  EnumMembers(Value);
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
  Kind := sokIndices;
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
  Title := 'Drop Table';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvDropTable.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  LoadEditor('drop table ' + Value);
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

procedure TsqlvSelectTable.DoExecute(const Value: string; Params: TmncParams);
begin
  inherited;
  LoadEditor('select * from ' + Value);
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

procedure TsqlvInsertTable.DoExecute(const Value: string; Params: TmncParams);
var
  aSchema: TmncSQLiteSchema;
  aItems: TmncSchemaItems;
  s1, s2: string;
  i: Integer;
begin
  inherited DoExecute(Value, Params);
  aItems := TmncSchemaItems.create;
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
  LoadEditor('insert into ' + Value + '(' + s1 + ') values (' + s2 +')');
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

procedure TsqlvExportSQL.DoExecute(const Value: string; Params: TmncParams);
begin
  with MainForm do
    ExecuteScript(execExport);
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

procedure TsqlvImportSQL.DoExecute(const Value: string; Params: TmncParams);
begin
  with MainForm do
    ExecuteScript(execImport);
end;

initialization
  sqlvEngine.RegisterViewer([TsqlvDatabase]);
  sqlvEngine.RegisterViewer([TsqlvTables, TsqlvTable, TsqlvTableFields, TsqlvExportSQL, TsqlvImportSQL]);
  sqlvEngine.RegisterViewer([TsqlvEmptyTable, TsqlvDropTable, TsqlvSelectTable, TsqlvInsertTable]);
  sqlvEngine.RegisterViewer([TsqlvIndices, TsqlvTableIndices, TsqlvIndex, TsqlvDropIndex]);
  sqlvEngine.RegisterViewer([TsqlvDropField{, TsqlvNewField}]);
  sqlvEngine.RegisterViewer([TsqlvViews, TsqlvView]);
  sqlvEngine.RegisterViewer([TsqlvTriggers, TsqlvTrigger, TsqlvTableTriggers]);
  //sqlvEngine.RegisterViewer([TsqlvDomains, TsqlvExceptions, TsqlvFunctions]);
  //sqlvEngine.RegisterViewer([TsqlvProcedures, TsqlvProcedure]);
  //sqlvEngine.RegisterViewer([TsqlvSequences]);
end.

