unit sqlvStdClasses;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  mncSchemes, sqlvSessions, sqlvClasses, ComCtrls, StdCtrls, mncSQLiteSchemes;

type

  { TsqlvGuiNode }

  TsqlvGuiNode = class(TsqlvNode)
  public
    procedure EnumGroups(vRelated: string; OpenDefault: Boolean);
    procedure EnumMembers(const MemberName: string);
    procedure LoadEditor(Strings:TStringList);
    procedure LoadGroups(Nodes:TsqlvNodes; OpenDefault: Boolean);
    procedure LoadMembers(vSchemeName:string; Nodes:TsqlvNodes);
  end;

  { TsqlvDatabase }

  TsqlvDatabase = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvMembers = class(TsqlvGuiNode)
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  { TsqlvTables }

  TsqlvTables = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
    //procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvProcedures = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvProcedure = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvViews = class(TsqlvMembers)
  public
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvView = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvSequences = class(TsqlvMembers)
  public
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvTypes = class(TsqlvMembers)
  public
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvExceptions = class(TsqlvMembers)
  public
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvFunctions = class(TsqlvMembers)
  public
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvTriggers }

  TsqlvTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvTableTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvTrigger = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvTableIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvDropIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvDropField = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvNewField = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvEmptyTable = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

implementation

uses
  Main, Contnrs;

{ TsqlvDatabase }

constructor TsqlvDatabase.Create;
begin
  inherited;
  Related := 'Databases';
  Name := 'Database';
  Title := 'Database';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASE;
end;

procedure TsqlvDatabase.Execute(const MemberName: string);
begin
  inherited;
  EnumGroups('Database', True);
end;

{ TsqlvTables }

constructor TsqlvTables.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Tables';
  Title := 'Tables';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTables.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName := 'Table';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumTables(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvProcedures }

constructor TsqlvProcedures.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Procedures';
  Title := 'Procedures';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedures.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Procedure';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumProcedures(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvViews }

constructor TsqlvViews.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Views';
  Title := 'Views';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvViews.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='View';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumViews(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvSequences }

constructor TsqlvSequences.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Sequences';
  Title := 'Sequences';
  Kind := sokGenerator;
  ImageIndex := IMG_GENERATOR;
end;

procedure TsqlvSequences.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Sequences';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumSequences(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvTypes }

constructor TsqlvTypes.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Types';
  Title := 'Types';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TsqlvTypes.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Domain';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumTypes(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvExceptions }

constructor TsqlvExceptions.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Exceptions';
  Title := 'Exceptions';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TsqlvExceptions.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Exception';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumExceptions(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvFunctions }

constructor TsqlvFunctions.Create;
begin
  inherited;
  Related := 'Database';
  Name := 'Functions';
  Title := 'Functions';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TsqlvFunctions.Enum(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Function';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumFunctions(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvMembers }

constructor TsqlvMembers.Create;
begin
  inherited;
end;

procedure TsqlvMembers.Execute(const MemberName: string);
begin
  inherited;
  EnumMembers(MemberName);
end;

function TsqlvMembers.GetCanExecute: Boolean;
begin
  Result := True;
end;

{ TsqlvProcedure }

constructor TsqlvProcedure.Create;
begin
  inherited;
  Related := 'Procedure';
  Name := 'ProcedureMetadata';
  Title := 'Procedure Metadata';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedure.Execute(const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aScheme := TmncSQLiteScheme.Create;
    try
      aScheme.Session := sqlvEngine.Session.DBSession;
      //aScheme.ExtractObject(aStrings, sokProcedure, MemberName, [ekAlter]);
    finally
      aScheme.Free;
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
  Related := 'View';
  Name := 'ViewMetadata';
  Title := 'View Metadata';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvView.Execute(const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aScheme := TmncSQLiteScheme.Create;
    try
      aScheme.Session := sqlvEngine.Session.DBSession;
      //aScheme.ExtractObject(aStrings, sokView, MemberName, [ekAlter]);
    finally
      aScheme.Free;
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
  Related := 'Table';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTableTriggers.Enum(var SchemeName: string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  inherited;
  SchemeName := 'Trigger';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumTriggers(SchemeItems, MemberName);
  finally
    aScheme.Free;
  end;
end;

{ TsqlvTrigger }

constructor TsqlvTrigger.Create;
begin
  inherited;
  Related := 'Trigger';
  Name := 'TriggerMetadata';
  Title := 'Trigger Metadata';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTrigger.Execute(const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aScheme := TmncSQLiteScheme.Create;
    try
      aScheme.Session := sqlvEngine.Session.DBSession;
      aScheme.GetTriggerSource(aStrings, MemberName, [ekAlter]);
    finally
      aScheme.Free;
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
  Related := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  Kind := sokIndexes;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvTableIndices.Enum(var SchemeName: string; SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Index';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumIndexes(SchemeItems, MemberName);
  finally
    aScheme.Free
  end;
end;

{ TsqlvDropIndex }

constructor TsqlvDropIndex.Create;
begin
  inherited;
  Related := 'Index';
  Name := 'DropIndex';
  Title := 'Drop Index';
  Kind := sokIndexes;
  Style := Style + [nsCommand];
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvDropIndex.Execute(const MemberName: string);
begin
  inherited;

end;

{ TsqlvTableFields }

constructor TsqlvTableFields.Create;
begin
  inherited;
  Related := 'Table';
  Name := 'Fields';
  Title := 'Fields';
  Kind := sokFields;
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvTableFields.Enum(var SchemeName: string;
  SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName:='Field';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumFields(SchemeItems, MemberName);
  finally
    aScheme.Free
  end;
end;

{ TsqlvNewField }

constructor TsqlvNewField.Create;
begin
  inherited;
  Related := 'Table';
  Name := 'NewField';
  Title := 'New Field';
  Kind := sokFields;
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvNewField.Execute(const MemberName: string);
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
  Related := 'Table';
  Name := 'EmptyTable';
  Title := 'Empty Table';
  Kind := sokNone;
  Style := [nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvEmptyTable.Execute(const MemberName: string);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + MemberName;
  LoadEditor(aStrings);
  aStrings.Free;
end;

{ TsqlvDropField }

constructor TsqlvDropField.Create;
begin
  inherited;
  Related := 'Fields';
  Name := 'DropField';
  Title := 'Drop Field';
  Kind := sokFields;
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvDropField.Execute(const MemberName: string);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'alter table ' + '' + ' drop column ' + MemberName;
  LoadEditor(aStrings);
  aStrings.Free;
end;

{ TsqlvSelectProcedure }

{ TsqlvGuiNode }

procedure TsqlvGuiNode.EnumGroups(vRelated: string; OpenDefault: Boolean);
var
  i: Integer;
  aNodes: TsqlvNodes;
begin
  aNodes := TsqlvNodes.Create;
  try
    sqlvEngine.EnumRelated(vRelated, aNodes);
    LoadGroups(aNodes, OpenDefault);
  finally
    aNodes.Free;
  end;
end;

procedure TsqlvGuiNode.EnumMembers(const MemberName: string);
var
  i: Integer;
  aItems: TmncSchemeItems;
  aSchemeName: string;
  aNodes: TsqlvNodes;
  aNode: TsqlvNode;
begin
  inherited;
  aNodes := TsqlvNodes.Create;
  aItems := TmncSchemeItems.Create;
  try
    Enum(aSchemeName, aItems, MemberName);
    for i := 0 to aItems.Count -1 do
    begin
      aNode := TsqlvNode.Create;
      aNode.Name := aItems[i].Name;
      aNode.Comment := aItems[i].Comment;
      aNode.Attributes.Assign(aItems[i].Attributes);
      aNodes.Add(aNode);
    end;
    LoadMembers(aSchemeName, aNodes);
  finally
    aItems.Free;
    aNodes.Free;
  end;
end;

procedure TsqlvGuiNode.LoadEditor(Strings: TStringList);
begin
  with MainForm do
  begin
    SQLEdit.Lines.BeginUpdate;
    SQLEdit.Lines.Text := Strings.Text;
    SQLEdit.Lines.EndUpdate;
    State := sqlsSQL;
    SQLEdit.SetFocus;
  end;
end;

procedure TsqlvGuiNode.LoadGroups(Nodes: TsqlvNodes; OpenDefault: Boolean);
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
      for i := 0 to Nodes.Count -1 do
      begin
        if not (nsCommand in Nodes[i].Style) then
        begin
          GroupsList.Items.Add(Nodes[i].Name);
          if (d < 0) and (nsDefault in Nodes[i].Style) then
            d := c;
          c := c + 1;
        end;
      end;
    finally
      GroupsList.Items.EndUpdate;
    end;
    if d < 0 then
      d := 0;
    GroupsList.ItemIndex := d;
    TitleLbl.Caption := Self.Title;
    if OpenDefault then
      OpenGroup;
    State := sqlsMembers;
  end;
end;

procedure TsqlvGuiNode.LoadMembers(vSchemeName:string; Nodes: TsqlvNodes);
var
  i, c: Integer;
  d: Integer;
begin
  with MainForm do
  begin
    d := -1;
    c := 0;
    MembersList.Items.BeginUpdate;
    try
      MembersList.Clear;
      for i := 0 to Nodes.Count -1 do
      begin
        if not (nsCommand in Nodes[i].Style) then
        begin
          MembersList.Items.Add(Nodes[i].Name);
          if (d < 0) and (nsDefault in Nodes[i].Style) then
            d := c;
          c := c + 1;
        end;
      end;
    finally
      MembersList.Items.EndUpdate;
    end;
    if d < 0 then
      d := 0;
    MembersList.ItemIndex := d;
    SchemeName := vSchemeName;
    State := sqlsMembers;
  end;
end;

{ TsqlvTriggers }

constructor TsqlvTriggers.Create;
begin
  inherited Create;
  Related := 'Database';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTriggers.Enum(var SchemeName: string;
  SchemeItems: TmncSchemeItems; const MemberName: string);
var
  aScheme: TmncSQLiteScheme;
begin
  SchemeName := 'Trigger';
  aScheme := TmncSQLiteScheme.Create;
  try
    aScheme.Session := sqlvEngine.Session.DBSession;
    aScheme.EnumTriggers(SchemeItems);
  finally
    aScheme.Free
  end;
end;

{ TsqlvTable }

constructor TsqlvTable.Create;
begin
  inherited Create;
  Related := 'Tables';
  Name := 'Table';
  Title := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTable.Execute(const MemberName: string);
begin
  inherited;
  EnumGroups(Name, True);
end;

initialization
  sqlvEngine.RegisterViewer([TsqlvDatabase]);
  sqlvEngine.RegisterViewer([TsqlvTables, TsqlvTable, TsqlvTableFields, TsqlvTableIndices, TsqlvDropIndex]);
  sqlvEngine.RegisterViewer([TsqlvEmptyTable, TsqlvDropField, TsqlvNewField]);
  sqlvEngine.RegisterViewer([TsqlvViews, TsqlvView]);
  sqlvEngine.RegisterViewer([TsqlvTriggers, TsqlvTrigger, TsqlvTableTriggers]);
  sqlvEngine.RegisterViewer([TsqlvTypes, TsqlvExceptions, TsqlvFunctions]);
  //sqlvEngine.RegisterViewer([TsqlvProcedures, TsqlvProcedure]);
  //sqlvEngine.RegisterViewer([TsqlvSequences]);
end.

