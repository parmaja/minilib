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
    procedure EnumGroups(vGroup, vMemberName: string; vOpenDefault: Boolean);
    procedure EnumMembers(const vMemberName: string);
    procedure LoadHeader(vHeader:TStringList);
    procedure LoadEditor(vStrings:TStringList);
    procedure LoadGroups(vNodes:TsqlvNodes; vMemberName:string; vOpenDefault: Boolean);
    procedure LoadMembers(vSchemeName:string; vNodes:TsqlvNodes);
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
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
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
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvProcedure = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvViews = class(TsqlvMembers)
  public
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvView = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvSequences = class(TsqlvMembers)
  public
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvTypes = class(TsqlvMembers)
  public
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvExceptions = class(TsqlvMembers)
  public
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  TsqlvFunctions = class(TsqlvMembers)
  public
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
    constructor Create; override;
  end;

  { TsqlvTriggers }

  TsqlvTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvTableTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvTrigger = class(TsqlvGuiNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  TsqlvTableIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
  end;

  TsqlvDropIndex = class(TsqlvNode)
  public
    constructor Create; override;
    procedure Execute(const MemberName: string); override;
  end;

  { TsqlvTableFields }

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string); override;
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
  Group := 'Databases';
  Name := 'Database';
  Title := 'Database';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASE;
end;

procedure TsqlvDatabase.Execute(const MemberName: string);
begin
  inherited;
  EnumGroups('Database', MemberName, True);
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

procedure TsqlvTables.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Procedures';
  Title := 'Procedures';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedures.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Views';
  Title := 'Views';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvViews.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Sequences';
  Title := 'Sequences';
  Kind := sokGenerator;
  ImageIndex := IMG_GENERATOR;
end;

procedure TsqlvSequences.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Types';
  Title := 'Types';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TsqlvTypes.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Exceptions';
  Title := 'Exceptions';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TsqlvExceptions.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Database';
  Name := 'Functions';
  Title := 'Functions';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TsqlvFunctions.EnumScheme(var SchemeName:string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Procedure';
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
  Group := 'View';
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
  Group := 'Table';
  Name := 'TableTriggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTableTriggers.EnumScheme(var SchemeName: string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Triggers';
  Name := 'Trigger';
  Title := 'Trigger';
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
  Group := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  Kind := sokIndexes;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvTableIndices.EnumScheme(var SchemeName: string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Index';
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
  Group := 'Table';
  Name := 'TableFields';
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

procedure TsqlvTableFields.EnumScheme(var SchemeName: string;
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
  Group := 'Table';
  Name := 'NewField';
  Title := 'New Field';
  Kind := sokFields;
  Style := Style + [nsCommand];
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
  Group := 'Table';
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
  Group := 'Fields';
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

procedure TsqlvGuiNode.EnumGroups(vGroup, vMemberName: string; vOpenDefault: Boolean);
var
  i: Integer;
  aNodes: TsqlvNodes;
begin
  aNodes := TsqlvNodes.Create;
  try
    sqlvEngine.Enum(vGroup, aNodes);
    LoadGroups(aNodes, vMemberName, vOpenDefault);
  finally
    aNodes.Free;
  end;
end;

procedure TsqlvGuiNode.EnumMembers(const vMemberName: string);
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
    EnumScheme(aSchemeName, aItems, vMemberName);
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
    end;
  end;
end;

procedure TsqlvGuiNode.LoadEditor(vStrings: TStringList);
begin
  with MainForm do
  begin
    SQLEdit.Lines.BeginUpdate;
    SQLEdit.Lines.Text := vStrings.Text;
    SQLEdit.Lines.EndUpdate;
    State := sqlsSQL;
    SQLEdit.SetFocus;
  end;
end;

procedure TsqlvGuiNode.LoadGroups(vNodes: TsqlvNodes; vMemberName:string; vOpenDefault: Boolean);
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
    GroupsList.ItemIndex := d;
    MebmerName := vMemberName;
    TitleLbl.Caption := Self.Title + ': ' + vMemberName;
    if vOpenDefault then
      OpenGroup;
    State := sqlsMembers;
  end;
end;

procedure TsqlvGuiNode.LoadMembers(vSchemeName:string; vNodes: TsqlvNodes);
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
      MembersGrid.Cells[0, 0] := vSchemeName;
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
    SchemeName := vSchemeName;
    State := sqlsMembers;
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

procedure TsqlvTriggers.EnumScheme(var SchemeName: string; SchemeItems: TmncSchemeItems; const MemberName: string);
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
  Group := 'Tables';
  Name := 'Table';
  Title := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTable.Execute(const MemberName: string);
begin
  inherited;
  EnumGroups(Name, MemberName, True);
end;

initialization
  sqlvEngine.RegisterViewer([TsqlvDatabase]);
  sqlvEngine.RegisterViewer([TsqlvTables, TsqlvTable, TsqlvTableFields, TsqlvTableIndices, TsqlvDropIndex]);
  sqlvEngine.RegisterViewer([TsqlvEmptyTable, TsqlvDropField, TsqlvNewField]);
  sqlvEngine.RegisterViewer([TsqlvViews, TsqlvView]);
  sqlvEngine.RegisterViewer([TsqlvTriggers, TsqlvTrigger, TsqlvTableTriggers]);
  //sqlvEngine.RegisterViewer([TsqlvTypes, TsqlvExceptions, TsqlvFunctions]);
  //sqlvEngine.RegisterViewer([TsqlvProcedures, TsqlvProcedure]);
  //sqlvEngine.RegisterViewer([TsqlvSequences]);
end.

