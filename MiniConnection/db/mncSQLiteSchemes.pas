unit mncSQLiteSchemes;
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
  SysUtils, Classes, contnrs,
  mncSQL, mncSchemes, mncConnections, mncSQLite;

type
  { TmncSQLiteScheme }

  TmncSQLiteScheme = class(TmncScheme)
  private
  protected
    function CreateCMD(SQL: string): TmncSQLiteCommand;
    procedure EnumCMD(Scheme: TmncSchemeItems; SQL: string);//use field 'name' and 'comment'
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
  public
    procedure EnumTables(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumTypes(Scheme: TmncSchemeItems; Options: TschmEnumOptions = []); override;
    procedure EnumConstraints(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndexes(Scheme: TmncSchemeItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; RelationName: string; Options: TschmEnumOptions = []); override;
  end;

implementation

{ TmncSchemeItems }

function TmncSQLiteScheme.CreateCMD(SQL: string): TmncSQLiteCommand;
begin
  Result := TmncSQLiteCommand.Create(Session);
  Result.SQL.Text := SQL;
end;

procedure TmncSQLiteScheme.EnumCMD(Scheme: TmncSchemeItems; SQL: string);
var
  aCMD: TmncSQLiteCommand;
  aComment: string;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      if aCMD.FieldIsExist('comment') then
        aComment := aCMD.Field['comment'].AsString;
      Scheme.Add(aCMD.Field['name'].AsString, aComment);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteScheme.FetchCMD(Strings: TStringList; SQL: string);
var
  aCMD: TmncSQLiteCommand;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      Strings.Add(aCMD.Field['name'].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteScheme.EnumTables(Scheme: TmncSchemeItems; Options: TschmEnumOptions);
begin
  EnumCMD(Scheme, 'select name from sqlite_master where type = ''table''');
end;

procedure TmncSQLiteScheme.EnumViews(Scheme: TmncSchemeItems; Options: TschmEnumOptions
  );
begin

end;

procedure TmncSQLiteScheme.EnumProcedures(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumSequences(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumFunctions(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumExceptions(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumTypes(Scheme: TmncSchemeItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumConstraints(Scheme: TmncSchemeItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteScheme.EnumTriggers(Scheme: TmncSchemeItems;
  RelationName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''trigger''';
  if RelationName <> '' then
    s := s + ' and tbl_name = ''' +RelationName+ '''';
  EnumCMD(Scheme, s);
end;

procedure TmncSQLiteScheme.EnumIndexes(Scheme: TmncSchemeItems; RelationName: string;
  Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''index''';
  if RelationName <> '' then
    s := s + ' and tbl_name = ''' +RelationName+ '''';
  EnumCMD(Scheme, s);
end;

procedure TmncSQLiteScheme.GetTriggerSource(Strings: TStringList; RelationName: string; Options: TschmEnumOptions = []);
var
  s: string;
begin
  s := 'select "sql" as name from sqlite_master where type = ''trigger''';
  s := s + ' and name = ''' +RelationName+ '''';
  FetchCMD(Strings, s);
end;

procedure TmncSQLiteScheme.EnumFields(Scheme: TmncSchemeItems; RelationName: string;
  Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  i: Integer;
  aItem: TmncSchemeItem;
begin
  aCMD := CreateCMD('pragma table_info(''' + (RelationName) + ''')');
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := TmncSchemeItem.Create;
      aItem.Name := aCMD.Field['name'].AsString;
      aItem.Attributes.Add(aCMD.Field['type'].AsString);
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add(aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add(aCMD.Field['cid'].AsString);
      Scheme.Add(aItem);
      aCMD.Next;
    end;
  finally
  end;
end;

end.

