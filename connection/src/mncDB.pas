unit mncDB;
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
  SysUtils, Classes, Contnrs,
  mnUtils, mncCommons, mncConnections, mncORM, mncMeta;

type
  { TmncEngine }

  TmncEngine = class(TObject)
  public
    Name: string;
    Title: string;
    ConnectionClass: TmncConnectionClass;
    ORMClass: TmncORMClass;
    MetaClass: TmncMetaClass;
  end;

{ TmncEngines }

  TmncEngines = class(TObjectList)
  private
    function GetItems(Index: Integer): TmncEngine;
  public
    function ComposeConnectionString(EngineName, Resource, Host, User, Password, Role: string): string; overload;
    function ComposeConnectionString(Connection: TmncConnection): string; overload;
    procedure DecomposeConnectionString(Composed:string; out EngineName, Resource, Host, User, Password, Role: string); overload;
    procedure DecomposeConnectionString(Composed:string; Connection: TmncConnection); overload;

    function RegisterConnection(vName, vTitle: string; vConnectionClass: TmncConnectionClass): TmncEngine;
    function RegisterORM(vName: string; vORMClass: TmncORMClass): TmncEngine;
    function RegisterMeta(vName: string; vMetaClass: TmncMetaClass): TmncEngine;
    function Find(vName: string): TmncEngine; overload;
    function Find(vORM: TmncORM): TmncEngine; overload;
    function Find(vConnection: TmncConnection): TmncEngine; overload;
    function IndexOf(vName: string): Integer;
    function CreateConnection(vEngineName: string): TmncConnection; overload;
    function CreateConnection(vORM: TmncORM): TmncConnection; overload;
    function CreateMeta(vConnection: TmncConnection): TmncMeta; overload;
    procedure EnumConnections(Strings: TStrings);
    procedure EnumORMs(Strings: TStrings);
    procedure EnumMatas(Strings: TStrings);
    property Items[Index:Integer]: TmncEngine read GetItems; default;
  end;

function Engines: TmncEngines;

implementation

var
  FmncEngines: TmncEngines = nil;

function Engines: TmncEngines;
begin
  if FmncEngines = nil then
    FmncEngines := TmncEngines.Create;
  Result := FmncEngines;
end;

{ TmncEngines }

function TmncEngines.GetItems(Index: Integer): TmncEngine;
begin
  Result := inherited Items[Index] as TmncEngine;
end;

function TmncEngines.ComposeConnectionString(EngineName, Resource, Host, User, Password, Role: string): string;
begin
  Result := 'Resource="'+Resource+'",Engine="'+EngineName+'",Host="'+Host+'",User="'+User+'",Password="'+Password+'",Role="'+Role+'"';
end;

function TmncEngines.ComposeConnectionString(Connection: TmncConnection): string;
begin
  with Connection do
    Result := ComposeConnectionString(EngineName, Resource, Host, UserName, Password, Role);
end;

procedure TmncEngines.DecomposeConnectionString(Composed: string; out EngineName, Resource, Host, User, Password, Role: string);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    StrToStrings(Composed, Strings, [',']);
    Resource := DequoteStr(Strings.Values['Resource']);
    EngineName := DequoteStr(Strings.Values['Engine']);
    Host := DequoteStr(Strings.Values['Host']);
    User := DequoteStr(Strings.Values['User']);
    Password := DequoteStr(Strings.Values['Password']);
    Role := DequoteStr(Strings.Values['Role']);

  finally
    Strings.Free;
  end;
end;

procedure TmncEngines.DecomposeConnectionString(Composed: string; Connection: TmncConnection);
begin

end;

function TmncEngines.RegisterConnection(vName, vTitle: string; vConnectionClass: TmncConnectionClass): TmncEngine;
begin
  Result := Find(vName);
  if (Result <> nil) and (Result.ConnectionClass <> nil) then
    raise exception.Create(vName + ' is already registered');

  if Result = nil then
  begin
    Result := TmncEngine.Create;
    Result.Name := vName;
    inherited Add(Result);
  end;
  Result.ConnectionClass := vConnectionClass;
  Result.Title := vTitle;
end;

function TmncEngines.RegisterORM(vName: string; vORMClass: TmncORMClass): TmncEngine;
begin
  Result := Find(vName);
  if (Result <> nil) and (Result.ORMClass <> nil) then
    raise exception.Create(vName + ' is already registered');

  if Result = nil then
  begin
    Result := TmncEngine.Create;
    Result.Name := vName;
    inherited Add(Result);
  end;
  Result.ORMClass := vORMClass;
end;

function TmncEngines.RegisterMeta(vName: string; vMetaClass: TmncMetaClass): TmncEngine;
begin
  Result := Find(vName);
  if (Result <> nil) and (Result.MetaClass <> nil) then
    raise exception.Create(vName + ' is already registered');

  if Result = nil then
  begin
    Result := TmncEngine.Create;
    Result.Name := vName;
    inherited Add(Result);
  end;
  Result.MetaClass := vMetaClass;
end;

function TmncEngines.Find(vName: string): TmncEngine;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncEngines.Find(vORM: TmncORM): TmncEngine;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if Items[i].ORMClass = vORM.ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncEngines.Find(vConnection: TmncConnection): TmncEngine;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if Items[i].ConnectionClass = vConnection.ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmncEngines.IndexOf(vName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count -1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TmncEngines.CreateConnection(vEngineName: string): TmncConnection;
var
  P: TmncEngine;
begin
  Result := nil;
  P := Find(vEngineName);
  if P <> nil then
    Result := P.ConnectionClass.Create
  else
    raise EmncException.Create('Engine ' + vEngineName + ' not found');
end;

function TmncEngines.CreateConnection(vORM: TmncORM): TmncConnection;
var
  P: TmncEngine;
begin
  Result := nil;
  P := Find(vORM);
  if P <> nil then
    Result := P.ConnectionClass.Create
  else
    raise EmncException.Create('ORM class not not found');
end;

function TmncEngines.CreateMeta(vConnection: TmncConnection): TmncMeta;
var
  P: TmncEngine;
begin
  Result := nil;
  P := Find(vConnection);
  if (P <> nil)and(P.MetaClass<>nil) then
    Result := P.MetaClass.Create
  else
    raise EmncException.Create('Meta class not not found');
end;

procedure TmncEngines.EnumConnections(Strings: TStrings);
var
  item: TmncEngine;
begin
  for item in Self do
  begin
    if item.ConnectionClass <> nil then
      Strings.AddObject(Item.Title, Item);
  end;
end;

procedure TmncEngines.EnumORMs(Strings: TStrings);
var
  item: TmncEngine;
begin
  for item in Self do
  begin
    if (item.ORMClass <> nil) then
      if Item.Title <> '' then
        Strings.AddObject(Item.Title, Item)
      else
        Strings.AddObject(Item.Name, Item)
  end;
end;

procedure TmncEngines.EnumMatas(Strings: TStrings);
var
  item: TmncEngine;
begin
  for item in Self do
  begin
    if (item.MetaClass <> nil) then
      if Item.Title <> '' then
        Strings.AddObject(Item.Title, Item)
      else
        Strings.AddObject(Item.Name, Item)
  end;
end;

initialization
finalization
  FreeAndNil(FmncEngines);
end.
