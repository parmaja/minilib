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
  mncCommons, mncConnections;

type

  TmncConnectionClass = class of TmncConnection;

  { TmncEngine }

  TmncEngine = class(TObject)
  public
    Name: string;
    Title: string;
    ConnectionClass: TmncConnectionClass;
  end;

{ TmncEngines }

  TmncEngines = class(TObjectList)
  private
    function GetItems(Index: Integer): TmncEngine;
  public
    function Add(vConnectionClass: TmncConnectionClass): TmncEngine;
    function Find(vName: string): TmncEngine;
    function IndexOf(vName: string): Integer;
    function CreateByName(vName: string): TmncConnection;
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

function TmncEngines.Add(vConnectionClass: TmncConnectionClass): TmncEngine;
begin
  Result := TmncEngine.Create;
  Result.Name := vConnectionClass.Model.Name;
  Result.Title := vConnectionClass.Model.Title;
  Result.ConnectionClass := vConnectionClass;
  inherited Add(Result);
  //mnDriversClasses.Add('Engines', 'SerialEngines', 'Serial Engine', vConnectionClass);//TODO
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

function TmncEngines.CreateByName(vName: string): TmncConnection;
var
  P: TmncEngine;
begin
  Result := nil;
  P := Find(vName);
  if P <> nil then
    Result := P.ConnectionClass.Create
  else
    raise EmncException.Create('Engine ' + vName + ' not found');
end;

initialization

finalization
  FreeAndNil(FmncEngines);

end.
