unit mnDrivers;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, mnClasses, SysUtils;

type
{$ifdef WI}
  ImnDriver = interface;
{$endif}

  EmnDriver = class(Exception);

  TmnDriverOption = (mndLiveDriver);

  TmnDriverItem = class(TObject)
  public
    Category: string;
    Name: string;
    Title: string;
    DriverClass: TClass;
  end;

  TmnDriversItems = class(TmnObjectList<TmnDriverItem>)
  public
    function Find(const vCategory: string; const vName: string): TmnDriverItem; overload;
    function Find(const vName: string): TmnDriverItem; overload;
    function IndexOf(const vName: string): Integer; overload;
    function FindClass(const vName: string): TClass; overload;
    procedure Enum(vStrings: TStrings); overload;
    procedure EnumCategory(const Category:string; vStrings: TStrings); overload;
    //You need to free the result list
    function EnumCategory(const Category:string): TmnDriversItems; overload;
  end;

  { TmnDriversClasses }

  TmnDriversClasses = class(TmnDriversItems)
  private
  public
    function Add(const Category, Name, Title: string; vDriverClass: TClass): TmnDriverItem;
  end;

function mnDriversClasses: TmnDriversClasses;

implementation

var
  FmnDriversClasses: TmnDriversClasses = nil;

function mnDriversClasses: TmnDriversClasses;
begin
  if FmnDriversClasses = nil then
    FmnDriversClasses := TmnDriversClasses.Create;
  Result := FmnDriversClasses;
end;
{ TmnDriversClasses }

function TmnDriversItems.IndexOf(const vName: string): Integer;
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

function TmnDriversClasses.Add(const Category, Name, Title: string; vDriverClass: TClass): TmnDriverItem;
begin
  Result := TmnDriverItem.Create;
  Result.Category := Category; 
  Result.Name := Name;
  Result.Title := Title;
  Result.DriverClass := vDriverClass;
  Inherited Add(Result);
end;

procedure TmnDriversItems.EnumCategory(const Category: string; vStrings: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if SameText(Category, Items[i].Category) then
    begin
      vStrings.AddObject(Items[i].Title, Items[i]);
    end;
  end;
end;

procedure TmnDriversItems.Enum(vStrings: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    vStrings.AddObject(Items[i].Title, Items[i]);
  end;
end;

function TmnDriversItems.EnumCategory(const Category: string): TmnDriversItems;
var
  i: Integer;
begin
  Result := TmnDriversItems.Create(False);
  for i := 0 to Count -1 do
  begin
    if SameText(Category, Items[i].Category) then
    begin
      Result.Add(Items[i]);
    end;
  end;
end;

function TmnDriversItems.Find(const vName: string): TmnDriverItem;
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

function TmnDriversItems.FindClass(const vName: string): TClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i].DriverClass;
      break;
    end;
  end;
end;

function TmnDriversItems.Find(const vCategory, vName: string): TmnDriverItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if SameText(vCategory, Items[i].Category) and SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

end.


