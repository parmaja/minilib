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
  Classes, Contnrs, SysUtils;

type
{$ifdef WI}
  ImnDriver = interface;
{$endif}

  EmnDriver = class(Exception);

  TmnDriverOption = (mndLiveDriver);

  TmnDriverOptions = set of TmnDriverOption;

  TmnDriver = class(TObject)
  private
  protected
    FDriverOptions: TmnDriverOptions;
    function GetName: string; virtual;
{$ifdef WI}
    function GetIObject:ImnDriver;
{$endif}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function DriverTitle: string; virtual;
    class function DriverName: string; virtual;
    property DriverOptions:TmnDriverOptions read FDriverOptions;
    property Name:string read GetName;
  end;

  TmnDriverClass = class of TmnDriver;

  TmnRegisteredDriver = class(TObject)
  public
    Category: string;
    Name: string;
    Title: string;
    DriverClass: TmnDriverClass;
  end;

  { TmnRegisteredDrivers }

  TmnRegisteredDrivers = class(TObjectList)
  private
    function GetItems(Index: Integer): TmnRegisteredDriver;
  public
    function Add(Category: string; vDriverClass: TmnDriverClass): TmnRegisteredDriver;
    function Find(vCategory: string; vName: string): TmnRegisteredDriver; overload;
    function Find(vName: string): TmnRegisteredDriver; overload;
    function CreateByName(vCategory: string; vName: string): TmnDriver;
    property Items[Index:Integer]: TmnRegisteredDriver read GetItems; default;
  end;

function mnRegisteredDrivers: TmnRegisteredDrivers;

implementation

var
  FmnRegisteredDrivers: TmnRegisteredDrivers = nil;

function mnRegisteredDrivers: TmnRegisteredDrivers;
begin
  if FmnRegisteredDrivers = nil then
    FmnRegisteredDrivers := TmnRegisteredDrivers.Create;
  Result := FmnRegisteredDrivers;
end;

{ TmnDriver }

constructor TmnDriver.Create;
begin
  inherited Create;
end;

function TmnDriver.GetName: string;
begin
  Result := '';
end;

destructor TmnDriver.Destroy;
begin
  inherited;
end;

class function TmnDriver.DriverTitle: string;
begin
  Result := '';
end;

class function TmnDriver.DriverName: string;
begin
  Result := '';
end;

{ TmnRegisteredDrivers }

function TmnRegisteredDrivers.GetItems(Index: Integer): TmnRegisteredDriver;
begin
  Result := inherited Items[Index] as TmnRegisteredDriver;
end;

function TmnRegisteredDrivers.Add(Category: string; vDriverClass: TmnDriverClass): TmnRegisteredDriver;
begin
  Result := TmnRegisteredDriver.Create;
  Result.Category := Category; 
  Result.Name := vDriverClass.DriverName;
  Result.Title := vDriverClass.DriverTitle;
  Result.DriverClass := vDriverClass;
  Inherited Add(Result);
end;

function TmnRegisteredDrivers.Find(vName: string): TmnRegisteredDriver;
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

function TmnRegisteredDrivers.CreateByName(vCategory: string; vName: string): TmnDriver;
var
  P: TmnRegisteredDriver;
begin
  //Result := nil;
  P := Find(vCategory, vName);
  if P <> nil then
    Result := P.DriverClass.Create
  else
    raise EmnDriver.Create('Driver ' + vName + ' not found');
end;

function TmnRegisteredDrivers.Find(vCategory, vName: string): TmnRegisteredDriver;
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


