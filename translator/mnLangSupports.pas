unit mnLangSupports;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$M+}

interface

uses
  Windows, Messages, 
  SysUtils, Variants, Classes,
  Dialogs, ComObj, Contnrs;

type
  TLangSupport = class;
  TLangSupportClass = class of TLangSupport;

  TLangSupport = class(TObject)
  private
  public
    FComponent: TComponent;
    constructor Create(AComponent: TComponent); dynamic;
    destructor Destroy; override;
    procedure WriteLanguage; virtual;
    procedure ReadLanguage; virtual;
    class function Support(AComponent: TComponent): boolean; dynamic;
  published
  end;

  TLangSupportsList = class(TClassList)
  private
    function GetItems(Index: Integer): TLangSupportClass;
    procedure SetItems(Index: Integer; const Value: TLangSupportClass);
  public
    property Items[Index: Integer]: TLangSupportClass read GetItems write SetItems; default;
  end;

  TLangSupports = class(TComponent)
  private
    FItems: TLangSupportsList;
  protected
    procedure RegisterSupport(DefaultSupport: TLangSupportClass);
    function CreateSupport(AComponent: TComponent): TLangSupport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadLanguage(AComponent: TComponent);
    procedure WriteLanguage(AComponent: TComponent);
    property Items: TLangSupportsList read FItems;
  published
  end;

procedure RegisterLangSupports(DefaultSupports: array of TLangSupportClass);
function LanguageSupports: TLangSupports;

implementation

var
  FLangSupports: TLangSupports = nil;

{ TLangSupportsList }

function TLangSupportsList.GetItems(Index: Integer): TLangSupportClass;
begin
  Result := TLangSupportClass(inherited Items[Index]);
end;

procedure TLangSupportsList.SetItems(Index: Integer;
  const Value: TLangSupportClass);
begin
  inherited Items[Index] := Value;
end;

function LanguageSupports: TLangSupports;
begin
  if FLangSupports = nil then
    FLangSupports := TLangSupports.Create(nil);
  Result := FLangSupports;
end;

constructor TLangSupports.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TLangSupportsList.Create;
end;

destructor TLangSupports.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TLangSupports.RegisterSupport(DefaultSupport: TLangSupportClass);
begin
  FItems.Add(DefaultSupport);
end;

procedure TLangSupports.ReadLanguage(AComponent: TComponent);
var
  aSupport: TLangSupport;
  aClass: TLangSupportClass;
  i: Integer;
begin
  if (AComponent.Name <> '') and (AComponent.Name[1] <> '_') then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      aClass := Items[i];
      if aClass.Support(AComponent) then
      begin
        aSupport := aClass.Create(AComponent);
        try
          aSupport.ReadLanguage;
        finally
          aSupport.Free;
        end;
        break;
      end;
    end;
  end;
end;

function TLangSupports.CreateSupport(AComponent: TComponent): TLangSupport;
var
  i: Integer;
begin
  Result := nil;
  if (AComponent.Name <> '') and (AComponent.Name[1] <> '_') then
    for i := 0 to Items.Count - 1 do
    begin
      if (Items[i].Support(AComponent)) then
      begin
        Result := Items[i].Create(AComponent);
      end;
    end;
end;

procedure TLangSupports.WriteLanguage(AComponent: TComponent);
var
  aSupport: TLangSupport;
  aClass: TLangSupportClass;
  i: Integer;
begin
  if (AComponent.Name <> '') and (AComponent.Name[1] <> '_') then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      aClass := Items[i];
      if aClass.Support(AComponent) then
      begin
        aSupport := aClass.Create(AComponent);
        try
          aSupport.WriteLanguage;
        finally
          aSupport.Free;
        end;
        break;
      end;
    end;
  end;
end;

{ TLangSupport }

constructor TLangSupport.Create(AComponent: TComponent);
begin
  inherited Create;
  FComponent := AComponent;
end;

destructor TLangSupport.Destroy;
begin
  inherited;
end;

procedure TLangSupport.ReadLanguage;
begin

end;

class function TLangSupport.Support(AComponent: TComponent): boolean;
begin
  Result := False;
end;

procedure RegisterLangSupports(DefaultSupports: array of TLangSupportClass);
var
  i: integer;
begin
  for i := 0 to Length(DefaultSupports) - 1 do
    LanguageSupports.RegisterSupport(DefaultSupports[i]);
end;

procedure TLangSupport.WriteLanguage;
begin

end;

initialization
finalization
  FreeAndNil(FLangSupports);
end.

