unit IAddons;
{$mode objfpc}{$H+}
{$INTERFACES DEFAULT}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Addon main unit
}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs;

type
  TAddonStatus = (adnsNone, adnsActive);

  { IAddon }

  IAddon = interface(IInterface)
    ['{D87616D9-5B2E-464A-BBB1-5881D1F0FEA7}']
    function GetObject: TObject;
    function Status: TAddonStatus;
  end;

  { TAddonObject }

  TAddonObject = class(TObject, IInterface)
  protected
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  public
    function GetObject: TObject;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    destructor Destroy; override;
  end;

  TAddon = class(TAddonObject, IAddon)
  public
    function Status: TAddonStatus;
  end;

  TAddonClass = class of TAddon;

  { TAddonItem }

  TAddonItem = class(TObject)
  protected
    FAddon: IAddon;
    FAddonObject: TAddon;//kill this object when free
  public
    ID: int64;
    Category: string;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    property Addon: IAddon read FAddon;
  end;

  { TAddons }

  TAddons = class(TObjectList)
  private
    function GetItem(Index: integer): TAddonItem;
  public
    function Add(Category, Name: string; vAddon: IAddon): TAddonItem; overload;
    function Add(Category, Name: string; vAddon: TAddonClass): TAddonItem; overload;
    property Items[Index: integer]: TAddonItem read GetItem; default;
  end;

{
  Most common addons
}

  { IClickAddon }

  IClickAddon = interface(IAddon)
    ['{780D2A49-8F64-45A3-8F2C-96DE2318DB90}']
    procedure Click(Sender: TObject);
  end;

  ICaptionAddon = interface(IAddon)
    ['{3510E934-3C40-46E8-9FFF-494BE33FF97D}']
    function GetCaption: string;
  end;

  ICheckAddon = interface(IAddon)
    ['{439B198B-E879-4779-AC70-5F2F378719DA}']
    function GetChecked: boolean;
    procedure SetChecked(const AValue: boolean);
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  IMenuAddon = interface(ICaptionAddon)
    ['{23FA4EA8-C13E-4110-A6AC-B97489E9E10A}']
  end;

  IToolbarButtonAddon = interface(ICaptionAddon)
    ['{5DD80B1E-9B86-427D-8CDC-5E447FDBAC1E}']
  end;

  IImageIndexAddon = interface(IAddon)
    ['{73B0A164-68FE-4C19-AF4E-99DA3875612F}']
  end;

  IFileAddon = interface(IAddon)
    ['{40A84C67-30B5-4A38-98B5-B56AE3842625}']
  end;

  ISetupAddon = interface(IAddon) //Add this interface to make setup to your addon
    ['{F00BFDF0-7037-4BC9-B268-54E6BEECF98C}']
    procedure ShowSetup;
  end;

  IConfigFormAddon = interface(IAddon)
    ['{4949B2E5-E8F6-41F5-94A0-0E5243352154}']
    procedure SetSender(Sender: TObject);
  end;

  IDebugAddon = interface(IAddon)
    ['{FB4CD381-EE59-4259-8A04-0F80F161710E}']
    procedure Reset;
    procedure StepInto;
    procedure StepOver;
    procedure StepOut;
    procedure Run;
    procedure Resume;
    function GetActive: boolean;
    procedure SetActive(const AValue: Boolean);
    property Active: Boolean read GetActive write SetActive;
  end;

function Addons: TAddons;

implementation

var
  FAddons: TAddons = nil;

function Addons: TAddons;
begin
  if FAddons = nil then
    FAddons := TAddons.Create(True);
  Result := FAddons;
end;

function TAddon.Status: TAddonStatus;
begin
  Result := adnsActive;
end;

{ TAddonItem }

constructor TAddonItem.Create;
begin
  inherited Create;
  FAddonObject := nil;
end;

destructor TAddonItem.Destroy;
begin
  FAddon := nil;//i need that :-(
  FreeAndNil(FAddonObject);
  inherited Destroy;
end;

{ TAddons }

function TAddons.GetItem(Index: integer): TAddonItem;
begin
  Result := inherited Items[Index] as TAddonItem;
end;

function TAddons.Add(Category, Name: string; vAddon: IAddon): TAddonItem;
begin
  Result := TAddonItem.Create;
  Result.Category := Category;
  Result.Name := Name;
  Result.FAddon := vAddon;
  inherited Add(Result);
end;

function TAddons.Add(Category, Name: string; vAddon: TAddonClass): TAddonItem;
var
  AO: TAddon;
begin
  AO := vAddon.Create;
  Result := Add(Category, Name, AO);
  Result.FAddonObject := AO; //to kill the object when free the list
end;

function TAddonObject._AddRef: integer; stdcall;
begin
  Result := 0;
end;

function TAddonObject._Release: integer; stdcall;
begin
  Result := 0;
end;

function TAddonObject.GetObject: TObject;
begin
  Result := Self;
end;

function TAddonObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

destructor TAddonObject.Destroy;
begin
  inherited;
end;

initialization

finalization
  FreeAndNil(FAddons);
end.

