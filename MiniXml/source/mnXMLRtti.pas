unit mnXMLRtti;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  mnXML, mnXMLReader, mnXMLWriter;

type
  TmnXMLRttiFiler = class;
  TmnXMLStackFilers = class;

  TmnXMLRttiCustomWriter = class(TmnXMLWriter)
  public
    procedure WriteObject(Instance: TObject); virtual; abstract;
    procedure WriteRoot(Instance: TObject); virtual; abstract;
  end;

  TmnXMLRttiCustomRead = class(TmnXMLReader)
  private
    FStack: TmnXMLStackFilers;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Stack: TmnXMLStackFilers read FStack;
  end;

  TmnXMLRttiFilerNode = class(TInterfacedObject)
  public
    Parent: TmnXMLStackFilers;
    Prior: TmnXMLRttiFiler;
  end;

  TmnXMLRttiFiler = class(TmnXMLRttiFilerNode)
  private
    FPropertyName: string;
    FPropertyClass: TClass;
  protected
    property PropertyName: string read FPropertyName write FPropertyName;
    property PropertyClass: TClass read FPropertyClass write FPropertyClass;
  public
    Depth: Integer;
    Instance: Pointer;
    Owner: TObject;
    Attributes: TStrings;
    constructor Create(Owner: TObject; Instance: Pointer);
    destructor Destroy; override;
    procedure ReadStart; virtual;
    procedure ReadOpen(const Name:string); virtual; abstract;
    procedure ReadValue(const Text: string); virtual; abstract;
    procedure ReadClose(const Name: string); virtual; abstract;
    procedure ReadFinish; virtual;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); virtual; abstract;
  end;

  TmnXMLStackFilers = class(TObject)
  private
    FCount: Integer;
    FCurrent: TmnXMLRttiFiler;
    function GetCurrent: TmnXMLRttiFiler;
  public
    function IsEmpty: Boolean;
    procedure Push(Filer: TmnXMLRttiFiler);
    function Extract: TmnXMLRttiFiler;
    procedure Pop;
    procedure Clear;
    property Count: Integer read FCount;
    property Current: TmnXMLRttiFiler read GetCurrent;
  end;

  TEnumCustomPropertiesProc = procedure(Instance, Filer: TObject) of object; //TmnXMLRttiFiler

  TmnXMLRttiFilerClass = class of TmnXMLRttiFiler;

  TmnXMLRttiRegisterItem = class(TObject)
  public
    PropertyClassName:string;
    PropertyName: string;
    PropertyClass: TClass;
    PropertyInterface:TGUID; 
    IsInterface: Boolean;
    FilerClass: TmnXMLRttiFilerClass;
  end;

  TmnRttiRegister = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnXMLRttiRegisterItem;
    procedure SetItem(Index: Integer; const Value: TmnXMLRttiRegisterItem);
    function FindFilerClass(const PropertyName: string; Instance: TObject): TmnXMLRttiRegisterItem;
    function FindFilerInterface(const PropertyName: string; Instance: Pointer): TmnXMLRttiRegisterItem;
  public
    //register functions
    procedure RegisterClassProperty(PropertyClassName:string; PropertyClass: TClass; PropertyName: string; FilerClass: TmnXMLRttiFilerClass);
    procedure RegisterInterfaceProperty(PropertyName: string; PropertyInterface:TGUID; FilerClass: TmnXMLRttiFilerClass);
    //read/write functions
    function CreateFiler(Owner: TObject; const PropertyName: string; Instance: Pointer; IsInterface:Boolean; DefaultFiler: TmnXMLRttiFilerClass = nil): TmnXMLRttiFiler;
    procedure WriteClassProperties(const PropertyClassName: string; Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
    procedure WriteInterface(const PropertyName: string; Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
    property Items[Index: Integer]: TmnXMLRttiRegisterItem read GetItem write SetItem; default;
  end;

  TmnPermanentRegister = class(TmnRttiRegister)
  end;

function PermanentRegister: TmnPermanentRegister;

implementation

uses
  mnXMLUtils, mnXMLRttiStdClasses;

{ TmnXMLRttiFiler }

constructor TmnXMLRttiFiler.Create(Owner: TObject; Instance: Pointer);
begin
  inherited Create;
  Attributes := TStringList.Create;
  Self.Instance := Instance;
  Self.Owner := Owner;
end;

var
  FPermanentRegister: TmnPermanentRegister = nil;

function PermanentRegister: TmnPermanentRegister;
begin
  if FPermanentRegister = nil then
    FPermanentRegister := TmnPermanentRegister.Create;
  Result := FPermanentRegister;
end;

destructor TmnXMLRttiFiler.Destroy;
begin
  FreeAndNil(Attributes);
  inherited;
end;

procedure TmnXMLRttiFiler.ReadStart;
begin
end;

procedure TmnXMLRttiFiler.ReadFinish;
begin
end;

{ TmnRttiRegister }

procedure TmnRttiRegister.WriteClassProperties(const PropertyClassName: string; Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
var
  i:Integer;
  aFiler: TmnXMLRttiFiler;
begin
  for i := 0 to Count - 1 do
  begin
    if ((PropertyClassName = '') or (Items[i].PropertyClassName = '') or (PropertyClassName = Items[i].PropertyClassName)) and (TObject(Instance).InheritsFrom(Items[i].PropertyClass)) then
    begin
      aFiler := Items[i].FilerClass.Create(Writer, Instance);
      try
        aFiler.Write(Writer, Instance);
      finally
        aFiler.Free;
      end;
    end;
  end;
end;

function TmnRttiRegister.GetItem(Index: Integer): TmnXMLRttiRegisterItem;
begin
  Result := inherited Items[Index] as TmnXMLRttiRegisterItem;
end;

procedure TmnRttiRegister.RegisterClassProperty(PropertyClassName:string; PropertyClass: TClass; PropertyName: string; FilerClass: TmnXMLRttiFilerClass);
var
  aObject: TmnXMLRttiRegisterItem;
begin
  if PropertyName = '' then
    raise EmnXMLException.Create('PropertyName not defined');
  aObject := TmnXMLRttiRegisterItem.Create;
  aObject.PropertyClassName := PropertyClassName; 
  aObject.PropertyName := PropertyName;
  aObject.PropertyClass := PropertyClass;
  aObject.FilerClass := FilerClass;
  Add(aObject);
end;

procedure TmnRttiRegister.SetItem(Index: Integer; const Value: TmnXMLRttiRegisterItem);
begin
  inherited Items[Index] := Value;
end;

function TmnRttiRegister.FindFilerClass(const PropertyName: string; Instance: TObject): TmnXMLRttiRegisterItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
{    if (Result = nil) and (PropertyName = '') and (Instance.InheritsFrom(Items[i].PropertyClass)) then
    begin
      Result := Items[i]; //first general one
    end; //take care there is no else here, named Property is more power than non named}
    if ((PropertyName = Items[i].PropertyName)) and (Instance.InheritsFrom(Items[i].PropertyClass)) then
    begin
      Result := Items[i]; //first sepical one
      break;
    end;
  end;
end;

function TmnRttiRegister.FindFilerInterface(const PropertyName: string;
  Instance: Pointer): TmnXMLRttiRegisterItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ((PropertyName = Items[i].PropertyName)) and (Supports(IInterface(Instance), (Items[i].PropertyInterface))) then
    begin
      Result := Items[i]; //first sepical one
      break;
    end;
  end;
end;

function TmnRttiRegister.CreateFiler(Owner: TObject; const PropertyName: string;
  Instance: Pointer; IsInterface:Boolean; DefaultFiler: TmnXMLRttiFilerClass): TmnXMLRttiFiler;
var
  aItem: TmnXMLRttiRegisterItem;
begin
  if IsInterface then
    aItem := FindFilerInterface(PropertyName, Instance)
  else
    aItem := FindFilerClass(PropertyName, Instance);
  if aItem <> nil then
    Result := aItem.FilerClass.Create(Owner, Instance)
  else if DefaultFiler <> nil then
    Result := DefaultFiler.Create(Owner, Instance)
  else
    Result := nil;
end;

{ TmnXMLRttiFilers }

procedure TmnXMLStackFilers.Pop;
begin
  Extract.Free;
end;

function TmnXMLStackFilers.GetCurrent: TmnXMLRttiFiler;
begin
  Result := FCurrent;
end;

function TmnXMLStackFilers.IsEmpty: Boolean;
begin
  Result := FCurrent = nil;
end;

function TmnXMLStackFilers.Extract: TmnXMLRttiFiler;
begin
  if FCurrent = nil then
    raise EmnXMLException.Create(sStackIsEmpty);
  Result := FCurrent;
  Result.ReadFinish;
  FCurrent := Result.Prior;
  Dec(FCount);
end;

procedure TmnXMLStackFilers.Push(Filer: TmnXMLRttiFiler);
begin
  Filer.Prior := FCurrent;
  Filer.Parent := Self;
  FCurrent := Filer;
  Inc(FCount);
  Filer.ReadStart;
end;

procedure TmnRttiRegister.RegisterInterfaceProperty(PropertyName: string; PropertyInterface:TGUID; FilerClass: TmnXMLRttiFilerClass);
var
  aObject: TmnXMLRttiRegisterItem;
begin
  if PropertyName = '' then
    raise EmnXMLException.Create('PropertyName not defined');
  aObject := TmnXMLRttiRegisterItem.Create;
  aObject.PropertyName := PropertyName;
  aObject.PropertyInterface := PropertyInterface;
  aObject.IsInterface := True;
  aObject.FilerClass := FilerClass;
  Add(aObject);
end;

procedure TmnRttiRegister.WriteInterface(const PropertyName: string; Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
var
  i: Integer;
  aFiler: TmnXMLRttiFiler;
begin
  for i := 0 to Count - 1 do
  begin
    if (Items[i].IsInterface) and (Items[i].PropertyName = PropertyName) then
    begin
      aFiler := Items[i].FilerClass.Create(Writer, Instance);
      try
        aFiler.Write(Writer, Instance);
      finally
        aFiler.Free;
      end;
    end;
  end;
end;

{ TmnXMLRttiCustomRead }

constructor TmnXMLRttiCustomRead.Create;
begin
  inherited;
  FStack := TmnXMLStackFilers.Create;
end;

destructor TmnXMLRttiCustomRead.Destroy;
begin
  FStack.Free;
  inherited;
end;

{ TmnXMLRttiFilerNode }

procedure TmnXMLStackFilers.Clear;
var
  aObject: TmnXMLRttiFiler;
begin
  aObject := Current;
  while aObject <> nil do
  begin
    Pop;
    aObject := Current;
  end;
end;

initialization
finalization
  FreeAndNil(FPermanentRegister);
end.

