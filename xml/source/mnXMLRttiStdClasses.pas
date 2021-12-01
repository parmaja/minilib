unit mnXMLRttiStdClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo,
  mnXML, mnXMLRttiProfile, mnXMLRttiReader, mnXMLRtti;

type
  TmnXMLRttiStrings = class(TmnXMLRttiFiler)
  private
    FReadState: Boolean;
  public
    procedure ReadStart; override;
    procedure ReadOpen(const Name: string); override;
    procedure ReadValue(const Text: string); override;
    procedure ReadClose(const Name: string); override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiCollection = class(TmnXMLRttiObjectFiler)
  protected
    function FindClass(const ClassName: string): TClass; override;
    function CreateObject(Instance: TObject; const ClassName, Name: string): TObject; override;
  public
    procedure ReadStart; override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

  TmnXMLRttiProfileItems= class(TmnXMLRttiObjectFiler)
  protected
    function CreateObject(Instance: TObject; const ClassName, Name: string): TObject; override;
  public
    procedure ReadStart; override;
    procedure Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer); override;
  end;

implementation

{ TmnXMLRttiStrings }

procedure TmnXMLRttiStrings.ReadClose(const Name: string);
begin
  if Name <> 'Line' then
    raise EmnXMLParserException.Create('/Line tag expected but found ' + Name);
  FReadState := False;
end;

procedure TmnXMLRttiStrings.ReadOpen(const Name: string);
begin
  if Name <> 'Line' then
    raise EmnXMLParserException.Create('Line tag expected but found ' + Name);
  FReadState := True;
end;

procedure TmnXMLRttiStrings.ReadStart;
begin
  inherited;
  (TObject(Instance) as TStrings).Clear;
end;

procedure TmnXMLRttiStrings.ReadValue(const Text: string);
begin
  if FReadState then
    (TObject(Instance) as TStrings).Add(Text)
  else
    raise EmnXMLException.Create('Text not allowed outside of Line element');
end;

procedure TmnXMLRttiStrings.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
var
  i: Integer;
begin
  if not (TObject(Instance) is TStrings) then
    raise EmnXMLException.Create('not support this class');
  if TStrings(Instance).Count > 0 then
    with Writer do
    begin
      OpenTag('Strings');
      for i := 0 to TStrings(Instance).Count - 1 do
        AddTextTag('Line', TStrings(Instance)[i]);
      CloseTag('Strings');
    end;
end;

{ TmnXMLRttiCollection }

function TmnXMLRttiCollection.CreateObject(Instance: TObject; const ClassName, Name: string): TObject;
begin
  if SameText((Instance as TCollection).ItemClass.ClassName, ClassName) then
    Result := (Instance as TCollection).ItemClass.Create(Instance as TCollection)
  else
    Result := SafeFindClass(ClassName).Create;
end;

function TmnXMLRttiCollection.FindClass(const ClassName: string): TClass;
begin
  if SameText((TObject(Instance) as TCollection).ItemClass.ClassName, ClassName) then
    Result := (TObject(Instance) as TCollection).ItemClass
  else
    Result := inherited FindClass(ClassName);
end;

procedure TmnXMLRttiCollection.ReadStart;
begin
  inherited;
  if not (TObject(Instance) is TCollection) then
    raise EmnXMLException.Create('not support this class');
  (TObject(Instance) as TCollection).Clear;
end;

procedure TmnXMLRttiCollection.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
var
  i: Integer;
begin
  if not (TObject(Instance) is TCollection) then
    raise EmnXMLException.Create('not support this class');
  if (TObject(Instance) as TCollection).Count > 0 then
    with Writer do
    begin
      OpenTag('Items');
      for I := 0 to (TObject(Instance) as TCollection).Count - 1 do
        Writer.WriteObject((TObject(Instance) as TCollection).Items[I]);
      CloseTag('Items');
    end;
end;

{ TmnXMLRttiProfileItems }

function TmnXMLRttiProfileItems.CreateObject(Instance: TObject; const ClassName, Name: string): TObject;
var
  AClass: TClass;
begin
  AClass := FindClass(ClassName);
  if (AClass = nil) or AClass.InheritsFrom(TmnXMLItem) then // if we not found the class we will pass AClass as nil and make XMLItems create is childs
    Result := TmnXMLItems(Instance).CreateItem(TmnXMLItemClass(AClass))
  else
    raise EmnXMLException.Create('Not support this class '+ ClassName);
end;

procedure TmnXMLRttiProfileItems.ReadStart;
begin
  if not (TObject(Instance) is TmnXMLItems) then
    raise EmnXMLException.Create('Not support this class');
  (TObject(Instance) as TmnXMLItems).Clear;
end;

procedure TmnXMLRttiProfileItems.Write(Writer: TmnXMLRttiCustomWriter; Instance: Pointer);
var
  I:Integer;
begin
  if not (TObject(Instance) is TmnXMLItems) then
    raise EmnXMLException.Create('Not support this class');
  if TmnXMLItems(Instance).Count > 0 then
    with Writer do
    begin
      OpenTag('Objects');
      for I := 0 to TmnXMLItems(Instance).Count - 1 do
      begin
        Writer.WriteObject(TmnXMLItems(Instance).Items[I]);
      end;
      CloseTag('Objects');
    end;
end;

initialization
  RttiFilers.RegisterClassProperty('', TStrings, 'Strings', TmnXMLRttiStrings);
  RttiFilers.RegisterClassProperty('', TCollection, 'Items', TmnXMLRttiCollection);
  RttiFilers.RegisterClassProperty('', TmnXMLItems, 'Objects', TmnXMLRttiProfileItems);
finalization
end.

