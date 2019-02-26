unit mnClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Types,
  {$ifdef FPC}
  Contnrs
  {$else}
  System.Generics.Collections
  {$endif};

type
  { TmnObjectList }

  {$ifdef FPC}
  TmnObjectList<_Object_> = class(TObjectList)
  {$else}
  TmnObjectList<_Object_: class> = class(TObjectList<_Object_>)
  {$endif}
  private
    function GetItem(Index: Integer): _Object_;
    procedure SetItem(Index: Integer; AObject: _Object_);
  protected

    function _AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};

    {$H-}procedure Added(Item: _Object_); virtual;{$H+}
    procedure Created; virtual;
  public
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj):HResult; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    procedure AfterConstruction; override;
    function Add(Item: _Object_): Integer;
    function Extract(Item: _Object_): _Object_;

    property Items[Index: Integer]: _Object_ read GetItem write SetItem; default;
    function Last: _Object_;
  end;

  { TmnNamedObjectList }

  TmnNamedObject = class(TObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  {$ifdef FPC}
  TmnNamedObjectList<_Object_> = class(TmnObjectList<_Object_>)
  {$else}
  TmnNamedObjectList<_Object_: TmnNamedObject> = class(TmnObjectList<_Object_>)
  {$endif}
  private
  public
    function Find(const Name: string): _Object_;
    function IndexOfName(vName: string): Integer;
  end;

implementation

function TmnObjectList<_Object_>.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

procedure TmnObjectList<_Object_>.SetItem(Index: Integer; AObject: _Object_);
begin
  inherited Items[Index] := AObject;
end;

function TmnObjectList<_Object_>.Last: _Object_;
begin
  Result := _Object_(inherited Last);
end;

function TmnObjectList<_Object_>._AddRef: Integer;
begin
  Result := 0;
end;

function TmnObjectList<_Object_>._Release: Integer;
begin
  Result := 0;
end;

function TmnObjectList<_Object_>.QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj):HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TmnObjectList<_Object_>.Added(Item: _Object_);
begin
end;

function TmnObjectList<_Object_>.Add(Item: _Object_): Integer;
begin
  Result := inherited Add(Item);
  Added(Item);
end;

function TmnObjectList<_Object_>.Extract(Item: _Object_): _Object_;
begin
  Result := _Object_(inherited Extract(Item));
end;

procedure TmnObjectList<_Object_>.Created;
begin
end;

procedure TmnObjectList<_Object_>.AfterConstruction;
begin
  inherited;
  Created;
end;

{ TmnNamedObjectList }

function  TmnNamedObjectList<_Object_>.Find(const Name: string): _Object_;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnNamedObjectList<_Object_>.IndexOfName(vName: string): Integer;
var
  i: integer;
begin
  Result := -1;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := i;
        break;
      end;
    end;
end;

end.
