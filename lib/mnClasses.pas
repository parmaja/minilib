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
  Classes, SysUtils, StrUtils, DateUtils, Types, Contnrs;

type

  { GItems }

  GItems<_Object_> = class(TObjectList)
  private
    function GetItem(Index: Integer): _Object_;
  public
    property Items[Index: Integer]: _Object_ read GetItem; default;
    procedure Added(Item: _Object_); virtual;
    function Add(Item: _Object_): Integer;
  end;

  GListItems<_Object_> = class(TObjectList)
  private
    function GetItem(Index: Integer): _Object_;
    procedure SetItem(Index: Integer; const Value: _Object_);
  public
    property Items[Index: Integer]: _Object_ read GetItem write SetItem; default;
    function Add(Item: _Object_): Integer;
  end;

  GNamedItems<_Object_> = class(TObjectList)
  private
    function GetItem(Index: Integer): _Object_;
  public
    property Items[Index: Integer]: _Object_ read GetItem; default;
    function Add(Item: _Object_): Integer;
    function  Find(const Name: string): _Object_;
  end;
{
  FreePascal:

    TMyItems = class(specialize GItems<TMyObject>)

  Delphi:

    TMyItems = class(GItems<TMyObject>)
}
implementation

function GItems<_Object_>.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

procedure GItems<_Object_>.Added(Item: _Object_);
begin
end;

function GItems<_Object_>.Add(Item: _Object_): Integer;
begin
  inherited Add(Item);
  Added(Item);
end;

{ GListItems }

function GListItems<_Object_>.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

procedure GListItems<_Object_>.SetItem(Index: Integer; const Value: _Object_);
begin
  Items[Index] := Value;
end;

function GListItems<_Object_>.Add(Item: _Object_): Integer;
begin
  inherited Add(Item);
end;

{ GNamedItems }

function GNamedItems<_Object_>.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

function GNamedItems<_Object_>.Add(Item: _Object_): Integer;
begin
  inherited Add(Item);
end;

function  GNamedItems<_Object_>.Find(const Name: string): _Object_;
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

end.
