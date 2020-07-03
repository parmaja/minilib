unit mncCommons;
{**
 *  This file is part of the "Mini Connections"
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
  SysUtils, Classes, Contnrs, mnClasses;

type
  EmncException = class(Exception)
  end;

  TmncObject = class(TObject)
  end;

  TmncLinks = class;

  { TmncLinksObject }

  TmncLinksObject = class(TmncObject)
  private
    FLinks: TmncLinks;
  public
    constructor Create;
    destructor Destroy; override;
    property Links: TmncLinks read FLinks;
  end;

  { TmncLinkObject }

  TmncLinkObject = class(TmncObject)
  private
  protected
    FLink: TmncLinksObject;
    procedure SetLink(const Value: TmncLinksObject);
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    constructor CreateBy(vLink: TmncLinksObject);
    property Active: Boolean read GetActive write SetActive;
    property Link: TmncLinksObject read FLink write SetLink;
  end;

  { TmncLinks }

  TmncLinks = class(TmnObjectList<TmncLinkObject>)
  private
  protected
  public
    procedure Close;
    procedure Unlink;
  end;

implementation

uses
  mncConnections;

{ TmncLinksObject }

constructor TmncLinksObject.Create;
begin
  inherited Create;
  FLinks := TmncLinks.Create(False);
end;

destructor TmncLinksObject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLinks);//TODO is it safe
end;

{ TmncLinks }

procedure TmncLinks.Close;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Active := False;
  end;
end;

procedure TmncLinks.Unlink;
begin
  while Count > 0 do
    Items[0].Link := nil;
end;

procedure TmncLinkObject.SetLink(const Value: TmncLinksObject);
begin
  if FLink <> Value then
  begin
    if FLink <> nil then
      FLink.Links.Remove(Self);
    FLink := Value;
    if FLink <> nil then
      FLink.Links.Add(Self);
  end;
end;

function TmncLinkObject.GetActive: Boolean;
begin
  Result := False;
end;

procedure TmncLinkObject.SetActive(const AValue: Boolean);
begin
end;

constructor TmncLinkObject.Create;
begin
  inherited Create;
end;

destructor TmncLinkObject.Destroy;
begin
  Link := nil;
  inherited Destroy;
end;

constructor TmncLinkObject.CreateBy(vLink: TmncLinksObject);
begin
  Create;
  Link := vLink;
end;

end.

