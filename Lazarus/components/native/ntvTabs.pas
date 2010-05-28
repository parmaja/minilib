unit ntvTabs;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Alhamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TTabStyle = (tbDefault, tbNormal, tbSheets);

  { TntvTabItem }

  TntvTabItem = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FImageIndex: Integer;
    FName: string;
    FEnabled: Boolean;
    FVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetWidth(const Value: Integer);
    procedure SetImageIndex(const AValue: Integer);
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Update; virtual;
  public
    constructor Create(vCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Width: Integer read FWidth write SetWidth;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Name: string read GetName write SetName;
    property Enabled: boolean read FEnabled write FEnabled;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TntvTabItemClass = class of TntvTabItem;

  { TntvPages }

  { TntvTabs }

  TntvTabs = class(TCollection)
  private
    FImages: TImageList;
    FItemIndex: Integer;
    FTabStyle: TTabStyle;
    FTopIndex: Integer;
  protected
    function GetItem(Index: Integer): TntvTabItem;
    procedure SetItem(Index: Integer; Value: TntvTabItem);
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateItems(vCanvas: TCanvas);
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TntvTabItem;

    function GetTabRect(UseRightToLeftAlignment: Boolean; TopIndex, Index: Integer; var vRect: TRect): Boolean; overload;
    function GetTabRect(UseRightToLeftAlignment: Boolean; Index: Integer; var vRect: TRect): Boolean; overload; virtual;
    function GetTabOffset(UseRightToLeftAlignment: Boolean; Index: Integer): Integer;

    property Items[Index: Integer]: TntvTabItem read GetItem write SetItem stored False; default;
    property Images: TImageList read FImages write FImages;
    property TabStyle: TTabStyle read FTabStyle write FTabStyle;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property TopIndex: Integer read FTopIndex write FTopIndex;
  published
  end;

implementation

{ TntvTabItem }

procedure TntvTabItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Update;
  end;
end;

procedure TntvTabItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

procedure TntvTabItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex = AValue then exit;
  FImageIndex :=AValue;
end;

function TntvTabItem.GetName: string;
begin
  Result := FName;
end;

procedure TntvTabItem.SetName(const Value: string);
begin
  FName := Value;
  DisplayName:=Value;
end;

procedure TntvTabItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

function TntvTabItem.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TntvTabItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
end;

procedure TntvTabItem.Update;
begin
end;

constructor TntvTabItem.Create(vCollection: TCollection);
begin
  inherited Create(vCollection);
  FVisible := True;
  FImageIndex := -1;
end;

destructor TntvTabItem.Destroy;
begin
  inherited Destroy;
end;

procedure TntvTabItem.Assign(Source: TPersistent);
begin
  if Source is TntvTabItem then
  begin
    FCaption := TntvTabItem(Source).FCaption;
    FName := TntvTabItem(Source).FName;
    FImageIndex := TntvTabItem(Source).FImageIndex;
    FEnabled := TntvTabItem(Source).FEnabled;
    FVisible := TntvTabItem(Source).FVisible;
    Update;
  end
  else
    inherited;
end;

{ TntvTabs }

procedure TntvTabs.SetItem(Index: Integer; Value: TntvTabItem);
begin
  inherited SetItem(Index, Value);
end;

function TntvTabs.GetItem(Index: Integer): TntvTabItem;
begin
  Result := (inherited GetItem(Index) as TntvTabItem);
end;

procedure TntvTabs.Update(Item: TCollectionItem);
begin
  inherited;
end;

procedure TntvTabs.UpdateItems(vCanvas: TCanvas);
begin

end;

constructor TntvTabs.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(TntvTabItem);
end;

destructor TntvTabs.Destroy;
begin
  inherited;
end;

function TntvTabs.Add: TntvTabItem;
begin

end;

function TntvTabs.GetTabRect(UseRightToLeftAlignment: Boolean; TopIndex, Index: Integer; var vRect: TRect): Boolean;
var
  R: Trect;
  i, x: Integer;
  aList: TntvTabList;
begin
  Result := Rect(0, 0, 0, 0);
  TntvTabList := CreateTabList;
  if (Index < aList.Count) and (Index > -1) then

  begin
    x := 0;
    for i := TopIndex to Index do
      x := x + FPageList[i].Width;
    R := Bounds(0, 0, 0, HeaderHeight);
    if UseRightToLeftAlignment then
    begin
      x := Width - x;
      r.Left := x;
      r.Right := x + FPageList[Index].Width
    end
    else
    begin
      r.Right := x;
      r.Left := x - FPageList[Index].Width;
    end;
    Result := R;
  end;
end;

function TntvTabs.GetTabRect(UseRightToLeftAlignment: Boolean; Index: Integer; var vRect: TRect): Boolean;
begin
  Result := GetTabRect(UseRightToLeftAlignment, TopIndex, Index);
end;

function TntvTabs.GetTabOffset(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Index - 1 downto TopIndex do
    if Items[i].Visible then
      Result := Result + Items[index].Width;
end;

end.

