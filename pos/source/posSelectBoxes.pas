unit posSelectBoxes;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Types,
 posTypes, posUtils, posControls, posEdits;

type

  { TposSelectBox }

  TposSelectBox = class(TposEdit)
  private
    FAllowEdit: Boolean;
    FItemIndex: Integer;
    FAllowNull: Boolean;
    FItems: TStrings;
    function GetItemText: string;
    procedure SetAllowEdit(const AValue: Boolean);
    procedure SetItemText(const AValue: string);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetAllowNull(const Value: Boolean);
    function GetItemIndexStored: Boolean;
  protected
    procedure ItemSelected; virtual;
    procedure SelectNext;
    procedure Changed; virtual;
    procedure Click; override;
    procedure GetText(out vText: string); override;
    function DoKeyPress(var Key: Char): Boolean; override;
    procedure DoButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function GetInputs: TposFrameInputs; override;
    property ItemText: string read GetItemText write SetItemText;
  published
    property AllowNull: Boolean read FAllowNull write SetAllowNull default True;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default True;
    property ItemIndex: Integer read FItemIndex write SetItemIndex stored GetItemIndexStored;
    property Items: TStrings read FItems write SetItems; 
  end;

implementation

{ TposSelectBox }

procedure TposSelectBox.SelectNext;
var
  i: Integer;
begin
  i := ItemIndex + 1;
  if i >= Items.Count then
  begin
    if AllowNull then
      i := -1
    else
      i := 0;
  end;
  if i <> ItemIndex then
  begin
    ItemIndex := i;
    Changed;
  end;
  ItemSelected;
end;

procedure TposSelectBox.SetAllowNull(const Value: Boolean);
begin
  if FAllowNull <> Value then
  begin
    if ItemIndex <= 0 then
    begin
      if Value then
        ItemIndex := -1
      else if Items.Count > 0 then
        ItemIndex := 0;
    end;
    FAllowNull := Value;
    Invalidate;
  end;
end;

procedure TposSelectBox.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    if not (csLoading in ComponentState) then
    begin
      if Value >= Items.Count then
        raise Exception.Create('ItemIndex out of bounds');
    end;
    FItemIndex := Value;
    ItemSelected;
    Invalidate;
  end;
end;

function TposSelectBox.GetItemText: string;
begin
  Result := Text;
end;

procedure TposSelectBox.SetAllowEdit(const AValue: Boolean);
begin
  if FAllowEdit =AValue then exit;
  FAllowEdit :=AValue;
end;

procedure TposSelectBox.SetItems(const Value: TStrings);
begin
  BeginUpdate;
  try
    FItems.Assign(Value);
    ItemSelected;
  finally
    EndUpdate;
  end;
end;

procedure TposSelectBox.Changed;
begin
  inherited;
end;

function TposSelectBox.GetInputs: TposFrameInputs;
begin
  Result :=inherited GetInputs;
  if FAllowEdit then
    Result := Result + [fiText]
  else
    Result := Result - [fiText];
end;

procedure TposSelectBox.Click;
begin
  inherited;
  if not ButtonShow then
    SelectNext;
end;

constructor TposSelectBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks] + [csOpaque];
  Style := Style + [fsOpaque];
  Width := 60;
  Height := 22;
  TabStop := True;
  FItems := TStringList.Create;
  FItemIndex := -1;
  FAllowNull := True;
  FAllowEdit := True;
end;

destructor TposSelectBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TposSelectBox.DoKeyPress(var Key: Char): Boolean;
begin
  Result := Inherited DoKeyPress(Key);
  if not ButtonShow then
    if (Key = ' ') then
      SelectNext;
end;

procedure TposSelectBox.DoButtonClick;
begin
  inherited;
  SelectNext;
end;

function TposSelectBox.GetItemIndexStored: Boolean;
begin
  Result := (ItemIndex > 0) or (AllowNull and (ItemIndex = 0)) or (not AllowNull and (ItemIndex = -1)); 
end;

procedure TposSelectBox.SetItemText(const AValue: string);
var
  i: Integer;
begin
  i := Items.IndexOf(AValue);
  ItemIndex := i;
  if i >=0 then
    ItemSelected
  else
    Text := AValue;
end;

procedure TposSelectBox.ItemSelected;
begin
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
    Text := ''
  else
    Text := Items[ItemIndex];
end;

procedure TposSelectBox.GetText(out vText: string);
begin
  inherited;
  {if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
    vText := ''
  else
    vText := Items[ItemIndex];}
end;

procedure TposSelectBox.Loaded;
begin
  inherited;
  if not AllowNull and (ItemIndex < 0) then
    ItemIndex := 0;
end;

initialization
finalization
end.

