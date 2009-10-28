unit posSelectBoxes;

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types,
  posTypes, posUtils, posControls, posEdits;

type
  TposSelectBox = class(TposEdit)
  private
    FItemIndex: Integer;
    FAllowNull: Boolean;
    FItems: TStrings;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetAllowNull(const Value: Boolean);
    function GetItemIndexStored: Boolean;
  protected
    procedure SelectNext;
    procedure Changed; virtual;
    procedure Click; override;
    procedure GetText(var vText: string); override;
    function DoKeyPress(var Key: Char): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property AllowNull: Boolean read FAllowNull write SetAllowNull default True;
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
    Invalidate;
  end;
end;

procedure TposSelectBox.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TposSelectBox.Changed;
begin
end;

procedure TposSelectBox.Click;
begin
  inherited;
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
end;

destructor TposSelectBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TposSelectBox.DoKeyPress(var Key: Char): Boolean;
begin
  Result := Inherited DoKeyPress(Key);
  if (Key = ' ') then
    SelectNext;
end;

function TposSelectBox.GetItemIndexStored: Boolean;
begin
  Result := (ItemIndex > 0) or (AllowNull and (ItemIndex = 0)) or (not AllowNull and (ItemIndex = -1)); 
end;

procedure TposSelectBox.GetText(var vText: string);
begin
  inherited;
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
    vText := ''
  else
    vText := Items[ItemIndex];
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

