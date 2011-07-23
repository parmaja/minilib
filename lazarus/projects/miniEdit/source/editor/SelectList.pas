unit SelectList;
{$mode objfpc}{$H+}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  LMessages, LCLIntf, LCLProc, LCLType, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Match, EditorEngine, Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TSelectListFormStyle = set of (slfIncludeNone, slfUseNameTitle);
  //slfUseNameTitle instead if Title/Description

  TSelectListForm = class(TForm)
    ItemsList: TListView;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure ItemsListDblClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FStyle: TSelectListFormStyle;
  public
    Elements: TEditorElements;
    Items: array of string;
    procedure ShowItems(vSelect: string);
  end;

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string): Boolean;

implementation

uses
  mneResources, mneClasses;

{$R *.lfm}

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      FStyle := Style;
      Elements := vElements;
      ShowItems(vName);
      Result := (ShowModal = mrOK) and (ItemsList.Selected <> nil);
      if Result then
        vName := Items[ItemsList.ItemIndex];
      Free;
    finally
    end;
  end;
end;

procedure TSelectListForm.ItemsListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectListForm.ShowItems(vSelect: string);
var
  s: string;
  i, c, t: Integer;
  aItem: TListItem;
  procedure AddItem(Name, Title, Description: string; ImageIndex: Integer);
  var
    s: string;
  begin
    aItem := ItemsList.Items.Add;

    if slfUseNameTitle in FStyle then
    begin
      aItem.Caption := Name;
      aItem.SubItems.Add(Title);
    end
    else
    begin
      aItem.Caption := Title;
      aItem.SubItems.Add(Description);
    end;

    aItem.ImageIndex := ImageIndex;
    SetLength(Items, c + 1);
    Items[c] := Name;
    if SameText(vSelect, Name) then
      t := c;
    inc(c);
  end;
begin
  ItemsList.Items.BeginUpdate;
  with Engine do
  try
    ItemsList.Clear;
    c := 0;
    t := 0;
    if slfIncludeNone in FStyle then
      AddItem('', 'None', '', -1);
    for i := 0 to Elements.Count - 1 do
      AddItem(Elements[i].Name, Elements[i].Title, Elements[i].Description, Elements[i].ImageIndex);
  finally
    ItemsList.Items.EndUpdate;
  end;
  if ItemsList.Items.Count > 0 then
  begin
    ItemsList.Items[t].Selected := True;
    ItemsList.Items[t].Focused := True;
  end;
end;

procedure TSelectListForm.OkBtnClick(Sender: TObject);
begin
 ModalResult := mrOK
end;

end.

