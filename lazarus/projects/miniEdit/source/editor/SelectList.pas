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
  TSelectListForm = class(TForm)
    ItemsList: TListView;
    OkBtn: TButton;
    CancelBtn: TButton;
    FilterEdit: TEdit;
    Timer: TTimer;
    procedure ItemsListDblClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
    Elements: TEditorElements;
    Items: array of string;
    procedure ShowItems(vSelect: string);
  end;

function ShowSelectList(vElements: TEditorElements; var vName: string): Boolean;

implementation

uses
  mneResources, mneClasses;

function ShowSelectList(vElements: TEditorElements; var vName: string): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
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

{$R *.lfm}

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
  begin
     aItem := ItemsList.Items.Add;
     aItem.Caption := Title;
     aItem.SubItems.Add(Description);
     aItem.ImageIndex := ImageIndex;
     SetLength(Items, c + 1);
     Items[c] := Name;
  end;
begin
  ItemsList.Items.BeginUpdate;
  with Engine do
  try
    ItemsList.Clear;
    c := 0;
    t := 0;
//    AddItem('Default', 'No type', '', -1);
    for i := 0 to Elements.Count - 1 do
    begin
      AddItem(Elements[i].Name, Elements[i].Title, Elements[i].Description, Elements[i].ImageIndex);
      if SameText(vSelect, Elements[i].Name) then
        t := c;
      inc(c);
    end;
  finally
    ItemsList.Items.EndUpdate;
  end;
  if ItemsList.Items.Count > 0 then
  begin
    ItemsList.Items[t].Selected := True;
  end;
end;

procedure TSelectListForm.FilterEditChange(Sender: TObject);
begin
  Timer.Enabled := False;
  Timer.Enabled := True;
end;

procedure TSelectListForm.OkBtnClick(Sender: TObject);
begin
 ModalResult := mrOK
end;

end.

