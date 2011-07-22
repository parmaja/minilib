unit SelectPerspective;
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
  TSelectPerspectiveForm = class(TForm)
    ItemsList: TListView;
    OkBtn: TButton;
    CancelBtn: TButton;
    FilterEdit: TEdit;
    Timer: TTimer;
    procedure ItemsListDblClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    Items: array of string;
  public
    procedure ShowItems(vSelect:string);
  end;

function ShowSelectPerspective(var vName: string): Boolean;

implementation

uses
  mneResources, mneClasses;

{$R *.lfm}

function ShowSelectPerspective(var vName: string): Boolean;
begin
  with TSelectPerspectiveForm.Create(Application) do
  begin
    try
      ShowItems(vName);
      Result := (ShowModal = mrOK) and (ItemsList.Selected <> nil);
      if Result then
        vName := Items[ItemsList.ItemIndex];
      Free;
    finally
    end;
  end;
end;

procedure TSelectPerspectiveForm.ItemsListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectPerspectiveForm.ShowItems(vSelect: string);
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
    for i := 0 to Perspectives.Count - 1 do
    begin
      AddItem(Perspectives[i].Name, Perspectives[i].Title, Perspectives[i].Description, Perspectives[i].ImageIndex);
      if SameText(vSelect, Perspectives[i].Name) then
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

procedure TSelectPerspectiveForm.FilterEditChange(Sender: TObject);
begin
  Timer.Enabled := False;
  Timer.Enabled := True;
end;

procedure TSelectPerspectiveForm.OkBtnClick(Sender: TObject);
begin
 ModalResult := mrOK
end;

end.

