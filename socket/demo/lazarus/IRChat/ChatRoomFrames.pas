unit ChatRoomFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus,
  SynEdit, laz.VirtualTrees;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    MenuItem1: TMenuItem;
    WhoIsMnu: TMenuItem;
    OpMnu: TMenuItem;
    MenuItem2: TMenuItem;
    MsgEdit: TMemo;
    UsersPopupMenu: TPopupMenu;
    Splitter2: TSplitter;
    TopicEdit: TEdit;
    UserListBox: TListView;
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgEditDblClick(Sender: TObject);
    procedure OpMnuClick(Sender: TObject);
    procedure WhoIsMnuClick(Sender: TObject);
  private
    function GetCurrentUser: string;
  public
    RoomName: string;
    IsRoom: Boolean;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  MainForm;

{$R *.lfm}

{ TChatRoomFrame }

procedure TChatRoomFrame.MsgEditDblClick(Sender: TObject);
begin
end;

procedure TChatRoomFrame.MenuItem1Click(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.OpUser(RoomName, aUser);
end;

procedure TChatRoomFrame.OpMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.OpUser(RoomName, aUser);
end;

procedure TChatRoomFrame.WhoIsMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.WhoIs(aUser);
end;

function TChatRoomFrame.GetCurrentUser: string;
begin
  Result := '';
  if UserListBox.Items.Count >0 then
  begin
    Result := UserListBox.Selected.Caption;
  end;
end;

constructor TChatRoomFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

