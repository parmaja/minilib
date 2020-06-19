unit ChatRoomFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, SynEdit,
  laz.VirtualTrees;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    SmallImageList: TImageList;
    MsgEdit: TMemo;
    Splitter2: TSplitter;
    TopicEdit: TEdit;
    UserListBox: TListView;
    procedure MsgEditDblClick(Sender: TObject);
  private

  public
    RoomName: string;
    IsRoom: Boolean;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TChatRoomFrame }

procedure TChatRoomFrame.MsgEditDblClick(Sender: TObject);
begin
end;

constructor TChatRoomFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

