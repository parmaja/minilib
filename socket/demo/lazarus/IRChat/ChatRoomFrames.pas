unit ChatRoomFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, SynEdit;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    MsgEdit: TMemo;
    Splitter2: TSplitter;
    TopicEdit: TEdit;
    UserListBox: TListBox;
  private

  public
    RoomName: string;
    IsRoom: Boolean;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TChatRoomFrame }

constructor TChatRoomFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

