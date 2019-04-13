unit ChatRoomFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, HtmlView, SynEdit,
  IpHtml;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    HtmlViewer1: THtmlViewer;
    MsgEdit: TMemo;
    Splitter2: TSplitter;
    TopicEdit: TEdit;
    UserListBox: TListBox;
    procedure MsgEditDblClick(Sender: TObject);
  private

  public
    RoomName: string;
    IsRoom: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure AddMsg(s: string);
  end;

implementation

{$R *.lfm}

{ TChatRoomFrame }

procedure TChatRoomFrame.MsgEditDblClick(Sender: TObject);
begin
  AddMsg('')
end;

constructor TChatRoomFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TChatRoomFrame.AddMsg(s: string);
begin
  HtmlViewer1.LoadFromFile(Application.Location + 'chat.html');
end;

end.

