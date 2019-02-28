unit ChatRoomFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    MsgEdit: TMemo;
    Splitter2: TSplitter;
    TopicEdit: TEdit;
    UserListBox: TListBox;
  private

  public

  end;

implementation

{$R *.lfm}

end.

