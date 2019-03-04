unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, IniFiles,
  StdCtrls, ExtCtrls, ComCtrls, Menus, LCLType,
  ChatRoomFrames, mnIRCClients;

type

  { TMyIRCClient }

  TMyIRCClient = class(TmnIRCClient)
  public
    procedure DoChanged(vStates: TIRCStates; vChannel: string; vNick: string); override;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String); override;
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    ConnectBtn: TButton;
    JoinBtn: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MsgPageControl: TPageControl;
    NicknameBtn: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    PasswordEdit: TEdit;
    LogEdit: TMemo;
    MenuItem1: TMenuItem;
    LogPopupMenu: TPopupMenu;
    Panel2: TPanel;
    RoomEdit: TEdit;
    SendBtn: TButton;
    SendEdit: TEdit;
    UserEdit: TEdit;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure JoinBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgPageControlChange(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    IRC: TMyIRCClient;
    Recents: TStringList;
    RecentsIndex: Integer;
    procedure RecentUp;
    procedure RecentDown;
    procedure AddRecent(S: string);
    function CurrentRoom: string;
    procedure ConnectNow;
    procedure SendNow;
    function NeedRoom(vRoomName: string; ActiveIt: Boolean = false): TChatRoomFrame;
    procedure SetNick(ANick: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
    procedure ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

{ TMyIRCClient }

procedure TMyIRCClient.DoChanged(vStates: TIRCStates; vChannel: string; vNick: string);
begin
  inherited;
  if scChannelNames in vStates then
    MainFrm.ReceiveNames(vChannel, Session.Channels.Find(vChannel));
  if scUserInfo in vStates then
  begin
    MainFrm.SetNick(Session.Nick);
  end;
  if scProgress in vStates then
    case Progress of
      prgDisconnected:
      begin
        MainFrm.ConnectBtn.Caption := 'Connect';
        while MainFrm.MsgPageControl.PageCount > 0 do
          MainFrm.MsgPageControl.Page[0].Free;
      end;
      prgConnected: MainFrm.ConnectBtn.Caption := 'Disconnect...';
      prgReady: MainFrm.ConnectBtn.Caption := 'Disconnect';
    end;
end;

procedure TMyIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
begin
  MainFrm.DoReceive(vMsgType, vChannel, vUser, vMsg);
end;

{ TMainFrm }

procedure TMainFrm.ConnectBtnClick(Sender: TObject);
begin
  if IRC.Active then
    IRC.Disconnect
  else
  begin
    ConnectNow;
    SendEdit.SetFocus;
  end;
end;

procedure TMainFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) then
  begin
    if Shift = [ssCtrl] then
      MsgPageControl.SelectNextPage(True)
    else if Shift = [ssCtrl, ssShift] then
      MsgPageControl.SelectNextPage(False);
  end;
end;

procedure TMainFrm.Button1Click(Sender: TObject);
begin
end;

procedure TMainFrm.ConnectNow;
begin
  IRC.Host := HostEdit.Text;
  IRC.Port := '6667';
  IRC.Auth := authIDENTIFY;
  IRC.Nick := UserEdit.Text;
  IRC.RealName := UserEdit.Text;
  IRC.Username := UserEdit.Text;
  IRC.Password := PasswordEdit.Text;
  IRC.Connect;
  IRC.Join(RoomEdit.Text);
end;

procedure TMainFrm.HostEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ConnectNow;
  end;
end;

procedure TMainFrm.JoinBtnClick(Sender: TObject);
begin
  IRC.Join(RoomEdit.Text);
end;

procedure TMainFrm.MenuItem1Click(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainFrm.MsgPageControlChange(Sender: TObject);
begin

end;

procedure TMainFrm.SendBtnClick(Sender: TObject);
begin
  SendNow;
end;

procedure TMainFrm.SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_UP: RecentUp;
      VK_DOWN: RecentDown;
    end;
  end;
end;

procedure TMainFrm.SendNow;
begin
  IRC.ChannelSend(CurrentRoom, SendEdit.Text);
  AddRecent(SendEdit.Text);
  SendEdit.Text := '';
end;

function TMainFrm.NeedRoom(vRoomName: string; ActiveIt: Boolean): TChatRoomFrame;
var
  i, Index: Integer;
  TabSheet: TTabSheet;
  ARoomName: string;
  AIsRoom: Boolean;
begin
  if vRoomName = '*' then
    vRoomName := '';

  ARoomName := vRoomName;
  if LeftStr(ARoomName, 1) = '#' then
  begin
    ARoomName := MidStr(ARoomName, 2, MaxInt);
    AIsRoom := True;
  end
  else
    AIsRoom := False;

  index := -1;
  for i := 0 to MsgPageControl.PageCount - 1 do
  begin
    if SameText((MsgPageControl.Pages[i].Controls[0] as TChatRoomFrame).RoomName, vRoomName) then
    begin
      Index := i;
      break;
    end;
  end;

  if Index < 0 then
  begin
    TabSheet := MsgPageControl.AddTabSheet;
    with TChatRoomFrame.Create(TabSheet) do
    begin
      Parent := TabSheet;
      Align := alClient;
      RoomName := vRoomName;
      Visible := True;

      IsRoom := AIsRoom;
    end;
    ActiveIt := True; //force to focus it
    //TabSheet.Name := ARoomName + '_Room';
    if ARoomName = '' then
      TabSheet.Caption := '[Server]'
    else
      TabSheet.Caption := vRoomName;
    Result := TabSheet.Controls[0] as TChatRoomFrame;
  end
  else
  begin
    TabSheet := MsgPageControl.Pages[Index];
    Result := TabSheet.Controls[0] as TChatRoomFrame;
  end;

  if (TabSheet <> nil) and ActiveIt then
    MsgPageControl.PageIndex := TabSheet.PageIndex;
end;

procedure TMainFrm.SetNick(ANick: string);
begin
  NicknameBtn.Caption := ANick;
end;

procedure TMainFrm.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    SendNow;
  end;
end;

procedure TMainFrm.RecentUp;
begin
  if Recents.Count > 0 then
  begin
    if RecentsIndex > 0 then
    begin
      RecentsIndex := RecentsIndex - 1;
      if RecentsIndex = 0 then
        SendEdit.Text := ''
      else
        SendEdit.Text := Recents[RecentsIndex - 1];
    end
    else
    begin
      RecentsIndex := Recents.Count;
      SendEdit.Text := Recents[RecentsIndex - 1];
    end;
  end;
end;

procedure TMainFrm.RecentDown;
begin
  if Recents.Count > 0 then
  begin
    if RecentsIndex < Recents.Count then
    begin
      RecentsIndex := RecentsIndex + 1;
      SendEdit.Text := Recents[RecentsIndex - 1];
    end
    else
    begin
      RecentsIndex := 0;
      SendEdit.Text := '';
    end;
  end;
end;

procedure TMainFrm.AddRecent(S: string);
var
  i: Integer;
begin
  i := Recents.IndexOf(S);
  if i >= 0 then
    Recents.Delete(i);
  Recents.Add(S);
  RecentsIndex := 0;
end;

function TMainFrm.CurrentRoom: string;
begin
  if MsgPageControl.ActivePage <> nil then
  begin
    Result := (MsgPageControl.ActivePage.Controls[0] as TChatRoomFrame).RoomName;
  end;
end;

constructor TMainFrm.Create(TheOwner: TComponent);
var
  ini: TIniFile;
begin
  inherited;
  {$macro on}
  Caption := Caption + ' ' + IntToStr(FPC_FULLVERSION);
  Recents := TStringList.Create;
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    UserEdit.Text := Ini.ReadString('User', 'Username', '');
    PasswordEdit.Text := Ini.ReadString('User', 'Password', '');
    RoomEdit.Text := Ini.ReadString('User', 'Room', '');
    HostEdit.Text := Ini.ReadString('User', 'Host', '');
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);
    LogEdit.Height := Ini.ReadInteger('Window', 'LogHeight', LogEdit.Height);
  finally
    FreeAndNil(ini);
  end;
  IRC := TMyIRCClient.Create;
  MsgPageControl.ActivePageIndex := 0;
  LogEdit.Clear;
end;

destructor TMainFrm.Destroy;
var
  ini: TIniFile;
begin
  IRC.Disconnect;
  IRC.Free;
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    if WindowState <> wsMaximized then
    begin
      Ini.WriteInteger('Window', 'Width', Width);
      Ini.WriteInteger('Window', 'Height', Height);
      Ini.WriteInteger('Window', 'LogHeight', LogEdit.Height);
    end;

    Ini.WriteString('User', 'Username', UserEdit.Text);
    Ini.WriteString('User', 'Password', PasswordEdit.Text);
    Ini.WriteString('User', 'Room', RoomEdit.Text);
    Ini.WriteString('User', 'Host', HostEdit.Text);
  finally
    FreeAndNil(ini);
  end;
  FreeAndNil(Recents);
  inherited;
end;

procedure TMainFrm.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
var
  ChatRoom: TChatRoomFrame;
  i: Integer;
begin
  ChatRoom := NeedRoom(vChannel);
  if ChatRoom <> nil then
    with ChatRoom do
      begin
        case vMsgType of
          mtWelcome:
            MsgEdit.Lines.Add(vMSG);
          mtMOTD:
            MsgEdit.Lines.Add(vMSG);
          mtTopic:
            TopicEdit.Text := vMSG;
          mtJoin:
          begin
            MsgEdit.Lines.Add(vUser + ' is joined');
            i := UserListBox.items.IndexOf(vUser);
            if i < 0 then
              UserListBox.items.Add(vUser);
          end;
          mtPart:
          begin
            MsgEdit.Lines.Add(vUser + ' is parted: ' + vMsg);
            i := UserListBox.items.IndexOf(vUser);
            if i>=0 then
              UserListBox.items.Delete(i);
          end;
          mtQuit:
            MsgEdit.Lines.Add(vUser + ' is quit: ' + vMsg);
          mtNotice:
            MsgEdit.Lines.Add('[' + vUser + '] ' + vMSG);
          mtCTCPNotice, mtCTCPMessage:
            MsgEdit.Lines.Add(vMSG);
          mtMessage, mtSend:
          begin
            MsgEdit.Lines.Add(vUser + ': ' + vMSG);
            MsgEdit.ScrollBy(0, 1);
          end;
        else
            LogEdit.Lines.Add(vMSG);
        end;
      end;
end;

procedure TMainFrm.ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
var
  ChatRoom: TChatRoomFrame;
  i: Integer;
begin
  ChatRoom := NeedRoom(vChannel);
  if ChatRoom <> nil then
    with ChatRoom do
  begin
    UserListBox.Clear;
    for i := 0 to vUserNames.Count -1 do
    begin
      UserListBox.Items.Add(vUserNames[i].Name);
    end;
  end;
end;

end.

