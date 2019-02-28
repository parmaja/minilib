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
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    IRC: TMyIRCClient;
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
      prgRegistering: MainFrm.ConnectBtn.Caption := 'Disconnect...';
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
  IRC.Nick := UserEdit.Text;
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

procedure TMainFrm.SendNow;
begin
  IRC.ChannelSend(CurrentRoom, SendEdit.Text);
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
    if SameText(MsgPageControl.Pages[i].Name, ARoomName + '_Room') then
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
    TabSheet.Name := ARoomName + '_Room';
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
  inherited Create(TheOwner);
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
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);
    Ini.WriteInteger('Window', 'LogHeight', LogEdit.Height);

    Ini.WriteString('User', 'Username', UserEdit.Text);
    Ini.WriteString('User', 'Password', PasswordEdit.Text);
    Ini.WriteString('User', 'Room', RoomEdit.Text);
    Ini.WriteString('User', 'Host', HostEdit.Text);
  finally
    FreeAndNil(ini);
  end;
  inherited Destroy;
end;

procedure TMainFrm.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
var
  ChatRoom: TChatRoomFrame;
begin
  ChatRoom := NeedRoom(vChannel);
  if ChatRoom <> nil then
    with ChatRoom do
  begin
    case vMsgType of
      mtLog:
        LogEdit.Lines.Add(vMSG);
      mtWelcome:
        MsgEdit.Lines.Add(vMSG);
      mtNotice:
        MsgEdit.Lines.Add(vMSG);
      mtTopic:
        TopicEdit.Text := vMSG;
      mtJoin:
        MsgEdit.Lines.Add(vUser + ' is joined: ' + vMsg);
      mtPart:
        MsgEdit.Lines.Add(vUser + ' is parted: ' + vMsg);
      mtQuit:
        MsgEdit.Lines.Add(vUser + ' is quit: ' + vMsg);
    else
        MsgEdit.Lines.Add(vUser + ': ' + vMSG);
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

