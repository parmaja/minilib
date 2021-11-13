unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, IpHtml, Forms, Controls, Graphics,
  Dialogs, Buttons, IniFiles, StdCtrls, ExtCtrls, ComCtrls, Menus, LCLType,
  mnMsgBox, GUIMsgBox, mnLogs,
  ChatRoomFrames, mnIRCClients;

type

  { TMyIRCClient }

  TMyIRCClient = class(TmnIRCClient)
  public
    procedure GetCurrentChannel(out vChannel: string); override;
    procedure DoLog(S: string); override;
    procedure DoMyInfoChanged; override;
    procedure DoConnected; override;
    procedure DoDisconnected; override;
    procedure DoUserChanged(vChannel: string; vUser, vNewNick: string); override;
    procedure DoProgressChanged; override;
    procedure DoUsersChanged(vChannelName: string; vChannel: TIRCChannel); override;
    procedure DoWhoIs(vUser: string); override;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string); override;
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    SendEdit: TMemo;
    WelcomeHtmlPnl: TIpHtmlPanel;
    OptionsBtn: TButton;
    WelcomePnl: TPanel;
    StatusPnl: TPanel;
    ProfileCbo: TComboBox;
    Label6: TLabel;
    UseSSLChk: TCheckBox;
    ConnectBtn: TButton;
    PortEdit: TEdit;
    JoinBtn: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MsgPageControl: TPageControl;
    NicknameBtn: TButton;
    SendPnl: TPanel;
    ChatPnl: TPanel;
    PasswordEdit: TEdit;
    LogEdit: TMemo;
    MenuItem1: TMenuItem;
    LogPopupMenu: TPopupMenu;
    Panel2: TPanel;
    RoomsEdit: TEdit;
    SendBtn: TButton;
    SmallImageList: TImageList;
    UserEdit: TEdit;
    Splitter1: TSplitter;
    NicknameEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure JoinBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgPageControlChange(Sender: TObject);
    procedure NicknameBtnClick(Sender: TObject);
    procedure ProfileCboSelect(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Body: TIpHtmlNodeBODY;
    Recents: TStringList;
    RecentsIndex: Integer;
    procedure AddMessage(aMsg: string; AClassName: string);
    procedure RecentUp;
    procedure RecentDown;
    procedure AddRecent(S: string);
    procedure LogMessage(S: string);
    function CurrentRoom: string;
    procedure ConnectNow;
    procedure SendNow;
    function NeedRoom(vRoomName: string; ActiveIt: Boolean = false): TChatRoomFrame;
    procedure SetNick(ANick: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnumProfiles;
    procedure SaveConfig;
    procedure SaveProfile(AName: string);
    procedure LoadProfile(AName: string);
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
    procedure ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
  end;

var
  MainFrm: TMainFrm;
  IRC: TMyIRCClient = nil;

implementation

{$R *.lfm}

{ TMyIRCClient }

procedure TMyIRCClient.GetCurrentChannel(out vChannel: string);
begin
  vChannel := MainFrm.CurrentRoom;
end;

procedure TMyIRCClient.DoLog(S: string);
begin
  inherited;
  MainFrm.LogMessage(S);
end;

procedure TMyIRCClient.DoMyInfoChanged;
begin
  inherited;
  MainFrm.SetNick(Session.Nick);
end;

procedure TMyIRCClient.DoConnected;
begin
  inherited DoConnected;
  MainFrm.LogMessage('Yes it is connected');
end;

procedure TMyIRCClient.DoDisconnected;
begin
  MainFrm.LogMessage('Yes it is disconnected');
  inherited;
end;

procedure TMyIRCClient.DoUserChanged(vChannel: string; vUser, vNewNick: string);
begin
  inherited;
  //TODO
end;

procedure TMyIRCClient.DoProgressChanged;
begin
  inherited;
  case Progress of
    prgDisconnected:
    begin
      MainFrm.ChatPnl.Visible := False;
      MainFrm.WelcomePnl.Visible := True;
      MainFrm.ConnectBtn.Caption := 'Connect';
      while MainFrm.MsgPageControl.PageCount > 0 do
        MainFrm.MsgPageControl.Page[0].Free;
    end;
    prgConnecting:
      MainFrm.ConnectBtn.Caption := 'Connecting';
    prgConnected:
    begin
      MainFrm.ConnectBtn.Caption := 'Disconnect';
      MainFrm.WelcomePnl.Visible := False;
      MainFrm.ChatPnl.Visible := True;
      MainFrm.SendEdit.SetFocus;
    end;
    prgReady:
    begin
      MainFrm.ConnectBtn.Caption := 'Disconnect';
    end;
  end;
end;

procedure TMyIRCClient.DoUsersChanged(vChannelName: string; vChannel: TIRCChannel);
begin
  inherited;
  MainFrm.ReceiveNames(vChannelName, vChannel);
end;

procedure TMyIRCClient.DoWhoIs(vUser: string);
var
  aUser: TIRCUser;
begin
  inherited;
  aUser := IRC.Session.Channels.FindUser('', vUser);
  if aUser <> nil then
  begin
    MainFrm.LogMessage(aUser.WhoIs.RealName);
    MainFrm.LogMessage(aUser.WhoIs.Server);
    MainFrm.LogMessage(aUser.WhoIs.Channels);
  end;
end;

procedure TMyIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string);
begin
  MainFrm.DoReceive(vMsgType, vChannel, vUser, vMsg);
end;

{ TMainFrm }

procedure TMainFrm.ConnectBtnClick(Sender: TObject);
begin
  if IRC.IsOpen then
    IRC.Close
  else
  begin
    ConnectNow;
  end;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin

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
var
  aName: string;
begin
  aName := ProfileCbo.Text;
  if MsgBox.Input(aName, 'Name a profile') then
  begin
    SaveProfile(AName);
    EnumProfiles;
    ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(aName);
  end;
end;

procedure TMainFrm.Button2Click(Sender: TObject);
begin
  if ProfileCbo.Text <> '' then
  begin
    DeleteFile(Application.Location + ProfileCbo.Text + '.profile');
    EnumProfiles;
  end;
end;

procedure TMainFrm.ConnectNow;
var
  Room: string;
  Rooms: TStringList;
begin
  SaveConfig;
  IRC.Host := HostEdit.Text;
  IRC.Port := PortEdit.Text;
  IRC.UseSSL := UseSSLChk.Checked;

  IRC.Nicks.Clear;
  IRC.Nicks.Add(NicknameEdit.Text);
  IRC.Nicks.Add(NicknameEdit.Text+'_');
  IRC.Nicks.Add(NicknameEdit.Text+'__');
  IRC.RealName := NicknameEdit.Text;

  IRC.Auth := authPASS;
  //IRC.Auth := authIDENTIFY;
  IRC.Username := UserEdit.Text;
  IRC.Password := PasswordEdit.Text;

  IRC.Open;
  Rooms := TStringList.Create;
  try
    Rooms.CommaText := RoomsEdit.Text;
    for Room in Rooms do
    begin
      IRC.Join(Room);
      //IRC.Who(Room); //TODO
    end;
  finally
    Rooms.Free;
  end;
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
  IRC.Join(RoomsEdit.Text);
end;

procedure TMainFrm.MenuItem1Click(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainFrm.MsgPageControlChange(Sender: TObject);
begin

end;

procedure TMainFrm.NicknameBtnClick(Sender: TObject);
var
  aNick: string;
begin
  aNick := IRC.Session.Nick;
  if MsgBox.Input(aNick, 'New Nickname?') then
  begin
    IRC.SetNick(aNick);
  end;
end;

procedure TMainFrm.ProfileCboSelect(Sender: TObject);
begin
  LoadProfile(ProfileCbo.Text);
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
      VK_TAB: //TODO
      begin
        if copy(SendEdit.Text, SendEdit.SelStart, 1) = '@' then
        begin
          Key := 0;
        end;
      end;
      VK_UP: RecentUp;
      VK_DOWN: RecentDown;
      VK_RETURN:
      begin
        Key := 0;
        SendNow;
      end;
    end;
  end;
end;

procedure TMainFrm.SendNow;
var
  s: string;
begin
  s := TrimRight(SendEdit.Text);
  if s <> '' then
    if IRC.Online then
    begin
      IRC.SendMsg(CurrentRoom, SendEdit.Text);
      AddRecent(SendEdit.Text);
      SendEdit.Text := '';
    end;
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
    with TChatRoomFrame.CreateParented(TabSheet.Handle) do
    begin
      Parent := TabSheet;
      Align := alClient;
      RoomName := vRoomName;
      IsRoom := AIsRoom;
      Visible := True;
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
 //TODO change it in the list
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

procedure TMainFrm.LogMessage(S: string);
begin
  LogEdit.Lines.Add(S)
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
  i: Integer;
  ini: TIniFile;
  aProfile: string;
begin
  inherited;
  InstallFileLog('log.txt');
  InstallEventLog(@LogMessage);
  {$ifdef DEBUG}
  InstallConsoleLog;
  InstallDebugOutputLog;
  {$endif}
  {$macro on}
  WelcomePnl.Align := alClient;
  ChatPnl.Align := alClient;
  Caption := Caption + ' ' + IntToStr(FPC_FULLVERSION);
  Recents := TStringList.Create;
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    aProfile := Ini.ReadString('Options', 'Profile', '');
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);
    LogEdit.Height := Ini.ReadInteger('Window', 'LogHeight', LogEdit.Height);
  finally
    FreeAndNil(ini);
  end;
  IRC := TMyIRCClient.Create;
  MsgPageControl.ActivePageIndex := 0;
  LogEdit.Clear;
  EnumProfiles;
  ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(aProfile);
  LoadProfile(aProfile);
  WelcomeHtmlPnl.SetHtmlFromFile(Application.Location + 'chat.html');
  //Find Body
  //Viewer.EnumDocuments(@HtmlEnumerator);
  for i :=0 to WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildCount - 1 do
    if WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildNode[i] is TIpHtmlNodeBODY then
    begin
      Body := TIpHtmlNodeBODY(WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildNode[i]);
      break;
    end;
  //AddMessage('Welcome to irc, click connect', 'action');
end;

destructor TMainFrm.Destroy;
begin
  IRC.Close;
  IRC.Free;
  SaveConfig;
  FreeAndNil(Recents);
  inherited;
end;

procedure TMainFrm.EnumProfiles;
var
  AProfiles: TStringList;
  s: string;
begin
  ProfileCbo.Clear;
  AProfiles := TStringList.Create;
  try
    FindAllFiles(AProfiles, Application.Location, '*.profile', False);
    for s in AProfiles do
    begin
      ProfileCbo.Items.Add(ExtractFileNameWithoutExt(ExtractFileName(s)));
    end;
  finally
    AProfiles.Free;
  end;
end;

procedure TMainFrm.SaveProfile(AName: string);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Application.Location + AName + '.profile');
  try
    Ini.WriteString('User', 'Username', UserEdit.Text);
    Ini.WriteString('User', 'Password', PasswordEdit.Text);
    Ini.WriteString('User', 'Nickname', NicknameEdit.Text);
    Ini.WriteString('User', 'Room', RoomsEdit.Text);
    Ini.WriteString('User', 'Host', HostEdit.Text);
    Ini.WriteString('User', 'Port', PortEdit.Text);
    Ini.WriteBool('User', 'SSL', UseSSLChk.Checked);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainFrm.LoadProfile(AName: string);
var
  ini: TIniFile;
begin
  inherited;
  ini := TIniFile.Create(Application.Location + AName + '.profile');
  try
    UserEdit.Text := Ini.ReadString('User', 'Username', '');
    PasswordEdit.Text := Ini.ReadString('User', 'Password', '');
    NicknameEdit.Text := Ini.ReadString('User', 'Nickname', '');
    RoomsEdit.Text := Ini.ReadString('User', 'Room', '');
    HostEdit.Text := Ini.ReadString('User', 'Host', '');
    PortEdit.Text := Ini.ReadString('User', 'Port', '6667');
    UseSSLChk.Checked := Ini.ReadBool('User', 'SSL', false);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainFrm.SaveConfig;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    if WindowState <> wsMaximized then
    begin
      Ini.WriteInteger('Window', 'Width', Width);
      Ini.WriteInteger('Window', 'Height', Height);
      Ini.WriteInteger('Window', 'LogHeight', LogEdit.Height);
      Ini.WriteString('Options', 'Profile', ProfileCbo.Text);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainFrm.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
var
  ChatRoom: TChatRoomFrame;
  aItem: TListItem;
  oUser: TIRCUser;
begin
  if vChannel = '' then
    LogMessage(vMSG)
  else
  begin
    ChatRoom := NeedRoom(vChannel);
    if ChatRoom <> nil then
      with ChatRoom do
        begin
          case vMsgType of
            mtWelcome:
            begin
              AddMessage(vMSG);
              TopicEdit.Text := vMSG;
            end;
            mtMOTD:
              AddMessage(vMSG);
            mtTopic:
            begin
              TopicEdit.Text := vMSG;
              AddMessage(vMSG, '', True);
            end;
            mtJoin:
            begin
              AddMessage(vUser + ' is joined', 'hint');
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem = nil then
              begin
                aItem := UserListBox.Items.Add;
                aItem.Caption := vUser;
                oUser := IRC.Session.Channels.FindUser(vChannel, vUser);
                if oUser <> nil then
                begin
                  if ([umAdmin, umOwner] * oUser.Mode <> []) then
                    aItem.ImageIndex := 3
                  else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
                    aItem.ImageIndex := 2
                  else if ([umVoice] * oUser.Mode <> []) then
                    aItem.ImageIndex := 1
                  else
                    aItem.ImageIndex := 0;
                end;
              end;
            end;
            mtLeft:
            begin
              //if me close the tab
              AddMessage(vUser + ' is left: ' + vMsg, 'hint');
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem <> nil then
                UserListBox.items.Delete(aItem.Index);
            end;
            mtUserMode:
            begin
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem = nil then
              begin
                aItem := UserListBox.Items.Add;
                aItem.Caption := vUser;
                aItem.ImageIndex := 0;
              end;
              oUser := IRC.Session.Channels.FindUser(vChannel, vUser);
              if oUser <> nil then
              begin
                if ([umAdmin, umOwner] * oUser.Mode <> []) then
                  aItem.ImageIndex := 3
                else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
                  aItem.ImageIndex := 2
                else if ([umVoice] * oUser.Mode <> []) then
                  aItem.ImageIndex := 1
                else
                  aItem.ImageIndex := 0;
              end;
            end;
            mtNotice:
              AddMessage('[' + vUser + '] ' + vMSG, 'notice');
            mtMessage:
            begin
              AddMessage(vUser + ': ' + vMSG, ' received');
            end;
            mtSend:
            begin
              AddMessage(vUser + ': ' + vMSG, 'self');
            end;
            mtAction:
            begin
              AddMessage('* ' + vUser + ': -' + vMSG + '-', 'action');
            end;
          else
              AddMessage(vUser + ': ' + vMSG, 'log');
          end;
        end;
   end;
end;

procedure TMainFrm.ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
var
  oUser: TIRCUser;
  ChatRoom: TChatRoomFrame;
  aItem: TListItem;
  i: Integer;
begin
  ChatRoom := NeedRoom(vChannel);
  if ChatRoom <> nil then
    with ChatRoom do
  begin
    UserListBox.Clear;
    for i := 0 to vUserNames.Count -1 do
    begin
      aItem := UserListBox.Items.Add;
      aItem.Caption := vUserNames[i].Name;
      oUser := vUserNames[i];
      if oUser <> nil then
      begin
        if ([umAdmin, umOwner] * oUser.Mode <> []) then
          aItem.ImageIndex := 3
        else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
          aItem.ImageIndex := 2
        else if ([umVoice] * oUser.Mode <> []) then
          aItem.ImageIndex := 1
        else
          aItem.ImageIndex := 0;
      end;
    end;
  end;
end;

procedure TMainFrm.AddMessage(aMsg: string; AClassName: string);
var
  TextNode: TIpHtmlNodeText;
  PNode: TIpHtmlNodeP;
  //DivNode: TIpHtmlNodeDIV;
begin
  {DivNode := TIpHtmlNodeDIV.Create(Body);
  DivNode.ClassId := AClassName;}
  PNode := TIpHtmlNodeP.Create(Body);
  PNode.ClassId := AClassName;
  TextNode := TIpHtmlNodeText.Create(PNode);
  TextNode.EscapedText := aMsg;

  with TIpHtmlNodeBR.Create(Body) do
  begin
  end;

  WelcomeHtmlPnl.Update;
  WelcomeHtmlPnl.Scroll(hsaEnd);
end;

end.

