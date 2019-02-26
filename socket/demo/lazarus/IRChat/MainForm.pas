unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, IniFiles,
  StdCtrls, ExtCtrls, ComCtrls, Menus, mnIRCClients;

type

  { TMyIRCClient }

  TMyIRCClient = class(TmnIRCClient)
  public
    procedure ReceiveNames(vChannel: string; vUserNames: TIRCChannelUserNames); override;
    procedure StateChanged; override;
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    ConnectBtn: TButton;
    TopicEdit: TEdit;
    JoinBtn: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PasswordEdit: TEdit;
    NicknameBtn: TButton;
    SendEdit: TEdit;
    ServerMsgEdit: TMemo;
    Splitter2: TSplitter;
    TabSheet2: TTabSheet;
    UserListBox: TListBox;
    LogEdit: TMemo;
    MenuItem1: TMenuItem;
    LogPopupMenu: TPopupMenu;
    MsgEdit: TMemo;
    MsgPageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RoomEdit: TEdit;
    UserEdit: TEdit;
    SendBtn: TButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgPageControlChange(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    IRC: TMyIRCClient;
    procedure ConnectNow;
    procedure SendNow;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoReceive(Sender: TObject; vMsgType: TIRCMsgType; vChannel, vMSG: String);

    procedure DoLog(Sender: TObject; vLogType: TIRCLogType; vMsg: String);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

{ TMyIRCClient }

procedure TMyIRCClient.ReceiveNames(vChannel: string; vUserNames: TIRCChannelUserNames);
var
  i: Integer;
begin
  inherited;
  MainFrm.UserListBox.Clear;
  for i := 0 to vUserNames.Count -1 do
  begin
    MainFrm.UserListBox.Items.Add(vUserNames[i].Name);
  end;
end;

procedure TMyIRCClient.StateChanged;
begin
  inherited StateChanged;
  case State of
    isDisconnected: MainFrm.ConnectBtn.Caption := 'Connect';
    isRegistering: MainFrm.ConnectBtn.Caption := 'Disconnect...';
    isReady: MainFrm.ConnectBtn.Caption := 'Disconnect';
  end;
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
  IRC.ChannelSend(RoomEdit.Text, SendEdit.Text);
  SendEdit.Text := '';
end;

procedure TMainFrm.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    SendNow;
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
  finally
    FreeAndNil(ini);
  end;
  IRC := TMyIRCClient.Create;
  IRC.OnLog := @DoLog;
  IRC.OnReceive := @DoReceive;
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

    Ini.WriteString('User', 'Username', UserEdit.Text);
    Ini.WriteString('User', 'Password', PasswordEdit.Text);
    Ini.WriteString('User', 'Room', RoomEdit.Text);
    Ini.WriteString('User', 'Host', HostEdit.Text);
  finally
    FreeAndNil(ini);
  end;
  inherited Destroy;
end;

procedure TMainFrm.DoReceive(Sender: TObject; vMsgType: TIRCMsgType; vChannel, vMSG: String);
begin
  case vMsgType of
    mtWelcome:
      MsgEdit.Lines.Add(vMSG);
    mtNotice:
      MsgEdit.Lines.Add(vMSG);
    mtTopic:
      TopicEdit.Text := vMSG;
  else
    if vChannel = '' then
      ServerMsgEdit.Lines.Add(vMSG)
    else
      MsgEdit.Lines.Add(vMSG);
  end;
end;

procedure TMainFrm.DoLog(Sender: TObject; vLogType: TIRCLogType; vMsg: String);
begin
  case vLogType of
    lgMsg: LogEdit.Lines.Add(vMsg);
    lgSend: LogEdit.Lines.Add('>'+vMsg);
    lgReceive: LogEdit.Lines.Add('<'+vMsg);
  end;
end;

end.

