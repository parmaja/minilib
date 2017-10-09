unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, mnIRCClients;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    Button2: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    LogEdit: TMemo;
    RoomMsgEdit: TMemo;
    MsgEdit: TMemo;
    MsgPageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RoomEdit: TEdit;
    UserEdit: TEdit;
    SendBtn: TButton;
    SendEdit: TEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button2Click(Sender: TObject);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure MsgPageControlChange(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    IRC: TmnIRCClient;
    procedure ConnectNow;
    procedure SendNow;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoReceive(Sender: TObject; vChannel, vMSG: String);

    procedure DoLog(Sender: TObject; vLogType: TIRCLogType; vMsg: String);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

{ TMainFrm }

procedure TMainFrm.Button2Click(Sender: TObject);
begin
  ConnectNow;
end;

procedure TMainFrm.ConnectNow;
begin
  IRC.Host := HostEdit.Text;
  IRC.Port := '6667';
  IRC.Nick := UserEdit.Text;
  //IRC.Username := 'Zezo';
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

procedure TMainFrm.MsgPageControlChange(Sender: TObject);
begin

end;

procedure TMainFrm.SendBtnClick(Sender: TObject);
begin
  SendNow;
end;

procedure TMainFrm.SendNow;
begin
  IRC.Send(RoomEdit.Text, SendEdit.Text);
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
begin
  inherited Create(TheOwner);
  IRC := TmnIRCClient.Create;
  IRC.OnLog := @DoLog;
  IRC.OnReceive := @DoReceive;
  MsgPageControl.ActivePageIndex := 0;
end;

destructor TMainFrm.Destroy;
begin
  IRC.Close;
  IRC.Free;
  inherited Destroy;
end;

procedure TMainFrm.DoReceive(Sender: TObject; vChannel, vMSG: String);
begin
  MsgEdit.Lines.Add(vMSG);
end;

procedure TMainFrm.DoLog(Sender: TObject; vLogType: TIRCLogType; vMsg: String);
begin
  case vLogType of
    lgMsg: LogEdit.Lines.Add('#'+vMsg);
    lgSend: LogEdit.Lines.Add('>'+vMsg);
    lgReceive: LogEdit.Lines.Add('<'+vMsg);
  end;
end;

end.

