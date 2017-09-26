unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ntvSplitters, mnIRCClients;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    Button2: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    MsgEdit: TMemo;
    LogEdit: TMemo;
    Panel2: TPanel;
    RoomEdit: TEdit;
    SendBtn: TButton;
    SendEdit: TEdit;
    Splitter1: TSplitter;
    procedure Button2Click(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    IRC: TmnIRCClient;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoLog(Sender: TObject; AResponse: String);
    procedure DoSendData(Sender: TObject; AResponse: String);
    procedure DoReceive(Sender: TObject; vRoom, vMSG: String);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

{ TMainFrm }

procedure TMainFrm.Button2Click(Sender: TObject);
begin
  IRC.Host := HostEdit.Text;
  IRC.Port := '6667';
  IRC.Nick := 'Zezo';
  //IRC.Username := 'Zezo';
  IRC.Connect;
  IRC.Join(RoomEdit.Text);
end;

procedure TMainFrm.SendBtnClick(Sender: TObject);
begin
  IRC.Send(RoomEdit.Text, SendEdit.Text);
  SendEdit.Text := '';
end;

procedure TMainFrm.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Key := #0;
end;

constructor TMainFrm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IRC := TmnIRCClient.Create;
  IRC.OnSendData := @DoSendData;
  IRC.OnReceiveData := @DoSendData;
  //IRC.OnLog := @DoLog;
  IRC.OnReceive := @DoReceive;
end;

destructor TMainFrm.Destroy;
begin
  IRC.Close;
  IRC.Free;
  inherited Destroy;
end;

procedure TMainFrm.DoLog(Sender: TObject; AResponse: String);
begin
  LogEdit.Lines.Add(AResponse);
end;

procedure TMainFrm.DoReceive(Sender: TObject; vRoom, vMSG: String);
begin
  MsgEdit.Lines.Add(vMSG);
end;

procedure TMainFrm.DoSendData(Sender: TObject; AResponse: String);
begin
  LogEdit.Lines.Add(AResponse);
end;

end.

