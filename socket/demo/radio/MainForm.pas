unit MainForm;
{**
 *  Radio demo for "Mini Library"
 *  Streaming with mnIceCasts, sound output with Windows API (ACM + waveOut)
 *}

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RadioPlayer;

type
  TMainForm = class(TForm)
    URLLabel: TLabel;
    URLEdit: TEdit;
    PlayButton: TButton;
    StopButton: TButton;
    VolumeLabel: TLabel;
    VolumeBar: TTrackBar;
    StatusLabel: TLabel;
    TitleLabel: TLabel;
    InfoLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure VolumeBarChange(Sender: TObject);
  private
    FPlayer: TRadioPlayer;
    procedure PlayerStateChanged(Sender: TObject; AState: TRadioPlayerState; const AMessage: string);
    procedure PlayerTitleChanged(Sender: TObject; const ATitle: string);
  end;

var
  RadioForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPlayer := TRadioPlayer.Create;
  FPlayer.OnStateChanged := PlayerStateChanged;
  FPlayer.OnTitleChanged := PlayerTitleChanged;
  VolumeBar.Position := FPlayer.Volume;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPlayer);
end;

procedure TMainForm.PlayButtonClick(Sender: TObject);
begin
  PlayButton.Enabled := False;
  FPlayer.Play(Trim(URLEdit.Text));
end;

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  FPlayer.Stop;
end;

procedure TMainForm.VolumeBarChange(Sender: TObject);
begin
  FPlayer.Volume := VolumeBar.Position;
end;

procedure TMainForm.PlayerStateChanged(Sender: TObject; AState: TRadioPlayerState; const AMessage: string);
begin
  case AState of
    rpsConnecting: StatusLabel.Caption := 'Connecting ...';
    rpsBuffering:  StatusLabel.Caption := 'Buffering ...';
    rpsPlaying:    StatusLabel.Caption := 'Playing';
    rpsStopped:    StatusLabel.Caption := 'Stopped';
    rpsError:      StatusLabel.Caption := 'Error: ' + AMessage;
  else
    StatusLabel.Caption := 'Stopped';
  end;
  StopButton.Enabled := AState in [rpsConnecting, rpsBuffering, rpsPlaying];
  PlayButton.Enabled := not StopButton.Enabled;
  if AState in [rpsBuffering, rpsPlaying] then
  begin
    InfoLabel.Caption := FPlayer.Station;
    if FPlayer.Bitrate <> '' then
      InfoLabel.Caption := InfoLabel.Caption + '  |  ' + FPlayer.Bitrate + ' kbps';
    if FPlayer.ContentType <> '' then
      InfoLabel.Caption := InfoLabel.Caption + '  |  ' + FPlayer.ContentType;
  end
  else if AState = rpsError then
    InfoLabel.Caption := '';
end;

procedure TMainForm.PlayerTitleChanged(Sender: TObject; const ATitle: string);
begin
  TitleLabel.Caption := ATitle;
end;

end.
