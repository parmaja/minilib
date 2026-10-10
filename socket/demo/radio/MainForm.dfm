object MainForm: TMainForm
  Left = 400
  Top = 260
  Caption = 'Radio - MiniLib IceCast + Windows API sound'
  ClientHeight = 190
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object URLLabel: TLabel
    Left = 16
    Top = 14
    Width = 26
    Height = 15
    Caption = 'URL:'
  end
  object URLEdit: TEdit
    Left = 16
    Top = 35
    Width = 318
    Height = 23
    TabOrder = 0
    Text = 'http://solid24.streamupsolutions.com:8026/stream'
  end
  object PlayButton: TButton
    Left = 348
    Top = 34
    Width = 66
    Height = 25
    Caption = 'Play'
    TabOrder = 1
    OnClick = PlayButtonClick
  end
  object StopButton: TButton
    Left = 16
    Top = 70
    Width = 66
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object VolumeLabel: TLabel
    Left = 100
    Top = 76
    Width = 44
    Height = 15
    Caption = 'Volume:'
  end
  object VolumeBar: TTrackBar
    Left = 148
    Top = 66
    Width = 266
    Height = 33
    Max = 100
    Frequency = 10
    Position = 90
    TabOrder = 3
    OnChange = VolumeBarChange
  end
  object StatusLabel: TLabel
    Left = 16
    Top = 112
    Width = 398
    Height = 15
    AutoSize = False
    Caption = 'Stopped'
  end
  object TitleLabel: TLabel
    Left = 16
    Top = 134
    Width = 398
    Height = 17
    AutoSize = False
    Caption = ''
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object InfoLabel: TLabel
    Left = 16
    Top = 158
    Width = 398
    Height = 15
    AutoSize = False
    Caption = ''
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
end
