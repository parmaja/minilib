object Main: TMain
  Left = 1859
  Top = 554
  BiDiMode = bdLeftToRight
  Caption = 'Mini Web Server'
  ClientHeight = 359
  ClientWidth = 434
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  ParentBiDiMode = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    434
    359)
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 8
    Width = 23
    Height = 13
    Caption = 'Root'
  end
  object Label2: TLabel
    Left = 9
    Top = 33
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label5: TLabel
    Left = 134
    Top = 33
    Width = 22
    Height = 13
    Caption = 'Alias'
  end
  object Memo: TMemo
    Left = 0
    Top = 152
    Width = 434
    Height = 207
    Align = alBottom
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 151
    ExplicitWidth = 430
  end
  object StartBtn: TButton
    Left = 368
    Top = 6
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
    ExplicitLeft = 364
  end
  object RootEdit: TEdit
    Left = 46
    Top = 6
    Width = 317
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 2
    Text = 'c:\offline\'
    ExplicitWidth = 313
  end
  object StopBtn: TButton
    Left = 368
    Top = 30
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = StopBtnClick
    ExplicitLeft = 364
  end
  object PortEdit: TEdit
    Left = 46
    Top = 30
    Width = 75
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 3
    Text = '81'
  end
  object StayOnTopChk: TCheckBox
    Left = 8
    Top = 52
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 4
    OnClick = StayOnTopChkClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 118
    Width = 434
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 6
    ExplicitTop = 117
    ExplicitWidth = 430
    object LastIDLabel: TLabel
      AlignWithMargins = True
      Left = 233
      Top = 6
      Width = 60
      Height = 25
      Align = alLeft
      AutoSize = False
      Caption = '0'
      Color = 13224393
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = 13
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ShowAccelChar = False
      Transparent = False
      Layout = tlCenter
      ExplicitLeft = 221
      ExplicitTop = 3
      ExplicitHeight = 20
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 193
      Top = 6
      Width = 34
      Height = 25
      Align = alLeft
      Caption = 'Last ID'
      Color = clBtnFace
      Font.Charset = ARABIC_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 101
      Top = 6
      Width = 20
      Height = 25
      Align = alLeft
      Caption = 'Max'
      Color = clBtnFace
      Font.Charset = ARABIC_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object MaxOfThreadsLabel: TLabel
      AlignWithMargins = True
      Left = 127
      Top = 6
      Width = 60
      Height = 25
      Align = alLeft
      AutoSize = False
      Caption = '0'
      Color = 13224393
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = 13
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ShowAccelChar = False
      Transparent = False
      Layout = tlCenter
      ExplicitLeft = 113
      ExplicitTop = 3
      ExplicitHeight = 20
    end
    object NumberOfThreads: TLabel
      AlignWithMargins = True
      Left = 35
      Top = 6
      Width = 60
      Height = 25
      Align = alLeft
      AutoSize = False
      Caption = '0'
      Color = 13224393
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = 13
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ShowAccelChar = False
      Transparent = False
      Layout = tlCenter
      ExplicitTop = 3
      ExplicitHeight = 20
    end
    object NumberOfThreadsLbl: TLabel
      Left = 3
      Top = 3
      Width = 29
      Height = 28
      Align = alLeft
      Caption = 'Count'
      Color = clBtnFace
      Font.Charset = ARABIC_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 13
    end
  end
  object Button1: TButton
    Left = 368
    Top = 53
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'MakeCert'
    Default = True
    TabOrder = 7
    OnClick = Button1Click
    ExplicitLeft = 364
  end
  object UseSSLChk: TCheckBox
    Left = 108
    Top = 54
    Width = 97
    Height = 17
    Caption = 'Use SSL'
    TabOrder = 8
    OnClick = StayOnTopChkClick
  end
  object Button2: TButton
    Left = 287
    Top = 53
    Width = 75
    Height = 22
    Caption = 'Test'
    TabOrder = 9
    OnClick = Button2Click
  end
  object ModuleNameEdit: TEdit
    Left = 162
    Top = 31
    Width = 200
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 10
    Text = 'doc'
    ExplicitWidth = 196
  end
  object KeepAliveChk: TCheckBox
    Left = 8
    Top = 77
    Width = 97
    Height = 17
    Caption = 'Keep Alive'
    TabOrder = 11
    OnClick = StayOnTopChkClick
  end
  object CompressChk: TCheckBox
    Left = 108
    Top = 77
    Width = 97
    Height = 17
    Caption = 'Compress'
    TabOrder = 12
    OnClick = StayOnTopChkClick
  end
end
