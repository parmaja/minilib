object Main: TMain
  Left = 1859
  Top = 554
  BiDiMode = bdLeftToRight
  Caption = 'Mini Web Server'
  ClientHeight = 388
  ClientWidth = 439
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
    439
    388)
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Home Path'
  end
  object Label2: TLabel
    Left = 10
    Top = 33
    Width = 51
    Height = 13
    Caption = 'HTTP Port'
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
    Top = 176
    Width = 439
    Height = 212
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StartBtn: TButton
    Left = 369
    Top = 6
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
    ExplicitLeft = 365
  end
  object HomePathEdit: TEdit
    Left = 88
    Top = 6
    Width = 276
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 2
    Text = 'c:\offline\'
    ExplicitWidth = 272
  end
  object StopBtn: TButton
    Left = 369
    Top = 30
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = StopBtnClick
    ExplicitLeft = 365
  end
  object PortEdit: TEdit
    Left = 88
    Top = 30
    Width = 40
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 3
    Text = '81'
  end
  object StayOnTopChk: TCheckBox
    Left = 10
    Top = 59
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 4
    OnClick = StayOnTopChkClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 142
    Width = 439
    Height = 34
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 6
    ExplicitTop = 119
    ExplicitWidth = 435
    object LastIDLabel: TLabel
      AlignWithMargins = True
      Left = 233
      Top = 6
      Width = 60
      Height = 22
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
      Height = 13
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
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 101
      Top = 6
      Width = 20
      Height = 13
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
    end
    object MaxOfThreadsLabel: TLabel
      AlignWithMargins = True
      Left = 127
      Top = 6
      Width = 60
      Height = 22
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
      Height = 22
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
      Height = 13
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
    end
  end
  object Button1: TButton
    Left = 355
    Top = 58
    Width = 76
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'MakeCert2'
    Default = True
    TabOrder = 7
    OnClick = Button1Click
  end
  object UseSSLChk: TCheckBox
    Left = 108
    Top = 61
    Width = 97
    Height = 17
    Caption = 'Use SSL'
    TabOrder = 8
    OnClick = StayOnTopChkClick
  end
  object Button2: TButton
    Left = 371
    Top = 114
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Test'
    TabOrder = 9
    OnClick = Button2Click
  end
  object AliasNameEdit: TEdit
    Left = 163
    Top = 30
    Width = 201
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 10
    Text = 'doc'
    ExplicitWidth = 197
  end
  object KeepAliveChk: TCheckBox
    Left = 10
    Top = 84
    Width = 97
    Height = 17
    Caption = 'Keep Alive'
    TabOrder = 11
    OnClick = StayOnTopChkClick
  end
  object CompressChk: TCheckBox
    Left = 108
    Top = 84
    Width = 97
    Height = 17
    Caption = 'Compress'
    TabOrder = 12
    OnClick = StayOnTopChkClick
  end
  object AutoOpenChk: TCheckBox
    Left = 108
    Top = 108
    Width = 141
    Height = 17
    Caption = 'Auto Open web page'
    TabOrder = 13
    OnClick = StayOnTopChkClick
  end
  object AutoRunChk: TCheckBox
    Left = 10
    Top = 108
    Width = 97
    Height = 17
    Caption = 'AutoRun'
    TabOrder = 14
    OnClick = StayOnTopChkClick
  end
  object OpenBtn: TButton
    Left = 355
    Top = 83
    Width = 76
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Open URL'
    TabOrder = 15
    OnClick = OpenBtnClick
  end
  object Button3: TButton
    Left = 290
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 16
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 273
    Top = 58
    Width = 76
    Height = 22
    Caption = 'MakeCert1'
    TabOrder = 17
    OnClick = Button4Click
  end
end
