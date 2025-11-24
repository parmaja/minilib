object Main: TMain
  Left = 1859
  Top = 554
  BiDiMode = bdLeftToRight
  Caption = 'Mini Web Server'
  ClientHeight = 333
  ClientWidth = 453
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
    453
    333)
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 9
    Width = 53
    Height = 13
    Caption = 'Home Path'
  end
  object Label2: TLabel
    Left = 276
    Top = 9
    Width = 19
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Port'
    ExplicitLeft = 264
  end
  object Label5: TLabel
    Left = 167
    Top = 33
    Width = 22
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Alias'
    ExplicitLeft = 170
  end
  object Label6: TLabel
    Left = 259
    Top = 33
    Width = 28
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Home'
    ExplicitLeft = 262
  end
  object Label7: TLabel
    Left = 10
    Top = 31
    Width = 21
    Height = 13
    Caption = 'Bind'
  end
  object Memo: TMemo
    Left = 0
    Top = 143
    Width = 453
    Height = 190
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object StartBtn: TButton
    Left = 377
    Top = 6
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object HomePathEdit: TEdit
    Left = 80
    Top = 6
    Width = 173
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 3
    Text = '.\html'
  end
  object StopBtn: TButton
    Left = 377
    Top = 30
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = StopBtnClick
  end
  object PortEdit: TEdit
    Left = 301
    Top = 6
    Width = 62
    Height = 21
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 4
    Text = '81'
  end
  object StayOnTopChk: TCheckBox
    Left = 8
    Top = 84
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 8
    OnClick = StayOnTopChkClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 108
    Width = 453
    Height = 35
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 9
    object LastIDLabel: TLabel
      AlignWithMargins = True
      Left = 233
      Top = 6
      Width = 60
      Height = 23
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
      Height = 23
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
      Height = 23
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
    Left = 299
    Top = 58
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'MakeCert2'
    Default = True
    TabOrder = 10
    OnClick = Button1Click
  end
  object UseSSLChk: TCheckBox
    Left = 8
    Top = 63
    Width = 97
    Height = 17
    Caption = 'Use SSL'
    TabOrder = 11
    OnClick = StayOnTopChkClick
  end
  object Button2: TButton
    Left = 299
    Top = 86
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Test'
    TabOrder = 12
    OnClick = Button2Click
  end
  object DocAliasEdit: TEdit
    Left = 199
    Top = 30
    Width = 54
    Height = 21
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 6
    Text = 'doc'
  end
  object KeepAliveChk: TCheckBox
    Left = 106
    Top = 63
    Width = 97
    Height = 17
    Caption = 'Keep Alive'
    TabOrder = 13
    OnClick = StayOnTopChkClick
  end
  object CompressChk: TCheckBox
    Left = 196
    Top = 63
    Width = 97
    Height = 17
    Caption = 'Compress'
    TabOrder = 14
    OnClick = StayOnTopChkClick
  end
  object AutoOpenChk: TCheckBox
    Left = 304
    Top = 114
    Width = 141
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Auto Open URL'
    TabOrder = 15
    OnClick = StayOnTopChkClick
  end
  object AutoRunChk: TCheckBox
    Left = 106
    Top = 84
    Width = 97
    Height = 17
    Caption = 'AutoRun'
    TabOrder = 16
    OnClick = StayOnTopChkClick
  end
  object OpenBtn: TButton
    Left = 377
    Top = 86
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Open URL'
    TabOrder = 17
    OnClick = OpenBtnClick
  end
  object Button4: TButton
    Left = 377
    Top = 58
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'MakeCert'
    TabOrder = 18
    OnClick = Button4Click
  end
  object HomeAliasEdit: TEdit
    Left = 298
    Top = 30
    Width = 65
    Height = 21
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 7
    Text = 'home'
  end
  object BindEdit: TEdit
    Left = 80
    Top = 30
    Width = 81
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 5
    Text = '0.0.0.0'
  end
end
