object Main: TMain
  Left = 165
  Top = 169
  BiDiMode = bdRightToLeft
  Caption = 'Mini Web Server'
  ClientHeight = 319
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    426
    319)
  PixelsPerInch = 96
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
    Top = 32
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Memo: TMemo
    Left = 0
    Top = 114
    Width = 426
    Height = 205
    Align = alBottom
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 120
  end
  object StartBtn: TButton
    Left = 360
    Top = 6
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object RootEdit: TEdit
    Left = 46
    Top = 6
    Width = 309
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 2
    Text = 'c:\offline\'
  end
  object StopBtn: TButton
    Left = 360
    Top = 30
    Width = 60
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = StopBtnClick
  end
  object PortEdit: TEdit
    Left = 46
    Top = 29
    Width = 155
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 3
    Text = '81'
  end
  object StayOnTopChk: TCheckBox
    Left = 2
    Top = 52
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 4
    OnClick = StayOnTopChkClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 88
    Width = 426
    Height = 26
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 6
    ExplicitLeft = 8
    object LastIDLabel: TLabel
      Left = 206
      Top = 3
      Width = 60
      Height = 20
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
    end
    object Label4: TLabel
      Left = 172
      Top = 3
      Width = 34
      Height = 20
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
      Left = 92
      Top = 3
      Width = 20
      Height = 20
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
      Left = 112
      Top = 3
      Width = 60
      Height = 20
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
      ExplicitLeft = 121
    end
    object NumberOfThreads: TLabel
      Left = 32
      Top = 3
      Width = 60
      Height = 20
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
      ExplicitLeft = 35
    end
    object NumberOfThreadsLbl: TLabel
      Left = 3
      Top = 3
      Width = 29
      Height = 20
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
end
