object Main: TMain
  Left = 165
  Top = 169
  Width = 556
  Height = 224
  BiDiMode = bdRightToLeft
  Caption = 'Mini Web Server'
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
    548
    197)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 390
    Top = 53
    Width = 78
    Height = 18
  end
  object Label1: TLabel
    Left = 7
    Top = 8
    Width = 23
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Root'
  end
  object Label2: TLabel
    Left = 15
    Top = 32
    Width = 19
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Port'
  end
  object NumberOfThreadsLbl: TLabel
    Left = 295
    Top = 55
    Width = 90
    Height = 13
    Caption = 'Connections Count'
  end
  object NumberOfThreads: TLabel
    Left = 397
    Top = 55
    Width = 65
    Height = 13
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = '0'
    Color = clWhite
    ParentBiDiMode = False
    ParentColor = False
    Transparent = True
  end
  object Memo: TMemo
    Left = 0
    Top = 90
    Width = 548
    Height = 106
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StartBtn: TButton
    Left = 484
    Top = 6
    Width = 60
    Height = 22
    Caption = 'Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object RootEdit: TEdit
    Left = 46
    Top = 6
    Width = 431
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 2
    Text = 'c:\offline\'
  end
  object StopBtn: TButton
    Left = 484
    Top = 30
    Width = 60
    Height = 22
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = StopBtnClick
  end
  object VirtualDomainsChk: TCheckBox
    Left = 4
    Top = 52
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Virtual Domain'
    TabOrder = 4
  end
  object PortEdit: TEdit
    Left = 46
    Top = 29
    Width = 287
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 5
    Text = '81'
  end
  object StayOnTopChk: TCheckBox
    Left = 4
    Top = 69
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Stay on top'
    TabOrder = 6
    OnClick = StayOnTopChkClick
  end
  object WebServer: TmnHttpServer
    Port = '81'
    Address = '0.0.0.0'
    OnBeforeOpen = WebServerBeforeOpen
    OnAfterClose = WebServerAfterClose
    OnLog = WebServerLog
    OnChanged = WebServerChanged
    VirtualDomains = False
    DefaultDocument.Strings = (
      'index.html'
      'index.htm'
      'default.html'
      'default.htm')
    Left = 359
    Top = 88
  end
end
