object Form4: TForm4
  Left = 383
  Top = 74
  Caption = 'Form4'
  ClientHeight = 592
  ClientWidth = 965
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    965
    592)
  TextHeight = 13
  object Button2: TButton
    Left = 882
    Top = 7
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Download'
    TabOrder = 0
    OnClick = Button2Click
    ExplicitLeft = 590
  end
  object HostEdit: TEdit
    Left = 8
    Top = 8
    Width = 868
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'http://localhost:8080/doc/wallpaper.jpg'
    ExplicitWidth = 576
  end
  object Panel1: TPanel
    Left = 8
    Top = 36
    Width = 949
    Height = 548
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 657
    ExplicitHeight = 336
    object Image1: TImage
      Left = 319
      Top = 0
      Width = 630
      Height = 548
      Align = alClient
      Center = True
      ExplicitLeft = 311
      ExplicitWidth = 349
      ExplicitHeight = 336
    end
    object Splitter1: TSplitter
      Left = 305
      Top = 0
      Width = 14
      Height = 548
      ResizeStyle = rsUpdate
      ExplicitLeft = 311
      ExplicitHeight = 336
    end
    object LogEdit: TMemo
      Left = 0
      Top = 0
      Width = 305
      Height = 548
      Align = alLeft
      TabOrder = 0
      ExplicitLeft = 120
      ExplicitTop = -1
      ExplicitHeight = 336
    end
  end
end
