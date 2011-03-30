object Form1: TForm1
  Left = 223
  Top = 192
  Caption = 'Form1'
  ClientHeight = 213
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    405
    213)
  PixelsPerInch = 96
  TextHeight = 13
  object Command: TButton
    Left = 320
    Top = 34
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Send'
    TabOrder = 1
    OnClick = CommandClick
  end
  object Memo: TMemo
    Left = 93
    Top = 7
    Width = 221
    Height = 192
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 320
    Top = 6
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Connect'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 62
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Disconnect'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 10
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = Button3Click
  end
end
