object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 366
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    502
    366)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 98
    Top = 6
    Width = 199
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    WordWrap = False
  end
  object Button2: TButton
    Left = 6
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 301
    Top = 5
    Width = 199
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    WordWrap = False
  end
end
