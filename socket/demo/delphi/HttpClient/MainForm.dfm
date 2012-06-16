object Form4: TForm4
  Left = 383
  Top = 74
  Caption = 'Form4'
  ClientHeight = 381
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DocList: TMemo
    Left = 8
    Top = 110
    Width = 617
    Height = 265
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button2: TButton
    Left = 550
    Top = 7
    Width = 75
    Height = 23
    Caption = 'Test'
    TabOrder = 1
    OnClick = Button2Click
  end
  object HeaderList: TMemo
    Left = 8
    Top = 33
    Width = 617
    Height = 73
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object HostEdit: TEdit
    Left = 8
    Top = 8
    Width = 537
    Height = 21
    TabOrder = 3
    Text = 'www.google.com'
  end
end
