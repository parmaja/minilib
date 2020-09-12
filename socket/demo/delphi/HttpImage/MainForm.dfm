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
  object Image1: TImage
    Left = 360
    Top = 104
    Width = 241
    Height = 209
  end
  object Button2: TButton
    Left = 550
    Top = 7
    Width = 75
    Height = 23
    Caption = 'Test'
    TabOrder = 0
    OnClick = Button2Click
  end
  object HostEdit: TEdit
    Left = 8
    Top = 8
    Width = 537
    Height = 21
    TabOrder = 1
    Text = 'www.google.com'
  end
  object Button1: TButton
    Left = 8
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 112
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
  end
  object LogEdit: TMemo
    Left = 8
    Top = 104
    Width = 289
    Height = 269
    Lines.Strings = (
      'LogEdit')
    TabOrder = 4
  end
end
