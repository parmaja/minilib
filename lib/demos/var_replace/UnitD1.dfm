object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 328
  ClientWidth = 624
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  DesignSize = (
    624
    328)
  TextHeight = 15
  object Memo1: TMemo
    Left = 21
    Top = 20
    Width = 106
    Height = 68
    Lines.Strings = (
      'Value1=Test1'
      'Value2=Test2'
      'Value 3=Test 3')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 295
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse'
    TabOrder = 1
    OnClick = Button1Click
    ExplicitTop = 379
  end
  object Memo2: TMemo
    Left = 133
    Top = 20
    Width = 221
    Height = 237
    Lines.Strings = (
      '$value1$value2'
      '{#value1}{#value2}'
      '$value1'
      '$value1 and $[value 3]'
      '$value1 and $value2[$value 3]'
      '$value1 and $[value 3] and test'
      '$value1 and {#value 3} and test'
      '$Value1,$Value2'
      '$value1$value$value2'
      ' $value1,$value2 '
      'test $value1$value2 test'
      'test $value1,$value2 test')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo3: TMemo
    Left = 360
    Top = 20
    Width = 257
    Height = 237
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button2: TButton
    Left = 285
    Top = 265
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse'
    TabOrder = 4
    OnClick = Button2Click
    ExplicitTop = 349
  end
  object InEdit: TEdit
    Left = 9
    Top = 266
    Width = 270
    Height = 23
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Text = '$Value1'
    ExplicitTop = 350
  end
  object Button3: TButton
    Left = 89
    Top = 295
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse'
    TabOrder = 6
    OnClick = Button3Click
  end
end
