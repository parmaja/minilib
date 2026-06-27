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
  DesignSize = (
    624
    328)
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 20
    Width = 119
    Height = 68
    Lines.Strings = (
      'Value1=Test1'
      'Value2=Test2'
      'Value 3=Test 3'
      'Value4=Test\')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 111
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse '#39'$'#39
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 133
    Top = 20
    Width = 221
    Height = 271
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
      'test $value1,$value2 test'
      'test $value4\$value2'
      'test $value4$value2')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo3: TMemo
    Left = 360
    Top = 20
    Width = 257
    Height = 270
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button2: TButton
    Left = 540
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse'
    TabOrder = 4
    OnClick = Button2Click
  end
  object InEdit: TEdit
    Left = 8
    Top = 297
    Width = 345
    Height = 23
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Text = '$Value1'
  end
  object Button3: TButton
    Left = 8
    Top = 142
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Parse '#39'{#'#39', '#39'}'#39
    TabOrder = 6
    OnClick = Button3Click
  end
  object PrefixEdit1: TEdit
    Left = 359
    Top = 297
    Width = 81
    Height = 23
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    Text = '$'
  end
  object SuffixEdit1: TEdit
    Left = 446
    Top = 297
    Width = 89
    Height = 23
    Anchors = [akLeft, akBottom]
    TabOrder = 8
  end
end
