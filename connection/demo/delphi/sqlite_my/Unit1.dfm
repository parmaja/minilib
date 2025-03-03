object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 213
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 258
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ListBox1: TListBox
    Left = 173
    Top = 40
    Width = 159
    Height = 166
    ItemHeight = 13
    TabOrder = 2
  end
  object Button3: TButton
    Left = 6
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 6
    Top = 61
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 4
    OnClick = Button4Click
  end
end
