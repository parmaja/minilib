object Form1: TForm1
  Left = 770
  Top = 179
  Caption = 'Form1'
  ClientHeight = 328
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
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
    Left = 0
    Top = 64
    Width = 378
    Height = 264
    Align = alBottom
    ItemHeight = 13
    TabOrder = 2
  end
  object PassEdit: TEdit
    Left = 8
    Top = 37
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object BinaryResultChk: TCheckBox
    Left = 95
    Top = 14
    Width = 97
    Height = 17
    Caption = 'Binary Result'
    TabOrder = 4
  end
end
