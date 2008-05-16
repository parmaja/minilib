object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 318
  ClientWidth = 365
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
    Left = 18
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Canvas'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 19
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Lines'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 12
    Top = 12
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'COM61'
  end
  object ToFileChk: TCheckBox
    Left = 145
    Top = 13
    Width = 97
    Height = 17
    Caption = 'To File'
    TabOrder = 3
  end
  object LowChk: TCheckBox
    Left = 145
    Top = 33
    Width = 97
    Height = 17
    Caption = 'Low Density'
    TabOrder = 4
  end
end
