object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 305
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 23
    Width = 48
    Height = 13
    Caption = 'Hejri Date'
  end
  object Label2: TLabel
    Left = 11
    Top = 78
    Width = 65
    Height = 13
    Caption = 'Gregory Date'
  end
  object Edit1: TEdit
    Left = 83
    Top = 20
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object GregEdit: TEdit
    Left = 83
    Top = 75
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 80
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = Button1Click
  end
end
