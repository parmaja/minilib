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
  DesignSize = (
    365
    318)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 14
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object PortEdit: TEdit
    Left = 46
    Top = 9
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'COM11'
  end
  object Memo1: TMemo
    Left = 3
    Top = 93
    Width = 358
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 81
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Start Read'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 287
    Top = 66
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 4
    Top = 68
    Width = 279
    Height = 21
    TabOrder = 4
  end
  object Button3: TButton
    Left = 159
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Start Thread'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 247
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 3
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Start Read'
    TabOrder = 7
    OnClick = Button5Click
  end
end
