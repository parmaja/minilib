object Form1: TForm1
  Left = 466
  Height = 318
  Top = 320
  Width = 365
  Caption = 'Form1'
  ClientHeight = 318
  ClientWidth = 365
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  LCLVersion = '0.9.31'
  object Label1: TLabel
    Left = 12
    Height = 14
    Top = 14
    Width = 20
    Caption = 'Port'
    ParentColor = False
  end
  object PortEdit: TEdit
    Left = 48
    Height = 23
    Top = 9
    Width = 121
    TabOrder = 0
    Text = 'COM11'
  end
  object Memo1: TMemo
    Left = 3
    Height = 222
    Top = 93
    Width = 358
    Anchors = [akTop, akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 81
    Height = 25
    Top = 36
    Width = 75
    Caption = 'Start Read'
    OnClick = Button1Click
    TabOrder = 2
  end
  object Button2: TButton
    Left = 287
    Height = 25
    Top = 66
    Width = 75
    Caption = 'Write'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 4
    Height = 23
    Top = 68
    Width = 279
    TabOrder = 4
  end
  object Button3: TButton
    Left = 159
    Height = 25
    Top = 36
    Width = 75
    Caption = 'Start Thread'
    OnClick = Button3Click
    TabOrder = 5
  end
  object Button4: TButton
    Left = 247
    Height = 25
    Top = 36
    Width = 75
    Caption = 'Cancel'
    OnClick = Button4Click
    TabOrder = 6
  end
  object Button5: TButton
    Left = 3
    Height = 25
    Top = 36
    Width = 75
    Caption = 'Start Read'
    OnClick = Button5Click
    TabOrder = 7
  end
end