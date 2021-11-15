object Form1: TForm1
  Left = 191
  Top = 103
  Caption = 'Form1'
  ClientHeight = 314
  ClientWidth = 476
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    476
    314)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 4
    Top = 7
    Width = 75
    Height = 25
    Caption = 'Write XML'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 4
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Manual Read'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 4
    Top = 59
    Width = 75
    Height = 25
    Caption = 'File Read'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Memo: TMemo
    Left = 84
    Top = 5
    Width = 387
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ARABIC_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button4: TButton
    Left = 4
    Top = 292
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 4
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 5
    OnClick = Button5Click
  end
end
