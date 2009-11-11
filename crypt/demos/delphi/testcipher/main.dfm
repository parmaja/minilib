object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Crypto'
  ClientHeight = 467
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SrcMemo: TMemo
    Left = 24
    Top = 16
    Width = 417
    Height = 145
    Lines.Strings = (
      'testing')
    TabOrder = 0
  end
  object DecryptMemo: TMemo
    Left = 24
    Top = 318
    Width = 417
    Height = 145
    TabOrder = 1
  end
  object EncryptMemo: TMemo
    Left = 24
    Top = 167
    Width = 417
    Height = 145
    TabOrder = 2
  end
  object ExecuteBtn: TButton
    Left = 570
    Top = 16
    Width = 75
    Height = 25
    Caption = 'ExecuteBtn'
    TabOrder = 3
    OnClick = ExecuteBtnClick
  end
end
