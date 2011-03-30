object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 315
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TestSpeedBtn: TButton
    Left = 397
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test Speed'
    TabOrder = 0
    OnClick = TestSpeedBtnClick
  end
  object TestReportBtn: TButton
    Left = 397
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Test Report'
    TabOrder = 1
    OnClick = TestReportBtnClick
  end
  object TestWriteBtn: TButton
    Left = 397
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Test Write'
    TabOrder = 2
    OnClick = TestWriteBtnClick
  end
  object DesignReportBtn: TButton
    Left = 397
    Top = 129
    Width = 75
    Height = 25
    Caption = 'Design Report'
    TabOrder = 3
    OnClick = DesignReportBtnClick
  end
end
