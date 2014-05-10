object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Crypto'
  ClientHeight = 350
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    664
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameLbl: TLabel
    Left = 19
    Top = 78
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object EncFileNameLbl: TLabel
    Left = 19
    Top = 110
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object DecFileNameLbl: TLabel
    Left = 19
    Top = 134
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object SrcTextLbl: TLabel
    Left = 7
    Top = 168
    Width = 58
    Height = 13
    Caption = 'Source Text'
  end
  object EncTextLbl: TLabel
    Left = 3
    Top = 195
    Width = 62
    Height = 13
    Caption = 'Encrypt Text'
  end
  object DecTextLbl: TLabel
    Left = 2
    Top = 222
    Width = 63
    Height = 13
    Caption = 'Decrypt Text'
  end
  object MethodLbl: TLabel
    Left = 29
    Top = 14
    Width = 36
    Height = 13
    Caption = 'Method'
  end
  object ExMethodLbl: TLabel
    Left = 17
    Top = 46
    Width = 48
    Height = 13
    Caption = 'ExMethod'
  end
  object SrcEdit: TEdit
    Left = 72
    Top = 165
    Width = 588
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = SrcEditChange
  end
  object DecEdit: TEdit
    Left = 72
    Top = 219
    Width = 588
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
  end
  object EncEdit: TEdit
    Left = 72
    Top = 192
    Width = 588
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 6
  end
  object TestReadBtn: TButton
    Left = 585
    Top = 102
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test Read'
    TabOrder = 3
    OnClick = TestReadBtnClick
  end
  object TestWriteBtn: TButton
    Left = 585
    Top = 133
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test Write'
    TabOrder = 4
    OnClick = TestWriteBtnClick
  end
  object FileNameEdit: TEdit
    Left = 72
    Top = 75
    Width = 544
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'FileNameEdit'
  end
  object SelectFileBtn: TButton
    Left = 622
    Top = 75
    Width = 38
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = SelectFileBtnClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 331
    Width = 664
    Height = 19
    Panels = <
      item
        Width = 260
      end
      item
        Width = 50
      end>
  end
  object MethodBox: TComboBox
    Left = 72
    Top = 8
    Width = 209
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnClick = MethodBoxClick
  end
  object LogBox: TListBox
    Left = 0
    Top = 246
    Width = 664
    Height = 85
    Align = alBottom
    ItemHeight = 13
    TabOrder = 9
  end
  object TestZLibBtn: TButton
    Left = 489
    Top = 102
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test ZLib'
    TabOrder = 10
    OnClick = TestZLibBtnClick
  end
  object TestZLibBufferBtn: TButton
    Left = 489
    Top = 133
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test ZLib Buff'
    TabOrder = 11
    OnClick = TestZLibBufferBtnClick
  end
  object TestExCipherBtn: TButton
    Left = 312
    Top = 134
    Width = 75
    Height = 25
    Caption = 'Test Ex'
    TabOrder = 12
    OnClick = TestExCipherBtnClick
  end
  object TestExWriteBtn: TButton
    Left = 393
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test ExWrite'
    TabOrder = 13
    OnClick = TestExWriteBtnClick
  end
  object TextExReadBtn: TButton
    Left = 393
    Top = 102
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test ExRead'
    TabOrder = 14
    OnClick = TextExReadBtnClick
  end
  object ExMethodBox: TComboBox
    Left = 72
    Top = 40
    Width = 209
    Height = 21
    Style = csDropDownList
    TabOrder = 15
    OnClick = MethodBoxClick
  end
end
