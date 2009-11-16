object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Crypto'
  ClientHeight = 266
  ClientWidth = 585
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
    585
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameLbl: TLabel
    Left = 19
    Top = 48
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object EncFileNameLbl: TLabel
    Left = 19
    Top = 80
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object DecFileNameLbl: TLabel
    Left = 19
    Top = 104
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object SrcTextLbl: TLabel
    Left = 7
    Top = 138
    Width = 58
    Height = 13
    Caption = 'Source Text'
  end
  object EncTextLbl: TLabel
    Left = 3
    Top = 165
    Width = 62
    Height = 13
    Caption = 'Encrypt Text'
  end
  object DecTextLbl: TLabel
    Left = 2
    Top = 192
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
  object ResultLbl: TLabel
    Left = 0
    Top = 214
    Width = 585
    Height = 33
    Align = alBottom
    Alignment = taCenter
    Caption = 'Result:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 96
  end
  object SrcEdit: TEdit
    Left = 72
    Top = 135
    Width = 509
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = SrcEditChange
  end
  object DecEdit: TEdit
    Left = 72
    Top = 189
    Width = 509
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
  end
  object EncEdit: TEdit
    Left = 72
    Top = 162
    Width = 509
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 6
  end
  object TestReadBtn: TButton
    Left = 506
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test Read'
    TabOrder = 3
    OnClick = TestReadBtnClick
  end
  object TestWriteBtn: TButton
    Left = 506
    Top = 103
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test Write'
    TabOrder = 4
    OnClick = TestWriteBtnClick
  end
  object FileNameEdit: TEdit
    Left = 72
    Top = 45
    Width = 465
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'FileNameEdit'
  end
  object SelectFileBtn: TButton
    Left = 543
    Top = 45
    Width = 38
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = SelectFileBtnClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 247
    Width = 585
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 50
      end>
    ExplicitLeft = 296
    ExplicitTop = 136
    ExplicitWidth = 0
  end
  object MethodBox: TComboBox
    Left = 72
    Top = 8
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 0
    OnClick = MethodBoxClick
  end
end
