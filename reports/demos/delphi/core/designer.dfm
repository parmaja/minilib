object DesignerForm: TDesignerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Designer'
  ClientHeight = 271
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    554
    271)
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutsList: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 271
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 18
    TabOrder = 0
    OnDblClick = LayoutsListDblClick
  end
  object SaveBtn: TButton
    Left = 127
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 1
    OnClick = SaveBtnClick
    ExplicitTop = 223
  end
  object CellsListBox: TListBox
    Left = 121
    Top = 24
    Width = 392
    Height = 177
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 2
    OnClick = CellsListBoxClick
  end
  object SectionsListBox: TComboBox
    Left = 123
    Top = 0
    Width = 215
    Height = 21
    AutoCloseUp = True
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 0
    TabOrder = 3
    OnClick = SectionsListBoxClick
  end
  object DeleteBtn: TButton
    Left = 519
    Top = 24
    Width = 34
    Height = 25
    Caption = 'X'
    TabOrder = 4
    OnClick = DeleteBtnClick
  end
  object WidthEdit: TLabeledEdit
    Left = 160
    Top = 204
    Width = 121
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'Width'
    LabelPosition = lpLeft
    TabOrder = 5
    OnChange = WidthEditChange
  end
end
