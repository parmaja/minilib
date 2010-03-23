object DesignerForm: TDesignerForm
  Left = 0
  Top = 0
  Caption = 'Designer'
  ClientHeight = 254
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutsList: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 254
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 18
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 121
    Top = 0
    Width = 405
    Height = 254
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 359
    DesignSize = (
      405
      254)
    object SectionsListBox: TComboBox
      Left = 192
      Top = 0
      Width = 215
      Height = 21
      AutoCloseUp = True
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      ExplicitLeft = 144
    end
    object ListBox2: TListBox
      Left = 0
      Top = 24
      Width = 406
      Height = 193
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      ExplicitWidth = 358
    end
  end
end
