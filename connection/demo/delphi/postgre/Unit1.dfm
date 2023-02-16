object Form1: TForm1
  Left = 877
  Top = 182
  Caption = 'Form1'
  ClientHeight = 428
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 258
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ListBox1: TListBox
    Left = 0
    Top = 308
    Width = 378
    Height = 120
    Align = alBottom
    ItemHeight = 13
    TabOrder = 2
    ExplicitTop = 307
    ExplicitWidth = 374
  end
  object PassEdit: TEdit
    Left = 8
    Top = 37
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object BinaryResultChk: TCheckBox
    Left = 87
    Top = 14
    Width = 97
    Height = 17
    Caption = 'Binary Result'
    TabOrder = 4
  end
  object Button3: TButton
    Left = 258
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 5
    OnClick = Button3Click
  end
  object SynEdit1: TSynEdit
    Left = 0
    Top = 208
    Width = 378
    Height = 98
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 6
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = (
      'SynEdit1')
    FontSmoothing = fsmNone
  end
  object TreeBtn: TButton
    Left = 258
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Tree'
    TabOrder = 7
    OnClick = TreeBtnClick
  end
  object Button4: TButton
    Left = 8
    Top = 73
    Width = 75
    Height = 25
    Caption = 'Read Cursor'
    TabOrder = 8
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Blob'
    TabOrder = 9
    OnClick = Button5Click
  end
end
