object Form1: TForm1
  Left = 232
  Top = 129
  Width = 292
  Height = 187
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 194
    Top = 93
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object mnHttpServer1: TmnHttpServer
    Port = '81'
    Address = '0.0.0.0'
    AutoOpen = False
    DocumentRoot = 'c:\httpd\music_en'
    MultiDomain = False
    DefaultDocument.Strings = (
      'index.html'
      'default.html'
      'index.htm'
      'default.htm')
    Left = 55
    Top = 25
  end
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    Port = 80
    Left = 132
    Top = 79
  end
end
