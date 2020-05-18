object Form4: TForm4
  Left = 383
  Top = 74
  Caption = 'Form4'
  ClientHeight = 381
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DocList: TMemo
    Left = 8
    Top = 152
    Width = 617
    Height = 223
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button2: TButton
    Left = 550
    Top = 7
    Width = 75
    Height = 23
    Caption = 'Test'
    TabOrder = 1
    OnClick = Button2Click
  end
  object HeaderList: TMemo
    Left = 8
    Top = 73
    Width = 617
    Height = 73
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object HostEdit: TEdit
    Left = 8
    Top = 8
    Width = 537
    Height = 21
    TabOrder = 3
    Text = 'www.google.com'
  end
  object Button1: TButton
    Left = 8
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 112
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ContentRangeInstanceLength = 0
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 304
    Top = 192
  end
end
