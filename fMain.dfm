object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Formatter'
  ClientHeight = 792
  ClientWidth = 929
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMinimized
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object edSrc: TMemo
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 471
    Height = 782
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    OnChange = edSrcChange
    ExplicitLeft = 0
    ExplicitTop = 430
    ExplicitHeight = 362
  end
  object Pages: TPageControl
    Left = 481
    Top = 0
    Width = 448
    Height = 792
    ActivePage = tabParser
    Align = alClient
    TabOrder = 1
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ExplicitWidth = 281
      ExplicitHeight = 165
      object edTokenizer: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 430
        Height = 754
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ReadOnly = True
        TabOrder = 0
        ExplicitLeft = -83
        ExplicitTop = -347
        ExplicitWidth = 364
        ExplicitHeight = 512
      end
    end
    object tabParser: TTabSheet
      Caption = #1057#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ImageIndex = 1
      object treeParser: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 430
        Height = 754
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Indent = 19
        TabOrder = 0
        ExplicitLeft = 319
        ExplicitTop = 265
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
  end
end
