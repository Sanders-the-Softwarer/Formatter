object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Formatter'
  ClientHeight = 607
  ClientWidth = 929
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMinimized
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object edSrc: TMemo
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 471
    Height = 597
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    OnChange = edSrcChange
  end
  object Pages: TPageControl
    Left = 481
    Top = 0
    Width = 448
    Height = 607
    ActivePage = tabResult
    Align = alClient
    TabOrder = 1
    OnChange = PagesChange
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object edTokenizer: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 430
        Height = 569
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tabParser: TTabSheet
      Caption = #1057#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object treeParser: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 430
        Height = 569
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object tabResult: TTabSheet
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090
      ImageIndex = 2
      object edResult: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 430
        Height = 569
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
