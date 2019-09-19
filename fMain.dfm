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
  object spVert: TSplitter
    Left = 185
    Top = 110
    Height = 497
    ExplicitLeft = 405
    ExplicitTop = 0
    ExplicitHeight = 100
  end
  object pgDest: TPageControl
    AlignWithMargins = True
    Left = 188
    Top = 110
    Width = 736
    Height = 492
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabResult
    Align = alClient
    TabOrder = 0
    OnChange = UpdateRequired
    ExplicitLeft = 481
    ExplicitTop = 0
    ExplicitWidth = 448
    ExplicitHeight = 607
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ExplicitWidth = 440
      ExplicitHeight = 579
      object edTokenizer: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 718
        Height = 454
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 430
        ExplicitHeight = 569
      end
    end
    object tabParser: TTabSheet
      Caption = #1057#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ImageIndex = 1
      ExplicitWidth = 440
      ExplicitHeight = 579
      object treeParser: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 718
        Height = 454
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Indent = 19
        TabOrder = 0
        ExplicitWidth = 430
        ExplicitHeight = 569
      end
    end
    object tabResult: TTabSheet
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090
      ImageIndex = 2
      ExplicitWidth = 440
      ExplicitHeight = 579
      object edResult: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 718
        Height = 454
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
        ExplicitWidth = 430
        ExplicitHeight = 569
      end
    end
  end
  object panSrc: TPanel
    Left = 0
    Top = 110
    Width = 185
    Height = 497
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 0
    ExplicitHeight = 579
    object pgSrc: TPageControl
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 180
      Height = 492
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      ActivePage = tabSrc
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 906
      ExplicitHeight = 67
      object tabSrc: TTabSheet
        Caption = #1048#1089#1093#1086#1076#1085#1080#1082
        ExplicitWidth = 898
        ExplicitHeight = 39
        object edSrc: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 162
          Height = 454
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
          TabOrder = 0
          OnChange = UpdateRequired
          ExplicitWidth = 888
          ExplicitHeight = 29
        end
      end
    end
  end
  object pgSettings: TPageControl
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 919
    Height = 100
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabSettings
    Align = alTop
    TabOrder = 2
    ExplicitLeft = 0
    ExplicitTop = -7
    ExplicitWidth = 929
    object tabSettings: TTabSheet
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ExplicitWidth = 177
      ExplicitHeight = 165
      object Label1: TLabel
        Left = 5
        Top = 15
        Width = 217
        Height = 13
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1086#1074' '#1074' '#1086#1076#1085#1091' '#1089#1090#1088#1086#1082#1091' '#1085#1077' '#1073#1086#1083#1100#1096#1077', '#1095#1077#1084
      end
      object Label2: TLabel
        Left = 5
        Top = 43
        Width = 216
        Height = 13
        Caption = #1040#1088#1075#1091#1084#1077#1085#1090#1086#1074' '#1074' '#1086#1076#1085#1091' '#1089#1090#1088#1086#1082#1091' '#1085#1077' '#1073#1086#1083#1100#1096#1077', '#1095#1077#1084
      end
      object edDeclarationSingleLineParamLimit: TSpinEdit
        Left = 235
        Top = 12
        Width = 46
        Height = 22
        MaxValue = 1000
        MinValue = 0
        TabOrder = 0
        Value = 1
        OnChange = UpdateRequired
      end
      object edArgumentSingleLineParamLimit: TSpinEdit
        Left = 235
        Top = 40
        Width = 46
        Height = 22
        MaxValue = 1000
        MinValue = 0
        TabOrder = 1
        Value = 3
        OnChange = UpdateRequired
      end
    end
  end
end
