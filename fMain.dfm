object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Formatter'
  ClientHeight = 607
  ClientWidth = 1128
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
    Width = 935
    Height = 492
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabResult
    Align = alClient
    TabOrder = 0
    OnChange = UpdateRequired
    ExplicitWidth = 736
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ExplicitWidth = 728
      object edTokenizer: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 917
        Height = 454
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 718
      end
    end
    object tabParser: TTabSheet
      Caption = #1057#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ImageIndex = 1
      ExplicitWidth = 728
      object treeParser: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 917
        Height = 454
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Indent = 19
        TabOrder = 0
        ExplicitWidth = 718
      end
    end
    object tabResult: TTabSheet
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090
      ImageIndex = 2
      ExplicitWidth = 728
      object edResult: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 917
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
        ExplicitWidth = 718
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
      ExplicitLeft = 7
      ExplicitTop = 15
      ExplicitWidth = 176
      ExplicitHeight = 36
      object tabSrc: TTabSheet
        Caption = #1048#1089#1093#1086#1076#1085#1080#1082
        ExplicitWidth = 168
        ExplicitHeight = 8
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
          ExplicitWidth = 158
          ExplicitHeight = 0
        end
      end
    end
  end
  object pgSettings: TPageControl
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 1118
    Height = 100
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabSettings
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 919
    object tabSettings: TTabSheet
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ExplicitTop = 27
      ExplicitWidth = 911
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
      object GroupBox1: TGroupBox
        Left = 310
        Top = 3
        Width = 561
        Height = 58
        Caption = '  '#1042#1099#1088#1072#1074#1085#1080#1074#1072#1090#1100'  '
        TabOrder = 2
        object checkAlignSubroutineParams: TCheckBox
          Left = 13
          Top = 26
          Width = 148
          Height = 17
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1087#1086#1076#1087#1088#1086#1075#1088#1072#1084#1084
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdateRequired
        end
        object checkAlignVariables: TCheckBox
          Left = 178
          Top = 26
          Width = 89
          Height = 17
          Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1099#1077
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = UpdateRequired
        end
        object checkAlignCallArguments: TCheckBox
          Left = 273
          Top = 26
          Width = 131
          Height = 17
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1074' '#1074#1099#1079#1086#1074#1072#1093
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = UpdateRequired
        end
        object checkAlignCommentInsert: TCheckBox
          Left = 418
          Top = 26
          Width = 131
          Height = 17
          Caption = #1050#1086#1084#1084#1077#1085#1090#1072#1088#1080#1080' '#1074' INSERT'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = UpdateRequired
        end
      end
      object GroupBox2: TGroupBox
        Left = 877
        Top = 3
        Width = 124
        Height = 58
        Caption = '  '#1044#1086#1087'. '#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1080'  '
        TabOrder = 3
        object checkCommentInsert: TCheckBox
          Left = 13
          Top = 26
          Width = 83
          Height = 17
          Caption = #1044#1083#1103' INSERT'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdateRequired
        end
      end
    end
  end
end
