object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Formatter'
  ClientHeight = 629
  ClientWidth = 986
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
    Left = 350
    Top = 0
    Height = 629
    ExplicitLeft = 349
    ExplicitHeight = 501
  end
  object pgDest: TPageControl
    AlignWithMargins = True
    Left = 353
    Top = 0
    Width = 628
    Height = 624
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabResult
    Align = alClient
    TabOrder = 0
    OnChange = pgDestChange
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      object edTokenizer: TListBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 586
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = edTokenizerClick
      end
    end
    object tabParser: TTabSheet
      Caption = #1057#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      ImageIndex = 1
      object treeParser: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 27
        Width = 610
        Height = 564
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = treeParserChange
      end
      object checkShowTransparent: TCheckBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 17
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 0
        Align = alTop
        Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1090#1077#1093#1085#1080#1095#1077#1089#1082#1080#1077' '#1091#1079#1083#1099
        TabOrder = 1
        OnClick = checkShowTransparentClick
      end
    end
    object tabResult: TTabSheet
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090
      ImageIndex = 2
      object edResult: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 586
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
        HideSelection = False
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabAlarmToken: TTabSheet
      Caption = '>>> '#1058#1056#1045#1042#1054#1043#1040' <<<'
      ImageIndex = 3
      object edAlarmToken: TListBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 586
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = edAlarmTokenClick
      end
    end
    object tabAlarmStatement: TTabSheet
      Caption = '>>> '#1058#1056#1045#1042#1054#1043#1040' <<<'
      ImageIndex = 3
      object edAlarmStatement: TListBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 586
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = edAlarmStatementClick
      end
    end
    object tabStats: TTabSheet
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      ImageIndex = 6
      object edStats: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 610
        Height = 586
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
        HideSelection = False
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabCompareAutoTestResult: TTabSheet
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1072#1074#1090#1086#1090#1077#1089#1090#1072
      ImageIndex = 5
      object edCompareAutoTestResult: TMemo
        Left = 0
        Top = 0
        Width = 620
        Height = 596
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = edCompareAutoTestResultChange
      end
    end
  end
  object panSrc: TPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 629
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object pgSrc: TPageControl
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 345
      Height = 624
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      ActivePage = tabSrc
      Align = alClient
      TabOrder = 0
      object tabSrc: TTabSheet
        Caption = #1048#1089#1093#1086#1076#1085#1080#1082
        object spDebugInfo: TSplitter
          Left = 0
          Top = 405
          Width = 337
          Height = 4
          Cursor = crVSplit
          Align = alBottom
          Visible = False
          ExplicitTop = 0
        end
        object edSrc: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 327
          Height = 373
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
          HideSelection = False
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
          OnChange = UpdateRequired
        end
        object edDebugInfo: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 414
          Width = 327
          Height = 177
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          HideSelection = False
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          Visible = False
        end
        object checkShowDebugInfo: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 388
          Width = 327
          Height = 17
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 0
          Align = alBottom
          Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1085#1077#1083#1100' '#1086#1090#1083#1072#1076#1086#1095#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
          TabOrder = 2
          OnClick = checkShowDebugInfoClick
        end
      end
      object tabSettings: TTabSheet
        Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
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
        object Label3: TLabel
          Left = 3
          Top = 124
          Width = 219
          Height = 13
          Caption = #1057#1086#1087#1086#1089#1090#1072#1074#1083#1103#1090#1100' '#1087#1086#1083#1103' '#1074#1099#1088#1072#1078#1077#1085#1080#1103#1084' '#1085#1072#1095#1080#1085#1072#1103' '#1089
        end
        object Label4: TLabel
          Left = 3
          Top = 453
          Width = 233
          Height = 13
          Caption = #1064#1080#1088#1080#1085#1072' '#1074#1099#1088#1072#1078#1077#1085#1080#1081' '#1087#1086' '#1074#1086#1079#1084#1086#1078#1085#1086#1089#1090#1080' '#1085#1077' '#1073#1086#1083#1077#1077
        end
        object Label5: TLabel
          Left = 180
          Top = 68
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Caption = #1048#1084#1077#1085#1086#1074#1072#1085#1085#1099#1093
        end
        object Label6: TLabel
          Left = 181
          Top = 96
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = #1055#1086#1079#1080#1094#1080#1086#1085#1085#1099#1093
        end
        object edDeclarationSingleLineParamLimit: TSpinEdit
          Left = 269
          Top = 12
          Width = 46
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 0
          Value = 1
          OnChange = UpdateRequired
        end
        object edNamedArgumentSingleLineParamLimit: TSpinEdit
          Left = 267
          Top = 65
          Width = 46
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 1
          Value = 3
          OnChange = UpdateRequired
        end
        object GroupBox1: TGroupBox
          Left = 3
          Top = 147
          Width = 153
          Height = 219
          Caption = '  '#1042#1099#1088#1072#1074#1085#1080#1074#1072#1090#1100'  '
          TabOrder = 2
          object checkAlignFields: TCheckBox
            Left = 13
            Top = 26
            Width = 128
            Height = 17
            Caption = #1055#1086#1083#1103
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = UpdateRequired
          end
          object checkAlignVariables: TCheckBox
            Left = 13
            Top = 74
            Width = 128
            Height = 17
            Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1099#1077
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = UpdateRequired
          end
          object checkAlignSpecialComments: TCheckBox
            Left = 13
            Top = 120
            Width = 128
            Height = 17
            Caption = #1057#1086#1087#1086#1089#1090#1072#1074#1083#1077#1085#1080#1103
            Checked = True
            State = cbChecked
            TabOrder = 2
            OnClick = UpdateRequired
          end
          object checkAlignCommands: TCheckBox
            Left = 13
            Top = 143
            Width = 128
            Height = 17
            Caption = #1050#1086#1084#1072#1085#1076#1099
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = UpdateRequired
          end
          object checkAlignExpressions: TCheckBox
            Left = 13
            Top = 97
            Width = 128
            Height = 17
            Caption = #1042#1099#1088#1072#1078#1077#1085#1080#1103
            Checked = True
            State = cbChecked
            TabOrder = 4
            OnClick = UpdateRequired
          end
          object checkAlignColumns: TCheckBox
            Left = 13
            Top = 49
            Width = 128
            Height = 17
            Caption = #1050#1086#1083#1086#1085#1082#1080
            Checked = True
            State = cbChecked
            TabOrder = 5
            OnClick = UpdateRequired
          end
          object checkUseSpace: TCheckBox
            Left = 13
            Top = 189
            Width = 128
            Height = 17
            Caption = #1047#1072#1087#1086#1083#1085#1103#1103' '#1087#1091#1089#1090#1086#1090#1099
            Checked = True
            State = cbChecked
            TabOrder = 6
            OnClick = UpdateRequired
          end
          object checkAlignRightComments: TCheckBox
            Left = 13
            Top = 166
            Width = 140
            Height = 17
            Caption = #1057#1090#1088#1086#1095#1085#1099#1077' '#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1080
            Checked = True
            State = cbChecked
            TabOrder = 7
            OnClick = UpdateRequired
          end
        end
        object GroupBox3: TGroupBox
          Left = 162
          Top = 147
          Width = 153
          Height = 80
          Caption = '  '#1047#1072#1084#1077#1085#1103#1090#1100'  '
          TabOrder = 3
          object checkReplaceDefault: TCheckBox
            Left = 13
            Top = 26
            Width = 128
            Height = 17
            Caption = 'default '#1085#1072' :='
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = UpdateRequired
          end
          object checkReplaceAsIs: TCheckBox
            Left = 13
            Top = 49
            Width = 128
            Height = 17
            Caption = 'as '#1085#1072' is'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = UpdateRequired
          end
        end
        object edMatchParamLimit: TSpinEdit
          Left = 267
          Top = 121
          Width = 46
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 4
          Value = 3
          OnChange = UpdateRequired
        end
        object edPreferredExpressionLength: TSpinEdit
          Left = 267
          Top = 450
          Width = 46
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 5
          Value = 60
          OnChange = UpdateRequired
        end
        object GroupBox2: TGroupBox
          Left = 162
          Top = 233
          Width = 153
          Height = 51
          Caption = ' '#1044#1086#1073#1072#1074#1083#1103#1090#1100' '
          TabOrder = 6
          object checkAddInAccessSpecificator: TCheckBox
            Left = 13
            Top = 22
            Width = 128
            Height = 17
            Caption = 'in '#1074' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = UpdateRequired
          end
        end
        object edPositionalArgumentSingleLineParamLimit: TSpinEdit
          Left = 267
          Top = 93
          Width = 46
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 7
          Value = 3
          OnChange = UpdateRequired
        end
        object GroupBox4: TGroupBox
          Left = 162
          Top = 347
          Width = 153
          Height = 91
          Caption = ' '#1042' '#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1103#1093' '
          TabOrder = 8
          object checkChangeCommentType: TCheckBox
            Left = 13
            Top = 26
            Width = 128
            Height = 26
            Caption = #1052#1077#1085#1103#1090#1100' '#1090#1080#1087
            Checked = True
            State = cbChecked
            TabOrder = 0
            WordWrap = True
            OnClick = UpdateRequired
          end
          object checkCommentCorrectSpaces: TCheckBox
            Left = 13
            Top = 49
            Width = 128
            Height = 26
            Caption = #1048#1089#1087#1088#1072#1074#1083#1103#1090#1100' '#1087#1088#1086#1073#1077#1083#1099
            Checked = True
            State = cbChecked
            TabOrder = 1
            WordWrap = True
            OnClick = UpdateRequired
          end
        end
        object GroupBox5: TGroupBox
          Left = 162
          Top = 290
          Width = 153
          Height = 51
          Caption = #1059#1073#1080#1088#1072#1090#1100
          TabOrder = 9
          object checkRemovePasswords: TCheckBox
            Left = 13
            Top = 22
            Width = 128
            Height = 17
            Caption = #1087#1072#1088#1086#1083#1080' '#1080#1079' connect'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = UpdateRequired
          end
        end
        object checkLongOperands: TCheckBox
          Left = 16
          Top = 387
          Width = 128
          Height = 28
          Caption = #1059#1095#1080#1090#1099#1074#1072#1090#1100' '#1076#1083#1080#1085#1085#1099#1077' '#1086#1087#1077#1088#1072#1085#1076#1099
          Checked = True
          State = cbChecked
          TabOrder = 10
          WordWrap = True
          OnClick = UpdateRequired
        end
      end
    end
  end
  object tmMemo: TTimer
    Interval = 20
    OnTimer = tmMemoTimer
    Left = 100
    Top = 105
  end
end
