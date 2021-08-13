object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Formatter'
  ClientHeight = 629
  ClientWidth = 1175
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
    Left = 926
    Top = 0
    Height = 629
    ExplicitLeft = 349
    ExplicitHeight = 501
  end
  object pgDest: TPageControl
    AlignWithMargins = True
    Left = 929
    Top = 0
    Width = 241
    Height = 624
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabResult
    Align = alClient
    TabOrder = 2
    OnChange = pgDestChange
    object tabTokenizer: TTabSheet
      Caption = #1051#1077#1082#1089#1080#1095#1077#1089#1082#1080#1081' '#1072#1085#1072#1083#1080#1079
      object edTokenizer: TListBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 223
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
        Width = 223
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
        Width = 223
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
        Width = 223
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
        Width = 223
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
        Width = 223
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
    object tabCompareAutoTestResult: TTabSheet
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1072#1074#1090#1086#1090#1077#1089#1090#1072
      ImageIndex = 5
      object edCompareAutoTestResult: TMemo
        Left = 0
        Top = 0
        Width = 233
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
  inline frSettings: TFrameSettings
    Left = 0
    Top = 0
    Width = 326
    Height = 629
    Align = alLeft
    TabOrder = 0
    Visible = False
    ExplicitHeight = 629
  end
  object panSrc: TPanel
    Left = 326
    Top = 0
    Width = 600
    Height = 629
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object pgSrc: TPageControl
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 595
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
          Top = 374
          Width = 587
          Height = 4
          Cursor = crVSplit
          Align = alBottom
          Visible = False
          ExplicitTop = 0
          ExplicitWidth = 337
        end
        object edSrc: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 577
          Height = 364
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
          Width = 577
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
        object grpCheckbox: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 381
          Width = 581
          Height = 25
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object checkShowDebugInfo: TCheckBox
            Left = 351
            Top = 0
            Width = 230
            Height = 25
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 0
            Align = alRight
            Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1085#1077#1083#1100' '#1086#1090#1083#1072#1076#1086#1095#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
            TabOrder = 1
            OnClick = checkShowDebugInfoClick
            ExplicitLeft = 350
            ExplicitTop = 1
            ExplicitHeight = 23
          end
          object checkShowSettings: TCheckBox
            Left = 0
            Top = 0
            Width = 125
            Height = 25
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 0
            Align = alLeft
            Caption = #1054#1090#1082#1088#1099#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
            TabOrder = 0
            OnClick = checkShowSettingsClick
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitHeight = 23
          end
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
