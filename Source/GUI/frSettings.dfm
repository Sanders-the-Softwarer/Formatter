object FrameSettings: TFrameSettings
  Left = 0
  Top = 0
  Width = 326
  Height = 551
  TabOrder = 0
  object Label1: TLabel
    Left = 5
    Top = 45
    Width = 237
    Height = 15
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1086#1074' '#1074' '#1086#1076#1085#1091' '#1089#1090#1088#1086#1082#1091' '#1085#1077' '#1073#1086#1083#1100#1096#1077', '#1095#1077#1084
  end
  object Label2: TLabel
    Left = 5
    Top = 73
    Width = 235
    Height = 15
    Caption = #1040#1088#1075#1091#1084#1077#1085#1090#1086#1074' '#1074' '#1086#1076#1085#1091' '#1089#1090#1088#1086#1082#1091' '#1085#1077' '#1073#1086#1083#1100#1096#1077', '#1095#1077#1084
  end
  object Label3: TLabel
    Left = 3
    Top = 154
    Width = 239
    Height = 15
    Caption = #1057#1086#1087#1086#1089#1090#1072#1074#1083#1103#1090#1100' '#1087#1086#1083#1103' '#1074#1099#1088#1072#1078#1077#1085#1080#1103#1084' '#1085#1072#1095#1080#1085#1072#1103' '#1089
  end
  object Label4: TLabel
    Left = 3
    Top = 524
    Width = 259
    Height = 15
    Caption = #1064#1080#1088#1080#1085#1072' '#1074#1099#1088#1072#1078#1077#1085#1080#1081' '#1087#1086' '#1074#1086#1079#1084#1086#1078#1085#1086#1089#1090#1080' '#1085#1077' '#1073#1086#1083#1077#1077
  end
  object Label5: TLabel
    Left = 170
    Top = 98
    Width = 79
    Height = 15
    Alignment = taRightJustify
    Caption = #1048#1084#1077#1085#1086#1074#1072#1085#1085#1099#1093
  end
  object Label6: TLabel
    Left = 171
    Top = 126
    Width = 78
    Height = 15
    Alignment = taRightJustify
    Caption = #1055#1086#1079#1080#1094#1080#1086#1085#1085#1099#1093
  end
  object btnSaveToFile: TButton
    Left = 240
    Top = 9
    Width = 75
    Height = 25
    Caption = #1042' '#1092#1072#1081#1083
    TabOrder = 1
    OnClick = btnSaveToFileClick
  end
  object edDeclarationSingleLineParamLimit: TSpinEdit
    Left = 269
    Top = 42
    Width = 46
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 2
    Value = 1
    OnChange = UpdateRequired
  end
  object edNamedArgumentSingleLineParamLimit: TSpinEdit
    Left = 267
    Top = 95
    Width = 46
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 3
    Value = 3
    OnChange = UpdateRequired
  end
  object GroupBox1: TGroupBox
    Left = 3
    Top = 177
    Width = 153
    Height = 244
    Caption = '  '#1042#1099#1088#1072#1074#1085#1080#1074#1072#1090#1100'  '
    TabOrder = 6
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
      TabOrder = 2
      OnClick = UpdateRequired
    end
    object checkAlignSpecialComments: TCheckBox
      Left = 13
      Top = 145
      Width = 128
      Height = 17
      Caption = #1057#1086#1087#1086#1089#1090#1072#1074#1083#1077#1085#1080#1103
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = UpdateRequired
    end
    object checkAlignCommands: TCheckBox
      Left = 13
      Top = 168
      Width = 128
      Height = 17
      Caption = #1050#1086#1084#1072#1085#1076#1099
      Checked = True
      State = cbChecked
      TabOrder = 6
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
      TabOrder = 3
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
      TabOrder = 1
      OnClick = UpdateRequired
    end
    object checkUseSpace: TCheckBox
      Left = 13
      Top = 214
      Width = 128
      Height = 17
      Caption = #1047#1072#1087#1086#1083#1085#1103#1103' '#1087#1091#1089#1090#1086#1090#1099
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = UpdateRequired
    end
    object checkAlignRightComments: TCheckBox
      Left = 13
      Top = 191
      Width = 140
      Height = 17
      Caption = #1057#1090#1088#1086#1095#1085#1099#1077' '#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1080
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = UpdateRequired
    end
    object checkAlignFrom: TCheckBox
      Left = 13
      Top = 120
      Width = 128
      Height = 17
      Caption = #1058#1072#1073#1083#1080#1094#1099' '#1074#1086' FROM'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = UpdateRequired
    end
  end
  object GroupBox3: TGroupBox
    Left = 162
    Top = 177
    Width = 153
    Height = 80
    Caption = '  '#1047#1072#1084#1077#1085#1103#1090#1100'  '
    TabOrder = 7
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
    Top = 151
    Width = 46
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 5
    Value = 3
    OnChange = UpdateRequired
  end
  object edPreferredExpressionLength: TSpinEdit
    Left = 267
    Top = 521
    Width = 46
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 13
    Value = 60
    OnChange = UpdateRequired
  end
  object GroupBox2: TGroupBox
    Left = 162
    Top = 263
    Width = 153
    Height = 51
    Caption = ' '#1044#1086#1073#1072#1074#1083#1103#1090#1100' '
    TabOrder = 8
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
    Top = 123
    Width = 46
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 4
    Value = 3
    OnChange = UpdateRequired
  end
  object GroupBox4: TGroupBox
    Left = 162
    Top = 377
    Width = 153
    Height = 91
    Caption = ' '#1042' '#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1103#1093' '
    TabOrder = 10
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
    Top = 320
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
    Left = 175
    Top = 476
    Width = 128
    Height = 28
    Caption = #1059#1095#1080#1090#1099#1074#1072#1090#1100' '#1076#1083#1080#1085#1085#1099#1077' '#1086#1087#1077#1088#1072#1085#1076#1099
    Checked = True
    State = cbChecked
    TabOrder = 12
    WordWrap = True
    OnClick = UpdateRequired
  end
  object GroupBox6: TGroupBox
    Left = 3
    Top = 427
    Width = 153
    Height = 91
    Caption = ' '#1057#1076#1074#1080#1075' '
    TabOrder = 11
    object checkShiftPackageHeader: TCheckBox
      Left = 13
      Top = 26
      Width = 128
      Height = 26
      Caption = #1042' '#1079#1072#1075#1086#1083#1086#1074#1082#1072#1093' '#1087#1072#1082#1077#1090#1086#1074
      Checked = True
      State = cbChecked
      TabOrder = 0
      WordWrap = True
      OnClick = UpdateRequired
    end
    object checkShiftPackageBody: TCheckBox
      Left = 13
      Top = 49
      Width = 128
      Height = 26
      Caption = #1042' '#1090#1077#1083#1072#1093' '#1087#1072#1082#1077#1090#1086#1074
      Checked = True
      State = cbChecked
      TabOrder = 1
      WordWrap = True
      OnClick = UpdateRequired
    end
  end
  object btLoadFromFile: TButton
    Left = 3
    Top = 9
    Width = 75
    Height = 25
    Caption = #1048#1079' '#1092#1072#1081#1083#1072
    TabOrder = 0
    OnClick = btLoadFromFileClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.ini'
    Filter = #1060#1072#1081#1083#1099' '#1085#1072#1089#1090#1088#1086#1077#1082'|*.ini|'#1042#1089#1077' '#1092#1072#1081#1083#1099'|*.*'
    Left = 68
    Top = 99
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.ini'
    Filter = #1060#1072#1081#1083#1099' '#1085#1072#1089#1090#1088#1086#1077#1082'|*.ini|'#1042#1089#1077' '#1092#1072#1081#1083#1099'|*.*'
    Left = 128
    Top = 114
  end
end
