object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1092#1086#1088#1084#1072#1090#1080#1088#1086#1074#1072#1085#1080#1103
  ClientHeight = 593
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inline frSettings: TFrameSettings
    Left = 0
    Top = 0
    Width = 326
    Height = 551
    TabOrder = 0
    inherited edDeclarationSingleLineParamLimit: TSpinEdit
      Left = 267
      ExplicitLeft = 267
    end
  end
  object btOk: TButton
    Left = 3
    Top = 564
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 240
    Top = 564
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
  end
end
