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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object edSrc: TMemo
    Left = 0
    Top = 0
    Width = 471
    Height = 792
    Align = alLeft
    TabOrder = 0
    OnChange = edSrcChange
  end
  object edDest: TMemo
    Left = 471
    Top = 0
    Width = 458
    Height = 792
    Align = alClient
    ReadOnly = True
    TabOrder = 1
    ExplicitLeft = 5
    ExplicitWidth = 185
  end
end
