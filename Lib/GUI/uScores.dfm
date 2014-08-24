object fScores: TfScores
  Left = 2560
  Top = 442
  Width = 497
  Height = 292
  Caption = 'High Scores'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOk: TDButton
    Left = 400
    Top = 232
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object DViewHighScores: TDView
    Left = 0
    Top = 0
    Width = 489
    Height = 225
    Zoom = 1.000000000000000000
    Align = alTop
    TabOrder = 1
    OnGetData = DViewHighScoresGetData
  end
end
