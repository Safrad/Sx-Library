object fScores: TfScores
  Left = 2560
  Top = 442
  Caption = 'High Scores'
  ClientHeight = 253
  ClientWidth = 481
  Color = clBtnFace
  ParentFont = True
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
    Width = 481
    Height = 225
    Align = alTop
    Zoom = 1.000000000000000000
    EnableZoom = True
    DisplayMode = dmCustom
    TabOrder = 1
    OnGetData = DViewHighScoresGetData
    ExplicitWidth = 489
  end
end
