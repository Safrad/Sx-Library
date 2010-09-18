object fScores: TfScores
  Left = 614
  Top = 499
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'High scores'
  ClientHeight = 265
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOk: TDButton
    Left = 304
    Top = 232
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
  end
  object ButtonCancel: TDButton
    Left = 400
    Top = 232
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
  end
  object PanelHigh: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 226
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 2
    object ImageHigh: TDImage
      Left = 0
      Top = 0
      Width = 485
      Height = 222
      DrawFPS = False
      HandScroll = False
      HotTrack = True
      OnFill = ImageHighFill
      Align = alClient
      TabOrder = 0
      TabStop = False
    end
  end
end
