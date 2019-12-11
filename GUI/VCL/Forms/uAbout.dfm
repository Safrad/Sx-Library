object fAbout: TfAbout
  Left = 640
  Top = 264
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  Caption = 'About'
  ClientHeight = 441
  ClientWidth = 328
  Color = clBtnFace
  Constraints.MinHeight = 451
  Constraints.MinWidth = 344
  ParentFont = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ImageAbout: TDImage
    Left = 8
    Top = 8
    Width = 320
    Height = 180
    Zoom = 1.000000000000000000
    DisplayMode = dmCustom
    OnFill = ImageAboutFill
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    OnMouseDown = ImageAboutMouseDown
    OnKeyDown = FormKeyDown
  end
  object DViewAbout: TDView
    Left = 8
    Top = 216
    Width = 320
    Height = 201
    Zoom = 1.000000000000000000
    EnableZoom = True
    DisplayMode = dmCustom
    TabOrder = 4
    OnKeyDown = FormKeyDown
    OnGetData = DViewAboutGetData
    OnCellClick = DViewAboutCellClick
  end
  object ButtonOk: TDButton
    Left = 248
    Top = 424
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = ButtonOkClick
    OnKeyDown = FormKeyDown
  end
  object ButtonSysInfo: TDButton
    Left = 8
    Top = 424
    Width = 81
    Height = 25
    Caption = '&System Info...'
    TabOrder = 5
    OnClick = ButtonSysInfoClick
    OnKeyDown = FormKeyDown
  end
  object ButtonStatistics: TDButton
    Tag = 1
    Left = 88
    Top = 200
    Width = 73
    Height = 17
    Caption = '&Statistics'
    TabOrder = 3
    OnClick = ButtonXClick
    OnKeyDown = FormKeyDown
  end
  object ButtonVersionInfo: TDButton
    Left = 8
    Top = 200
    Width = 73
    Height = 17
    Caption = '&Version Info'
    TabOrder = 2
    OnClick = ButtonXClick
    OnKeyDown = FormKeyDown
  end
  object ButtonBuildParams: TDButton
    Left = 168
    Top = 424
    Width = 65
    Height = 25
    Caption = '&Build Params'
    TabOrder = 6
    OnClick = ButtonBuildParamsClick
    OnKeyDown = FormKeyDown
  end
  object TimerFlash: TDTimer
    ActiveOnly = True
    Enabled = False
    Interval = 25
    IntervalInSeconds = 1.000000000000000000
    EventStep = esFrequency
    OnTimer = DTimerFlashTimer
    Left = 24
    Top = 24
  end
end
