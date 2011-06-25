object fAbout: TfAbout
  Left = 413
  Top = 263
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 424
  ClientWidth = 336
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 200
    Width = 320
    Height = 180
    Shape = bsFrame
  end
  object ButtonOk: TDButton
    Left = 248
    Top = 392
    Width = 81
    Height = 25
    Cancel = True
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
    OnClick = ButtonOkClick
  end
  object ButtonSysInfo1: TDButton
    Left = 64
    Top = 392
    Width = 81
    Height = 25
    Caption = '&System Info...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonSysInfo1Click
  end
  object ButtonMemoryStatus: TDButton
    Left = 152
    Top = 392
    Width = 89
    Height = 25
    Caption = '&Mem. Status...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonMemoryStatusClick
  end
  object ImageAbout: TDImage
    Left = 8
    Top = 8
    Width = 320
    Height = 180
    Zoom = 1.000000000000000000
    OnFill = ImageAboutFill
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    TabStop = False
    OnMouseDown = ImageAboutMouseDown
    OnMouseMove = ImageAboutMouseMove
  end
  object DViewAbout: TDView
    Left = 10
    Top = 202
    Width = 316
    Height = 176
    Zoom = 1.000000000000000000
    TabOrder = 2
    OnGetData = DViewAboutGetData
  end
  object ButtonStat: TDButton
    Left = 8
    Top = 392
    Width = 49
    Height = 25
    Caption = '&Stat.'
    TabOrder = 3
    OnClick = ButtonStatClick
    AutoChange = True
  end
  object Timer1: TDTimer
    ActiveOnly = True
    Enabled = False
    Interval = 25
    EventStep = esFrequency
    OnTimer = DTimer1Timer
    Left = 24
    Top = 24
  end
end
