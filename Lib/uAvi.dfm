object fAvi: TfAvi
  Left = 280
  Top = 100
  BorderStyle = bsNone
  Caption = 'Image'
  ClientHeight = 206
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelAvi: TPanel
    Left = 0
    Top = 0
    Width = 326
    Height = 206
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ImageAvi: TDImage
      Left = 0
      Top = 0
      Width = 326
      Height = 206
      HandScroll = False
      DrawFPS = False
      WaitVBlank = False
      OnFill = ImageAviFill
      Align = alClient
      PopupMenu = PopupMenu1
      TabOrder = 0
      TabStop = False
      OnKeyDown = ImageAviKeyDown
    end
  end
  object DTimer1: TDTimer
    ActiveOnly = False
    Enabled = True
    Interval = 40
    OnTimer = DTimer1Timer
    Left = 10
    Top = 10
  end
  object PopupMenu1: TPopupMenu
    Left = 42
    Top = 10
    object LastPage1: TMenuItem
      Caption = 'Last Page'
      OnClick = LastPage1Click
    end
    object NextPage1: TMenuItem
      Caption = 'Next Page'
      OnClick = NextPage1Click
    end
    object DecSpeed1: TMenuItem
      Caption = 'Dec Speed'
      OnClick = DecSpeed1Click
    end
    object IncSpeed1: TMenuItem
      Caption = 'Inc Speed'
      OnClick = IncSpeed1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ShowFPS1: TMenuItem
      Caption = 'Show FPS'
      OnClick = ShowFPS1Click
    end
    object CustomFPS1: TMenuItem
      Caption = 'Custom FPS...'
      OnClick = CustomFPS1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Caption = 'Close'
      OnClick = Close1Click
    end
  end
end
