object fLogo: TfLogo
  Left = 270
  Top = 210
  Cursor = crAppStart
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 384
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseMove = ImageLogoMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TDTimer
    ActiveOnly = False
    Enabled = False
    Interval = 1000
    EventStep = esInterval
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
end
