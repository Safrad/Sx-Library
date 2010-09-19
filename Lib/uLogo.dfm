object fLogo: TfLogo
  Left = 427
  Top = 333
  Cursor = crHourGlass
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  AlphaBlend = True
  AlphaBlendValue = 191
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = ImageLogoMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TDTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
end
