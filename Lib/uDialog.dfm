object fDialog: TfDialog
  Left = 224
  Top = 125
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 223
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 12
    Top = 12
    Width = 48
    Height = 48
    Transparent = True
  end
  object LabelTimeLeft: TDLabel
    Left = 8
    Top = 88
    Width = 57
    Height = 17
    AutoSize = False
    Caption = 'Time Left:'
    Layout = tlCenter
    FontShadow = 1
    Displ.Enabled = False
    Displ.Format = '88'
    Displ.SizeX = 4
    Displ.SizeY = 4
    Displ.SpaceSX = 2
    Displ.SpaceSY = 2
    Displ.SizeT = 1
    Displ.Spacing = 0
    Displ.ColorA = clRed
    Displ.ColorD = clMaroon
    Displ.Size = 0
    BevelOuter = bvNone
  end
  object Memo: TMemo
    Left = 56
    Top = 12
    Width = 417
    Height = 69
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 0
    WordWrap = False
  end
  object PanelTimeLeft: TDPanel
    Left = 64
    Top = 88
    Width = 57
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    TabOrder = 1
  end
  object CheckBoxA: TCheckBox
    Left = 8
    Top = 200
    Width = 489
    Height = 17
    Caption = 'Do not show again'
    TabOrder = 2
  end
  object Timer1: TDTimer
    ActiveOnly = True
    Enabled = True
    Interval = 1000
    EventStep = esInterval
    OnTimer = Timer1Timer
  end
end
