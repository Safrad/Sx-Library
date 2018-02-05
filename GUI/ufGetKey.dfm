object fGetKey: TfGetKey
  Left = 721
  Top = 402
  AlphaBlend = True
  AlphaBlendValue = 224
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Get Key'
  ClientHeight = 36
  ClientWidth = 227
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelGetKey: TLabel
    Left = 0
    Top = 0
    Width = 227
    Height = 36
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 'Please press a key'#8230
    Transparent = True
    Layout = tlCenter
  end
  object Timer: TTimer
    Interval = 40
    OnTimer = TimerTimer
    Left = 200
  end
end
