object fInfoWindow: TfInfoWindow
  Left = 776
  Top = 404
  AlphaBlend = True
  AlphaBlendValue = 191
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'InfoWindow'
  ClientHeight = 156
  ClientWidth = 196
  Color = clWindow
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 196
    Height = 156
    Align = alClient
  end
  object LabelText: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = '&LabelText'
    Color = clInfoBk
    ParentColor = False
    Transparent = True
    WordWrap = True
    OnMouseDown = LabelTextMouseDown
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 40
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end
