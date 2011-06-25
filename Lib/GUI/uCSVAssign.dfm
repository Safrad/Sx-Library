object fFormats: TfFormats
  Left = 550
  Top = 280
  Width = 294
  Height = 117
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  Caption = 'Assign of Comma Separated Values'
  Color = clBtnFace
  Constraints.MaxWidth = 294
  Constraints.MinHeight = 64
  Constraints.MinWidth = 294
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object LabelType: TDLabel
    Left = 112
    Top = 8
    Width = 65
    Height = 17
    Caption = 'Type'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
  end
  object LabelFunction: TDLabel
    Left = 184
    Top = 8
    Width = 89
    Height = 17
    Caption = 'Function'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
  end
  object LabelName: TDLabel
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Name'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
  end
  object ButtonOK: TDButton
    Left = 8
    Top = 56
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TDButton
    Left = 96
    Top = 56
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
