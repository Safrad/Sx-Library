object fStyle: TfStyle
  Left = 246
  Top = 176
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Custom Graphics Style'
  ClientHeight = 179
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxStyles: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxStylesChange
  end
  object ButtonOk: TDButton
    Left = 160
    Top = 144
    Width = 73
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
    TabOrder = 1
  end
  object ButtonCancel: TDButton
    Left = 248
    Top = 144
    Width = 73
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
    TabOrder = 2
  end
  object ButtonApply: TDButton
    Left = 8
    Top = 144
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
end
