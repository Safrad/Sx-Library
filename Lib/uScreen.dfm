object fScreen: TfScreen
  Left = 201
  Top = 107
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Can not assign graphics driver, select driver manually'
  ClientHeight = 57
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxDriver: TComboBox
    Left = 0
    Top = 0
    Width = 377
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object ButtonOk: TDButton
    Left = 296
    Top = 32
    Width = 81
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
end
