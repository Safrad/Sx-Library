object fScreen: TfScreen
  Left = 239
  Top = 172
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Can not assign graphics driver, select driver manually'
  ClientHeight = 57
  ClientWidth = 377
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxDriver: TComboBox
    Left = 0
    Top = 0
    Width = 377
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object ButtonOk: TDButton
    Left = 296
    Top = 32
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
