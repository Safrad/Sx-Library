object fGetStr: TfGetStr
  Left = 288
  Top = 162
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 105
  ClientWidth = 266
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object EditInput: TEdit
    Left = 8
    Top = 32
    Width = 249
    Height = 21
    MaxLength = 16
    TabOrder = 0
    OnChange = EditInputChange
  end
  object ButtonOK: TDButton
    Left = 96
    Top = 72
    Width = 73
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TDButton
    Left = 184
    Top = 72
    Width = 73
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonCur: TDButton
    Left = 210
    Top = 11
    Width = 48
    Height = 18
    Caption = '&Cur'
    TabOrder = 3
    OnClick = ButtonCurClick
  end
  object ButtonDef: TDButton
    Left = 154
    Top = 11
    Width = 48
    Height = 18
    Caption = '&Def'
    TabOrder = 4
    OnClick = ButtonDefClick
  end
end
