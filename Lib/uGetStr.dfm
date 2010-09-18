object fGetStr: TfGetStr
  Left = 288
  Top = 162
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 105
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ImageBackground: TImage
    Left = 0
    Top = 0
    Width = 266
    Height = 105
    Align = alClient
  end
  object EditInput: TEdit
    Left = 8
    Top = 32
    Width = 249
    Height = 19
    AutoSize = False
    MaxLength = 16
    TabOrder = 0
    OnChange = EditInputChange
  end
  object ButtonOK: TDButton
    Left = 96
    Top = 72
    Width = 73
    Height = 25
    Caption = 'OK'
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
    Left = 184
    Top = 72
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
  object ButtonCur: TDButton
    Left = 210
    Top = 11
    Width = 48
    Height = 18
    Caption = 'Cur'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonCurClick
  end
  object ButtonDef: TDButton
    Left = 154
    Top = 11
    Width = 48
    Height = 18
    Caption = 'Def'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonDefClick
  end
end
