object TestForm: TTestForm
  Left = 262
  Top = 107
  Width = 417
  Height = 172
  Caption = 'TestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object xButton: TButton
    Left = 0
    Top = 0
    Width = 113
    Height = 25
    Caption = 'xButton'
    TabOrder = 0
    OnClick = xButtonClick
  end
  object xEdit: TEdit
    Left = 0
    Top = 96
    Width = 409
    Height = 21
    TabOrder = 3
    OnKeyDown = xEditKeyDown
    OnKeyUp = xEditKeyUp
  end
  object xMemo: TMemo
    Left = 120
    Top = 0
    Width = 289
    Height = 89
    TabOrder = 4
  end
  object xButton2: TButton
    Left = 0
    Top = 32
    Width = 113
    Height = 25
    Caption = 'xButton2'
    TabOrder = 1
  end
  object xButton3: TButton
    Left = 0
    Top = 64
    Width = 113
    Height = 25
    Caption = 'xButton3'
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 8
    object est11: TMenuItem
      Caption = 'Test Menu'
      object xAltBackspace: TMenuItem
        Caption = 'xAltBackspace'
        ShortCut = 32776
        OnClick = xAltBackspaceClick
      end
      object xCtrlA: TMenuItem
        Caption = 'Ctrl A'
        ShortCut = 16449
        OnClick = xCtrlAClick
      end
      object F21: TMenuItem
        Caption = 'F8'
        ShortCut = 119
        OnClick = F8Click
      end
    end
  end
end
