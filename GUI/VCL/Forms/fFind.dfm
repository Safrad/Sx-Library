object fFindDialog: TfFindDialog
  Left = 321
  Top = 144
  BorderStyle = bsToolWindow
  Caption = 'Find Text'
  ClientHeight = 196
  ClientWidth = 244
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelText: TLabel
    Left = 16
    Top = 12
    Width = 60
    Height = 13
    Hint = 'Pattern'
    Caption = '&Text to find:'
    FocusControl = cbPattern
    ParentShowHint = False
    ShowHint = True
    Transparent = True
  end
  object Bevel: TBevel
    Left = 8
    Top = 144
    Width = 225
    Height = 2
  end
  object cbPattern: TComboBox
    Left = 88
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 0
    OnChange = cbPatternChange
  end
  object cbIgnoreCaseSensitive: TCheckBox
    Left = 16
    Top = 48
    Width = 217
    Height = 17
    Caption = 'Ignore &Case Sensitive'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = cbIgnoreCaseSensitiveClick
  end
  object cbIgnoreDiacriticMarks: TCheckBox
    Left = 16
    Top = 72
    Width = 217
    Height = 17
    Caption = 'Ignore &Diacritic Marks'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = cbIgnoreDiacriticMarksClick
  end
  object cbWholeWordsOnly: TCheckBox
    Left = 16
    Top = 96
    Width = 217
    Height = 17
    Caption = '&Whole Words Only'
    Enabled = False
    TabOrder = 3
    OnClick = cbWholeWordsOnlyClick
  end
  object cbInteligentMode: TCheckBox
    Left = 16
    Top = 120
    Width = 217
    Height = 17
    Hint = '"this is my pen" can be found with pattern "t i m p"'
    Caption = '&Intelligent Mode'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbInteligentModeClick
  end
  object ButtonOk: TDButton
    Left = 80
    Top = 160
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 5
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 163
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 6
    OnClick = ButtonCancelClick
  end
end
