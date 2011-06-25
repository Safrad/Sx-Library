object fGetInt: TfGetInt
  Left = 297
  Top = 207
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 193
  ClientWidth = 335
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
  object LabelMin: TLabel
    Left = 16
    Top = 136
    Width = 113
    Height = 13
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object LabelMax: TLabel
    Left = 208
    Top = 136
    Width = 113
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object LabelNow: TLabel
    Left = 11
    Top = 45
    Width = 125
    Height = 13
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 6
    Top = 102
    Width = 324
    Height = 32
  end
  object EditInput: TLabeledEdit
    Left = 8
    Top = 24
    Width = 137
    Height = 19
    AutoSize = False
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Value'
    EditLabel.Transparent = True
    MaxLength = 15
    TabOrder = 0
    Text = '0'
    OnChange = EditInputChange
  end
  object ButtonOk: TDButton
    Left = 168
    Top = 160
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
    TabOrder = 6
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 256
    Top = 160
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
    TabOrder = 7
    OnClick = ButtonCancelClick
  end
  object TrackBar: TTrackBar
    Left = 8
    Top = 104
    Width = 320
    Height = 28
    Max = 99
    PageSize = 10
    TabOrder = 5
    ThumbLength = 19
    OnChange = TrackBarChange
  end
  object ButtonMin: TDButton
    Left = 168
    Top = 18
    Width = 48
    Height = 18
    Caption = '&Min'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonMinClick
  end
  object ButtonCur: TDButton
    Left = 224
    Top = 6
    Width = 48
    Height = 18
    Caption = '&Cur'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonCurClick
  end
  object ButtonMax: TDButton
    Left = 280
    Top = 18
    Width = 48
    Height = 18
    Caption = '&Max'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonMaxClick
  end
  object ButtonDef: TDButton
    Left = 224
    Top = 28
    Width = 48
    Height = 18
    Caption = '&Def'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonDefClick
  end
  object ButtonApply: TDButton
    Left = 8
    Top = 160
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Visible = False
  end
  object EditError: TMemo
    Left = 8
    Top = 62
    Width = 321
    Height = 33
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
  end
  object UpDown: TUpDown
    Left = 144
    Top = 24
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 10
    OnChangingEx = UpDownChangingEx
  end
end
