object fGetInt: TfGetInt
  Left = 297
  Top = 207
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 193
  ClientWidth = 394
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMin: TLabel
    Left = 16
    Top = 136
    Width = 145
    Height = 13
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object LabelMax: TLabel
    Left = 224
    Top = 138
    Width = 153
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object LabelNow: TLabel
    Left = 8
    Top = 45
    Width = 201
    Height = 13
    AutoSize = False
    Caption = '0123456789012345'
    Transparent = True
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 6
    Top = 102
    Width = 380
    Height = 32
  end
  object EditInput: TLabeledEdit
    Left = 8
    Top = 24
    Width = 201
    Height = 19
    AutoSize = False
    EditLabel.Width = 26
    EditLabel.Height = 13
    EditLabel.Caption = '&Value'
    EditLabel.Transparent = True
    TabOrder = 0
    Text = '0'
    OnChange = EditInputChange
  end
  object ButtonOk: TDButton
    Left = 153
    Top = 160
    Width = 73
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 233
    Top = 160
    Width = 73
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 7
    OnClick = ButtonCancelClick
  end
  object TrackBar: TTrackBar
    Left = 8
    Top = 104
    Width = 378
    Height = 28
    Max = 99
    PageSize = 10
    TabOrder = 5
    ThumbLength = 19
    OnChange = TrackBarChange
  end
  object ButtonMin: TDButton
    Left = 231
    Top = 4
    Width = 73
    Height = 25
    Caption = 'Minimal'
    TabOrder = 1
    OnClick = ButtonMinClick
  end
  object ButtonCur: TDButton
    Left = 231
    Top = 34
    Width = 73
    Height = 25
    Caption = '&Currrent'
    TabOrder = 2
    OnClick = ButtonCurClick
  end
  object ButtonMax: TDButton
    Left = 313
    Top = 4
    Width = 73
    Height = 25
    Caption = 'Maximal'
    TabOrder = 4
    OnClick = ButtonMaxClick
  end
  object ButtonDef: TDButton
    Left = 313
    Top = 33
    Width = 73
    Height = 25
    Caption = '&Default'
    TabOrder = 3
    OnClick = ButtonDefClick
  end
  object ButtonApply: TDButton
    Left = 313
    Top = 160
    Width = 73
    Height = 25
    Caption = '&Apply'
    TabOrder = 8
    Visible = False
  end
  object EditError: TDMemo
    Left = 8
    Top = 62
    Width = 378
    Height = 33
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
  end
  object UpDown: TUpDown
    Left = 208
    Top = 24
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 10
    OnChangingEx = UpDownChangingEx
  end
end
