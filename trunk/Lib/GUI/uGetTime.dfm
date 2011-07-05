object fGetTime: TfGetTime
  Left = 254
  Top = 147
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 283
  ClientWidth = 362
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
  object Label1: TLabel
    Left = 8
    Top = 88
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'H'
    Transparent = True
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'M'
    Transparent = True
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 8
    Top = 168
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'S'
    Transparent = True
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 8
    Top = 208
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'D'
    Transparent = True
    Layout = tlCenter
  end
  object LabelH: TLabel
    Left = 304
    Top = 88
    Width = 28
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '1 200'
    Transparent = True
    Layout = tlCenter
  end
  object LabelM: TLabel
    Left = 312
    Top = 128
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '59'
    Transparent = True
    Layout = tlCenter
  end
  object LabelS: TLabel
    Left = 312
    Top = 168
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '59'
    Transparent = True
    Layout = tlCenter
  end
  object LabelD: TLabel
    Left = 312
    Top = 208
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '59'
    Transparent = True
    Layout = tlCenter
  end
  object TrackBarH: TTrackBar
    Left = 32
    Top = 88
    Width = 273
    Height = 28
    Max = 23
    TabOrder = 5
    ThumbLength = 19
    OnChange = TrackBarHMSDChange
  end
  object TrackBarM: TTrackBar
    Left = 32
    Top = 128
    Width = 259
    Height = 28
    Max = 59
    PageSize = 10
    TabOrder = 7
    ThumbLength = 19
    OnChange = TrackBarHMSDChange
  end
  object TrackBarS: TTrackBar
    Left = 32
    Top = 168
    Width = 259
    Height = 28
    Max = 59
    PageSize = 10
    TabOrder = 9
    ThumbLength = 19
    OnChange = TrackBarHMSDChange
  end
  object TrackBarD: TTrackBar
    Left = 32
    Top = 208
    Width = 223
    Height = 28
    Max = 1000
    PageSize = 10
    Frequency = 10
    TabOrder = 11
    ThumbLength = 19
    OnChange = TrackBarHMSDChange
  end
  object ButtonOk: TDButton
    Left = 120
    Top = 248
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
    TabOrder = 13
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 200
    Top = 248
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
    TabOrder = 14
    OnClick = ButtonCancelClick
  end
  object EditInput: TDEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    DoubleBuffered = True
    MaxLength = 15
    ParentDoubleBuffered = False
    TabOrder = 0
    OnChange = EditInputChange
  end
  object ButtonMin: TDButton
    Left = 136
    Top = 8
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
    Left = 192
    Top = 8
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
    Left = 304
    Top = 8
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
  object SpinButtonH: TUpDown
    Tag = 3600000
    Left = 336
    Top = 88
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 6
    OnChangingEx = SpinButtonHMSDChangingEx
  end
  object SpinButtonS: TUpDown
    Tag = 1000
    Left = 336
    Top = 168
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 10
    OnChangingEx = SpinButtonHMSDChangingEx
  end
  object SpinButtonM: TUpDown
    Tag = 60000
    Left = 336
    Top = 128
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 8
    OnChangingEx = SpinButtonHMSDChangingEx
  end
  object SpinButtonD: TUpDown
    Tag = 100
    Left = 336
    Top = 208
    Width = 17
    Height = 19
    Min = -32768
    Max = 32767
    TabOrder = 12
    OnChangingEx = SpinButtonHMSDChangingEx
  end
  object ButtonDef: TDButton
    Left = 248
    Top = 8
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
    Left = 280
    Top = 248
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    Visible = False
  end
  object EditError: TDMemo
    Left = 8
    Top = 40
    Width = 345
    Height = 33
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 16
    WantReturns = False
  end
end
