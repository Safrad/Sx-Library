object fGetInt: TfGetInt
  Left = 551
  Top = 111
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 165
  ClientWidth = 335
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
    Width = 335
    Height = 165
    Align = alClient
  end
  object LabelMin: TDLabel
    Left = 16
    Top = 104
    Width = 73
    Height = 13
    AutoSize = False
    Caption = '0'
    Transparent = True
    Layout = tlCenter
    BackEffect = ef00
    FontShadow = 1
    Displ.Enabled = False
    Displ.Format = '88'
    Displ.SizeX = 4
    Displ.SizeY = 4
    Displ.SpaceSX = 2
    Displ.SpaceSY = 2
    Displ.SizeT = 1
    Displ.Spacing = 0
    Displ.ColorA = clRed
    Displ.ColorD = clMaroon
    Displ.Size = 0
  end
  object LabelMax: TDLabel
    Left = 248
    Top = 104
    Width = 73
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
    Transparent = True
    Layout = tlCenter
    BackEffect = ef00
    FontShadow = 1
    Displ.Enabled = False
    Displ.Format = '88'
    Displ.SizeX = 4
    Displ.SizeY = 4
    Displ.SpaceSX = 2
    Displ.SpaceSY = 2
    Displ.SizeT = 1
    Displ.Spacing = 0
    Displ.ColorA = clRed
    Displ.ColorD = clMaroon
    Displ.Size = 0
  end
  object LabelNow: TDLabel
    Left = 132
    Top = 104
    Width = 73
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Transparent = True
    Layout = tlCenter
    BackEffect = ef00
    FontShadow = 1
    Displ.Enabled = False
    Displ.Format = '88'
    Displ.SizeX = 4
    Displ.SizeY = 4
    Displ.SpaceSX = 2
    Displ.SpaceSY = 2
    Displ.SizeT = 1
    Displ.Spacing = 0
    Displ.ColorA = clRed
    Displ.ColorD = clMaroon
    Displ.Size = 0
  end
  object DLabelError: TDLabel
    Left = 16
    Top = 50
    Width = 313
    Height = 15
    AutoSize = False
    Caption = 'No Error'
    Transparent = True
    Layout = tlCenter
    BackEffect = ef00
    FontShadow = 1
    Displ.Enabled = False
    Displ.Format = '88'
    Displ.SizeX = 4
    Displ.SizeY = 4
    Displ.SpaceSX = 2
    Displ.SpaceSY = 2
    Displ.SizeT = 1
    Displ.Spacing = 0
    Displ.ColorA = clRed
    Displ.ColorD = clMaroon
    Displ.Size = 0
  end
  object EditInput: TEdit
    Left = 16
    Top = 26
    Width = 113
    Height = 19
    AutoSize = False
    MaxLength = 15
    TabOrder = 0
    OnChange = EditInputChange
  end
  object ButtonOk: TDButton
    Left = 160
    Top = 128
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
    TabOrder = 7
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 248
    Top = 128
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
    TabOrder = 8
    OnClick = ButtonCancelClick
  end
  object TrackBar: TTrackBar
    Left = 8
    Top = 72
    Width = 320
    Height = 28
    Max = 99
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 6
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarChange
  end
  object ButtonMin: TDButton
    Left = 168
    Top = 24
    Width = 48
    Height = 18
    Caption = 'Min'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonMinClick
  end
  object ButtonCur: TDButton
    Left = 224
    Top = 8
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
  object ButtonMax: TDButton
    Left = 280
    Top = 24
    Width = 48
    Height = 18
    Caption = 'Max'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonMaxClick
  end
  object SpinButton1: TSpinButton
    Left = 136
    Top = 25
    Width = 17
    Height = 22
    DownGlyph.Data = {
      AE000000424DAE00000000000000360000002800000009000000060000000100
      1000000000007800000000000000000000000000000000000000E03DE03DE03D
      E03DE03DE03DE03DE03DE03D0000E03DE03DE03DE03D0000E03DE03DE03DE03D
      FCFFE03DE03DE03D000000000000E03DE03DE03D7902E03DE03D000000000000
      00000000E03DE03D0200E03D0000000000000000000000000000E03D5152E03D
      E03DE03DE03DE03DE03DE03DE03DE03D7902}
    TabOrder = 1
    UpGlyph.Data = {
      AE000000424DAE00000000000000360000002800000009000000060000000100
      1000000000007800000000000000000000000000000000000000E03DE03DE03D
      E03DE03DE03DE03DE03DE03D0500E03D0000000000000000000000000000E03D
      3302E03DE03D00000000000000000000E03DE03D1303E03DE03DE03D00000000
      0000E03DE03DE03D0400E03DE03DE03DE03D0000E03DE03DE03DE03D0602E03D
      E03DE03DE03DE03DE03DE03DE03DE03DB181}
    OnDownClick = SpinButton1DownClick
    OnUpClick = SpinButton1UpClick
  end
  object ButtonDef: TDButton
    Left = 224
    Top = 32
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
  object ButtonApply: TDButton
    Left = 8
    Top = 128
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    Visible = False
  end
end
