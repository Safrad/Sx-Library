object fGetTime: TfGetTime
  Left = 345
  Top = 166
  ActiveControl = EditInput
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  ClientHeight = 272
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
  object Label1: TDLabel
    Left = 8
    Top = 68
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'H'
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
  object Label2: TDLabel
    Left = 8
    Top = 108
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'M'
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
  object Label3: TDLabel
    Left = 8
    Top = 148
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'S'
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
  object Label4: TDLabel
    Left = 8
    Top = 188
    Width = 17
    Height = 17
    AutoSize = False
    Caption = 'D'
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
  object LabelH: TDLabel
    Left = 312
    Top = 68
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '000'
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
  object LabelM: TDLabel
    Left = 312
    Top = 108
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '000'
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
  object LabelS: TDLabel
    Left = 312
    Top = 148
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '000'
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
  object LabelD: TDLabel
    Left = 312
    Top = 188
    Width = 20
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = '000'
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
  object TrackBarH: TTrackBar
    Left = 32
    Top = 64
    Width = 273
    Height = 28
    Ctl3D = True
    Max = 23
    Orientation = trHorizontal
    ParentCtl3D = False
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 5
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarHMSDChange
  end
  object TrackBarM: TTrackBar
    Left = 32
    Top = 104
    Width = 259
    Height = 28
    Max = 59
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 7
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarHMSDChange
  end
  object TrackBarS: TTrackBar
    Left = 32
    Top = 144
    Width = 259
    Height = 28
    Max = 59
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 9
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarHMSDChange
  end
  object TrackBarD: TTrackBar
    Left = 32
    Top = 184
    Width = 223
    Height = 28
    Max = 1000
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 10
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 11
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarHMSDChange
  end
  object ButtonOk: TDButton
    Left = 192
    Top = 240
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
    TabOrder = 13
  end
  object ButtonCancel: TDButton
    Left = 280
    Top = 240
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
    TabOrder = 14
  end
  object EditInput: TEdit
    Left = 40
    Top = 16
    Width = 129
    Height = 19
    AutoSize = False
    MaxLength = 15
    TabOrder = 0
    OnChange = EditInputChange
  end
  object ButtonMin: TDButton
    Left = 192
    Top = 16
    Width = 48
    Height = 18
    Caption = 'Min'
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
    Left = 248
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
    TabOrder = 2
    OnClick = ButtonCurClick
  end
  object ButtonMax: TDButton
    Left = 304
    Top = 16
    Width = 48
    Height = 18
    Caption = 'Max'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonMaxClick
  end
  object SpinButtonH: TSpinButton
    Tag = 3600000
    Left = 336
    Top = 64
    Width = 17
    Height = 25
    Ctl3D = True
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0900E03DE03DE03D
      E03D0000E03DE03DE03DE03D2003E03DE03DE03D000000000000E03DE03DE03D
      0200E03DE03D00000000000000000000E03DE03DA601E03D0000000000000000
      000000000000E03D0400E03DE03DE03DE03DE03DE03DE03DE03DE03DA001}
    ParentCtl3D = False
    TabOrder = 6
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03D00000000
      00000000000000000000E03DB600E03DE03D00000000000000000000E03DE03D
      0200E03DE03DE03D000000000000E03DE03DE03D0200E03DE03DE03DE03D0000
      E03DE03DE03DE03DB281E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    OnDownClick = SpinButtonDownClick
    OnUpClick = SpinButtonUpClick
  end
  object SpinButtonS: TSpinButton
    Tag = 1000
    Left = 336
    Top = 144
    Width = 17
    Height = 25
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03DE03DE03D
      E03D0000E03DE03DE03DE03DD102E03DE03DE03D000000000000E03DE03DE03D
      D102E03DE03D00000000000000000000E03DE03DD902E03D0000000000000000
      000000000000E03DB181E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    TabOrder = 10
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0000E03D00000000
      00000000000000000000E03D0500E03DE03D00000000000000000000E03DE03D
      CE02E03DE03DE03D000000000000E03DE03DE03DE102E03DE03DE03DE03D0000
      E03DE03DE03DE03D5152E03DE03DE03DE03DE03DE03DE03DE03DE03D1F02}
    OnDownClick = SpinButtonDownClick
    OnUpClick = SpinButtonUpClick
  end
  object SpinButtonM: TSpinButton
    Tag = 60000
    Left = 336
    Top = 104
    Width = 17
    Height = 25
    Ctl3D = True
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0900E03DE03DE03D
      E03D0000E03DE03DE03DE03D2003E03DE03DE03D000000000000E03DE03DE03D
      0200E03DE03D00000000000000000000E03DE03DA601E03D0000000000000000
      000000000000E03D0400E03DE03DE03DE03DE03DE03DE03DE03DE03DA001}
    ParentCtl3D = False
    TabOrder = 8
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03D00000000
      00000000000000000000E03DB600E03DE03D00000000000000000000E03DE03D
      0200E03DE03DE03D000000000000E03DE03DE03D0200E03DE03DE03DE03D0000
      E03DE03DE03DE03DB281E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    OnDownClick = SpinButtonDownClick
    OnUpClick = SpinButtonUpClick
  end
  object SpinButtonD: TSpinButton
    Tag = 100
    Left = 336
    Top = 184
    Width = 17
    Height = 25
    Ctl3D = True
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0900E03DE03DE03D
      E03D0000E03DE03DE03DE03D2003E03DE03DE03D000000000000E03DE03DE03D
      0200E03DE03D00000000000000000000E03DE03DA601E03D0000000000000000
      000000000000E03D0400E03DE03DE03DE03DE03DE03DE03DE03DE03DA001}
    ParentCtl3D = False
    TabOrder = 12
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03D00000000
      00000000000000000000E03DB600E03DE03D00000000000000000000E03DE03D
      0200E03DE03DE03D000000000000E03DE03DE03D0200E03DE03DE03DE03D0000
      E03DE03DE03DE03DB281E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    OnDownClick = SpinButtonDownClick
    OnUpClick = SpinButtonUpClick
  end
  object ButtonDef: TDButton
    Left = 248
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
    TabOrder = 3
    OnClick = ButtonDefClick
  end
end
