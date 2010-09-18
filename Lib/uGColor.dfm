object fGColor: TfGColor
  Left = 115
  Top = 76
  BorderStyle = bsDialog
  Caption = 'Enter color'
  ClientHeight = 378
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ImageBackground: TImage
    Left = 0
    Top = 0
    Width = 433
    Height = 378
    Align = alClient
  end
  object Label1: TDLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Red'
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
    Top = 40
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Green'
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
    Top = 72
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Blue'
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
    Top = 104
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'All'
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
  object Bevel1: TBevel
    Left = 8
    Top = 332
    Width = 417
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 224
    Top = 280
    Width = 9
    Height = 49
    Shape = bsLeftLine
  end
  object LabelNow: TDLabel
    Left = 8
    Top = 284
    Width = 54
    Height = 16
    AutoSize = False
    Caption = 'Now'
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
    BevelOuter = bvNone
  end
  object LabelNowXBit: TDLabel
    Left = 8
    Top = 308
    Width = 54
    Height = 16
    AutoSize = False
    Caption = 'Now x bit'
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
    BevelOuter = bvNone
  end
  object LabelDefault: TDLabel
    Left = 232
    Top = 284
    Width = 39
    Height = 16
    AutoSize = False
    Caption = 'Default'
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
    BevelOuter = bvNone
  end
  object LabelCurrent: TDLabel
    Left = 232
    Top = 308
    Width = 39
    Height = 16
    AutoSize = False
    Caption = 'Current'
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
    BevelOuter = bvNone
  end
  object EditR: TEdit
    Left = 48
    Top = 8
    Width = 25
    Height = 19
    AutoSize = False
    TabOrder = 3
    Text = '255'
    OnChange = EditRChange
  end
  object TrackBarR: TTrackBar
    Left = 88
    Top = 8
    Width = 286
    Height = 32
    Max = 255
    Orientation = trHorizontal
    PageSize = 8
    Frequency = 8
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarRChange
  end
  object BitBtnR: TDBitBtn
    Left = 376
    Top = 8
    Width = 49
    Height = 24
    Caption = 'Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = BitBtnRClick
  end
  object ButtonOk: TDBitBtn
    Left = 256
    Top = 344
    Width = 73
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ButtonOkClick
    Kind = bkOK
  end
  object ButtonApply: TDBitBtn
    Left = 112
    Top = 344
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Visible = False
    Glyph.Data = {
      B6030000424DB603000000000000B60000002800000010000000100000000100
      1800000000000003000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00800080800080
      8000808000808000808000008000008000008000008000008000808000808000
      8080008080008080008000800080000080008080008080000000800000800000
      8000008000008000800000800000800080800080800080800080008000008000
      8000008000000080000080000080000080000080000080000080000080008000
      0080008080008080008000800000800000800000800000800000800000FF0000
      FF0000FF0000FF00008000008000008000800000800080800080008000008000
      00800000800000800000FF0080008080008080008080008000FF000080000080
      0000800080000080008000800000800000800000800000800080008080008080
      008080008080008080008000FF00008000008000800000800080008000008000
      00800000800000800000800080008080008080008080008080008000FF008000
      0080000080000080008000FF0000FF0000FF0000FF0000FF0000FF0000FF0080
      0080800080800080800080800080800080800080800080800080800080800080
      8000808000808000808000808000808000808000808000808000008000008000
      0080000080000080000000FF0080000080000080000080008080008080008080
      008080008000FF0000800000800000800000800000800080000000FF00008000
      00800080000080008080008080008080008080008080008000FF000080000080
      0000800000800080000080008000FF0000800000800080000080008080008080
      008080008080000080000000800000800000800000800080000080008000FF00
      0080000080000080008000008000008000008000000080000080000080000080
      0000800000800080000080008080008000FF0000800000800000800000800000
      800000800000800000800000800000FF0000FF00008000800000800080800080
      80008000FF0000FF0000800000800000800000800000800000FF0000FF008000
      8080008000FF0080008080008080008080008080008080008000FF0000FF0000
      FF0000FF0000FF00800080800080800080800080800080800080}
  end
  object ButtonCancel: TDBitBtn
    Left = 344
    Top = 344
    Width = 73
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonCancelClick
    Kind = bkCancel
  end
  object RadioGroup1: TRadioGroup
    Left = 312
    Top = 184
    Width = 113
    Height = 89
    Caption = 'Colors'
    ItemIndex = 4
    Items.Strings = (
      '1 bit'
      '4 bits'
      '15 bits'
      '18 bits'
      '24 bits')
    TabOrder = 18
    OnClick = RadioGroup1Click
  end
  object EditG: TEdit
    Left = 48
    Top = 40
    Width = 25
    Height = 19
    AutoSize = False
    TabOrder = 6
    Text = '255'
  end
  object TrackBarG: TTrackBar
    Left = 88
    Top = 40
    Width = 286
    Height = 32
    Max = 255
    Orientation = trHorizontal
    PageSize = 8
    Frequency = 8
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 7
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
  end
  object BitBtnG: TDBitBtn
    Left = 376
    Top = 40
    Width = 49
    Height = 24
    Caption = 'Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = BitBtnGClick
  end
  object EditB: TEdit
    Left = 48
    Top = 72
    Width = 25
    Height = 19
    AutoSize = False
    TabOrder = 9
    Text = '255'
  end
  object TrackBarB: TTrackBar
    Left = 88
    Top = 72
    Width = 286
    Height = 32
    Max = 255
    Orientation = trHorizontal
    PageSize = 8
    Frequency = 8
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 10
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
  end
  object BitBtnB: TDBitBtn
    Left = 376
    Top = 72
    Width = 49
    Height = 24
    Caption = 'Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = BitBtnBClick
  end
  object EditA: TEdit
    Left = 48
    Top = 104
    Width = 25
    Height = 19
    AutoSize = False
    TabOrder = 12
    Text = '255'
  end
  object TrackBarA: TTrackBar
    Left = 88
    Top = 104
    Width = 286
    Height = 32
    Max = 255
    Orientation = trHorizontal
    PageSize = 8
    Frequency = 8
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 13
    ThumbLength = 19
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarAChange
  end
  object BitBtnA: TDBitBtn
    Left = 376
    Top = 104
    Width = 49
    Height = 24
    Caption = 'Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = BitBtnAClick
  end
  object GroupBoxColors: TGroupBox
    Left = 8
    Top = 184
    Width = 264
    Height = 89
    Caption = 'Colors'
    TabOrder = 17
    object ShapeBorder: TShape
      Left = 0
      Top = 0
      Width = 20
      Height = 20
      Brush.Style = bsClear
      Enabled = False
      Pen.Color = clHighlight
      Pen.Width = 2
      Shape = stSquare
    end
  end
  object PanelS: TPanel
    Left = 24
    Top = 136
    Width = 388
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 15
    object ImageS: TImage
      Left = 0
      Top = 0
      Width = 384
      Height = 16
      Align = alClient
      OnMouseDown = ImageSMouseDown
      OnMouseMove = ImageSMouseMove
      OnMouseUp = ImageSMouseUp
    end
  end
  object PanelL: TPanel
    Left = 24
    Top = 160
    Width = 388
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 16
    object ImageL: TImage
      Left = 0
      Top = 0
      Width = 384
      Height = 16
      Align = alClient
      OnMouseDown = ImageLMouseDown
      OnMouseMove = ImageLMouseMove
      OnMouseUp = ImageLMouseUp
    end
  end
  object PanelNowColor: TDPanel
    Left = 64
    Top = 280
    Width = 153
    Height = 24
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = '$00000000'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 19
  end
  object PanelCurColor: TDPanel
    Left = 272
    Top = 304
    Width = 153
    Height = 24
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = '$00000000'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    TabOrder = 22
    OnClick = PanelCurColorClick
  end
  object PanelNowBitColor: TDPanel
    Left = 64
    Top = 304
    Width = 153
    Height = 24
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = '$00000000'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 20
    OnClick = PanelNowBitColorClick
  end
  object PanelDefaultColor: TDPanel
    Left = 272
    Top = 280
    Width = 153
    Height = 24
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = '$00000000'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    TabOrder = 21
    OnClick = PanelDefaultColorClick
  end
  object PopupMenu1: TPopupMenu
    Images = ImageList1
    OwnerDraw = True
    Left = 16
    Top = 336
    object clScrollBar1: TMenuItem
      Caption = 'ScrollBar'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clBackground: TMenuItem
      Tag = 1
      Caption = 'Background'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clActiveCaption1: TMenuItem
      Tag = 2
      Caption = 'ActiveCaption'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clInactiveCaption1: TMenuItem
      Tag = 3
      Caption = 'InactiveCaption'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clMenu1: TMenuItem
      Tag = 4
      Caption = 'Menu'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clWindow1: TMenuItem
      Tag = 5
      Caption = 'Window'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clWindowFrame1: TMenuItem
      Tag = 6
      Caption = 'WindowFrame'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clMenuText1: TMenuItem
      Tag = 7
      Caption = 'MenuText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clWindowText1: TMenuItem
      Tag = 8
      Caption = 'WindowText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clCaptionText1: TMenuItem
      Tag = 9
      Caption = 'CaptionText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clActiveBorder1: TMenuItem
      Tag = 10
      Caption = 'ActiveBorder'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clInactiveBorder1: TMenuItem
      Tag = 11
      Caption = 'InactiveBorder'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clAppWorkSpace1: TMenuItem
      Tag = 12
      Caption = 'AppWorkSpace'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clHighlight1: TMenuItem
      Tag = 13
      Break = mbBarBreak
      Caption = 'Highlight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clHighlightText1: TMenuItem
      Tag = 14
      Caption = 'HighlightText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clBtnFace1: TMenuItem
      Tag = 15
      Caption = 'BtnFace'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clBtnShadow1: TMenuItem
      Tag = 16
      Caption = 'BtnShadow'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clGrayText1: TMenuItem
      Tag = 17
      Caption = 'GrayText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clBtnText1: TMenuItem
      Tag = 18
      Caption = 'BtnText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clInactiveCaptionText1: TMenuItem
      Tag = 19
      Caption = 'InactiveCaptionText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clBtnHighlight1: TMenuItem
      Tag = 20
      Caption = 'BtnHighlight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object cl3DDkShadow1: TMenuItem
      Tag = 21
      Caption = '3DDkShadow'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object cl3DLight1: TMenuItem
      Tag = 22
      Caption = '3DLight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clInfoText1: TMenuItem
      Tag = 23
      Caption = 'InfoText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clInfoBk1: TMenuItem
      Tag = 24
      Caption = 'InfoBk'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
    object clNone1: TMenuItem
      Tag = -1
      Caption = 'None'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvencedDraw
    end
  end
  object ImageList1: TImageList
    AllocBy = 1
    Left = 48
    Top = 336
  end
end
