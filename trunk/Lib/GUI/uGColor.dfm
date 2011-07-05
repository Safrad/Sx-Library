object fGColor: TfGColor
  Left = 405
  Top = 153
  BorderStyle = bsDialog
  Caption = 'Enter color'
  ClientHeight = 314
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 180
    Width = 481
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 216
    Top = 184
    Width = 9
    Height = 121
    Shape = bsLeftLine
  end
  object BevelBasicColors: TBevel
    Left = 224
    Top = 192
    Width = 253
    Height = 81
  end
  object ShapeBorder: TShape
    Left = 232
    Top = 192
    Width = 20
    Height = 20
    Brush.Style = bsClear
    Enabled = False
    Pen.Color = clHighlight
    Pen.Width = 2
    Shape = stSquare
  end
  object LabelPrevious: TLabel
    Left = 8
    Top = 216
    Width = 49
    Height = 19
    AutoSize = False
    Caption = '&Previous'
    FocusControl = PanelPrevious
    Transparent = True
    Layout = tlCenter
  end
  object LabelNowXBit: TLabel
    Left = 8
    Top = 264
    Width = 57
    Height = 19
    AutoSize = False
    Caption = 'R&educed'
    FocusControl = ComboBoxBitDepth
    Transparent = True
    Layout = tlCenter
  end
  object LabelDefault: TLabel
    Left = 8
    Top = 192
    Width = 49
    Height = 19
    AutoSize = False
    Caption = '&Default'
    FocusControl = PanelDefaultColor
    Transparent = True
    Layout = tlCenter
  end
  object LabelCurrent: TLabel
    Left = 8
    Top = 240
    Width = 49
    Height = 19
    AutoSize = False
    Caption = 'C&urrent'
    FocusControl = PanelCurrent
    Transparent = True
    Layout = tlCenter
  end
  object LabelRGB: TLabel
    Left = 8
    Top = 80
    Width = 49
    Height = 19
    AutoSize = False
    Caption = 'RGB'
    FocusControl = EditRGBA
    Transparent = True
    Layout = tlCenter
  end
  object LabelFormat: TLabel
    Left = 8
    Top = 288
    Width = 41
    Height = 19
    AutoSize = False
    Caption = '&Format'
    FocusControl = ComboBoxNF
    Transparent = True
    Layout = tlCenter
  end
  object LabelR: TDLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 19
    Caption = '&Red'
    Color = clRed
    FocusControl = EditR
    Displ.Format = '88'
    ParentColor = False
    Transparent = True
  end
  object LabelG: TDLabel
    Left = 8
    Top = 32
    Width = 57
    Height = 19
    Caption = '&Green'
    Color = 50176
    FocusControl = EditG
    Displ.Format = '88'
    ParentColor = False
    Transparent = True
  end
  object LabelB: TDLabel
    Left = 8
    Top = 56
    Width = 57
    Height = 19
    Caption = '&Blue'
    Color = clBlue
    FocusControl = EditB
    Displ.Format = '88'
    ParentColor = False
    Transparent = True
  end
  object PanelPrevious: TDButton
    Left = 64
    Top = 216
    Width = 145
    Height = 19
    Caption = '&$00000000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 23
    OnClick = PanelPreviousClick
    Color = clBlack
  end
  object PanelCurrent: TDButton
    Left = 64
    Top = 240
    Width = 145
    Height = 19
    Caption = '&$00000000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 24
    Color = clBlack
  end
  object PanelNowBitColor: TDButton
    Left = 120
    Top = 264
    Width = 89
    Height = 19
    Caption = '&$00000000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 29
    OnClick = PanelNowBitColorClick
    Color = clBlack
  end
  object PanelDefaultColor: TDButton
    Left = 64
    Top = 192
    Width = 145
    Height = 19
    Caption = '&$00000000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 22
    OnClick = PanelDefaultColorClick
    Color = clBlack
  end
  object EditR: TDEdit
    Left = 64
    Top = 8
    Width = 34
    Height = 21
    TabOrder = 4
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object ButtonR: TDButton
    Left = 424
    Top = 8
    Width = 55
    Height = 19
    Caption = '&Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    OnClick = ButtonRGBAClick
  end
  object ButtonOk: TDButton
    Left = 248
    Top = 280
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
    TabOrder = 0
    OnClick = ButtonOkClick
  end
  object ButtonApply: TDButton
    Left = 408
    Top = 280
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
  end
  object ButtonCancel: TDButton
    Left = 328
    Top = 280
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
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object EditG: TDEdit
    Tag = 1
    Left = 64
    Top = 32
    Width = 34
    Height = 21
    TabOrder = 7
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object ButtonG: TDButton
    Tag = 1
    Left = 424
    Top = 32
    Width = 55
    Height = 19
    Caption = '&Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = ButtonRGBAClick
  end
  object EditB: TDEdit
    Tag = 2
    Left = 64
    Top = 56
    Width = 34
    Height = 21
    TabOrder = 10
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object ButtonB: TDButton
    Tag = 2
    Left = 424
    Top = 56
    Width = 55
    Height = 19
    Caption = '&Invert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = ButtonRGBAClick
  end
  object EditS: TDEdit
    Tag = 5
    Left = 64
    Top = 152
    Width = 34
    Height = 21
    TabOrder = 20
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object PanelH: TPanel
    Left = 104
    Top = 104
    Width = 310
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 17
    object ImageH: TDImage
      Tag = 3
      Left = 0
      Top = 0
      Width = 306
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object PanelL: TPanel
    Left = 104
    Top = 128
    Width = 260
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 19
    object ImageL: TDImage
      Tag = 4
      Left = 0
      Top = 0
      Width = 256
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object ComboBoxBitDepth: TComboBox
    Left = 64
    Top = 264
    Width = 57
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 25
    OnChange = ComboBoxBitDepthChange
  end
  object LabelH: TDLabel
    Left = 8
    Top = 104
    Width = 57
    Height = 19
    Caption = '&Hue'
    FocusControl = EditH
    FontShadow = 1
    Displ.Format = '88'
    Transparent = True
  end
  object EditL: TDEdit
    Tag = 4
    Left = 64
    Top = 128
    Width = 34
    Height = 21
    TabOrder = 18
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object LabelS: TDLabel
    Left = 8
    Top = 152
    Width = 57
    Height = 19
    Caption = '&Saturation'
    FocusControl = EditS
    FontShadow = 1
    Displ.Format = '88'
    Transparent = True
  end
  object LabelL: TDLabel
    Left = 8
    Top = 128
    Width = 57
    Height = 19
    Caption = '&Lightness'
    FocusControl = EditL
    FontShadow = 1
    Displ.Format = '88'
    Transparent = True
  end
  object PanelR: TPanel
    Left = 104
    Top = 8
    Width = 260
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 5
    object ImageR: TDImage
      Left = 0
      Top = 0
      Width = 256
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object PanelG: TPanel
    Left = 104
    Top = 32
    Width = 260
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 8
    object ImageG: TDImage
      Tag = 1
      Left = 0
      Top = 0
      Width = 256
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object PanelB: TPanel
    Left = 104
    Top = 56
    Width = 260
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 11
    object ImageB: TDImage
      Tag = 2
      Left = 0
      Top = 0
      Width = 256
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object EditH: TDEdit
    Tag = 3
    Left = 64
    Top = 104
    Width = 34
    Height = 21
    TabOrder = 16
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object PanelS: TPanel
    Left = 104
    Top = 152
    Width = 260
    Height = 20
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 21
    object ImageS: TDImage
      Tag = 5
      Left = 0
      Top = 0
      Width = 256
      Height = 16
      Zoom = 1.000000000000000000
      DisplayMode = dmCustom
      OnFill = ImageFill
      Align = alClient
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
    end
  end
  object ComboBoxNF: TComboBox
    Left = 64
    Top = 288
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    PopupMenu = PopupMenu1
    TabOrder = 30
    Text = 'Decadic'
    OnChange = ComboBoxNFChange
    Items.Strings = (
      'Decadic'
      'Hexadecimal')
  end
  object EditRGBA: TDEdit
    Tag = -1
    Left = 64
    Top = 80
    Width = 81
    Height = 21
    TabOrder = 15
    OnChange = EditChange
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object PopupMenu1: TPopupMenu
    Images = ImageList1
    OwnerDraw = True
    Left = 240
    Top = 216
    object clScrollBar1: TMenuItem
      Caption = 'ScrollBar'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clBackground: TMenuItem
      Tag = 1
      Caption = 'Background'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clActiveCaption1: TMenuItem
      Tag = 2
      Caption = 'ActiveCaption'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clInactiveCaption1: TMenuItem
      Tag = 3
      Caption = 'InactiveCaption'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clMenu1: TMenuItem
      Tag = 4
      Caption = 'Menu'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clWindow1: TMenuItem
      Tag = 5
      Caption = 'Window'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clWindowFrame1: TMenuItem
      Tag = 6
      Caption = 'WindowFrame'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clMenuText1: TMenuItem
      Tag = 7
      Caption = 'MenuText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clWindowText1: TMenuItem
      Tag = 8
      Caption = 'WindowText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clCaptionText1: TMenuItem
      Tag = 9
      Caption = 'CaptionText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clActiveBorder1: TMenuItem
      Tag = 10
      Caption = 'ActiveBorder'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clInactiveBorder1: TMenuItem
      Tag = 11
      Caption = 'InactiveBorder'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clAppWorkSpace1: TMenuItem
      Tag = 12
      Caption = 'AppWorkSpace'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clHighlight1: TMenuItem
      Tag = 13
      Break = mbBarBreak
      Caption = 'Highlight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clHighlightText1: TMenuItem
      Tag = 14
      Caption = 'HighlightText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clBtnFace1: TMenuItem
      Tag = 15
      Caption = 'BtnFace'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clBtnShadow1: TMenuItem
      Tag = 16
      Caption = 'BtnShadow'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clGrayText1: TMenuItem
      Tag = 17
      Caption = 'GrayText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clBtnText1: TMenuItem
      Tag = 18
      Caption = 'BtnText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clInactiveCaptionText1: TMenuItem
      Tag = 19
      Caption = 'InactiveCaptionText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clBtnHighlight1: TMenuItem
      Tag = 20
      Caption = 'BtnHighlight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object cl3DDkShadow1: TMenuItem
      Tag = 21
      Caption = '3DDkShadow'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object cl3DLight1: TMenuItem
      Tag = 22
      Caption = '3DLight'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clInfoText1: TMenuItem
      Tag = 23
      Caption = 'InfoText'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clInfoBk1: TMenuItem
      Tag = 24
      Caption = 'InfoBk'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
    object clNone1: TMenuItem
      Tag = -1
      Caption = 'None'
      ImageIndex = 0
      OnClick = ColorClick
      OnAdvancedDrawItem = AdvancedDraw
    end
  end
  object ImageList1: TImageList
    AllocBy = 1
    Left = 272
    Top = 216
  end
end
