object fSounds: TfSounds
  Left = 409
  Top = 260
  Width = 459
  Height = 390
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Sounds'
  Color = clBtnFace
  Constraints.MinHeight = 390
  Constraints.MinWidth = 459
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BevelSQ: TBevel
    Left = 8
    Top = 184
    Width = 169
    Height = 137
    Shape = bsFrame
  end
  object DViewS: TDView
    Left = 184
    Top = 8
    Width = 257
    Height = 313
    DrawFPS = False
    HandScroll = False
    HotTrack = True
    OnPaint = DViewSPaint
    TabOrder = 13
    TabStop = False
    OnDblClick = DViewSDblClick
    OnGetData = DViewSGetData
  end
  object ButtonSelect: TDButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '/...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonSelectClick
  end
  object ButtonPreview: TDButton
    Tag = 2
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = '/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = ButtonSelectClick
  end
  object ButtonDisable: TDButton
    Tag = 1
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = '/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonSelectClick
  end
  object ButtonOK: TDButton
    Left = 192
    Top = 328
    Width = 75
    Height = 25
    Caption = '/'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonApply: TDButton
    Left = 280
    Top = 328
    Width = 75
    Height = 25
    Caption = '/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonApplyClick
  end
  object ButtonCancel: TDButton
    Left = 368
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = '/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object LabelSQ: TDLabel
    Left = 8
    Top = 168
    Width = 169
    Height = 17
    AlphaBlend = False
    AlphaBlendValue = 0
    AutoSize = False
    Alignment = taLeftJustify
    Caption = '&Sound Quality:'
    BackEffect = ef00
    FocusControl = ButtonSound
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
    Layout = tlCenter
    Transparent = True
    TransparentColor = False
    TransparentColorValue = clBlack
    WordWrap = False
  end
  object LabelFrequency: TDLabel
    Left = 16
    Top = 288
    Width = 65
    Height = 21
    AlphaBlend = False
    AlphaBlendValue = 0
    AutoSize = False
    Alignment = taLeftJustify
    Caption = 'Frequency'
    BackEffect = ef00
    FocusControl = ComboBoxFrequency
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
    Layout = tlCenter
    Transparent = True
    TransparentColor = False
    TransparentColorValue = clBlack
    WordWrap = False
  end
  object ComboBoxFrequency: TComboBox
    Left = 96
    Top = 288
    Width = 73
    Height = 21
    AutoComplete = False
    ItemHeight = 13
    TabOrder = 10
    Items.Strings = (
      '11025'
      '22050'
      '33075'
      '44100'
      '48000'
      '96000'
      '192000')
  end
  object Button16bits: TDButton
    Left = 16
    Top = 256
    Width = 153
    Height = 21
    Caption = 'Use 16 bits Samples'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    AutoChange = True
    Down = True
  end
  object ButtonStereo: TDButton
    Left = 16
    Top = 224
    Width = 153
    Height = 21
    Caption = 'Stereo Channels'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    AutoChange = True
    Down = True
  end
  object ButtonReduce: TDButton
    Left = 16
    Top = 192
    Width = 153
    Height = 21
    Caption = 'Reduce More Sounds at Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    AutoChange = True
    Down = True
  end
  object ButtonMusic: TDButton
    Left = 96
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Music'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    AutoChange = True
    Down = True
  end
  object ButtonSound: TDButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Sound'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    AutoChange = True
    Down = True
  end
  object ButtonBeepSound: TDButton
    Tag = 3
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = '/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonSelectClick
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 200
  end
end
