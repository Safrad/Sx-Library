object fSounds: TfSounds
  Left = 409
  Top = 260
  Width = 458
  Height = 298
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Sounds'
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 458
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
    Top = 24
    Width = 433
    Height = 49
    Shape = bsFrame
  end
  object LabelSQ: TLabel
    Left = 8
    Top = 8
    Width = 169
    Height = 17
    AutoSize = False
    Caption = '&Sound Quality:'
    FocusControl = ButtonSound
    Transparent = True
    Layout = tlCenter
  end
  object LabelFrequency: TLabel
    Left = 304
    Top = 40
    Width = 57
    Height = 21
    AutoSize = False
    Caption = 'Frequency'
    FocusControl = ComboBoxFrequency
    Transparent = True
    Layout = tlCenter
  end
  object DViewSounds: TDView
    Left = 8
    Top = 80
    Width = 257
    Height = 137
    Zoom = 1.000000000000000000
    PopupMenu = PopupMenuSounds
    TabOrder = 9
    OnDblClick = DViewSoundsDblClick
    OnGetData = DViewSoundsGetData
  end
  object ButtonOK: TDButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = '&OK'
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
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Apply'
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
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ComboBoxFrequency: TComboBox
    Left = 360
    Top = 40
    Width = 73
    Height = 21
    AutoComplete = False
    ItemHeight = 13
    TabOrder = 6
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
    Left = 208
    Top = 32
    Width = 89
    Height = 33
    Caption = '&Use 16 bits Samples'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    AutoChange = True
    Down = True
  end
  object ButtonStereo: TDButton
    Left = 128
    Top = 32
    Width = 73
    Height = 33
    Caption = '&Stereo Channels'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    AutoChange = True
    Down = True
  end
  object ButtonReduce: TDButton
    Left = 16
    Top = 32
    Width = 105
    Height = 33
    Caption = '&Reduce More Sounds at Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    AutoChange = True
    Down = True
  end
  object ButtonMusic: TDButton
    Left = 96
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Music'
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
  object ButtonSound: TDButton
    Left = 8
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Sound'
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
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 328
    Top = 88
  end
  object PopupMenuSounds: TPopupMenu
    OnPopup = PopupMenuSoundsPopup
    Left = 16
    Top = 88
    object Preview1: TMenuItem
      Caption = 'Preview'
      Default = True
      OnClick = Select1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Enable1: TMenuItem
      Tag = 1
      Caption = 'Enable'
      OnClick = Select1Click
    end
    object Disable1: TMenuItem
      Tag = 2
      Caption = 'Disable'
      OnClick = Select1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Select1: TMenuItem
      Tag = 3
      Caption = 'Select...'
      OnClick = Select1Click
    end
    object SetDefault1: TMenuItem
      Tag = 4
      Caption = 'Set Default'
      OnClick = Select1Click
    end
    object SetBeep1: TMenuItem
      Tag = 5
      Caption = 'Set Beep'
      OnClick = Select1Click
    end
  end
end
