object fSounds: TfSounds
  Left = 472
  Top = 280
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  Caption = 'Sounds'
  ClientHeight = 259
  ClientWidth = 442
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 458
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    FocusControl = ButtonSounds
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
    Width = 433
    Height = 137
    Zoom = 1.000000000000000000
    EnableZoom = True
    DisplayMode = dmCustom
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
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonApply: TDButton
    Left = 368
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 2
    OnClick = ButtonApplyClick
  end
  object ButtonCancel: TDButton
    Left = 280
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ComboBoxFrequency: TComboBox
    Left = 360
    Top = 40
    Width = 73
    Height = 21
    AutoComplete = False
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
    TabOrder = 8
    AutoChange = True
    Down = True
  end
  object ButtonSounds: TDButton
    Left = 8
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Sounds'
    TabOrder = 7
    AutoChange = True
    Down = True
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
