object fIOError: TfIOError
  Left = 323
  Top = 256
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 135
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClick = FormClick
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseMove = FormMouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 4
    Width = 145
    Height = 93
  end
  object Image: TImage
    Left = 160
    Top = 8
    Width = 32
    Height = 32
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object LabelX: TLabel
    Left = 96
    Top = 32
    Width = 16
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = '/'
    Transparent = True
    Layout = tlCenter
    OnMouseMove = FormMouseMove
  end
  object PanelCount: TDLabel
    Left = 112
    Top = 32
    Width = 33
    Height = 19
    Alignment = taRightJustify
    Caption = '9,999'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvNone
    BorderStyle = bsSingle
    OnMouseMove = FormMouseMove
  end
  object LabelTimeLeft: TDLabel
    Left = 200
    Top = 8
    Width = 73
    Height = 17
    Caption = 'Time to Close:'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
    OnClick = LabelTimeLeftClick
    OnMouseMove = FormMouseMove
  end
  object PanelTimeLeft: TDLabel
    Left = 200
    Top = 24
    Width = 73
    Height = 17
    Alignment = taRightJustify
    Displ.Format = '88'
    BevelOuter = bvLowered
    OnClick = LabelTimeLeftClick
    OnMouseMove = FormMouseMove
  end
  object LabelCreated: TDLabel
    Left = 280
    Top = 8
    Width = 153
    Height = 17
    Caption = 'Created:'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object PanelCreated: TDLabel
    Left = 280
    Top = 24
    Width = 153
    Height = 17
    Alignment = taRightJustify
    Displ.Format = '88'
    BevelOuter = bvLowered
    OnMouseMove = FormMouseMove
  end
  object LabelMessage: TDLabel
    Left = 16
    Top = 8
    Width = 129
    Height = 19
    Caption = 'Message'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object ButtonRetry: TDButton
    Left = 256
    Top = 104
    Width = 81
    Height = 23
    Caption = '&Retry'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ButtonRetryClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonIgnore: TDButton
    Left = 160
    Top = 104
    Width = 81
    Height = 23
    Cancel = True
    Caption = '&Ignore'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonIgnoreClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonIgnoreAll: TDButton
    Left = 400
    Top = 0
    Width = 81
    Height = 23
    Caption = '&Ignore &All'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
    OnClick = ButtonIgnoreAllClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonExit: TDButton
    Left = 16
    Top = 64
    Width = 129
    Height = 23
    Caption = '&Close Program'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonExitClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonOpen: TDButton
    Left = 352
    Top = 104
    Width = 81
    Height = 23
    Caption = '&File...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonOpenClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object MemoMsg: TMemo
    Left = 160
    Top = 48
    Width = 273
    Height = 45
    Color = clBtnFace
    Lines.Strings = (
      '1'
      '2'
      '3')
    ReadOnly = True
    TabOrder = 5
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonAll: TDButton
    Left = 8
    Top = 103
    Width = 145
    Height = 23
    Caption = '&Use Answer for All'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
    AutoChange = True
  end
  object ButtonDown: TDButton
    Left = 16
    Top = 32
    Width = 19
    Height = 19
    Caption = '&Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = ButtonDownClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object EditIndex: TEdit
    Left = 40
    Top = 32
    Width = 37
    Height = 21
    TabOrder = 8
    Text = '9,999'
    OnChange = EditIndexChange
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonUp: TDButton
    Tag = 1
    Left = 80
    Top = 32
    Width = 19
    Height = 19
    Caption = '&Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = ButtonDownClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object OpenDialogFile: TOpenDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Title = 'Select file'
    Left = 472
    Top = 144
  end
  object Timer1: TDTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 8
    Top = 88
  end
end
