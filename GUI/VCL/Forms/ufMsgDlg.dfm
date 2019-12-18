object fMsgDlg: TfMsgDlg
  Left = 452
  Top = 340
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 135
  ClientWidth = 439
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
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
    Left = 102
    Top = 35
    Width = 4
    Height = 13
    Alignment = taCenter
    Caption = '/'
    Transparent = True
    Layout = tlCenter
    OnMouseMove = FormMouseMove
  end
  object PanelCount: TDEdit
    Left = 112
    Top = 32
    Width = 33
    Height = 21
    BevelOuter = bvNone
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 6
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
    Caption = ''
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
    Caption = ''
    Displ.Format = '88'
    BevelOuter = bvLowered
    OnMouseMove = FormMouseMove
  end
  object LabelMessage: TDLabel
    Left = 16
    Top = 7
    Width = 129
    Height = 19
    Caption = 'Message'
    FontShadow = 1
    Displ.Format = '88'
    BevelOuter = bvLowered
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object ButtonExit: TDButton
    Left = 16
    Top = 64
    Width = 129
    Height = 23
    Caption = '&Close Program'
    TabOrder = 0
    OnClick = ButtonExitClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object MemoMsg: TRichEdit
    Left = 160
    Top = 48
    Width = 273
    Height = 45
    Color = clBtnFace
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    WantReturns = False
    Zoom = 100
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
    TabOrder = 2
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
    AutoChange = True
  end
  object ButtonLeft: TDButton
    Left = 16
    Top = 32
    Width = 19
    Height = 19
    TabOrder = 3
    OnClick = ButtonLeftClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object EditIndex: TDEdit
    Left = 40
    Top = 32
    Width = 37
    Height = 21
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    OnChange = EditIndexChange
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseMove = FormMouseMove
  end
  object ButtonRight: TDButton
    Tag = 1
    Left = 80
    Top = 32
    Width = 19
    Height = 19
    TabOrder = 5
    OnClick = ButtonLeftClick
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
    IntervalInSeconds = 1.000000000000000000
    OnTimer = Timer1Timer
    Left = 8
    Top = 88
  end
end
