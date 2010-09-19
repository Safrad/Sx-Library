object fAbout: TfAbout
  Left = 423
  Top = 273
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 320
  ClientWidth = 304
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelIcq: TLabel
    Left = 200
    Top = 152
    Width = 25
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'ICQ'
    Color = clBtnFace
    FocusControl = EditEMail
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditIcqClick
  end
  object LabelEMail: TLabel
    Left = 16
    Top = 176
    Width = 57
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'E-Mail'
    Color = clBtnFace
    FocusControl = EditEMail
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditEMailClick
  end
  object LabelWeb: TLabel
    Left = 16
    Top = 200
    Width = 57
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'Web'
    Color = clBtnFace
    FocusControl = EditWeb
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditWebClick
  end
  object LabelAuthor: TLabel
    Left = 16
    Top = 152
    Width = 57
    Height = 19
    AutoSize = False
    Caption = 'Author'
    FocusControl = EditAuthor
    Transparent = True
    Layout = tlCenter
  end
  object Bevel: TBevel
    Left = 8
    Top = 8
    Width = 289
    Height = 273
    Shape = bsFrame
  end
  object BevelSep: TBevel
    Left = 16
    Top = 224
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object ImageEMail: TImage
    Left = 56
    Top = 177
    Width = 16
    Height = 16
    Cursor = crHandPoint
    AutoSize = True
    Picture.Data = {
      07544269746D6170F6000000424DF60000000000000076000000280000001000
      000010000000010004000000000080000000C40E0000C40E0000100000000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00555555555555555555555555555555555555555555555555500000000000
      0005588888888888880558BFFFBFFFBFF805583FBF333FFF380558B3F30003B3
      F80558FF30FFB03FB80558B80FBFFF03F8055880BFFFBFF03805580FFFBFFFBF
      030558FFBFFFBFFFB00558888888888888055555555555555555555555555555
      5555}
    Transparent = True
    OnClick = EditEMailClick
  end
  object ImageWeb: TImage
    Left = 56
    Top = 201
    Width = 16
    Height = 16
    Cursor = crHandPoint
    AutoSize = True
    Picture.Data = {
      07544269746D6170F6000000424DF60000000000000076000000280000001000
      000010000000010004000000000080000000C40E0000C40E0000100000000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF005555555555555019555580000085019955500CC22C001995550CCCC00001
      995550CCCC08FF80905580CCC08FEFF800850CCCC0FEFEFF0C050CCC20EFEFEF
      0C050CC2208FFEF802050CC22208EF80C2050CCF22200002220558CCFCCCCCC2
      208558CFCFC222222055558CCCFC2222055555588CCFC2005555555558888885
      5555}
    Transparent = True
    OnClick = EditWebClick
  end
  object LabelRunCount: TLabel
    Left = 16
    Top = 232
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'Run:'
    Color = clBtnFace
    FocusControl = PanelRC
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelNowRunTime: TLabel
    Left = 112
    Top = 232
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Now'
    Color = clBtnFace
    FocusControl = PanelNRT
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelTotalRunTime: TLabel
    Left = 112
    Top = 256
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Total'
    Color = clBtnFace
    FocusControl = PanelTRT
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelFirstRelease: TLabel
    Left = 152
    Top = 104
    Width = 65
    Height = 18
    AutoSize = False
    Caption = 'First Release'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelCount: TLabel
    Left = 16
    Top = 256
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'Count'
    Color = clBtnFace
    FocusControl = PanelRC
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelRelease: TLabel
    Left = 152
    Top = 129
    Width = 65
    Height = 18
    AutoSize = False
    Caption = 'Release'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object LabelVersion: TLabel
    Left = 152
    Top = 80
    Width = 65
    Height = 18
    AutoSize = False
    Caption = 'Version'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    Layout = tlCenter
  end
  object ButtonOk: TDButton
    Left = 216
    Top = 288
    Width = 81
    Height = 25
    Cancel = True
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
  object EditFirstRelease: TDEdit
    Left = 224
    Top = 104
    Width = 65
    Height = 18
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    TabOrder = 4
  end
  object PanelRC: TDEdit
    Left = 56
    Top = 256
    Width = 45
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    TabOrder = 11
  end
  object PanelTRT: TDEdit
    Left = 152
    Top = 256
    Width = 137
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    TabOrder = 12
  end
  object PanelNRT: TDEdit
    Left = 152
    Top = 232
    Width = 137
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    TabOrder = 10
  end
  object EditAuthor: TDEdit
    Left = 56
    Top = 152
    Width = 137
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
  object EditWeb: TDEdit
    Left = 80
    Top = 200
    Width = 209
    Height = 19
    Cursor = crHandPoint
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 9
    OnClick = EditWebClick
  end
  object EditEMail: TDEdit
    Left = 80
    Top = 176
    Width = 209
    Height = 19
    Cursor = crHandPoint
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 8
    OnClick = EditEMailClick
  end
  object EditIcq: TDEdit
    Left = 224
    Top = 152
    Width = 65
    Height = 19
    Cursor = crHandPoint
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
    OnClick = EditIcqClick
  end
  object SysInfo1: TDButton
    Left = 8
    Top = 288
    Width = 81
    Height = 25
    Caption = '&System Info...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = SysInfo1Click
  end
  object DButtonMemoryStatus: TDButton
    Left = 104
    Top = 288
    Width = 97
    Height = 25
    Caption = '&Memory Status...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = DButtonMemoryStatusClick
  end
  object ImageAbout: TDImage
    Left = 16
    Top = 16
    Width = 129
    Height = 129
    Zoom = 1.000000000000000000
    OnFill = ImageAboutFill
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    TabStop = False
    OnMouseDown = ImageAboutMouseDown
    OnMouseMove = ImageAboutMouseMove
  end
  object ImageName: TDImage
    Left = 152
    Top = 16
    Width = 137
    Height = 57
    Zoom = 1.000000000000000000
    OnFill = ImageNameFill
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    TabStop = False
  end
  object EditRelease: TDEdit
    Left = 224
    Top = 129
    Width = 65
    Height = 18
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    TabOrder = 5
  end
  object EditVersion: TDEdit
    Left = 200
    Top = 80
    Width = 89
    Height = 18
    BevelKind = bkSoft
    BiDiMode = bdLeftToRight
    BorderStyle = bsNone
    ParentBiDiMode = False
    ParentColor = True
    TabOrder = 3
  end
  object Timer1: TDTimer
    ActiveOnly = True
    Enabled = False
    Interval = 25
    EventStep = esFrequency
    OnTimer = DTimer1Timer
    Left = 24
    Top = 24
  end
end
