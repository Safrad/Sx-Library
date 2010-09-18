object fAbout: TfAbout
  Left = 180
  Top = 160
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 320
  ClientWidth = 304
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel6: TBevel
    Left = 8
    Top = 8
    Width = 289
    Height = 273
    Shape = bsFrame
  end
  object LabelIcq: TDLabel
    Left = 16
    Top = 152
    Width = 57
    Height = 17
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'Icq'
    Color = clBtnFace
    FocusControl = EditEmail
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditIcqClick
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelWeb: TDLabel
    Left = 16
    Top = 200
    Width = 57
    Height = 17
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'Web'
    Color = clBtnFace
    FocusControl = EditWeb
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditWebClick
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelEMail: TDLabel
    Left = 16
    Top = 176
    Width = 57
    Height = 17
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'E-Mail'
    Color = clBtnFace
    FocusControl = EditEmail
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    OnClick = EditEmailClick
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object Bevel5: TBevel
    Left = 16
    Top = 224
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object Image1: TImage
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
    OnClick = EditEmailClick
  end
  object Image2: TImage
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
  object LabelRunCount: TDLabel
    Left = 16
    Top = 232
    Width = 89
    Height = 17
    AutoSize = False
    Caption = 'Run:'
    Color = clBtnFace
    FocusControl = PanelRC
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelNowRunTime: TDLabel
    Left = 112
    Top = 256
    Width = 33
    Height = 17
    AutoSize = False
    Caption = 'Now'
    Color = clBtnFace
    FocusControl = PanelNRT
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelTotalRunTime: TDLabel
    Left = 112
    Top = 232
    Width = 33
    Height = 17
    AutoSize = False
    Caption = 'Total'
    Color = clBtnFace
    FocusControl = PanelTRT
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelAuthor: TDLabel
    Left = 16
    Top = 128
    Width = 56
    Height = 17
    AutoSize = False
    Caption = 'Author'
    Color = clBtnFace
    FocusControl = EditAuthor
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object LabelBuild: TDLabel
    Left = 136
    Top = 96
    Width = 56
    Height = 17
    AutoSize = False
    Caption = 'Build'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object Image3: TImage
    Left = 56
    Top = 153
    Width = 16
    Height = 16
    Cursor = crHandPoint
    AutoSize = True
    Transparent = True
    OnClick = EditEmailClick
  end
  object Image4: TImage
    Left = 56
    Top = 129
    Width = 16
    Height = 16
    AutoSize = True
    Transparent = True
  end
  object DLabel1: TDLabel
    Left = 16
    Top = 256
    Width = 33
    Height = 17
    AutoSize = False
    Caption = 'Count'
    Color = clBtnFace
    FocusControl = PanelRC
    ParentColor = False
    Transparent = True
    Layout = tlCenter
    BackEffect = ef04
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
    BevelOuter = bvLowered
  end
  object PanelImage: TDPanel
    Left = 16
    Top = 16
    Width = 113
    Height = 105
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 1
    object ImageAbout: TDImage
      Left = 0
      Top = 0
      Width = 109
      Height = 101
      DrawFPS = False
      HandScroll = False
      HotTrack = True
      OnFill = ImageAboutFill
      Align = alClient
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      TabStop = False
      OnMouseDown = ImageAboutMouseDown
      OnMouseMove = ImageAboutMouseMove
    end
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
  object PanelBuild: TEdit
    Left = 192
    Top = 96
    Width = 97
    Height = 21
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 4
    Text = '10/1999-01/2000'
  end
  object PanelRC: TEdit
    Left = 56
    Top = 256
    Width = 45
    Height = 21
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 8
    Text = '000 000'
  end
  object PanelTRT: TEdit
    Left = 152
    Top = 232
    Width = 137
    Height = 21
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 9
    Text = '356 days, 00:00:00.000'
  end
  object PanelNRT: TEdit
    Left = 152
    Top = 256
    Width = 137
    Height = 21
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 10
    Text = '40 days, 00:00.000'
  end
  object PanelName: TDPanel
    Left = 136
    Top = 16
    Width = 153
    Height = 33
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Program Name'
    ParentColor = True
    TabOrder = 2
    object ImageName: TDImage
      Left = 0
      Top = 0
      Width = 149
      Height = 29
      DrawFPS = False
      HandScroll = False
      HotTrack = True
      OnFill = ImageNameFill
      Align = alClient
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      TabStop = False
    end
  end
  object PenelVersion: TDPanel
    Left = 136
    Top = 64
    Width = 153
    Height = 17
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Program Version'
    ParentColor = True
    TabOrder = 3
    object ImageVersion: TDImage
      Left = 0
      Top = 0
      Width = 149
      Height = 13
      DrawFPS = False
      HandScroll = False
      HotTrack = True
      OnFill = ImageVersionFill
      Align = alClient
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      TabStop = False
    end
  end
  object EditAuthor: TEdit
    Left = 80
    Top = 128
    Width = 89
    Height = 19
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
    Text = 'Safranek David'
  end
  object EditWeb: TEdit
    Left = 80
    Top = 200
    Width = 209
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
    Text = 'http://safrad.webzdarma.cz'
    OnClick = EditWebClick
  end
  object EditEmail: TEdit
    Left = 80
    Top = 176
    Width = 209
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
    OnClick = EditEmailClick
  end
  object EditIcq: TEdit
    Left = 80
    Top = 152
    Width = 65
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 11
    Text = '69941919'
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
    TabOrder = 12
    OnClick = SysInfo1Click
  end
  object DButtonMemoryStatus: TDButton
    Left = 104
    Top = 288
    Width = 97
    Height = 25
    Caption = 'Memory Status...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = DButtonMemoryStatusClick
  end
  object Timer1: TDTimer
    ActiveOnly = False
    Enabled = False
    Interval = 15
    EventStep = esCPU
    OnTimer = DTimer1Timer
    Left = 24
    Top = 24
  end
end
