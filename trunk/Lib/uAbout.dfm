object fAbout: TfAbout
  Left = 328
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 424
  ClientWidth = 306
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
  object ImageBackground: TImage
    Left = 0
    Top = 0
    Width = 306
    Height = 424
    Align = alClient
  end
  object Bevel6: TBevel
    Left = 8
    Top = 8
    Width = 289
    Height = 377
    Shape = bsFrame
  end
  object LabelWeb: TDLabel
    Left = 16
    Top = 176
    Width = 57
    Height = 17
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'Web'
    Color = clNone
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
    Top = 152
    Width = 57
    Height = 17
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'E-Mail'
    Color = clNone
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
  object Bevel1: TBevel
    Left = 16
    Top = 256
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 16
    Top = 328
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 112
    Top = 312
    Width = 7
    Height = 65
    Shape = bsLeftLine
  end
  object Bevel4: TBevel
    Left = 16
    Top = 304
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object Bevel5: TBevel
    Left = 16
    Top = 200
    Width = 273
    Height = 9
    Shape = bsTopLine
  end
  object Image1: TImage
    Left = 56
    Top = 153
    Width = 16
    Height = 16
    Cursor = crHandPoint
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
    Top = 177
    Width = 16
    Height = 16
    Cursor = crHandPoint
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
  object LabelTPhysicalMemory: TDLabel
    Left = 16
    Top = 336
    Width = 89
    Height = 17
    AutoSize = False
    Caption = 'Physical Memory'
    Color = clNone
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
  object LabelTPageFile: TDLabel
    Left = 16
    Top = 360
    Width = 89
    Height = 17
    AutoSize = False
    Caption = 'Page File'
    Color = clNone
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
  object LabelRunCount: TDLabel
    Left = 16
    Top = 208
    Width = 56
    Height = 17
    AutoSize = False
    Caption = 'Run Count'
    Color = clNone
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
    Left = 128
    Top = 232
    Width = 80
    Height = 17
    AutoSize = False
    Caption = 'Now Run Time'
    Color = clNone
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
    Left = 128
    Top = 208
    Width = 80
    Height = 17
    AutoSize = False
    Caption = 'Total Run Time'
    Color = clNone
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
  object LabelTotal: TDLabel
    Left = 240
    Top = 308
    Width = 49
    Height = 17
    AutoSize = False
    Caption = 'Total'
    Color = clNone
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
  object LabelFree: TDLabel
    Left = 184
    Top = 308
    Width = 49
    Height = 17
    AutoSize = False
    Caption = 'Free'
    Color = clNone
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
  object LabelUsed: TDLabel
    Left = 128
    Top = 308
    Width = 49
    Height = 17
    AutoSize = False
    Caption = 'Used'
    Color = clNone
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
  object LabelTOperatingSystem: TDLabel
    Left = 16
    Top = 260
    Width = 89
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Operating System'
    FocusControl = EditOS
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
  object LabelAuthor: TDLabel
    Left = 16
    Top = 128
    Width = 56
    Height = 17
    AutoSize = False
    Caption = 'Author'
    Color = clNone
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
    Color = clNone
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
  object BitBtnOk: TDBitBtn
    Left = 224
    Top = 392
    Width = 73
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
    OnClick = BitBtnOkClick
    Glyph.Data = {
      76030000424D7603000000000000760000002800000010000000100000000100
      1800000000000003000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00800080800080
      8000808000808000008000008000808000808000808000808000808000808000
      8080008080008080008080008080008080008080000000800000800080000080
      0080800080800080800080800080800080800080800080800080800080800080
      8000000080000080000080000080008000008000808000808000808000808000
      8080008080008080008080008080000000800000800000800000800000800000
      8000800000800080800080800080800080800080800080800080800000008000
      00800000800000FF0000FF000080000080000080008000008000808000808000
      8080008080008080008000800000800000800000FF0080008080008000FF0000
      800000800000800080000080008080008080008080008080008000FF00008000
      00FF0080008080008080008080008000FF000080000080000080008000008000
      8080008080008080008080008000FF0080008080008080008080008080008080
      008000FF00008000008000008000800000800080800080800080800080800080
      80008080008080008080008080008080008080008000FF000080000080000080
      0080000080008080008080008080008080008080008080008080008080008080
      008080008080008000FF00008000008000008000800000800080800080800080
      80008080008080008080008080008080008080008080008080008000FF000080
      0000800000800080000080008080008080008080008080008080008080008080
      008080008080008080008080008000FF00008000008000800000800080800080
      8000808000808000808000808000808000808000808000808000808000808000
      8000FF0000800000800080008080008080008080008080008080008080008080
      008080008080008080008080008080008080008000FF00800080800080800080
      8000808000808000808000808000808000808000808000808000808000808000
      8080008080008080008080008080008080008080008080008080008080008080
      0080800080800080800080800080800080800080800080800080}
  end
  object PMT: TDPanel
    Left = 240
    Top = 336
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 13
  end
  object PMF: TDPanel
    Left = 184
    Top = 336
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 14
  end
  object PFT: TDPanel
    Left = 240
    Top = 360
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 17
  end
  object PFF: TDPanel
    Left = 184
    Top = 360
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 16
  end
  object PMU: TDPanel
    Left = 128
    Top = 336
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 12
  end
  object PFU: TDPanel
    Left = 128
    Top = 360
    Width = 49
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 15
  end
  object PanelBuild: TDPanel
    Left = 192
    Top = 96
    Width = 97
    Height = 17
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = '10/1999-01/2000'
    ParentColor = True
    TabOrder = 4
  end
  object PanelImage: TDPanel
    Left = 16
    Top = 16
    Width = 100
    Height = 100
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 1
    object ImageAbout: TImage
      Left = 0
      Top = 0
      Width = 96
      Height = 96
      Align = alClient
      AutoSize = True
      ParentShowHint = False
      ShowHint = False
      OnMouseDown = ImageAboutMouseDown
    end
  end
  object PanelRC: TDPanel
    Left = 72
    Top = 208
    Width = 45
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = '999 999'
    ParentColor = True
    TabOrder = 8
  end
  object PanelTRT: TDPanel
    Left = 208
    Top = 208
    Width = 81
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = '0000:00:00.000'
    ParentColor = True
    TabOrder = 9
  end
  object PanelNRT: TDPanel
    Left = 208
    Top = 232
    Width = 81
    Height = 17
    Alignment = taRightJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = '0000:00:00.000'
    ParentColor = True
    TabOrder = 10
  end
  object EditOS: TEdit
    Left = 16
    Top = 280
    Width = 273
    Height = 19
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 11
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
    object ImageName: TImage
      Left = 0
      Top = 0
      Width = 149
      Height = 29
      Align = alClient
      AutoSize = True
      ParentShowHint = False
      ShowHint = False
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
    object ImageVersion: TImage
      Left = 0
      Top = 0
      Width = 149
      Height = 13
      Align = alClient
      AutoSize = True
      ParentShowHint = False
      ShowHint = False
    end
  end
  object EditAuthor: TEdit
    Left = 80
    Top = 128
    Width = 97
    Height = 19
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
    Text = 'Safranek David'
  end
  object EditWeb: TEdit
    Left = 80
    Top = 176
    Width = 209
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
    Text = 'http://safrad1.fbi.cz'
    OnClick = EditWebClick
  end
  object EditEmail: TEdit
    Left = 80
    Top = 152
    Width = 209
    Height = 19
    Cursor = crHandPoint
    AutoSize = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
    OnClick = EditEmailClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 8
    Top = 392
  end
end
