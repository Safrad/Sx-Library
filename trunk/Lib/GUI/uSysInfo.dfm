object fSysInfo: TfSysInfo
  Left = 964
  Top = 291
  BorderStyle = bsDialog
  Caption = 'System Info'
  ClientHeight = 297
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 32
    Width = 289
    Height = 9
    Shape = bsTopLine
  end
  object Bevel4: TBevel
    Left = 8
    Top = 161
    Width = 289
    Height = 9
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 104
    Top = 172
    Width = 7
    Height = 71
    Shape = bsLeftLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 192
    Width = 289
    Height = 9
    Shape = bsTopLine
  end
  object Bevel5: TBevel
    Left = 8
    Top = 252
    Width = 289
    Height = 9
    Shape = bsTopLine
  end
  object LabelTOperatingSystem: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 19
    AutoSize = False
    Caption = 'OS Id'
    FocusControl = EditOS
    Transparent = True
    Layout = tlCenter
  end
  object LabelUsed: TLabel
    Left = 112
    Top = 168
    Width = 57
    Height = 19
    AutoSize = False
    Caption = 'Used'
    Transparent = True
    Layout = tlCenter
  end
  object LabelFree: TLabel
    Left = 176
    Top = 168
    Width = 57
    Height = 19
    AutoSize = False
    Caption = 'Free'
    Transparent = True
    Layout = tlCenter
  end
  object LabelTotal: TLabel
    Left = 240
    Top = 168
    Width = 57
    Height = 19
    AutoSize = False
    Caption = 'Total'
    Transparent = True
    Layout = tlCenter
  end
  object LabelTPhysicalMemory: TLabel
    Left = 8
    Top = 200
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'Physical Memory'
    Transparent = True
    Layout = tlCenter
  end
  object LabelTPageFile: TLabel
    Left = 8
    Top = 224
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'Commit Charge'
    Transparent = True
    Layout = tlCenter
  end
  object DLabel3: TLabel
    Left = 8
    Top = 40
    Width = 49
    Height = 19
    AutoSize = False
    Caption = 'CPU Id'
    FocusControl = EditCPU
    Transparent = True
    Layout = tlCenter
  end
  object DLabelCPUFrequency: TLabel
    Left = 8
    Top = 88
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'CPU Frequency'
    FocusControl = EditCPUFrequency
    Transparent = True
    Layout = tlCenter
  end
  object LabelAMDDuronCmp: TLabel
    Left = 8
    Top = 112
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'AMD Duron Cmp'
    FocusControl = EditDuron
    Transparent = True
    Layout = tlCenter
  end
  object DLabelCPUUsage: TLabel
    Left = 8
    Top = 64
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'CPU Usage'
    FocusControl = EditCPUUsage
    Transparent = True
    Layout = tlCenter
  end
  object LabelMBoardCounter: TLabel
    Left = 8
    Top = 136
    Width = 89
    Height = 19
    AutoSize = False
    Caption = 'M-Board Counter'
    FocusControl = EditDuron
    Transparent = True
    Layout = tlCenter
  end
  object edMT: TDEdit
    Left = 240
    Top = 200
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object edMF: TDEdit
    Left = 176
    Top = 200
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object edFF: TDEdit
    Left = 176
    Top = 224
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object edFT: TDEdit
    Left = 240
    Top = 224
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object edMU: TDEdit
    Left = 112
    Top = 200
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 10
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object edFU: TDEdit
    Left = 112
    Top = 224
    Width = 57
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object EditOS: TDEdit
    Left = 64
    Top = 9
    Width = 233
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object EditCPU: TDEdit
    Left = 64
    Top = 41
    Width = 233
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object ButtonOk: TDButton
    Left = 216
    Top = 264
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ButtonOkClick
  end
  object EditCPUFrequency: TDEdit
    Left = 104
    Top = 89
    Width = 193
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object EditDuron: TDEdit
    Left = 104
    Top = 113
    Width = 105
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object EditCPUUsage: TDEdit
    Left = 103
    Top = 65
    Width = 193
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object EditCounter: TDEdit
    Left = 103
    Top = 137
    Width = 193
    Height = 19
    BevelKind = bkSoft
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 12
    DoubleBuffered = True
    ParentDoubleBuffered = False
  end
  object ComboBoxSize: TComboBox
    Left = 216
    Top = 112
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 13
  end
end
