object fStyle: TfStyle
  Left = 246
  Top = 176
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Custom Graphics Style'
  ClientHeight = 179
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 136
    Width = 249
    Height = 9
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 21
    AutoSize = False
    Caption = 'Style'
    Transparent = True
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 57
    Height = 21
    AutoSize = False
    Caption = 'Effect'
    Transparent = True
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 65
    Height = 21
    AutoSize = False
    Caption = 'Border Size'
    Transparent = True
    Layout = tlCenter
  end
  object ComboBoxStyles: TComboBox
    Left = 72
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    DropDownCount = 16
    ItemHeight = 13
    TabOrder = 0
    OnChange = FormToData
  end
  object ButtonOk: TDButton
    Left = 8
    Top = 144
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
    TabOrder = 1
  end
  object ButtonCancel: TDButton
    Left = 184
    Top = 144
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
    TabOrder = 2
  end
  object ButtonApply: TDButton
    Left = 96
    Top = 144
    Width = 73
    Height = 25
    Caption = '&Apply'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
  object ButtonColor0: TDButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Color...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonColor0Click
  end
  object ButtonSelectFile: TDButton
    Left = 184
    Top = 104
    Width = 75
    Height = 25
    Caption = 'File...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object ButtonColor1: TDButton
    Tag = 1
    Left = 96
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Color...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = ButtonColor0Click
  end
  object ComboBoxEffect: TComboBox
    Left = 72
    Top = 40
    Width = 73
    Height = 21
    Style = csDropDownList
    DropDownCount = 24
    ItemHeight = 13
    TabOrder = 7
    OnChange = FormToData
  end
  object ComboBoxLineSize: TComboBox
    Left = 72
    Top = 72
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 8
    OnChange = FormToData
  end
end
