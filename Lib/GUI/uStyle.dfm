object fStyle: TfStyle
  Left = 274
  Top = 157
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Custom Graphics Style'
  ClientHeight = 202
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 160
    Width = 265
    Height = 9
    Shape = bsTopLine
  end
  object LabelGraphicStyle: TLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 21
    AutoSize = False
    Caption = '&Style'
    FocusControl = ComboBoxStyles
    Transparent = True
    Layout = tlCenter
  end
  object LabelTransparentEffect: TLabel
    Left = 8
    Top = 40
    Width = 89
    Height = 21
    AutoSize = False
    Caption = '&Transpar. Effect'
    FocusControl = ComboBoxEffect
    Transparent = True
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'B&order Size'
    FocusControl = ComboBoxLineSize
    Transparent = True
    Layout = tlCenter
  end
  object LabelGenEffect: TLabel
    Left = 8
    Top = 96
    Width = 105
    Height = 21
    AutoSize = False
    Caption = '&Generated Effects'
    FocusControl = ComboBoxGenEffect
    Transparent = True
    Layout = tlCenter
  end
  object ComboBoxStyles: TComboBox
    Left = 40
    Top = 8
    Width = 113
    Height = 21
    Style = csDropDownList
    DropDownCount = 24
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxStylesChange
  end
  object ButtonOk: TDButton
    Left = 8
    Top = 168
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 96
    Top = 168
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ButtonApply: TDButton
    Left = 184
    Top = 168
    Width = 81
    Height = 25
    Caption = '&Apply'
    TabOrder = 3
    OnClick = ButtonApplyClick
  end
  object ButtonBrushColor: TDButton
    Left = 8
    Top = 128
    Width = 81
    Height = 25
    Caption = '&Brush Color...'
    TabOrder = 4
    OnClick = ButtonBrushColorClick
  end
  object ButtonSelectFile: TDButton
    Left = 184
    Top = 128
    Width = 81
    Height = 25
    Caption = '&Image...'
    TabOrder = 5
    OnClick = ButtonSelectFileClick
  end
  object ButtonPenColor: TDButton
    Tag = 1
    Left = 96
    Top = 128
    Width = 81
    Height = 25
    Caption = '&Pen Color...'
    TabOrder = 6
    OnClick = ButtonBrushColorClick
  end
  object ComboBoxEffect: TComboBox
    Left = 96
    Top = 40
    Width = 57
    Height = 21
    Style = csDropDownList
    DropDownCount = 24
    ItemHeight = 13
    TabOrder = 7
    OnChange = FormToData
  end
  object ComboBoxLineSize: TComboBox
    Left = 96
    Top = 64
    Width = 57
    Height = 21
    DropDownCount = 24
    ItemHeight = 13
    TabOrder = 8
    OnChange = FormToData
  end
  object ImageSample: TDImage
    Left = 160
    Top = 8
    Width = 104
    Height = 78
    Zoom = 1.000000000000000000
    OnFill = ImageSampleFill
    TabOrder = 9
    TabStop = False
  end
  object ComboBoxGenEffect: TComboBox
    Left = 112
    Top = 96
    Width = 153
    Height = 21
    Style = csDropDownList
    DropDownCount = 24
    ItemHeight = 13
    TabOrder = 10
    OnChange = FormToData
  end
end
