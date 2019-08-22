object fOptions: TfOptions
  Left = 586
  Top = 268
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Options'
  ClientHeight = 307
  ClientWidth = 415
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 34
    Width = 369
    Height = 8
    Shape = bsTopLine
  end
  object LabelTemplate: TLabel
    Left = 8
    Top = 12
    Width = 44
    Height = 13
    Caption = '&Template'
    FocusControl = ComboBoxTemplate
    Transparent = True
    OnClick = LabelTemplateClick
  end
  object Bevel2: TBevel
    Left = 7
    Top = 264
    Width = 369
    Height = 8
    Shape = bsTopLine
  end
  object ButtonOk: TDButton
    Left = 136
    Top = 272
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 224
    Top = 272
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonApply: TDButton
    Left = 312
    Top = 272
    Width = 81
    Height = 25
    Caption = '&Apply'
    TabOrder = 2
    OnClick = ButtonApplyClick
  end
  object ComboBoxTemplate: TComboBox
    Left = 64
    Top = 8
    Width = 217
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBoxTemplateChange
  end
  object DButton1: TDButton
    Left = 288
    Top = 8
    Width = 41
    Height = 21
    Caption = '&Add'
    Enabled = False
    TabOrder = 4
    Visible = False
  end
  object DButton2: TDButton
    Left = 336
    Top = 8
    Width = 49
    Height = 21
    Caption = '&Delete'
    Enabled = False
    TabOrder = 5
    Visible = False
  end
end
