object fFileExt: TfFileExt
  Left = 355
  Top = 297
  Width = 448
  Height = 442
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object DViewFileExtensions: TDView
    Left = 0
    Top = 0
    Width = 440
    Height = 415
    Zoom = 1.000000000000000000
    Align = alClient
    PopupMenu = PopupMenuFE
    TabOrder = 0
    TabStop = False
    OnKeyDown = FormKeyDown
    OnGetData = DViewFileExtensionsGetData
  end
  object PopupMenuFE: TPopupMenu
    OnPopup = PopupMenuFEPopup
    Left = 8
    Top = 8
    object Register1: TMenuItem
      Caption = 'Register'
      OnClick = Register1Click
    end
    object Unregister1: TMenuItem
      Tag = 1
      Caption = 'Unregister'
      OnClick = Register1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object RegisterAll1: TMenuItem
      Tag = 2
      Caption = 'Register All'
      OnClick = Register1Click
    end
    object UnregisterAll1: TMenuItem
      Tag = 3
      Caption = 'Unregister All'
      OnClick = Register1Click
    end
  end
end
