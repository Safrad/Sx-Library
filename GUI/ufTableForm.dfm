object fTableForm: TfTableForm
  Left = 351
  Top = 272
  HorzScrollBar.Tracking = True
  HorzScrollBar.Visible = False
  VertScrollBar.Tracking = True
  VertScrollBar.Visible = False
  ClientHeight = 403
  ClientWidth = 432
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DViewTable: TDView
    Left = 0
    Top = 0
    Width = 432
    Height = 382
    Align = alClient
    Zoom = 1.000000000000000000
    EnableZoom = True
    DisplayMode = dmCustom
    PopupMenu = PopupMenu
    TabOrder = 1
    TabStop = False
    OnKeyDown = FormKeyDown
    OnDblClick = DViewTableDblClick
    OnGetDataEx = DViewTableGetDataEx
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 382
    Width = 432
    Height = 21
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 8
    Top = 8
    object Add1: TMenuItem
      Caption = 'Add...'
      OnClick = Add1Click
    end
    object Edit1: TMenuItem
      Caption = 'Edit...'
      OnClick = Edit1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = Delete1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ImportFromCSV1: TMenuItem
      Caption = 'Import From CSV...'
      Enabled = False
      OnClick = ImportFromCSV1Click
    end
    object ExportToCSV1: TMenuItem
      Caption = 'Export To CSV...'
      Enabled = False
      OnClick = ExportToCSV1Click
    end
  end
end
