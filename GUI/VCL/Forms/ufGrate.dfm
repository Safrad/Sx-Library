object fGrate: TfGrate
  Left = 656
  Top = 585
  BorderStyle = bsSingle
  Caption = 'Grate'
  ClientHeight = 415
  ClientWidth = 601
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 8
    object Color1: TMenuItem
      Caption = 'Grate Color...'
      OnClick = Color1Click
    end
    object BackgroundColor1: TMenuItem
      Caption = 'Background Color...'
      OnClick = BackgroundColor1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ShowGrate1: TMenuItem
      Caption = 'Grate'
      Checked = True
      OnClick = ShowGrate1Click
    end
    object Size1: TMenuItem
      Caption = '|- Grate Size...'
      OnClick = Size1Click
    end
    object Centered1: TMenuItem
      Caption = '|-  Centered'
      Checked = True
      OnClick = Centered1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FullScreen1: TMenuItem
      Caption = 'Full Screen'
      Checked = True
      ShortCut = 32781
      OnClick = FullScreen1Click
    end
    object Close1: TMenuItem
      Caption = 'Close'
      OnClick = Close1Click
    end
  end
end
