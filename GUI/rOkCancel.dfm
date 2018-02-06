object FrameOkCancel: TFrameOkCancel
  Left = 0
  Top = 0
  Width = 319
  Height = 44
  TabOrder = 0
  OnResize = FrameResize
  object Bevel: TBevel
    Left = 0
    Top = 0
    Width = 319
    Height = 2
    Align = alTop
  end
  object ButtonOk: TDButton
    Left = 144
    Top = 8
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
  end
  object ButtonCancel: TDButton
    Left = 232
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
  end
end
