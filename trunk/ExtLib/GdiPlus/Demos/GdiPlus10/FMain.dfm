object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'GDI+ 1.0 Demos'
  ClientHeight = 672
  ClientWidth = 1029
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 329
    Top = 0
    Height = 672
    ExplicitLeft = 284
    ExplicitTop = 36
    ExplicitHeight = 100
  end
  object TreeViewDemos: TTreeView
    Left = 0
    Top = 0
    Width = 329
    Height = 672
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeViewDemosChange
  end
  object PanelClient: TPanel
    Left = 332
    Top = 0
    Width = 697
    Height = 672
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterRight: TSplitter
      Left = 0
      Top = 357
      Width = 697
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 424
    end
    object Pages: TPageControl
      Left = 0
      Top = 0
      Width = 697
      Height = 357
      ActivePage = TabSheetPrint
      Align = alClient
      TabOrder = 0
      object TabSheetGraphic: TTabSheet
        Caption = 'Graphic'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object PaintBox: TPaintBox
          Left = 15
          Top = 15
          Width = 674
          Height = 314
          Align = alClient
          OnPaint = PaintBoxPaint
          ExplicitLeft = 188
          ExplicitTop = 148
          ExplicitWidth = 105
          ExplicitHeight = 105
        end
        object PaintBoxTopRuler: TPaintBox
          Left = 0
          Top = 0
          Width = 689
          Height = 15
          Align = alTop
          OnPaint = PaintBoxTopRulerPaint
        end
        object PaintBoxLeftRuler: TPaintBox
          Left = 0
          Top = 15
          Width = 15
          Height = 314
          Align = alLeft
          OnPaint = PaintBoxLeftRulerPaint
          ExplicitTop = 16
          ExplicitHeight = 628
        end
      end
      object TabSheetText: TTabSheet
        Caption = 'Text'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Memo: TMemo
          Left = 0
          Top = 0
          Width = 689
          Height = 329
          Align = alClient
          BorderStyle = bsNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheetPrint: TTabSheet
        Caption = 'Printer'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object ButtonPrint: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Print'
          TabOrder = 0
          OnClick = ButtonPrintClick
        end
      end
    end
    object RichEdit: TRichEdit
      Left = 0
      Top = 360
      Width = 697
      Height = 312
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object PrintDialog: TPrintDialog
    Left = 424
    Top = 32
  end
end
