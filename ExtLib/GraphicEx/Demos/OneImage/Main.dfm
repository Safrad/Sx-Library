object MainForm: TMainForm
  Left = 192
  Top = 117
  Width = 889
  Height = 563
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 240
    Top = 248
    Width = 417
    Height = 249
  end
  object memoErr: TMemo
    Left = 16
    Top = 248
    Width = 217
    Height = 249
    Lines.Strings = (
      'Errorrs:')
    TabOrder = 0
  end
  object cbEnableExtension: TCheckBox
    Left = 240
    Top = 504
    Width = 113
    Height = 17
    Caption = 'Enable extension'
    TabOrder = 1
    OnClick = cbEnableExtensionClick
  end
  object PanelBig: TPanel
    Left = 0
    Top = 0
    Width = 881
    Height = 241
    Align = alTop
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 322
      Top = 1
      Width = 7
      Height = 239
      Cursor = crHSplit
      ResizeStyle = rsUpdate
    end
    object PanelTree: TPanel
      Left = 1
      Top = 1
      Width = 321
      Height = 239
      Align = alLeft
      TabOrder = 0
      DesignSize = (
        321
        239)
      object TreeView: TTreeView
        Left = 0
        Top = 0
        Width = 313
        Height = 185
        Images = ImageList1
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = TreeViewChange
        OnExpanding = TreeViewExpanding
      end
      object edDir: TEdit
        Left = 8
        Top = 192
        Width = 257
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object btnChooseDir: TBitBtn
        Left = 272
        Top = 190
        Width = 33
        Height = 25
        Caption = '...'
        TabOrder = 2
        OnClick = btnChooseDirClick
      end
    end
    object Panel1: TPanel
      Left = 329
      Top = 1
      Width = 551
      Height = 239
      Align = alClient
      TabOrder = 1
      DesignSize = (
        551
        239)
      object ListView: TListView
        Left = 8
        Top = 0
        Width = 265
        Height = 161
        Columns = <
          item
            Caption = 'Name'
            Width = 170
          end
          item
            Alignment = taRightJustify
            Caption = 'Size'
            Width = 80
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ListViewChange
      end
      object cbOnlyHandledExtensions: TCheckBox
        Left = 8
        Top = 168
        Width = 177
        Height = 17
        Caption = 'Show only handled extensions'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbOnlyHandledExtensionsClick
      end
      object edImagePath: TEdit
        Left = 8
        Top = 192
        Width = 537
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
    end
  end
  object ImageList1: TImageList
    Left = 472
    Top = 264
    Bitmap = {
      494C010101000400040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CFCFCF00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00D0D0D00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004F9DD3004398D2004094D0003E92
      CF003E92CE003F92CE003F92CE003F92CE003F92CE003F92CE003F92CE003F92
      CE003F93CF004C9AD100F1F1F100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004499D2003F94D000ABFBFF009BF3
      FF0092F1FF0093F1FF0093F1FF0093F1FF0093F1FF0093F1FF0093F1FF0093F1
      FF00A6F8FF0065B8E300B2C9DA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004398D2004FA6D9008EDAF500A2EE
      FF0082E5FE0084E5FE0084E5FE0085E6FE0085E6FE0085E6FE0085E6FE0084E6
      FE0096EBFF008CD8F50070A7CE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004296D1006BBEE8006DBDE600BBF2
      FF0075DEFD0077DEFC0078DEFC007BDFFC007DDFFC007DDFFC007DDFFC007CDF
      FC0080E0FD00ADF0FF004D9DD300F1F1F1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004095D0008AD7F50044A1D800DDFD
      FF00DAFAFF00DBFAFF00DEFAFF0074DCFC0076DBFA0075DAFA0074DAFA0074DA
      FA0072D9FA00A1E8FF007CBFE600B2CADA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003E94D000ABF0FF00449DD600368C
      CB00368CCB00368CCB00378BCB005CBEEA006FD9FB006AD6FA0068D5F90067D4
      F90066D4F90082DEFC00AAE0F6006FA6CE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D92CF00B9F4FF0073DBFB006BCC
      F2006CCDF3006CCEF3006DCEF300479CD40056BAE900DAF8FF00D7F6FF00D6F6
      FF00D5F6FF00D5F7FF00DBFCFF003E94D0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003C92CF00C0F3FF0071DAFB0074DB
      FB0075DBFC0075DBFC0076DCFC0073DAFA00449CD400378CCB00368CCB00358C
      CC00348DCC003890CE003D94D00052A0D5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003B92CF00CAF6FF0069D5F9006CD5
      F9006BD5F90069D5F90069D5FA006AD7FB0068D4FA005EC7F1005EC7F2005DC8
      F200B4E3F8003D94D000B0D1E700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003B92CF00D5F7FF0060D1F90061D0
      F800B4EBFD00D9F6FF00DAF8FF00DAF8FF00DBF9FF00DCFAFF00DCFAFF00DCFB
      FF00E0FFFF003E95D000D9EAF600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D94D000DCFCFF00D8F7FF00D8F7
      FF00DBFAFF00358ECD003991CE003A92CF003A92CF003A92CF003A92CF003B92
      CF003D94D00061A8D90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007DB8E0003D94D0003A92CF003A92
      CF003D94D00063A9D90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF0000000000000003000000000000
      0001000000000000000100000000000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00010000000000000001000000000000000300000000000003FF000000000000
      FFFF000000000000FFFF000000000000}
  end
end
