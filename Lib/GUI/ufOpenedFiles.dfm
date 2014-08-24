object fOpenedFiles: TfOpenedFiles
  Left = 557
  Top = 169
  Width = 289
  Height = 439
  BorderIcons = []
  Caption = 'Opened Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = DViewOpenedFilesKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DViewOpenedFiles: TDView
    Left = 0
    Top = 0
    Width = 281
    Height = 412
    Zoom = 1.000000000000000000
    HotTrack = False
    Align = alClient
    TabOrder = 0
    OnKeyDown = DViewOpenedFilesKeyDown
    OnKeyPress = DViewOpenedFilesKeyPress
    OnKeyUp = DViewOpenedFilesKeyUp
    OnGetData = DViewOpenedFilesGetData
    OnColumnClick = DViewOpenedFilesColumnClick
  end
end
