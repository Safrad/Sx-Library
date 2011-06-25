object fStatus: TfStatus
  Left = 903
  Top = 715
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Status'
  ClientHeight = 117
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DGauge: TDGauge
    Left = 0
    Top = 0
    Width = 275
    Height = 25
    FontShadow = 1
    Displ.Format = '88'
    Max = 1024
    Color = clAppWorkSpace
  end
  object ButtonStop: TDButton
    Left = 177
    Top = 85
    Width = 89
    Height = 25
    Caption = '&Stop'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object ButtonPause: TDButton
    Left = 177
    Top = 57
    Width = 89
    Height = 25
    Caption = '&Pause'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonPauseClick
  end
  object EditElapsedTime: TLabeledEdit
    Left = 80
    Top = 32
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'Elapsed Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 3
  end
  object EditRemainTime: TLabeledEdit
    Left = 80
    Top = 56
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 62
    EditLabel.Height = 13
    EditLabel.Caption = 'Remain Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 4
  end
  object ButtonResume: TDButton
    Left = 177
    Top = 29
    Width = 89
    Height = 25
    Caption = '&Resume'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonResumeClick
  end
  object EditTotalTime: TLabeledEdit
    Left = 80
    Top = 80
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'Total Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 6
  end
end
