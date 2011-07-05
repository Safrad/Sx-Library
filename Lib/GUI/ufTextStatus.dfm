object fTextStatus: TfTextStatus
  Left = 903
  Top = 715
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Status'
  ClientHeight = 149
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DGauge: TDGauge
    Left = 0
    Top = 32
    Width = 275
    Height = 25
    FontShadow = 1
    Displ.Format = '88'
    Max = 1024
    Color = clAppWorkSpace
  end
  object EditElapsedTime: TLabeledEdit
    Left = 80
    Top = 64
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'Elapsed Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 1
  end
  object EditRemainTime: TLabeledEdit
    Left = 80
    Top = 88
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 62
    EditLabel.Height = 13
    EditLabel.Caption = 'Remain Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 2
  end
  object EditTotalTime: TLabeledEdit
    Left = 80
    Top = 112
    Width = 89
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'Total Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 3
  end
  object ButtonStop: TDButton
    Left = 177
    Top = 117
    Width = 89
    Height = 25
    Caption = '&Stop'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonStopClick
  end
  object ButtonPause: TDButton
    Left = 177
    Top = 89
    Width = 89
    Height = 25
    Caption = '&Pause'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonPauseClick
  end
  object ButtonResume: TDButton
    Left = 177
    Top = 61
    Width = 89
    Height = 25
    Caption = '&Resume'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = ButtonResumeClick
  end
  object edtAction: TLabeledEdit
    Left = 40
    Top = 8
    Width = 225
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 30
    EditLabel.Height = 13
    EditLabel.Caption = 'Action'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 7
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 120
  end
end
