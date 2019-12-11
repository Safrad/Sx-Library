object fStatus: TfStatus
  Left = 903
  Top = 715
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Status'
  ClientHeight = 117
  ClientWidth = 275
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
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
  object ButtonStop: TButton
    Left = 177
    Top = 85
    Width = 89
    Height = 25
    Caption = '&Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object ButtonPause: TButton
    Left = 177
    Top = 57
    Width = 89
    Height = 25
    Caption = '&Pause'
    TabOrder = 2
    OnClick = ButtonPauseClick
  end
  object EditElapsedTime: TLabeledEdit
    Left = 100
    Top = 32
    Width = 69
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 62
    EditLabel.Height = 13
    EditLabel.Caption = 'Elapsed Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 3
  end
  object EditRemainTime: TLabeledEdit
    Left = 100
    Top = 56
    Width = 69
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = 'Remain Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 4
  end
  object ButtonResume: TButton
    Left = 177
    Top = 29
    Width = 89
    Height = 25
    Caption = '&Resume'
    TabOrder = 5
    OnClick = ButtonResumeClick
  end
  object EditTotalTime: TLabeledEdit
    Left = 100
    Top = 80
    Width = 69
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 49
    EditLabel.Height = 13
    EditLabel.Caption = 'Total Time'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 6
  end
end
