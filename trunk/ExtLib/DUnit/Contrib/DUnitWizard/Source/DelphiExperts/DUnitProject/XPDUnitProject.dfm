object XPDUnitProjectForm: TXPDUnitProjectForm
  Left = 784
  Top = 526
  Width = 320
  Height = 210
  BorderStyle = bsSizeToolWin
  Caption = 'New DUnit Project...'
  Color = clBtnFace
  Constraints.MaxHeight = 210
  Constraints.MinHeight = 210
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 1
    Width = 95
    Height = 18
    AutoSize = False
    Caption = 'Project &name'
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 5
    Top = 42
    Width = 95
    Height = 18
    AutoSize = False
    Caption = 'Project filename'
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 5
    Top = 85
    Width = 95
    Height = 18
    AutoSize = False
    Caption = 'Project &path'
    Layout = tlCenter
  end
  object SelectPath: TSpeedButton
    Left = 284
    Top = 103
    Width = 23
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '...'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = SelectPathClick
  end
  object ProjectName: TEdit
    Left = 5
    Top = 20
    Width = 302
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ProjectNameChange
  end
  object ProjectFileName: TEdit
    Left = 5
    Top = 61
    Width = 302
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object ProjectPath: TEdit
    Left = 5
    Top = 104
    Width = 273
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 151
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    TabOrder = 4
    Kind = bkCancel
  end
  object CreateProject: TBitBtn
    Left = 232
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'C&reate'
    TabOrder = 5
    OnClick = CreateProjectClick
    Kind = bkOK
  end
  object AddToProjectGroup: TCheckBox
    Left = 5
    Top = 132
    Width = 224
    Height = 17
    Caption = '&Add to current IDE project group'
    TabOrder = 3
  end
end
