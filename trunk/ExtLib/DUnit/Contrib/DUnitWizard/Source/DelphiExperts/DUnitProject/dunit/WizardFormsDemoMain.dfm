object Form1: TForm1
  Left = 720
  Top = 281
  Width = 424
  Height = 161
  BorderWidth = 3
  Caption = 'WizardFormsDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 16
    Width = 115
    Height = 25
    Caption = 'Show Setup'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 6
    Top = 50
    Width = 115
    Height = 25
    Caption = 'Show TestModule'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 6
    Top = 87
    Width = 115
    Height = 25
    Caption = 'Show Project'
    TabOrder = 2
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 133
    Top = 0
    Width = 277
    Height = 128
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 3
  end
end
