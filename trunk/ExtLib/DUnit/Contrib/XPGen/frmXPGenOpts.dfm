object frm_XPGenOpts: Tfrm_XPGenOpts
  Left = 319
  Top = 210
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'XP Gen Options'
  ClientHeight = 377
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object chkTestSTubOnly: TCheckBox
    Left = 8
    Top = 8
    Width = 241
    Height = 17
    Caption = 'Generate Test Stub Only - Best for Test First'
    TabOrder = 0
  end
  object grpRegistrationOpts: TGroupBox
    Left = 8
    Top = 144
    Width = 249
    Height = 89
    Caption = 'Registration Options'
    TabOrder = 1
    object rdbClassic: TRadioButton
      Left = 16
      Top = 24
      Width = 217
      Height = 17
      Caption = 'Classic - procedure suite : ITestSuite'
      TabOrder = 0
    end
    object rdbPreferredOpts: TRadioButton
      Left = 16
      Top = 56
      Width = 217
      Height = 17
      Caption = 'Preferred - testframework.registerSuites'
      TabOrder = 1
    end
  end
  object grpModuleNaming: TGroupBox
    Left = 280
    Top = 32
    Width = 257
    Height = 97
    Caption = 'Module Naming'
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 112
      Height = 13
      Caption = 'Test Module Name Tag'
    end
    object Label6: TLabel
      Left = 8
      Top = 44
      Width = 42
      Height = 13
      Caption = 'Tag Is a '
    end
    object Label7: TLabel
      Left = 8
      Top = 76
      Width = 44
      Height = 13
      Caption = 'Sample : '
    end
    object edtModuleNameTag: TEdit
      Left = 128
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'edtModuleNameTag'
    end
    object ComboBox1: TComboBox
      Left = 56
      Top = 44
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Prefix'
        'Postfix')
    end
  end
  object grpTestClassNaming: TGroupBox
    Left = 8
    Top = 32
    Width = 257
    Height = 97
    Caption = 'Test Class Naming'
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 102
      Height = 13
      Caption = 'Test Class Name Tag'
    end
    object Label5: TLabel
      Left = 8
      Top = 44
      Width = 33
      Height = 13
      Caption = 'Tag Is '
    end
    object Label8: TLabel
      Left = 8
      Top = 76
      Width = 44
      Height = 13
      Caption = 'Sample : '
    end
    object edtTestNameTag: TEdit
      Left = 120
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'edtTestC'
    end
    object cboClass: TComboBox
      Left = 48
      Top = 44
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Prefix'
        'Postfix'
        ' ')
    end
  end
  object GroupBox1: TGroupBox
    Left = 272
    Top = 144
    Width = 225
    Height = 89
    Caption = 'Stub Location Options'
    TabOrder = 4
    object Label3: TLabel
      Left = 16
      Top = 60
      Width = 60
      Height = 13
      Caption = 'Folder Name'
    end
    object chkStubsInSubFolder: TCheckBox
      Left = 16
      Top = 24
      Width = 177
      Height = 17
      Caption = 'Generate Stubs in SubFolder'
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 88
      Top = 52
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'edtStubSubFolder'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 336
    Width = 548
    Height = 41
    Align = alBottom
    TabOrder = 5
    object Panel2: TPanel
      Left = 371
      Top = 1
      Width = 176
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnCancel: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'CANCEL'
        ModalResult = 2
        TabOrder = 0
        OnClick = btnCancelClick
      end
      object btnOK: TButton
        Left = 96
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 1
        OnClick = btnOKClick
      end
    end
  end
  object grpTestInstancing: TGroupBox
    Left = 120
    Top = 240
    Width = 289
    Height = 89
    Caption = 'Test Instancing'
    TabOrder = 6
    object Label4: TLabel
      Left = 8
      Top = 52
      Width = 149
      Height = 13
      Caption = 'Auto Instance Identifier Name : '
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 24
      Width = 217
      Height = 17
      Caption = 'Auto Instance in Test Class'
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 160
      Top = 52
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'edtTestInstanceName'
    end
  end
end
