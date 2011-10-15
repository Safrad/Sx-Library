object XPDUnitSetupForm: TXPDUnitSetupForm
  Left = 650
  Top = 241
  Width = 536
  Height = 380
  BorderStyle = bsSizeToolWin
  BorderWidth = 1
  Caption = 'DUnit Wizard Options...'
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 536
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 526
    Height = 317
    ActivePage = ParametersPage
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    Style = tsFlatButtons
    TabOrder = 0
    object BehaviourPage: TTabSheet
      Caption = 'Behaviour'
      ParentShowHint = False
      ShowHint = False
      object TestProject: TGroupBox
        Left = 0
        Top = 230
        Width = 518
        Height = 56
        Hint = '-'
        Align = alBottom
        Caption = 'New TestProject'
        TabOrder = 1
        object AddToProjectGroup: TCheckBox
          Left = 23
          Top = 22
          Width = 227
          Height = 17
          Caption = 'Add to current Project &Group'
          TabOrder = 0
          OnClick = AddToProjectGroupClick
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 518
        Height = 230
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object NewTestModule: TGroupBox
          Left = 0
          Top = 0
          Width = 256
          Height = 227
          Hint = '-'
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'New TestModule'
          TabOrder = 0
          object Label4: TLabel
            Left = 23
            Top = 31
            Width = 105
            Height = 15
            AutoSize = False
            Caption = 'Current IDE unit :'
            Layout = tlCenter
          end
          object Label5: TLabel
            Left = 23
            Top = 104
            Width = 212
            Height = 15
            AutoSize = False
            Caption = 'Current IDE unit global-scoped classes :'
            Layout = tlCenter
          end
          object AddCurrentToTestProjectUses: TCheckBox
            Left = 23
            Top = 74
            Width = 212
            Height = 17
            Caption = 'Add to current Project "u&ses" clause'
            TabOrder = 1
            OnClick = AddCurrentToTestProjectUsesClick
          end
          object AddCurrentToTestModuleUses: TCheckBox
            Left = 23
            Top = 56
            Width = 212
            Height = 17
            Caption = 'A&dd to TestModule "&uses" clause'
            TabOrder = 0
            OnClick = AddCurrentToTestModuleUsesClick
          end
          object ModuleAddPublishedMethods: TCheckBox
            Left = 23
            Top = 132
            Width = 212
            Height = 17
            Caption = 'Add PUBLISHED method tests'
            TabOrder = 2
            OnClick = ModuleAddPublishedMethodsClick
          end
          object ModuleAddPublicMethods: TCheckBox
            Left = 23
            Top = 150
            Width = 212
            Height = 17
            Caption = 'Add PUBLIC method tests'
            TabOrder = 3
            OnClick = ModuleAddPublicMethodsClick
          end
          object ModuleAddProtectedMethods: TCheckBox
            Left = 23
            Top = 168
            Width = 212
            Height = 17
            Caption = 'Add PROTECTED method tests'
            TabOrder = 4
            OnClick = ModuleAddProtectedMethodsClick
          end
        end
        object NewTestClass: TGroupBox
          Left = 265
          Top = 0
          Width = 252
          Height = 227
          Hint = '-'
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'New TestClass'
          TabOrder = 1
          object Label7: TLabel
            Left = 24
            Top = 31
            Width = 204
            Height = 16
            AutoSize = False
            Caption = 'Selected class in current IDE unit:'
            Layout = tlCenter
          end
          object ClassAddPublishedMethods: TCheckBox
            Left = 24
            Top = 57
            Width = 204
            Height = 17
            Caption = 'Add PUBLISHED method tests'
            TabOrder = 0
            OnClick = ClassAddPublishedMethodsClick
          end
          object ClassAddPublicMethods: TCheckBox
            Left = 24
            Top = 74
            Width = 204
            Height = 19
            Caption = 'Add PUBLIC method tests'
            TabOrder = 1
            OnClick = ClassAddPublicMethodsClick
          end
          object ClassAddProtectedMethods: TCheckBox
            Left = 24
            Top = 92
            Width = 204
            Height = 17
            Caption = 'Add PROTECTED method tests'
            TabOrder = 2
            OnClick = ClassAddProtectedMethodsClick
          end
          object ClassAddPrivateMethods: TCheckBox
            Left = 24
            Top = 110
            Width = 204
            Height = 17
            Caption = 'Add PRIVATE method tests'
            TabOrder = 3
            OnClick = ClassAddPrivateMethodsClick
          end
        end
      end
    end
    object ParametersPage: TTabSheet
      Caption = 'Parameters'
      ImageIndex = 1
      object ParameterTemplates: TGroupBox
        Left = 0
        Top = 0
        Width = 518
        Height = 286
        Align = alClient
        Caption = 'Code skeleton Parameter templates'
        TabOrder = 0
        object Label1: TLabel
          Left = 13
          Top = 27
          Width = 134
          Height = 17
          AutoSize = False
          Caption = '&Select parameter...'
          FocusControl = ParameterList
        end
        object Label3: TLabel
          Left = 13
          Top = 179
          Width = 134
          Height = 15
          AutoSize = False
          Caption = 'Description'
          Layout = tlCenter
        end
        object ParameterList: TListBox
          Left = 13
          Top = 46
          Width = 134
          Height = 121
          Hint = 'gsfdhdfhdsyh'
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 0
          OnClick = ParameterListClick
        end
        object TemplateGroup: TPanel
          Left = 160
          Top = 18
          Width = 345
          Height = 257
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvLowered
          TabOrder = 2
          object TemplateLabel: TLabel
            Left = 11
            Top = 8
            Width = 270
            Height = 17
            AutoSize = False
            Caption = '&Template'
            FocusControl = ParameterTemplate
          end
          object label12: TLabel
            Left = 12
            Top = 56
            Width = 134
            Height = 14
            AutoSize = False
            Caption = '&Macros'
            FocusControl = MacroList
            Layout = tlCenter
          end
          object Label2: TLabel
            Left = 176
            Top = 57
            Width = 134
            Height = 14
            AutoSize = False
            Caption = 'Description'
            Layout = tlCenter
          end
          object ParameterTemplate: TEdit
            Left = 10
            Top = 27
            Width = 325
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ParameterTemplateChange
            OnExit = ParameterTemplateExit
            OnKeyPress = ParameterTemplateKeyPress
          end
          object MacroList: TListBox
            Left = 10
            Top = 77
            Width = 156
            Height = 138
            Anchors = [akLeft, akTop, akBottom]
            ItemHeight = 13
            TabOrder = 1
            OnClick = MacroListClick
          end
          object InsertMacro: TButton
            Left = 39
            Top = 223
            Width = 93
            Height = 25
            Action = InsertMacroAction
            Anchors = [akLeft, akBottom]
            TabOrder = 3
          end
          object MacroDescription: TMemo
            Left = 174
            Top = 77
            Width = 160
            Height = 138
            TabStop = False
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
        end
        object ParameterDescription: TMemo
          Left = 13
          Top = 198
          Width = 134
          Height = 75
          TabStop = False
          Anchors = [akLeft, akTop, akBottom]
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
        end
      end
    end
  end
  object Buttons: TPanel
    Left = 0
    Top = 317
    Width = 526
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object RestoreDefaults: TButton
      Left = 138
      Top = 2
      Width = 93
      Height = 28
      Action = RestoreDefaultsAction
      Anchors = [akRight, akBottom]
      TabOrder = 0
    end
    object CancelChanges: TButton
      Left = 235
      Top = 2
      Width = 93
      Height = 28
      Action = CancelAction
      Anchors = [akRight, akBottom]
      Cancel = True
      TabOrder = 1
    end
    object ApplyChanges: TButton
      Left = 332
      Top = 2
      Width = 93
      Height = 28
      Hint = 'Save changes to disk'
      Action = ApplyAction
      Anchors = [akRight, akBottom]
      Caption = '&Apply'
      TabOrder = 2
    end
    object CloseForm: TButton
      Left = 429
      Top = 2
      Width = 93
      Height = 28
      Hint = 'Close dialog. Changes since last Apply are *not* saved'
      Action = CloseAction
      Anchors = [akRight, akBottom]
      Default = True
      TabOrder = 3
    end
  end
  object ActionList1: TActionList
    Left = 233
    Top = 1
    object ApplyAction: TAction
      Caption = 'Apply'
      OnExecute = ApplyActionExecute
      OnUpdate = ApplyActionUpdate
    end
    object CancelAction: TAction
      Caption = 'Disca&rd'
      Hint = 'Lose any changes since last apply'
      OnExecute = CancelActionExecute
      OnUpdate = CancelActionUpdate
    end
    object CloseAction: TAction
      Caption = '&Close'
      Hint = #39'Close dialog. Changes since last Apply are *not* saved'#39
      OnExecute = CloseActionExecute
    end
    object InsertMacroAction: TAction
      Caption = '&Insert'
      Hint = 'Insert selected Macro into Template at last cursor position'
      OnExecute = InsertMacroActionExecute
    end
    object SelectNameAction: TAction
    end
    object RestoreDefaultsAction: TAction
      Caption = '&Defaults'
      Hint = 'Discard all changes and saved settings.'
      OnExecute = RestoreDefaultsActionExecute
    end
  end
end
