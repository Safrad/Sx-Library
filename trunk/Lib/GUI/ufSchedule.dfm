object fSchedule: TfSchedule
  Left = 710
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Edit Schedule'
  ClientHeight = 440
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 32
    Width = 409
    Height = 9
    Shape = bsTopLine
  end
  object BevelDT: TBevel
    Left = 7
    Top = 144
    Width = 306
    Height = 217
  end
  object LabelScheduleTask: TLabel
    Left = 8
    Top = 67
    Width = 113
    Height = 19
    AutoSize = False
    Caption = 'Schedule:'
    FocusControl = ComboBoxSchedule
    Transparent = True
    Layout = tlCenter
  end
  object LabelStartTime: TLabel
    Left = 200
    Top = 67
    Width = 106
    Height = 19
    AutoSize = False
    Caption = 'Start Time:'
    FocusControl = TimePicker
    Transparent = True
    Layout = tlCenter
  end
  object LabelEveryText: TLabel
    Left = 192
    Top = 120
    Width = 9
    Height = 13
    Caption = '...'
    Transparent = True
    Layout = tlCenter
  end
  object LabelEvery: TLabel
    Left = 8
    Top = 118
    Width = 35
    Height = 19
    AutoSize = False
    Caption = 'Every'
    FocusControl = ComboBoxEvery
    Transparent = True
    Layout = tlCenter
  end
  object LabelWofY: TLabel
    Left = 216
    Top = 208
    Width = 69
    Height = 19
    AutoSize = False
    Caption = 'Week of Year:'
    FocusControl = EditWofY
    Transparent = True
    Layout = tlCenter
  end
  object LabelDofY: TLabel
    Left = 216
    Top = 248
    Width = 59
    Height = 19
    AutoSize = False
    Caption = 'Day of Year:'
    FocusControl = EditDofY
    Transparent = True
    Layout = tlCenter
  end
  object LabelDuration: TLabel
    Left = 8
    Top = 376
    Width = 50
    Height = 19
    AutoSize = False
    Caption = 'Duration:'
    FocusControl = TimePickerDuration
    Transparent = True
    Layout = tlCenter
  end
  object LabelNextRun: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 19
    AutoSize = False
    Caption = 'Next Run:'
    FocusControl = EditNextRun
    Transparent = True
    Layout = tlCenter
  end
  object LabelDurationDays: TLabel
    Left = 239
    Top = 375
    Width = 43
    Height = 19
    AutoSize = False
    Caption = 'days'
    FocusControl = ComboBoxDuration
    Transparent = True
    Layout = tlCenter
  end
  object Bevel2: TBevel
    Left = 8
    Top = 400
    Width = 409
    Height = 9
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 368
    Width = 409
    Height = 9
    Shape = bsTopLine
  end
  object ButtonOk: TDButton
    Left = 240
    Top = 408
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 15
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TDButton
    Left = 336
    Top = 408
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 16
    OnClick = ButtonCancelClick
  end
  object ComboBoxSchedule: TComboBox
    Left = 8
    Top = 88
    Width = 177
    Height = 21
    Style = csDropDownList
    DropDownCount = 20
    TabOrder = 3
    OnChange = ComboBoxScheduleChange
  end
  object Calendar: TCalendar
    Left = 16
    Top = 208
    Width = 193
    Height = 145
    StartOfWeek = 0
    TabOrder = 10
    UseCurrentDate = False
    OnChange = CalendarChange
  end
  object TimePicker: TDateTimePicker
    Left = 200
    Top = 88
    Width = 105
    Height = 21
    Date = 37709.000000000000000000
    Time = 37709.000000000000000000
    Checked = False
    DateFormat = dfLong
    Kind = dtkTime
    TabOrder = 4
    OnChange = TimePickerChange
  end
  object ComboBoxM: TComboBox
    Left = 16
    Top = 176
    Width = 129
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    TabOrder = 8
    OnChange = ComboBoxMChange
  end
  object ComboBoxY: TComboBox
    Left = 16
    Top = 152
    Width = 89
    Height = 21
    TabOrder = 6
    OnChange = ComboBoxYChange
  end
  object EditNextRun: TDEdit
    Left = 72
    Top = 8
    Width = 137
    Height = 21
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 0
  end
  object EditWofY: TDEdit
    Left = 216
    Top = 224
    Width = 89
    Height = 21
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 11
  end
  object EditPrename: TDEdit
    Left = 152
    Top = 176
    Width = 153
    Height = 21
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 9
  end
  object DatePicker: TDateTimePicker
    Left = 112
    Top = 152
    Width = 193
    Height = 21
    Date = 37709.000000000000000000
    Time = 37709.000000000000000000
    Checked = False
    DateFormat = dfLong
    TabOrder = 7
    OnChange = DatePickerChange
  end
  object ComboBoxEvery: TComboBox
    Left = 49
    Top = 117
    Width = 137
    Height = 21
    AutoComplete = False
    AutoDropDown = True
    DropDownCount = 31
    TabOrder = 5
    OnChange = ComboBoxEveryChange
  end
  object EditSchedule: TDEdit
    Left = 8
    Top = 40
    Width = 409
    Height = 21
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 1
  end
  object EditDofY: TDEdit
    Left = 216
    Top = 264
    Width = 89
    Height = 21
    Color = clBtnFace
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ReadOnly = True
    TabOrder = 12
  end
  object TimePickerDuration: TDateTimePicker
    Left = 64
    Top = 376
    Width = 105
    Height = 21
    Date = 37709.000000000000000000
    Time = 37709.000000000000000000
    Checked = False
    DateFormat = dfLong
    Kind = dtkTime
    TabOrder = 13
    OnChange = TimePickerDurationChange
  end
  object ComboBoxDuration: TComboBox
    Left = 176
    Top = 376
    Width = 57
    Height = 21
    TabOrder = 14
    OnChange = TimePickerDurationChange
  end
  object ButtonNow: TDButton
    Left = 320
    Top = 82
    Width = 97
    Height = 27
    Caption = '&Reset to Now'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = ButtonNowClick
  end
  object CheckBoxEnabled: TSxCheckBox
    Left = 8
    Top = 416
    Width = 65
    Height = 17
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 17
    OnClick = CheckBoxEnabledClick
  end
end
