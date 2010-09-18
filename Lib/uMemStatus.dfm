object fMemStatus: TfMemStatus
  Left = 382
  Top = 127
  BorderStyle = bsDialog
  Caption = 'Memory Status'
  ClientHeight = 329
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 51
    Width = 369
    Height = 13
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 3
    Top = 43
    Width = 10
    Height = 94
    Shape = bsLeftLine
  end
  object ButtonStart: TDButton
    Left = 200
    Top = 296
    Width = 81
    Height = 25
    Caption = 'Run'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = False
    OnClick = ButtonStartClick
    AutoChange = True
  end
  object DPanel1: TDPanel
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Hint = 
      'AllocMemCount contains the number of currently allocated blocks ' +
      'of memory that were requested by the user. AllocMemCount is incr' +
      'emented each time a block of memory is allocated and is decremen' +
      'ted each time a block of memory is freed. Use AllocMemCount to '
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'AllocMemCount [1]'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    FontShadow = 1
  end
  object DPanel2: TDPanel
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Hint = 
      'AllocMemSize contains the total size, in bytes, of all currently' +
      ' allocated blocks of memory in use by an application. Use AllocM' +
      'emSize to find out how many bytes of memory an application is cu' +
      'rrently using.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'AllocMemSize'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    FontShadow = 1
  end
  object DPanel3: TDPanel
    Left = 8
    Top = 56
    Width = 97
    Height = 17
    Hint = 
      'The (current) total address space available to your program, in ' +
      'bytes. This will grow as your program'#39's dynamic memory usage gro' +
      'ws.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'TotalAddrSpace'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    FontShadow = 1
  end
  object DPanel4: TDPanel
    Left = 24
    Top = 80
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes (of TotalAddrSpace) for which space ha' +
      's not been allocated in the swap file.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'TotalUncommitted'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    FontShadow = 1
  end
  object DPanel5: TDPanel
    Left = 16
    Top = 128
    Width = 97
    Height = 17
    Hint = 'The total number of bytes dynamically allocated by your program.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'TotalAllocated'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    FontShadow = 1
  end
  object DPanel6: TDPanel
    Left = 16
    Top = 152
    Width = 97
    Height = 17
    Hint = 
      'The total number of free bytes available in the (current) addres' +
      's space for allocation by your program. If this number is exceed' +
      'ed, and enough virtual memory is available, more address space w' +
      'ill be allocated from the OS; TotalAddrSpace will be incremente'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'TotalFree'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    FontShadow = 1
  end
  object DPanel7: TDPanel
    Left = 32
    Top = 176
    Width = 97
    Height = 17
    Hint = 
      'Total bytes of small memory blocks which are not currently alloc' +
      'ated by your program.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'FreeSmall'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    FontShadow = 1
  end
  object DPanel8: TDPanel
    Left = 32
    Top = 200
    Width = 97
    Height = 17
    Hint = 
      'Total bytes of big memory blocks which are not currently allocat' +
      'ed by your program. Large free blocks can be created by coalesci' +
      'ng smaller, contiguous, free blocks or by freeing a large dynami' +
      'c allocation.  (The exact size of the blocks is immaterial)'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'FreeBig'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    FontShadow = 1
  end
  object DPanel9: TDPanel
    Left = 32
    Top = 224
    Width = 97
    Height = 17
    Hint = 
      'Total bytes which have never been allocated by your program. Not' +
      'e: Unused + FreeBig + FreeSmall = TotalFree These three fields (' +
      'Unused, FreeBig, and FreeSmall) refer to dynamic allocation by t' +
      'he user program.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Unused'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    FontShadow = 1
  end
  object DPanel10: TDPanel
    Left = 8
    Top = 248
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes required by the heap manager to manage' +
      ' all the blocks dynamically allocated by your program.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Overhead'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    FontShadow = 1
  end
  object DPanel11: TDPanel
    Left = 8
    Top = 272
    Width = 97
    Height = 17
    Hint = 
      'Indicates the current status of the heap, as internally determin' +
      'ed.'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'HeapErrorCode [?]'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    FontShadow = 1
  end
  object DPanel12: TDPanel
    Left = 24
    Top = 104
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes (of TotalAddrSpace) for which space ha' +
      's been allocated in the swap file. Note: TotalUncommitted + Tota' +
      'lCommitted = TotalAddrSpace'
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'TotalCommitted'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    FontShadow = 1
  end
  object ButtonOk: TDButton
    Left = 296
    Top = 296
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = ButtonOkClick
  end
  object Timer1: TDTimer
    ActiveOnly = False
    Enabled = False
    Interval = 1000
    EventStep = esInterval
    OnTimer = Timer1Timer
    Left = 8
    Top = 296
  end
end
