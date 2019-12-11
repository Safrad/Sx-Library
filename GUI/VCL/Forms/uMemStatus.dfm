object fMemStatus: TfMemStatus
  Left = 393
  Top = 150
  BorderStyle = bsDialog
  Caption = 'Memory Status'
  ClientHeight = 329
  ClientWidth = 384
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
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
  object LabelAllocMemCount: TDLabel
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Hint = 
      'AllocMemCount contains the number of currently allocated blocks ' +
      'of memory that were requested by the user. AllocMemCount is incr' +
      'emented each time a block of memory is allocated and is decremen' +
      'ted each time a block of memory is freed. Use AllocMemCount to '
    Caption = 'AllocMemCount [1]'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelAllocMemSize: TDLabel
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Hint = 
      'AllocMemSize contains the total size, in bytes, of all currently' +
      ' allocated blocks of memory in use by an application. Use AllocM' +
      'emSize to find out how many bytes of memory an application is cu' +
      'rrently using.'
    Caption = 'AllocMemSize'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelTotalAddrSpace: TDLabel
    Left = 8
    Top = 56
    Width = 97
    Height = 17
    Hint = 
      'The (current) total address space available to your program, in ' +
      'bytes. This will grow as your program'#39's dynamic memory usage gro' +
      'ws.'
    Caption = 'TotalAddrSpace'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelTotalUncommited: TDLabel
    Left = 24
    Top = 80
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes (of TotalAddrSpace) for which space ha' +
      's not been allocated in the swap file.'
    Caption = 'TotalUncommitted'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelTotalAllocated: TDLabel
    Left = 16
    Top = 128
    Width = 97
    Height = 17
    Hint = 'The total number of bytes dynamically allocated by your program.'
    Caption = 'TotalAllocated'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelTotalFree: TDLabel
    Left = 16
    Top = 152
    Width = 97
    Height = 17
    Hint = 
      'The total number of free bytes available in the (current) addres' +
      's space for allocation by your program. If this number is exceed' +
      'ed, and enough virtual memory is available, more address space w' +
      'ill be allocated from the OS; TotalAddrSpace will be incremente'
    Caption = 'TotalFree'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelFreeSmall: TDLabel
    Left = 32
    Top = 176
    Width = 97
    Height = 17
    Hint = 
      'Total bytes of small memory blocks which are not currently alloc' +
      'ated by your program.'
    Caption = 'FreeSmall'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelFreeBig: TDLabel
    Left = 32
    Top = 200
    Width = 97
    Height = 17
    Hint = 
      'Total bytes of big memory blocks which are not currently allocat' +
      'ed by your program. Large free blocks can be created by coalesci' +
      'ng smaller, contiguous, free blocks or by freeing a large dynami' +
      'c allocation.  (The exact size of the blocks is immaterial)'
    Caption = 'FreeBig'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelUnused: TDLabel
    Left = 32
    Top = 224
    Width = 97
    Height = 17
    Hint = 
      'Total bytes which have never been allocated by your program. Not' +
      'e: Unused + FreeBig + FreeSmall = TotalFree These three fields (' +
      'Unused, FreeBig, and FreeSmall) refer to dynamic allocation by t' +
      'he user program.'
    Caption = 'Unused'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelOverhead: TDLabel
    Left = 8
    Top = 248
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes required by the heap manager to manage' +
      ' all the blocks dynamically allocated by your program.'
    Caption = 'Overhead'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelHeapErrorCode: TDLabel
    Left = 8
    Top = 272
    Width = 97
    Height = 17
    Hint = 
      'Indicates the current status of the heap, as internally determin' +
      'ed.'
    Caption = 'HeapErrorCode [?]'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object LabelTotalCommited: TDLabel
    Left = 24
    Top = 104
    Width = 97
    Height = 17
    Hint = 
      'The total number of bytes (of TotalAddrSpace) for which space ha' +
      's been allocated in the swap file. Note: TotalUncommitted + Tota' +
      'lCommitted = TotalAddrSpace'
    Caption = 'TotalCommitted'
    FontShadow = 1
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ShowHint = True
    ParentShowHint = False
  end
  object ButtonStart: TDButton
    Left = 200
    Top = 296
    Width = 81
    Height = 25
    Caption = '&Run'
    TabOrder = 0
    TabStop = False
    OnClick = ButtonStartClick
    AutoChange = True
    Down = True
  end
  object ButtonOk: TDButton
    Left = 296
    Top = 296
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOkClick
  end
  object Timer1: TDTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 8
    Top = 296
  end
end
