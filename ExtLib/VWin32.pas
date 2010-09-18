{------------------------------------------------------------------------------}
{ unit       : vwin32                                                          }
{ version    : 1.0                                                             }
{ last update: 1999/04/06                                                      }
{ written for: Delphi 3 & 4                                                    }
{ written by : Geir Wikran                                                     }
{ e-mail     : gwikran@online.no                                               }
{                                                                              }
{ This source code is freeware. You may use, change, and distribute without    }
{ charge the source code as you like. This unit can be used freely in any      }
{ commercial applications. However, it may not be sold as a standalone product }
{ and the source code may not be included in a commercial product. This unit   }
{ is provided as is with no warrent or support. Make sure to read relevant     }
{ information and documentation from Microsoft before using this unit.         }
{------------------------------------------------------------------------------}


{- Relevant information and documentation from Microsoft ----------------------}
{                                                                              }
{ Go to http://www.microsoft.com/ and select "Search".                         }
{ Enter "VWIN32" for search word, and select search                            }
{ category "Support & the Knowledge Base". The search                          }
{ will give articles on how to do low-level disk access                        }
{ under Win9x.                                                                 }
{                                                                              }
{ Go to http://msdn.microsoft.com/library/                                     }
{ Browse down the tree to find                                                 }
{ -Platform SDK                                                                }
{   -Windows Base Services                                                     }
{     -Windows 95 Features                                                     }
{       -Windows 95 Reference                                                  }
{ Before doing any direct disk I/O make sure to read                           }
{ all information on using the drive locking and lock                          }
{ hierarcy. This is important.                                                 }
{                                                                              }
{------------------------------------------------------------------------------}


unit vwin32;

{==============================================================================}
interface

uses
  Windows;

{- VWIN32 DeviceIoControl -----------------------------------------------------}

{ Windows 95/98 does not support opening disk drives or partitions with the    }
{ CreateFile() function as Windows NT does. Windows 9x does not support the    }
{ DeviceIoControl() IOCTL APIs as Windows NT does. Instead, low-level disk     }
{ access in Windows 9x is supported through DeviceIoControl() calls to the     }
{ VWIN32 VxD, which supports a set of control codes that applications can use  }
{ to issue low-level disk I/O functions. These functions include interrup 13h, }
{ interrupt 25h, interrupt 26h, interrupt 21h function 44xx and function 730x. }

const
  { Name of DIOC device: }
  VWIN32_DEVICE_NAME        = '\\.\VWIN32';

  { VWIN32 device control codes used with VWIN32DIOC function: }
  VWIN32_DIOC_CLOSE         = 0; { Close the device.                           }

  VWIN32_DIOC_DOS_IOCTL     = 1; { MS-DOS device I/O control function,         }
                                 { interrupt 21h function 4400h through 4411h  }
  VWIN32_DIOC_DOS_INT25     = 2; { MS-DOS absolute disk read command,          }
                                 { interrupt 25h.                              }
  VWIN32_DIOC_DOS_INT26     = 3; { MS-DOS absolute disk write command,         }
                                 { interrupt 26h.                              }
  VWIN32_DIOC_DOS_INT13     = 4; { Low-level BIOS disk functions,              }
                                 { interrupt 13h.                              }
  VWIN32_DIOC_DOS_DRIVEINFO = 6; { MS-DOS Interrupt 21h new function 730x.     }
                                 { Supported only by Windows 95 OSR2 and later.}

type
  PDIOC_Registers = ^TDIOC_Registers;
  TDIOC_Registers = record
    case Byte of
    0: (EBX,EDX,ECX,EAX,EDI,ESI,Flags: DWord);
    1: (BX,BXE,DX,DXE,CX,CXE,AX,AXE,DI,DIE,SI,SIE: Word);
  end;

  { Some functions require far pointers passed in segment:offset pairs. Since  }
  { 32-bit code does not have segments, the TDIOC_Registers structure contains }
  { no segment registers. The full pointer should be placed into the register  }
  { corresponding to the offset portion of the real-mode pointer. For example, }
  { use EDX for pointer that go into DS:DX.                                    }

const
  FLAG_CARRY = $00000001;

function  VWIN32DIOC(ControlCode: Integer; Registers: PDIOC_Registers): Boolean;
          { Implements DeviceIoControl() calls to VWIN32.VXD. The device is    }
          { automatically opened when needed. The ControlCode VWIN32_DIOC_CLOSE}
          { can be used to close the device. However, the device will be closed}
          { automatically by the units finalization. The function returnes     }
          { ERROR_OS_NOT_SUPPORTED in VWIN32Error it is not supported by the   }
          { running Windows version, or it returne ERROR_OPENING_DEVICE if an  }
          { error occured when trying to open the device.                      }

{- Windows Version ------------------------------------------------------------}

function  WindowsVersion(Major,Minor,Build,Platform: DWord): Boolean;
          { Tests the running version of Windows. Returns true if it is equal  }
          { or higher than the specified version info. See GetVersionEx in     }
          { Win32.hlp for more information.                                    }

{ Windows 9x      - WindowsVersion(0,0,0,VER_PLATFORM_WIN32_WINDOWS)           }
{ Windows 95 OSR2 - WindowsVersion(0,0,1081,VER_PLATFORM_WIN32_WINDOWS)        }

{ Because of changes to functions and data structures after the introduction   }
{ of FAT32 in Windows 95 OSR2, a test for this Windows version will often be   }
{ needed. To make the test simple this unit provides the boolean variable      }
{ Win95OSR2, which is initialized to true if the running Windows 9x is         }
{ Windows 95 OSR2 or later.                                                    }

var
  Win95OSR2: Boolean;

{------------------------------------------------------------------------------}

{ Logical drives: 0=default, 1=A, 2=B, 3=C ... 26=Z }

{------------------------------------------------------------------------------}

function  DriveIsRemovable(Drive: Byte): Boolean;
          { Returns true if a drive is removable.                              }
function  DriveIsRemote(Drive: Byte): Boolean;
          { Returns true if a drive is remote.                                 }
function  DriveIsSubstitute(Drive: Byte): Boolean;
          { Returns true if a drive is substituted (see MS-DOS subst command). }
function  DirectAccessAllowed(Drive: Byte): Boolean;
          { Returns true if a drive allows direct I/O.                         }
function  LockRemovableMedia(Drive: Byte): Boolean;
          { Locks the media in a drive (preventing its removal).               }
function  UnlockRemovableMedia(Drive: Byte): Boolean;
          { Unlocks the media in a drive (permitting its removal).             }
function  GetRemovableMediaLocks(Drive: Byte; var Locks: Byte): Boolean;
          { Returns the lock status on a drive (number of pending locks).      }
function  EjectRemovableMedia(Drive: Byte): Boolean;
          { Ejects the media in a drive.                                       }
function  GetAccessFlag(Drive: Byte; var Flag: Byte): Boolean;
          { Returns the access flag for a drive. Flag=0 if access blocked      }
          { (unformatted), Flag<>0 if access allowed.                          }
function  SetAccessFlag(Drive: Byte; Flag: Byte): Boolean;
          { Sets the access flag for a drive.                                  }


{- Volume Locking -------------------------------------------------------------}

const
  { Lock peermission codes: }
  LOCK_ALLOW_WRITING  = $0001; { Allow write operations in level 1 lock. Write }
                               { operations are always blocked in level 2 & 3  }
                               { lock.                                         }
  LOCK_BLOCK_MAPPING  = $0002; { Block new file mappings in level 1 & 2 lock.  }
                               { New file mappings are always blocked in level }
                               { 3 lock.                                       }
                               { Read operations are always allowed in level   }
                               { 1 & 2 lock, and blocked in level 3 lock.      }
  LOCK_FOR_FORMATTING = $0004; { Locks the volume for formatting. Specified    }
                               { when a level 0 lock is obtained for the second}
                               { time.                                         }

function  LockLogicalVolume(Drive: Byte; Level,Permission: Byte): Boolean;
          { Locks a logical drive. A drive must be locked before direct disk   }
          { write operations through Interrupt 26h or Interrupt 21h IOCTL      }
          { functions can be performed.                                        }
function  LockPhysicalVolume(Disk: Byte; Level,Permission: Byte): Boolean;
          { Locks a physical disk. Disk 00-7Fh for floppy disk drives          }
          { (00=first floppy drive, 01=second, and so on). Disk 80-FEh         }
          { for hard disk drives (80=first hard disk, 81=second, and so on).   }
          { A disk must be locked before direct disk write operations through  }
          { Interrupt 13h can be performed. The system automatically takes a   }
          { logical volume lock on all logical drives on the physical disk.    }
function  UnlockLogicalVolume(Drive: Byte): Boolean;
          { Unlocks a logical drive and decrements the lock level. To release  }
          { the lock on a drive the unlock function must be called the same    }
          { number of times that lock function was called.                     }
function  UnlockPhysicalVolume(Disk: Byte): Boolean;
          { Unlocks a physical disk and decrements the lock level. To release  }
          { the lock on a disk the unlock function must be called the same     }
          { number of times that lock function was called.                     }
function  GetCurrentLockState(Drive: Byte; var Level,Permission: Byte): Boolean;
          { Returns a drive's current lock level and permission.               }
function  GetLockFlagState(Drive: Byte; var Flags: Word): Boolean;
          { Polls the state of the access flag for a drive to determine if a   }
          { write operation (deleting or renaming a file, writing to a file    }
          { etc.) or a new file mapping has occurred since the last time the   }
          { flags were polled. Flag values:                                    }
          { 0= no operation has occured                                        }
          { 1= write operations have occured                                   }
          { 2= file mapping has occured                                        }

const
  { Access modes (bits 0-3): }
  OPEN_ACCESS_READONLY           = $0000;
  OPEN_ACCESS_WRITEONLY          = $0001;
  OPEN_ACCESS_READWRITE          = $0002;
  OPEN_ACCESS_RO_NOMODLASTACCESS = $0004;
  { Share modes (bits 4-6): }
  OPEN_SHARE_COMPATIBLE          = $0000;
  OPEN_SHARE_DENYREADWRITE       = $0010;
  OPEN_SHARE_DENYWRITE           = $0020;
  OPEN_SHARE_DENYREAD            = $0030;
  OPEN_SHARE_DENYNONE            = $0040;
  { Open flags (bits 7-15): }
  OPEN_FLAGS_NOINHERIT           = $0080;
  OPEN_FLAGS_NO_BUFFERING        = $0100;
  OPEN_FLAGS_NO_COMPRESS         = $0200;
  OPEN_FLAGS_ALIAS_HINT          = $0400;
  OPEN_FLAGS_NOCRITERR           = $2000;
  OPEN_FLAGS_COMMIT              = $4000;

  { File types: }
  FILE_TYPE_NORMAL               = $0000; { Normal file                        }
  FILE_TYPE_MEMORY_MAPPED        = $0001; { Memory-mapped file (are unmovable) }
  FILE_TYPE_UNMOVABLE            = $0002; { Unmovable (32-bit DLLs and EXEs)   }
  FILE_TYPE_SWAP                 = $0004; { Windows' swap file                 }

function  EnumerateOpenFiles(Drive: Byte; FileIndex: DWord; var FilePath: String;
                             var OpenMode,FileType: Word): Boolean;
          { Enumerates open files on the specified drive. The function returns }
          { information about one file at a time. To enumerate all open files, }
          { the function must be called repeatedly with FileIndex set to a new }
          { value for each call. FileIndex should be set to zero initially and }
          { then incremented by one for each subsequent call. The path of the  }
          { open file is returned in PathBuf. The mode that the file was opened}
          { in is returned in OpenMode, which is a combination of access mode, }
          { share mode, and open flags. The type of the file is returned in    }
          { FileType.                                                          }
          { ERROR_NO_MORE_FILES are returned when all open files on the volume }
          { have been enumerated. ERROR_ACCESS_DENIED is returned if FileIndex }
          { exceeds the number of open files on the drive.                     }
          { The function may return inconsistent results when used on a volume }
          { where other processes may be opening and closing files. The volume }
          { should be in a level 3 lock before enumerating open files.         }

function  FindSwapFile(var FilePath: String; var PagerType: Word; var PageCount: DWord): Boolean;
          { Returns information about Windows' swap file. The path of the swap }
          { file is returned in FilePath. The type of the pager is returned in }
          { PagerType (1=no pager, 2=paging trough MS-DOS, 3=paging through    }
          { protected-mode I/O supervisor). The current number of 4Kb pages in }
          { the swap file is returned in PageCount.                            }

const
  { Reset drive flags: }
  RESET_DRIVE_BUFFERS       = $0000; { Reset drive, flush file system buffers. }
  RESET_DRIVE_BUFFERS_CACHE = $0001; { Reset drive, flush file system buffers, }
                                     { flush and ivalidate drive cache.        }
  RESET_DRIVE_DRIVESPACE    = $0002; { Remounts drivespace volume.             }

{function  ResetDrive(Drive: Byte; Flag: Word): Boolean;}
          { Flushes file system buffers and cache and optionally remounts the  }
          { drivespace volume. Any Buffered write operations are performed, and}
          { all waiting data is written to the appropriate drive.              }


{- Media Identifier -----------------------------------------------------------}

type
  TMediaIdentifier = packed record
    InfoLevel          : Word;  { Information level (must be zero).            }
    SerialNumber       : DWord; { Serial number for the medium.                }
    VolumeLabel        : array[0..10] of Char; { Volume label for the medium.  }
    FileSysType        : array[0..7] of Char; { 'FAT12' 12-bit FAT file system }
                                              { 'FAT16' 16-bit FAT file system }
                                              { 'FAT32' 32-bit FAT file system }
                                              { 'CDROM' High Sierra file system}
                                              { 'CD001' ISO9660 file system    }
                                              { 'CDAUDIO' Audio disk           }
  end;

function  GetMediaIdentifier(Drive: Byte; var Media: TMediaIdentifier): Boolean;
          { Returnes a drive's serial number, volume label, and file system.   }
function  SetMediaIdentifier(Drive: Byte; Media: TMediaIdentifier): Boolean;
          { Sets a drive's serial number, volume label, and file system.       }


{- Media Type -----------------------------------------------------------------}

type
  TMediaType = packed record
    DefaultType        : ByteBool; { True for the default media type.          }
    CurrentType        : Byte;     { See TDeviceParameters.DeviceType.         }
  end;

function  SenseMediaType(Drive: Byte; var Media: TMediaType): Boolean;
          { Returnes the media type for a drive.                               }


{- Drive Map Info -------------------------------------------------------------}

const
  { Drive map info flags: }
  PROT_MODE_LOGICAL_DRIVE  = $01; { Protected-mode driver is in use for this   }
                                  { logical drive.                             }
  PROT_MODE_PHYSICAL_DRIVE = $02; { Protected-mode driver is in use for the    }
                                  { physical drive of this logical drive.      }
  PROT_MODE_ONLY_DRIVE     = $04; { Drive is not available when running with   }
                                  { MS-DOS.                                    }
  PROT_MODE_EJECT          = $08; { Protected-mode drive supports an electronic}
                                  { eject operation.                           }
  PROT_MODE_ASYNC_NOTIFY   = $10; { Drive issues media arrival and removal     }
                                  { notifications.                             }

type
  TDriveMapInfo = packed record
    Flags              : Byte;  { Combination of flags describing the drive.   }
    Int13Unit          : Byte;  { Physical drive number of the given drive.    }
                                { 00-7Fh = floppy disk drive (00 for the first }
                                { floppy drive, 01 for the second, and so on). }
                                { 80-FEh = hard disk drive (80 for the first   }
                                { hard disk, 81 for the second, and so on).    }
                                { FFh = drive does not map to a physical drive.}
    AssociatedDriveMap : DWord; { Logical drive numbers associated with the    }
                                { physical drive. For example, a host drive C  }
                                { with child drive letters A and B would return}
                                { with bits 0 and 1 set.                       }
    PartitionStartRBA  : DWord; { 64 bits relative block address offset from   }
    PartitionStartRBAHi: DWord; { start of the physical volume to start of the }
                                { given partition.                             }
  end;

function  GetDriveMapInfo(Drive: Byte; var Info: TDriveMapInfo): Boolean;
          { Retrieves maping information about the specified drive.            }


{- Parameter Blocks -----------------------------------------------------------}

type
  PDPB = ^TDPB;
  TDPB = packed record { Drive Parameter Block for FAT12 and FAT16: }
    DriveNumber        : Byte;  { Drive number (0=A, 1=B, and so on).          }
    UnitNumber         : Byte;  { Drive unit number on device driver.          }
    SectorSize         : Word;  { Size of each sector in bytes.                }
    ClusterMask        : Byte;  { Number of sectors per cluster minus 1.       }
    ClusterShift       : Byte;  { Number of sectors per cluster as power of 2. }
    FirstFAT           : Word;  { First sector for the File Allocation Table.  }
    FATCount           : Byte;  { Number of FATs on the drive.                 }
    RootEntries        : Word;  { Number of entries in root directory.         }
    FirstSector        : Word;  { First sector of the first cluster.           }
    MaxCluster         : Word;  { Number of clusters on drive plus 1.          }
    FATSize            : Word;  { Number of sectors occupied by each FAT.      }
    DirSector          : Word;  { First sector for the root directory.         }
    Reserved2          : DWord;
    MediaID            : Byte;  { Media descriptor for the drive.              }
    FirstAccess        : Byte;  { 00h if disk accessed, FFh if not accessed.   }
    Reserved3          : DWord;
    NextFree           : Word;  { Most recently allocated cluster. Cluster at  }
                                { which to start search for free clusters.     }
    FreeCount          : Word;  { Number of free clusters on the drive.        }
                                { FFFF if the number is unknown.               }
  end;

  PExtDPB = ^TExtDPB;
  TExtDPB = packed record { Extended Drive Parameter Block for FAT32: }
    DriveNumber        : Byte;  { Drive number (0=A, 1=B, and so on).          }
    UnitNumber         : Byte;  { Drive unit number on device driver.          }
    SectorSize         : Word;  { Size of each sector in bytes.                }
    ClusterMask        : Byte;  { Number of sectors per cluster minus 1.       }
    ClusterShift       : Byte;  { Number of sectors per cluster as power of 2. }
    FirstFAT           : Word;  { First sector for the File Allocation Table.  }
    FATCount           : Byte;  { Number of FATs on the drive.                 }
    RootEntries        : Word;  { Number of entries in root directory.         }
    FirstSector        : Word;  { First sector of the first cluster.           }
    MaxCluster         : Word;  { Number of clusters on drive plus 1.          }
                                { Field is undefined for FAT32 drives.         }
    FATSize            : Word;  { Number of sectors occupied by each FAT.      }
                                { Zero indicates a FAT32 drive.                }
                                { Use ExtFATSize for FAT32 media.              }
    DirSector          : Word;  { First sector for the root directory.         }
                                { Field is undefined for FAT32 drives.         }
    Reserved2          : DWord;
    MediaID            : Byte;  { Media descriptor for the drive.              }
    Reserved           : Byte;
    Reserved3          : DWord;
    NextFree           : Word;  { Most recently allocated cluster. Cluster at  }
                                { which to start search for free clusters.     }
    ExtFreeCount       : DWord; { Number of free clusters on the drive.        }
                                { FFFFFFFF if the number is unknown.           }
    ExtFlags           : Word;  { Flags describing the drive:                  }
                                { bits 0-3: 0-based number of active FAT       }
                                { bits 4-6: reserved                           }
                                { bit 7: FAT mirroring enabled if bit cleared  }
                                { bits 8-15: reserved                          }
    ExtFSInfoSec       : Word;  { Sector containing information about the file }
                                { system in TBifFATBootFSInfo structure.       }
                                { This field is set to 0FFFFh if there is no   }
                                { FileSysInfo sector. Otherwise, this value    }
                                { must be non-zero and less than the reserved  }
                                { sector count.                                }
    ExtBkUpBootSec     : Word;  { Sector containing the backup boot sector.    }
                                { This field is set to 0FFFFh if there is no   }
                                { backup boot sector. Otherwise, this value    }
                                { must be non-zero and less than the reserved  }
                                { sector count.                                }
    ExtFirstSector     : DWord; { First sector of the first cluster.           }
    ExtMaxCluster      : DWord; { Number of clusters on drive plus 1.          }
    ExtFATSize         : DWord; { Number of sectors occupied by each FAT.      }
    ExtRootCluster     : DWord; { First cluster in the root directory.         }
    ExtNextFree        : DWord; { Most recently allocated cluster. Cluster at  }
                                { which to start search for free clusters.     }
  end;

  PBPB = ^TBPB;
  TBPB = packed record { BIOS Parameter Block for FAT12 and FAT16: }
    BytesPerSector     : Word;  { Number of bytes per sector.                  }
    SectorsPerCluster  : Byte;  { Number of sectors per cluster.               }
    ReservedSectors    : Word;  { Number of reserved sectors.                  }
    NumberOfFATs       : Byte;  { Number of File Allocation Tables.            }
    RootDirEntries     : Word;  { Number of entries in root directory.         }
    SectorsOnDrive     : Word;  { Number of sectors on the drive or partition. }
    MediaDescriptor    : Byte;  { Media descriptor.                            }
    SectorsPerFAT      : Word;  { Number of sectors per FAT.                   }
    SectorsPerTrack    : Word;  { Number of sectors per track.                 }
    NumberOfHeads      : Word;  { Number of heads on the drive.                }
    HiddenSectors      : DWord; { Number of hidden sectors on the drive.       }
    BigSectorsOnDrive  : DWord; { Number of sectors if SectorsOnDrive=0.       }
  end;

  PExtBPB = ^TExtBPB;
  TExtBPB = packed record { Extended BIOS Parameter Block for FAT32: }
    BytesPerSector     : Word;  { Number of bytes per sector.                  }
    SectorsPerCluster  : Byte;  { Number of sectors per cluster.               }
    ReservedSectors    : Word;  { Number of reserved sectors.                  }
    NumberOfFATs       : Byte;  { Number of File Allocation Tables.            }
    RootDirEntries     : Word;  { Number of entries in root directory.         }
                                { Ignored on FAT32 drives.                     }
    SectorsOnDrive     : Word;  { Number of sectorson the drive or partition.  }
    MediaDescriptor    : Byte;  { Media descriptor.                            }
    SectorsPerFAT      : Word;  { Number of sectors per FAT.                   }
                                { This field will be zero in a FAT32 BPB.      }
                                { Use BigSectorsPerFat for FAT32 media.        }
    SectorsPerTrack    : Word;  { Number of sectors per track.                 }
    NumberOfHeads      : Word;  { Number of heads on the drive.                }
    HiddenSectors      : DWord; { Number of hidden sectors on the drive.       }
    BigSectorsOnDrive  : DWord; { Number of sectors if SectorsOnDrive=0.       }
    BigSectorsPerFat   : DWord; { Number of sectors per FAT on FAT32 drive.    }
    ExtFlags           : Word;  { Flags describing the drive:                  }
                                { bits 0-3: 0-based number of active FAT       }
                                { bits 4-6: reserved                           }
                                { bit 7: FAT mirroring enabled if bit cleared  }
                                { bits 8-15: reserved                          }
    FileSysVersion     : Word;  { File system version of the FAT32 drive:      }
                                { high byte: major version.                    }
                                { low byte: minor version.                     }
    RootDirStartCluster: DWord; { First cluster of root directory on FAT32     }
                                { drive.                                       }
    FileSysInfoSector  : Word;  { Sector containing information about the file }
                                { system in TBifFATBootFSInfo structure.       }
                                { This field is set to FFFFh if there is no    }
                                { FileSysInfo sector. Otherwise, this value    }
                                { must be non-zero and less than the reserved  }
                                { sector count.                                }
    BackupBootSector   : Word;  { Sector containing the backup boot sector.    }
                                { This field is set to FFFFh if there is no    }
                                { backup boot sector. Otherwise, this value    }
                                { must be non-zero and less than the reserved  }
                                { sector count.                                }
    Reserved           : array[0..5] of Word;
  end;

  PBigFATBootInfo = ^TBigFATBootInfo;
  TBigFATBootInfo = packed record
    Signature          : DWord; { Signature of FileSysInfo sector, 61417272h.  }
    FreeClustersCount  : DWord; { Number of free clusters on drive,            }
                                { FFFFFFFF if the number is unknown.           }
    NextFreeCluster    : DWord; { Most recently allocated cluster.             }
    Reserved           : array[0..5] of Word;
  end;

{- Device Parameters ----------------------------------------------------------}

  PSectorEntry = ^TSectorEntry;
  TSectorEntry = packed record { Sector entry for SectorTable: }
    SectorNumber: Word;
    SectorSize  : Word;
  end;

  PDeviceParameters = ^TDeviceParameters;
  TDeviceParameters = packed record { Device Parameters for FAT12 and FAT16: }
    SpecialFunctions   : Byte; { Special functions:                            }
                               { bit 0 set to use current BPB                  }
                               {       clear to use BPB in this structure      }
                               { bit 1 set to use track layout field only      }
                               {       must be clear for get function          }
                               { bit 2 set if all sectors has the same size    }
                               {       (should be set)                         }
                               { bit 3-7 reserved                              }
    DeviceType         : Byte; { Device type:                                  }
                               { 00h = 5.25" 320Kb/360Kb disk                  }
                               { 01h = 5.25" 1.2Mb disk                        }
                               { 02h = 3.5" 720Kb disk                         }
                               { 03h = 8" low-density disk                     }
                               { 04h = 8" high-density disk                    }
                               { 05h = hard disk                               }
                               { 06h = tape drive                              }
                               { 07h = 3.5" 1.44Mb disk                        }
                               { 08h = optical disk                            }
                               { 09h = 3.5" 2.88Mb disk                        }
    DeviceAttributes   : Word; { Device attributes:                            }
                               { bit 0 set if nonremovable media               }
                               { bit 1 set if door lock supported              }
                               { bit 2-15 reserved                             }
    Cylinders          : Word; { Number of cylinders                           }
    MediaType          : Byte; { Media type:                                   }
                               { for 1.2Mb drive:                              }
                               {   00h=1.2Mb disk                              }
                               {   01h=320Kb/360Kb disk                        }
                               { always 00h for other drive types              }
    BPB                : TBPB; { 31 bytes reserved for BPB                     }
    Fill31Bytes        : array[1..31-SizeOf(TBPB)] of Byte; { fill up 31 bytes }
    EntriesInTable     : Word; { Number of entries in SectorTable. Should be   }
                               { equal to SectorsPerTrack. (Not used by the    }
                               { get function.)                                }
    SectorTable        : array[0..0] of TSectorEntry; { When planing to use a  }
                               { device parameters structure with calls to set }
                               { function, one should allocate memory for this }
                               { structure (SizeOf(TDeviceParameters)) +       }
                               { SizeOf(TSectoryEntry) * SectorsPerTrack.      }
  end;

  PExtDeviceParameters = ^TExtDeviceParameters;
  TExtDeviceParameters = packed record { Extended Device Parameters for FAT32: }
    SpecialFunctions   : Byte; { Special functions:                            }
                               { bit 0 set to use current BPB                  }
                               {       clear to use BPB in this structure      }
                               { bit 1 set to use track layout field only      }
                               {       must be clear for get function          }
                               { bit 2 set if all sectors has the same size    }
                               {       (should be set)                         }
                               { bit 3-7 reserved                              }
    DeviceType         : Byte; { Device type:                                  }
                               { 00h = 5.25" 320Kb/360Kb disk                  }
                               { 01h = 5.25" 1.2Mb disk                        }
                               { 02h = 3.5" 720Kb disk                         }
                               { 03h = 8" low-density disk                     }
                               { 04h = 8" high-density disk                    }
                               { 05h = hard disk                               }
                               { 06h = tape drive                              }
                               { 07h = 3.5" 1.44Mb disk                        }
                               { 08h = optical disk                            }
                               { 09h = 3.5" 2.88Mb disk                        }
    DeviceAttributes   : Word; { Device attributes:                            }
                               { bit 0 set if nonremovable media               }
                               { bit 1 set if door lock supported              }
                               { bit 2-15 reserved                             }
    Cylinders          : Word; { Number of cylinders                           }
    MediaType          : Byte; { Media type:                                   }
                               { for 1.2Mb drive:                              }
                               {   00h=1.2Mb disk                              }
                               {   01h=320Kb/360Kb disk                        }
                               { always 00h for other drive types              }
    BPB                : TExtBPB; { ExtBPB for FAT32                           }
    Reserved           : array[0..31] of Byte;
    EntriesInTable     : Word; { Number of entries in SectorTable. Should be   }
                               { equal to SectorsPerTrack. (Not used by the    }
                               { get function.)                                }
    SectorTable        : array[0..0] of TSectorEntry; { When planing to use a  }
                               { device parameters structure with calls to set }
                               { function, one should allocate memory for this }
                               { structure (SizeOf(TFAT32DeviceParameters)) +  }
                               { SizeOf(TSectoryEntry) * SectorsPerTrack.      }
  end;

function  GetDeviceParameters(Drive: Byte; Buffer: Pointer; Size: Word): Boolean;
          { Returnes device parameters for a drive. The device parameters are  }
          { returned in the buffer pointet to by Buffer. The buffer should be  }
          { able to hold a TExtDeviceParameters structure. The size of the     }
          { structure wanted must be given in the Size parameter to identify   }
          { for the function which structure (TDeviceParameters or TExtDevice- }
          { Parameters) is wanted on return. If the function failes geting a   }
          { FAT32 device parameters structure it will get a FAT12/16 structure }
          { In any case it will convert the structure it gets to the structure }
          { that are wanted in the buffer on return.                           }
function  SetDeviceParameters(Drive: Byte; Buffer: Pointer; Size: Word): Boolean;
          { Sets the device parameters for a drive. The buffer pointed to by   }
          { Buffer should hold a correct device parameters structure for the   }
          { current file system (FAT12/16 or FAT32). The size of the structure }
          { must be given in the Size parameter to identify for the function   }
          { which structure (TDeviceParameters or TExtDeviceParameters) is     }
          { pased in the buffer. If the function failes seting a FAT32 device  }
          { parameters structure it will convert to a FAT12/16 structure and   }
          { try to set that. This will not affect the original structure in    }
          { the buffer.                                                        }

{ Even though these two functions can take and return either FAT12/16 or FAT32 }
{ device parameter (intentifying the actual structure by the Size parameter)   }
{ it will be wise not to use the FAT12/16 structure, but to only implement the }
{ FAT32 structure. For these two functions the FAT32 structure will work fine  }
{ on FAT12/16 drives as well as for FAT32. However, the FAT12/16 structure will}
{ not be appropriate for working with FAT32 drives. By usin the FAT32 structure}
{ in any case there will be no need to match the device parameter structure to }
{ the different drives because the functions will temporarily convert a FAT32  }
{ structure to FAT12/16 if needed.                                             }


{- Cylinders, Heads, Sectors --------------------------------------------------}
{                                                                              }
{ Sectors are the smallest accessable units on a disk drive, and (usually) are }
{ 512 bytes in size. Sectors are organized on a drive in a matrix of cylinders,}
{ heads, and sectors. The cylinder/head/sector (CHS) values are referred to as }
{ a drive's geometry. Hard disks are made up of several disk plates stacked on }
{ top of each other, with tracks defined as concentric circles on both sides   }
{ of each plate. The read/write heads are part of an assembly with one head    }
{ for each side of each platter. In the CHS system, cylinder refers to the set }
{ of tracks for all the plates that line up on top of each other, head refers  }
{ to a side of a platter, and a sector is a 512 bytes portion of a track.      }
{ The cylinders on a drive is numbered from 0 and up, heads are numbered from  }
{ 0 and up, and sectors on each track are numbered from 1 and up.              }

{ Logical sectors are numbered sequentially from cylinder 0 and up, head 0 and }
{ up. Sector 1 on head 0, cylinder 0 is the first logical sector (sector 0) on }
{ the drive, and sector 2 on the same head and cylinder is the second logical  }
{ sector (sector 1). The formula for calculating logical sector numbers are:   }
{     Cylinder * (BPB.NumberOfHeads * BPB.SectorsPerTrack)                     }
{   + Head * BPB.SectorsPerTrack                                               }
{   + Sector - 1 (-1 because logical sectors on a drive are 0-based)           }


{- Direct Drive Access --------------------------------------------------------}

{ Windows 95 OSR2 has a bug that affects interrupt 21h function 440Dh code 61h }
{ (read track) and code 41h (write track). The function succeed but no data is }
{ read or written. Both functions work on the retail version of Windows 95.    }
{ Solutions: (1) Use interrupt 25h and 26h to read and write logical sectors.  }
{                This works on FAT12 and FAT16, but is not compatible with     }
{                FAT32.                                                        }
{            (2) Use interrupt 21h function 7305h to read and write logical    }
{                sectors. This method works on FAT12, FAT16, and FAT32, but    }
{                is only supported on Windows 95 OSR2 and later.               }

function  ReadTrack(Drive: Byte; Cylinder,Head,Sector,Count: Word; Buffer: Pointer): Boolean;
          { Reads Count number of sectors from the drive into the buffer. On   }
          { Windows 95 OSR2 this function will return successful but no data   }
          { will be read.                                                      }
function  WriteTrack(Drive: Byte; Cylinder,Head,Sector,Count: Word; Buffer: Pointer): Boolean;
          { Writes Count number of sectors from the buffer onto the drive. On  }
          { Windows 95 OSR2 this function will return successful but no data   }
          { will be writen.                                                    }
function  FormatTrack(Drive: Byte; Cylinder,Head: Word): Boolean;
          { Formats and verifies a track on the drive.                         }
function  VerifyTrack(Drive: Byte; Cylinder,Head: Word): Boolean;
          { Verifies a track on the drive.                                     }

const
  { Write modes for WriteSector: }
  WRITE_MODE_UNSPECIFIED_DATA = $0000; { Other/unknown }
  WRITE_MODE_FAT_DATA         = $1000; { FAT data }
  WRITE_MODE_DIRECTORY_DATA   = $2000; { Directory data }
  WRITE_MODE_NORMAL_FILE_DATA = $3000; { Normal file data }

function  ReadSector(Drive: Byte; Sector: DWord; Count: Word; Buffer: Pointer): Boolean;
          { Reads Count number of logical sectors from the drive into the      }
          { buffer. The Sector parameter gives the sector number (0 beeing the }
          { first sector on a drive) to start reading from. The buffer must be }
          { able to hold BytesPerSector*Count number of bytes. On Windows 95   }
          { OSR2 and later this function uses interrupt 21h function 7305h.    }
function  WriteSector(Drive: Byte; Sector: DWord; Count: Word; Buffer: Pointer;  Mode: Byte): Boolean;
          { Write Count number of logical sectors from the buffer onto the     }
          { drive. On Windows 95 OSR2 and later this function uses interrupt   }
          { 21h function 7305h. The mode (one of the write modes) is needed in }
          { Windows 95 OSR2 and and later only. This provides information to   }
          { applications (like compression drivers) so that they can write     }
          { data properly based upon the data type specified.                  }


{- Master Boot Record ---------------------------------------------------------}

{ The Master Boot Record (MBR) is the first physical sector of a hard disk,    }
{ and always contains a partition table. This partition table is 64 bytes long }
{ and is followed by a two-byte signature (55AAh), which indicates the end of  }
{ the MBR. The 64-byte partition table and the 2-byte signature occupies the   }
{ last 66 bytes of the sector.                                                 }

type
  PPartitionEntry = ^TPartitionEntry;
  TPartitionEntry = packed record
    BootIndicator      : Byte;  { Boot indicator:                              }
                                { 80h = active/bootable                        }
                                { 00h = nonactive/non-bootable                 }
    StartHead          : Byte;  { Starting head of partition.                  }
    StartCylinderSector: Word;  { Starting cylinder and sector of partition:   }
                                { 10 bits for cylinder, 6 bits for sector.     }
                                { bits 0-5  6 bits of sector                   }
                                { bits 6-7  2 high bits of cylinder            }
                                { bits 8-15 8 low bits of cylinder             }

    { Partitions usually start on sector 1, head 0, except the first partition }
    { after the master boot record which may start in sector 2.                }

    PartitionType      : Byte;  { Partition type (system flag/indicator):      }
                                { 00h = unused partition table entry           }
                                { 01h = FAT12 primary partition <16Mb          }
                                { 02h = XENIX                                  }
                                { 04h = FAT16 primary partition 16-32Mb        }
                                { 05h = FAT12/16 extended partition            }
                                { 06h = FAT16 primary partition >32Mb          }
                                { 07h = HPFS, NTFS, Unix, other                }
                                { 0Ah = OS/2 boot manager                      }
                                { 0Bh = FAT32                                  }
                                { 0Ch = FAT32 LBA (ext int 13h required)       }
                                { 0Eh = FAT16 LBA (ext int 13h required)       }
                                { 0Fh = FAT16 LBA ext (ext int 13h required)   }
                                { 64h = Novell                                 }
                                { 75h = PCIX                                   }
                                { 83h = Linux EXT2                             }
                                { A5h = FreeBSD, NetBSD, 386BSD                }
                                { DBh = CP/M                                   }
                                { FFh = BBT                                    }
    EndHead            : Byte;  { Ending head of partition.                    }
    EndCylinderSector  : Word;  { Ending cylinder and sector of partition:     }
                                { 10 bits for cylinder, 6 bits for sector.     }
    SectorOffset       : DWord; { Relative sector offset of partition.         }
                                { (Number of sectors preceding partition.)     }
    NumberOfSectors    : DWord; { Number of sectors in partition.              }
  end;

  PPartitionTable = ^TPartitionTable;
  TPartitionTable = array[1..4] of TPartitionEntry;

  PMBR = ^TMBR;
  TMBR = packed record { Master Boot Record: }
    { The first portion (446 bytes on disks with 512 bytes per sector) of a    }
    { master boot record, preceding the partition table and two-byte signature }
    { is occupied by executable bootup code.                                   }
    BootCode           : array[0..445] of Byte; { Bootup program code.         }

    PartitionTable     : TPartitionTable; { Partition table occupying the last }
                                          { 64 bytes preceding the two-byte    }
                                          { signature (55h AAh).               }

    { The last two bytes in a boot record containes a two-byte signature, and  }
    { must always have the values: 55h AAh                                     }
    SectorSignature    : Word;
  end;

{- File System Boot Record ----------------------------------------------------}

  PFSBR = ^TFSBR;
  TFSBR = packed record { File System Boot Record for FAT12 and FAT16: }
    BootJump           : array[0..2] of Byte; { Jump instruction to boot code. }
    OEMName            : array[0..7] of Char; { System that formated the drive.}
    BPB                : TBPB;  { BPB for FAT12 and FAT16.                     }
    PhysicalDrive      : Byte;  { Number of physical drive.                    }
    Reserved           : Byte;
    BootSignature      : Byte;  { Boot record signature, usually 29h.          }
    VolumeSerial       : DWord; { Volume serial number.                        }
    VolumeLabel        : array[0..10] of Char; { Volume label.                 }
    SystemType         : array[0..7] of Char; { 'FAT12   ' or 'FAT16   '       }

    { The portion of a boot record following the extended BIOS parameter block }
    { (448 bytes on disks with 512 bytes per sector) is occupied by executable }
    { boot code.                                                               }
    BootCode           : array[0..447] of Byte; { Bootup program code.         }

    { The last two bytes in a boot record containes a two-byte signature, and  }
    { must always have the values: 55h AAh                                     }
    SectorSignature    : Word;
  end;

  PExtFSBR = ^TExtFSBR;
  TExtFSBR = packed record { Extended File System Boot Record for FAT32: }
    BootJump           : array[0..2] of Byte; { Jump instruction to boot code. }
    OEMName            : array[0..7] of Char; { System that formated the drive.}
    BPB                : TExtBPB; { ExtBPB for FAT32.                          }
    PhysicalDrive      : Byte;  { Number of physical drive.                    }
    Reserved           : Byte;
    BootSignature      : Byte;  { Boot record signature, usually 29h.          }
    VolumeSerial       : DWord; { Volume serial number.                        }
    VolumeLabel        : array[0..10] of Char; { Volume label.                 }
    SystemType         : array[0..7] of Char; { 'FAT32   '                     }

    { The portion of a boot record following the extended BIOS parameter block }
    { (420 bytes on disks with 512 bytes per sector) is occupied by executable }
    { boot code.                                                               }
    BootCode           : array[0..419] of Byte; { Bootup program code.         }

    { The last two bytes in any boot record always have the values: 55h AAh.   }
    SectorSignature    : Word;
  end;


{- File Allocation Table ------------------------------------------------------}

{ Although sectors are the smallest addressable units on a disk, the data area }
{ (the area used to store files and directories, opposed to the system area    }
{ where the boot record and FATs are stored) are organiced into clusters. A    }
{ clusters (or allocation units) occupy several sectors grouped together. The  }
{ number of sectors for each cluster depends on the partition, and is given in }
{ BPB.SectorsPerCluster.                                                       }

{ The File Allocation Table (FAT) is used to map each cluster in the data eara }
{ and will have one entry for each cluster on the drive. Thuse it is possible  }
{ to keep track of clusters allocated to each files and directory. The size of }
{ the entries in the FAT depends on the FAT-type: FAT12 uses 12-bit entries,   }
{ FAT16 uses 16-bit entries, and FAT32 uses 32-bit entries. Entries in a FAT   }
{ are indexed from 0 and up. Because the first two entries are reserved, entry }
{ cluster 2 (index 2 in the FAT) is the first data cluster on a drive. The     }
{ first byte in a FAT must always clontain a copy of the media desriptor byte  }
{ (BPB.MediaDescriptor), and the remaining bytes for the first and second FAT  }
{ entries (index 0 and 1) must be filled with FFh.                             }

{ Each entry in a FAT is an integer value. The values [FFFF]F]FF0-[FFFF]F]FFF  }
{ are reserved. [FFFF]F]FF7 indicates a bad cluster. [FFFF]F]FFF indicates the }
{ end of cluster chain. Values from [FFFF]F]FF8-[FFFF]F]FFFF may also be used  }
{ to indicate end of cluster chain. The value [0000]0]000 indicates an unused  }
{ cluster. All other value will represent the next cluster in a cluster chain. }


{- Directory Entries ----------------------------------------------------------}

  PDirEntry = ^TDirEntry;
  TDirEntry = packed record
    Name               : array[0..7] of Char; { 8 character name.              }
                         { first byte: 00h = entry never used                  }
                         {             05h = first character of name is E5h    }
                         {             E5h = entry has been deleted            }
                         {             2Eh = subdirectory entry                }
    Extention          : array[0..2] of Char; { 3 character extention.         }
    Attributes         : Byte;  { Bits: 0 = read-only                          }
                                {       1 = hidden                             }
                                {       2 = system                             }
                                {       3 = volume label (Name+Ext=11 label)   }
                                {       4 = directory                          }
                                {       5 = archive                            }
    Reserved           : array[0..9] of Char;
    CreationTime       : Word;  { Bits:  0-4  = seconds/2 (0-29)               }
                                {        5-10 = minutes (0-59)                 }
                                {       11-15 = hour (0-23)                    }
    CreationDate       : Word;  { Bits:  0-4  = day (0-31)                     }
                                {        5-8  = month (1-12)                   }
                                {        9-15 = year-1980                      }
    FirstCluster       : Word;  { First cluster of the file/directory.         }
    FileSize           : DWord; { Size of a file.                              }
  end;

  PLongNameDirEntry = ^TLongNameDirEntry;
  TLongNameDirEntry = packed record
    Sequence           : Byte; { Number in the chain of directory entries for  }
                               { this name, 01h,02h, ... 4xh (last entry will  }
                               { have bit 6 set to indicate end of chaine).    }
    Name               : array[0..4] of WideChar; { Charachters for long name. }
    Attributes         : Byte; { OFh (00001111b) a directory entry has the     }
                               { attributes read-only, hidden, system, and     }
                               { volume label set to indicate a long name.     }
    EntryType          : Byte; { 00h                                           }
    Checksum           : Byte; { Checksum for matching short (DOS) name.       }
    Name2              : array[0..5] of WideChar; { Charachters for long name. }
    Reserved           : Word;
    Name3              : array[0..1] of WideChar; { Charachters for long name. }
  end;

  PVolumeLabelDirEntry = ^TVolumeLabelDirEntry;
  TVolumeLabelDirEntry = packed record
    VolumeLabel        : array[0..10] of Char; { 11 character volume label.    }
    Attributes         : Byte;  { 00001000b (volume label attribute set)       }
    Reserved           : array[0..9] of Byte;
    CreationTime       : Word;
    CreationDate       : Word;
    Unused             : array[0..5] of Byte;
  end;


{------------------------------------------------------------------------------}

function  GetFirstCluster(Path: String; var Cluster: DWord): Boolean;
          { Retrives the first cluster of a file or directory. Caller must     }
          { a level 3 lock on the drive. It is the callers resposibility to    }
          { check that the returned cluster number is valid.                   }

function  MakeSerialNumber: DWord;
          { Generates a volume serial number using the current date and time.  }
          { The current system date and time is converted into DOS date and    }
          { DOS time, with date in the high word and time in the low word.     }


{------------------------------------------------------------------------------}

{ Most functions have a boolean return value to indicate success or failiour.  }
{ If a function failes an error code in the variable VWIN32Error, and when it  }
{ succedes VWIN32Error will be zero.                                           }

var
  VWIN32Error: DWord;

const
  ERROR_NON                  = $0000; { no error                               }

  { MS-DOS/Windows error codes: }
  ERROR_INVALID_FUNCTION     = $0001; { invalid function number                }
  ERROR_FILE_NOT_FOUND       = $0002; { file not found                         }
  ERROR_ACCESS_DENIED        = $0005; { specified access denied on drive       }
  ERROR_INVALID_DRIVE        = $000F; { invalid drive number                   }
  ERROR_MEDIA_NOT_LOCKED     = $00B0; { media is not locked in drive           }
  ERROR_MEDIA_LOCKED         = $00B1; { media is locked in drive               }
  ERROR_MEDIA_NOT_REMOVABLE  = $00B2; { media is not removable                 }
  ERROR_LOCKE_COUNT_EXCEEDED = $00B4; { media locke count exceeded             }
  ERROR_EJECT_REQUEST_FAILED = $00B5; { valid media eject request failed       }

  { Interrupt 13h/25h/26h error codes: }
  ERROR_BAD_COMMAND          = $10001; { bad command                           }
  ERROR_BAD_ADDRESS_MARK     = $10002; { bad address mark                      }
  ERROR_WRITE_PROTECTED      = $10003; { write-protected disk                  }
  ERROR_SECTOR_NOT_FOUND     = $10004; { requested sector not found            }
  ERROR_RESET_FAILED         = $10005; { reset failed                          }
  ERROR_DISK_CHANGED         = $10006; { disk changed (floppy disk)            }
  ERROR_PARAMETER_FAILED     = $10007; { drive parameter activity failed       }
  ERROR_DMA_FAILURE          = $10008; { DMA failure/overrun                   }
  ERROR_DMA_SEGMENT_FAULT    = $10009; { attempted DMA across 64K boundary     }
  ERROR_BAD_SECTOR_DETECTED  = $1000A; { bad sector detected                   }
  ERROR_BAD_TRACK_DETECTED   = $1000B; { bad track detected                    }
  ERROR_INVALID_MEDIA        = $1000C; { invalid media or unsupported track    }
  ERROR_INVALID_SECTORS      = $1000D; { invalid number of sectors on format   }
  ERROR_CONTROL_DATA         = $1000E; { control data address mark detected    }
  ERROR_DMA_ARBITRATION      = $1000F; { DMA arbitration level out of range    }
  ERROR_DATA_ERROR           = $10010; { data error (uncorrectable CRC or ECC) }
  ERROR_DATA_ECC_CORRECTED   = $10011; { data ECC corrected                    }
  ERROR_CONTROLLER_FAILED    = $10020; { controller failed                     }
  ERROR_SEEK_FAILED          = $10040; { seek operation failed                 }
  ERROR_DEVICE_FAILED        = $10080; { device failed to respond (timeout)    }
  ERROR_DRIVE_NOT_READY      = $100AA; { drive not ready                       }
  ERROR_UNDEFINED            = $100BB; { undefined error                       }
  ERROR_WRITE_FAULT          = $100CC; { write fault                           }
  ERROR_STATUS_REGISTER      = $100E0; { status register error                 }
  ERROR_SENSE_FAILED         = $100FF; { sense operation failed                }

  { VWIN32 custom error codes: }
  ERROR_UNKNOWN              = $00100000; { unknown error                      }
  ERROR_OS_NOT_SUPPORTED     = $00200000; { OS version not supported           }
  ERROR_OPENING_DEVICE       = $00300000; { error trying to open device        }

{==============================================================================}
implementation

var
  VWIN32Device : THandle;
  OSVersionInfo: TOSVersionInfo;

function VWIN32DIOC;
var
  BytesReturned : DWord;

  function OpenDevice: Boolean;
  begin
    VWIN32Error := ERROR_NON;
    if VWIN32Device = INVALID_HANDLE_VALUE then begin
      if WindowsVersion(0,0,0,VER_PLATFORM_WIN32_WINDOWS) then begin {supports only Win9x}
        VWIN32Device := CreateFile(VWIN32_DEVICE_NAME,0,0,nil,0,FILE_FLAG_DELETE_ON_CLOSE,0);
        if VWIN32Device = INVALID_HANDLE_VALUE then VWIN32Error := ERROR_OPENING_DEVICE;
      end
      else VWIN32Error := ERROR_OS_NOT_SUPPORTED;
    end;
    Result := VWIN32Error = ERROR_NON;
  end;

  procedure CloseDevice;
  begin
    if VWIN32Device <> INVALID_HANDLE_VALUE then CloseHandle(VWIN32Device);
    VWIN32Device := INVALID_HANDLE_VALUE;
    Result := true;
  end;

begin
  VWIN32Error := ERROR_NON;
  if ControlCode = VWIN32_DIOC_CLOSE then CloseDevice
  else if OpenDevice then begin
    Result := DeviceIoControl(VWIN32Device,ControlCode,Registers,SizeOf(Registers^),
                              Registers,SizeOf(Registers^),BytesReturned,nil);
    if not result then VWIN32Error := ERROR_UNKNOWN;
  end
  else Result := false;
end;

function WindowsVersion;
begin
  with OSVersionInfo do
    Result := (dwMajorVersion >= Major) and
              (dwMinorVersion >= Minor) and
              (Word(dwBuildNumber) >= Word(Build)) and
              (dwPlatformID = Platform);
end;

{ The device category pased as parameter to most of the following functions    }
{ is used to specifie FAT32, FAT16 or FAT12 drives: 08h = FAT16 or FAT12 drive,}
{ 48h = FAT32, FAT16 or FAT12 drive. The 48h value is supported on Windows 95  }
{ OSR2 and later only. Because function calls may be implemented in the device }
{ driver, the 48h form of a call may fail on FAT16 or FAT12 media. The caller  }
{ must fall back on the 08h form if the 48h form call fails. Therefore, the    }
{ folowing functions first makes a 48h form call, but only on Windows 95 OSR2  }
{ and later, and then a 08h form call if the 48h form call failed.             }

function DriveIsRemovable;
{ int 21h, func 4408h                               }
{ in  AX = 4408h                                    }
{     BL = drive number                             }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     AX = 0000h=removable 0001h=fixed              }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  with Registers do begin
    EAX := $4408;
    EBX := Drive;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := (AX and $0001) = 0
      else VWIN32Error := AX;
    end;
  end;
end;

function DriveIsRemote;
{ int 21h, func 4409h                               }
{ in  AX = 4409h                                    }
{     BL = drive number                             }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     DX = device attribute                         }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  with Registers do begin
    EAX := $4409;
    EBX := Drive;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := (DX and $1000) > 0
      else VWIN32Error := AX;
    end;
  end;
end;

function DriveIsSubstitute;
{ int 21h, func 4409h                               }
{ in  AX = 4409h                                    }
{     BL = drive number                             }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     DX = device attribute                         }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  with Registers do begin
    EAX := $4409;
    EBX := Drive;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := (DX and $8000) > 0
      else VWIN32Error := AX;
    end;
  end;
end;

function DirectAccessAllowed;
{ int 21h, func 4409h                               }
{ in  AX = 4409h                                    }
{     BL = drive number                             }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     DX = device attribute                         }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  with Registers do begin
    EAX := $4409;
    EBX := Drive;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := (DX and $0200) = 0
      else VWIN32Error := AX;
    end;
  end;
end;

type
  TParamBlock = packed record
    Operation : Byte;
    NumLocks  : Byte;
  end;

function LockRemovableMedia;
{ int 21h, func 440Dh, code 48h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 48h                                      }
{     DS:DX -> address of ParamBlock                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     ParamBlock.NumLocks = number of pending locks }
var
  Registers : TDIOC_Registers;
  ParamBlock: TParamBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  ParamBlock.Operation := 0; {locks the volume in the drive}
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function UnlockRemovableMedia;
{ int 21h, func 440Dh, code 48h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 48h                                      }
{     DS:DX -> address of ParamBlock                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     ParamBlock.NumLocks = number of pending locks }
var
  Registers : TDIOC_Registers;
  ParamBlock: TParamBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  ParamBlock.Operation := 1; {unlocks the volume in the drive}
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function GetRemovableMediaLocks;
{ int 21h, func 440Dh, code 48h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 48h                                      }
{     DS:DX -> address of ParamBlock                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     ParamBlock.NumLocks = number of pending locks }
var
  Registers : TDIOC_Registers;
  ParamBlock: TParamBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  ParamBlock.Operation := 2; {returns the lock or unlock status}
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0848;
    EDX := DWord(@ParamBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then Locks := ParamBlock.NumLocks;
end;

function EjectRemovableMedia;
{ int 21h, func 440Dh, code 49h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 49h                                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers : TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4849;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0849;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

type
  TAccessFlag = packed record
    SpecialFunction: Byte; { must be zero }
    AccessFlag     : Byte;
  end;

function GetAccessFlag;
{ int 21h, func 440Dh, code 67h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 67h                                      }
{     DS:DX -> address of buffer                    }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     buffer = AccessFlag                           }
var
  Registers : TDIOC_Registers;
  AccessFlag: TAccessFlag;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4867;
    AccessFlag.SpecialFunction := 0;
    EDX := DWord(@AccessFlag);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0867;
    AccessFlag.SpecialFunction := 0;
    EDX := DWord(@AccessFlag);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then Flag := AccessFlag.AccessFlag;
end;

function SetAccessFlag;
{ int 21h, func 440Dh, code 47h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 47h                                      }
{     DS:DX -> address of AccessFlag                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers : TDIOC_Registers;
  AccessFlag: TAccessFlag;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4847;
    AccessFlag.SpecialFunction := 0;
    AccessFlag.AccessFlag := Flag;
    EDX := DWord(@AccessFlag);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0847;
    AccessFlag.SpecialFunction := 0;
    AccessFlag.AccessFlag := Flag;
    EDX := DWord(@AccessFlag);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function LockLogicalVolume;
{ int 21h, func 440Dh, code 4Ah                     }
{ in  AX = 440Dh                                    }
{     BH = lock level (0..3)                        }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 4Ah                                      }
{     DX = permisson                                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Level > 1 then Permission := $00
  else Permission := Permission and $07;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := (Level shl 8) or Drive;
    ECX := $484A;
    EDX := Permission;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := (Level shl 8) or Drive;
    ECX := $084A;
    EDX := Permission;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function LockPhysicalVolume;
{ int 21h, func 440Dh, code 4Bh                     }
{ in  AX = 440Dh                                    }
{     BH = lock level (0..3)                        }
{     BL = disk number                              }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 4Bh                                      }
{     DX = permisson                                }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Level > 1 then Permission := $00
  else Permission := Permission and $07;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := (Level shl 8) or Disk;
    ECX := $484B;
    EDX := Permission;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := (Level shl 8) or Disk;
    ECX := $084B;
    EDX := Permission;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function UnlockLogicalVolume;
{ int 21h, func 440Dh, code 6Ah                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Ah                                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $486A;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $086A;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
  end;
end;

function UnlockPhysicalVolume;
{ int 21h, func 440Dh, code 6Bh                     }
{ in  AX = 440Dh                                    }
{     BL = disk number                              }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Bh                                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Disk;
    ECX := $486B;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Disk;
    ECX := $086B;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function GetCurrentLockState;
{ int 21h, func 440Dh, code 70h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 70h                                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     AX = lock level                               }
{     CX = lock permission                          }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4870;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0870;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then with Registers do begin
    Level := AX;
    Permission := CX;
  end;
end;

function GetLockFlagState;
{ int 21h, func 440Dh, 6Ch                          }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Ch                                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     AX = flag state                               }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $486C;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $086C;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then with Registers do Flags := AX;
end;

function EnumerateOpenFiles;
{ int 21h, func 440Dh, code 6Dh                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Dh                                      }
{     DS:DX -> address of buffer                    }
{     SI = file index                               }
{     DI = enumeraton type (0=all, 1=unmovable)     }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     AX = open mode                                }
{     CX = file type                                }
{     buffer = file path                            }
var
  Registers : TDIOC_Registers;
  PathBuffer: PChar;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  GetMem(PathBuffer,MAX_PATH);
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $486D;
    EDX := DWord(PathBuffer);
    ESI := FileIndex;
    EDI := 0;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $086D;
    EDX := DWord(PathBuffer);
    ESI := FileIndex;
    EDI := 0;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then with Registers do begin
    FilePath := PathBuffer;
    OpenMode := AX;
    FileType := CX;
  end;
  FreeMem(PathBuffer,MAX_PATH);
end;

function FindSwapFile;
{ int 21h, func 440Dh, code 6Eh                     }
{ in  AX = 440Dh                                    }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Eh                                      }
{     DS:DX -> address of buffer                    }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     AX = pager type                               }
{     CX:BX = file size (in 4Kb pages)              }
{     buffer = file path                            }
var
  Registers : TDIOC_Registers;
  PathBuffer: PChar;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  GetMem(PathBuffer,MAX_PATH);
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    ECX := $486E;
    EDX := DWord(PathBuffer);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    ECX := $086E;
    EDX := DWord(PathBuffer);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then with Registers do begin
    FilePath := PathBuffer;
    PagerType := AX;
    PageCount := (CX shl 16) or BX;
  end;
  FreeMem(PathBuffer,MAX_PATH);
end;

{function ResetDrive;}
{ don't know how to perform this interrupt in Win9x }
{ int 21h, func 710Dh                               }
{ in  AX = 710Dh                                    }
{     CX = flag                                     }
{     DX = drive number                             }

function GetMediaIdentifier;
{ int 21h, func 440Dh, code 66h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 66h                                      }
{     DS:DX -> address of buffer                    }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     buffer = MediaIdentifier                      }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4866;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0866;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function SetMediaIdentifier;
{ int 21h, func 440Dh, code 46h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 46h                                      }
{     DS:DX -> address of MediaIdentifier           }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4846;
    Media.InfoLevel := 0;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0846;
    Media.InfoLevel := 0;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function SenseMediaType;
{ int 21h, func 440Dh, code 68h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 68h                                      }
{     DS:DX -> address of MediaIdentifier           }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4868;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0868;
    EDX := DWord(@Media);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

type
  TDriveMapInfoBuffer = packed record
    AllocationLength   : Byte; { length of this structure upon call }
    InfoLength         : Byte; { number of bytes used in the structure }
    DriveMapInfo       : TDriveMapInfo;
  end;

function GetDriveMapInfo;
{ int 21h, func 440Dh, code 6Fh                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 6Fh                                      }
{     DS:DX -> address of buffer                    }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     buffer = DriveMapInfo                         }
var
  Registers: TDIOC_Registers;
  Buffer   : TDriveMapInfoBuffer;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $486F;
    Buffer.AllocationLength := SizeOf(TDriveMapInfoBuffer);
    EDX := DWord(@Buffer);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $086F;
    Buffer.AllocationLength := SizeOf(TDriveMapInfoBuffer);
    EDX := DWord(@Buffer);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then Info := Buffer.DriveMapInfo;
end;

function GetDeviceParameters;
{ int 21h, func 440Dh, code 60h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 60h                                      }
{     DS:DX -> address of buffer                    }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     buffer = DeviceParameters                     }
var
  Registers   : TDIOC_Registers;
  TmpDevPar   : PDeviceParameters;
  TmpExtDevPar: PExtDeviceParameters;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    TmpExtDevPar := nil;
    EAX := $440D;
    EBX := Drive;
    ECX := $4860;
    if Size = SizeOf(TExtDeviceParameters) then EDX := DWord(Buffer)
    else begin
      GetMem(TmpExtDevPar,SizeOf(TExtDeviceParameters));
      EDX := DWord(TmpExtDevPar)
    end;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then begin
        Result := true;
        if Size = SizeOf(TDeviceParameters) then begin
          PDeviceParameters(Buffer)^ := PDeviceParameters(TmpExtDevPar)^;
          with PDeviceParameters(Buffer)^ do begin
            FillChar(Fill31Bytes,SizeOf(Fill31Bytes),0);
            EntriesInTable := 0;
          end;
          ReallocMem(TmpExtDevPar,0);
        end;
      end;
  end;
  if not Result then with Registers do begin
    TmpDevPar := nil;
    EAX := $440D;
    EBX := Drive;
    ECX := $0860;
    if Size = SizeOf(TDeviceParameters) then EDX := DWord(Buffer)
    else begin
      GetMem(TmpDevPar,SizeOf(TExtDeviceParameters));
      EDX := DWord(TmpDevPar)
    end;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then begin
        Result := true;
        if Size = SizeOf(TExtDeviceParameters) then begin
          PDeviceParameters(Buffer)^ := PDeviceParameters(TmpDevPar)^;
          with PExtDeviceParameters(Buffer)^ do begin
            BPB.BigSectorsPerFat := 0;
            BPB.ExtFlags := 0;
            BPB.FileSysVersion := 0;
            BPB.RootDirStartCluster := 0;
            BPB.FileSysInfoSector := $FFFF;
            BPB.BackupBootSector := $FFFF;
            FillChar(BPB.Reserved,SizeOf(BPB.Reserved),0);
            FillChar(Reserved,SizeOf(Reserved),0);
          end;
          ReallocMem(TmpDevPar,0);
        end;
      end
      else VWIN32Error := AX;
    end;
  end;
end;

function SetDeviceParameters;
{ int 21h, func 440Dh, code 40h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 40h                                      }
{     DS:DX -> address of DeviceParameters          }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers   : TDIOC_Registers;
  TmpDevPar   : PDeviceParameters;
  TmpExtDevPar: PExtDeviceParameters;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 and (Size = SizeOf(TExtDeviceParameters)) then with Registers do begin
    TmpExtDevPar := nil;
    EAX := $440D;
    EBX := Drive;
    ECX := $4840;
    if Size = SizeOf(TExtDeviceParameters) then EDX := DWord(Buffer)
    else begin
      GetMem(TmpExtDevPar,SizeOf(TExtDeviceParameters)
             +SizeOf(TSectorEntry)*PExtDeviceParameters(Buffer)^.EntriesInTable);
      PDeviceParameters(TmpExtDevPar)^ := PDeviceParameters(Buffer)^;
      with TmpExtDevPar^ do begin
        BPB.BigSectorsPerFat := 0;
        BPB.ExtFlags := 0;
        BPB.FileSysVersion := 0;
        BPB.RootDirStartCluster := 0;
        BPB.FileSysInfoSector := $FFFF;
        BPB.BackupBootSector := $FFFF;
        FillChar(BPB.Reserved,SizeOf(BPB.Reserved),0);
        FillChar(Reserved,SizeOf(Reserved),0);
      end;
      with PDeviceParameters(Buffer)^ do begin
        TmpExtDevPar^.EntriesInTable := EntriesInTable;
        CopyMemory(@TmpExtDevPar^.SectorTable,
                   @SectorTable,EntriesInTable*SizeOf(TSectorEntry));
      end;
      EDX := DWord(TmpExtDevPar);
    end;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
    if TmpExtDevPar <> nil then ReallocMem(TmpExtDevPar,0);
  end;
  if not Result then with Registers do begin
    TmpDevPar := nil;
    EAX := $440D;
    EBX := Drive;
    ECX := $0840;
    if Size = SizeOf(TDeviceParameters) then EDX := DWord(Buffer)
    else begin
      GetMem(TmpDevPar,SizeOf(TExtDeviceParameters)
             +SizeOf(TSectorEntry)*PExtDeviceParameters(Buffer)^.EntriesInTable);
      PDeviceParameters(TmpDevPar)^ := PDeviceParameters(Buffer)^;
      with PExtDeviceParameters(Buffer)^ do begin
        TmpDevPar^.EntriesInTable := EntriesInTable;
        CopyMemory(@TmpDevPar^.SectorTable,
                   @SectorTable,EntriesInTable*SizeOf(TSectorEntry));
      end;
      EDX := DWord(TmpDevPar);
    end;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
    if TmpDevPar <> nil then ReallocMem(TmpDevPar,0);
  end;
end;

type
  TReadWriteBlock = packed record
    SpecFunc   : Byte; { must be zero }
    Head       : Word;
    Cylinder   : Word;
    FirstSector: Word;
    Sectors    : Word;
    Buffer     : Pointer;
  end;

function ReadTrack;
{ int 21h, func 440Dh, code 61h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 61h                                      }
{     DS:DX -> address of ReadWriteBlock            }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers     : TDIOC_Registers;
  ReadWriteBlock: TReadWriteBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4861;
    ReadWriteBlock.SpecFunc := 0;
    ReadWriteBlock.Head := Head;
    ReadWriteBlock.Cylinder := Cylinder;
    ReadWriteBlock.Firstsector := Sector;
    ReadWriteBlock.Sectors := Count;
    ReadWriteBlock.Buffer := Buffer;
    EDX := DWord(@ReadWriteBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0861;
    ReadWriteBlock.SpecFunc := 0;
    ReadWriteBlock.Head := Head;
    ReadWriteBlock.Cylinder := Cylinder;
    ReadWriteBlock.Firstsector := Sector;
    ReadWriteBlock.Sectors := Count;
    ReadWriteBlock.Buffer := Buffer;
    EDX := DWord(@ReadWriteBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function  WriteTrack;
{ int 21h, func 440Dh, code 41h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 41h                                      }
{     DS:DX -> address of ReadWriteBlock            }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers     : TDIOC_Registers;
  ReadWriteBlock: TReadWriteBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4841;
    ReadWriteBlock.SpecFunc := 0;
    ReadWriteBlock.Head := Head;
    ReadWriteBlock.Cylinder := Cylinder;
    ReadWriteBlock.Firstsector := Sector;
    ReadWriteBlock.Sectors := Count;
    ReadWriteBlock.Buffer := Buffer;
    EDX := DWord(@ReadWriteBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0841;
    ReadWriteBlock.SpecFunc := 0;
    ReadWriteBlock.Head := Head;
    ReadWriteBlock.Cylinder := Cylinder;
    ReadWriteBlock.Firstsector := Sector;
    ReadWriteBlock.Sectors := Count;
    ReadWriteBlock.Buffer := Buffer;
    EDX := DWord(@ReadWriteBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

type
  TFormatVerifyBlock = packed record
    SpecFunc: Byte;
    Head    : Word;
    Cylinder: Word;
    Tracks  : Word;
  end;

function FormatTrack;
{ int 21h, func 440Dh, code 42h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 42h                                      }
{     DS:DX -> address of FormatVerifyBlock         }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers        : TDIOC_Registers;
  FormatVerifyBlock: TFormatVerifyBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4842;
    FormatVerifyBlock.SpecFunc := 0;
    FormatVerifyBlock.Head := Head;
    FormatVerifyBlock.Cylinder := Cylinder;
    FormatVerifyBlock.Tracks := 1;
    EDX := DWord(@FormatVerifyBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0842;
    FormatVerifyBlock.SpecFunc := 0;
    FormatVerifyBlock.Head := Head;
    FormatVerifyBlock.Cylinder := Cylinder;
    FormatVerifyBlock.Tracks := 1;
    EDX := DWord(@FormatVerifyBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

function VerifyTrack;
{ int 21h, func 440Dh, code 62h                     }
{ in  AX = 440Dh                                    }
{     BL = drive number                             }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 62h                                      }
{     DS:DX -> address of FormatVerifyBlock         }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers        : TDIOC_Registers;
  FormatVerifyBlock: TFormatVerifyBlock;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $4862;
    FormatVerifyBlock.SpecFunc := 0;
    FormatVerifyBlock.Head := Head;
    FormatVerifyBlock.Cylinder := Cylinder;
    FormatVerifyBlock.Tracks := 1;
    EDX := DWord(@FormatVerifyBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := Drive;
    ECX := $0862;
    FormatVerifyBlock.SpecFunc := 0;
    FormatVerifyBlock.Head := Head;
    FormatVerifyBlock.Cylinder := Cylinder;
    FormatVerifyBlock.Tracks := 1;
    EDX := DWord(@FormatVerifyBlock);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
end;

type
  TReadWritePacket = packed record
    StartSector: DWord;
    Sectors    : Word;
    Buffer     : Pointer;
  end;

function ReadSector;
{ int 21h, func 7305h                               }
{ in  AX = 7305h                                    }
{     DS:BX -> address of ReadWritePacket           }
{     CX = must be -1 (FFFFh)                       }
{     DL = drive number                             }
{     SI = 0000h (read)                             }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{                                                   }
{ int 25h                                           }
{ in  AL = drive number (0-based)                   }
{     DS:BX -> address of ReadWritePacket           }
{     CX = FFFFh (large hard disk partition)        }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers      : TDIOC_Registers;
  ReadWritePacket: TReadWritePacket;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $7305;
    ReadWritePacket.StartSector := Sector;
    ReadWritePacket.Sectors := Count;
    ReadWritePacket.Buffer := Buffer;
    EBX := DWord(@ReadWritePacket);
    ECX := $FFFFFFFF;
    EDX := Drive;
    ESI := $0000;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_DRIVEINFO,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := Drive-1;
    ReadWritePacket.StartSector := Sector;
    ReadWritePacket.Sectors := Count;
    ReadWritePacket.Buffer := Buffer;
    EBX := DWord(@ReadWritePacket);
    ECX := $FFFFFFFF;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_INT25,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := $10000 or (AX and $00FF);
    end;
  end;
end;

function WriteSector;
{ int 21h, func 7305h                               }
{ in  AX = 7305h                                    }
{     DS:BX -> address of ReadWritePacket           }
{     CX = must be -1 (FFFFh)                       }
{     DL = drive number                             }
{     SI = 0001h (write) or write mode              }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{                                                   }
{ int 26h                                           }
{ in  AL = drive number (0-based)                   }
{     DS:BX -> address of ReadWritePacket           }
{     CX = FFFFh (large hard disk partition)        }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
var
  Registers      : TDIOC_Registers;
  ReadWritePacket: TReadWritePacket;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $7305;
    ReadWritePacket.StartSector := Sector;
    ReadWritePacket.Sectors := Count;
    ReadWritePacket.Buffer := Buffer;
    EBX := DWord(@ReadWritePacket);
    ECX := $FFFFFFFF;
    EDX := Drive;
    ESI := $0001 or (Mode and $6000);
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_DRIVEINFO,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := Drive-1;
    ReadWritePacket.StartSector := Sector;
    ReadWritePacket.Sectors := Count;
    ReadWritePacket.Buffer := Buffer;
    EBX := DWord(@ReadWritePacket);
    ECX := $FFFFFFFF;
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_INT26,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := $10000 or (AX and $00FF);
    end;
  end;
end;

function GetFirstCluster;
{ int 21h, func 440Dh, code 71h                     }
{ in  AX = 440Dh                                    }
{     BX = character set (0=ANSI, 1=OEM, 2=UNICODE) }
{     CH = device category (08h=FAT16, 48h=FAT32)   }
{     CL = 71h                                      }
{     DS:DX -> address of path                      }
{ out CF set on error                               }
{     AX = status                                   }
{ out CF clear if successful                        }
{     DX:AX = cluster number                        }
var
  Registers: TDIOC_Registers;
begin
  VWIN32Error := ERROR_NON;
  Result := false;
  if Win95OSR2 then with Registers do begin
    EAX := $440D;
    EBX := 0;
    ECX := $4871;
    EDX := DWord(PChar(Path));
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then
      if (Flags and FLAG_CARRY) = 0 then Result := true;
  end;
  if not Result then with Registers do begin
    EAX := $440D;
    EBX := 0;
    ECX := $0871;
    EDX := DWord(PChar(Path));
    Flags := $00000000;
    if VWIN32DIOC(VWIN32_DIOC_DOS_IOCTL,@Registers) then begin
      if (Flags and FLAG_CARRY) = 0 then Result := true
      else VWIN32Error := AX;
    end;
  end;
  if Result then with Registers do Cluster := EAX;
  {?: if Result then with Registers do Cluster := (DX shl 16) or AX;}
end;

function MakeSerialNumber;
var
  SystemTime: TSystemTime;
  FileTime  : TFileTime;
  DateTime  : array[0..1] of Word;
begin
  GetSystemTime(SystemTime);
  SystemTimeToFileTime(SystemTime,FileTime);
  FileTimeToDosDateTime(FileTime,DateTime[1],DateTime[0]);
  Result := DWord(DateTime);
end;

{==============================================================================}
Initialization
  VWIN32Error := ERROR_NON;
  VWIN32Device := INVALID_HANDLE_VALUE;
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Win95OSR2 := WindowsVersion(0,0,1081,VER_PLATFORM_WIN32_WINDOWS);

finalization
  VWIN32DIOC(VWIN32_DIOC_CLOSE,nil);

end.
