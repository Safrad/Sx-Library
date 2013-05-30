{ *********************************************************************** }
{                                                                         }
{ Delphi / Kylix Cross-Platform Runtime Library                           }
{ System Utilities Unit                                                   }
{                                                                         }
{ Copyright (c) 1995-2002 Borland Softwrare Corporation                   }
{                                                                         }
{ *********************************************************************** }

unit SysUtils;

{$H+}
{$T-}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
{$IFDEF LINUX}
Types,
Libc,
{$ENDIF}
SysConst;

const
{ File open modes }

{$IFDEF LINUX}
  fmOpenRead       = O_RDONLY;
  fmOpenWrite      = O_WRONLY;
  fmOpenReadWrite  = O_RDWR;
//  fmShareCompat not supported
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
//  fmShareDenyRead  not supported
  fmShareDenyNone  = $0030;
{$ENDIF}
{$IFDEF MSWINDOWS}
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;

  fmShareCompat    = $0000 platform; // DOS compatibility mode is not portable
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030 platform; // write-only not supported on all platforms
  fmShareDenyNone  = $0040;
{$ENDIF}

{ File attribute constants }

  faReadOnly  = $00000001 platform;
  faHidden    = $00000002 platform;
  faSysFile   = $00000004 platform;
  faVolumeID  = $00000008 platform;
  faDirectory = $00000010;
  faArchive   = $00000020 platform;
  faSymLink   = $00000040 platform;
  faAnyFile   = $0000003F;

{ Units of time }

  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;

{ Days between 1/1/0001 and 12/31/1899 }

  DateDelta = 693594;

{ Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970) }

  UnixDateDelta = 25569;

type

{ Standard Character set type }

  TSysCharSet = set of Char;

{ Set access to an integer }

  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

{ Type conversion records }

  WordRec = packed record
    case Integer of
      0: (Lo, Hi: Byte);
      1: (Bytes: array [0..1] of Byte);
  end;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  Int64Rec = packed record
    case Integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;

{ General arrays }

  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

{ Generic procedure pointer }

  TProcedure = procedure;

{ Generic filename type }

  TFileName = type string;

{ Search record used by FindFirst, FindNext, and FindClose }

  TSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: TFileName;
    ExcludeAttr: Integer;
{$IFDEF MSWINDOWS}
    FindHandle: THandle  platform;
    FindData: TWin32FindData  platform;
{$ENDIF}
{$IFDEF LINUX}
    Mode: mode_t  platform;
    FindHandle: Pointer  platform;
    PathOnly: String  platform;
    Pattern: String  platform;
{$ENDIF}
  end;

{ FloatToText, FloatToTextFmt, TextToFloat, and FloatToDecimal type codes }

  TFloatValue = (fvExtended, fvCurrency);

{ FloatToText format codes }

  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);

{ FloatToDecimal result record }

  TFloatRec = packed record
    Exponent: Smallint;
    Negative: Boolean;
    Digits: array[0..20] of Char;
  end;

{ Date and time record }

  TTimeStamp = record
    Time: Integer;      { Number of milliseconds since midnight }
    Date: Integer;      { One plus number of days since 1/1/0001 }
  end;

{ MultiByte Character Set (MBCS) byte type }
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

{ System Locale information record }
  TSysLocale = packed record
    DefaultLCID: Integer;
    PriLangID: Integer;
    SubLangID: Integer;
    FarEast: Boolean;
    MiddleEast: Boolean;
  end;

{$IFDEF MSWINDOWS}
{ This is used by TLanguages }
  TLangRec = packed record
    FName: string;
    FLCID: LCID;
    FExt: string;
  end;

{ This stores the languages that the system supports }
  TLanguages = class
  private
    FSysLangs: array of TLangRec;
    function LocalesCallback(LocaleID: PChar): Integer; stdcall;
    function GetExt(Index: Integer): string;
    function GetID(Index: Integer): string;
    function GetLCID(Index: Integer): LCID;
    function GetName(Index: Integer): string;
    function GetNameFromLocaleID(ID: LCID): string;
    function GetNameFromLCID(const ID: string): string;
    function GetCount: integer;
  public
    constructor Create;
    function IndexOf(ID: LCID): Integer;
    property Count: Integer read GetCount;
    property Name[Index: Integer]: string read GetName;
    property NameFromLocaleID[ID: LCID]: string read GetNameFromLocaleID;
    property NameFromLCID[const ID: string]: string read GetNameFromLCID;
    property ID[Index: Integer]: string read GetID;
    property LocaleID[Index: Integer]: LCID read GetLCID;
    property Ext[Index: Integer]: string read GetExt;
  end platform;
{$ENDIF}

{$IFDEF LINUX}
  TEraRange = record
    StartDate : Integer;         // whole days since 12/31/1899 (TDateTime basis)
    EndDate   : Integer;         // whole days since 12/31/1899 (TDateTime basis)
//    Direction : Char;
  end;
{$ENDIF}

{ Exceptions }

  Exception = class(TObject)
  private
    FMessage: string;
    FHelpContext: Integer;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: Integer; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
    constructor CreateHelp(const Msg: string; AHelpContext: Integer);
    constructor CreateFmtHelp(const Msg: string; const Args: array of const;
      AHelpContext: Integer);
    constructor CreateResHelp(Ident: Integer; AHelpContext: Integer); overload;
    constructor CreateResHelp(ResStringRec: PResStringRec; AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(ResStringRec: PResStringRec; const Args: array of const;
      AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(Ident: Integer; const Args: array of const;
      AHelpContext: Integer); overload;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property Message: string read FMessage write FMessage;
  end;

  ExceptClass = class of Exception;

  EAbort = class(Exception);

  EHeapException = class(Exception)
  private
    AllowFree: Boolean;
  public
    procedure FreeInstance; override;
  end;

  EOutOfMemory = class(EHeapException);

  EInOutError = class(Exception)
  public
    ErrorCode: Integer;
  end;

{$IFDEF MSWINDOWS}
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    ExceptionInformation: array[0..14] of Cardinal;
  end;
{$ENDIF}

  EExternal = class(Exception)
  public
{$IFDEF MSWINDOWS}
    ExceptionRecord: PExceptionRecord platform;
{$ENDIF}
{$IFDEF LINUX}
    ExceptionAddress: LongWord platform;
    AccessAddress: LongWord platform;
    SignalNumber: Integer platform;
{$ENDIF}
  end;

  EExternalException = class(EExternal);

  EIntError = class(EExternal);
  EDivByZero = class(EIntError);
  ERangeError = class(EIntError);
  EIntOverflow = class(EIntError);

  EMathError = class(EExternal);
  EInvalidOp = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow = class(EMathError);
  EUnderflow = class(EMathError);

  EInvalidPointer = class(EHeapException);

  EInvalidCast = class(Exception);

  EConvertError = class(Exception);

  EAccessViolation = class(EExternal);
  EPrivilege = class(EExternal);
  EStackOverflow = class(EExternal);
	EControlC = class(EExternal);
{$IFDEF LINUX}
  EQuit = class(EExternal) end platform;
{$ENDIF}

{$IFDEF LINUX}
  ECodesetConversion = class(Exception) end platform;
{$ENDIF}

  EVariantError = class(Exception);

  EPropReadOnly = class(Exception);
  EPropWriteOnly = class(Exception);

  EAssertionFailed = class(Exception);

{$IFNDEF PC_MAPPED_EXCEPTIONS}
  EAbstractError = class(Exception) end platform;
{$ENDIF}

  EIntfCastError = class(Exception);

  EInvalidContainer = class(Exception);
  EInvalidInsert = class(Exception);

  EPackageError = class(Exception);

  EOSError = class(Exception)
  public
    ErrorCode: DWORD;
  end;
{$IFDEF MSWINDOWS}
  EWin32Error = class(EOSError)
  end deprecated;
{$ENDIF}

  ESafecallException = class(Exception);

{$IFDEF LINUX}

{
        Signals

    External exceptions, or signals, are, by default, converted to language
    exceptions by the Delphi RTL.  Under Linux, a Delphi application installs
    signal handlers to trap the raw signals, and convert them.  Delphi libraries
    do not install handlers by default.  So if you are implementing a standalone
    library, such as an Apache DSO, and you want to have signals converted to
    language exceptions that you can catch, you must install signal hooks
    manually, using the interfaces that the Delphi RTL provides.

    For most libraries, installing signal handlers is pretty
    straightforward.  Call HookSignal(RTL_SIGDEFAULT) at initialization time,
    and UnhookSignal(RTL_SIGNALDEFAULT) at shutdown.  This will install handlers
    for a set of signals that the RTL normally hooks for Delphi applications.

    There are some cases where the above initialization will not work properly:
    The proper behaviour for setting up signal handlers is to set
    a signal handler, and then later restore the signal handler to its previous
    state when you clean up.  If you have two libraries lib1 and lib2, and lib1
    installs a signal handler, and then lib2 installs a signal handler, those
    libraries have to uninstall in the proper order if they restore signal
    handlers, or the signal handlers can be left in an inconsistent and
    potentially fatal state.  Not all libraries behave well with respect to
    installing signal handlers.  To hedge against this possibility, and allow
    you to manage signal handlers better in the face of whatever behaviour
    you may find in external libraries, we provide a set of four interfaces to
    allow you to tailor the Delphi signal handler hooking/unhooking in the
    event of an emergency.  These are:
        InquireSignal
        AbandonSignalHandler
        HookSignal
        UnhookSignal

    InquireSignal allows you to look at the state of a signal handler, so
    that you can find out if someone grabbed it out from under you.

    AbandonSignalHandler tells the RTL never to unhook a particular
    signal handler.  This can be used if you find a case where it would
    be unsafe to return to the previous state of signal handling.  For
    example, if the previous signal handler was installed by a library
    which has since been unloaded.

    HookSignal/UnhookSignal setup signal handlers that map certain signals
    into language exceptions.

    See additional notes at InquireSignal, et al, below.
}

const
    RTL_SIGINT          = 0;    // User interrupt (SIGINT)
    RTL_SIGFPE          = 1;    // Floating point exception (SIGFPE)
    RTL_SIGSEGV         = 2;    // Segmentation violation (SIGSEGV)
    RTL_SIGILL          = 3;    // Illegal instruction (SIGILL)
    RTL_SIGBUS          = 4;    // Bus error (SIGBUS)
    RTL_SIGQUIT         = 5;    // User interrupt (SIGQUIT)
    RTL_SIGLAST         = RTL_SIGQUIT; // Used internally.  Don't use this.
    RTL_SIGDEFAULT      = -1;   // Means all of a set of signals that the we capture
                                // normally.  This is currently all of the preceding
                                // signals.  You cannot pass this to InquireSignal.

type
    { TSignalState is the state of a given signal handler, as returned by
      InquireSignal.  See InquireSignal, below.
    }
    TSignalState = (ssNotHooked, ssHooked, ssOverridden);

var

  {
    If DeferUserInterrupts is set, we do not raise either SIGINT or SIGQUIT as
    an exception, instead, we set SIGINTIssued or SIGQUITIssued when the
    signal arrives, and swallow the signal where the OS issued it.  This gives
    GUI applications the chance to defer the actual handling of the signal
    until a time when it is safe to do so.
  }

  DeferUserInterrupts: Boolean;
  SIGINTIssued: Boolean;
  SIGQUITIssued: Boolean;
{$ENDIF}

{$IFDEF LINUX}
const
  MAX_PATH = 4095;  // From /usr/include/linux/limits.h PATH_MAX
{$ENDIF}

var

{ Empty string and null string pointer. These constants are provided for
  backwards compatibility only.  }

  EmptyStr: string = '';
  NullStr: PString = @EmptyStr;

  EmptyWideStr: WideString = '';
  NullWideStr: PWideString = @EmptyWideStr;

{$IFDEF MSWINDOWS}
{ Win32 platform identifier.  This will be one of the following values:

    VER_PLATFORM_WIN32s
    VER_PLATFORM_WIN32_WINDOWS
    VER_PLATFORM_WIN32_NT

  See WINDOWS.PAS for the numerical values. }

  Win32Platform: Integer = 0;

{ Win32 OS version information -

  see TOSVersionInfo.dwMajorVersion/dwMinorVersion/dwBuildNumber }

  Win32MajorVersion: Integer = 0;
  Win32MinorVersion: Integer = 0;
  Win32BuildNumber: Integer = 0;

{ Win32 OS extra version info string -

  see TOSVersionInfo.szCSDVersion }

  Win32CSDVersion: string = '';

{ Win32 OS version tester }

function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;

{ GetFileVersion returns the most significant 32 bits of a file's binary
  version number. Typically, this includes the major and minor version placed
  together in one 32-bit integer. It generally does not include the release
  or build numbers. It returns Cardinal(-1) if it failed. }
function GetFileVersion(const AFileName: string): Cardinal;

{$ENDIF}

{ Currency and date/time formatting options

  The initial values of these variables are fetched from the system registry
  using the GetLocaleInfo function in the Win32 API. The description of each
  variable specifies the LOCALE_XXXX constant used to fetch the initial
  value.

  CurrencyString - Defines the currency symbol used in floating-point to
  decimal conversions. The initial value is fetched from LOCALE_SCURRENCY.

  CurrencyFormat - Defines the currency symbol placement and separation
  used in floating-point to decimal conversions. Possible values are:

    0 = '$1'
    1 = '1$'
    2 = '$ 1'
    3 = '1 $'

  The initial value is fetched from LOCALE_ICURRENCY.

  NegCurrFormat - Defines the currency format for used in floating-point to
  decimal conversions of negative numbers. Possible values are:

    0 = '($1)'      4 = '(1$)'      8 = '-1 $'      12 = '$ -1'
    1 = '-$1'       5 = '-1$'       9 = '-$ 1'      13 = '1- $'
    2 = '$-1'       6 = '1-$'      10 = '1 $-'      14 = '($ 1)'
    3 = '$1-'       7 = '1$-'      11 = '$ 1-'      15 = '(1 $)'

  The initial value is fetched from LOCALE_INEGCURR.

  ThousandSeparator - The character used to separate thousands in numbers
  with more than three digits to the left of the decimal separator. The
  initial value is fetched from LOCALE_STHOUSAND.  A value of #0 indicates
  no thousand separator character should be output even if the format string
  specifies thousand separators.

  DecimalSeparator - The character used to separate the integer part from
  the fractional part of a number. The initial value is fetched from
  LOCALE_SDECIMAL.  DecimalSeparator must be a non-zero value.

  CurrencyDecimals - The number of digits to the right of the decimal point
  in a currency amount. The initial value is fetched from LOCALE_ICURRDIGITS.

  DateSeparator - The character used to separate the year, month, and day
  parts of a date value. The initial value is fetched from LOCATE_SDATE.

  ShortDateFormat - The format string used to convert a date value to a
  short string suitable for editing. For a complete description of date and
  time format strings, refer to the documentation for the FormatDate
  function. The short date format should only use the date separator
  character and the  m, mm, d, dd, yy, and yyyy format specifiers. The
  initial value is fetched from LOCALE_SSHORTDATE.

  LongDateFormat - The format string used to convert a date value to a long
  string suitable for display but not for editing. For a complete description
  of date and time format strings, refer to the documentation for the
  FormatDate function. The initial value is fetched from LOCALE_SLONGDATE.

  TimeSeparator - The character used to separate the hour, minute, and
  second parts of a time value. The initial value is fetched from
  LOCALE_STIME.

  TimeAMString - The suffix string used for time values between 00:00 and
  11:59 in 12-hour clock format. The initial value is fetched from
  LOCALE_S1159.

  TimePMString - The suffix string used for time values between 12:00 and
  23:59 in 12-hour clock format. The initial value is fetched from
  LOCALE_S2359.

  ShortTimeFormat - The format string used to convert a time value to a
  short string with only hours and minutes. The default value is computed
  from LOCALE_ITIME and LOCALE_ITLZERO.

  LongTimeFormat - The format string used to convert a time value to a long
  string with hours, minutes, and seconds. The default value is computed
  from LOCALE_ITIME and LOCALE_ITLZERO.

  ShortMonthNames - Array of strings containing short month names. The mmm
  format specifier in a format string passed to FormatDate causes a short
  month name to be substituted. The default values are fecthed from the
  LOCALE_SABBREVMONTHNAME system locale entries.

  LongMonthNames - Array of strings containing long month names. The mmmm
  format specifier in a format string passed to FormatDate causes a long
  month name to be substituted. The default values are fecthed from the
  LOCALE_SMONTHNAME system locale entries.

  ShortDayNames - Array of strings containing short day names. The ddd
  format specifier in a format string passed to FormatDate causes a short
  day name to be substituted. The default values are fecthed from the
  LOCALE_SABBREVDAYNAME system locale entries.

  LongDayNames - Array of strings containing long day names. The dddd
  format specifier in a format string passed to FormatDate causes a long
  day name to be substituted. The default values are fecthed from the
  LOCALE_SDAYNAME system locale entries.

  ListSeparator - The character used to separate items in a list.  The
  initial value is fetched from LOCALE_SLIST.

  TwoDigitYearCenturyWindow - Determines what century is added to two
  digit years when converting string dates to numeric dates.  This value
  is subtracted from the current year before extracting the century.
  This can be used to extend the lifetime of existing applications that
  are inextricably tied to 2 digit year data entry.  The best solution
  to Year 2000 (Y2k) issues is not to accept 2 digit years at all - require
  4 digit years in data entry to eliminate century ambiguities.

  Examples:

  Current TwoDigitCenturyWindow  Century  StrToDate() of:
  Year    Value                  Pivot    '01/01/03' '01/01/68' '01/01/50'
  -------------------------------------------------------------------------
  1998    0                      1900     1903       1968       1950
  2002    0                      2000     2003       2068       2050
  1998    50 (default)           1948     2003       1968       1950
  2002    50 (default)           1952     2003       1968       2050
  2020    50 (default)           1970     2003       2068       2050
 }

var
  CurrencyString: string;
  CurrencyFormat: Byte;
  NegCurrFormat: Byte;
  ThousandSeparator: Char;
  DecimalSeparator: Char;
  CurrencyDecimals: Byte;
  DateSeparator: Char;
  ShortDateFormat: string;
  LongDateFormat: string;
  TimeSeparator: Char;
  TimeAMString: string;
  TimePMString: string;
  ShortTimeFormat: string;
  LongTimeFormat: string;
  ShortMonthNames: array[1..12] of string;
  LongMonthNames: array[1..12] of string;
  ShortDayNames: array[1..7] of string;
  LongDayNames: array[1..7] of string;
  SysLocale: TSysLocale;
  TwoDigitYearCenturyWindow: Word = 50;
  ListSeparator: Char;


{ Thread safe currency and date/time formatting

  The TFormatSettings record is designed to allow thread safe formatting,
  equivalent to the gloabal variables described above. Each of the
  formatting routines that use the gloabal variables have overloaded
  equivalents, requiring an additional parameter of type TFormatSettings.

  A TFormatSettings record must be populated before use. This can be done
  using the GetLocaleFormatSettings function, which will populate the
  record with values based on the given locale (using the Win32 API
  function GetLocaleInfo). Note that some format specifiers still require
  specific thread locale settings (such as period/era names).
}

type
  TFormatSettings = record
    CurrencyFormat: Byte;
    NegCurrFormat: Byte;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    CurrencyDecimals: Byte;
    DateSeparator: Char;
    TimeSeparator: Char;
    ListSeparator: Char;
    CurrencyString: string;
    ShortDateFormat: string;
    LongDateFormat: string;
    TimeAMString: string;
    TimePMString: string;
    ShortTimeFormat: string;
    LongTimeFormat: string;
    ShortMonthNames: array[1..12] of string;
    LongMonthNames: array[1..12] of string;
    ShortDayNames: array[1..7] of string;
    LongDayNames: array[1..7] of string;
    TwoDigitYearCenturyWindow: Word;
  end;

const
  MaxEraCount = 7;

var
  EraNames: array [1..MaxEraCount] of string;
  EraYearOffsets: array [1..MaxEraCount] of Integer;
{$IFDEF LINUX}
  EraRanges : array [1..MaxEraCount] of TEraRange platform;
  EraYearFormats: array [1..MaxEraCount] of string platform;
  EraCount: Byte platform;
{$ENDIF}

const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
  DriveDelim = {$IFDEF MSWINDOWS} ':'; {$ELSE} '';  {$ENDIF}
  PathSep    = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF}

{$IFDEF MSWINDOWS}
function Languages: TLanguages;
{$ENDIF}

{ Memory management routines }

{ AllocMem allocates a block of the given size on the heap. Each byte in
  the allocated buffer is set to zero. To dispose the buffer, use the
  FreeMem standard procedure. }

function AllocMem(Size: Cardinal): Pointer;

{ Exit procedure handling }

{ AddExitProc adds the given procedure to the run-time library's exit
  procedure list. When an application terminates, its exit procedures are
  executed in reverse order of definition, i.e. the last procedure passed
  to AddExitProc is the first one to get executed upon termination. }

procedure AddExitProc(Proc: TProcedure);

{ String handling routines }

{ NewStr allocates a string on the heap. NewStr is provided for backwards
  compatibility only. }

function NewStr(const S: string): PString; deprecated;

{ DisposeStr disposes a string pointer that was previously allocated using
  NewStr. DisposeStr is provided for backwards compatibility only. }

procedure DisposeStr(P: PString); deprecated;

{ AssignStr assigns a new dynamically allocated string to the given string
  pointer. AssignStr is provided for backwards compatibility only. }

procedure AssignStr(var P: PString; const S: string); deprecated;

{ AppendStr appends S to the end of Dest. AppendStr is provided for
  backwards compatibility only. Use "Dest := Dest + S" instead. }

procedure AppendStr(var Dest: string; const S: string); deprecated;

{ UpperCase converts all ASCII characters in the given string to upper case.
  The conversion affects only 7-bit ASCII characters between 'a' and 'z'. To
  convert 8-bit international characters, use AnsiUpperCase. }

function UpperCase(const S: string): string;

{ LowerCase converts all ASCII characters in the given string to lower case.
  The conversion affects only 7-bit ASCII characters between 'A' and 'Z'. To
  convert 8-bit international characters, use AnsiLowerCase. }

function LowerCase(const S: string): string;

{ CompareStr compares S1 to S2, with case-sensitivity. The return value is
  less than 0 if S1 < S2, 0 if S1 = S2, or greater than 0 if S1 > S2. The
  compare operation is based on the 8-bit ordinal value of each character
  and is not affected by the current user locale. }

function CompareStr(const S1, S2: string): Integer;

{ CompareMem performs a binary compare of Length bytes of memory referenced
  by P1 to that of P2.  CompareMem returns True if the memory referenced by
  P1 is identical to that of P2. }

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;

{ CompareText compares S1 to S2, without case-sensitivity. The return value
  is the same as for CompareStr. The compare operation is based on the 8-bit
  ordinal value of each character, after converting 'a'..'z' to 'A'..'Z',
  and is not affected by the current user locale. }

function CompareText(const S1, S2: string): Integer;

{ SameText compares S1 to S2, without case-sensitivity. Returns true if
  S1 and S2 are the equal, that is, if CompareText would return 0. SameText
  has the same 8-bit limitations as CompareText }

function SameText(const S1, S2: string): Boolean;

{ AnsiUpperCase converts all characters in the given string to upper case.
  The conversion uses the current user locale. }

function AnsiUpperCase(const S: string): string;

{ AnsiLowerCase converts all characters in the given string to lower case.
  The conversion uses the current user locale. }

function AnsiLowerCase(const S: string): string;

{ AnsiCompareStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiCompareStr(const S1, S2: string): Integer;

{ AnsiSameStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is True if AnsiCompareStr would have returned 0. }

function AnsiSameStr(const S1, S2: string): Boolean;

{ AnsiCompareText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiCompareText(const S1, S2: string): Integer;

{ AnsiSameText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is True if AnsiCompareText would have returned 0. }

function AnsiSameText(const S1, S2: string): Boolean;

{ AnsiStrComp compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiStrComp(S1, S2: PChar): Integer;

{ AnsiStrIComp compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiStrIComp(S1, S2: PChar): Integer;

{ AnsiStrLComp compares S1 to S2, with case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current user locale. The return value is the same as for CompareStr. }

function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer;

{ AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current user locale. The return value is the same as for CompareStr. }

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;

{ AnsiStrLower converts all characters in the given string to lower case.
  The conversion uses the current user locale. }

function AnsiStrLower(Str: PChar): PChar;

{ AnsiStrUpper converts all characters in the given string to upper case.
  The conversion uses the current user locale. }

function AnsiStrUpper(Str: PChar): PChar;

{ AnsiLastChar returns a pointer to the last full character in the string.
  This function supports multibyte characters  }

function AnsiLastChar(const S: string): PChar;

{ AnsiStrLastChar returns a pointer to the last full character in the string.
  This function supports multibyte characters.  }

function AnsiStrLastChar(P: PChar): PChar;

{ WideUpperCase converts all characters in the given string to upper case. }

function WideUpperCase(const S: WideString): WideString;

{ WideLowerCase converts all characters in the given string to lower case. }

function WideLowerCase(const S: WideString): WideString;

{ WideCompareStr compares S1 to S2, with case-sensitivity. The return value
  is the same as for CompareStr. }

function WideCompareStr(const S1, S2: WideString): Integer;

{ WideSameStr compares S1 to S2, with case-sensitivity. The return value
  is True if WideCompareStr would have returned 0. }

function WideSameStr(const S1, S2: WideString): Boolean;

{ WideCompareText compares S1 to S2, without case-sensitivity. The return value
  is the same as for CompareStr. }

function WideCompareText(const S1, S2: WideString): Integer;

{ WideSameText compares S1 to S2, without case-sensitivity. The return value
  is True if WideCompareText would have returned 0. }

function WideSameText(const S1, S2: WideString): Boolean;

{ Trim trims leading and trailing spaces and control characters from the
  given string. }

function Trim(const S: string): string; overload;
function Trim(const S: WideString): WideString; overload;

{ TrimLeft trims leading spaces and control characters from the given
  string. }

function TrimLeft(const S: string): string; overload;
function TrimLeft(const S: WideString): WideString; overload;

{ TrimRight trims trailing spaces and control characters from the given
  string. }

function TrimRight(const S: string): string; overload;
function TrimRight(const S: WideString): WideString; overload;

{ QuotedStr returns the given string as a quoted string. A single quote
  character is inserted at the beginning and the end of the string, and
  for each single quote character in the string, another one is added. }

function QuotedStr(const S: string): string;

{ AnsiQuotedStr returns the given string as a quoted string, using the
  provided Quote character.  A Quote character is inserted at the beginning
  and end of the string, and each Quote character in the string is doubled.
  This function supports multibyte character strings (MBCS). }

function AnsiQuotedStr(const S: string; Quote: Char): string;

{ AnsiExtractQuotedStr removes the Quote characters from the beginning and end
  of a quoted string, and reduces pairs of Quote characters within the quoted
  string to a single character. If the first character in Src is not the Quote
  character, the function returns an empty string.  The function copies
  characters from the Src to the result string until the second solitary
  Quote character or the first null character in Src. The Src parameter is
  updated to point to the first character following the quoted string.  If
  the Src string does not contain a matching end Quote character, the Src
  parameter is updated to point to the terminating null character in Src.
  This function supports multibyte character strings (MBCS).  }

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;

{ AnsiDequotedStr is a simplified version of AnsiExtractQuotedStr }

function AnsiDequotedStr(const S: string; AQuote: Char): string;

{ AdjustLineBreaks adjusts all line breaks in the given string to the
  indicated style.
  When Style is tlbsCRLF, the function changes all
  CR characters not followed by LF and all LF characters not preceded
  by a CR into CR/LF pairs.
  When Style is tlbsLF, the function changes all CR/LF pairs and CR characters
  not followed by LF to LF characters. }

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle =
        {$IFDEF LINUX} tlbsLF {$ENDIF}
        {$IFDEF MSWINDOWS} tlbsCRLF {$ENDIF}): string;

{ IsValidIdent returns true if the given string is a valid identifier. An
  identifier is defined as a character from the set ['A'..'Z', 'a'..'z', '_']
  followed by zero or more characters from the set ['A'..'Z', 'a'..'z',
  '0..'9', '_']. }

function IsValidIdent(const Ident: string): Boolean;

{ IntToStr converts the given value to its decimal string representation. }

function IntToStr(Value: Integer): string; overload;
function IntToStr(Value: Int64): string; overload;

{ IntToHex converts the given value to a hexadecimal string representation
  with the minimum number of digits specified. }

function IntToHex(Value: Integer; Digits: Integer): string; overload;
function IntToHex(Value: Int64; Digits: Integer): string; overload;

{ StrToInt converts the given string to an integer value. If the string
  doesn't contain a valid value, an EConvertError exception is raised. }

function StrToInt(const S: string): Integer;
function StrToIntDef(const S: string; Default: Integer): Integer;
function TryStrToInt(const S: string; out Value: Integer): Boolean;

{ Similar to the above functions but for Int64 instead }

function StrToInt64(const S: string): Int64;
function StrToInt64Def(const S: string; const Default: Int64): Int64;
function TryStrToInt64(const S: string; out Value: Int64): Boolean;

{ StrToBool converts the given string to a boolean value.  If the string
  doesn't contain a valid value, an EConvertError exception is raised.
  BoolToStr converts boolean to a string value that in turn can be converted
  back into a boolean.  BoolToStr will always pick the first element of
  the TrueStrs/FalseStrs arrays. }

var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;

const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

function StrToBool(const S: string): Boolean;
function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
function TryStrToBool(const S: string; out Value: Boolean): Boolean;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;

{ LoadStr loads the string resource given by Ident from the application's
  executable file or associated resource module. If the string resource
  does not exist, LoadStr returns an empty string. }

function LoadStr(Ident: Integer): string;

{ FmtLoadStr loads the string resource given by Ident from the application's
  executable file or associated resource module, and uses it as the format
  string in a call to the Format function with the given arguments. }

function FmtLoadStr(Ident: Integer; const Args: array of const): string;

{ File management routines }

{ FileOpen opens the specified file using the specified access mode. The
  access mode value is constructed by OR-ing one of the fmOpenXXXX constants
  with one of the fmShareXXXX constants. If the return value is positive,
  the function was successful and the value is the file handle of the opened
  file. A return value of -1 indicates that an error occurred. }

function FileOpen(const FileName: string; Mode: LongWord): Integer;

{ FileCreate creates a new file by the specified name. If the return value
  is positive, the function was successful and the value is the file handle
  of the new file. A return value of -1 indicates that an error occurred.
  On Linux, this calls FileCreate(FileName, DEFFILEMODE) to create
  the file with read and write access for the current user only.  }

function FileCreate(const FileName: string): Integer; overload;

{ This second version of FileCreate lets you specify the access rights to put on the newly
  created file.  The access rights parameter is ignored on Win32 }

function FileCreate(const FileName: string; Rights: Integer): Integer; overload;

{ FileRead reads Count bytes from the file given by Handle into the buffer
  specified by Buffer. The return value is the number of bytes actually
  read; it is less than Count if the end of the file was reached. The return
  value is -1 if an error occurred. }

function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;

{ FileWrite writes Count bytes to the file given by Handle from the buffer
  specified by Buffer. The return value is the number of bytes actually
  written, or -1 if an error occurred. }

function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;

{ FileSeek changes the current position of the file given by Handle to be
  Offset bytes relative to the point given by Origin. Origin = 0 means that
  Offset is relative to the beginning of the file, Origin = 1 means that
  Offset is relative to the current position, and Origin = 2 means that
  Offset is relative to the end of the file. The return value is the new
  current position, relative to the beginning of the file, or -1 if an error
  occurred. }

function FileSeek(Handle, Offset, Origin: Integer): Integer; overload;
function FileSeek(Handle: Integer; const Offset: Int64; Origin: Integer): Int64; overload;

{ FileClose closes the specified file. }

procedure FileClose(Handle: Integer);

{ FileAge returns the date-and-time stamp of the specified file. The return
  value can be converted to a TDateTime value using the FileDateToDateTime
  function. The return value is -1 if the file does not exist. }

function FileAge(const FileName: string): Integer;

{ FileExists returns a boolean value that indicates whether the specified
  file exists. }

function FileExists(const FileName: string): Boolean;

{ DirectoryExists returns a boolean value that indicates whether the
  specified directory exists (and is actually a directory) }

function DirectoryExists(const Directory: string): Boolean;

{ ForceDirectories ensures that all the directories in a specific path exist.
  Any portion that does not already exist will be created.  Function result
  indicates success of the operation.  The function can fail if the current
  user does not have sufficient file access rights to create directories in
  the given path.  }

function ForceDirectories(Dir: string): Boolean;

{ FindFirst searches the directory given by Path for the first entry that
  matches the filename given by Path and the attributes given by Attr. The
  result is returned in the search record given by SearchRec. The return
  value is zero if the function was successful. Otherwise the return value
  is a system error code. After calling FindFirst, always call FindClose.
  FindFirst is typically used with FindNext and FindClose as follows:

    Result := FindFirst(Path, Attr, SearchRec);
    while Result = 0 do
    begin
      ProcessSearchRec(SearchRec);
      Result := FindNext(SearchRec);
    end;
    FindClose(SearchRec);

  where ProcessSearchRec represents user-defined code that processes the
  information in a search record. }

function FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;

{ FindNext returs the next entry that matches the name and attributes
  specified in a previous call to FindFirst. The search record must be one
  that was passed to FindFirst. The return value is zero if the function was
  successful. Otherwise the return value is a system error code. }

function FindNext(var F: TSearchRec): Integer;

{ FindClose terminates a FindFirst/FindNext sequence and frees memory and system
  resources allocated by FindFirst.
  Every FindFirst/FindNext must end with a call to FindClose. }

procedure FindClose(var F: TSearchRec);

{ FileGetDate returns the OS date-and-time stamp of the file given by
  Handle. The return value is -1 if the handle is invalid. The
  FileDateToDateTime function can be used to convert the returned value to
  a TDateTime value. }

function FileGetDate(Handle: Integer): Integer;

{ FileSetDate sets the OS date-and-time stamp of the file given by FileName
  to the value given by Age. The DateTimeToFileDate function can be used to
  convert a TDateTime value to an OS date-and-time stamp. The return value
  is zero if the function was successful. Otherwise the return value is a
  system error code.        }

function FileSetDate(const FileName: string; Age: Integer): Integer; overload;

{$IFDEF MSWINDOWS}
{  FileSetDate by handle is not available on Unix platforms because there
  is no standard way to set a file's modification time using only a file
  handle, and no standard way to obtain the file name of an open
  file handle.  }

function FileSetDate(Handle: Integer; Age: Integer): Integer; overload; platform;

{ FileGetAttr returns the file attributes of the file given by FileName. The
  attributes can be examined by AND-ing with the faXXXX constants defined
  above. A return value of -1 indicates that an error occurred. }

function FileGetAttr(const FileName: string): Integer; platform;

{ FileSetAttr sets the file attributes of the file given by FileName to the
  value given by Attr. The attribute value is formed by OR-ing the
  appropriate faXXXX constants. The return value is zero if the function was
  successful. Otherwise the return value is a system error code. }

function FileSetAttr(const FileName: string; Attr: Integer): Integer; platform;
{$ENDIF}

{ FileIsReadOnly tests whether a given file is read-only for the current
  process and effective user id.  If the file does not exist, the
  function returns False.  (Check FileExists before calling FileIsReadOnly)
  This function is platform portable. }

function FileIsReadOnly(const FileName: string): Boolean;

{ FileSetReadOnly sets the read only state of a file.  The file must
  exist and the current effective user id must be the owner of the file.
  On Unix systems, FileSetReadOnly attempts to set or remove
  all three (user, group, and other) write permissions on the file.
  If you want to grant partial permissions (writeable for owner but not
  for others), use platform specific functions such as chmod.
  The function returns True if the file was successfully modified,
  False if there was an error.  This function is platform portable.  }

function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;

{ DeleteFile deletes the file given by FileName. The return value is True if
  the file was successfully deleted, or False if an error occurred. }

function DeleteFile(const FileName: string): Boolean;

{ RenameFile renames the file given by OldName to the name given by NewName.
  The return value is True if the file was successfully renamed, or False if
  an error occurred. }

function RenameFile(const OldName, NewName: string): Boolean;

{ ChangeFileExt changes the extension of a filename. FileName specifies a
  filename with or without an extension, and Extension specifies the new
  extension for the filename. The new extension can be a an empty string or
  a period followed by up to three characters. }

function ChangeFileExt(const FileName, Extension: string): string;

{ ExtractFilePath extracts the drive and directory parts of the given
  filename. The resulting string is the leftmost characters of FileName,
  up to and including the colon or backslash that separates the path
  information from the name and extension. The resulting string is empty
  if FileName contains no drive and directory parts. }

function ExtractFilePath(const FileName: string): string;

{ ExtractFileDir extracts the drive and directory parts of the given
  filename. The resulting string is a directory name suitable for passing
  to SetCurrentDir, CreateDir, etc. The resulting string is empty if
  FileName contains no drive and directory parts. }

function ExtractFileDir(const FileName: string): string;

{ ExtractFileDrive extracts the drive part of the given filename.  For
  filenames with drive letters, the resulting string is '<drive>:'.
  For filenames with a UNC path, the resulting string is in the form
  '\\<servername>\<sharename>'.  If the given path contains neither
  style of filename, the result is an empty string. }

function ExtractFileDrive(const FileName: string): string;

{ ExtractFileName extracts the name and extension parts of the given
  filename. The resulting string is the leftmost characters of FileName,
  starting with the first character after the colon or backslash that
  separates the path information from the name and extension. The resulting
  string is equal to FileName if FileName contains no drive and directory
  parts. }

function ExtractFileName(const FileName: string): string;

{ ExtractFileExt extracts the extension part of the given filename. The
  resulting string includes the period character that separates the name
  and extension parts. The resulting string is empty if the given filename
  has no extension. }

function ExtractFileExt(const FileName: string): string;

{ ExpandFileName expands the given filename to a fully qualified filename.
  The resulting string consists of a drive letter, a colon, a root relative
  directory path, and a filename. Embedded '.' and '..' directory references
  are removed. }

function ExpandFileName(const FileName: string): string;

{ ExpandFilenameCase returns a fully qualified filename like ExpandFilename,
  but performs a case-insensitive filename search looking for a close match
  in the actual file system, differing only in uppercase versus lowercase of
  the letters.  This is useful to convert lazy user input into useable file
  names, or to convert filename data created on a case-insensitive file
  system (Win32) to something useable on a case-sensitive file system (Linux).

  The MatchFound out parameter indicates what kind of match was found in the
  file system, and what the function result is based upon:

  ( in order of increasing difficulty or complexity )
  mkExactMatch:  Case-sensitive match.  Result := ExpandFileName(FileName).
  mkSingleMatch: Exactly one file in the given directory path matches the
        given filename on a case-insensitive basis.
        Result := ExpandFileName(FileName as found in file system).
  mkAmbiguous: More than one file in the given directory path matches the
        given filename case-insensitively.
        In many cases, this should be considered an error.
        Result := ExpandFileName(First matching filename found).
  mkNone:  File not found at all.  Result := ExpandFileName(FileName).

  Note that because this function has to search the file system it may be
  much slower than ExpandFileName, particularly when the given filename is
  ambiguous or does not exist.  Use ExpandFilenameCase only when you have
  a filename of dubious orgin - such as from user input - and you want
  to make a best guess before failing.  }

type
  TFilenameCaseMatch = (mkNone, mkExactMatch, mkSingleMatch, mkAmbiguous);

function ExpandFileNameCase(const FileName: string;
  out MatchFound: TFilenameCaseMatch): string;

{ ExpandUNCFileName expands the given filename to a fully qualified filename.
  This function is the same as ExpandFileName except that it will return the
  drive portion of the filename in the format '\\<servername>\<sharename> if
  that drive is actually a network resource instead of a local resource.
  Like ExpandFileName, embedded '.' and '..' directory references are
  removed. }

function ExpandUNCFileName(const FileName: string): string;

{ ExtractRelativePath will return a file path name relative to the given
  BaseName.  It strips the common path dirs and adds '..\' on Windows,
  and '../' on Linux for each level up from the BaseName path. }

function ExtractRelativePath(const BaseName, DestName: string): string;

{$IFDEF MSWINDOWS}
{ ExtractShortPathName will convert the given filename to the short form
  by calling the GetShortPathName API.  Will return an empty string if
  the file or directory specified does not exist }

function ExtractShortPathName(const FileName: string): string;
{$ENDIF}

{ FileSearch searches for the file given by Name in the list of directories
  given by DirList. The directory paths in DirList must be separated by
  PathSep chars. The search always starts with the current directory of the
  current drive. The returned value is a concatenation of one of the
  directory paths and the filename, or an empty string if the file could not
  be located. }

function FileSearch(const Name, DirList: string): string;

{$IFDEF MSWINDOWS}
{ DiskFree returns the number of free bytes on the specified drive number,
  where 0 = Current, 1 = A, 2 = B, etc. DiskFree returns -1 if the drive
  number is invalid. }

function DiskFree(Drive: Byte): Int64;

{ DiskSize returns the size in bytes of the specified drive number, where
  0 = Current, 1 = A, 2 = B, etc. DiskSize returns -1 if the drive number
  is invalid. }

function DiskSize(Drive: Byte): Int64;
{$ENDIF}

{ FileDateToDateTime converts an OS date-and-time value to a TDateTime
  value. The FileAge, FileGetDate, and FileSetDate routines operate on OS
  date-and-time values, and the Time field of a TSearchRec used by the
  FindFirst and FindNext functions contains an OS date-and-time value. }

function FileDateToDateTime(FileDate: Integer): TDateTime;

{ DateTimeToFileDate converts a TDateTime value to an OS date-and-time
  value. The FileAge, FileGetDate, and FileSetDate routines operate on OS
  date-and-time values, and the Time field of a TSearchRec used by the
  FindFirst and FindNext functions contains an OS date-and-time value. }

function DateTimeToFileDate(DateTime: TDateTime): Integer;

{ GetCurrentDir returns the current directory. }

function GetCurrentDir: string;

{ SetCurrentDir sets the current directory. The return value is True if
  the current directory was successfully changed, or False if an error
  occurred. }

function SetCurrentDir(const Dir: string): Boolean;

{ CreateDir creates a new directory. The return value is True if a new
  directory was successfully created, or False if an error occurred. }

function CreateDir(const Dir: string): Boolean;

{ RemoveDir deletes an existing empty directory. The return value is
  True if the directory was successfully deleted, or False if an error
  occurred. }

function RemoveDir(const Dir: string): Boolean;

{ PChar routines }
{ const params help simplify C++ code.  No effect on pascal code }

{ StrLen returns the number of characters in Str, not counting the null
  terminator. }

function StrLen(const Str: PChar): Cardinal;

{ StrEnd returns a pointer to the null character that terminates Str. }

function StrEnd(const Str: PChar): PChar;

{ StrMove copies exactly Count characters from Source to Dest and returns
  Dest. Source and Dest may overlap. }

function StrMove(Dest: PChar; const Source: PChar; Count: Cardinal): PChar;

{ StrCopy copies Source to Dest and returns Dest. }

function StrCopy(Dest: PChar; const Source: PChar): PChar;

{ StrECopy copies Source to Dest and returns StrEnd(Dest). }

function StrECopy(Dest:PChar; const Source: PChar): PChar;

{ StrLCopy copies at most MaxLen characters from Source to Dest and
  returns Dest. }

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

{ StrPCopy copies the Pascal style string Source into Dest and
  returns Dest. }

function StrPCopy(Dest: PChar; const Source: string): PChar;

{ StrPLCopy copies at most MaxLen characters from the Pascal style string
  Source into Dest and returns Dest. }

function StrPLCopy(Dest: PChar; const Source: string;
  MaxLen: Cardinal): PChar;

{ StrCat appends a copy of Source to the end of Dest and returns Dest. }

function StrCat(Dest: PChar; const Source: PChar): PChar;

{ StrLCat appends at most MaxLen - StrLen(Dest) characters from Source to
  the end of Dest, and returns Dest. }

function StrLCat(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

{ StrComp compares Str1 to Str2. The return value is less than 0 if
  Str1 < Str2, 0 if Str1 = Str2, or greater than 0 if Str1 > Str2. }

function StrComp(const Str1, Str2: PChar): Integer;

{ StrIComp compares Str1 to Str2, without case sensitivity. The return
  value is the same as StrComp. }

function StrIComp(const Str1, Str2: PChar): Integer;

{ StrLComp compares Str1 to Str2, for a maximum length of MaxLen
  characters. The return value is the same as StrComp. }

function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;

{ StrLIComp compares Str1 to Str2, for a maximum length of MaxLen
  characters, without case sensitivity. The return value is the same
  as StrComp. }

function StrLIComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;

{ StrScan returns a pointer to the first occurrence of Chr in Str. If Chr
  does not occur in Str, StrScan returns NIL. The null terminator is
  considered to be part of the string. }

function StrScan(const Str: PChar; Chr: Char): PChar;

{ StrRScan returns a pointer to the last occurrence of Chr in Str. If Chr
  does not occur in Str, StrRScan returns NIL. The null terminator is
  considered to be part of the string. }

function StrRScan(const Str: PChar; Chr: Char): PChar;

{ StrPos returns a pointer to the first occurrence of Str2 in Str1. If
  Str2 does not occur in Str1, StrPos returns NIL. }

function StrPos(const Str1, Str2: PChar): PChar;

{ StrUpper converts Str to upper case and returns Str. }

function StrUpper(Str: PChar): PChar;

{ StrLower converts Str to lower case and returns Str. }

function StrLower(Str: PChar): PChar;

{ StrPas converts Str to a Pascal style string. This function is provided
  for backwards compatibility only. To convert a null terminated string to
  a Pascal style string, use a string type cast or an assignment. }

function StrPas(const Str: PChar): string;

{ StrAlloc allocates a buffer of the given size on the heap. The size of
  the allocated buffer is encoded in a four byte header that immediately
  preceeds the buffer. To dispose the buffer, use StrDispose. }

function StrAlloc(Size: Cardinal): PChar;

{ StrBufSize returns the allocated size of the given buffer, not including
  the two byte header. }

function StrBufSize(const Str: PChar): Cardinal;

{ StrNew allocates a copy of Str on the heap. If Str is NIL, StrNew returns
  NIL and doesn't allocate any heap space. Otherwise, StrNew makes a
  duplicate of Str, obtaining space with a call to the StrAlloc function,
  and returns a pointer to the duplicated string. To dispose the string,
  use StrDispose. }

function StrNew(const Str: PChar): PChar;

{ StrDispose disposes a string that was previously allocated with StrAlloc
  or StrNew. If Str is NIL, StrDispose does nothing. }

procedure StrDispose(Str: PChar);

{ String formatting routines }

{ The Format routine formats the argument list given by the Args parameter
  using the format string given by the Format parameter.

  Format strings contain two types of objects--plain characters and format
  specifiers. Plain characters are copied verbatim to the resulting string.
  Format specifiers fetch arguments from the argument list and apply
  formatting to them.

  Format specifiers have the following form:

    "%" [index ":"] ["-"] [width] ["." prec] type

  A format specifier begins with a % character. After the % come the
  following, in this order:

  -  an optional argument index specifier, [index ":"]
  -  an optional left-justification indicator, ["-"]
  -  an optional width specifier, [width]
  -  an optional precision specifier, ["." prec]
  -  the conversion type character, type

  The following conversion characters are supported:

  d  Decimal. The argument must be an integer value. The value is converted
     to a string of decimal digits. If the format string contains a precision
     specifier, it indicates that the resulting string must contain at least
     the specified number of digits; if the value has less digits, the
     resulting string is left-padded with zeros.

  u  Unsigned decimal.  Similar to 'd' but no sign is output.

  e  Scientific. The argument must be a floating-point value. The value is
     converted to a string of the form "-d.ddd...E+ddd". The resulting
     string starts with a minus sign if the number is negative, and one digit
     always precedes the decimal point. The total number of digits in the
     resulting string (including the one before the decimal point) is given
     by the precision specifer in the format string--a default precision of
     15 is assumed if no precision specifer is present. The "E" exponent
     character in the resulting string is always followed by a plus or minus
     sign and at least three digits.

  f  Fixed. The argument must be a floating-point value. The value is
     converted to a string of the form "-ddd.ddd...". The resulting string
     starts with a minus sign if the number is negative. The number of digits
     after the decimal point is given by the precision specifier in the
     format string--a default of 2 decimal digits is assumed if no precision
     specifier is present.

  g  General. The argument must be a floating-point value. The value is
     converted to the shortest possible decimal string using fixed or
     scientific format. The number of significant digits in the resulting
     string is given by the precision specifier in the format string--a
     default precision of 15 is assumed if no precision specifier is present.
     Trailing zeros are removed from the resulting string, and a decimal
     point appears only if necessary. The resulting string uses fixed point
     format if the number of digits to the left of the decimal point in the
     value is less than or equal to the specified precision, and if the
     value is greater than or equal to 0.00001. Otherwise the resulting
     string uses scientific format.

  n  Number. The argument must be a floating-point value. The value is
     converted to a string of the form "-d,ddd,ddd.ddd...". The "n" format
     corresponds to the "f" format, except that the resulting string
     contains thousand separators.

  m  Money. The argument must be a floating-point value. The value is
     converted to a string that represents a currency amount. The conversion
     is controlled by the CurrencyString, CurrencyFormat, NegCurrFormat,
     ThousandSeparator, DecimalSeparator, and CurrencyDecimals global
     variables, all of which are initialized from locale settings provided
     by the operating system.  For example, Currency Format preferences can be
     set in the International section of the Windows Control Panel. If the format
     string contains a precision specifier, it overrides the value given
     by the CurrencyDecimals global variable.

  p  Pointer. The argument must be a pointer value. The value is converted
     to a string of the form "XXXX:YYYY" where XXXX and YYYY are the
     segment and offset parts of the pointer expressed as four hexadecimal
     digits.

  s  String. The argument must be a character, a string, or a PChar value.
     The string or character is inserted in place of the format specifier.
     The precision specifier, if present in the format string, specifies the
     maximum length of the resulting string. If the argument is a string
     that is longer than this maximum, the string is truncated.

  x  Hexadecimal. The argument must be an integer value. The value is
     converted to a string of hexadecimal digits. If the format string
     contains a precision specifier, it indicates that the resulting string
     must contain at least the specified number of digits; if the value has
     less digits, the resulting string is left-padded with zeros.

  Conversion characters may be specified in upper case as well as in lower
  case--both produce the same results.

  For all floating-point formats, the actual characters used as decimal and
  thousand separators are obtained from the DecimalSeparator and
  ThousandSeparator global variables.

  Index, width, and precision specifiers can be specified directly using
  decimal digit string (for example "%10d"), or indirectly using an asterisk
  charcater (for example "%*.*f"). When using an asterisk, the next argument
  in the argument list (which must be an integer value) becomes the value
  that is actually used. For example "Format('%*.*f', [8, 2, 123.456])" is
  the same as "Format('%8.2f', [123.456])".

  A width specifier sets the minimum field width for a conversion. If the
  resulting string is shorter than the minimum field width, it is padded
  with blanks to increase the field width. The default is to right-justify
  the result by adding blanks in front of the value, but if the format
  specifier contains a left-justification indicator (a "-" character
  preceding the width specifier), the result is left-justified by adding
  blanks after the value.

  An index specifier sets the current argument list index to the specified
  value. The index of the first argument in the argument list is 0. Using
  index specifiers, it is possible to format the same argument multiple
  times. For example "Format('%d %d %0:d %d', [10, 20])" produces the string
  '10 20 10 20'.

  The Format function can be combined with other formatting functions. For
  example

    S := Format('Your total was %s on %s', [
      FormatFloat('$#,##0.00;;zero', Total),
      FormatDateTime('mm/dd/yy', Date)]);

  which uses the FormatFloat and FormatDateTime functions to customize the
  format beyond what is possible with Format.

  Each of the string formatting routines that uses global variables for
  formatting (separators, decimals, date/time formats etc.), has an
  overloaded equivalent requiring a parameter of type TFormatSettings. This
  additional parameter provides the formatting information rather than the
  global variables. For more information see the notes at TFormatSettings.  }

function Format(const Format: string;
  const Args: array of const): string; overload;
function Format(const Format: string; const Args: array of const;
  const FormatSettings: TFormatSettings): string; overload;

{ FmtStr formats the argument list given by Args using the format string
  given by Format into the string variable given by Result. For further
  details, see the description of the Format function. }

procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const); overload;
procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const; const FormatSettings: TFormatSettings); overload;

{ StrFmt formats the argument list given by Args using the format string
  given by Format into the buffer given by Buffer. It is up to the caller to
  ensure that Buffer is large enough for the resulting string. The returned
  value is Buffer. For further details, see the description of the Format
  function. }

function StrFmt(Buffer, Format: PChar;
  const Args: array of const): PChar; overload;
function StrFmt(Buffer, Format: PChar; const Args: array of const;
  const FormatSettings: TFormatSettings): PChar; overload;

{ StrLFmt formats the argument list given by Args using the format string
  given by Format into the buffer given by Buffer. The resulting string will
  contain no more than MaxBufLen characters, not including the null terminator.
  The returned value is Buffer. For further details, see the description of
  the Format function. }

function StrLFmt(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar;
  const Args: array of const): PChar; overload;
function StrLFmt(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar;
  const Args: array of const;
  const FormatSettings: TFormatSettings): PChar; overload;

{ FormatBuf formats the argument list given by Args using the format string
  given by Format and FmtLen into the buffer given by Buffer and BufLen.
  The Format parameter is a reference to a buffer containing FmtLen
  characters, and the Buffer parameter is a reference to a buffer of BufLen
  characters. The returned value is the number of characters actually stored
  in Buffer. The returned value is always less than or equal to BufLen. For
  further details, see the description of the Format function. }

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal; overload;

{ The WideFormat routine formats the argument list given by the Args parameter
  using the format WideString given by the Format parameter. This routine is
  the WideString equivalent of Format. For further details, see the description
  of the Format function. }
function WideFormat(const Format: WideString;
  const Args: array of const): WideString; overload;
function WideFormat(const Format: WideString;
  const Args: array of const;
  const FormatSettings: TFormatSettings): WideString; overload;

{ WideFmtStr formats the argument list given by Args using the format WideString
  given by Format into the WideString variable given by Result. For further
  details, see the description of the Format function. }
procedure WideFmtStr(var Result: WideString; const Format: WideString;
  const Args: array of const); overload;
procedure WideFmtStr(var Result: WideString; const Format: WideString;
  const Args: array of const; const FormatSettings: TFormatSettings); overload;

{ WideFormatBuf formats the argument list given by Args using the format string
  given by Format and FmtLen into the buffer given by Buffer and BufLen.
  The Format parameter is a reference to a buffer containing FmtLen
  UNICODE characters (WideChar), and the Buffer parameter is a reference to a
  buffer of BufLen UNICODE characters (WideChar). The return value is the number
  of UNICODE characters actually stored in Buffer. The return value is always
  less than or equal to BufLen. For further details, see the description of the
  Format function.

  Important: BufLen, FmtLen and the return result are always the number of
  UNICODE characters, *not* the number of bytes. To calculate the number of bytes
  multiply them by SizeOf(WideChar). }
function WideFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
function WideFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal; overload;

{ Floating point conversion routines }

{ Each of the floating point conversion routines that uses global variables
  for formatting (separators, decimals, etc.), has an overloaded equivalent
  requiring a parameter of type TFormatSettings. This additional parameter
  provides the formatting information rather than the global variables. For
  more information see the notes at TFormatSettings.  }

{ FloatToStr converts the floating-point value given by Value to its string
  representation. The conversion uses general number format with 15
  significant digits. For further details, see the description of the
  FloatToStrF function. }

function FloatToStr(Value: Extended): string; overload;
function FloatToStr(Value: Extended;
  const FormatSettings: TFormatSettings): string; overload;

{ CurrToStr converts the currency value given by Value to its string
  representation. The conversion uses general number format. For further
  details, see the description of the CurrToStrF function. }

function CurrToStr(Value: Currency): string; overload;
function CurrToStr(Value: Currency;
  const FormatSettings: TFormatSettings): string; overload;

{ FloatToCurr will range validate a value to make sure it falls
  within the acceptable currency range }

const
  MinCurrency: Currency = -922337203685477.5807 {$IFDEF LINUX} + 1 {$ENDIF};  //!! overflow?
  MaxCurrency: Currency =  922337203685477.5807 {$IFDEF LINUX} - 1 {$ENDIF};  //!! overflow?

function FloatToCurr(const Value: Extended): Currency;
function TryFloatToCurr(const Value: Extended; out AResult: Currency): Boolean;

{ FloatToStrF converts the floating-point value given by Value to its string
  representation. The Format parameter controls the format of the resulting
  string. The Precision parameter specifies the precision of the given value.
  It should be 7 or less for values of type Single, 15 or less for values of
  type Double, and 18 or less for values of type Extended. The meaning of the
  Digits parameter depends on the particular format selected.

  The possible values of the Format parameter, and the meaning of each, are
  described below.

  ffGeneral - General number format. The value is converted to the shortest
  possible decimal string using fixed or scientific format. Trailing zeros
  are removed from the resulting string, and a decimal point appears only
  if necessary. The resulting string uses fixed point format if the number
  of digits to the left of the decimal point in the value is less than or
  equal to the specified precision, and if the value is greater than or
  equal to 0.00001. Otherwise the resulting string uses scientific format,
  and the Digits parameter specifies the minimum number of digits in the
  exponent (between 0 and 4).

  ffExponent - Scientific format. The value is converted to a string of the
  form "-d.ddd...E+dddd". The resulting string starts with a minus sign if
  the number is negative, and one digit always precedes the decimal point.
  The total number of digits in the resulting string (including the one
  before the decimal point) is given by the Precision parameter. The "E"
  exponent character in the resulting string is always followed by a plus
  or minus sign and up to four digits. The Digits parameter specifies the
  minimum number of digits in the exponent (between 0 and 4).

  ffFixed - Fixed point format. The value is converted to a string of the
  form "-ddd.ddd...". The resulting string starts with a minus sign if the
  number is negative, and at least one digit always precedes the decimal
  point. The number of digits after the decimal point is given by the Digits
  parameter--it must be between 0 and 18. If the number of digits to the
  left of the decimal point is greater than the specified precision, the
  resulting value will use scientific format.

  ffNumber - Number format. The value is converted to a string of the form
  "-d,ddd,ddd.ddd...". The ffNumber format corresponds to the ffFixed format,
  except that the resulting string contains thousand separators.

  ffCurrency - Currency format. The value is converted to a string that
  represents a currency amount. The conversion is controlled by the
  CurrencyString, CurrencyFormat, NegCurrFormat, ThousandSeparator, and
  DecimalSeparator global variables, all of which are initialized from
  locale settings provided by the operating system.  For example,
  Currency Format preferences can be set in the International section
  of the Windows Control Panel.
  The number of digits after the decimal point is given by the Digits
  parameter--it must be between 0 and 18.

  For all formats, the actual characters used as decimal and thousand
  separators are obtained from the DecimalSeparator and ThousandSeparator
  global variables.

  If the given value is a NAN (not-a-number), the resulting string is 'NAN'.
  If the given value is positive infinity, the resulting string is 'INF'. If
  the given value is negative infinity, the resulting string is '-INF'. }

function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer): string; overload;
function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): string; overload;

{ CurrToStrF converts the currency value given by Value to its string
  representation. A call to CurrToStrF corresponds to a call to
  FloatToStrF with an implied precision of 19 digits. }

function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): string; overload;
function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer; const FormatSettings: TFormatSettings): string; overload;

{ FloatToText converts the given floating-point value to its decimal
  representation using the specified format, precision, and digits. The
  Value parameter must be a variable of type Extended or Currency, as
  indicated by the ValueType parameter. The resulting string of characters
  is stored in the given buffer, and the returned value is the number of
  characters stored. The resulting string is not null-terminated. For
  further details, see the description of the FloatToStrF function. }

function FloatToText(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer): Integer; overload;
function FloatToText(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): Integer; overload;

{ FormatFloat formats the floating-point value given by Value using the
  format string given by Format. The following format specifiers are
  supported in the format string:

  0     Digit placeholder. If the value being formatted has a digit in the
        position where the '0' appears in the format string, then that digit
        is copied to the output string. Otherwise, a '0' is stored in that
        position in the output string.

  #     Digit placeholder. If the value being formatted has a digit in the
        position where the '#' appears in the format string, then that digit
        is copied to the output string. Otherwise, nothing is stored in that
        position in the output string.

  .     Decimal point. The first '.' character in the format string
        determines the location of the decimal separator in the formatted
        value; any additional '.' characters are ignored. The actual
        character used as a the decimal separator in the output string is
        determined by the DecimalSeparator global variable, which is initialized
        from locale settings obtained from the operating system.

  ,     Thousand separator. If the format string contains one or more ','
        characters, the output will have thousand separators inserted between
        each group of three digits to the left of the decimal point. The
        placement and number of ',' characters in the format string does not
        affect the output, except to indicate that thousand separators are
        wanted. The actual character used as a the thousand separator in the
        output is determined by the ThousandSeparator global variable, which
        is initialized from locale settings obtained from the operating system.

  E+    Scientific notation. If any of the strings 'E+', 'E-', 'e+', or 'e-'
  E-    are contained in the format string, the number is formatted using
  e+    scientific notation. A group of up to four '0' characters can
  e-    immediately follow the 'E+', 'E-', 'e+', or 'e-' to determine the
        minimum number of digits in the exponent. The 'E+' and 'e+' formats
        cause a plus sign to be output for positive exponents and a minus
        sign to be output for negative exponents. The 'E-' and 'e-' formats
        output a sign character only for negative exponents.

  'xx'  Characters enclosed in single or double quotes are output as-is, and
  "xx"  do not affect formatting.

  ;     Separates sections for positive, negative, and zero numbers in the
        format string.

  The locations of the leftmost '0' before the decimal point in the format
  string and the rightmost '0' after the decimal point in the format string
  determine the range of digits that are always present in the output string.

  The number being formatted is always rounded to as many decimal places as
  there are digit placeholders ('0' or '#') to the right of the decimal
  point. If the format string contains no decimal point, the value being
  formatted is rounded to the nearest whole number.

  If the number being formatted has more digits to the left of the decimal
  separator than there are digit placeholders to the left of the '.'
  character in the format string, the extra digits are output before the
  first digit placeholder.

  To allow different formats for positive, negative, and zero values, the
  format string can contain between one and three sections separated by
  semicolons.

  One section - The format string applies to all values.

  Two sections - The first section applies to positive values and zeros, and
  the second section applies to negative values.

  Three sections - The first section applies to positive values, the second
  applies to negative values, and the third applies to zeros.

  If the section for negative values or the section for zero values is empty,
  that is if there is nothing between the semicolons that delimit the
  section, the section for positive values is used instead.

  If the section for positive values is empty, or if the entire format string
  is empty, the value is formatted using general floating-point formatting
  with 15 significant digits, corresponding to a call to FloatToStrF with
  the ffGeneral format. General floating-point formatting is also used if
  the value has more than 18 digits to the left of the decimal point and
  the format string does not specify scientific notation.

  The table below shows some sample formats and the results produced when
  the formats are applied to different values:

  Format string          1234        -1234       0.5         0
  -----------------------------------------------------------------------
                         1234        -1234       0.5         0
  0                      1234        -1234       1           0
  0.00                   1234.00     -1234.00    0.50        0.00
  #.##                   1234        -1234       .5
  #,##0.00               1,234.00    -1,234.00   0.50        0.00
  #,##0.00;(#,##0.00)    1,234.00    (1,234.00)  0.50        0.00
  #,##0.00;;Zero         1,234.00    -1,234.00   0.50        Zero
  0.000E+00              1.234E+03   -1.234E+03  5.000E-01   0.000E+00
  #.###E-0               1.234E3     -1.234E3    5E-1        0E0
  ----------------------------------------------------------------------- }

function FormatFloat(const Format: string; Value: Extended): string; overload;
function FormatFloat(const Format: string; Value: Extended;
  const FormatSettings: TFormatSettings): string; overload;

{ FormatCurr formats the currency value given by Value using the format
  string given by Format. For further details, see the description of the
  FormatFloat function. }

function FormatCurr(const Format: string; Value: Currency): string; overload;
function FormatCurr(const Format: string; Value: Currency;
  const FormatSettings: TFormatSettings): string; overload;

{ FloatToTextFmt converts the given floating-point value to its decimal
  representation using the specified format. The Value parameter must be a
  variable of type Extended or Currency, as indicated by the ValueType
  parameter. The resulting string of characters is stored in the given
  buffer, and the returned value is the number of characters stored. The
  resulting string is not null-terminated. For further details, see the
  description of the FormatFloat function. }

function FloatToTextFmt(Buf: PChar; const Value; ValueType: TFloatValue;
  Format: PChar): Integer; overload;
function FloatToTextFmt(Buf: PChar; const Value; ValueType: TFloatValue;
  Format: PChar; const FormatSettings: TFormatSettings): Integer; overload;

{ StrToFloat converts the given string to a floating-point value. The string
  must consist of an optional sign (+ or -), a string of digits with an
  optional decimal point, and an optional 'E' or 'e' followed by a signed
  integer. Leading and trailing blanks in the string are ignored. The
  DecimalSeparator global variable defines the character that must be used
  as a decimal point. Thousand separators and currency symbols are not
  allowed in the string. If the string doesn't contain a valid value, an
  EConvertError exception is raised. }

function StrToFloat(const S: string): Extended; overload;
function StrToFloat(const S: string;
  const FormatSettings: TFormatSettings): Extended; overload;

function StrToFloatDef(const S: string;
  const Default: Extended): Extended; overload;
function StrToFloatDef(const S: string; const Default: Extended;
  const FormatSettings: TFormatSettings): Extended; overload;

function TryStrToFloat(const S: string; out Value: Extended): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Extended;
  const FormatSettings: TFormatSettings): Boolean; overload;

function TryStrToFloat(const S: string; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Double;
  const FormatSettings: TFormatSettings): Boolean; overload;

function TryStrToFloat(const S: string; out Value: Single): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Single;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ StrToCurr converts the given string to a currency value. For further
  details, see the description of the StrToFloat function. }

function StrToCurr(const S: string): Currency; overload;
function StrToCurr(const S: string;
  const FormatSettings: TFormatSettings): Currency; overload;

function StrToCurrDef(const S: string;
  const Default: Currency): Currency; overload;
function StrToCurrDef(const S: string; const Default: Currency;
  const FormatSettings: TFormatSettings): Currency; overload;

function TryStrToCurr(const S: string; out Value: Currency): Boolean; overload;
function TryStrToCurr(const S: string; out Value: Currency;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ TextToFloat converts the null-terminated string given by Buffer to a
  floating-point value which is returned in the variable given by Value.
  The Value parameter must be a variable of type Extended or Currency, as
  indicated by the ValueType parameter. The return value is True if the
  conversion was successful, or False if the string is not a valid
  floating-point value. For further details, see the description of the
  StrToFloat function. }

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue): Boolean; overload;
function TextToFloat(Buffer: PChar; var Value; ValueType: TFloatValue;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ FloatToDecimal converts a floating-point value to a decimal representation
  that is suited for further formatting. The Value parameter must be a
  variable of type Extended or Currency, as indicated by the ValueType
  parameter. For values of type Extended, the Precision parameter specifies
  the requested number of significant digits in the result--the allowed range
  is 1..18. For values of type Currency, the Precision parameter is ignored,
  and the implied precision of the conversion is 19 digits. The Decimals
  parameter specifies the requested maximum number of digits to the left of
  the decimal point in the result. Precision and Decimals together control
  how the result is rounded. To produce a result that always has a given
  number of significant digits regardless of the magnitude of the number,
  specify 9999 for the Decimals parameter. The result of the conversion is
  stored in the specified TFloatRec record as follows:

  Exponent - Contains the magnitude of the number, i.e. the number of
  significant digits to the right of the decimal point. The Exponent field
  is negative if the absolute value of the number is less than one. If the
  number is a NAN (not-a-number), Exponent is set to -32768. If the number
  is INF or -INF (positive or negative infinity), Exponent is set to 32767.

  Negative - True if the number is negative, False if the number is zero
  or positive.

  Digits - Contains up to 18 (for type Extended) or 19 (for type Currency)
  significant digits followed by a null terminator. The implied decimal
  point (if any) is not stored in Digits. Trailing zeros are removed, and
  if the resulting number is zero, NAN, or INF, Digits contains nothing but
  the null terminator. }

procedure FloatToDecimal(var Result: TFloatRec; const Value;
  ValueType: TFloatValue; Precision, Decimals: Integer);

{ Date/time support routines }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp;

{ EncodeDate encodes the given year, month, and day into a TDateTime value.
  The year must be between 1 and 9999, the month must be between 1 and 12,
  and the day must be between 1 and N, where N is the number of days in the
  specified month. If the specified values are not within range, an
  EConvertError exception is raised. The resulting value is the number of
  days between 12/30/1899 and the given date. }

function EncodeDate(Year, Month, Day: Word): TDateTime;

{ EncodeTime encodes the given hour, minute, second, and millisecond into a
  TDateTime value. The hour must be between 0 and 23, the minute must be
  between 0 and 59, the second must be between 0 and 59, and the millisecond
  must be between 0 and 999. If the specified values are not within range, an
  EConvertError exception is raised. The resulting value is a number between
  0 (inclusive) and 1 (not inclusive) that indicates the fractional part of
  a day given by the specified time. The value 0 corresponds to midnight,
  0.5 corresponds to noon, 0.75 corresponds to 6:00 pm, etc. }

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;

{ Instead of generating errors the following variations of EncodeDate and
  EncodeTime simply return False if the parameters given are not valid.
  Other than that, these functions are functionally the same as the above
  functions. }

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;

{ DecodeDate decodes the integral (date) part of the given TDateTime value
  into its corresponding year, month, and day. If the given TDateTime value
  is less than or equal to zero, the year, month, and day return parameters
  are all set to zero. }

procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);

{ This variation of DecodeDate works similarly to the above function but
  returns more information.  The result value of this function indicates
  whether the year decoded is a leap year or not.  }

function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day,
  DOW: Word): Boolean;

{$IFDEF LINUX}
function InternalDecodeDate(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
{$ENDIF}

{ DecodeTime decodes the fractional (time) part of the given TDateTime value
  into its corresponding hour, minute, second, and millisecond. }

procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);

{$IFDEF MSWINDOWS}
{ DateTimeToSystemTime converts a date and time from Delphi's TDateTime
  format into the Win32 API's TSystemTime format. }

procedure DateTimeToSystemTime(const DateTime: TDateTime; var SystemTime: TSystemTime);

{ SystemTimeToDateTime converts a date and time from the Win32 API's
  TSystemTime format into Delphi's TDateTime format. }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
{$ENDIF}

{ DayOfWeek returns the day of the week of the given date. The result is an
  integer between 1 and 7, corresponding to Sunday through Saturday.
  This function is not ISO 8601 compliant, for that see the DateUtils unit. }

function DayOfWeek(const DateTime: TDateTime): Word;

{ Date returns the current date. }

function Date: TDateTime;

{ Time returns the current time. }

function Time: TDateTime;
{$IFDEF LINUX}
  { clashes with Time in <X11/Xlib.h>, use GetTime instead }
  {$EXTERNALSYM Time}
{$ENDIF}
function GetTime: TDateTime;

{ Now returns the current date and time, corresponding to Date + Time. }

function Now: TDateTime;

{ Current year returns the year portion of the date returned by Now }

function CurrentYear: Word;

{ IncMonth returns Date shifted by the specified number of months.
  NumberOfMonths parameter can be negative, to return a date N months ago.
  If the input day of month is greater than the last day of the resulting
  month, the day is set to the last day of the resulting month.
  Input time of day is copied to the DateTime result.  }

function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer = 1): TDateTime;

{ Optimized version of IncMonth that works with years, months and days
  directly.  See above comments for more detail as to what happens to the day
  when incrementing months }

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);

{ ReplaceTime replaces the time portion of the DateTime parameter with the given
  time value, adjusting the signs as needed if the date is prior to 1900
  (Date value less than zero)  }

procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);

{ ReplaceDate replaces the date portion of the DateTime parameter with the given
  date value, adjusting as needed for negative dates }

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);

{ IsLeapYear determines whether the given year is a leap year. }

function IsLeapYear(Year: Word): Boolean;

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;

{ The MonthDays array can be used to quickly find the number of
  days in a month:  MonthDays[IsLeapYear(Y), M]      }

const
  MonthDays: array [Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

{ Each of the date/time formatting routines that uses global variables
  for formatting (separators, decimals, etc.), has an overloaded equivalent
  requiring a parameter of type TFormatSettings. This additional parameter
  provides the formatting information rather than the global variables. For
  more information see the note at TFormatSettings.  }

{ DateToStr converts the date part of the given TDateTime value to a string.
  The conversion uses the format specified by the ShortDateFormat global
  variable. }

function DateToStr(const DateTime: TDateTime): string; overload;
function DateToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;

{ TimeToStr converts the time part of the given TDateTime value to a string.
  The conversion uses the format specified by the LongTimeFormat global
  variable. }

function TimeToStr(const DateTime: TDateTime): string; overload;
function TimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;

{ DateTimeToStr converts the given date and time to a string. The resulting
  string consists of a date and time formatted using the ShortDateFormat and
  LongTimeFormat global variables. Time information is included in the
  resulting string only if the fractional part of the given date and time
  value is non-zero. }

function DateTimeToStr(const DateTime: TDateTime): string; overload;
function DateTimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;

{ StrToDate converts the given string to a date value. The string must
  consist of two or three numbers, separated by the character defined by
  the DateSeparator global variable. The order for month, day, and year is
  determined by the ShortDateFormat global variable--possible combinations
  are m/d/y, d/m/y, and y/m/d. If the string contains only two numbers, it
  is interpreted as a date (m/d or d/m) in the current year. Year values
  between 0 and 99 are assumed to be in the current century. If the given
  string does not contain a valid date, an EConvertError exception is
  raised. }

function StrToDate(const S: string): TDateTime; overload;
function StrToDate(const S: string;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function StrToDateDef(const S: string;
  const Default: TDateTime): TDateTime; overload;
function StrToDateDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function TryStrToDate(const S: string; out Value: TDateTime): Boolean; overload;
function TryStrToDate(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ StrToTime converts the given string to a time value. The string must
  consist of two or three numbers, separated by the character defined by
  the TimeSeparator global variable, optionally followed by an AM or PM
  indicator. The numbers represent hour, minute, and (optionally) second,
  in that order. If the time is followed by AM or PM, it is assumed to be
  in 12-hour clock format. If no AM or PM indicator is included, the time
  is assumed to be in 24-hour clock format. If the given string does not
  contain a valid time, an EConvertError exception is raised. }

function StrToTime(const S: string): TDateTime; overload;
function StrToTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function StrToTimeDef(const S: string;
  const Default: TDateTime): TDateTime; overload;
function StrToTimeDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function TryStrToTime(const S: string; out Value: TDateTime): Boolean; overload;
function TryStrToTime(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ StrToDateTime converts the given string to a date and time value. The
  string must contain a date optionally followed by a time. The date and
  time parts of the string must follow the formats described for the
  StrToDate and StrToTime functions. }

function StrToDateTime(const S: string): TDateTime; overload;
function StrToDateTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function StrToDateTimeDef(const S: string;
  const Default: TDateTime): TDateTime; overload;
function StrToDateTimeDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime; overload;

function TryStrToDateTime(const S: string;
  out Value: TDateTime): Boolean; overload;
function TryStrToDateTime(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean; overload;

{ FormatDateTime formats the date-and-time value given by DateTime using the
  format given by Format. The following format specifiers are supported:

  c       Displays the date using the format given by the ShortDateFormat
          global variable, followed by the time using the format given by
          the LongTimeFormat global variable. The time is not displayed if
          the fractional part of the DateTime value is zero.

  d       Displays the day as a number without a leading zero (1-31).

  dd      Displays the day as a number with a leading zero (01-31).

  ddd     Displays the day as an abbreviation (Sun-Sat) using the strings
          given by the ShortDayNames global variable.

  dddd    Displays the day as a full name (Sunday-Saturday) using the strings
          given by the LongDayNames global variable.

  ddddd   Displays the date using the format given by the ShortDateFormat
          global variable.

  dddddd  Displays the date using the format given by the LongDateFormat
          global variable.

  g       Displays the period/era as an abbreviation (Japanese and
          Taiwanese locales only).

  gg      Displays the period/era as a full name.

  e       Displays the year in the current period/era as a number without
          a leading zero (Japanese, Korean and Taiwanese locales only).

  ee      Displays the year in the current period/era as a number with
          a leading zero (Japanese, Korean and Taiwanese locales only).

  m       Displays the month as a number without a leading zero (1-12). If
          the m specifier immediately follows an h or hh specifier, the
          minute rather than the month is displayed.

  mm      Displays the month as a number with a leading zero (01-12). If
          the mm specifier immediately follows an h or hh specifier, the
          minute rather than the month is displayed.

  mmm     Displays the month as an abbreviation (Jan-Dec) using the strings
          given by the ShortMonthNames global variable.

  mmmm    Displays the month as a full name (January-December) using the
          strings given by the LongMonthNames global variable.

  yy      Displays the year as a two-digit number (00-99).

  yyyy    Displays the year as a four-digit number (0000-9999).

  h       Displays the hour without a leading zero (0-23).

  hh      Displays the hour with a leading zero (00-23).

  n       Displays the minute without a leading zero (0-59).

  nn      Displays the minute with a leading zero (00-59).

  s       Displays the second without a leading zero (0-59).

  ss      Displays the second with a leading zero (00-59).

  z       Displays the millisecond without a leading zero (0-999).

  zzz     Displays the millisecond with a leading zero (000-999).

  t       Displays the time using the format given by the ShortTimeFormat
          global variable.

  tt      Displays the time using the format given by the LongTimeFormat
          global variable.

  am/pm   Uses the 12-hour clock for the preceding h or hh specifier, and
          displays 'am' for any hour before noon, and 'pm' for any hour
          after noon. The am/pm specifier can use lower, upper, or mixed
          case, and the result is displayed accordingly.

  a/p     Uses the 12-hour clock for the preceding h or hh specifier, and
          displays 'a' for any hour before noon, and 'p' for any hour after
          noon. The a/p specifier can use lower, upper, or mixed case, and
          the result is displayed accordingly.

  ampm    Uses the 12-hour clock for the preceding h or hh specifier, and
          displays the contents of the TimeAMString global variable for any
          hour before noon, and the contents of the TimePMString global
          variable for any hour after noon.

  /       Displays the date separator character given by the DateSeparator
          global variable.

  :       Displays the time separator character given by the TimeSeparator
          global variable.

  'xx'    Characters enclosed in single or double quotes are displayed as-is,
  "xx"    and do not affect formatting.

  Format specifiers may be written in upper case as well as in lower case
  letters--both produce the same result.

  If the string given by the Format parameter is empty, the date and time
  value is formatted as if a 'c' format specifier had been given.

  The following example:

    S := FormatDateTime('"The meeting is on" dddd, mmmm d, yyyy, ' +
      '"at" hh:mm AM/PM', StrToDateTime('2/15/95 10:30am'));

  assigns 'The meeting is on Wednesday, February 15, 1995 at 10:30 AM' to
  the string variable S. }

function FormatDateTime(const Format: string;
  DateTime: TDateTime): string; overload;
function FormatDateTime(const Format: string; DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;

{ DateTimeToString converts the date and time value given by DateTime using
  the format string given by Format into the string variable given by Result.
  For further details, see the description of the FormatDateTime function. }

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime); overload;
procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime; const FormatSettings: TFormatSettings); overload;

{ FloatToDateTime will range validate a value to make sure it falls
  within the acceptable date range }

const
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }

function FloatToDateTime(const Value: Extended): TDateTime;
function TryFloatToDateTime(const Value: Extended; out AResult: TDateTime): Boolean;

{ System error messages }

function SysErrorMessage(ErrorCode: Integer): string;

{ Initialization file support }

function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string; platform;
function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char; platform;

{ GetFormatSettings resets all locale-specific variables (date, time, number,
  currency formats, system locale) to the values provided by the operating system. }

procedure GetFormatSettings;

{ GetLocaleFormatSettings loads locale-specific variables (date, time, number,
  currency formats) with values provided by the operating system for the
  specified locale (LCID). The values are stored in the FormatSettings record. }

{$IFDEF MSWINDOWS}
procedure GetLocaleFormatSettings(LCID: Integer;
  var FormatSettings: TFormatSettings);
{$ENDIF}

{ Exception handling routines }

{$IFDEF LINUX}
{   InquireSignal is used to determine the state of an OS signal handler.
    Pass it one of the RTL_SIG* constants, and it will return a TSignalState
    which will tell you if the signal has been hooked, not hooked, or overriden
    by some other module.  You can use this function to determine if some other
    module has hijacked your signal handlers, should you wish to reinstall your
    own. This is a risky proposition under Linux, and is only recommended as a
    last resort.  Do not pass RTL_SIGDEFAULT to this function.
}
function InquireSignal(RtlSigNum: Integer): TSignalState;

{ AbandonSignalHandler tells the RTL to leave a signal handler
    in place, even if we believe that we hooked it at startup time.

    Once you have called AbandonSignalHandler with a specific signal number,
    neither UnhookSignal nor the RTL will restore any previous signal handler
    under any condition.
}
procedure AbandonSignalHandler(RtlSigNum: Integer);

{ HookSignal is used to hook individual signals, or an RTL-defined default 
    set of signals.  It does not test whether a signal has already been
    hooked, so it should be used in conjunction with InquireSignal.  It is
    exposed to enable users to hook signals in standalone libraries, or in the
    event that an external module hijacks the RTL installed signal handlers.
    Pass RTL_SIGDEFAULT if you want to hook all the signals that the RTL
    normally hooks at startup time.
}
procedure HookSignal(RtlSigNum: Integer);

{ UnhookSignal is used to remove signal handlers installed by HookSignal.
    It can remove individual signal handlers, or the RTL-defined default set
    of signals.  If OnlyIfHooked is True, then we will only unhook the signal
    if the signal handler has been hooked, and has not since been overriden by
    some foreign handler.
}
procedure UnhookSignal(RtlSigNum: Integer; OnlyIfHooked: Boolean = True);

{ HookOSExceptions is used internally by thread support.  DON'T call this
  function yourself. }
procedure HookOSExceptions;

{ MapSignal is used internally as well.  It maps a signal and associated
  context to an internal value that represents the type of Exception
  class to raise. }
function MapSignal(SigNum: Integer; Context: PSigContext): LongWord;

{ SignalConverter is used internally to properly reinit the FPU and properly
  raise an external OS exception object.  DON'T call this function yourself. }
procedure SignalConverter(ExceptionEIP: LongWord; FaultAddr: LongWord; ErrorCode: LongWord);

{
    See the comment at the threadvar declarations for these below.  The access
    to these has been implemented through getter/setter functions because you
    cannot use threadvars across packages.
}
procedure SetSafeCallExceptionMsg(const Msg: String);
procedure SetSafeCallExceptionAddr(Addr: Pointer);
function GetSafeCallExceptionMsg: String;
function GetSafeCallExceptionAddr: Pointer;

{ HookOSExceptionsProc is used internally and cannot be used in a conventional
  manner.  DON'T ever set this variable. }
var
  HookOSExceptionsProc: procedure = nil platform deprecated;

{ LoadLibrary / FreeLibrary are defined here only for convenience.  On Linux,
  they map directly to dlopen / dlclose.  Note that module loading semantics
  on Linux are not identical to Windows.  }

function LoadLibrary(ModuleName: PChar): HMODULE;

function FreeLibrary(Module: HMODULE): LongBool;

{ GetProcAddress does what it implies.  It performs the same function as the like
  named function under Windows.  dlsym does not quite have the same sematics as
  GetProcAddress as it will return the address of a symbol in another module if
  it was not found in the given HMODULE.  This function will verify that the 'Proc'
  is actually found within the 'Module', and if not returns nil }
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;

{ Given a module name, this function will return the module handle.  There is no
  direct equivalent in Linux so this function provides that capability.  Also
  note, this function is specific to glibc. }
function GetModuleHandle(ModuleName: PChar): HMODULE;

{ This function works just like GetModuleHandle, except it will look for a module
  that matches the given base package name.  For example, given the base package
  name 'package', the actual module name is, by default, 'bplpackage.so'.  This
  function will search for the string 'package' within the module name. }
function GetPackageModuleHandle(PackageName: PChar): HMODULE;

{$ENDIF}

{ In Linux, the parameter to sleep() is in whole seconds.  In Windows, the
  parameter is in milliseconds.  To ease headaches, we implement a version
  of sleep here for Linux that takes milliseconds and calls a Linux system
  function with sub-second resolution.  This maps directly to the Windows
  API on Windows. }

procedure Sleep(milliseconds: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ENDIF}
{$IFDEF MSWINDOWS}
(*$EXTERNALSYM Sleep*)
{$ENDIF}

function GetModuleName(Module: HMODULE): string;

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PChar; Size: Integer): Integer;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);

procedure Abort;

procedure OutOfMemoryError;

procedure Beep;

{ MBCS functions }

{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
var
  LeadBytes: set of Char = [];
(*$EXTERNALSYM LeadBytes*)
(*$HPPEMIT 'namespace Sysutils {'*)
(*$HPPEMIT 'extern PACKAGE System::Set<Byte, 0, 255>  LeadBytes;'*)
(*$HPPEMIT '} // namespace Sysutils'*)

{ ByteType indicates what kind of byte exists at the Index'th byte in S.
  Western locales always return mbSingleByte.  Far East multibyte locales
  may also return mbLeadByte, indicating the byte is the first in a multibyte
  character sequence, and mbTrailByte, indicating that the byte is one of
  a sequence of bytes following a lead byte.  One or more trail bytes can
  follow a lead byte, depending on locale charset encoding and OS platform.
  Parameters are assumed to be valid. }

function ByteType(const S: string; Index: Integer): TMbcsByteType;

{ StrByteType works the same as ByteType, but on null-terminated PChar strings }

function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;

{ ByteToCharLen returns the character length of a MBCS string, scanning the
  string for up to MaxLen bytes.  In multibyte character sets, the number of
  characters in a string may be less than the number of bytes.  }

function ByteToCharLen(const S: string; MaxLen: Integer): Integer;

{ CharToByteLen returns the byte length of a MBCS string, scanning the string
  for up to MaxLen characters. }

function CharToByteLen(const S: string; MaxLen: Integer): Integer;

{ ByteToCharIndex returns the 1-based character index of the Index'th byte in
  a MBCS string.  Returns zero if Index is out of range:
  (Index <= 0) or (Index > Length(S)) }

function ByteToCharIndex(const S: string; Index: Integer): Integer;

{ CharToByteIndex returns the 1-based byte index of the Index'th character
  in a MBCS string.  Returns zero if Index or Result are out of range:
  (Index <= 0) or (Index > Length(S)) or (Result would be > Length(S)) }

function CharToByteIndex(const S: string; Index: Integer): Integer;

{ StrCharLength returns the number of bytes required by the first character
  in Str.  In Windows, multibyte characters can be up to two bytes in length.
  In Linux, multibyte characters can be up to six bytes in length (UTF-8). }

function StrCharLength(const Str: PChar): Integer;

{ StrNextChar returns a pointer to the first byte of the character following
  the character pointed to by Str.  }

function StrNextChar(const Str: PChar): PChar;

{ CharLength returns the number of bytes required by the character starting
  at bytes S[Index].  }

function CharLength(const S: String; Index: Integer): Integer;

{ NextCharIndex returns the byte index of the first byte of the character
  following the character starting at S[Index].  }

function NextCharIndex(const S: String; Index: Integer): Integer;

{ IsPathDelimiter returns True if the character at byte S[Index]
  is a PathDelimiter ('\' or '/'), and it is not a MBCS lead or trail byte. }

function IsPathDelimiter(const S: string; Index: Integer): Boolean;

{ IsDelimiter returns True if the character at byte S[Index] matches any
  character in the Delimiters string, and the character is not a MBCS lead or
  trail byte.  S may contain multibyte characters; Delimiters must contain
  only single byte characters. }

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;

{ IncludeTrailingPathDelimiter returns the path with a PathDelimiter
  ('/' or '\') at the end.  This function is MBCS enabled. }

function IncludeTrailingPathDelimiter(const S: string): string;

{ IncludeTrailingBackslash is the old name for IncludeTrailingPathDelimiter. }

function IncludeTrailingBackslash(const S: string): string; platform;

{ ExcludeTrailingPathDelimiter returns the path without a PathDelimiter
  ('\' or '/') at the end.  This function is MBCS enabled. }

function ExcludeTrailingPathDelimiter(const S: string): string;

{ ExcludeTrailingBackslash is the old name for ExcludeTrailingPathDelimiter. }

function ExcludeTrailingBackslash(const S: string): string; platform;

{ LastDelimiter returns the byte index in S of the rightmost whole
  character that matches any character in Delimiters (except null (#0)).
  S may contain multibyte characters; Delimiters must contain only single
  byte non-null characters.
  Example: LastDelimiter('\.:', 'c:\filename.ext') returns 12. }

function LastDelimiter(const Delimiters, S: string): Integer;

{ AnsiCompareFileName supports DOS file name comparison idiosyncracies
  in Far East locales (Zenkaku) on Windows.
  In non-MBCS locales on Windows, AnsiCompareFileName is identical to
  AnsiCompareText (case insensitive).
  On Linux, AnsiCompareFileName is identical to AnsiCompareStr (case sensitive).
  For general purpose file name comparisions, you should use this function
  instead of AnsiCompareText. }

function AnsiCompareFileName(const S1, S2: string): Integer;

function SameFileName(const S1, S2: string): Boolean;

{ AnsiLowerCaseFileName supports lowercase conversion idiosyncracies of
  DOS file names in Far East locales (Zenkaku).  In non-MBCS locales,
  AnsiLowerCaseFileName is identical to AnsiLowerCase. }

function AnsiLowerCaseFileName(const S: string): string;

{ AnsiUpperCaseFileName supports uppercase conversion idiosyncracies of
  DOS file names in Far East locales (Zenkaku).  In non-MBCS locales,
  AnsiUpperCaseFileName is identical to AnsiUpperCase. }

function AnsiUpperCaseFileName(const S: string): string;

{ AnsiPos:  Same as Pos but supports MBCS strings }

function AnsiPos(const Substr, S: string): Integer;

{ AnsiStrPos: Same as StrPos but supports MBCS strings }

function AnsiStrPos(Str, SubStr: PChar): PChar;

{ AnsiStrRScan: Same as StrRScan but supports MBCS strings }

function AnsiStrRScan(Str: PChar; Chr: Char): PChar;

{ AnsiStrScan: Same as StrScan but supports MBCS strings }

function AnsiStrScan(Str: PChar; Chr: Char): PChar;

{ StringReplace replaces occurances of <oldpattern> with <newpattern> in a
  given string.  Assumes the string may contain Multibyte characters }

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;

{ WrapText will scan a string for BreakChars and insert the BreakStr at the
  last BreakChar position before MaxCol.  Will not insert a break into an
  embedded quoted string (both ''' and '"' supported) }

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;
  MaxCol: Integer): string; overload;
function WrapText(const Line: string; MaxCol: Integer = 45): string; overload;

{ FindCmdLineSwitch determines whether the string in the Switch parameter
  was passed as a command line argument to the application.  SwitchChars
  identifies valid argument-delimiter characters (i.e., "-" and "/" are
  common delimiters). The IgnoreCase paramter controls whether a
  case-sensistive or case-insensitive search is performed. }

const
  SwitchChars = {$IFDEF MSWINDOWS} ['/','-']; {$ENDIF}
                {$IFDEF LINUX}  ['-'];  {$ENDIF}

function FindCmdLineSwitch(const Switch: string; const Chars: TSysCharSet;
  IgnoreCase: Boolean): Boolean; overload;

{ These versions of FindCmdLineSwitch are convenient for writing portable
  code.  The characters that are valid to indicate command line switches vary
  on different platforms.  For example, '/' cannot be used as a switch char
  on Linux because '/' is the path delimiter. }

{ This version uses SwitchChars defined above, and IgnoreCase False. }
function FindCmdLineSwitch(const Switch: string): Boolean; overload;

{ This version uses SwitchChars defined above. }
function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean; overload;

{ FreeAndNil frees the given TObject instance and sets the variable reference
  to nil.  Be careful to only pass TObjects to this routine. }

procedure FreeAndNil(var Obj);

{ Interface support routines }

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGUID): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
function Supports(const AClass: TClass; const IID: TGUID): Boolean; overload;

function CreateGUID(out Guid: TGUID): HResult;
{$IFDEF MSWINDOWS}
  stdcall;
{$ENDIF}
function StringToGUID(const S: string): TGUID;
function GUIDToString(const GUID: TGUID): string;
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
{$IFDEF MSWINDOWS}
  stdcall;  {$EXTERNALSYM IsEqualGUID}
{$ENDIF}

{ Package support routines }

{ Package Info flags }

const
  pfNeverBuild = $00000001;
  pfDesignOnly = $00000002;
  pfRunOnly = $00000004;
  pfIgnoreDupUnits = $00000008;
  pfModuleTypeMask = $C0000000;
  pfExeModule = $00000000;
  pfPackageModule = $40000000;
  pfProducerMask = $0C000000;
  pfV3Produced =  $00000000;
  pfProducerUndefined = $04000000;
  pfBCB4Produced = $08000000;
  pfDelphi4Produced = $0C000000;
  pfLibraryModule = $80000000;

{ Unit info flags }

const
  ufMainUnit = $01;
  ufPackageUnit = $02;
  ufWeakUnit = $04;
  ufOrgWeakUnit = $08;
  ufImplicitUnit = $10;

  ufWeakPackageUnit = ufPackageUnit or ufWeakUnit;

{$IFDEF LINUX}
var
  PkgLoadingMode: Integer = RTLD_LAZY;
{$ENDIF}

{ Procedure type of the callback given to GetPackageInfo.  Name is the actual
  name of the package element.  If IsUnit is True then Name is the name of
  a contained unit; a required package if False.  Param is the value passed
  to GetPackageInfo }

type
  TNameType = (ntContainsUnit, ntRequiresPackage, ntDcpBpiName);

  TPackageInfoProc = procedure (const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);

{ LoadPackage loads a given package DLL, checks for duplicate units and
  calls the initialization blocks of all the contained units }

function LoadPackage(const Name: string): HMODULE;

{ UnloadPackage does the opposite of LoadPackage by calling the finalization
  blocks of all contained units, then unloading the package DLL }

procedure UnloadPackage(Module: HMODULE);

{ GetPackageInfo accesses the given package's info table and enumerates
  all the contained units and required packages }

procedure GetPackageInfo(Module: HMODULE; Param: Pointer; var Flags: Integer;
  InfoProc: TPackageInfoProc);

{ GetPackageDescription loads the description resource from the package
  library. If the description resource does not exist,
  an empty string is returned. }
function GetPackageDescription(ModuleName: PChar): string;

{ InitializePackage validates and initializes the given package DLL }

procedure InitializePackage(Module: HMODULE);

{ FinalizePackage finalizes the given package DLL }

procedure FinalizePackage(Module: HMODULE);

{ RaiseLastOSError calls GetLastError to retrieve the code for
  the last occuring error in a call to an OS or system library function.
  If GetLastError returns an error code,  RaiseLastOSError raises
  an EOSError exception with the error code and a system-provided
  message associated with with error. }

procedure RaiseLastOSError;

{$IFDEF MSWINDOWS}
procedure RaiseLastWin32Error; deprecated;  // use RaiseLastOSError

{ Win32Check is used to check the return value of a Win32 API function     }
{ which returns a BOOL to indicate success.  If the Win32 API function     }
{ returns False (indicating failure), Win32Check calls RaiseLastOSError }
{ to raise an exception.  If the Win32 API function returns True,          }
{ Win32Check returns True. }

function Win32Check(RetVal: BOOL): BOOL; platform;
{$ENDIF}

{ Termination procedure support }

type
  TTerminateProc = function: Boolean;

{ Call AddTerminateProc to add a terminate procedure to the system list of }
{ termination procedures.  Delphi will call all of the function in the     }
{ termination procedure list before an application terminates.  The user-  }
{ defined TermProc function should return True if the application can      }
{ safely terminate or False if the application cannot safely terminate.    }
{ If one of the functions in the termination procedure list returns False, }
{ the application will not terminate. }

procedure AddTerminateProc(TermProc: TTerminateProc);

{ CallTerminateProcs is called by VCL when an application is about to }
{ terminate.  It returns True only if all of the functions in the     }
{ system's terminate procedure list return True.  This function is    }
{ intended only to be called by Delphi, and it should not be called   }
{ directly. }

function CallTerminateProcs: Boolean;

function GDAL: LongWord;
procedure RCS;
procedure RPR;


{ HexDisplayPrefix contains the prefix to display on hexadecimal
  values - '$' for Pascal syntax, '0x' for C++ syntax.  This is
  for display only - this does not affect the string-to-integer
  conversion routines. }
var
  HexDisplayPrefix: string = '$';

{$IFDEF MSWINDOWS}
{ The GetDiskFreeSpace Win32 API does not support partitions larger than 2GB
  under Win95.  A new Win32 function, GetDiskFreeSpaceEx, supports partitions
  larger than 2GB but only exists on Win NT 4.0 and Win95 OSR2.
  The GetDiskFreeSpaceEx function pointer variable below will be initialized
  at startup to point to either the actual OS API function if it exists on
  the system, or to an internal Delphi function if it does not.  When running
  on Win95 pre-OSR2, the output of this function will still be limited to
  the 2GB range reported by Win95, but at least you don't have to worry
  about which API function to call in code you write.  }

var
  GetDiskFreeSpaceEx: function (Directory: PChar; var FreeAvailable,
    TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool stdcall = nil;

{ SafeLoadLibrary calls LoadLibrary, disabling normal Win32 error message
  popup dialogs if the requested file can't be loaded.  SafeLoadLibrary also
  preserves the current FPU control word (precision, exception masks) across
  the LoadLibrary call (in case the DLL you're loading hammers the FPU control
  word in its initialization, as many MS DLLs do)}

function SafeLoadLibrary(const FileName: string;
  ErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE;

{$ENDIF}

{$IFDEF LINUX}
{ SafeLoadLibrary calls LoadLibrary preserves the current FPU control
  word (precision, exception masks) across the LoadLibrary call (in
  case the shared object you're loading hammers the FPU control
  word in its initialization, as many MS DLLs do) }

function SafeLoadLibrary(const FileName: string;
  Dummy: LongWord = 0): HMODULE;
{$ENDIF}

{ Thread synchronization }

{ IReadWriteSync is an abstract interface for general read/write synchronization.
  Some implementations may allow simultaneous readers, but writers always have
  exclusive locks.

  Worst case is that this class behaves identical to a TRTLCriticalSection -
  that is, read and write locks block all other threads. }

type
  IReadWriteSync = interface
    ['{7B108C52-1D8F-4CDB-9CDF-57E071193D3F}']
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

  TSimpleRWSync = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

{ TThreadLocalCounter

  This class implements a lightweight non-blocking thread local storage
  mechanism specifically built for tracking per-thread recursion counts
  in TMultiReadExclusiveWriteSynchronizer.  This class is intended for
  Delphi RTL internal use only.  In the future it may be generalized
  and "hardened" for general application use, but until then leave it alone.

  Rules of Use:
  The tls object must be opened to gain access to the thread-specific data
  structure.  If a threadinfo block does not exist for the current thread,
  Open will allocate one.  Every call to Open must be matched with a call
  to Close.  The pointer returned by Open is invalid after the matching call
  to Close (or Delete).

  The thread info structure is unique to each thread.  Once you have it, it's
  yours.  You don't need to guard against concurrent access to the thread
  data by multiple threads - your thread is the only thread that will ever
  have access to the structure that Open returns.  The thread info structure
  is allocated and owned by the tls object.  If you put allocated pointers
  in the thread info make sure you free them before you delete the threadinfo
  node.

  When thread data is no longer needed, call the Delete method on the pointer.
  This must be done between calls to Open and Close.  You should not use the
  thread data after calling Delete.

  Important:  Do not keep the tls object open for long periods of time.
  In particular, be careful not to wait on a thread synchronization event or
  critical section while you have the tls object open.  It's much better to
  open and close the tls object before and after the blocking event than to
  leave the tls object open while waiting.

  Implementation Notes:
  The main purpose of this storage class is to provide thread-local storage
  without using limited / problematic OS tls slots and without requiring
  expensive blocking thread synchronization.  This class performs no
  blocking waits or spin loops!  (except for memory allocation)

  Thread info is kept in linked lists to facilitate non-blocking threading
  techniques.  A hash table indexed by a hash of the current thread ID
  reduces linear search times.

  When a node is deleted, its thread ID is stripped and its Active field is
  set to zero, meaning it is available to be recycled for other threads.
  Nodes are never removed from the live list or freed while the class is in
  use.  All nodes are freed when the class is destroyed.

  Nodes are only inserted at the front of the list (each list in the hash table).

  The linked list management relies heavily on InterlockedExchange to perform
  atomic node pointer replacements.  There are brief windows of time where
  the linked list may be circular while a two-step insertion takes place.
  During that brief window, other threads traversing the lists may see
  the same node more than once more than once. (pun!) This is fine for what this
  implementation needs.  Don't do anything silly like try to count the
  nodes during a traversal.
}

type
  PThreadInfo = ^TThreadInfo;
  TThreadInfo = record
    Next: PThreadInfo;
    ThreadID: Cardinal;
    Active: Integer;
    RecursionCount: Cardinal;
  end;

  TThreadLocalCounter = class
  private
    FHashTable: array [0..15] of PThreadInfo;
    function HashIndex: Byte;
    function Recycle: PThreadInfo;
  public
    destructor Destroy; override;
    procedure Open(var Thread: PThreadInfo);
    procedure Delete(var Thread: PThreadInfo);
    procedure Close(var Thread: PThreadInfo);
  end;

{$IFDEF MSWINDOWS}

{ TMultiReadExclusiveWriteSynchronizer minimizes thread serialization to gain
  read access to a resource shared among threads while still providing complete
  exclusivity to callers needing write access to the shared resource.
  (multithread shared reads, single thread exclusive write)
  Read locks are allowed while owning a write lock.
  Read locks can be promoted to write locks within the same thread.
  (BeginRead, BeginWrite, EndWrite, EndRead)

  Note: Other threads have an opportunity to modify the protected resource
  when you call BeginWrite before you are granted the write lock, even
  if you already have a read lock open.  Best policy is not to retain
  any info about the protected resource (such as count or size) across a
  write lock.  Always reacquire samples of the protected resource after
  acquiring or releasing a write lock.

  The function result of BeginWrite indicates whether another thread got
  the write lock while the current thread was waiting for the write lock.
  Return value of True means that the write lock was acquired without
  any intervening modifications by other threads.  Return value of False
  means another thread got the write lock while you were waiting, so the
  resource protected by the MREWS object should be considered modified.
  Any samples of the protected resource should be discarded.

  In general, it's better to just always reacquire samples of the protected
  resource after obtaining a write lock.  The boolean result of BeginWrite
  and the RevisionLevel property help cases where reacquiring the samples
  is computationally expensive or time consuming.

  RevisionLevel changes each time a write lock is granted.  You can test
  RevisionLevel for equality with a previously sampled value of the property
  to determine if a write lock has been granted, implying that the protected
  resource may be changed from its state when the original RevisionLevel
  value was sampled.  Do not rely on the sequentiality of the current
  RevisionLevel implementation (it will wrap around to zero when it tops out).
  Do not perform greater than / less than comparisons on RevisionLevel values.
  RevisionLevel indicates only the stability of the protected resource since
  your original sample.  It should not be used to calculate how many
  revisions have been made.
}

type
  TMultiReadExclusiveWriteSynchronizer = class(TInterfacedObject, IReadWriteSync)
  private
    FSentinel: Integer;
    FReadSignal: THandle;
    FWriteSignal: THandle;
    FWaitRecycle: Cardinal;
    FWriteRecursionCount: Cardinal;
    tls: TThreadLocalCounter;
    FWriterID: Cardinal;
    FRevisionLevel: Cardinal;
    procedure BlockReaders;
    procedure UnblockReaders;
    procedure UnblockOneWriter;
    procedure WaitForReadSignal;
    procedure WaitForWriteSignal;
{$IFDEF DEBUG_MREWS}
    procedure Debug(const Msg: string);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
    property RevisionLevel: Cardinal read FRevisionLevel;
  end;
{$ELSE}
type
  TMultiReadExclusiveWriteSynchronizer = TSimpleRWSync;
{$ENDIF}

type
  TMREWSync = TMultiReadExclusiveWriteSynchronizer;  // short form

function GetEnvironmentVariable(const Name: string): string; overload;

{$IFDEF LINUX}
function InterlockedIncrement(var I: Integer): Integer;
function InterlockedDecrement(var I: Integer): Integer;
function InterlockedExchange(var A: Integer; B: Integer): Integer;
function InterlockedExchangeAdd(var A: Integer; B: Integer): Integer;
{$ENDIF}

implementation

{$IFDEF LINUX}
{
        Exceptions raised in methods that are safecall will be filtered
        through the virtual method SafeCallException on the class.  The
        implementation of this method under Linux has the option of setting
        the following thread vars:  SafeCallExceptionMsg, SafeCallExceptionAddr.
        If these are set, then the implementation of SafeCallError here will
        reraise a generic exception based on these.  One might consider actually
        having the SafeCallException implementation store off the exception
        object itself, but this raises the issue that the exception object
        might have to live a long time (if an external application calls a
        Delphi safecall method).  Since an arbitrary exception object could
        be holding large resources hostage, we hold only the string and
        address as a hedge.
}
threadvar
    SafeCallExceptionMsg: String;
    SafeCallExceptionAddr: Pointer;

procedure SetSafeCallExceptionMsg(const Msg: String);
begin
  SafeCallExceptionMsg := Msg;
end;

procedure SetSafeCallExceptionAddr(Addr: Pointer);
begin
  SafeCallExceptionAddr := Addr;
end;

function GetSafeCallExceptionMsg: String;
begin
  Result := SafeCallExceptionMsg;
end;

function GetSafeCallExceptionAddr: Pointer;
begin
  Result := SafeCallExceptionAddr;
end;
{$ENDIF}

{ Utility routines }

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

{$IFDEF PIC}
function GetGOT: Pointer; export;
begin
  asm
        MOV     Result,EBX
  end;
end;
{$ENDIF}

procedure ConvertError(ResString: PResStringRec); local;
begin
  raise EConvertError.CreateRes(ResString);
end;

procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const); local;
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

{$IFDEF MSWINDOWS}
{$EXTERNALSYM CoCreateGuid}
function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(Guid);
end;
//function CreateGUID; external 'ole32.dll' name 'CoCreateGuid';
{$ENDIF}
{$IFDEF LINUX}

{ CreateGUID }

{ libuuid.so implements the tricky code to create GUIDs using the
  MAC address of the network adapter plus other flavor bits.
  libuuid.so is currently distributed with the ext2 file system
  package, but does not depend upon the ext2 file system libraries.
  Ideally, libuuid.so should be distributed separately.

  If you do not have libuuid.so.1 on your Linux distribution, you
  can extract the library from the e2fsprogs RPM.

  Note:  Do not use the generic uuid_generate function in libuuid.so.
  In the current implementation (e2fsprogs-1.19), uuid_generate
  gives preference to generating guids entirely from random number
  streams over generating guids based on the NIC MAC address.
  No matter how "random" a random number generator is, it will
  never produce guids that can be guaranteed unique across all
  systems on the planet.  MAC-address based guids are guaranteed
  unique because the MAC address of the NIC is guaranteed unique
  by the manufacturer.

  For this reason, we call uuid_generate_time instead of the
  generic uuid_generate.  uuid_generate_time constructs the guid
  using the MAC address, and falls back to randomness if no NIC
  can be found.  }

var
  libuuidHandle: Pointer;
  uuid_generate_time: procedure (out Guid: TGUID) cdecl;

function CreateGUID(out Guid: TGUID): HResult;

const
  E_NOTIMPL = HRESULT($80004001);

begin
  Result := E_NOTIMPL;
  if libuuidHandle = nil then
  begin
    libuuidHandle := dlopen('libuuid.so.1', RTLD_LAZY);
    if libuuidHandle = nil then Exit;
    uuid_generate_time := dlsym(libuuidHandle, 'uuid_generate_time');
    if @uuid_generate_time = nil then Exit;
  end;
  uuid_generate_time(Guid);
  Result := 0;
end;
{$ENDIF}


{$IFDEF MSWINDOWS}
function StringFromCLSID(const clsid: TGUID; out psz: PWideChar): HResult; stdcall;
  external 'ole32.dll' name 'StringFromCLSID';
procedure CoTaskMemFree(pv: Pointer); stdcall;
  external 'ole32.dll' name 'CoTaskMemFree';
function CLSIDFromString(psz: PWideChar; out clsid: TGUID): HResult; stdcall;
  external 'ole32.dll' name 'CLSIDFromString';
{$ENDIF MSWINDOWS}

function StringToGUID(const S: string): TGUID;
{$IFDEF MSWINDOWS}
begin
  if not Succeeded(CLSIDFromString(PWideChar(WideString(S)), Result)) then
    ConvertErrorFmt(@SInvalidGUID, [s]);
end;
{$ENDIF}
{$IFDEF LINUX}

  procedure InvalidGUID;
  begin
    ConvertErrorFmt(@SInvalidGUID, [s]);
  end;

  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      InvalidGUID;
      Result := 0;
    end;
  end;

  function HexByte(p: PChar): Char;
  begin
    Result := Char((HexChar(p[0]) shl 4) + HexChar(p[1]));
  end;

var
  i: Integer;
  src, dest: PChar;
begin
  if ((Length(S) <> 38) or (s[1] <> '{')) then InvalidGUID;
  dest := @Result;
  src := PChar(s);
  Inc(src);
  for i := 0 to 3 do
    dest[i] := HexByte(src+(3-i)*2);
  Inc(src, 8);
  Inc(dest, 4);
  if src[0] <> '-' then InvalidGUID;
  Inc(src);
  for i := 0 to 1 do
  begin
    dest^ := HexByte(src+2);
    Inc(dest);
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 4);
    if src[0] <> '-' then InvalidGUID;
    inc(src);
  end;
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  if src[0] <> '-' then InvalidGUID;
  Inc(src);
  for i := 0 to 5 do
  begin
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
end;
{$ENDIF LINUX}

{$IFDEF MSWINDOWS}
function GUIDToString(const GUID: TGUID): string;
var
  P: PWideChar;
begin
  if not Succeeded(StringFromCLSID(GUID, P)) then
    ConvertError(@SInvalidGUID);
  Result := P;
  CoTaskMemFree(P);
end;
{$ENDIF}
{$IFDEF LINUX}
function GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
    GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function IsEqualGUID; external 'ole32.dll' name 'IsEqualGUID';
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@guid1);
  b := PIntegerArray(@guid2);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;
{$ENDIF LINUX}


{ Memory management routines }

function AllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

{ Exit procedure handling }

type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = record
    Next: PExitProcInfo;
    SaveExit: Pointer;
    Proc: TProcedure;
  end;

var
  ExitProcList: PExitProcInfo = nil;

procedure DoExitProc;
var
  P: PExitProcInfo;
  Proc: TProcedure;
begin
  P := ExitProcList;
  ExitProcList := P^.Next;
  ExitProc := P^.SaveExit;
  Proc := P^.Proc;
  Dispose(P);
  Proc;
end;

procedure AddExitProc(Proc: TProcedure);
var
  P: PExitProcInfo;
begin
  New(P);
  P^.Next := ExitProcList;
  P^.SaveExit := ExitProc;
  P^.Proc := Proc;
  ExitProcList := P;
  ExitProc := @DoExitProc;
end;

{ String handling routines }

function NewStr(const S: string): PString;
begin
  if S = '' then Result := NullStr else
  begin
    New(Result);
    Result^ := S;
  end;
end;

procedure DisposeStr(P: PString);
begin
  if (P <> nil) and (P^ <> '') then Dispose(P);
end;

procedure AssignStr(var P: PString; const S: string);
var
  Temp: PString;
begin
  Temp := P;
  P := NewStr(S);
  DisposeStr(Temp);
end;

procedure AppendStr(var Dest: string; const S: string);
begin
  Dest := Dest + S;
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function CompareStr(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    CMP     ECX,ECX
        REPE    CMPSB
        JE      @@4
        MOVZX   EAX,BYTE PTR [ESI-1]
        MOVZX   EDX,BYTE PTR [EDI-1]
@@4:    SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SAR     ECX,2
        JS      @@1     // Negative Length implies identity.
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

function CompareText(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@0
        MOV     EAX,[EAX-4]
@@0:    OR      EDX,EDX
        JE      @@1
        MOV     EDX,[EDX-4]
@@1:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@2
        MOV     ECX,EDX
@@2:    CMP     ECX,ECX
@@3:    REPE    CMPSB
        JE      @@6
        MOV     BL,BYTE PTR [ESI-1]
        CMP     BL,'a'
        JB      @@4
        CMP     BL,'z'
        JA      @@4
        SUB     BL,20H
@@4:    MOV     BH,BYTE PTR [EDI-1]
        CMP     BH,'a'
        JB      @@5
        CMP     BH,'z'
        JA      @@5
        SUB     BH,20H
@@5:    CMP     BL,BH
        JE      @@3
        MOVZX   EAX,BL
        MOVZX   EDX,BH
@@6:    SUB     EAX,EDX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function SameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
        CALL    CompareText
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;

function AnsiUpperCase(const S: string): string;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(S), Len);
  if Len > 0 then CharUpperBuff(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := WideUpperCase(S);
end;
{$ENDIF}

function AnsiLowerCase(const S: string): string;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(S), Len);
  if Len > 0 then CharLowerBuff(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := WideLowerCase(S);
end;
{$ENDIF}

function AnsiCompareStr(const S1, S2: string): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareString(LOCALE_USER_DEFAULT, 0, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := strcoll(PChar(S1), PChar(S2));
{$ENDIF}
end;

function AnsiSameStr(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareStr(S1, S2) = 0;
end;

function AnsiCompareText(const S1, S2: string): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := WideCompareText(S1, S2);
{$ENDIF}
end;

function AnsiSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;

function AnsiStrComp(S1, S2: PChar): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareString(LOCALE_USER_DEFAULT, 0, S1, -1, S2, -1) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := strcoll(S1, S2);
{$ENDIF}
end;

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := AnsiCompareText(S1, S2);
{$ENDIF}
end;

// StrLenLimit:  Scan Src for a null terminator up to MaxLen bytes
function StrLenLimit(Src: PChar; MaxLen: Cardinal): Cardinal;
begin
  if Src = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := MaxLen;
  while (Src^ <> #0) and (Result > 0) do
  begin
    Inc(Src);
    Dec(Result);
  end;
  Result := MaxLen - Result;
end;

{ StrBufLimit: Return a pointer to a buffer that contains no more than MaxLen
  bytes of Src, avoiding heap allocation if possible.
  If clipped Src length is less than MaxLen, return Src.  Allocated = False.
  If clipped Src length is less than StaticBufLen, return StaticBuf with a
    copy of Src.  Allocated = False.
  Otherwise, return a heap allocated buffer with a copy of Src.  Allocated = True.
}
function StrBufLimit(Src: PChar; MaxLen: Cardinal; StaticBuf: PChar;
  StaticBufLen: Cardinal; var Allocated: Boolean): PChar;
var
  Len: Cardinal;
begin
  Len := StrLenLimit(Src, MaxLen);
  Allocated := False;
  if Len < MaxLen then
    Result := Src
  else
  begin
    if Len < StaticBufLen then
      Result := StaticBuf
    else
    begin
      GetMem(Result, Len+1);
      Allocated := True;
    end;
    Move(Src^, Result^, Len);
    Result[Len] := #0;
  end;
end;

function InternalAnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal; CaseSensitive: Boolean): Integer;
var
  Buf1, Buf2: array [0..4095] of Char;
  P1, P2: PChar;
  Allocated1, Allocated2: Boolean;
begin
  // glibc has no length-limited strcoll!
  P1 := nil;
  P2 := nil;
  Allocated1 := False;
  Allocated2 := False;
  try
    P1 := StrBufLimit(S1, MaxLen, Buf1, High(Buf1), Allocated1);
    P2 := StrBufLimit(S2, MaxLen, Buf2, High(Buf2), Allocated2);
    if CaseSensitive then
      Result := AnsiStrComp(P1, P2)
    else
      Result := AnsiStrIComp(P1, P2);
  finally
    if Allocated1 then
      FreeMem(P1);
    if Allocated2 then
      FreeMem(P2);
  end;
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0,
    S1, MaxLen, S2, MaxLen) - 2;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := InternalAnsiStrLComp(S1, S2, MaxLen, True);
end;
{$ENDIF}

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := InternalAnsiStrLComp(S1, S2, MaxLen, False);
{$ENDIF}
end;

function AnsiStrLower(Str: PChar): PChar;
{$IFDEF MSWINDOWS}
begin
  CharLower(Str);
  Result := Str;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  Temp: WideString;
  Squish: AnsiString;
  I: Integer;
begin
  Temp := Str;     // expand and copy multibyte to widechar
  for I := 1 to Length(Temp) do
    Temp[I] := WideChar(towlower(UCS4Char(Temp[I])));
  Squish := Temp;  // reduce and copy widechar to multibyte

  if Cardinal(Length(Squish)) > StrLen(Str) then
    raise ERangeError.CreateRes(@SRangeError);

  Move(Squish[1], Str^, Length(Squish));
  Result := Str;
end;
{$ENDIF}

function AnsiStrUpper(Str: PChar): PChar;
{$IFDEF MSWINDOWS}
begin
  CharUpper(Str);
  Result := Str;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  Temp: WideString;
  Squish: AnsiString;
  I: Integer;
begin
  Temp := Str;    // expand and copy multibyte to widechar
  for I := 1 to Length(Temp) do
    Temp[I] := WideChar(towupper(UCS4Char(Temp[I])));
  Squish := Temp;  // reduce and copy widechar to multibyte
  if Cardinal(Length(Squish)) > StrLen(Str) then
    raise ERangeError.CreateRes(@SRangeError);

  Move(Squish[1], Str^, Length(Squish));
  Result := Str;
end;
{$ENDIF}

function WideUpperCase(const S: WideString): WideString;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I: Integer;
  P: PWideChar;
begin
  SetLength(Result, Length(S));
  P := @Result[1];
  for I := 1 to Length(S) do
    P[I-1] := WideChar(towupper(UCS4Char(S[I])));
end;
{$ENDIF}

function WideLowerCase(const S: WideString): WideString;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I: Integer;
  P: PWideChar;
begin
  SetLength(Result, Length(S));
  P := @Result[1];
  for I := 1 to Length(S) do
    P[I-1] := WideChar(towlower(UCS4Char(S[I])));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function DumbItDownFor95(const S1, S2: WideString; CmpFlags: Integer): Integer;
var
  a1, a2: AnsiString;
begin
  a1 := s1;
  a2 := s2;
  Result := CompareStringA(LOCALE_USER_DEFAULT, CmpFlags, PChar(a1), Length(a1),
    PChar(a2), Length(a2)) - 2;
end;
{$ENDIF}

function WideCompareStr(const S1, S2: WideString): Integer;
{$IFDEF MSWINDOWS}
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(S1), Length(S1),
    PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED: Result := DumbItDownFor95(S1, S2, 0);
  else
    RaiseLastOSError;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  UCS4_S1, UCS4_S2: UCS4String;
begin
  UCS4_S1 := WideStringToUCS4String(S1);
  UCS4_S2 := WideStringToUCS4String(S2);
  // glibc 2.1.2 / 2.1.3 implementations of wcscoll() and wcsxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  SetLastError(0);
  Result := wcscoll(PUCS4Chars(UCS4_S1), PUCS4Chars(UCS4_S2));
  if GetLastError <> 0 then
    RaiseLastOSError;
end;
{$ENDIF}

function WideSameStr(const S1, S2: WideString): Boolean;
begin
  Result := WideCompareStr(S1, S2) = 0;
end;

function WideCompareText(const S1, S2: WideString): Integer;
begin
{$IFDEF MSWINDOWS}
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED: Result := DumbItDownFor95(S1, S2, NORM_IGNORECASE);
  else
    RaiseLastOSError;
  end;
{$ENDIF}
{$IFDEF LINUX}
  Result := WideCompareStr(WideUpperCase(S1), WideUpperCase(S2));
{$ENDIF}
end;

function WideSameText(const S1, S2: WideString): Boolean;
begin
  Result := WideCompareText(S1, S2) = 0;
end;

function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function Trim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimLeft(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function TrimRight(const S: WideString): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function QuotedStr(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '''' then Insert('''', Result, I);
  Result := '''' + Result + '''';
end;

function AnsiQuotedStr(const S: string; Quote: Char): string;
var
  P, Src, Dest: PChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := AnsiStrScan(PChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := AnsiStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := AnsiStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := AnsiStrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
var
  P, Dest: PChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := AnsiStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := AnsiStrScan(Src, Quote);
  end;
  if Src = nil then Src := StrEnd(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PChar(Result);
    Src := AnsiStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := AnsiStrScan(Src, Quote);
    end;
    if Src = nil then Src := StrEnd(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Source, SourceEnd, Dest: PChar;
  DestLen: Integer;
  L: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  DestLen := Length(S);
  while Source < SourceEnd do
  begin
    case Source^ of
      #10:
        if Style = tlbsCRLF then
          Inc(DestLen);
      #13:
        if Style = tlbsCRLF then
          if Source[1] = #10 then
            Inc(Source)
          else
            Inc(DestLen)
        else
          if Source[1] = #10 then
            Dec(DestLen);
    else
      if Source^ in LeadBytes then
      begin
        Source := StrNextChar(Source);
        continue;
      end;
    end;
    Inc(Source);
  end;
  if DestLen = Length(Source) then
    Result := S
  else
  begin
    Source := Pointer(S);
    SetString(Result, nil, DestLen);
    Dest := Pointer(Result);
    while Source < SourceEnd do
      case Source^ of
        #10:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
          end;
        #13:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
            if Source^ = #10 then Inc(Source);
          end;
      else
        if Source^ in LeadBytes then
        begin
          L := StrCharLength(Source);
          Move(Source^, Dest^, L);
          Inc(Dest, L);
          Inc(Source, L);
          continue;
        end;
        Dest^ := Source^;
        Inc(Dest);
        Inc(Source);
      end;
  end;
end;

function IsValidIdent(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

procedure CvtInt;
{ IN:
    EAX:  The integer value to be converted to text
    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[16]
    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted text (not start of buffer)
    ECX:  Length of converted text
}
asm
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        DEC     ESI
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,('A'-'0')-10
@D2:    MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX],AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AL
@D5:
end;

procedure CvtIntW;
{ IN:
    EAX:  The integer value to be converted to text
    ESI:  Ptr to the right-hand side of the widechar output buffer:  LEA ESI, WStrBuf[32]
    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted widechar text (not start of buffer)
    ECX:  Character length of converted text
}
asm
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AX,'-'
        MOV     [ESI-2],AX
        SUB     ESI, 2
        INC     ECX
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        ADD     DX,'0'
        SUB     ESI,2
        CMP     DX,'0'+10
        JB      @D2
        ADD     DX,('A'-'0')-10
@D2:    MOV     [ESI],DX
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SHR     ECX, 1
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        SUB     ESI,EDX
        MOV     AX,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX*2],AX
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AX
@D5:
end;

function IntToStr(Value: Integer): string;
//  FmtStr(Result, '%d', [Value]);
asm
        PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 16
        XOR     ECX, ECX       // base: 0 for signed decimal
        PUSH    EDX            // result ptr
        XOR     EDX, EDX       // zero filled field width: 0 for no leading zeros
        CALL    CvtInt
        MOV     EDX, ESI
        POP     EAX            // result ptr
        CALL    System.@LStrFromPCharLen
        ADD     ESP, 16
        POP     ESI
end;

procedure CvtInt64W;
{ IN:
    EAX:  Address of the int64 value to be converted to text
    ESI:  Ptr to the right-hand side of the widechar output buffer:  LEA ESI, WStrBuf[32]
    ECX:  Base for conversion: 10 or 16
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted widechar text (not start of buffer)
    ECX:  Character length of converted text
}
asm
        OR      CL, CL
        JNZ     @start
        MOV     ECX, 10
        TEST    [EAX + 4], $80000000
        JZ      @start
        PUSH    [EAX + 4]
        PUSH    [EAX]
        MOV     EAX, ESP
        NEG     [ESP]              // negate the value
        ADC     [ESP + 4],0
        NEG     [ESP + 4]
        CALL    @start
        INC     ECX
        MOV     [ESI-2].Word, '-'
        SUB     ESI, 2
        ADD     ESP, 8
        JMP     @done

@start:
        PUSH    ESI
        SUB     ESP, 4
        FNSTCW  [ESP+2].Word       // save
        FNSTCW  [ESP].Word         // scratch
        OR      [ESP].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP].Word

        MOV     [ESP].Word, CX
        FLD1
        TEST    [EAX + 4], $80000000 // test for negative
        JZ      @ld1                 // FPU doesn't understand unsigned ints
        PUSH    [EAX + 4]            // copy value before modifying
        PUSH    [EAX]
        AND     [ESP + 4], $7FFFFFFF // clear the sign bit
        PUSH    $7FFFFFFF
        PUSH    $FFFFFFFF
        FILD    [ESP + 8].QWord     // load value
        FILD    [ESP].QWord
        FADD    ST(0), ST(2)        // Add 1.  Produces unsigned $80000000 in ST(0)
        FADDP   ST(1), ST(0)        // Add $80000000 to value to replace the sign bit
        ADD     ESP, 16
        JMP     @ld2
@ld1:
        FILD    [EAX].QWord         // value
@ld2:
        FILD    [ESP].Word          // base
        FLD     ST(1)
@loop:
        SUB     ESI, 2
        FPREM                       // accumulator mod base
        FISTP   [ESI].Word
        FDIV    ST(1), ST(0)        // accumulator := acumulator / base
        MOV     AX, [ESI].Word      // overlap long division op with int ops
        ADD     AX, '0'
        CMP     AX, '0'+10
        JB      @store
        ADD     AX, ('A'-'0')-10
@store:
        MOV     [ESI].Word, AX
        FLD     ST(1)           // copy accumulator
        FCOM    ST(3)           // if accumulator >= 1.0 then loop
        FSTSW   AX
        SAHF
        JAE @loop

        FLDCW   [ESP+2].Word
        ADD     ESP,4

        FFREE   ST(3)
        FFREE   ST(2)
        FFREE   ST(1);
        FFREE   ST(0);

@zeropad:
        POP     ECX             // original ESI
        SUB     ECX,ESI
        SHR     ECX, 1          // ECX = char length of converted string
        OR      EDX,EDX
        JS      @done
        SUB     EDX,ECX
        JBE     @done           // output longer than field width = no pad
        SUB     ESI,EDX
        MOV     AX,'0'
        SUB     ESI,EDX
        ADD     ECX,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX*2].Word,AX
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI].Word,AX
@done:
end;

procedure CvtInt64;
{ IN:
    EAX:  Address of the int64 value to be converted to text
    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[32]
    ECX:  Base for conversion: 0 for signed decimal, or 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted text (not start of buffer)
    ECX:  Byte length of converted text
}
asm
        OR      CL, CL
        JNZ     @start             // CL = 0  => signed integer conversion
        MOV     ECX, 10
        TEST    [EAX + 4], $80000000
        JZ      @start
        PUSH    [EAX + 4]
        PUSH    [EAX]
        MOV     EAX, ESP
        NEG     [ESP]              // negate the value
        ADC     [ESP + 4],0
        NEG     [ESP + 4]
        CALL    @start             // perform unsigned conversion
        MOV     [ESI-1].Byte, '-'  // tack on the negative sign
        DEC     ESI
        INC     ECX
        ADD     ESP, 8
        RET

@start:   // perform unsigned conversion
        PUSH    ESI
        SUB     ESP, 4
        FNSTCW  [ESP+2].Word     // save
        FNSTCW  [ESP].Word       // scratch
        OR      [ESP].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP].Word

        MOV     [ESP].Word, CX
        FLD1
        TEST    [EAX + 4], $80000000 // test for negative
        JZ      @ld1                 // FPU doesn't understand unsigned ints
        PUSH    [EAX + 4]            // copy value before modifying
        PUSH    [EAX]
        AND     [ESP + 4], $7FFFFFFF // clear the sign bit
        PUSH    $7FFFFFFF
        PUSH    $FFFFFFFF
        FILD    [ESP + 8].QWord     // load value
        FILD    [ESP].QWord
        FADD    ST(0), ST(2)        // Add 1.  Produces unsigned $80000000 in ST(0)
        FADDP   ST(1), ST(0)        // Add $80000000 to value to replace the sign bit
        ADD     ESP, 16
        JMP     @ld2
@ld1:
        FILD    [EAX].QWord         // value
@ld2:
        FILD    [ESP].Word          // base
        FLD     ST(1)
@loop:
        DEC     ESI
        FPREM                       // accumulator mod base
        FISTP   [ESP].Word
        FDIV    ST(1), ST(0)        // accumulator := acumulator / base
        MOV     AL, [ESP].Byte      // overlap long FPU division op with int ops
        ADD     AL, '0'
        CMP     AL, '0'+10
        JB      @store
        ADD     AL, ('A'-'0')-10
@store:
        MOV     [ESI].Byte, AL
        FLD     ST(1)           // copy accumulator
        FCOM    ST(3)           // if accumulator >= 1.0 then loop
        FSTSW   AX
        SAHF
        JAE @loop

        FLDCW   [ESP+2].Word
        ADD     ESP,4

        FFREE   ST(3)
        FFREE   ST(2)
        FFREE   ST(1);
        FFREE   ST(0);

        POP     ECX             // original ESI
        SUB     ECX, ESI        // ECX = length of converted string
        SUB     EDX,ECX
        JBE     @done           // output longer than field width = no pad
        SUB     ESI,EDX
        MOV     AL,'0'
        ADD     ECX,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX].Byte,AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI].Byte,AL
@done:
end;

function IntToStr(Value: Int64): string;
//  FmtStr(Result, '%d', [Value]);
asm
        PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 32        // 32 chars
        XOR     ECX, ECX       // base 10 signed
        PUSH    EAX            // result ptr
        XOR     EDX, EDX       // zero filled field width: 0 for no leading zeros
        LEA     EAX, Value;
        CALL    CvtInt64

        MOV     EDX, ESI
        POP     EAX            // result ptr
        CALL    System.@LStrFromPCharLen
        ADD     ESP, 32
        POP     ESI
end;

function IntToHex(Value: Integer; Digits: Integer): string;
//  FmtStr(Result, '%.*x', [Digits, Value]);
asm
        CMP     EDX, 32        // Digits < buffer length?
        JBE     @A1
        XOR     EDX, EDX
@A1:    PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 32
        PUSH    ECX            // result ptr
        MOV     ECX, 16        // base 16     EDX = Digits = field width
        CALL    CvtInt
        MOV     EDX, ESI
        POP     EAX            // result ptr
        CALL    System.@LStrFromPCharLen
        ADD     ESP, 32
        POP     ESI
end;

function IntToHex(Value: Int64; Digits: Integer): string;
//  FmtStr(Result, '%.*x', [Digits, Value]);
asm
        CMP     EAX, 32        // Digits < buffer length?
        JLE     @A1
        XOR     EAX, EAX
@A1:    PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 32        // 32 chars
        MOV     ECX, 16        // base 10
        PUSH    EDX            // result ptr
        MOV     EDX, EAX       // zero filled field width: 0 for no leading zeros
        LEA     EAX, Value;
        CALL    CvtInt64

        MOV     EDX, ESI
        POP     EAX            // result ptr
        CALL    System.@LStrFromPCharLen
        ADD     ESP, 32
        POP     ESI
end;

function StrToInt(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then ConvertErrorFmt(@SInvalidInteger, [S]);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function StrToInt64(const S: string): Int64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then ConvertErrorFmt(@SInvalidInteger, [S]);
end;

function StrToInt64Def(const S: string; const Default: Int64): Int64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function TryStrToInt64(const S: string; out Value: Int64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: string;
  end;

function EnumStringModules(Instance: Longint; Data: Pointer): Boolean;
{$IFDEF MSWINDOWS}
var
  Buffer: array [0..1023] of char;
begin
  with PStrData(Data)^ do
  begin
    SetString(Str, Buffer,
      LoadString(Instance, Ident, Buffer, sizeof(Buffer)));
    Result := Str = '';
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  rs: TResStringRec;
  Module: HModule;
begin
  Module := Instance;
  rs.Module := @Module;
  with PStrData(Data)^ do
  begin
    rs.Identifier := Ident;
    Str := LoadResString(@rs);
    Result := Str = '';
  end;
end;
{$ENDIF}

function FindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.Str;
end;

function LoadStr(Ident: Integer): string;
begin
  Result := FindStringResource(Ident);
end;

function FmtLoadStr(Ident: Integer; const Args: array of const): string;
begin
  FmtStr(Result, FindStringResource(Ident), Args);
end;

{ File management routines }

function FileOpen(const FileName: string; Mode: LongWord): Integer;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    Result := Integer(CreateFile(PChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;
{$ENDIF}
{$IFDEF LINUX}
const
  ShareMode: array[0..fmShareDenyNone shr 4] of Byte = (
    0,        //No share mode specified
    F_WRLCK,  //fmShareExclusive
    F_RDLCK,  //fmShareDenyWrite
    0);       //fmShareDenyNone
var
  FileHandle, Tvar: Integer;
  LockVar: TFlock;
  smode: Byte;
begin
  Result := -1;
  if FileExists(FileName) and
     ((Mode and 3) <= fmOpenReadWrite) and
     ((Mode and $F0) <= fmShareDenyNone) then
  begin
    FileHandle := open(PChar(FileName), (Mode and 3), FileAccessRights);

    if FileHandle = -1 then  Exit;

    smode := Mode and $F0 shr 4;
    if ShareMode[smode] <> 0 then
    begin
      with LockVar do
      begin
        l_whence := SEEK_SET;
        l_start := 0;
        l_len := 0;
        l_type := ShareMode[smode];
      end;
      Tvar :=  fcntl(FileHandle, F_SETLK, LockVar);
      if Tvar = -1 then
      begin
        __close(FileHandle);
        Exit;
      end;
    end;
    Result := FileHandle;
  end;
end;
{$ENDIF}

function FileCreate(const FileName: string): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := Integer(CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := FileCreate(FileName, FileAccessRights);
end;
{$ENDIF}

function FileCreate(const FileName: string; Rights: Integer): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := FileCreate(FileName);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := Integer(open(PChar(FileName), O_RDWR or O_CREAT or O_TRUNC, Rights));
end;
{$ENDIF}

function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;
begin
{$IFDEF MSWINDOWS}
  if not ReadFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
{$ENDIF}
{$IFDEF LINUX}
  Result := __read(Handle, Buffer, Count);
{$ENDIF}
end;

function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;
begin
{$IFDEF MSWINDOWS}
  if not WriteFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
{$ENDIF}
{$IFDEF LINUX}
  Result := __write(Handle, Buffer, Count);
{$ENDIF}
end;

function FileSeek(Handle, Offset, Origin: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := SetFilePointer(THandle(Handle), Offset, nil, Origin);
{$ENDIF}
{$IFDEF LINUX}
  Result := __lseek(Handle, Offset, Origin);
{$ENDIF}
end;

function FileSeek(Handle: Integer; const Offset: Int64; Origin: Integer): Int64;
{$IFDEF MSWINDOWS}
begin
  Result := Offset;
  Int64Rec(Result).Lo := SetFilePointer(THandle(Handle), Int64Rec(Result).Lo,
    @Int64Rec(Result).Hi, Origin);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  Temp: Integer;
begin
  Temp := Offset;  // allow for range-checking
  Result := FileSeek(Handle, Temp, Origin);
end;
{$ENDIF}

procedure FileClose(Handle: Integer);
begin
{$IFDEF MSWINDOWS}
  CloseHandle(THandle(Handle));
{$ENDIF}
{$IFDEF LINUX}
  __close(Handle); // No need to unlock since all locks are released on close.
{$ENDIF}
end;

function FileAge(const FileName: string): Integer;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if stat(PChar(FileName), st) = 0 then
    Result := st.st_mtime
  else
    Result := -1;
end;
{$ENDIF}

function FileExists(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := FileAge(FileName) <> -1;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := euidaccess(PChar(FileName), F_OK) = 0;
end;
{$ENDIF}

function DirectoryExists(const Directory: string): Boolean;
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if stat(PChar(Directory), st) = 0 then
    Result := S_ISDIR(st.st_mode)
  else
    Result := False;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
	Code: Cardinal;
begin
	Code := GetFileAttributes(PChar(Directory));
	Result := (Code <> High(Code)) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}

function ForceDirectories(Dir: string): Boolean;
var
  E: EInOutError;
begin
  Result := True;
  if Dir = '' then
  begin
    E := EInOutError.CreateRes(@SCannotCreateDir);
    E.ErrorCode := 3;
    raise E;
  end;
  Dir := ExcludeTrailingPathDelimiter(Dir);
{$IFDEF MSWINDOWS}
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
{$ENDIF}
{$IFDEF LINUX}
  if (Dir = '') or DirectoryExists(Dir) then Exit;
{$ENDIF}
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function FileGetDate(Handle: Integer): Integer;
{$IFDEF MSWINDOWS}
var
  FileTime, LocalFileTime: TFileTime;
begin
  if GetFileTime(THandle(Handle), nil, nil, @FileTime) and
    FileTimeToLocalFileTime(FileTime, LocalFileTime) and
    FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
      LongRec(Result).Lo) then Exit;
  Result := -1;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if fstat(Handle, st) = 0 then
    Result := st.st_mtime
  else
    Result := -1;
end;
{$ENDIF}

function FileSetDate(const FileName: string; Age: Integer): Integer;
{$IFDEF MSWINDOWS}
var
  f: THandle;
begin
  f := FileOpen(FileName, fmOpenWrite);
  if f = THandle(-1) then
    Result := GetLastError
  else
  begin
    Result := FileSetDate(f, Age);
    FileClose(f);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  ut: TUTimeBuffer;
begin
  Result := 0;
  ut.actime := Age;
  ut.modtime := Age;
  if utime(PChar(FileName), @ut) = -1 then
    Result := GetLastError;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function FileSetDate(Handle: Integer; Age: Integer): Integer;
var
  LocalFileTime, FileTime: TFileTime;
begin
  Result := 0;
  if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
    LocalFileTimeToFileTime(LocalFileTime, FileTime) and
    SetFileTime(Handle, nil, nil, @FileTime) then Exit;
  Result := GetLastError;
end;

function FileGetAttr(const FileName: string): Integer;
begin
  Result := GetFileAttributes(PChar(FileName));
end;

function FileSetAttr(const FileName: string; Attr: Integer): Integer;
begin
  Result := 0;
  if not SetFileAttributes(PChar(FileName), Attr) then
    Result := GetLastError;
end;
{$ENDIF}

function FileIsReadOnly(const FileName: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (GetFileAttributes(PChar(FileName)) and faReadOnly) <> 0;
{$ENDIF}
{$IFDEF LINUX}
  Result := (euidaccess(PChar(FileName), R_OK) = 0) and
    (euidaccess(PChar(FileName), W_OK) <> 0);
{$ENDIF}
end;

function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  Flags: Integer;
begin
  Result := False;
  Flags := GetFileAttributes(PChar(FileName));
  if Flags = -1 then Exit;
  if ReadOnly then
    Flags := Flags or faReadOnly
  else
    Flags := Flags and not faReadOnly;
  Result := SetFileAttributes(PChar(FileName), Flags);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  st: TStatBuf;
  Flags: Integer;
begin
  Result := False;
  if stat(PChar(FileName), st) <> 0 then Exit;
  if ReadOnly then
    Flags := st.st_mode and not (S_IWUSR or S_IWGRP or S_IWOTH)
  else
    Flags := st.st_mode or (S_IWUSR or S_IWGRP or S_IWOTH);
  Result := chmod(PChar(FileName), Flags) = 0;
end;
{$ENDIF}


function FindMatchingFile(var F: TSearchRec): Integer;
{$IFDEF MSWINDOWS}
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFile(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := Integer(FindData.nFileSizeLow);
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  PtrDirEnt: PDirEnt;
  Scratch: TDirEnt;
  StatBuf: TStatBuf;
  LinkStatBuf: TStatBuf;
  FName: string;
  Attr: Integer;
  Mode: mode_t;
begin
  Result := -1;
  PtrDirEnt := nil;
  if readdir_r(F.FindHandle, @Scratch, PtrDirEnt) <> 0 then
    Exit;
  while PtrDirEnt <> nil do
  begin
    if fnmatch(PChar(F.Pattern), PtrDirEnt.d_name, 0) = 0 then
    begin   // F.PathOnly must include trailing backslash
      FName := F.PathOnly + string(PtrDirEnt.d_name);

      if lstat(PChar(FName), StatBuf) = 0 then
      begin
        Attr := 0;
        Mode := StatBuf.st_mode;

        if S_ISDIR(Mode) then
          Attr := Attr or faDirectory
        else
        if not S_ISREG(Mode) then  // directories shouldn't be treated as system files
        begin
          if S_ISLNK(Mode) then
          begin
            Attr := Attr or faSymLink;
            if (stat(PChar(FName), LinkStatBuf) = 0) and
              S_ISDIR(LinkStatBuf.st_mode) then
                Attr := Attr or faDirectory
          end;
          Attr := Attr or faSysFile;
        end;

        if (PtrDirEnt.d_name[0] = '.') and (PtrDirEnt.d_name[1] <> #0) then
        begin
          if not ((PtrDirEnt.d_name[1] = '.') and (PtrDirEnt.d_name[2] = #0)) then
            Attr := Attr or faHidden;
        end;

        if euidaccess(PChar(FName), W_OK) <> 0 then
          Attr := Attr or faReadOnly;

        if Attr and F.ExcludeAttr = 0 then
        begin
          F.Size := StatBuf.st_size;
          F.Attr := Attr;
          F.Mode := StatBuf.st_mode;
          F.Name := PtrDirEnt.d_name;
          F.Time := StatBuf.st_mtime;
          Result := 0;
          Break;
        end;
      end;
    end;
    Result := -1;
    if readdir_r(F.FindHandle, @Scratch, PtrDirEnt) <> 0 then
      Break;
  end // End of While
end;
{$ENDIF}

function FindFirst(const Path: string; Attr: Integer;
  var  F: TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
{$IFDEF MSWINDOWS}
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFile(PChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFile(F);
    if Result <> 0 then FindClose(F);
  end else
    Result := GetLastError;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.PathOnly := ExtractFilePath(Path);
  F.Pattern := ExtractFileName(Path);
  if F.PathOnly = '' then
    F.PathOnly := IncludeTrailingPathDelimiter(GetCurrentDir);

  F.FindHandle := opendir(PChar(F.PathOnly));
  if F.FindHandle <> nil then
  begin
    Result := FindMatchingFile(F);
    if Result <> 0 then
      FindClose(F);
  end
  else
    Result:= GetLastError;
end;
{$ENDIF}

function FindNext(var F: TSearchRec): Integer;
begin
{$IFDEF MSWINDOWS}
  if FindNextFile(F.FindHandle, F.FindData) then
    Result := FindMatchingFile(F) else
    Result := GetLastError;
{$ENDIF}
{$IFDEF LINUX}
  Result := FindMatchingFile(F);
{$ENDIF}
end;

procedure FindClose(var F: TSearchRec);
begin
{$IFDEF MSWINDOWS}
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if F.FindHandle <> nil then
  begin
    closedir(F.FindHandle);
    F.FindHandle := nil;
  end;
{$ENDIF}
end;

function DeleteFile(const FileName: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.DeleteFile(PChar(FileName));
{$ENDIF}
{$IFDEF LINUX}
  Result := unlink(PChar(FileName)) <> -1;
{$ENDIF}
end;

function RenameFile(const OldName, NewName: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := MoveFile(PChar(OldName), PChar(NewName));
{$ENDIF}
{$IFDEF LINUX}
  Result := __rename(PChar(OldName), PChar(NewName)) = 0;
{$ENDIF}
end;

function AnsiStrLastChar(P: PChar): PChar;
var
  LastByte: Integer;
begin
  LastByte := StrLen(P) - 1;
  Result := @P[LastByte];
{$IFDEF MSWINDOWS}
  if StrByteType(P, LastByte) = mbTrailByte then Dec(Result);
{$ENDIF}
{$IFDEF LINUX}
  while StrByteType(P, Result - P) = mbTrailByte do Dec(Result);
{$ENDIF}
end;

function AnsiLastChar(const S: string): PChar;
var
  LastByte: Integer;
begin
  LastByte := Length(S);
  if LastByte <> 0 then
  begin
    while ByteType(S, LastByte) = mbTrailByte do Dec(LastByte);
    Result := @S[LastByte];
  end
  else
    Result := nil;
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
{$IFDEF MSWINDOWS}
      if (ByteType(S, Result) = mbTrailByte) then
        Dec(Result)
      else
        Exit;
{$ENDIF}
{$IFDEF LINUX}
    begin
      if (ByteType(S, Result) <> mbTrailByte) then
        Exit;
      Dec(Result);
      while ByteType(S, Result) = mbTrailByte do Dec(Result);
    end;
{$ENDIF}
    Dec(Result);
  end;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim,Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDir(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not IsDelimiter( PathDelim + DriveDelim, FileName, I-1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := '';  // Linux doesn't support drive letters
end;
{$ENDIF}

function ExtractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function ExpandFileName(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  FName: PChar;
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer, GetFullPathName(PChar(FileName), SizeOf(Buffer),
    Buffer, FName));
end;
{$ENDIF}

{$IFDEF LINUX}
function ExpandTilde(const InString: string): string;
var
  W: wordexp_t;
  SpacePos: Integer;
  PostSpaceStr: string;
begin
  Result := InString;
  SpacePos := AnsiPos(' ', Result);  // only expand stuff up to the first space in the filename
  if SpacePos > 0 then               // then just add the space and the rest of the string
    PostSpaceStr := Copy(Result, SpacePos, Length(Result) - (SpacePos-1));
  case wordexp(PChar(Result), W, WRDE_NOCMD) of
    0:             // success
      begin
        Result := PChar(W.we_wordv^);
        wordfree(W);
      end;
    WRDE_NOSPACE:  // error, but W may be partially allocated
      wordfree(W);
  end;
  if PostSpaceStr <> '' then
    Result := Result + PostSpaceStr;
end;

var
  I, J: Integer;
  LastWasPathDelim: Boolean;
  TempName: string;
begin
  Result := '';
  if Length(Filename) = 0 then Exit;

  if FileName[1] = PathDelim then
    TempName := FileName
  else
  begin
    TempName := FileName;
    if FileName[1] = '~' then
      TempName := ExpandTilde(TempName)
    else
      TempName := IncludeTrailingPathDelimiter(GetCurrentDir) + TempName;
  end;

  I := 1;
  J := 1;

  LastWasPathDelim := False;

  while I <= Length(TempName) do
  begin
    case TempName[I] of
      PathDelim:
        if J < I then
        begin
          // Check for consecutive 'PathDelim' characters and skip them if present
          if (I = 1) or (TempName[I - 1] <> PathDelim) then
            Result := Result + Copy(TempName, J, I - J);
          J := I;
          // Set a flag indicating that we just processed a path delimiter
          LastWasPathDelim := True;
        end;
      '.':
        begin
          // If the last character was a path delimiter then this '.' is
          // possibly a relative path modifier
          if LastWasPathDelim then
          begin
            // Check if the path ends in a '.'
            if I < Length(TempName) then
            begin
              // If the next character is another '.' then this may be a relative path
              // except if there is another '.' after that one.  In this case simply
              // treat this as just another filename.
              if (TempName[I + 1] = '.') and
                ((I + 1 >= Length(TempName)) or (TempName[I + 2] <> '.')) then
              begin
                // Don't attempt to backup past the Root dir
                if Length(Result) > 1 then
                  // For the purpose of this excercise, treat the last dir as a
                  // filename so we can use this function to remove it
                  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
                J := I;
              end
              // Simply skip over and ignore any 'current dir' constrcucts, './'
              // or the remaining './' from a ../ constrcut.
              else if TempName[I + 1] = PathDelim then
              begin
                Result := IncludeTrailingPathDelimiter(Result);
                if TempName[I] in LeadBytes then
                  Inc(I, StrCharLength(@TempName[I]))
                else
                  Inc(I);
                J := I + 1;
              end else
                // If any of the above tests fail, then this is not a 'current dir' or
                // 'parent dir' construct so just clear the state and continue.
                LastWasPathDelim := False;
            end else
            begin
              // Don't let the expanded path end in a 'PathDelim' character
              Result := ExcludeTrailingPathDelimiter(Result);
              J := I + 1;
            end;
          end;
        end;
    else
      LastWasPathDelim := False;
    end;
    if TempName[I] in LeadBytes then
      Inc(I, StrCharLength(@TempName[I]))
    else
      Inc(I);
  end;
  // This will finally append what is left
  if (I - J > 1) or (TempName[I] <> PathDelim) then
    Result := Result + Copy(TempName, J, I - J);
end;
{$ENDIF}

function ExpandFileNameCase(const FileName: string;
  out MatchFound: TFilenameCaseMatch): string;
var
  SR: TSearchRec;
  FullPath, Name: string;
  Temp: Integer;
  FoundOne: Boolean;
  {$IFDEF LINUX}
  Scans: Byte;
  FirstLetter, TestLetter: string;
  {$ENDIF}
begin
  Result := ExpandFileName(FileName);
  FullPath := ExtractFilePath(Result);
  Name := ExtractFileName(Result);
  MatchFound := mkNone;

  // if FullPath is not the root directory  (portable)
  if not SameFileName(FullPath, IncludeTrailingPathDelimiter(ExtractFileDrive(FullPath))) then
  begin  // Does the path need case-sensitive work?
    Temp := FindFirst(FullPath, faAnyFile, SR);
    FindClose(SR);   // close search before going recursive
    if Temp <> 0 then
    begin
      FullPath := ExcludeTrailingPathDelimiter(FullPath);
      FullPath := ExpandFileNameCase(FullPath, MatchFound);
      if MatchFound = mkNone then
        Exit;    // if we can't find the path, we certainly can't find the file!
      FullPath := IncludeTrailingPathDelimiter(FullPath);
    end;
  end;

  // Path is validated / adjusted.  Now for the file itself
  try
    if FindFirst(FullPath + Name, faAnyFile, SR)= 0 then    // exact match on filename
    begin
      if not (MatchFound in [mkSingleMatch, mkAmbiguous]) then  // path might have been inexact
        MatchFound := mkExactMatch;
      Result := FullPath + SR.Name;
      Exit;
    end;
  finally
    FindClose(SR);
  end;

  FoundOne := False; // Windows should never get to here except for file-not-found

{$IFDEF LINUX}

{ Scan the directory.
  To minimize the number of filenames tested, scan the directory
  using upper/lowercase first letter + wildcard.
  This results in two scans of the directory (particularly on Linux) but
  vastly reduces the number of times we have to perform an expensive
  locale-charset case-insensitive string compare.  }

  // First, scan for lowercase first letter
  FirstLetter := AnsiLowerCase(Name[1]);
  for Scans := 0 to 1 do
  begin
    Temp := FindFirst(FullPath + FirstLetter + '*', faAnyFile, SR);
    while Temp = 0 do
    begin
      if AnsiSameText(SR.Name, Name) then
      begin
        if FoundOne then
        begin  // this is the second match
          MatchFound := mkAmbiguous;
          Exit;
        end
        else
        begin
          FoundOne := True;
          Result := FullPath + SR.Name;
        end;
      end;
      Temp := FindNext(SR);
    end;
    FindClose(SR);
    TestLetter := AnsiUpperCase(Name[1]);
    if TestLetter = FirstLetter then Break;
    FirstLetter := TestLetter;
  end;
{$ENDIF}

  if MatchFound <> mkAmbiguous then
  begin
    if FoundOne then
      MatchFound := mkSingleMatch
    else
      MatchFound := mkNone;
  end;
end;

{$IFDEF MSWINDOWS}
function GetUniversalName(const FileName: string): string;
type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..MaxInt div SizeOf(TNetResource) - 1] of TNetResource;
var
  I, BufSize, NetResult: Integer;
  Count, Size: LongWord;
  Drive: Char;
  NetHandle: THandle;
  NetResources: PNetResourceArray;
  RemoteNameInfo: array[0..1023] of Byte;
begin
  Result := FileName;
  if (Win32Platform <> VER_PLATFORM_WIN32_WINDOWS) or (Win32MajorVersion > 4) then
  begin
    Size := SizeOf(RemoteNameInfo);
    if WNetGetUniversalName(PChar(FileName), UNIVERSAL_NAME_INFO_LEVEL,
      @RemoteNameInfo, Size) <> NO_ERROR then Exit;
    Result := PRemoteNameInfo(@RemoteNameInfo).lpUniversalName;
  end else
  begin
  { The following works around a bug in WNetGetUniversalName under Windows 95 }
    Drive := UpCase(FileName[1]);
    if (Drive < 'A') or (Drive > 'Z') or (Length(FileName) < 3) or
      (FileName[2] <> ':') or (FileName[3] <> '\') then
      Exit;
    if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil,
      NetHandle) <> NO_ERROR then Exit;
    try
      BufSize := 50 * SizeOf(TNetResource);
      GetMem(NetResources, BufSize);
      try
        while True do
        begin
          Count := $FFFFFFFF;
          Size := BufSize;
          NetResult := WNetEnumResource(NetHandle, Count, NetResources, Size);
          if NetResult = ERROR_MORE_DATA then
          begin
            BufSize := Size;
            ReallocMem(NetResources, BufSize);
            Continue;
          end;
          if NetResult <> NO_ERROR then Exit;
          for I := 0 to Count - 1 do
            with NetResources^[I] do
              if (lpLocalName <> nil) and (Drive = UpCase(lpLocalName[0])) then
              begin
                Result := lpRemoteName + Copy(FileName, 3, Length(FileName) - 2);
                Exit;
              end;
        end;
      finally
        FreeMem(NetResources, BufSize);
      end;
    finally
      WNetCloseEnum(NetHandle);
    end;
  end;
end;

function ExpandUNCFileName(const FileName: string): string;
begin
  { First get the local resource version of the file name }
  Result := ExpandFileName(FileName);
  if (Length(Result) >= 3) and (Result[2] = ':') and (Upcase(Result[1]) >= 'A')
    and (Upcase(Result[1]) <= 'Z') then
    Result := GetUniversalName(Result);
end;
{$ENDIF}
{$IFDEF LINUX}
function ExpandUNCFileName(const FileName: string): string;
begin
  Result := ExpandFileName(FileName);
end;
{$ENDIF}

function ExtractRelativePath(const BaseName, DestName: string): string;
var
  BasePath, DestPath: string;
  BaseLead, DestLead: PChar;
  BasePtr, DestPtr: PChar;

  function ExtractFilePathNoDrive(const FileName: string): string;
  begin
    Result := ExtractFilePath(FileName);
    Delete(Result, 1, Length(ExtractFileDrive(FileName)));
  end;

  function Next(var Lead: PChar): PChar;
  begin
    Result := Lead;
    if Result = nil then Exit;
    Lead := AnsiStrScan(Lead, PathDelim);
    if Lead <> nil then
    begin
      Lead^ := #0;
      Inc(Lead);
    end;
  end;

begin
  if SameFilename(ExtractFileDrive(BaseName), ExtractFileDrive(DestName)) then
  begin
    BasePath := ExtractFilePathNoDrive(BaseName);
    UniqueString(BasePath);
    DestPath := ExtractFilePathNoDrive(DestName);
    UniqueString(DestPath);
    BaseLead := Pointer(BasePath);
    BasePtr := Next(BaseLead);
    DestLead := Pointer(DestPath);
    DestPtr := Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and SameFilename(BasePtr, DestPtr) do
    begin
      BasePtr := Next(BaseLead);
      DestPtr := Next(DestLead);
    end;
    Result := '';
    while BaseLead <> nil do
    begin
      Result := Result + '..' + PathDelim;             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result := Result + DestPtr + PathDelim;
    if DestLead <> nil then
      Result := Result + DestLead;     // destlead already has a trailing backslash
    Result := Result + ExtractFileName(DestName);
  end
  else
    Result := DestName;
end;

{$IFDEF MSWINDOWS}
function ExtractShortPathName(const FileName: string): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer,
    GetShortPathName(PChar(FileName), Buffer, SizeOf(Buffer)));
end;
{$ENDIF}

function FileSearch(const Name, DirList: string): string;
var
  I, P, L: Integer;
  C: Char;
begin
  Result := Name;
  P := 1;
  L := Length(DirList);
  while True do
  begin
    if FileExists(Result) then Exit;
    while (P <= L) and (DirList[P] = PathSep) do Inc(P);
    if P > L then Break;
    I := P;
    while (P <= L) and (DirList[P] <> PathSep) do
    begin
      if DirList[P] in LeadBytes then
        P := NextCharIndex(DirList, P)
      else
        Inc(P);
    end;
    Result := Copy(DirList, I, P - I);
    C := AnsiLastChar(Result)^;
    if (C <> DriveDelim) and (C <> PathDelim) then
      Result := Result + PathDelim;
    Result := Result + Name;
  end;
  Result := '';
end;

{$IFDEF MSWINDOWS}
// This function is used if the OS doesn't support GetDiskFreeSpaceEx
function BackfillGetDiskFreeSpaceEx(Directory: PChar; var FreeAvailable,
    TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool; stdcall;
var
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: LongWord;
  Temp: Int64;
  Dir: PChar;
begin
  if Directory <> nil then
    Dir := Directory
  else
    Dir := nil;
  Result := GetDiskFreeSpaceA(Dir, SectorsPerCluster, BytesPerSector,
    FreeClusters, TotalClusters);
  Temp := SectorsPerCluster * BytesPerSector;
  FreeAvailable := Temp * FreeClusters;
  TotalSpace := Temp * TotalClusters;
end;

function InternalGetDiskSpace(Drive: Byte;
  var TotalSpace, FreeSpaceAvailable: Int64): Bool;
var
  RootPath: array[0..4] of Char;
  RootPtr: PChar;
begin
  RootPtr := nil;
  if Drive > 0 then
  begin
    RootPath[0] := Char(Drive + $40);
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
  end;
  Result := GetDiskFreeSpaceEx(RootPtr, FreeSpaceAvailable, TotalSpace, nil);
end;

function DiskFree(Drive: Byte): Int64;
var
  TotalSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, TotalSpace, Result) then
    Result := -1;
end;

function DiskSize(Drive: Byte): Int64;
var
  FreeSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, Result, FreeSpace) then
    Result := -1;
end;
{$ENDIF}

function FileDateToDateTime(FileDate: Integer): TDateTime;
{$IFDEF MSWINDOWS}
begin
  Result :=
    EncodeDate(
      LongRec(FileDate).Hi shr 9 + 1980,
      LongRec(FileDate).Hi shr 5 and 15,
      LongRec(FileDate).Hi and 31) +
    EncodeTime(
      LongRec(FileDate).Lo shr 11,
      LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  UT: TUnixTime;
begin
  localtime_r(@FileDate, UT);
  Result := EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) +
              EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, 0);
end;
{$ENDIF}

function DateTimeToFileDate(DateTime: TDateTime): Integer;
{$IFDEF MSWINDOWS}
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2107) then Result := 0 else
  begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(Result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  tm: TUnixTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  { Valid range for 32 bit Unix time_t:  1970 through 2038  }
  if (Year < 1970) or (Year > 2038) then
    Result := 0
  else
  begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    FillChar(tm, sizeof(tm), 0);
    with tm do
    begin
      tm_sec := Sec;
      tm_min := Min;
      tm_hour := Hour;
      tm_mday := Day;
      tm_mon  := Month - 1;
      tm_year := Year - 1900;
      tm_isdst := -1;
    end;
    Result := mktime(tm);
  end;
end;
{$ENDIF}

function GetCurrentDir: string;
begin
  GetDir(0, Result);
end;

function SetCurrentDir(const Dir: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := SetCurrentDirectory(PChar(Dir));
{$ENDIF}
{$IFDEF LINUX}
  Result := __chdir(PChar(Dir)) = 0;
{$ENDIF}
end;

function CreateDir(const Dir: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := CreateDirectory(PChar(Dir), nil);
{$ENDIF}
{$IFDEF LINUX}
  Result := __mkdir(PChar(Dir), mode_t(-1)) = 0;
{$ENDIF}
end;

function RemoveDir(const Dir: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := RemoveDirectory(PChar(Dir));
{$ENDIF}
{$IFDEF LINUX}
  Result := __rmdir(PChar(Dir)) = 0;
{$ENDIF}
end;

{ PChar routines }

function StrLen(const Str: PChar): Cardinal; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function StrEnd(const Str: PChar): PChar; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        LEA     EAX,[EDI-1]
        MOV     EDI,EDX
end;

function StrMove(Dest: PChar; const Source: PChar; Count: Cardinal): PChar;
begin
  Result := Dest;
  Move(Source^, Dest^, Count);
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
end;

function StrECopy(Dest: PChar; const Source: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        LEA     EAX,[EDI-1]
        POP     ESI
        POP     EDI
end;

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AL,AL
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASB
        JNE     @@1
        INC     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,3
        REP     MOVSB
        STOSB
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

function StrPCopy(Dest: PChar; const Source: string): PChar;
begin
  Result := StrLCopy(Dest, PChar(Source), Length(Source));
end;

function StrPLCopy(Dest: PChar; const Source: string;
  MaxLen: Cardinal): PChar;
begin
  Result := StrLCopy(Dest, PChar(Source), MaxLen);
end;

function StrCat(Dest: PChar; const Source: PChar): PChar;
begin
  StrCopy(StrEnd(Dest), Source);
  Result := Dest;
end;

function StrLCat(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,Dest
        MOV     ESI,Source
        MOV     EBX,MaxLen
        CALL    StrEnd
        MOV     ECX,EDI
        ADD     ECX,EBX
        SUB     ECX,EAX
        JBE     @@1
        MOV     EDX,ESI
        CALL    StrLCopy
@@1:    MOV     EAX,EDI
        POP     EBX
        POP     ESI
        POP     EDI
end;

function StrComp(const Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSB
        MOV     AL,[ESI-1]
        MOV     DL,[EDI-1]
        SUB     EAX,EDX
        POP     ESI
        POP     EDI
end;

function StrIComp(const Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,[ESI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,[EDI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     EAX,EDX
        JE      @@1
@@4:    POP     ESI
        POP     EDI
end;

function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@1
        REPNE   SCASB
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSB
        MOV     AL,[ESI-1]
        MOV     DL,[EDI-1]
        SUB     EAX,EDX
@@1:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrLIComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@4
        REPNE   SCASB
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
        XOR     EDX,EDX
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,[ESI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,[EDI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     EAX,EDX
        JE      @@1
@@4:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrScan(const Str: PChar; Chr: Char): PChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function StrRScan(const Str: PChar; Chr: Char): PChar;
var
  MostRecentFound: PChar;
begin
  if Chr = #0 then
    Result := StrEnd(Str)
  else
  begin
    Result := nil;

    MostRecentFound := Str;
    while True do
    begin
      while MostRecentFound^ <> Chr do
      begin
        if MostRecentFound^ = #0 then
          Exit;
        Inc(MostRecentFound);
      end;
      Result := MostRecentFound;
      Inc(MostRecentFound);
    end;
  end;
end;

function StrPos(const Str1, Str2: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX
        JE      @@2
        OR      EDX,EDX
        JE      @@2
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     AL,AL
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        DEC     ECX
        JE      @@2
        MOV     ESI,ECX
        MOV     EDI,EBX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        SUB     ECX,ESI
        JBE     @@2
        MOV     EDI,EBX
        LEA     EBX,[ESI-1]
@@1:    MOV     ESI,EDX
        LODSB
        REPNE   SCASB
        JNE     @@2
        MOV     EAX,ECX
        PUSH    EDI
        MOV     ECX,EBX
        REPE    CMPSB
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrUpper(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'a'
        JB      @@1
        CMP     AL,'z'
        JA      @@1
        SUB     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;

function StrLower(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'A'
        JB      @@1
        CMP     AL,'Z'
        JA      @@1
        ADD     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end;

function StrAlloc(Size: Cardinal): PChar;
begin
  Inc(Size, SizeOf(Cardinal));
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(Cardinal));
end;

function StrBufSize(const Str: PChar): Cardinal;
var
  P: PChar;
begin
  P := Str;
  Dec(P, SizeOf(Cardinal));
  Result := Cardinal(Pointer(P)^) - SizeOf(Cardinal);
end;

function StrNew(const Str: PChar): PChar;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := StrLen(Str) + 1;
    Result := StrMove(StrAlloc(Size), Str, Size);
  end;
end;

procedure StrDispose(Str: PChar);
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

{ String formatting routines }

procedure FormatError(ErrorCode: Integer; Format: PChar; FmtLen: Cardinal);
const
  FormatErrorStrs: array[0..1] of PResStringRec = (
    @SInvalidFormat, @SArgumentMissing);
var
  Buffer: array[0..31] of Char;
begin
  if FmtLen > 31 then FmtLen := 31;
  if StrByteType(Format, FmtLen-1) = mbLeadByte then Dec(FmtLen);
  StrMove(Buffer, Format, FmtLen);
  Buffer[FmtLen] := #0;
  ConvertErrorFmt(FormatErrorStrs[ErrorCode], [PChar(@Buffer)]);
end;

procedure FormatVarToStr(var S: string; const V: TVarData);
begin
  if Assigned(System.VarToLStrProc) then
    System.VarToLStrProc(S, V)
  else
    System.Error(reVarInvalidOp);
end;

procedure FormatClearStr(var S: string);
begin
  S := '';
end;

function FloatToTextEx(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): Integer;
begin
  Result := FloatToText(BufferArg, Value, ValueType, Format, Precision, Digits,
    FormatSettings);
end;

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr, TempStr: PChar;
  JustFlag: Byte;
  StrBuf: array[0..64] of Char;
  TempAnsiStr: string;
  SaveGOT: Integer;
{ in: eax <-> Buffer }
{ in: edx <-> BufLen }
{ in: ecx <-> Format }

asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,ECX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
{$ELSE}
        XOR     EAX,EAX
{$ENDIF}
        MOV     SaveGOT,EAX
        ADD     ECX,FmtLen
        MOV     BufferOrg,EDI
        XOR     EAX,EAX
        MOV     ArgIndex,EAX
        MOV     TempStr,EAX
        MOV     TempAnsiStr,EAX

@Loop:
        OR      EDX,EDX
        JE      @Done

@NextChar:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @Format

@StoreChar:
        STOSB
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX,EDI
        SUB     EAX,BufferOrg
        JMP     @Exit

@Format:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @StoreChar
        LEA     EBX,[ESI-2]
        MOV     FormatOrg,EBX
@A0:    MOV     JustFlag,AL
        CMP     AL,'-'
        JNE     @A1
        CMP     ESI,ECX
        JE      @Done
        LODSB
@A1:    CALL    @Specifier
        CMP     AL,':'
        JNE     @A2
        MOV     ArgIndex,EBX
        CMP     ESI,ECX
        JE      @Done
        LODSB
        JMP     @A0

@A2:    MOV     Width,EBX
        MOV     EBX,-1
        CMP     AL,'.'
        JNE     @A3
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CALL    @Specifier
@A3:    MOV     Prec,EBX
        MOV     FormatPtr,ESI
        PUSH    ECX
        PUSH    EDX

        CALL    @Convert

        POP     EDX
        MOV     EBX,Width
        SUB     EBX,ECX        //(* ECX <=> number of characters output *)
        JAE     @A4            //(*         jump -> output smaller than width *)
        XOR     EBX,EBX

@A4:    CMP     JustFlag,'-'
        JNE     @A6
        SUB     EDX,ECX
        JAE     @A5
        ADD     ECX,EDX
        XOR     EDX,EDX

@A5:    REP     MOVSB

@A6:    XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A7
        ADD     ECX,EDX
        XOR     EDX,EDX
@A7:    MOV     AL,' '
        REP     STOSB
        XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A8
        ADD     ECX,EDX
        XOR     EDX,EDX
@A8:    REP     MOVSB
        CMP     TempStr,0
        JE      @A9
        PUSH    EDX
        LEA     EAX,TempStr
//      PUSH    EBX                   // GOT setup unnecessary for
//      MOV     EBX, SaveGOT          // same-unit calls to Pascal procedures
        CALL    FormatClearStr
//        POP     EBX
        POP     EDX
@A9:    POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX,EBX
        CMP     AL,'*'
        JE      @B3
@B1:    CMP     AL,'0'
        JB      @B5
        CMP     AL,'9'
        JA      @B5
        IMUL    EBX,EBX,10
        SUB     AL,'0'
        MOVZX   EAX,AL
        ADD     EBX,EAX
        CMP     ESI,ECX
        JE      @B2
        LODSB
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX,ArgIndex
        CMP     EAX,Args.Integer[-4]
        JG      @B4
        INC     ArgIndex
        MOV     EBX,Args
        CMP     [EBX+EAX*8].Byte[4],vtInteger
        MOV     EBX,[EBX+EAX*8]
        JE      @B4
        XOR     EBX,EBX
@B4:    CMP     ESI,ECX
        JE      @B2
        LODSB
@B5:    RET

@Convert:
        AND     AL,0DFH
        MOV     CL,AL
        MOV     EAX,1
        MOV     EBX,ArgIndex
        CMP     EBX,Args.Integer[-4]
        JG      @ErrorExit
        INC     ArgIndex
        MOV     ESI,Args
        LEA     ESI,[ESI+EBX*8]
        MOV     EAX,[ESI].Integer[0]       // TVarRec.data
        MOVZX   EDX,[ESI].Byte[4]          // TVarRec.VType
{$IFDEF PIC}
        MOV     EBX, SaveGOT
        ADD     EBX, offset @CvtVector
        MOV     EBX, [EBX+EDX*4]
        ADD     EBX, SaveGOT
        JMP     EBX
{$ELSE}
        JMP     @CvtVector.Pointer[EDX*4]
{$ENDIF}

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtWideChar:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
        CALL    @ClearTmpAnsiStr
        MOV     EDX,FormatOrg
        MOV     ECX,FormatPtr
        SUB     ECX,EDX
{$IFDEF PC_MAPPED_EXCEPTIONS}
        //  Because of all the assembly code here, we can't call a routine
        //  that throws an exception if it looks like we're still on the
        //  stack.  The static disassembler cannot give sufficient unwind
        //  frame info to unwind the confusion that is generated from the
        //  assembly code above.  So before we throw the exception, we
        //  go to some lengths to excise ourselves from the stack chain.
        //  We were passed 12 bytes of parameters on the stack, and we have
        //  to make sure that we get rid of those, too.
        MOV     EBX, SaveGOT
        MOV     ESP, EBP        // Ditch everthing to the frame
        MOV     EBP, [ESP + 4]  // Get the return addr
        MOV     [ESP + 16], EBP // Move the ret addr up in the stack
        POP     EBP             // Ditch the rest of the frame
        ADD     ESP, 12         // Ditch the space that was taken by params
        JMP     FormatError     // Off to FormatErr
{$ELSE}
        MOV     EBX, SaveGOT
        CALL    FormatError
{$ENDIF}
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     ESI,StrBuf[32]
        MOV     EDX, Prec
        CMP     EDX, 32
        JBE     @I64_1           // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@I64_1: MOV     EBX, ECX
        SUB     CL, 'D'
        JZ      CvtInt64         // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt64
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt64
        JMP     @CvtError

{        LEA     EBX, TempInt64       // (input is array of const; save original)
        MOV     EDX, [EAX]
        MOV     [EBX], EDX
        MOV     EDX, [EAX + 4]
        MOV     [EBX + 4], EDX

        // EBX <= address of TempInt64

        CMP     CL,'D'
        JE      @DecI64
        CMP     CL,'U'
        JE      @DecI64_2
        CMP     CL,'X'
        JNE     @CvtError

@HexI64:
        MOV     ECX,16               // hex divisor
        JMP     @CvtI64

@DecI64:
        TEST    DWORD PTR [EBX + 4], $80000000      // sign bit set?
        JE      @DecI64_2            //   no -> bypass '-' output

        NEG     DWORD PTR [EBX]      // negate lo-order, then hi-order
        ADC     DWORD PTR [EBX+4], 0
        NEG     DWORD PTR [EBX+4]

        CALL    @DecI64_2

        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET

@DecI64_2:                           // unsigned int64 output
        MOV     ECX,10               // decimal divisor

@CvtI64:
        LEA     ESI,StrBuf[32]

@CvtI64_1:
        PUSH    EBX
        PUSH    ECX                  // save radix
        PUSH    0
        PUSH    ECX                  // radix divisor (10 or 16 only)
        MOV     EAX, [EBX]
        MOV     EDX, [EBX + 4]
        MOV     EBX, SaveGOT
        CALL    System.@_llumod
        POP     ECX                  // saved radix
        POP     EBX

        XCHG    EAX, EDX             // lo-value to EDX for character output
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @CvtI64_2

        ADD     DL,('A'-'0')-10

@CvtI64_2:
        DEC     ESI
        MOV     [ESI],DL

        PUSH    EBX
        PUSH    ECX                  // save radix
        PUSH    0
        PUSH    ECX                  // radix divisor (10 or 16 only)
        MOV     EAX, [EBX]           // value := value DIV radix
        MOV     EDX, [EBX + 4]
        MOV     EBX, SaveGOT
        CALL    System.@_lludiv
        POP     ECX                  // saved radix
        POP     EBX
        MOV     [EBX], EAX
        MOV     [EBX + 4], EDX
        OR      EAX,EDX              // anything left to output?
        JNE     @CvtI64_1            //   no jump => EDX:EAX = 0

        LEA     ECX,StrBuf[32]
        SUB     ECX,ESI
        MOV     EDX,Prec
        CMP     EDX,16
        JBE     @CvtI64_3
        RET

@CvtI64_3:
        SUB     EDX,ECX
        JBE     @CvtI64_5
        ADD     ECX,EDX
        MOV     AL,'0'

@CvtI64_4:
        DEC     ESI
        MOV     [ESI],AL
        DEC     EDX
        JNE     @CvtI64_4

@CvtI64_5:
        RET
}
////////////////////////////////////////////////

@CvtInteger:
        LEA     ESI,StrBuf[16]
        MOV     EDX, Prec
        MOV     EBX, ECX
        CMP     EDX, 16
        JBE     @C1             // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@C1:    SUB     CL, 'D'
        JZ      CvtInt          // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt
        JMP     @CvtError

{        CMP     CL,'D'
        JE      @C1
        CMP     CL,'U'
        JE      @C2
        CMP     CL,'X'
        JNE     @CvtError
        MOV     ECX,16
        JMP     @CvtLong
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLong:
        LEA     ESI,StrBuf[16]
@D1:    XOR     EDX,EDX
        DIV     ECX
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,('A'-'0')-10
@D2:    DEC     ESI
        MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        LEA     ECX,StrBuf[16]
        SUB     ECX,ESI
        MOV     EDX,Prec
        CMP     EDX,16
        JBE     @D3
        RET
@D3:    SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
@D4:    DEC     ESI
        MOV     [ESI],AL
        DEC     EDX
        JNE     @D4
@D5:    RET
}
@CvtChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ECX,1
        RET

@CvtVariant:
        CMP     CL,'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType,varNull
        JBE     @CvtEmptyStr
        MOV     EDX,EAX
        LEA     EAX,TempStr
//      PUSH    EBX                   // GOT setup unnecessary for
//      MOV     EBX, SaveGOT          // same-unit calls to Pascal procedures
        CALL    FormatVarToStr
//        POP     EBX
        MOV     ESI,TempStr
        JMP     @CvtStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        LODSB
        MOVZX   ECX,AL
        JMP     @CvtStrLen

@CvtPWideChar:
        MOV    ESI,OFFSET System.@LStrFromPWChar
        JMP    @CvtWideThing

@CvtWideString:
        MOV    ESI,OFFSET System.@LStrFromWStr

@CvtWideThing:
        ADD    ESI, SaveGOT
{$IFDEF PIC}
        MOV    ESI, [ESI]
{$ENDIF}
        CMP    CL,'S'
        JNE    @CvtError
        MOV    EDX,EAX
        LEA    EAX,TempAnsiStr
        PUSH   EBX
        MOV    EBX, SaveGOT
        CALL   ESI
        POP    EBX
        MOV    ESI,TempAnsiStr
        MOV    EAX,ESI
        JMP    @CvtStrRef

@CvtAnsiStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX

@CvtStrRef:
        OR      ESI,ESI
        JE      @CvtEmptyStr
        MOV     ECX,[ESI-4]

@CvtStrLen:
        CMP     ECX,Prec
        JA      @E1
        RET
@E1:    MOV     ECX,Prec
        RET

@CvtPChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        PUSH    EDI
        MOV     EDI,EAX
        XOR     AL,AL
        MOV     ECX,Prec
        JECXZ   @F1
        REPNE   SCASB
        JNE     @F1
        DEC     EDI
@F1:    MOV     ECX,EDI
        SUB     ECX,ESI
        POP     EDI
        RET

@CvtPointer:
        CMP     CL,'P'
        JNE     @CvtError
        MOV     EDX,8
        MOV     ECX,16
        LEA     ESI,StrBuf[16]
        JMP     CvtInt

@CvtCurrency:
        MOV     BH,fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH,fvExtended

@CvtFloat:
        MOV     ESI,EAX
        MOV     BL,ffGeneral
        CMP     CL,'G'
        JE      @G2
        MOV     BL,ffExponent
        CMP     CL,'E'
        JE      @G2
        MOV     BL,ffFixed
        CMP     CL,'F'
        JE      @G1
        MOV     BL,ffNumber
        CMP     CL,'N'
        JE      @G1
        CMP     CL,'M'
        JNE     @CvtError
        MOV     BL,ffCurrency
@G1:    MOV     EAX,18
        MOV     EDX,Prec
        CMP     EDX,EAX
        JBE     @G3
        MOV     EDX,2
        CMP     CL,'M'
        JNE     @G3
        MOVZX   EDX,CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX,Prec
        MOV     EDX,3
        CMP     EAX,18
        JBE     @G3
        MOV     EAX,15
@G3:    PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,StrBuf
        MOV     EDX,ESI
        MOVZX   ECX,BH
        MOV     EBX, SaveGOT
        CALL    FloatToText
        MOV     ECX,EAX
        LEA     ESI,StrBuf
        RET

@ClearTmpAnsiStr:
        PUSH    EBX
        PUSH    EAX
        LEA     EAX,TempAnsiStr
        MOV     EBX, SaveGOT
        CALL    System.@LStrClr
        POP     EAX
        POP     EBX
        RET

@Exit:
        CALL    @ClearTmpAnsiStr
        POP     EDI
        POP     ESI
        POP     EBX
end;

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal;
var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr, TempStr: PChar;
  JustFlag: Byte;
  StrBuf: array[0..64] of Char;
  TempAnsiStr: string;
  SaveGOT: Integer;
{ in: eax <-> Buffer }
{ in: edx <-> BufLen }
{ in: ecx <-> Format }

asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,ECX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
{$ELSE}
        XOR     EAX,EAX
{$ENDIF}
        MOV     SaveGOT,EAX
        ADD     ECX,FmtLen
        MOV     BufferOrg,EDI
        XOR     EAX,EAX
        MOV     ArgIndex,EAX
        MOV     TempStr,EAX
        MOV     TempAnsiStr,EAX

@Loop:
        OR      EDX,EDX
        JE      @Done

@NextChar:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @Format

@StoreChar:
        STOSB
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX,EDI
        SUB     EAX,BufferOrg
        JMP     @Exit

@Format:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @StoreChar
        LEA     EBX,[ESI-2]
        MOV     FormatOrg,EBX
@A0:    MOV     JustFlag,AL
        CMP     AL,'-'
        JNE     @A1
        CMP     ESI,ECX
        JE      @Done
        LODSB
@A1:    CALL    @Specifier
        CMP     AL,':'
        JNE     @A2
        MOV     ArgIndex,EBX
        CMP     ESI,ECX
        JE      @Done
        LODSB
        JMP     @A0

@A2:    MOV     Width,EBX
        MOV     EBX,-1
        CMP     AL,'.'
        JNE     @A3
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CALL    @Specifier
@A3:    MOV     Prec,EBX
        MOV     FormatPtr,ESI
        PUSH    ECX
        PUSH    EDX

        CALL    @Convert

        POP     EDX
        MOV     EBX,Width
        SUB     EBX,ECX        //(* ECX <=> number of characters output *)
        JAE     @A4            //(*         jump -> output smaller than width *)
        XOR     EBX,EBX

@A4:    CMP     JustFlag,'-'
        JNE     @A6
        SUB     EDX,ECX
        JAE     @A5
        ADD     ECX,EDX
        XOR     EDX,EDX

@A5:    REP     MOVSB

@A6:    XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A7
        ADD     ECX,EDX
        XOR     EDX,EDX
@A7:    MOV     AL,' '
        REP     STOSB
        XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A8
        ADD     ECX,EDX
        XOR     EDX,EDX
@A8:    REP     MOVSB
        CMP     TempStr,0
        JE      @A9
        PUSH    EDX
        LEA     EAX,TempStr
//      PUSH    EBX                   // GOT setup unnecessary for
//      MOV     EBX, SaveGOT          // same-unit calls to Pascal procedures
        CALL    FormatClearStr
//        POP     EBX
        POP     EDX
@A9:    POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX,EBX
        CMP     AL,'*'
        JE      @B3
@B1:    CMP     AL,'0'
        JB      @B5
        CMP     AL,'9'
        JA      @B5
        IMUL    EBX,EBX,10
        SUB     AL,'0'
        MOVZX   EAX,AL
        ADD     EBX,EAX
        CMP     ESI,ECX
        JE      @B2
        LODSB
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX,ArgIndex
        CMP     EAX,Args.Integer[-4]
        JG      @B4          
        INC     ArgIndex
        MOV     EBX,Args
        CMP     [EBX+EAX*8].Byte[4],vtInteger
        MOV     EBX,[EBX+EAX*8]
        JE      @B4
        XOR     EBX,EBX
@B4:    CMP     ESI,ECX
        JE      @B2
        LODSB
@B5:    RET

@Convert:
        AND     AL,0DFH
        MOV     CL,AL
        MOV     EAX,1
        MOV     EBX,ArgIndex
        CMP     EBX,Args.Integer[-4]
        JG      @ErrorExit          
        INC     ArgIndex
        MOV     ESI,Args
        LEA     ESI,[ESI+EBX*8]
        MOV     EAX,[ESI].Integer[0]       // TVarRec.data
        MOVZX   EDX,[ESI].Byte[4]          // TVarRec.VType
{$IFDEF PIC}
        MOV     EBX, SaveGOT
        ADD     EBX, offset @CvtVector
        MOV     EBX, [EBX+EDX*4]
        ADD     EBX, SaveGOT
        JMP     EBX
{$ELSE}
        JMP     @CvtVector.Pointer[EDX*4]
{$ENDIF}

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtWideChar:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
        CALL    @ClearTmpAnsiStr
        MOV     EDX,FormatOrg
        MOV     ECX,FormatPtr
        SUB     ECX,EDX
{$IFDEF PC_MAPPED_EXCEPTIONS}
        //  Because of all the assembly code here, we can't call a routine
        //  that throws an exception if it looks like we're still on the
        //  stack.  The static disassembler cannot give sufficient unwind
        //  frame info to unwind the confusion that is generated from the
        //  assembly code above.  So before we throw the exception, we
        //  go to some lengths to excise ourselves from the stack chain.
        //  We were passed 12 bytes of parameters on the stack, and we have
        //  to make sure that we get rid of those, too.
        MOV     EBX, SaveGOT
        MOV     ESP, EBP        // Ditch everthing to the frame
        MOV     EBP, [ESP + 4]  // Get the return addr
        MOV     [ESP + 16], EBP // Move the ret addr up in the stack
        POP     EBP             // Ditch the rest of the frame
        ADD     ESP, 12         // Ditch the space that was taken by params
        JMP     FormatError     // Off to FormatErr
{$ELSE}
        MOV     EBX, SaveGOT
        CALL    FormatError
{$ENDIF}
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     ESI,StrBuf[32]
        MOV     EDX, Prec
        CMP     EDX, 32
        JBE     @I64_1           // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@I64_1: MOV     EBX, ECX
        SUB     CL, 'D'
        JZ      CvtInt64         // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt64
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt64
        JMP     @CvtError
////////////////////////////////////////////////

@CvtInteger:
        LEA     ESI,StrBuf[16]
        MOV     EDX, Prec
        MOV     EBX, ECX
        CMP     EDX, 16
        JBE     @C1             // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@C1:    SUB     CL, 'D'
        JZ      CvtInt          // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt
        JMP     @CvtError

@CvtChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ECX,1
        RET

@CvtVariant:
        CMP     CL,'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType,varNull
        JBE     @CvtEmptyStr
        MOV     EDX,EAX
        LEA     EAX,TempStr
//      PUSH    EBX                   // GOT setup unnecessary for
//      MOV     EBX, SaveGOT          // same-unit calls to Pascal procedures
        CALL    FormatVarToStr
//        POP     EBX
        MOV     ESI,TempStr
        JMP     @CvtStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        LODSB
        MOVZX   ECX,AL
        JMP     @CvtStrLen

@CvtPWideChar:
        MOV    ESI,OFFSET System.@LStrFromPWChar
        JMP    @CvtWideThing

@CvtWideString:
        MOV    ESI,OFFSET System.@LStrFromWStr

@CvtWideThing:
        ADD    ESI, SaveGOT
{$IFDEF PIC}
        MOV    ESI, [ESI]
{$ENDIF}
        CMP    CL,'S'
        JNE    @CvtError
        MOV    EDX,EAX
        LEA    EAX,TempAnsiStr
        PUSH   EBX
        MOV    EBX, SaveGOT
        CALL   ESI
        POP    EBX
        MOV    ESI,TempAnsiStr
        MOV    EAX,ESI
        JMP    @CvtStrRef

@CvtAnsiStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX

@CvtStrRef:
        OR      ESI,ESI
        JE      @CvtEmptyStr
        MOV     ECX,[ESI-4]

@CvtStrLen:
        CMP     ECX,Prec
        JA      @E1
        RET
@E1:    MOV     ECX,Prec
        RET

@CvtPChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        PUSH    EDI
        MOV     EDI,EAX
        XOR     AL,AL
        MOV     ECX,Prec
        JECXZ   @F1
        REPNE   SCASB
        JNE     @F1
        DEC     EDI
@F1:    MOV     ECX,EDI
        SUB     ECX,ESI
        POP     EDI
        RET

@CvtPointer:
        CMP     CL,'P'
        JNE     @CvtError
        MOV     EDX,8
        MOV     ECX,16
        LEA     ESI,StrBuf[16]
        JMP     CvtInt

@CvtCurrency:
        MOV     BH,fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH,fvExtended

@CvtFloat:
        MOV     ESI,EAX
        MOV     BL,ffGeneral
        CMP     CL,'G'
        JE      @G2
        MOV     BL,ffExponent
        CMP     CL,'E'
        JE      @G2
        MOV     BL,ffFixed
        CMP     CL,'F'
        JE      @G1
        MOV     BL,ffNumber
        CMP     CL,'N'
        JE      @G1
        CMP     CL,'M'
        JNE     @CvtError
        MOV     BL,ffCurrency
@G1:    MOV     EAX,18
        MOV     EDX,Prec
        CMP     EDX,EAX
        JBE     @G3
        MOV     EDX,2
        CMP     CL,'M'
        JNE     @G3
        MOV     EDX,FormatSettings
        MOVZX   EDX,[EDX].TFormatSettings.CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX,Prec
        MOV     EDX,3
        CMP     EAX,18
        JBE     @G3
        MOV     EAX,15
@G3:    PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,[FormatSettings]
        PUSH    EDX
        LEA     EAX,StrBuf
        MOV     EDX,ESI
        MOVZX   ECX,BH
        MOV     EBX, SaveGOT
        CALL    FloatToTextEx
        MOV     ECX,EAX
        LEA     ESI,StrBuf
        RET

@ClearTmpAnsiStr:
        PUSH    EBX
        PUSH    EAX
        LEA     EAX,TempAnsiStr
        MOV     EBX, SaveGOT
        CALL    System.@LStrClr
        POP     EAX
        POP     EBX
        RET

@Exit:
        CALL    @ClearTmpAnsiStr
        POP     EDI
        POP     ESI
        POP     EBX
end;

function StrFmt(Buffer, Format: PChar; const Args: array of const): PChar;
begin
  if (Buffer <> nil) and (Format <> nil) then
  begin
    Buffer[FormatBuf(Buffer^, MaxInt, Format^, StrLen(Format), Args)] := #0;
    Result := Buffer;
  end
  else
    Result := nil;
end;

function StrFmt(Buffer, Format: PChar; const Args: array of const;
  const FormatSettings: TFormatSettings): PChar;
begin
  if (Buffer <> nil) and (Format <> nil) then
  begin
    Buffer[FormatBuf(Buffer^, MaxInt, Format^, StrLen(Format), Args,
      FormatSettings)] := #0;
    Result := Buffer;
  end
  else
    Result := nil;
end;

function StrLFmt(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar;
  const Args: array of const): PChar;
begin
  if (Buffer <> nil) and (Format <> nil) then
  begin
    Buffer[FormatBuf(Buffer^, MaxBufLen, Format^, StrLen(Format), Args)] := #0;
    Result := Buffer;
  end
  else
    Result := nil;
end;

function StrLFmt(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar;
  const Args: array of const; const FormatSettings: TFormatSettings): PChar;
begin
  if (Buffer <> nil) and (Format <> nil) then
  begin
    Buffer[FormatBuf(Buffer^, MaxBufLen, Format^, StrLen(Format), Args,
      FormatSettings)] := #0;
    Result := Buffer;
  end
  else
    Result := nil;
end;

function Format(const Format: string; const Args: array of const): string;
begin
  FmtStr(Result, Format, Args);
end;

function Format(const Format: string; const Args: array of const;
  const FormatSettings: TFormatSettings): string;
begin
  FmtStr(Result, Format, Args, FormatSettings);
end;

procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of Char;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := FormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format), Args)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := FormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
      Length(Format), Args);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const; const FormatSettings: TFormatSettings);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of Char;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := FormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format),
      Args, FormatSettings)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := FormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args, FormatSettings);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

procedure WideFormatError(ErrorCode: Integer; Format: PWideChar;
  FmtLen: Cardinal);
var
  WideFormat: WideString;
  FormatText: string;
begin
  SetLength(WideFormat, FmtLen);
  SetString(WideFormat, Format, FmtLen);
  FormatText := WideFormat;
  FormatError(ErrorCode, PChar(FormatText), FmtLen);
end;

procedure WideFormatVarToStr(var S: WideString; const V: TVarData);
begin
  if Assigned(System.VarToWStrProc) then
    System.VarToWStrProc(S, V)
  else
    System.Error(reVarInvalidOp);
end;

function WideFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr: PWideChar;
  JustFlag: WideChar;
  StrBuf: array[0..64] of WideChar;
  TempWideStr: WideString;
  SaveGOT: Integer;
{ in: eax <-> Buffer }
{ in: edx <-> BufLen }
{ in: ecx <-> Format }

asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,ECX
{$IFDEF PIC}
        CALL    GetGOT
{$ELSE}
        XOR     EAX,EAX
{$ENDIF}
        MOV     SaveGOT,EAX
        MOV     ECX,FmtLen
        LEA     ECX,[ECX*2+ESI]
        MOV     BufferOrg,EDI
        XOR     EAX,EAX
        MOV     ArgIndex,EAX
        MOV     TempWideStr,EAX

@Loop:
        OR      EDX,EDX
        JE      @Done

@NextChar:
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CMP     AX,'%'
        JE      @Format

@StoreChar:
        STOSW
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX,EDI
        SUB     EAX,BufferOrg
        SHR     EAX,1
        JMP     @Exit

@Format:
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CMP     AX,'%'
        JE      @StoreChar
        LEA     EBX,[ESI-4]
        MOV     FormatOrg,EBX
@A0:    MOV     JustFlag,AX
        CMP     AX,'-'
        JNE     @A1
        CMP     ESI,ECX
        JE      @Done
        LODSW
@A1:    CALL    @Specifier
        CMP     AX,':'
        JNE     @A2
        MOV     ArgIndex,EBX
        CMP     ESI,ECX
        JE      @Done
        LODSW
        JMP     @A0

@A2:    MOV     Width,EBX
        MOV     EBX,-1
        CMP     AX,'.'
        JNE     @A3
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CALL    @Specifier
@A3:    MOV     Prec,EBX
        MOV     FormatPtr,ESI
        PUSH    ECX
        PUSH    EDX

        CALL    @Convert

        POP     EDX
        MOV     EBX,Width
        SUB     EBX,ECX        //(* ECX <=> number of characters output *)
        JAE     @A4            //(*         jump -> output smaller than width *)
        XOR     EBX,EBX

@A4:    CMP     JustFlag,'-'
        JNE     @A6
        SUB     EDX,ECX
        JAE     @A5
        ADD     ECX,EDX
        XOR     EDX,EDX

@A5:    REP     MOVSW

@A6:    XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A7
        ADD     ECX,EDX
        XOR     EDX,EDX
@A7:    MOV     AX,' '
        REP     STOSW
        XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A8
        ADD     ECX,EDX
        XOR     EDX,EDX
@A8:    REP     MOVSW
        POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX,EBX
        CMP     AX,'*'
        JE      @B3
@B1:    CMP     AX,'0'
        JB      @B5
        CMP     AX,'9'
        JA      @B5
        IMUL    EBX,EBX,10
        SUB     AX,'0'
        MOVZX   EAX,AX
        ADD     EBX,EAX
        CMP     ESI,ECX
        JE      @B2
        LODSW
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX,ArgIndex
        CMP     EAX,Args.Integer[-4]
        JG      @B4
        INC     ArgIndex
        MOV     EBX,Args
        CMP     [EBX+EAX*8].Byte[4],vtInteger
        MOV     EBX,[EBX+EAX*8]
        JE      @B4
        XOR     EBX,EBX
@B4:    CMP     ESI,ECX
        JE      @B2
        LODSW
@B5:    RET

@Convert:
        AND     AL,0DFH
        MOV     CL,AL
        MOV     EAX,1
        MOV     EBX,ArgIndex
        CMP     EBX,Args.Integer[-4]
        JG      @ErrorExit
        INC     ArgIndex
        MOV     ESI,Args
        LEA     ESI,[ESI+EBX*8]
        MOV     EAX,[ESI].Integer[0]       // TVarRec.data
        MOVZX   EDX,[ESI].Byte[4]          // TVarRec.VType
{$IFDEF PIC}
        MOV     EBX, SaveGOT
        ADD     EBX, offset @CvtVector
        MOV     EBX, [EBX+EDX*4]
        ADD     EBX, SaveGOT
        JMP     EBX
{$ELSE}
        JMP     @CvtVector.Pointer[EDX*4]
{$ENDIF}

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
        CALL    @ClearTmpWideStr
        MOV     EDX,FormatOrg
        MOV     ECX,FormatPtr
        SUB     ECX,EDX
        SHR     ECX,1
        MOV     EBX, SaveGOT
{$IFDEF PC_MAPPED_EXCEPTIONS}
        //  Because of all the assembly code here, we can't call a routine
        //  that throws an exception if it looks like we're still on the
        //  stack.  The static disassembler cannot give sufficient unwind
        //  frame info to unwind the confusion that is generated from the
        //  assembly code above.  So before we throw the exception, we
        //  go to some lengths to excise ourselves from the stack chain.
        //  We were passed 12 bytes of parameters on the stack, and we have
        //  to make sure that we get rid of those, too.
        MOV     ESP, EBP        // Ditch everthing to the frame
        MOV     EBP, [ESP + 4]  // Get the return addr
        MOV     [ESP + 16], EBP // Move the ret addr up in the stack
        POP     EBP             // Ditch the rest of the frame
        ADD     ESP, 12         // Ditch the space that was taken by params
        JMP     WideFormatError // Off to FormatErr
{$ELSE}
        CALL    WideFormatError
{$ENDIF}
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     ESI,StrBuf[64]
        MOV     EDX, Prec
        CMP     EDX, 32
        JBE     @I64_1           // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@I64_1: MOV     EBX, ECX
        SUB     CL, 'D'
        JZ      CvtInt64W         // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt64W
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt64W
        JMP     @CvtError

@CvtInteger:
        LEA     ESI,StrBuf[32]
        MOV     EDX, Prec
        MOV     EBX, ECX
        CMP     EDX, 16
        JBE     @C1             // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@C1:    SUB     CL, 'D'
        JZ      CvtIntW          // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtIntW
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtIntW
        JMP     @CvtError

@CvtChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     EAX,ESI
        MOV     ECX,1
        JMP     @CvtAnsiThingLen

@CvtWideChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ECX,1
        RET

@CvtVariant:
        CMP     CL,'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType,varNull
        JBE     @CvtEmptyStr
        MOV     EDX,EAX
        LEA     EAX,TempWideStr
        CALL    WideFormatVarToStr
        MOV     ESI,TempWideStr
        JMP     @CvtWideStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOVZX   ECX,BYTE PTR [EAX]
        INC     EAX

@CvtAnsiThingLen:
        MOV     ESI,OFFSET System.@WStrFromPCharLen
        JMP     @CvtAnsiThing

@CvtPChar:
        MOV    ESI,OFFSET System.@WStrFromPChar
        JMP    @CvtAnsiThingTest

@CvtAnsiStr:
        MOV    ESI,OFFSET System.@WStrFromLStr

@CvtAnsiThingTest:
        CMP    CL,'S'
        JNE    @CvtError

@CvtAnsiThing:
        ADD    ESI, SaveGOT
{$IFDEF PIC}
        MOV    ESI, [ESI]
{$ENDIF}
        MOV    EDX,EAX
        LEA    EAX,TempWideStr
        PUSH   EBX
        MOV    EBX, SaveGOT
        CALL   ESI
        POP    EBX
        MOV    ESI,TempWideStr
        JMP    @CvtWideStrRef

@CvtWideString:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX

@CvtWideStrRef:
        OR      ESI,ESI
        JE      @CvtEmptyStr
        MOV     ECX,[ESI-4]
        SHR     ECX,1

@CvtWideStrLen:
        CMP     ECX,Prec
        JA      @E1
        RET
@E1:    MOV     ECX,Prec
        RET

@CvtPWideChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        PUSH    EDI
        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     ECX,Prec
        JECXZ   @F1
        REPNE   SCASW
        JNE     @F1
        DEC     EDI
        DEC     EDI
@F1:    MOV     ECX,EDI
        SUB     ECX,ESI
        SHR     ECX,1
        POP     EDI
        RET

@CvtPointer:
        CMP     CL,'P'
        JNE     @CvtError
        MOV     EDX,8
        MOV     ECX,16
        LEA     ESI,StrBuf[32]
        JMP     CvtInt

@CvtCurrency:
        MOV     BH,fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH,fvExtended

@CvtFloat:
        MOV     ESI,EAX
        MOV     BL,ffGeneral
        CMP     CL,'G'
        JE      @G2
        MOV     BL,ffExponent
        CMP     CL,'E'
        JE      @G2
        MOV     BL,ffFixed
        CMP     CL,'F'
        JE      @G1
        MOV     BL,ffNumber
        CMP     CL,'N'
        JE      @G1
        CMP     CL,'M'
        JNE     @CvtError
        MOV     BL,ffCurrency
@G1:    MOV     EAX,18
        MOV     EDX,Prec
        CMP     EDX,EAX
        JBE     @G3
        MOV     EDX,2
        CMP     CL,'M'
        JNE     @G3
        MOVZX   EDX,CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX,Prec
        MOV     EDX,3
        CMP     EAX,18
        JBE     @G3
        MOV     EAX,15
@G3:    PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,StrBuf
        MOV     EDX,ESI
        MOVZX   ECX,BH
        MOV     EBX, SaveGOT
        CALL    FloatToText
        MOV     ECX,EAX
        LEA     EAX,StrBuf
        JMP     @CvtAnsiThingLen

@ClearTmpWideStr:
        PUSH    EBX
        PUSH    EAX
        LEA     EAX,TempWideStr
        MOV     EBX, SaveGOT
        CALL    System.@WStrClr
        POP     EAX
        POP     EBX
        RET

@Exit:
        CALL    @ClearTmpWideStr
        POP     EDI
        POP     ESI
        POP     EBX
end;

function WideFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal;
var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr: PWideChar;
  JustFlag: WideChar;
  StrBuf: array[0..64] of WideChar;
  TempWideStr: WideString;
  SaveGOT: Integer;
{ in: eax <-> Buffer }
{ in: edx <-> BufLen }
{ in: ecx <-> Format }

asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,ECX
{$IFDEF PIC}
        CALL    GetGOT
{$ELSE}
        XOR     EAX,EAX
{$ENDIF}
        MOV     SaveGOT,EAX
        MOV     ECX,FmtLen
        LEA     ECX,[ECX*2+ESI]
        MOV     BufferOrg,EDI
        XOR     EAX,EAX
        MOV     ArgIndex,EAX
        MOV     TempWideStr,EAX

@Loop:
        OR      EDX,EDX
        JE      @Done

@NextChar:
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CMP     AX,'%'
        JE      @Format

@StoreChar:
        STOSW
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX,EDI
        SUB     EAX,BufferOrg
        SHR     EAX,1
        JMP     @Exit

@Format:
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CMP     AX,'%'
        JE      @StoreChar
        LEA     EBX,[ESI-4]
        MOV     FormatOrg,EBX
@A0:    MOV     JustFlag,AX
        CMP     AX,'-'
        JNE     @A1
        CMP     ESI,ECX
        JE      @Done
        LODSW
@A1:    CALL    @Specifier
        CMP     AX,':'
        JNE     @A2
        MOV     ArgIndex,EBX
        CMP     ESI,ECX
        JE      @Done
        LODSW
        JMP     @A0

@A2:    MOV     Width,EBX
        MOV     EBX,-1
        CMP     AX,'.'
        JNE     @A3
        CMP     ESI,ECX
        JE      @Done
        LODSW
        CALL    @Specifier
@A3:    MOV     Prec,EBX
        MOV     FormatPtr,ESI
        PUSH    ECX
        PUSH    EDX

        CALL    @Convert

        POP     EDX
        MOV     EBX,Width
        SUB     EBX,ECX        //(* ECX <=> number of characters output *)
        JAE     @A4            //(*         jump -> output smaller than width *)
        XOR     EBX,EBX

@A4:    CMP     JustFlag,'-'
        JNE     @A6
        SUB     EDX,ECX
        JAE     @A5
        ADD     ECX,EDX
        XOR     EDX,EDX

@A5:    REP     MOVSW

@A6:    XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A7
        ADD     ECX,EDX
        XOR     EDX,EDX
@A7:    MOV     AX,' '
        REP     STOSW
        XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A8
        ADD     ECX,EDX
        XOR     EDX,EDX
@A8:    REP     MOVSW
        POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX,EBX
        CMP     AX,'*'
        JE      @B3
@B1:    CMP     AX,'0'
        JB      @B5
        CMP     AX,'9'
        JA      @B5
        IMUL    EBX,EBX,10
        SUB     AX,'0'
        MOVZX   EAX,AX
        ADD     EBX,EAX
        CMP     ESI,ECX
        JE      @B2
        LODSW
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX,ArgIndex
        CMP     EAX,Args.Integer[-4]
        JG      @B4
        INC     ArgIndex
        MOV     EBX,Args
        CMP     [EBX+EAX*8].Byte[4],vtInteger
        MOV     EBX,[EBX+EAX*8]
        JE      @B4
        XOR     EBX,EBX
@B4:    CMP     ESI,ECX
        JE      @B2
        LODSW
@B5:    RET

@Convert:
        AND     AL,0DFH
        MOV     CL,AL
        MOV     EAX,1
        MOV     EBX,ArgIndex
        CMP     EBX,Args.Integer[-4]
        JG      @ErrorExit
        INC     ArgIndex
        MOV     ESI,Args
        LEA     ESI,[ESI+EBX*8]
        MOV     EAX,[ESI].Integer[0]       // TVarRec.data
        MOVZX   EDX,[ESI].Byte[4]          // TVarRec.VType
{$IFDEF PIC}
        MOV     EBX, SaveGOT
        ADD     EBX, offset @CvtVector
        MOV     EBX, [EBX+EDX*4]
        ADD     EBX, SaveGOT
        JMP     EBX
{$ELSE}
        JMP     @CvtVector.Pointer[EDX*4]
{$ENDIF}

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
        CALL    @ClearTmpWideStr
        MOV     EDX,FormatOrg
        MOV     ECX,FormatPtr
        SUB     ECX,EDX
        SHR     ECX,1
        MOV     EBX, SaveGOT
{$IFDEF PC_MAPPED_EXCEPTIONS}
        //  Because of all the assembly code here, we can't call a routine
        //  that throws an exception if it looks like we're still on the
        //  stack.  The static disassembler cannot give sufficient unwind
        //  frame info to unwind the confusion that is generated from the
        //  assembly code above.  So before we throw the exception, we
        //  go to some lengths to excise ourselves from the stack chain.
        //  We were passed 12 bytes of parameters on the stack, and we have
        //  to make sure that we get rid of those, too.
        MOV     ESP, EBP        // Ditch everthing to the frame
        MOV     EBP, [ESP + 4]  // Get the return addr
        MOV     [ESP + 16], EBP // Move the ret addr up in the stack
        POP     EBP             // Ditch the rest of the frame
        ADD     ESP, 12         // Ditch the space that was taken by params
        JMP     WideFormatError // Off to FormatErr
{$ELSE}
        CALL    WideFormatError
{$ENDIF}
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     ESI,StrBuf[64]
        MOV     EDX, Prec
        CMP     EDX, 32
        JBE     @I64_1           // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@I64_1: MOV     EBX, ECX
        SUB     CL, 'D'
        JZ      CvtInt64W         // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtInt64W
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtInt64W
        JMP     @CvtError

@CvtInteger:
        LEA     ESI,StrBuf[32]
        MOV     EDX, Prec
        MOV     EBX, ECX
        CMP     EDX, 16
        JBE     @C1             // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@C1:    SUB     CL, 'D'
        JZ      CvtIntW          // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      CvtIntW
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      CvtIntW
        JMP     @CvtError

@CvtChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     EAX,ESI
        MOV     ECX,1
        JMP     @CvtAnsiThingLen

@CvtWideChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ECX,1
        RET

@CvtVariant:
        CMP     CL,'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType,varNull
        JBE     @CvtEmptyStr
        MOV     EDX,EAX
        LEA     EAX,TempWideStr
        CALL    WideFormatVarToStr
        MOV     ESI,TempWideStr
        JMP     @CvtWideStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOVZX   ECX,BYTE PTR [EAX]
        INC     EAX

@CvtAnsiThingLen:
        MOV     ESI,OFFSET System.@WStrFromPCharLen
        JMP     @CvtAnsiThing

@CvtPChar:
        MOV    ESI,OFFSET System.@WStrFromPChar
        JMP    @CvtAnsiThingTest

@CvtAnsiStr:
        MOV    ESI,OFFSET System.@WStrFromLStr

@CvtAnsiThingTest:
        CMP    CL,'S'
        JNE    @CvtError

@CvtAnsiThing:
        ADD    ESI, SaveGOT
{$IFDEF PIC}
        MOV    ESI, [ESI]
{$ENDIF}
        MOV    EDX,EAX
        LEA    EAX,TempWideStr
        PUSH   EBX
        MOV    EBX, SaveGOT
        CALL   ESI
        POP    EBX
        MOV    ESI,TempWideStr
        JMP    @CvtWideStrRef

@CvtWideString:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX

@CvtWideStrRef:
        OR      ESI,ESI
        JE      @CvtEmptyStr
        MOV     ECX,[ESI-4]
        SHR     ECX,1

@CvtWideStrLen:
        CMP     ECX,Prec
        JA      @E1
        RET
@E1:    MOV     ECX,Prec
        RET

@CvtPWideChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        PUSH    EDI
        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     ECX,Prec
        JECXZ   @F1
        REPNE   SCASW
        JNE     @F1
        DEC     EDI
        DEC     EDI
@F1:    MOV     ECX,EDI
        SUB     ECX,ESI
        SHR     ECX,1
        POP     EDI
        RET

@CvtPointer:
        CMP     CL,'P'
        JNE     @CvtError
        MOV     EDX,8
        MOV     ECX,16
        LEA     ESI,StrBuf[32]
        JMP     CvtInt

@CvtCurrency:
        MOV     BH,fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH,fvExtended

@CvtFloat:
        MOV     ESI,EAX
        MOV     BL,ffGeneral
        CMP     CL,'G'
        JE      @G2
        MOV     BL,ffExponent
        CMP     CL,'E'
        JE      @G2
        MOV     BL,ffFixed
        CMP     CL,'F'
        JE      @G1
        MOV     BL,ffNumber
        CMP     CL,'N'
        JE      @G1
        CMP     CL,'M'
        JNE     @CvtError
        MOV     BL,ffCurrency
@G1:    MOV     EAX,18
        MOV     EDX,Prec
        CMP     EDX,EAX
        JBE     @G3
        MOV     EDX,2
        CMP     CL,'M'
        JNE     @G3
        MOV     EDX,FormatSettings
        MOVZX   EDX,[EDX].TFormatSettings.CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX,Prec
        MOV     EDX,3
        CMP     EAX,18
        JBE     @G3
        MOV     EAX,15
@G3:    PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,[FormatSettings]
        PUSH    EDX
        LEA     EAX,StrBuf
        MOV     EDX,ESI
        MOVZX   ECX,BH
        MOV     EBX, SaveGOT
        CALL    FloatToTextEx
        MOV     ECX,EAX
        LEA     EAX,StrBuf
        JMP     @CvtAnsiThingLen

@ClearTmpWideStr:
        PUSH    EBX
        PUSH    EAX
        LEA     EAX,TempWideStr
        MOV     EBX, SaveGOT
        CALL    System.@WStrClr
        POP     EAX
        POP     EBX
        RET

@Exit:
        CALL    @ClearTmpWideStr
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure WideFmtStr(var Result: WideString; const Format: WideString;
  const Args: array of const);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of WideChar;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := WideFormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format), Args)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := WideFormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

procedure WideFmtStr(var Result: WideString; const Format: WideString;
  const Args: array of const; const FormatSettings: TFormatSettings);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of WideChar;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := WideFormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^,
      Length(Format), Args, FormatSettings)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := WideFormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args, FormatSettings);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

function WideFormat(const Format: WideString; const Args: array of const): WideString;
begin
  WideFmtStr(Result, Format, Args);
end;

function WideFormat(const Format: WideString; const Args: array of const;
  const FormatSettings: TFormatSettings): WideString;
begin
  WideFmtStr(Result, Format, Args, FormatSettings);
end;

{ Floating point conversion routines }

const
  // 1E18 as a 64-bit integer
  Const1E18Lo = $0A7640000;
  Const1E18Hi = $00DE0B6B3;
  FCon1E18: Extended = 1E18;
  DCon10: Integer = 10;

procedure PutExponent;
// Store exponent
// In   AL  = Exponent character ('E' or 'e')
//      AH  = Positive sign character ('+' or 0)
//      BL  = Zero indicator
//      ECX = Minimum number of digits (0..4)
//      EDX = Exponent
//      EDI = Destination buffer
asm
        PUSH    ESI
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     ESI,EAX
        POP     ECX
        POP     EAX
{$ELSE}
        XOR     ESI,ESI
{$ENDIF}
        STOSB
        OR      BL,BL
        JNE     @@0
        XOR     EDX,EDX
        JMP     @@1
@@0:    OR      EDX,EDX
        JGE     @@1
        MOV     AL,'-'
        NEG     EDX
        JMP     @@2
@@1:    OR      AH,AH
        JE      @@3
        MOV     AL,AH
@@2:    STOSB
@@3:    XCHG    EAX,EDX
        PUSH    EAX
        MOV     EBX,ESP
@@4:    XOR     EDX,EDX
        DIV     [ESI].DCon10
        ADD     DL,'0'
        MOV     [EBX],DL
        INC     EBX
        DEC     ECX
        OR      EAX,EAX
        JNE     @@4
        OR      ECX,ECX
        JG      @@4
@@5:    DEC     EBX
        MOV     AL,[EBX]
        STOSB
        CMP     EBX,ESP
        JNE     @@5
        POP     EAX
        POP     ESI
end;

function FloatToText(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer): Integer;
var
  Buffer: Cardinal;
  FloatRec: TFloatRec;
  SaveGOT: Integer;
  DecimalSep: Char;
  ThousandSep: Char;
  CurrencyStr: Pointer;
  CurrFmt: Byte;
  NegCurrFmt: Byte;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        MOV     SaveGOT,EAX
        MOV     ECX,[EAX].OFFSET DecimalSeparator
        MOV     CL,[ECX]
        MOV     DecimalSep,CL
        MOV     ECX,[EAX].OFFSET ThousandSeparator
        MOV     CL,[ECX].Byte
        MOV     ThousandSep,CL
        MOV     ECX,[EAX].OFFSET CurrencyString
        MOV     ECX,[ECX].Integer
        MOV     CurrencyStr,ECX
        MOV     ECX,[EAX].OFFSET CurrencyFormat
        MOV     CL,[ECX].Byte
        MOV     CurrFmt,CL
        MOV     ECX,[EAX].OFFSET NegCurrFormat
        MOV     CL,[ECX].Byte
        MOV     NegCurrFmt,CL
        POP     ECX
{$ELSE}
        MOV     AL,DecimalSeparator
        MOV     DecimalSep,AL
        MOV     AL,ThousandSeparator
        MOV     ThousandSep,AL
        MOV     EAX,CurrencyString
        MOV     CurrencyStr,EAX
        MOV     AL,CurrencyFormat
        MOV     CurrFmt,AL
        MOV     AL,NegCurrFormat
        MOV     NegCurrFmt,AL
        MOV     SaveGOT,0
{$ENDIF}
        MOV     EAX,19
        CMP     CL,fvExtended
        JNE     @@2
        MOV     EAX,Precision
        CMP     EAX,2
        JGE     @@1
        MOV     EAX,2
@@1:    CMP     EAX,18
        JLE     @@2
        MOV     EAX,18
@@2:    MOV     Precision,EAX
        PUSH    EAX
        MOV     EAX,9999
        CMP     Format,ffFixed
        JB      @@3
        MOV     EAX,Digits
@@3:    PUSH    EAX
        LEA     EAX,FloatRec
        CALL    FloatToDecimal
        MOV     EDI,Buffer
        MOVZX   EAX,FloatRec.Exponent
        SUB     EAX,7FFFH
        CMP     EAX,2
        JAE     @@4
        MOV     ECX, EAX
        CALL    @@PutSign
        LEA     ESI,@@INFNAN[ECX+ECX*2]
        ADD     ESI,SaveGOT
        MOV     ECX,3
        REP     MOVSB
        JMP     @@7
@@4:    LEA     ESI,FloatRec.Digits
        MOVZX   EBX,Format
        CMP     BL,ffExponent
        JE      @@6
        CMP     BL,ffCurrency
        JA      @@5
        MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,Precision
        JLE     @@6
@@5:    MOV     BL,ffGeneral
@@6:    LEA     EBX,@@FormatVector[EBX*4]
        ADD     EBX,SaveGOT
        MOV     EBX,[EBX]
        ADD     EBX,SaveGOT
        CALL    EBX
@@7:    MOV     EAX,EDI
        SUB     EAX,Buffer
        POP     EBX
        POP     ESI
        POP     EDI
        JMP     @@Exit

@@FormatVector:
        DD      @@PutFGeneral
        DD      @@PutFExponent
        DD      @@PutFFixed
        DD      @@PutFNumber
        DD      @@PutFCurrency

@@INFNAN: DB 'INFNAN'

// Get digit or '0' if at end of digit string

@@GetDigit:

        LODSB
        OR      AL,AL
        JNE     @@a1
        MOV     AL,'0'
        DEC     ESI
@@a1:   RET

// Store '-' if number is negative

@@PutSign:

        CMP     FloatRec.Negative,0
        JE      @@b1
        MOV     AL,'-'
        STOSB
@@b1:   RET

// Convert number using ffGeneral format

@@PutFGeneral:

        CALL    @@PutSign
        MOVSX   ECX,FloatRec.Exponent
        XOR     EDX,EDX
        CMP     ECX,Precision
        JG      @@c1
        CMP     ECX,-3
        JL      @@c1
        OR      ECX,ECX
        JG      @@c2
        MOV     AL,'0'
        STOSB
        CMP     BYTE PTR [ESI],0
        JE      @@c6
        MOV     AL,DecimalSep
        STOSB
        NEG     ECX
        MOV     AL,'0'
        REP     STOSB
        JMP     @@c3
@@c1:   MOV     ECX,1
        INC     EDX
@@c2:   LODSB
        OR      AL,AL
        JE      @@c4
        STOSB
        LOOP    @@c2
        LODSB
        OR      AL,AL
        JE      @@c5
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
@@c3:   LODSB
        OR      AL,AL
        JE      @@c5
        STOSB
        JMP     @@c3
@@c4:   MOV     AL,'0'
        REP     STOSB
@@c5:   OR      EDX,EDX
        JE      @@c6
        XOR     EAX,EAX
        JMP     @@PutFloatExpWithDigits
@@c6:   RET

// Convert number using ffExponent format

@@PutFExponent:

        CALL    @@PutSign
        CALL    @@GetDigit
        MOV     AH,DecimalSep
        STOSW
        MOV     ECX,Precision
        DEC     ECX
@@d1:   CALL    @@GetDigit
        STOSB
        LOOP    @@d1
        MOV     AH,'+'

@@PutFloatExpWithDigits:

        MOV     ECX,Digits
        CMP     ECX,4
        JBE     @@PutFloatExp
        XOR     ECX,ECX

// Store exponent
// In   AH  = Positive sign character ('+' or 0)
//      ECX = Minimum number of digits (0..4)

@@PutFloatExp:

        MOV     AL,'E'
        MOV     BL, FloatRec.Digits.Byte
        MOVSX   EDX,FloatRec.Exponent
        DEC     EDX
        CALL    PutExponent
        RET

// Convert number using ffFixed or ffNumber format

@@PutFFixed:
@@PutFNumber:

        CALL    @@PutSign

// Store number in fixed point format

@@PutNumber:

        MOV     EDX,Digits
        CMP     EDX,18
        JB      @@f1
        MOV     EDX,18
@@f1:   MOVSX   ECX,FloatRec.Exponent
        OR      ECX,ECX
        JG      @@f2
        MOV     AL,'0'
        STOSB
        JMP     @@f4
@@f2:   XOR     EBX,EBX
        CMP     Format,ffFixed
        JE      @@f3
        MOV     EAX,ECX
        DEC     EAX
        MOV     BL,3
        DIV     BL
        MOV     BL,AH
        INC     EBX
@@f3:   CALL    @@GetDigit
        STOSB
        DEC     ECX
        JE      @@f4
        DEC     EBX
        JNE     @@f3
        MOV     AL,ThousandSep
        TEST    AL,AL
        JZ      @@f3
        STOSB
        MOV     BL,3
        JMP     @@f3
@@f4:   OR      EDX,EDX
        JE      @@f7
        MOV     AL,DecimalSep
        TEST    AL,AL
        JZ      @@f4b
        STOSB
@@f4b:  JECXZ   @@f6
        MOV     AL,'0'
@@f5:   STOSB
        DEC     EDX
        JE      @@f7
        INC     ECX
        JNE     @@f5
@@f6:   CALL    @@GetDigit
        STOSB
        DEC     EDX
        JNE     @@f6
@@f7:   RET

// Convert number using ffCurrency format

@@PutFCurrency:

        XOR     EBX,EBX
        MOV     BL,CurrFmt.Byte
        MOV     ECX,0003H
        CMP     FloatRec.Negative,0
        JE      @@g1
        MOV     BL,NegCurrFmt.Byte
        MOV     ECX,040FH
@@g1:   CMP     BL,CL
        JBE     @@g2
        MOV     BL,CL
@@g2:   ADD     BL,CH
        LEA     EBX,@@MoneyFormats[EBX+EBX*4]
        ADD     EBX,SaveGOT
        MOV     ECX,5
@@g10:  MOV     AL,[EBX]
        CMP     AL,'@'
        JE      @@g14
        PUSH    ECX
        PUSH    EBX
        CMP     AL,'$'
        JE      @@g11
        CMP     AL,'*'
        JE      @@g12
        STOSB
        JMP     @@g13
@@g11:  CALL    @@PutCurSym
        JMP     @@g13
@@g12:  CALL    @@PutNumber
@@g13:  POP     EBX
        POP     ECX
        INC     EBX
        LOOP    @@g10
@@g14:  RET

// Store currency symbol string

@@PutCurSym:

        PUSH    ESI
        MOV     ESI,CurrencyStr
        TEST    ESI,ESI
        JE      @@h1
        MOV     ECX,[ESI-4]
        REP     MOVSB
@@h1:   POP     ESI
        RET

// Currency formatting templates

@@MoneyFormats:
        DB      '$*@@@'
        DB      '*$@@@'
        DB      '$ *@@'
        DB      '* $@@'
        DB      '($*)@'
        DB      '-$*@@'
        DB      '$-*@@'
        DB      '$*-@@'
        DB      '(*$)@'
        DB      '-*$@@'
        DB      '*-$@@'
        DB      '*$-@@'
        DB      '-* $@'
        DB      '-$ *@'
        DB      '* $-@'
        DB      '$ *-@'
        DB      '$ -*@'
        DB      '*- $@'
        DB      '($ *)'
        DB      '(* $)'

@@Exit:
end;

function FloatToText(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): Integer;
var
  Buffer: Cardinal;
  FloatRec: TFloatRec;
  SaveGOT: Integer;
  DecimalSep: Char;
  ThousandSep: Char;
  CurrencyStr: Pointer;
  CurrFmt: Byte;
  NegCurrFmt: Byte;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        MOV     SaveGOT,EAX
        POP     ECX
{$ENDIF}
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.DecimalSeparator
        MOV     DecimalSep,AL
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.ThousandSeparator
        MOV     ThousandSep,AL
        MOV     EAX,FormatSettings
        MOV     EAX,[EAX].TFormatSettings.CurrencyString
        MOV     CurrencyStr,EAX
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.CurrencyFormat
        MOV     CurrFmt,AL
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.NegCurrFormat
        MOV     NegCurrFmt,AL
        MOV     SaveGOT,0
        MOV     EAX,19
        CMP     CL,fvExtended
        JNE     @@2
        MOV     EAX,Precision
        CMP     EAX,2
        JGE     @@1
        MOV     EAX,2
@@1:    CMP     EAX,18
        JLE     @@2
        MOV     EAX,18
@@2:    MOV     Precision,EAX
        PUSH    EAX
        MOV     EAX,9999
        CMP     Format,ffFixed
        JB      @@3
        MOV     EAX,Digits
@@3:    PUSH    EAX
        LEA     EAX,FloatRec
        CALL    FloatToDecimal
        MOV     EDI,Buffer
        MOVZX   EAX,FloatRec.Exponent
        SUB     EAX,7FFFH
        CMP     EAX,2
        JAE     @@4
        MOV     ECX, EAX
        CALL    @@PutSign
        LEA     ESI,@@INFNAN[ECX+ECX*2]
        ADD     ESI,SaveGOT
        MOV     ECX,3
        REP     MOVSB
        JMP     @@7
@@4:    LEA     ESI,FloatRec.Digits
        MOVZX   EBX,Format
        CMP     BL,ffExponent
        JE      @@6
        CMP     BL,ffCurrency
        JA      @@5
        MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,Precision
        JLE     @@6
@@5:    MOV     BL,ffGeneral
@@6:    LEA     EBX,@@FormatVector[EBX*4]
        ADD     EBX,SaveGOT
        MOV     EBX,[EBX]
        ADD     EBX,SaveGOT
        CALL    EBX
@@7:    MOV     EAX,EDI
        SUB     EAX,Buffer
        POP     EBX
        POP     ESI
        POP     EDI
        JMP     @@Exit

@@FormatVector:
        DD      @@PutFGeneral
        DD      @@PutFExponent
        DD      @@PutFFixed
        DD      @@PutFNumber
        DD      @@PutFCurrency

@@INFNAN: DB 'INFNAN'

// Get digit or '0' if at end of digit string

@@GetDigit:

        LODSB
        OR      AL,AL
        JNE     @@a1
        MOV     AL,'0'
        DEC     ESI
@@a1:   RET

// Store '-' if number is negative

@@PutSign:

        CMP     FloatRec.Negative,0
        JE      @@b1
        MOV     AL,'-'
        STOSB
@@b1:   RET

// Convert number using ffGeneral format

@@PutFGeneral:

        CALL    @@PutSign
        MOVSX   ECX,FloatRec.Exponent
        XOR     EDX,EDX
        CMP     ECX,Precision
        JG      @@c1
        CMP     ECX,-3
        JL      @@c1
        OR      ECX,ECX
        JG      @@c2
        MOV     AL,'0'
        STOSB
        CMP     BYTE PTR [ESI],0
        JE      @@c6
        MOV     AL,DecimalSep
        STOSB
        NEG     ECX
        MOV     AL,'0'
        REP     STOSB
        JMP     @@c3
@@c1:   MOV     ECX,1
        INC     EDX
@@c2:   LODSB
        OR      AL,AL
        JE      @@c4
        STOSB
        LOOP    @@c2
        LODSB
        OR      AL,AL
        JE      @@c5
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
@@c3:   LODSB
        OR      AL,AL
        JE      @@c5
        STOSB
        JMP     @@c3
@@c4:   MOV     AL,'0'
        REP     STOSB
@@c5:   OR      EDX,EDX
        JE      @@c6
        XOR     EAX,EAX
        JMP     @@PutFloatExpWithDigits
@@c6:   RET

// Convert number using ffExponent format

@@PutFExponent:

        CALL    @@PutSign
        CALL    @@GetDigit
        MOV     AH,DecimalSep
        STOSW
        MOV     ECX,Precision
        DEC     ECX
@@d1:   CALL    @@GetDigit
        STOSB
        LOOP    @@d1
        MOV     AH,'+'

@@PutFloatExpWithDigits:

        MOV     ECX,Digits
        CMP     ECX,4
        JBE     @@PutFloatExp
        XOR     ECX,ECX

// Store exponent
// In   AH  = Positive sign character ('+' or 0)
//      ECX = Minimum number of digits (0..4)

@@PutFloatExp:

        MOV     AL,'E'
        MOV     BL, FloatRec.Digits.Byte
        MOVSX   EDX,FloatRec.Exponent
        DEC     EDX
        CALL    PutExponent
        RET

// Convert number using ffFixed or ffNumber format

@@PutFFixed:
@@PutFNumber:

        CALL    @@PutSign

// Store number in fixed point format

@@PutNumber:

        MOV     EDX,Digits
        CMP     EDX,18
        JB      @@f1
        MOV     EDX,18
@@f1:   MOVSX   ECX,FloatRec.Exponent
        OR      ECX,ECX
        JG      @@f2
        MOV     AL,'0'
        STOSB
        JMP     @@f4
@@f2:   XOR     EBX,EBX
        CMP     Format,ffFixed
        JE      @@f3
        MOV     EAX,ECX
        DEC     EAX
        MOV     BL,3
        DIV     BL
        MOV     BL,AH
        INC     EBX
@@f3:   CALL    @@GetDigit
        STOSB
        DEC     ECX
        JE      @@f4
        DEC     EBX
        JNE     @@f3
        MOV     AL,ThousandSep
        TEST    AL,AL
        JZ      @@f3
        STOSB
        MOV     BL,3
        JMP     @@f3
@@f4:   OR      EDX,EDX
        JE      @@f7
        MOV     AL,DecimalSep
        TEST    AL,AL
        JZ      @@f4b
        STOSB
@@f4b:  JECXZ   @@f6
        MOV     AL,'0'
@@f5:   STOSB
        DEC     EDX
        JE      @@f7
        INC     ECX
        JNE     @@f5
@@f6:   CALL    @@GetDigit
        STOSB
        DEC     EDX
        JNE     @@f6
@@f7:   RET

// Convert number using ffCurrency format

@@PutFCurrency:

        XOR     EBX,EBX
        MOV     BL,CurrFmt.Byte
        MOV     ECX,0003H
        CMP     FloatRec.Negative,0
        JE      @@g1
        MOV     BL,NegCurrFmt.Byte
        MOV     ECX,040FH
@@g1:   CMP     BL,CL
        JBE     @@g2
        MOV     BL,CL
@@g2:   ADD     BL,CH
        LEA     EBX,@@MoneyFormats[EBX+EBX*4]
        ADD     EBX,SaveGOT
        MOV     ECX,5
@@g10:  MOV     AL,[EBX]
        CMP     AL,'@'
        JE      @@g14
        PUSH    ECX
        PUSH    EBX
        CMP     AL,'$'
        JE      @@g11
        CMP     AL,'*'
        JE      @@g12
        STOSB
        JMP     @@g13
@@g11:  CALL    @@PutCurSym
        JMP     @@g13
@@g12:  CALL    @@PutNumber
@@g13:  POP     EBX
        POP     ECX
        INC     EBX
        LOOP    @@g10
@@g14:  RET

// Store currency symbol string

@@PutCurSym:

        PUSH    ESI
        MOV     ESI,CurrencyStr
        TEST    ESI,ESI
        JE      @@h1
        MOV     ECX,[ESI-4]
        REP     MOVSB
@@h1:   POP     ESI
        RET

// Currency formatting templates

@@MoneyFormats:
        DB      '$*@@@'
        DB      '*$@@@'
        DB      '$ *@@'
        DB      '* $@@'
        DB      '($*)@'
        DB      '-$*@@'
        DB      '$-*@@'
        DB      '$*-@@'
        DB      '(*$)@'
        DB      '-*$@@'
        DB      '*-$@@'
        DB      '*$-@@'
        DB      '-* $@'
        DB      '-$ *@'
        DB      '* $-@'
        DB      '$ *-@'
        DB      '$ -*@'
        DB      '*- $@'
        DB      '($ *)'
        DB      '(* $)'

@@Exit:
end;

function FloatToTextFmt(Buf: PChar; const Value; ValueType: TFloatValue;
  Format: PChar): Integer;

var
  Buffer: Pointer;
  ThousandSep: Boolean;
  DecimalSep: Char;
  ThousandsSep: Char;
  Scientific: Boolean;
  Section: Integer;
  DigitCount: Integer;
  DecimalIndex: Integer;
  FirstDigit: Integer;
  LastDigit: Integer;
  DigitPlace: Integer;
  DigitDelta: Integer;
  FloatRec: TFloatRec;
  SaveGOT: Pointer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     SaveGOT,EAX
        MOV     ECX,[EAX].OFFSET DecimalSeparator
        MOV     CL,[ECX].Byte
        MOV     DecimalSep,CL
        MOV     ECX,[EAX].OFFSET ThousandSeparator
        MOV     CL,[ECX].Byte
        MOV     ThousandsSep,CL
{$ELSE}
        MOV     SaveGOT,0
        MOV     AL,DecimalSeparator
        MOV     DecimalSep,AL
        MOV     AL,ThousandSeparator
        MOV     ThousandsSep,AL
{$ENDIF}
        MOV     ECX,2
        CMP     BL,fvExtended
        JE      @@1
        MOV     EAX,[EDI].Integer
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOV     ECX,[EDI].Integer[4]
        SHR     ECX,31
        JMP     @@2
@@1:    MOVZX   EAX,[EDI].Word[8]
        OR      EAX,[EDI].Integer[0]
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOVZX   ECX,[EDI].Word[8]
        SHR     ECX,15
@@2:    CALL    @@FindSection
        JE      @@5
        CALL    @@ScanSection
        MOV     EAX,DigitCount
        MOV     EDX,9999
        CMP     Scientific,0
        JNE     @@3
        SUB     EAX,DecimalIndex
        MOV     EDX,EAX
        MOV     EAX,18
@@3:    PUSH    EAX
        PUSH    EDX
        LEA     EAX,FloatRec
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    FloatToDecimal
        MOV     AX,FloatRec.Exponent
        CMP     AX,8000H
        JE      @@5
        CMP     AX,7FFFH
        JE      @@5
        CMP     BL,fvExtended
        JNE     @@6
        CMP     AX,18
        JLE     @@6
        CMP     Scientific,0
        JNE     @@6
@@5:    PUSH    ffGeneral
        PUSH    15
        PUSH    0
        MOV     EAX,Buffer
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    FloatToText
        JMP     @@Exit
@@6:    CMP     FloatRec.Digits.Byte,0
        JNE     @@7
        MOV     ECX,2
        CALL    @@FindSection
        JE      @@5
        CMP     ESI,Section
        JE      @@7
        CALL    @@ScanSection
@@7:    CALL    @@ApplyFormat
        JMP     @@Exit

// Find format section
// In   ECX = Section index
// Out  ESI = Section offset
//      ZF  = 1 if section is empty

@@FindSection:
        MOV     ESI,Format
        JECXZ   @@fs2
@@fs1:  LODSB
        CMP     AL,"'"
        JE      @@fs4
        CMP     AL,'"'
        JE      @@fs4
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs1
        LOOP    @@fs1
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs3
@@fs2:  MOV     ESI,Format
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs3
        CMP     AL,';'
@@fs3:  RET
@@fs4:  MOV     AH,AL
@@fs5:  LODSB
        CMP     AL,AH
        JE      @@fs1
        OR      AL,AL
        JNE     @@fs5
        JMP     @@fs2

// Scan format section

@@ScanSection:
        PUSH    EBX
        MOV     Section,ESI
        MOV     EBX,32767
        XOR     ECX,ECX
        XOR     EDX,EDX
        MOV     DecimalIndex,-1
        MOV     ThousandSep,DL
        MOV     Scientific,DL
@@ss1:  LODSB
@@ss2:  CMP     AL,'#'
        JE      @@ss10
        CMP     AL,'0'
        JE      @@ss11
        CMP     AL,'.'
        JE      @@ss13
        CMP     AL,','
        JE      @@ss14
        CMP     AL,"'"
        JE      @@ss15
        CMP     AL,'"'
        JE      @@ss15
        CMP     AL,'E'
        JE      @@ss20
        CMP     AL,'e'
        JE      @@ss20
        CMP     AL,';'
        JE      @@ss30
        OR      AL,AL
        JNE     @@ss1
        JMP     @@ss30
@@ss10: INC     EDX
        JMP     @@ss1
@@ss11: CMP     EDX,EBX
        JGE     @@ss12
        MOV     EBX,EDX
@@ss12: INC     EDX
        MOV     ECX,EDX
        JMP     @@ss1
@@ss13: CMP     DecimalIndex,-1
        JNE     @@ss1
        MOV     DecimalIndex,EDX
        JMP     @@ss1
@@ss14: MOV     ThousandSep,1
        JMP     @@ss1
@@ss15: MOV     AH,AL
@@ss16: LODSB
        CMP     AL,AH
        JE      @@ss1
        OR      AL,AL
        JNE     @@ss16
        JMP     @@ss30
@@ss20: LODSB
        CMP     AL,'-'
        JE      @@ss21
        CMP     AL,'+'
        JNE     @@ss2
@@ss21: MOV     Scientific,1
@@ss22: LODSB
        CMP     AL,'0'
        JE      @@ss22
        JMP     @@ss2
@@ss30: MOV     DigitCount,EDX
        CMP     DecimalIndex,-1
        JNE     @@ss31
        MOV     DecimalIndex,EDX
@@ss31: MOV     EAX,DecimalIndex
        SUB     EAX,ECX
        JLE     @@ss32
        XOR     EAX,EAX
@@ss32: MOV     LastDigit,EAX
        MOV     EAX,DecimalIndex
        SUB     EAX,EBX
        JGE     @@ss33
        XOR     EAX,EAX
@@ss33: MOV     FirstDigit,EAX
        POP     EBX
        RET

// Apply format string

@@ApplyFormat:
        CMP     Scientific,0
        JE      @@af1
        MOV     EAX,DecimalIndex
        XOR     EDX,EDX
        JMP     @@af3
@@af1:  MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,DecimalIndex
        JG      @@af2
        MOV     EAX,DecimalIndex
@@af2:  MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
@@af3:  MOV     DigitPlace,EAX
        MOV     DigitDelta,EDX
        MOV     ESI,Section
        MOV     EDI,Buffer
        LEA     EBX,FloatRec.Digits
        CMP     FloatRec.Negative,0
        JE      @@af10
        CMP     ESI,Format
        JNE     @@af10
        MOV     AL,'-'
        STOSB
@@af10: LODSB
        CMP     AL,'#'
        JE      @@af20
        CMP     AL,'0'
        JE      @@af20
        CMP     AL,'.'
        JE      @@af10
        CMP     AL,','
        JE      @@af10
        CMP     AL,"'"
        JE      @@af25
        CMP     AL,'"'
        JE      @@af25
        CMP     AL,'E'
        JE      @@af30
        CMP     AL,'e'
        JE      @@af30
        CMP     AL,';'
        JE      @@af40
        OR      AL,AL
        JE      @@af40
@@af11: STOSB
        JMP     @@af10
@@af20: CALL    @@PutFmtDigit
        JMP     @@af10
@@af25: MOV     AH,AL
@@af26: LODSB
        CMP     AL,AH
        JE      @@af10
        OR      AL,AL
        JE      @@af40
        STOSB
        JMP     @@af26
@@af30: MOV     AH,[ESI]
        CMP     AH,'+'
        JE      @@af31
        CMP     AH,'-'
        JNE     @@af11
        XOR     AH,AH
@@af31: MOV     ECX,-1
@@af32: INC     ECX
        INC     ESI
        CMP     [ESI].Byte,'0'
        JE      @@af32
        CMP     ECX,4
        JB      @@af33
        MOV     ECX,4
@@af33: PUSH    EBX
        MOV     BL,FloatRec.Digits.Byte
        MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
        CALL    PutExponent
        POP     EBX
        JMP     @@af10
@@af40: MOV     EAX,EDI
        SUB     EAX,Buffer
        RET

// Store formatted digit

@@PutFmtDigit:
        CMP     DigitDelta,0
        JE      @@fd3
        JL      @@fd2
@@fd1:  CALL    @@fd3
        DEC     DigitDelta
        JNE     @@fd1
        JMP     @@fd3
@@fd2:  INC     DigitDelta
        MOV     EAX,DigitPlace
        CMP     EAX,FirstDigit
        JLE     @@fd4
        JMP     @@fd7
@@fd3:  MOV     AL,[EBX]
        INC     EBX
        OR      AL,AL
        JNE     @@fd5
        DEC     EBX
        MOV     EAX,DigitPlace
        CMP     EAX,LastDigit
        JLE     @@fd7
@@fd4:  MOV     AL,'0'
@@fd5:  CMP     DigitPlace,0
        JNE     @@fd6
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
        JMP     @@fd7
@@fd6:  STOSB
        CMP     ThousandSep,0
        JE      @@fd7
        MOV     EAX,DigitPlace
        CMP     EAX,1
        JLE     @@fd7
        MOV     DL,3
        DIV     DL
        CMP     AH,1
        JNE     @@fd7
        MOV     AL,ThousandsSep
        TEST    AL,AL
        JZ      @@fd7
        STOSB
@@fd7:  DEC     DigitPlace
        RET

@@exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

function FloatToTextFmt(Buf: PChar; const Value; ValueType: TFloatValue;
  Format: PChar; const FormatSettings: TFormatSettings): Integer;

var
  Buffer: Pointer;
  ThousandSep: Boolean;
  DecimalSep: Char;
  ThousandsSep: Char;
  Scientific: Boolean;
  Section: Integer;
  DigitCount: Integer;
  DecimalIndex: Integer;
  FirstDigit: Integer;
  LastDigit: Integer;
  DigitPlace: Integer;
  DigitDelta: Integer;
  FloatRec: TFloatRec;
  SaveGOT: Pointer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     SaveGOT,EAX
{$ELSE}
        MOV     SaveGOT,0
{$ENDIF}
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.DecimalSeparator
        MOV     DecimalSep,AL
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.ThousandSeparator
        MOV     ThousandsSep,AL
        MOV     ECX,2
        CMP     BL,fvExtended
        JE      @@1
        MOV     EAX,[EDI].Integer
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOV     ECX,[EDI].Integer[4]
        SHR     ECX,31
        JMP     @@2
@@1:    MOVZX   EAX,[EDI].Word[8]
        OR      EAX,[EDI].Integer[0]
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOVZX   ECX,[EDI].Word[8]
        SHR     ECX,15
@@2:    CALL    @@FindSection
        JE      @@5
        CALL    @@ScanSection
        MOV     EAX,DigitCount
        MOV     EDX,9999
        CMP     Scientific,0
        JNE     @@3
        SUB     EAX,DecimalIndex
        MOV     EDX,EAX
        MOV     EAX,18
@@3:    PUSH    EAX
        PUSH    EDX
        LEA     EAX,FloatRec
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    FloatToDecimal
        MOV     AX,FloatRec.Exponent
        CMP     AX,8000H
        JE      @@5
        CMP     AX,7FFFH
        JE      @@5
        CMP     BL,fvExtended
        JNE     @@6
        CMP     AX,18
        JLE     @@6
        CMP     Scientific,0
        JNE     @@6
@@5:    PUSH    ffGeneral
        PUSH    15
        PUSH    0
        MOV     EAX,[FormatSettings]
        PUSH    EAX
        MOV     EAX,Buffer
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    FloatToTextEx
        JMP     @@Exit
@@6:    CMP     FloatRec.Digits.Byte,0
        JNE     @@7
        MOV     ECX,2
        CALL    @@FindSection
        JE      @@5
        CMP     ESI,Section
        JE      @@7
        CALL    @@ScanSection
@@7:    CALL    @@ApplyFormat
        JMP     @@Exit

// Find format section
// In   ECX = Section index
// Out  ESI = Section offset
//      ZF  = 1 if section is empty

@@FindSection:
        MOV     ESI,Format
        JECXZ   @@fs2
@@fs1:  LODSB
        CMP     AL,"'"
        JE      @@fs4
        CMP     AL,'"'
        JE      @@fs4
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs1
        LOOP    @@fs1
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs3
@@fs2:  MOV     ESI,Format
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs3
        CMP     AL,';'
@@fs3:  RET
@@fs4:  MOV     AH,AL
@@fs5:  LODSB
        CMP     AL,AH
        JE      @@fs1
        OR      AL,AL
        JNE     @@fs5
        JMP     @@fs2

// Scan format section

@@ScanSection:
        PUSH    EBX
        MOV     Section,ESI
        MOV     EBX,32767
        XOR     ECX,ECX
        XOR     EDX,EDX
        MOV     DecimalIndex,-1
        MOV     ThousandSep,DL
        MOV     Scientific,DL
@@ss1:  LODSB
@@ss2:  CMP     AL,'#'
        JE      @@ss10
        CMP     AL,'0'
        JE      @@ss11
        CMP     AL,'.'
        JE      @@ss13
        CMP     AL,','
        JE      @@ss14
        CMP     AL,"'"
        JE      @@ss15
        CMP     AL,'"'
        JE      @@ss15
        CMP     AL,'E'
        JE      @@ss20
        CMP     AL,'e'
        JE      @@ss20
        CMP     AL,';'
        JE      @@ss30
        OR      AL,AL
        JNE     @@ss1
        JMP     @@ss30
@@ss10: INC     EDX
        JMP     @@ss1
@@ss11: CMP     EDX,EBX
        JGE     @@ss12
        MOV     EBX,EDX
@@ss12: INC     EDX
        MOV     ECX,EDX
        JMP     @@ss1
@@ss13: CMP     DecimalIndex,-1
        JNE     @@ss1
        MOV     DecimalIndex,EDX
        JMP     @@ss1
@@ss14: MOV     ThousandSep,1
        JMP     @@ss1
@@ss15: MOV     AH,AL
@@ss16: LODSB
        CMP     AL,AH
        JE      @@ss1
        OR      AL,AL
        JNE     @@ss16
        JMP     @@ss30
@@ss20: LODSB
        CMP     AL,'-'
        JE      @@ss21
        CMP     AL,'+'
        JNE     @@ss2
@@ss21: MOV     Scientific,1
@@ss22: LODSB
        CMP     AL,'0'
        JE      @@ss22
        JMP     @@ss2
@@ss30: MOV     DigitCount,EDX
        CMP     DecimalIndex,-1
        JNE     @@ss31
        MOV     DecimalIndex,EDX
@@ss31: MOV     EAX,DecimalIndex
        SUB     EAX,ECX
        JLE     @@ss32
        XOR     EAX,EAX
@@ss32: MOV     LastDigit,EAX
        MOV     EAX,DecimalIndex
        SUB     EAX,EBX
        JGE     @@ss33
        XOR     EAX,EAX
@@ss33: MOV     FirstDigit,EAX
        POP     EBX
        RET

// Apply format string

@@ApplyFormat:
        CMP     Scientific,0
        JE      @@af1
        MOV     EAX,DecimalIndex
        XOR     EDX,EDX
        JMP     @@af3
@@af1:  MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,DecimalIndex
        JG      @@af2
        MOV     EAX,DecimalIndex
@@af2:  MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
@@af3:  MOV     DigitPlace,EAX
        MOV     DigitDelta,EDX
        MOV     ESI,Section
        MOV     EDI,Buffer
        LEA     EBX,FloatRec.Digits
        CMP     FloatRec.Negative,0
        JE      @@af10
        CMP     ESI,Format
        JNE     @@af10
        MOV     AL,'-'
        STOSB
@@af10: LODSB
        CMP     AL,'#'
        JE      @@af20
        CMP     AL,'0'
        JE      @@af20
        CMP     AL,'.'
        JE      @@af10
        CMP     AL,','
        JE      @@af10
        CMP     AL,"'"
        JE      @@af25
        CMP     AL,'"'
        JE      @@af25
        CMP     AL,'E'
        JE      @@af30
        CMP     AL,'e'
        JE      @@af30
        CMP     AL,';'
        JE      @@af40
        OR      AL,AL
        JE      @@af40
@@af11: STOSB
        JMP     @@af10
@@af20: CALL    @@PutFmtDigit
        JMP     @@af10
@@af25: MOV     AH,AL
@@af26: LODSB
        CMP     AL,AH
        JE      @@af10
        OR      AL,AL
        JE      @@af40
        STOSB
        JMP     @@af26
@@af30: MOV     AH,[ESI]
        CMP     AH,'+'
        JE      @@af31
        CMP     AH,'-'
        JNE     @@af11
        XOR     AH,AH
@@af31: MOV     ECX,-1
@@af32: INC     ECX
        INC     ESI
        CMP     [ESI].Byte,'0'
        JE      @@af32
        CMP     ECX,4
        JB      @@af33
        MOV     ECX,4
@@af33: PUSH    EBX
        MOV     BL,FloatRec.Digits.Byte
        MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
        CALL    PutExponent
        POP     EBX
        JMP     @@af10
@@af40: MOV     EAX,EDI
        SUB     EAX,Buffer
        RET

// Store formatted digit

@@PutFmtDigit:
        CMP     DigitDelta,0
        JE      @@fd3
        JL      @@fd2
@@fd1:  CALL    @@fd3
        DEC     DigitDelta
        JNE     @@fd1
        JMP     @@fd3
@@fd2:  INC     DigitDelta
        MOV     EAX,DigitPlace
        CMP     EAX,FirstDigit
        JLE     @@fd4
        JMP     @@fd7
@@fd3:  MOV     AL,[EBX]
        INC     EBX
        OR      AL,AL
        JNE     @@fd5
        DEC     EBX
        MOV     EAX,DigitPlace
        CMP     EAX,LastDigit
        JLE     @@fd7
@@fd4:  MOV     AL,'0'
@@fd5:  CMP     DigitPlace,0
        JNE     @@fd6
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
        JMP     @@fd7
@@fd6:  STOSB
        CMP     ThousandSep,0
        JE      @@fd7
        MOV     EAX,DigitPlace
        CMP     EAX,1
        JLE     @@fd7
        MOV     DL,3
        DIV     DL
        CMP     AH,1
        JNE     @@fd7
        MOV     AL,ThousandsSep
        TEST    AL,AL
        JZ      @@fd7
        STOSB
@@fd7:  DEC     DigitPlace
        RET

@@exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

const
// 8087 status word masks
  mIE = $0001;
  mDE = $0002;
  mZE = $0004;
  mOE = $0008;
  mUE = $0010;
  mPE = $0020;
  mC0 = $0100;
  mC1 = $0200;
  mC2 = $0400;
  mC3 = $4000;

procedure FloatToDecimal(var Result: TFloatRec; const Value;
  ValueType: TFloatValue; Precision, Decimals: Integer);
var
  StatWord: Word;
  Exponent: Integer;
  Temp: Double;
  BCDValue: Extended;
  SaveGOT: Pointer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EBX,EAX
        MOV     ESI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        MOV     SaveGOT,EAX
{$ELSE}
        MOV     SaveGOT,0
{$ENDIF}
        CMP     CL,fvExtended
        JE      @@1
        CALL    @@CurrToDecimal
        JMP     @@Exit
@@1:    CALL    @@ExtToDecimal
        JMP     @@Exit

// Convert Extended to decimal

@@ExtToDecimal:

        MOV     AX,[ESI].Word[8]
        MOV     EDX,EAX
        AND     EAX,7FFFH
        JE      @@ed1
        CMP     EAX,7FFFH
        JNE     @@ed10
// check for special values (INF, NAN)
        TEST    [ESI].Word[6],8000H
        JZ      @@ed2
// any significand bit set = NAN
// all significand bits clear = INF
        CMP     dword ptr [ESI], 0
        JNZ     @@ed0
        CMP     dword ptr [ESI+4], 80000000H
        JZ      @@ed2
@@ed0:  INC     EAX
@@ed1:  XOR     EDX,EDX
@@ed2:  MOV     [EBX].TFloatRec.Digits.Byte,0
        JMP     @@ed31
@@ed10: FLD     TBYTE PTR [ESI]
        SUB     EAX,3FFFH
        IMUL    EAX,19728
        SAR     EAX,16
        INC     EAX
        MOV     Exponent,EAX
        MOV     EAX,18
        SUB     EAX,Exponent
        FABS
        PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
        FRNDINT
        MOV     EDI,SaveGOT
        FLD     [EDI].FCon1E18
        FCOMP
        FSTSW   StatWord
        FWAIT
        TEST    StatWord,mC0+mC3
        JE      @@ed11
        FIDIV   [EDI].DCon10
        INC     Exponent
@@ed11: FBSTP   BCDValue
        LEA     EDI,[EBX].TFloatRec.Digits
        MOV     EDX,9
        FWAIT
@@ed12: MOV     AL,BCDValue[EDX-1].Byte
        MOV     AH,AL
        SHR     AL,4
        AND     AH,0FH
        ADD     AX,'00'
        STOSW
        DEC     EDX
        JNE     @@ed12
        XOR     AL,AL
        STOSB
@@ed20: MOV     EDI,Exponent
        ADD     EDI,Decimals
        JNS     @@ed21
        XOR     EAX,EAX
        JMP     @@ed1
@@ed21: CMP     EDI,Precision
        JB      @@ed22
        MOV     EDI,Precision
@@ed22: CMP     EDI,18
        JAE     @@ed26
        CMP     [EBX].TFloatRec.Digits.Byte[EDI],'5'
        JB      @@ed25
@@ed23: MOV     [EBX].TFloatRec.Digits.Byte[EDI],0
        DEC     EDI
        JS      @@ed24
        INC     [EBX].TFloatRec.Digits.Byte[EDI]
        CMP     [EBX].TFloatRec.Digits.Byte[EDI],'9'
        JA      @@ed23
        JMP     @@ed30
@@ed24: MOV     [EBX].TFloatRec.Digits.Word,'1'
        INC     Exponent
        JMP     @@ed30
@@ed26: MOV     EDI,18
@@ed25: MOV     [EBX].TFloatRec.Digits.Byte[EDI],0
        DEC     EDI
        JS      @@ed32
        CMP     [EBX].TFloatRec.Digits.Byte[EDI],'0'
        JE      @@ed25
@@ed30: MOV     DX,[ESI].Word[8]
@@ed30a:
        MOV     EAX,Exponent
@@ed31: SHR     DX,15
        MOV     [EBX].TFloatRec.Exponent,AX
        MOV     [EBX].TFloatRec.Negative,DL
        RET
@@ed32: XOR     EDX,EDX
        JMP     @@ed30a

@@DecimalTable:
        DD      10
        DD      100
        DD      1000
        DD      10000

// Convert Currency to decimal

@@CurrToDecimal:

        MOV     EAX,[ESI].Integer[0]
        MOV     EDX,[ESI].Integer[4]
        MOV     ECX,EAX
        OR      ECX,EDX
        JE      @@cd20
        OR      EDX,EDX
        JNS     @@cd1
        NEG     EDX
        NEG     EAX
        SBB     EDX,0
@@cd1:  XOR     ECX,ECX
        MOV     EDI,Decimals
        OR      EDI,EDI
        JGE     @@cd2
        XOR     EDI,EDI
@@cd2:  CMP     EDI,4
        JL      @@cd4
        MOV     EDI,4
@@cd3:  INC     ECX
        SUB     EAX,Const1E18Lo
        SBB     EDX,Const1E18Hi
        JNC     @@cd3
        DEC     ECX
        ADD     EAX,Const1E18Lo
        ADC     EDX,Const1E18Hi
@@cd4:  MOV     Temp.Integer[0],EAX
        MOV     Temp.Integer[4],EDX
        FILD    Temp
        MOV     EDX,EDI
        MOV     EAX,4
        SUB     EAX,EDX
        JE      @@cd5
        MOV     EDI,SaveGOT
        FIDIV   @@DecimalTable.Integer[EDI+EAX*4-4]
@@cd5:  FBSTP   BCDValue
        LEA     EDI,[EBX].TFloatRec.Digits
        FWAIT
        OR      ECX,ECX
        JNE     @@cd11
        MOV     ECX,9
@@cd10: MOV     AL,BCDValue[ECX-1].Byte
        MOV     AH,AL
        SHR     AL,4
        JNE     @@cd13
        MOV     AL,AH
        AND     AL,0FH
        JNE     @@cd14
        DEC     ECX
        JNE     @@cd10
        JMP     @@cd20
@@cd11: MOV     AL,CL
        ADD     AL,'0'
        STOSB
        MOV     ECX,9
@@cd12: MOV     AL,BCDValue[ECX-1].Byte
        MOV     AH,AL
        SHR     AL,4
@@cd13: ADD     AL,'0'
        STOSB
        MOV     AL,AH
        AND     AL,0FH
@@cd14: ADD     AL,'0'
        STOSB
        DEC     ECX
        JNE     @@cd12
        MOV     EAX,EDI
        LEA     ECX,[EBX].TFloatRec.Digits[EDX]
        SUB     EAX,ECX
@@cd15: MOV     BYTE PTR [EDI],0
        DEC     EDI
        CMP     BYTE PTR [EDI],'0'
        JE      @@cd15
        MOV     EDX,[ESI].Integer[4]
        SHR     EDX,31
        JMP     @@cd21
@@cd20: XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     [EBX].TFloatRec.Digits.Byte[0],AL
@@cd21: MOV     [EBX].TFloatRec.Exponent,AX
        MOV     [EBX].TFloatRec.Negative,DL
        RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue): Boolean;

const
// 8087 control word
// Infinity control  = 1 Affine
// Rounding Control  = 0 Round to nearest or even
// Precision Control = 3 64 bits
// All interrupts masked
  CWNear: Word = $133F;

var
  Temp: Integer;
  CtrlWord: Word;
  DecimalSep: Char;
  SaveGOT: Integer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     EBX
        MOV     SaveGOT,EAX
        MOV     ECX,[EAX].OFFSET DecimalSeparator
        MOV     CL,[ECX].Byte
        MOV     DecimalSep,CL
{$ELSE}
        MOV     SaveGOT,0
        MOV     AL,DecimalSeparator
        MOV     DecimalSep,AL
        MOV     EBX,ECX
{$ENDIF}
        FSTCW   CtrlWord
        FCLEX
{$IFDEF PIC}
        FLDCW   [EAX].CWNear
{$ELSE}
        FLDCW   CWNear
{$ENDIF}
        FLDZ
        CALL    @@SkipBlanks
        MOV     BH, byte ptr [ESI]
        CMP     BH,'+'
        JE      @@1
        CMP     BH,'-'
        JNE     @@2
@@1:    INC     ESI
@@2:    MOV     ECX,ESI
        CALL    @@GetDigitStr
        XOR     EDX,EDX
        MOV     AL,[ESI]
        CMP     AL,DecimalSep
        JNE     @@3
        INC     ESI
        CALL    @@GetDigitStr
        NEG     EDX
@@3:    CMP     ECX,ESI
        JE      @@9
        MOV     AL, byte ptr [ESI]
        AND     AL,0DFH
        CMP     AL,'E'
        JNE     @@4
        INC     ESI
        PUSH    EDX
        CALL    @@GetExponent
        POP     EAX
        ADD     EDX,EAX
@@4:    CALL    @@SkipBlanks
        CMP     BYTE PTR [ESI],0
        JNE     @@9
        MOV     EAX,EDX
        CMP     BL,fvCurrency
        JNE     @@5
        ADD     EAX,4
@@5:    PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
        CMP     BH,'-'
        JNE     @@6
        FCHS
@@6:    CMP     BL,fvExtended
        JE      @@7
        FISTP   QWORD PTR [EDI]
        JMP     @@8
@@7:    FSTP    TBYTE PTR [EDI]
@@8:    FSTSW   AX
        TEST    AX,mIE+mOE
        JNE     @@10
        MOV     AL,1
        JMP     @@11
@@9:    FSTP    ST(0)
@@10:   XOR     EAX,EAX
@@11:   FCLEX
        FLDCW   CtrlWord
        FWAIT
        JMP     @@Exit

@@SkipBlanks:

@@21:   LODSB
        OR      AL,AL
        JE      @@22
        CMP     AL,' '
        JE      @@21
@@22:   DEC     ESI
        RET

// Process string of digits
// Out EDX = Digit count

@@GetDigitStr:

        XOR     EAX,EAX
        XOR     EDX,EDX
@@31:   LODSB
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@32
{$IFDEF PIC}
        XCHG    SaveGOT,EBX
        FIMUL   [EBX].DCon10
        XCHG    SaveGOT,EBX
{$ELSE}
        FIMUL   DCon10
{$ENDIF}
        MOV     Temp,EAX
        FIADD   Temp
        INC     EDX
        JMP     @@31
@@32:   DEC     ESI
        RET

// Get exponent
// Out EDX = Exponent (-4999..4999)

@@GetExponent:

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     CL, byte ptr [ESI]
        CMP     CL,'+'
        JE      @@41
        CMP     CL,'-'
        JNE     @@42
@@41:   INC     ESI
@@42:   MOV     AL, byte ptr [ESI]
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@43
        INC     ESI
        IMUL    EDX,10
        ADD     EDX,EAX
        CMP     EDX,500
        JB      @@42
@@43:   CMP     CL,'-'
        JNE     @@44
        NEG     EDX
@@44:   RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue; const FormatSettings: TFormatSettings): Boolean;

const
// 8087 control word
// Infinity control  = 1 Affine
// Rounding Control  = 0 Round to nearest or even
// Precision Control = 3 64 bits
// All interrupts masked
  CWNear: Word = $133F;

var
  Temp: Integer;
  CtrlWord: Word;
  DecimalSep: Char;
  SaveGOT: Integer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     EBX
        MOV     SaveGOT,EAX
{$ELSE}
        MOV     SaveGOT,0
        MOV     EBX,ECX
{$ENDIF}
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.DecimalSeparator
        MOV     DecimalSep,AL
        FSTCW   CtrlWord
        FCLEX
{$IFDEF PIC}
        FLDCW   [EAX].CWNear
{$ELSE}
        FLDCW   CWNear
{$ENDIF}
        FLDZ
        CALL    @@SkipBlanks
        MOV     BH, byte ptr [ESI]
        CMP     BH,'+'
        JE      @@1
        CMP     BH,'-'
        JNE     @@2
@@1:    INC     ESI
@@2:    MOV     ECX,ESI
        CALL    @@GetDigitStr
        XOR     EDX,EDX
        MOV     AL,[ESI]
        CMP     AL,DecimalSep
        JNE     @@3
        INC     ESI
        CALL    @@GetDigitStr
        NEG     EDX
@@3:    CMP     ECX,ESI
        JE      @@9
        MOV     AL, byte ptr [ESI]
        AND     AL,0DFH
        CMP     AL,'E'
        JNE     @@4
        INC     ESI
        PUSH    EDX
        CALL    @@GetExponent
        POP     EAX
        ADD     EDX,EAX
@@4:    CALL    @@SkipBlanks
        CMP     BYTE PTR [ESI],0
        JNE     @@9
        MOV     EAX,EDX
        CMP     BL,fvCurrency
        JNE     @@5
        ADD     EAX,4
@@5:    PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
        CMP     BH,'-'
        JNE     @@6
        FCHS
@@6:    CMP     BL,fvExtended
        JE      @@7
        FISTP   QWORD PTR [EDI]
        JMP     @@8
@@7:    FSTP    TBYTE PTR [EDI]
@@8:    FSTSW   AX
        TEST    AX,mIE+mOE
        JNE     @@10
        MOV     AL,1
        JMP     @@11
@@9:    FSTP    ST(0)
@@10:   XOR     EAX,EAX
@@11:   FCLEX
        FLDCW   CtrlWord
        FWAIT
        JMP     @@Exit

@@SkipBlanks:

@@21:   LODSB
        OR      AL,AL
        JE      @@22
        CMP     AL,' '
        JE      @@21
@@22:   DEC     ESI
        RET

// Process string of digits
// Out EDX = Digit count

@@GetDigitStr:

        XOR     EAX,EAX
        XOR     EDX,EDX
@@31:   LODSB
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@32
{$IFDEF PIC}
        XCHG    SaveGOT,EBX
        FIMUL   [EBX].DCon10
        XCHG    SaveGOT,EBX
{$ELSE}
        FIMUL   DCon10
{$ENDIF}
        MOV     Temp,EAX
        FIADD   Temp
        INC     EDX
        JMP     @@31
@@32:   DEC     ESI
        RET

// Get exponent
// Out EDX = Exponent (-4999..4999)

@@GetExponent:

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     CL, byte ptr [ESI]
        CMP     CL,'+'
        JE      @@41
        CMP     CL,'-'
        JNE     @@42
@@41:   INC     ESI
@@42:   MOV     AL, byte ptr [ESI]
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@43
        INC     ESI
        IMUL    EDX,10
        ADD     EDX,EAX
        CMP     EDX,500
        JB      @@42
@@43:   CMP     CL,'-'
        JNE     @@44
        NEG     EDX
@@44:   RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

function FloatToStr(Value: Extended): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0));
end;

function FloatToStr(Value: Extended;
  const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0, FormatSettings));
end;

function CurrToStr(Value: Currency): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    ffGeneral, 0, 0));
end;

function CurrToStr(Value: Currency;
  const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    ffGeneral, 0, 0, FormatSettings));
end;

function TryFloatToCurr(const Value: Extended; out AResult: Currency): Boolean;
begin
  Result := (Value >= MinCurrency) and (Value <= MaxCurrency);
  if Result then
    AResult := Value;
end;

function FloatToCurr(const Value: Extended): Currency;
begin
  if not TryFloatToCurr(Value, Result) then
    ConvertErrorFmt(@SInvalidCurrency, [FloatToStr(Value)]);
end;

function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    Format, Precision, Digits));
end;

function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer; const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    Format, Precision, Digits, FormatSettings));
end;

function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    Format, 0, Digits));
end;

function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer; const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    Format, 0, Digits, FormatSettings));
end;

function FormatFloat(const Format: string; Value: Extended): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(@SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvExtended,
    PChar(Format)));
end;

function FormatFloat(const Format: string; Value: Extended;
  const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(@SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvExtended,
    PChar(Format), FormatSettings));
end;

function FormatCurr(const Format: string; Value: Currency): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(@SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvCurrency,
    PChar(Format)));
end;

function FormatCurr(const Format: string; Value: Currency;
  const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(@SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvCurrency,
    PChar(Format), FormatSettings));
end;

function StrToFloat(const S: string): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    ConvertErrorFmt(@SInvalidFloat, [S]);
end;

function StrToFloat(const S: string;
  const FormatSettings: TFormatSettings): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended, FormatSettings) then
    ConvertErrorFmt(@SInvalidFloat, [S]);
end;

function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;

function StrToFloatDef(const S: string; const Default: Extended;
  const FormatSettings: TFormatSettings): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended, FormatSettings) then
    Result := Default;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function TryStrToFloat(const S: string; out Value: Extended;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended, FormatSettings);
end;

function TryStrToFloat(const S: string; out Value: Double): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended);
  if Result then
    Value := LValue;
end;

function TryStrToFloat(const S: string; out Value: Double;
  const FormatSettings: TFormatSettings): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended, FormatSettings);
  if Result then
    Value := LValue;
end;

function TryStrToFloat(const S: string; out Value: Single): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended);
  if Result then
    Value := LValue;
end;

function TryStrToFloat(const S: string; out Value: Single;
  const FormatSettings: TFormatSettings): Boolean;
var
  LValue: Extended;
begin
  Result := TextToFloat(PChar(S), LValue, fvExtended, FormatSettings);
  if Result then
    Value := LValue;
end;

function StrToCurr(const S: string): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency) then
    ConvertErrorFmt(@SInvalidFloat, [S]);
end;

function StrToCurr(const S: string;
  const FormatSettings: TFormatSettings): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency, FormatSettings) then
    ConvertErrorFmt(@SInvalidFloat, [S]);
end;

function StrToCurrDef(const S: string; const Default: Currency): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency) then
    Result := Default;
end;

function StrToCurrDef(const S: string; const Default: Currency;
  const FormatSettings: TFormatSettings): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency, FormatSettings) then
    Result := Default;
end;

function TryStrToCurr(const S: string; out Value: Currency): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvCurrency);
end;

function TryStrToCurr(const S: string; out Value: Currency;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvCurrency, FormatSettings);
end;

{ Date/time support routines }

const
  FMSecsPerDay: Single = MSecsPerDay;
  IMSecsPerDay: Integer = MSecsPerDay;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
asm
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE}
        XOR     EBX,EBX
{$ENDIF}
        MOV     ECX,EAX
        FLD     DateTime
        FMUL    [EBX].FMSecsPerDay
        SUB     ESP,8
        FISTP   QWORD PTR [ESP]
        FWAIT
        POP     EAX
        POP     EDX
        OR      EDX,EDX
        JNS     @@1
        NEG     EDX
        NEG     EAX
        SBB     EDX,0
        DIV     [EBX].IMSecsPerDay
        NEG     EAX
        JMP     @@2
@@1:    DIV     [EBX].IMSecsPerDay
@@2:    ADD     EAX,DateDelta
        MOV     [ECX].TTimeStamp.Time,EDX
        MOV     [ECX].TTimeStamp.Date,EAX
        POP     EBX
end;

procedure ValidateTimeStamp(const TimeStamp: TTimeStamp);
begin
  if (TimeStamp.Time < 0) or (TimeStamp.Date <= 0) then
    ConvertErrorFmt(@SInvalidTimeStamp, [TimeStamp.Date, TimeStamp.Time]);
end;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
asm
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE}
        XOR     EBX,EBX
{$ENDIF}
        PUSH    EAX
        CALL    ValidateTimeStamp
        POP     EAX
        MOV     ECX,[EAX].TTimeStamp.Time
        MOV     EAX,[EAX].TTimeStamp.Date
        SUB     EAX,DateDelta
        IMUL    [EBX].IMSecsPerDay
        OR      EDX,EDX
        JNS     @@1
        SUB     EAX,ECX
        SBB     EDX,0
        JMP     @@2
@@1:    ADD     EAX,ECX
        ADC     EDX,0
@@2:    PUSH    EDX
        PUSH    EAX
        FILD    QWORD PTR [ESP]
        FDIV    [EBX].FMSecsPerDay
        ADD     ESP,8
        POP     EBX
end;

function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
asm
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE}
        XOR     EBX,EBX
{$ENDIF}
        MOV     ECX,EAX
        MOV     EAX,MSecs.Integer[0]
        MOV     EDX,MSecs.Integer[4]
        DIV     [EBX].IMSecsPerDay
        MOV     [ECX].TTimeStamp.Time,EDX
        MOV     [ECX].TTimeStamp.Date,EAX
        POP     EBX
end;

function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp;
asm
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE}
        XOR     EBX,EBX
{$ENDIF}
        PUSH    EAX
        CALL    ValidateTimeStamp
        POP     EAX
        FILD    [EAX].TTimeStamp.Date
        FMUL    [EBX].FMSecsPerDay
        FIADD   [EAX].TTimeStamp.Time
        POP     EBX
end;

{ Time encoding and decoding }

function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < HoursPerDay) and (Min < MinsPerHour) and (Sec < SecsPerMin) and (MSec < MSecsPerSec) then
  begin
    Time := (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             Min * (SecsPerMin * MSecsPerSec) +
             Sec * MSecsPerSec +
             MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  if not TryEncodeTime(Hour, Min, Sec, MSec, Result) then
    ConvertError(@STimeEncodeError);
end;

procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
var
  MinCount, MSecCount: Word;
begin
  DivMod(DateTimeToTimeStamp(DateTime).Time, SecsPerMin * MSecsPerSec, MinCount, MSecCount);
  DivMod(MinCount, MinsPerHour, Hour, Min);
  DivMod(MSecCount, MSecsPerSec, Sec, MSec);
end;

{ Date encoding and decoding }

function IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  if not TryEncodeDate(Year, Month, Day, Result) then
    ConvertError(@SDateEncodeError);
end;

function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(DateTime).Date;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
    Result := False;
  end else
  begin
    DOW := T mod 7 + 1;
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    Result := IsLeapYear(Y);
    DayTable := @MonthDays[Result];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

function InternalDecodeDate(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
begin
  Result := DecodeDateFully(DateTime, Year, Month, Day, DOW);
  Dec(DOW);
end;

procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
var
  Dummy: Word;
begin
  DecodeDateFully(DateTime, Year, Month, Day, Dummy);
end;

{$IFDEF MSWINDOWS}
procedure DateTimeToSystemTime(const DateTime: TDateTime; var SystemTime: TSystemTime);
begin
  with SystemTime do
  begin
    DecodeDateFully(DateTime, wYear, wMonth, wDay, wDayOfWeek);
    Dec(wDayOfWeek);
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
end;

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  with SystemTime do
  begin
    Result := EncodeDate(wYear, wMonth, wDay);
    if Result >= 0 then
      Result := Result + EncodeTime(wHour, wMinute, wSecond, wMilliSeconds)
    else
      Result := Result - EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
  end;
end;
{$ENDIF}

function DayOfWeek(const DateTime: TDateTime): Word;
begin
  Result := DateTimeToTimeStamp(DateTime).Date mod 7 + 1;
end;

function Date: TDateTime;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do Result := EncodeDate(wYear, wMonth, wDay);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  T: TTime_T;
  UT: TUnixTime;
begin
  __time(@T);
  localtime_r(@T, UT);
  Result := EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday);
end;
{$ENDIF}

function Time: TDateTime;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  T: TTime_T;
  TV: TTimeVal;
  UT: TUnixTime;
begin
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(@T, UT);
  Result := EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, TV.tv_usec div 1000);
end;
{$ENDIF}

function GetTime: TDateTime;
begin
  Result := Time;
end;

function Now: TDateTime;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  T: TTime_T;
  TV: TTimeVal;
  UT: TUnixTime;
begin
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(@T, UT);
  Result := EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) +
    EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, TV.tv_usec div 1000);
end;
{$ENDIF}

function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  IncAMonth(Year, Month, Day, NumberOfMonths);
  Result := EncodeDate(Year, Month, Day);
  ReplaceTime(Result, DateTime);
end;

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
var
  DayTable: PDayTable;
  Sign: Integer;
begin
  if NumberOfMonths >= 0 then Sign := 1 else Sign := -1;
  Year := Year + (NumberOfMonths div 12);
  NumberOfMonths := NumberOfMonths mod 12;
  Inc(Month, NumberOfMonths);
  if Word(Month-1) > 11 then    // if Month <= 0, word(Month-1) > 11)
  begin
    Inc(Year, Sign);
    Inc(Month, -12 * Sign);
  end;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if Day > DayTable^[Month] then Day := DayTable^[Month];
end;

procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
begin
  DateTime := Trunc(DateTime);
  if DateTime >= 0 then
    DateTime := DateTime + Abs(Frac(NewTime))
  else
    DateTime := DateTime - Abs(Frac(NewTime));
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
var
  Temp: TDateTime;
begin
  Temp := NewDate;
  ReplaceTime(Temp, DateTime);
  DateTime := Temp;
end;

function CurrentYear: Word;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  T: TTime_T;
  UT: TUnixTime;
begin
  __time(@T);
  localtime_r(@T, UT);
  Result := UT.tm_year + 1900;
end;
{$ENDIF}

{ Date/time to string conversions }

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := SizeOf(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N);
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, SizeOf(NumBuf), Format,
      SizeOf(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

{$IFDEF MSWINDOWS}
    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;
{$ENDIF}

{$IFDEF LINUX}
    function FindEra(Date: Integer): Byte;
    var
      I : Byte;
    begin
      Result := 0;
      for I := 1 to EraCount do
      begin
        if (EraRanges[I].StartDate <= Date) and
          (EraRanges[I].EndDate >= Date) then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;

    function ConvertEraString(const Count: Integer) : String;
    var
      I : Byte;
    begin
      Result := '';
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        Result := EraNames[I];
    end;

    function ConvertYearString(const Count: Integer) : String;
    var
      I : Byte;
      S : string;
    begin
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        S := IntToStr(Year - EraYearOffsets[I])
      else
        S := IntToStr(Year);
      while Length(S) < Count do
        S := '0' + S;
      if Length(S) > Count then
        S := Copy(S, Length(S) - (Count - 1), Count);
      Result := S;
    end;
{$ENDIF}

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        if Starter in LeadBytes then
        begin
          AppendChars(Format, StrCharLength(Format));
          Format := StrNextChar(Format);
          LastToken := ' ';
          Continue;
        end;
        Format := StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(ShortMonthNames[Month]);
              else
                AppendString(LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(ShortDayNames[DayOfWeek(DateTime)]);
                4: AppendString(LongDayNames[DayOfWeek(DateTime)]);
                5: AppendFormat(Pointer(ShortDateFormat));
              else
                AppendFormat(Pointer(LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if P^ in LeadBytes then
                begin
                  P := StrNextChar(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(ShortTimeFormat)) else
                AppendFormat(Pointer(LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(TimeAMString) else
                  AppendString(TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(LongDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(ShortDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(LongTimeFormat));
              end;
            end;
          '/':
            if DateSeparator <> #0 then
              AppendChars(@DateSeparator, 1);
          ':':
            if TimeSeparator <> #0 then
              AppendChars(@TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if Format^ in LeadBytes then
                  Format := StrNextChar(Format)
                else
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  SetString(Result, Buffer, BufPos);
end;

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime; const FormatSettings: TFormatSettings);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := SizeOf(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N);
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, SizeOf(NumBuf), Format,
      SizeOf(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

{$IFDEF MSWINDOWS}
    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;
{$ENDIF}

{$IFDEF LINUX}
    function FindEra(Date: Integer): Byte;
    var
      I : Byte;
    begin
      Result := 0;
      for I := 1 to EraCount do
      begin
        if (EraRanges[I].StartDate <= Date) and
          (EraRanges[I].EndDate >= Date) then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;

    function ConvertEraString(const Count: Integer) : String;
    var
      I : Byte;
    begin
      Result := '';
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        Result := EraNames[I];
    end;

    function ConvertYearString(const Count: Integer) : String;
    var
      I : Byte;
      S : string;
    begin
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        S := IntToStr(Year - EraYearOffsets[I])
      else
        S := IntToStr(Year);
      while Length(S) < Count do
        S := '0' + S;
      if Length(S) > Count then
        S := Copy(S, Length(S) - (Count - 1), Count);
      Result := S;
    end;
{$ENDIF}

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        if Starter in LeadBytes then
        begin
          AppendChars(Format, StrCharLength(Format));
          Format := StrNextChar(Format);
          LastToken := ' ';
          Continue;
        end;
        Format := StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(FormatSettings.ShortMonthNames[Month]);
              else
                AppendString(FormatSettings.LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                4: AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                5: AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              else
                AppendFormat(Pointer(FormatSettings.LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if P^ in LeadBytes then
                begin
                  P := StrNextChar(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(FormatSettings.ShortTimeFormat)) else
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(FormatSettings.TimeAMString) else
                  AppendString(FormatSettings.TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
              end;
            end;
          '/':
            if DateSeparator <> #0 then
              AppendChars(@FormatSettings.DateSeparator, 1);
          ':':
            if TimeSeparator <> #0 then
              AppendChars(@FormatSettings.TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if Format^ in LeadBytes then
                  Format := StrNextChar(Format)
                else
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  SetString(Result, Buffer, BufPos);
end;

function TryFloatToDateTime(const Value: Extended; out AResult: TDateTime): Boolean;
begin
  Result := not ((Value < MinDateTime) or (Value >= Int(MaxDateTime) + 1.0));
  if Result then
    AResult := Value;
end;

function FloatToDateTime(const Value: Extended): TDateTime;
begin
  if not TryFloatToDateTime(Value, Result) then
    ConvertErrorFmt(@SInvalidDateTimeFloat, [Value]);
end;

function DateToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, ShortDateFormat, DateTime);
end;

function DateToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, FormatSettings.ShortDateFormat, DateTime,
    FormatSettings);
end;

function TimeToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, LongTimeFormat, DateTime);
end;

function TimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, FormatSettings.LongTimeFormat, DateTime,
    FormatSettings);
end;

function DateTimeToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, '', DateTime);
end;

function DateTimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, '', DateTime, FormatSettings);
end;

function FormatDateTime(const Format: string; DateTime: TDateTime): string;
begin
  DateTimeToString(Result, Format, DateTime);
end;

function FormatDateTime(const Format: string; DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, Format, DateTime, FormatSettings);
end;

{ String to date/time conversions }

type
  TDateOrder = (doMDY, doDMY, doYMD);

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; var Pos: Integer;
  var Number: Word; var CharCount: Byte): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanString(const S: string; var Pos: Integer;
  const Symbol: string): Boolean;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ScanBlanks(S, Pos);
    if AnsiCompareText(Symbol, Copy(S, Pos, Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
	Result := False;
	if Ch <> ' ' then // FIX time format without ":" (Safrad 2007-05-20)
		ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := doMDY;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
  Result := doMDY;
end;

procedure ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not (S[Pos] in ['0'..'9']) do
  begin
    if S[Pos] in LeadBytes then
      Pos := NextCharIndex(S, Pos)
    else
      Inc(Pos);
  end;
end;

function GetEraYearOffset(const Name: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EraNames) to High(EraNames) do
  begin
    if EraNames[I] = '' then Break;
    if AnsiStrPos(PChar(EraNames[I]), PChar(Name)) <> nil then
    begin
      Result := EraYearOffsets[I];
      Exit;
    end;
  end;
end;

function ScanDate(const S: string; var Pos: Integer;
  var Date: TDateTime): Boolean; overload;
var
  DateOrder: TDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  CenturyBase: Integer;
  EraName : string;
  EraYearOffset: Integer;

  function EraToYear(Year: Integer): Integer;
  begin
{$IFDEF MSWINDOWS}
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
{$ENDIF}
    Result := Year + EraYearOffset;
  end;

begin
  Y := 0;
  M := 0;
  D := 0;
  YearLen := 0;
  Result := False;
  DateOrder := GetDateOrder(ShortDateFormat);
  EraYearOffset := 0;
  if (ShortDateFormat <> '') and (ShortDateFormat[1] = 'g') then  // skip over prefix text
  begin
    ScanToNumber(S, Pos);
    EraName := Trim(Copy(S, 1, Pos-1));
    EraYearOffset := GetEraYearOffset(EraName);
  end
  else
    if AnsiPos('e', ShortDateFormat) > 0 then
      EraYearOffset := EraYearOffsets[1];
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, DateSeparator) and
    ScanNumber(S, Pos, N2, L2)) then Exit;
  if ScanChar(S, Pos, DateSeparator) then
  begin
    if not ScanNumber(S, Pos, N3, L3) then Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      doYMD: begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else
    if (YearLen <= 2) then
    begin
      CenturyBase := CurrentYear - TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1; M := N2;
    end else
    begin
      M := N1; D := N2;
    end;
  end;
  ScanChar(S, Pos, DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', ShortDateFormat) <> 0) then
  begin     // ignore trailing text
    if ShortTimeFormat[1] in ['0'..'9'] then  // stop at time digit
      ScanToNumber(S, Pos)
    else  // stop at time prefix
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do Inc(Pos);
        ScanBlanks(S, Pos);
      until (Pos > Length(S)) or
        (AnsiCompareText(TimeAMString, Copy(S, Pos, Length(TimeAMString))) = 0) or
        (AnsiCompareText(TimePMString, Copy(S, Pos, Length(TimePMString))) = 0);
  end;
  Result := TryEncodeDate(Y, M, D, Date);
end;

function ScanDate(const S: string; var Pos: Integer; var Date: TDateTime;
  const FormatSettings: TFormatSettings): Boolean; overload;
var
  DateOrder: TDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  CenturyBase: Integer;
  EraName : string;
  EraYearOffset: Integer;

  function EraToYear(Year: Integer): Integer;
  begin
{$IFDEF MSWINDOWS}
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
{$ENDIF}
    Result := Year + EraYearOffset;
  end;

begin
  Y := 0;
  M := 0;
  D := 0;
  YearLen := 0;
  Result := False;
  DateOrder := GetDateOrder(FormatSettings.ShortDateFormat);
  EraYearOffset := 0;
  if FormatSettings.ShortDateFormat[1] = 'g' then  // skip over prefix text
  begin
    ScanToNumber(S, Pos);
    EraName := Trim(Copy(S, 1, Pos-1));
    EraYearOffset := GetEraYearOffset(EraName);
  end
  else
    if AnsiPos('e', FormatSettings.ShortDateFormat) > 0 then
      EraYearOffset := EraYearOffsets[1];
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, FormatSettings.DateSeparator) and
    ScanNumber(S, Pos, N2, L2)) then Exit;
  if ScanChar(S, Pos, FormatSettings.DateSeparator) then
  begin
    if not ScanNumber(S, Pos, N3, L3) then Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      doYMD: begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else
    if (YearLen <= 2) then
    begin
      CenturyBase := CurrentYear - FormatSettings.TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (FormatSettings.TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1; M := N2;
    end else
    begin
      M := N1; D := N2;
    end;
  end;
  ScanChar(S, Pos, FormatSettings.DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', FormatSettings.ShortDateFormat) <> 0) then
  begin     // ignore trailing text
    if FormatSettings.ShortTimeFormat[1] in ['0'..'9'] then  // stop at time digit
      ScanToNumber(S, Pos)
    else  // stop at time prefix
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do Inc(Pos);
        ScanBlanks(S, Pos);
      until (Pos > Length(S)) or
        (AnsiCompareText(FormatSettings.TimeAMString,
         Copy(S, Pos, Length(FormatSettings.TimeAMString))) = 0) or
        (AnsiCompareText(FormatSettings.TimePMString,
         Copy(S, Pos, Length(FormatSettings.TimePMString))) = 0);
  end;
  Result := TryEncodeDate(Y, M, D, Date);
end;

function ScanTime(const S: string; var Pos: Integer;
  var Time: TDateTime): Boolean; overload;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
begin
  Result := False;
  BaseHour := -1;
  if ScanString(S, Pos, TimeAMString) or ScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ScanString(S, Pos, TimePMString) or ScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ScanBlanks(S, Pos);
  if not ScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  Sec := 0;
  MSec := 0;
  if ScanChar(S, Pos, TimeSeparator) then
  begin
    if not ScanNumber(S, Pos, Min, Junk) then Exit;
    if ScanChar(S, Pos, TimeSeparator) then
    begin
      if not ScanNumber(S, Pos, Sec, Junk) then Exit;
      if ScanChar(S, Pos, DecimalSeparator) then
        if not ScanNumber(S, Pos, MSec, Junk) then Exit;
    end;
  end;
  if BaseHour < 0 then
    if ScanString(S, Pos, TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ScanString(S, Pos, TimePMString) or ScanString(S, Pos, 'PM') then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ScanBlanks(S, Pos);
  Result := TryEncodeTime(Hour, Min, Sec, MSec, Time);
end;

function ScanTime(const S: string; var Pos: Integer; var Time: TDateTime;
  const FormatSettings: TFormatSettings): Boolean; overload;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
begin
  Result := False;
  BaseHour := -1;
  if ScanString(S, Pos, FormatSettings.TimeAMString) or ScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ScanString(S, Pos, FormatSettings.TimePMString) or ScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ScanBlanks(S, Pos);
  if not ScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  Sec := 0;
  MSec := 0;
  if ScanChar(S, Pos, FormatSettings.TimeSeparator) then
  begin
    if not ScanNumber(S, Pos, Min, Junk) then Exit;
    if ScanChar(S, Pos, FormatSettings.TimeSeparator) then
    begin
      if not ScanNumber(S, Pos, Sec, Junk) then Exit;
      if ScanChar(S, Pos, FormatSettings.DecimalSeparator) then
        if not ScanNumber(S, Pos, MSec, Junk) then Exit;
    end;
  end;
  if BaseHour < 0 then
    if ScanString(S, Pos, FormatSettings.TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ScanString(S, Pos, FormatSettings.TimePMString) or ScanString(S, Pos, 'PM') then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ScanBlanks(S, Pos);
  Result := TryEncodeTime(Hour, Min, Sec, MSec, Time);
end;

function StrToDate(const S: string): TDateTime;
begin
  if not TryStrToDate(S, Result) then
    ConvertErrorFmt(@SInvalidDate, [S]);
end;

function StrToDate(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDate(S, Result, FormatSettings) then
    ConvertErrorFmt(@SInvalidDate, [S]);
end;

function StrToDateDef(const S: string; const Default: TDateTime): TDateTime;
begin
  if not TryStrToDate(S, Result) then
    Result := Default;
end;

function StrToDateDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDate(S, Result, FormatSettings) then
    Result := Default;
end;

function TryStrToDate(const S: string; out Value: TDateTime): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanDate(S, Pos, Value) and (Pos > Length(S));
end;

function TryStrToDate(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanDate(S, Pos, Value, FormatSettings) and (Pos > Length(S));
end;

function StrToTime(const S: string): TDateTime;
begin
  if not TryStrToTime(S, Result) then
    ConvertErrorFmt(@SInvalidTime, [S]);
end;

function StrToTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToTime(S, Result, FormatSettings) then
    ConvertErrorFmt(@SInvalidTime, [S]);
end;

function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime;
begin
  if not TryStrToTime(S, Result) then
    Result := Default;
end;

function StrToTimeDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToTime(S, Result, FormatSettings) then
    Result := Default;
end;

function TryStrToTime(const S: string; out Value: TDateTime): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanTime(S, Pos, Value) and (Pos > Length(S));
end;

function TryStrToTime(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanTime(S, Pos, Value, FormatSettings) and (Pos > Length(S));
end;

function StrToDateTime(const S: string): TDateTime;
begin
  if not TryStrToDateTime(S, Result) then
    ConvertErrorFmt(@SInvalidDateTime, [S]);
end;

function StrToDateTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDateTime(S, Result, FormatSettings) then
    ConvertErrorFmt(@SInvalidDateTime, [S]);
end;

function StrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(S, Result) then
    Result := Default;
end;

function StrToDateTimeDef(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDateTime(S, Result, FormatSettings) then
    Result := Default;
end;

function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
var
  Pos: Integer;
  Date, Time: TDateTime;
begin
  Result := True;
  Pos := 1;
  Time := 0;
  if not ScanDate(S, Pos, Date) or
     not ((Pos > Length(S)) or ScanTime(S, Pos, Time)) then

    // Try time only
    Result := TryStrToTime(S, Value)
  else
    if Date >= 0 then
      Value := Date + Time
    else
      Value := Date - Time;
end;

function TryStrToDateTime(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
var
  Pos: Integer;
  Date, Time: TDateTime;
begin
  Result := True;
  Pos := 1;
  Time := 0;
  if not ScanDate(S, Pos, Date, FormatSettings) or
     not ((Pos > Length(S)) or ScanTime(S, Pos, Time, FormatSettings)) then

    // Try time only
    Result := TryStrToTime(S, Value, FormatSettings)
  else
    if Date >= 0 then
      Value := Date + Time
    else
      Value := Date - Time;
end;

{ System error messages }

function SysErrorMessage(ErrorCode: Integer): string;
var
  Buffer: array[0..255] of Char;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
    SizeOf(Buffer), nil);
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do Dec(Len);
  SetString(Result, Buffer, Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  //Result := Format('System error: %4x',[ErrorCode]);
  Result := strerror_r(ErrorCode, Buffer, sizeof(Buffer));
end;
{$ENDIF}

{ Initialization file support }

function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
{$IFDEF MSWINDOWS}
var
  L: Integer;
  Buffer: array[0..255] of Char;
begin
  L := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  if L > 0 then SetString(Result, Buffer, L - 1) else Result := Default;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := Default;
end;
{$ENDIF}

function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
{$IFDEF MSWINDOWS}
var
  Buffer: array[0..1] of Char;
begin
  if GetLocaleInfo(Locale, LocaleType, Buffer, 2) > 0 then
    Result := Buffer[0] else
    Result := Default;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := Default;
end;
{$ENDIF}

var
  DefShortMonthNames: array[1..12] of Pointer = (@SShortMonthNameJan,
    @SShortMonthNameFeb, @SShortMonthNameMar, @SShortMonthNameApr,
    @SShortMonthNameMay, @SShortMonthNameJun, @SShortMonthNameJul,
    @SShortMonthNameAug, @SShortMonthNameSep, @SShortMonthNameOct,
    @SShortMonthNameNov, @SShortMonthNameDec);

  DefLongMonthNames: array[1..12] of Pointer = (@SLongMonthNameJan,
    @SLongMonthNameFeb, @SLongMonthNameMar, @SLongMonthNameApr,
    @SLongMonthNameMay, @SLongMonthNameJun, @SLongMonthNameJul,
    @SLongMonthNameAug, @SLongMonthNameSep, @SLongMonthNameOct,
    @SLongMonthNameNov, @SLongMonthNameDec);

  DefShortDayNames: array[1..7] of Pointer = (@SShortDayNameSun,
    @SShortDayNameMon, @SShortDayNameTue, @SShortDayNameWed,
    @SShortDayNameThu, @SShortDayNameFri, @SShortDayNameSat);

  DefLongDayNames: array[1..7] of Pointer = (@SLongDayNameSun,
    @SLongDayNameMon, @SLongDayNameTue, @SLongDayNameWed,
    @SLongDayNameThu, @SLongDayNameFri, @SLongDayNameSat);

procedure GetMonthDayNames;
{$IFDEF MSWINDOWS}
var
  I, Day: Integer;
  DefaultLCID: LCID;

  function LocalGetLocaleStr(LocaleType, Index: Integer;
    const DefValues: array of Pointer): string;
  begin
    Result := GetLocaleStr(DefaultLCID, LocaleType, '');
    if Result = '' then Result := LoadResString(DefValues[Index]);
  end;

begin
  DefaultLCID := GetThreadLocale;
  for I := 1 to 12 do
  begin
    ShortMonthNames[I] := LocalGetLocaleStr(LOCALE_SABBREVMONTHNAME1 + I - 1,
      I - Low(DefShortMonthNames), DefShortMonthNames);
    LongMonthNames[I] := LocalGetLocaleStr(LOCALE_SMONTHNAME1 + I - 1,
      I - Low(DefLongMonthNames), DefLongMonthNames);
  end;
  for I := 1 to 7 do
  begin
    Day := (I + 5) mod 7;
    ShortDayNames[I] := LocalGetLocaleStr(LOCALE_SABBREVDAYNAME1 + Day,
      I - Low(DefShortDayNames), DefShortDayNames);
    LongDayNames[I] := LocalGetLocaleStr(LOCALE_SDAYNAME1 + Day,
      I - Low(DefLongDayNames), DefLongDayNames);
  end;
end;
{$ELSE}
{$IFDEF LINUX}
  function GetLocaleStr(LocaleIndex, Index: Integer;
    const DefValues: array of Pointer): string;
  var
    temp: PChar;
  begin
    temp := nl_langinfo(LocaleIndex);
    if (temp = nil) or (temp^ = #0) then
      Result := LoadResString(DefValues[Index])
    else
      Result := temp;
  end;

var
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    ShortMonthNames[I] := GetLocaleStr(ABMON_1 + I - 1,
      I - Low(DefShortMonthNames), DefShortMonthNames);
    LongMonthNames[I] := GetLocaleStr(MON_1 + I - 1,
      I - Low(DefLongMonthNames), DefLongMonthNames);
  end;
  for I := 1 to 7 do
  begin
    ShortDayNames[I] := GetLocaleStr(ABDAY_1 + I - 1,
      I - Low(DefShortDayNames), DefShortDayNames);
    LongDayNames[I] := GetLocaleStr(DAY_1 + I - 1,
      I - Low(DefLongDayNames), DefLongDayNames);
  end;
end;
{$ELSE}
var
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    ShortMonthNames[I] := LoadResString(DefShortMonthNames[I]);
    LongMonthNames[I] := LoadResString(DefLongMonthNames[I]);
  end;
  for I := 1 to 7 do
  begin
    ShortDayNames[I] := LoadResString(DefShortDayNames[I]);
    LongDayNames[I] := LoadResString(DefLongDayNames[I]);
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure GetLocaleMonthDayNames(DefaultLCID: Integer;
  var FormatSettings: TFormatSettings);
var
  I, Day: Integer;

  function LocalGetLocaleStr(LocaleType, Index: Integer;
    const DefValues: array of Pointer): string;
  begin
    Result := GetLocaleStr(DefaultLCID, LocaleType, '');
    if Result = '' then Result := LoadResString(DefValues[Index]);
  end;

begin
  for I := 1 to 12 do
  begin
    FormatSettings.ShortMonthNames[I] := LocalGetLocaleStr(LOCALE_SABBREVMONTHNAME1 + I - 1,
      I - Low(DefShortMonthNames), DefShortMonthNames);
    FormatSettings.LongMonthNames[I] := LocalGetLocaleStr(LOCALE_SMONTHNAME1 + I - 1,
      I - Low(DefLongMonthNames), DefLongMonthNames);
  end;
  for I := 1 to 7 do
  begin
    Day := (I + 5) mod 7;
    FormatSettings.ShortDayNames[I] := LocalGetLocaleStr(LOCALE_SABBREVDAYNAME1 + Day,
      I - Low(DefShortDayNames), DefShortDayNames);
    FormatSettings.LongDayNames[I] := LocalGetLocaleStr(LOCALE_SDAYNAME1 + Day,
      I - Low(DefLongDayNames), DefLongDayNames);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function EnumEraNames(Names: PChar): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  I := Low(EraNames);
  while EraNames[I] <> '' do
    if (I = High(EraNames)) then
      Exit
    else Inc(I);
  EraNames[I] := Names;
  Result := 1;
end;

function EnumEraYearOffsets(YearOffsets: PChar): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  I := Low(EraYearOffsets);
  while EraYearOffsets[I] <> -1 do
    if (I = High(EraYearOffsets)) then
      Exit
    else Inc(I);
  EraYearOffsets[I] := StrToIntDef(YearOffsets, 0);
  Result := 1;
end;

procedure GetEraNamesAndYearOffsets;
var
  J: Integer;
  CalendarType: CALTYPE;
begin
  CalendarType := StrToIntDef(GetLocaleStr(GetThreadLocale,
    LOCALE_IOPTIONALCALENDAR, '1'), 1);
  if CalendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA] then
  begin
    EnumCalendarInfoA(@EnumEraNames, GetThreadLocale, CalendarType,
      CAL_SERASTRING);
    for J := Low(EraYearOffsets) to High(EraYearOffsets) do
      EraYearOffsets[J] := -1;
    EnumCalendarInfoA(@EnumEraYearOffsets, GetThreadLocale, CalendarType,
      CAL_IYEAROFFSETRANGE);
  end;
end;

function TranslateDateFormat(const FormatStr: string): string;
var
  I: Integer;
  L: Integer;
  CalendarType: CALTYPE;
  RemoveEra: Boolean;
begin
  I := 1;
  Result := '';
  CalendarType := StrToIntDef(GetLocaleStr(GetThreadLocale,
    LOCALE_ICALENDARTYPE, '1'), 1);
  if not (CalendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA]) then
  begin
    RemoveEra := SysLocale.PriLangID in [LANG_JAPANESE, LANG_CHINESE, LANG_KOREAN];
    if RemoveEra then
    begin
      While I <= Length(FormatStr) do
      begin
        if not (FormatStr[I] in ['g', 'G']) then
          Result := Result + FormatStr[I];
        Inc(I);
      end;
    end
    else
      Result := FormatStr;
    Exit;
  end;

  while I <= Length(FormatStr) do
  begin
    if FormatStr[I] in LeadBytes then
    begin
      L := CharLength(FormatStr, I);
      Result := Result + Copy(FormatStr, I, L);
      Inc(I, L);
    end else
    begin
      if StrLIComp(@FormatStr[I], 'gg', 2) = 0 then
      begin
        Result := Result + 'ggg';
        Inc(I, 1);
      end
      else if StrLIComp(@FormatStr[I], 'yyyy', 4) = 0 then
      begin
        Result := Result + 'eeee';
        Inc(I, 4-1);
      end
      else if StrLIComp(@FormatStr[I], 'yy', 2) = 0 then
      begin
        Result := Result + 'ee';
        Inc(I, 2-1);
      end
      else if FormatStr[I] in ['y', 'Y'] then
        Result := Result + 'e'
      else
        Result := Result + FormatStr[I];
      Inc(I);
    end;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
procedure InitEras;
var
  Count : Byte;
  I, J, Pos : Integer;
  Number : Word;
  S : string;
  Year, Month, Day: Word;
begin
  EraCount := 0;
  S := nl_langinfo(ERA);
  if S = '' then
    S := LoadResString(@SEraEntries);

  Pos := 1;
  for I := 1 to MaxEraCount do
  begin
    if Pos > Length(S) then Break;
    if not(ScanChar(S, Pos, '+') or ScanChar(S, Pos, '-')) then Break;
    // Eras in which year increases with negative time (eg Christian BC era)
    // are not currently supported.
//    EraRanges[I].Direction := S[Pos - 1];

    // Era offset, in years from Gregorian calendar year
    if not ScanChar(S, Pos, ':') then Break;
    if ScanChar(S, Pos, '-') then
      J := -1
    else
      J := 1;
    if not ScanNumber(S, Pos, Number, Count) then Break;
    EraYearOffsets[I] := J * Number;   // apply sign to Number

    // Era start date, in Gregorian year/month/day format
    if not ScanChar(S, Pos, ':') then Break;
    if not ScanNumber(S, Pos, Year, Count) then Break;
    if not ScanChar(S, Pos, '/') then Break;
    if not ScanNumber(S, Pos, Month, Count) then Break;
    if not ScanChar(S, Pos, '/') then Break;
    if not ScanNumber(S, Pos, Day, Count) then Break;
    EraRanges[I].StartDate := Trunc(EncodeDate(Year, Month, Day));
    EraYearOffsets[I] := Year - EraYearOffsets[I];

    // Era end date, in Gregorian year/month/day format
    if not ScanChar(S, Pos, ':') then Break;
    if ScanString(S, Pos, '+*') then       // positive infinity
      EraRanges[I].EndDate := High(EraRanges[I].EndDate)
    else if ScanString(S, Pos, '-*') then  // negative infinity
      EraRanges[I].EndDate := Low(EraRanges[I].EndDate)
    else if not ScanNumber(S, Pos, Year, Count) then
      Break
    else
    begin
      if not ScanChar(S, Pos, '/') then Break;
      if not ScanNumber(S, Pos, Month, Count) then Break;
      if not ScanChar(S, Pos, '/') then Break;
      if not ScanNumber(S, Pos, Day, Count) then Break;
      EraRanges[I].EndDate := Trunc(EncodeDate(Year, Month, Day));
    end;

    // Era name, in locale charset
    if not ScanChar(S, Pos, ':') then Break;
    J := AnsiPos(':', Copy(S, Pos, Length(S) + 1 - Pos));
    if J = 0 then Break;
    EraNames[I] := Copy(S, Pos, J - 1);
    Inc(Pos, J - 1);

    // Optional Era format string for era year, in locale charset
    if not ScanChar(S, Pos, ':') then Break;
    J := AnsiPos(';', Copy(S, Pos, Length(S) + 1 - Pos));
    if J = 0 then
      J := 1 + Length(S) + 1 - Pos;
    {if J = 0 then Break;}
    EraYearFormats[I] := Copy(S, Pos, J - 1);
    Inc(Pos, J - 1);
    Inc(EraCount);
    if not((Pos > Length(S)) or ScanChar(S, Pos, ';')) then Break;
  end;

  // Clear the rest of the era slots, including partial entry from failed parse
  for I := EraCount+1 to MaxEraCount do
  begin
    EraNames[I] := '';
    EraYearOffsets[I] := -1;
    EraRanges[I].StartDate := High(EraRanges[I].StartDate);
    EraRanges[I].EndDate := High(EraRanges[I].EndDate);
    EraYearFormats[I] := '';
  end;
end;
{$ENDIF}

{ Exception handling routines }

var
  OutOfMemory: EOutOfMemory;
  InvalidPointer: EInvalidPointer;

{ Convert physical address to logical address }

{ Format and return an exception error message }

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PChar; Size: Integer): Integer;
{$IFDEF MSWINDOWS}

  function ConvertAddr(Address: Pointer): Pointer; assembler;
  asm
          TEST    EAX,EAX         { Always convert nil to nil }
          JE      @@1
          SUB     EAX, $1000      { offset from code start; code start set by linker to $1000 }
  @@1:
  end;

var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  ModuleName: array[0..MAX_PATH] of Char;
  Temp: array[0..MAX_PATH] of Char;
  Format: array[0..255] of Char;
  Info: TMemoryBasicInformation;
  ConvertedAddress: Pointer;
begin
  VirtualQuery(ExceptAddr, Info, sizeof(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    ConvertedAddress := ConvertAddr(ExceptAddr);
  end
  else
    Integer(ConvertedAddress) := Integer(ExceptAddr) - Integer(Info.AllocationBase);
  StrLCopy(ModuleName, AnsiStrRScan(Temp, '\') + 1, SizeOf(ModuleName) - 1);
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  LoadString(FindResourceHInstance(HInstance),
    PResStringRec(@SException).Identifier, Format, SizeOf(Format));
  StrLFmt(Buffer, Size, Format, [ExceptObject.ClassName, ModuleName,
    ConvertedAddress, MsgPtr, MsgEnd]);
  Result := StrLen(Buffer);
end;
{$ENDIF}
{$IFDEF LINUX}
const
  UnknownModuleName = '<unknown>';
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  ModuleName: array[0..MAX_PATH] of Char;
  Info: TDLInfo;
begin
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  if (dladdr(ExceptAddr, Info) <> 0) and (Info.dli_fname <> nil) then
    StrLCopy(ModuleName, AnsiStrRScan(Info.dli_fname, PathDelim) + 1, SizeOf(ModuleName) - 1)
  else
    StrLCopy(ModuleName, UnknownModuleName, SizeOf(ModuleName) - 1);
  StrLFmt(Buffer, Size, PChar(SException), [ExceptObject.ClassName, ModuleName,
    ExceptAddr, MsgPtr, MsgEnd]);
  Result := StrLen(Buffer);
end;
{$ENDIF}

{ Display exception message box }

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
{$IFDEF MSWINDOWS}
var
  Title: array[0..63] of Char;
  Buffer: array[0..1023] of Char;
  Dummy: Cardinal;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
  if IsConsole then
  begin
    Flush(Output);
    CharToOemA(Buffer, Buffer);
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), Buffer, StrLen(Buffer), Dummy, nil);
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), sLineBreak, 2, Dummy, nil);
  end
  else
  begin
    LoadString(FindResourceHInstance(HInstance), PResStringRec(@SExceptTitle).Identifier,
      Title, SizeOf(Title));
    MessageBox(0, Buffer, Title, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  Buffer: array[0..1023] of Char;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, Sizeof(Buffer));
  if TTextRec(ErrOutput).Mode = fmOutput then
    Flush(ErrOutput);
  __write(STDERR_FILENO, Buffer, StrLen(Buffer));
end;
{$ENDIF}

{ Raise abort exception }

procedure Abort;

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP + 4]
  end;

begin
  raise EAbort.CreateRes(@SOperationAborted) at ReturnAddr;
end;

{ Raise out of memory exception }

procedure OutOfMemoryError;
begin
  raise OutOfMemory;
end;

{ Exception class }

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
end;

constructor Exception.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  FMessage := Format(Msg, Args);
end;

constructor Exception.CreateRes(Ident: Integer);
begin
  FMessage := LoadStr(Ident);
end;

constructor Exception.CreateRes(ResStringRec: PResStringRec);
begin
  FMessage := LoadResString(ResStringRec);
end;

constructor Exception.CreateResFmt(Ident: Integer;
  const Args: array of const);
begin
  FMessage := Format(LoadStr(Ident), Args);
end;

constructor Exception.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  FMessage := Format(LoadResString(ResStringRec), Args);
end;

constructor Exception.CreateHelp(const Msg: string; AHelpContext: Integer);
begin
  FMessage := Msg;
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateFmtHelp(const Msg: string; const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := Format(Msg, Args);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResHelp(Ident: Integer; AHelpContext: Integer);
begin
  FMessage := LoadStr(Ident);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResHelp(ResStringRec: PResStringRec;
  AHelpContext: Integer);
begin
  FMessage := LoadResString(ResStringRec);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResFmtHelp(Ident: Integer;
  const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := Format(LoadStr(Ident), Args);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResFmtHelp(ResStringRec: PResStringRec;
  const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := Format(LoadResString(ResStringRec), Args);
  FHelpContext := AHelpContext;
end;

{ EHeapException class }

procedure EHeapException.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

{ Create I/O exception }

function CreateInOutError: EInOutError;
type
  TErrorRec = record
    Code: Integer;
    Ident: string;
  end;
const
  ErrorMap: array[0..6] of TErrorRec = (
    (Code: 2; Ident: SFileNotFound),
    (Code: 3; Ident: SInvalidFilename),
    (Code: 4; Ident: STooManyOpenFiles),
    (Code: 5; Ident: SAccessDenied),
    (Code: 100; Ident: SEndOfFile),
    (Code: 101; Ident: SDiskFull),
    (Code: 106; Ident: SInvalidInput));
var
  I: Integer;
  InOutRes: Integer;
begin
  I := Low(ErrorMap);
  InOutRes := IOResult;  // resets IOResult to zero
  while (I <= High(ErrorMap)) and (ErrorMap[I].Code <> InOutRes) do Inc(I);
  if I <= High(ErrorMap) then
    Result := EInOutError.Create(ErrorMap[I].Ident) else
    Result := EInOutError.CreateResFmt(@SInOutError, [InOutRes]);
  Result.ErrorCode := InOutRes;
end;

{ RTL error handler }

type
  TExceptRec = record
    EClass: ExceptClass;
    EIdent: string;
  end;

const
  ExceptMap: array[Ord(reDivByZero)..Ord(High(TRuntimeError))] of TExceptRec = (
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: EAccessViolation; EIdent: SAccessViolationNoArg),
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlC; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: EVariantError; EIdent: SInvalidVarCast),
    (EClass: EVariantError; EIdent: SInvalidVarOp),
    (EClass: EVariantError; EIdent: SDispatchError),
    (EClass: EVariantError; EIdent: SVarArrayCreate),
    (EClass: EVariantError; EIdent: SVarInvalid),
    (EClass: EVariantError; EIdent: SVarArrayBounds),
    (EClass: EAssertionFailed; EIdent: SAssertionFailed),
    (EClass: EExternalException; EIdent: SExternalException),
    (EClass: EIntfCastError; EIdent: SIntfCastError),
    (EClass: ESafecallException; EIdent: SSafecallException)
  {$IFDEF LINUX}
    ,
    (EClass: EQuit; EIdent: SQuit),
    (EClass: ECodesetConversion; EIdent: SCodesetConversionError)
  {$ENDIF}
    );

procedure ErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer); export;
var
  E: Exception;
begin
  case ErrorCode of
    Ord(reOutOfMemory):
      E := OutOfMemory;
    Ord(reInvalidPtr):
      E := InvalidPointer;
    Ord(reDivByZero)..Ord(High(TRuntimeError)):
      begin
        with ExceptMap[ErrorCode] do
          E := EClass.Create(EIdent);
      end;
  else
    E := CreateInOutError;
  end;
  raise E at ErrorAddr;
end;

{ Assertion error handler }

{ This is complicated by the desire to make it look like the exception     }
{ happened in the user routine, so the debugger can give a decent stack    }
{ trace. To make that feasible, AssertErrorHandler calls a helper function }
{ to create the exception object, so that AssertErrorHandler itself does   }
{ not need any temps. After the exception object is created, the asm       }
{ routine RaiseAssertException sets up the registers just as if the user   }
{ code itself had raised the exception.                                    }

function CreateAssertException(const Message, Filename: string;
  LineNumber: Integer): Exception;
var
  S: string;
begin
  if Message <> '' then S := Message else S := SAssertionFailed;
  Result := EAssertionFailed.CreateFmt(SAssertError,
    [S, Filename, LineNumber]);
end;

{ This code is based on the following assumptions:                         }
{  - Our direct caller (AssertErrorHandler) has an EBP frame               }
{  - ErrorStack points to where the return address would be if the         }
{    user program had called System.@RaiseExcept directly                  }
procedure RaiseAssertException(const E: Exception; const ErrorAddr, ErrorStack: Pointer);
asm
        MOV     ESP,ECX
        MOV     [ESP],EDX
        MOV     EBP,[EBP]
        JMP     System.@RaiseExcept
end;

{ If you change this procedure, make sure it does not have any local variables }
{ or temps that need cleanup - they won't get cleaned up due to the way        }
{ RaiseAssertException frame works. Also, it can not have an exception frame.  }
procedure AssertErrorHandler(const Message, Filename: string;
  LineNumber: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
   E := CreateAssertException(Message, Filename, LineNumber);
{$IF Defined(LINUX)}
   RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+8);
{$ELSEIF Defined(MSWINDOWS)}
   RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+4);
{$ELSE}
   {$MESSAGE ERROR 'AssertErrorHandler not implemented'}
{$IFEND}
end;

{$IFNDEF PC_MAPPED_EXCEPTIONS}

{ Abstract method invoke error handler }

procedure AbstractErrorHandler;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;
{$ENDIF}

{$IFDEF LINUX}
const
  TRAP_ZERODIVIDE    = 0;
  TRAP_SINGLESTEP    = 1;
  TRAP_NMI           = 2;
  TRAP_BREAKPOINT    = 3;
  TRAP_OVERFLOW      = 4;
  TRAP_BOUND         = 5;
  TRAP_INVINSTR      = 6;
  TRAP_DEVICENA      = 7;
  TRAP_DOUBLEFAULT   = 8;
  TRAP_FPOVERRUN     = 9;
  TRAP_BADTSS        = 10;
  TRAP_SEGMENTNP     = 11;
  TRAP_STACKFAULT    = 12;
  TRAP_GPFAULT       = 13;
  TRAP_PAGEFAULT     = 14;
  TRAP_RESERVED      = 15;
  TRAP_FPE           = 16;
  TRAP_ALIGNMENT     = 17;
  TRAP_MACHINECHECK  = 18;
  TRAP_CACHEFAULT    = 19;
  TRAP_UNKNOWN       = -1;

function MapFPUStatus(Status: LongWord): TRuntimeError;
begin
  if (Status and 1) = 1 then Result := System.reInvalidOp           // STACK_CHECK or INVALID_OPERATION
  else if (Status and 2) = 2 then Result := System.reInvalidOp      // DENORMAL_OPERAND
  else if (Status and 4) = 4 then Result := System.reZeroDivide     // DIVIDE_BY_ZERO
  else if (Status and 8) = 8 then Result := System.reOverflow       // OVERFLOW
  else if (Status and $10) = $10 then Result := System.reUnderflow  // UNDERFLOW
  else if (Status and $20) = $20 then Result := System.reInvalidOp  // INEXACT_RESULT
  else Result := System.reInvalidOp;
end;

function MapFPE(Context: PSigContext): TRuntimeError;
begin
  case Context^.trapno of
    TRAP_ZERODIVIDE:
      Result := System.reDivByZero;
    TRAP_FPOVERRUN:
      Result := System.reInvalidOp;
    TRAP_FPE:
      Result := MapFPUStatus(Context^.fpstate^.sw);
  else
    Result := System.reInvalidOp;
  end;
end;

function MapFault(Context: PSigContext): TRuntimeError;
begin
  case Context^.trapno of
    TRAP_OVERFLOW:
      Result := System.reIntOverflow;
    TRAP_BOUND:
      Result := System.reRangeError;
    TRAP_INVINSTR:
      Result := System.rePrivInstruction; // This doesn't seem right, but we don't
                                          // have an external exception to match!
    TRAP_STACKFAULT:
      Result := System.reStackOverflow;
    TRAP_SEGMENTNP,
    TRAP_GPFAULT:
      Result := System.reAccessViolation;
    TRAP_PAGEFAULT:
      Result := System.reAccessViolation;
  else
    Result := System.reAccessViolation;
  end;
end;

function MapSignal(SigNum: Integer; Context: PSigContext): LongWord;
var
  Err: TRuntimeError;
begin
  case SigNum of
    SIGINT:       { Control-C }
      Err := System.reControlBreak;
    SIGQUIT:       { Quit key (Control-\) }
      Err := System.reQuit;
    SIGFPE:       { Floating Point Error }
      Err := MapFPE(Context);
    SIGSEGV:      { Segmentation Violation }
      Err := MapFault(Context);
    SIGILL:       { Illegal Instruction }
      Err := MapFault(Context);
    SIGBUS:       { Bus Error }
      Err := MapFault(Context);
  else
    Err := System.reExternalException;
  end;
  Result := LongWord(Err) or (LongWord(SigNum) shl 16);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function MapException(P: PExceptionRecord): TRuntimeError;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:
      Result := System.reDivByZero;
    STATUS_ARRAY_BOUNDS_EXCEEDED:
      Result := System.reRangeError;
    STATUS_INTEGER_OVERFLOW:
      Result := System.reIntOverflow;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:
      Result := System.reInvalidOp;
    STATUS_FLOAT_DIVIDE_BY_ZERO:
      Result := System.reZeroDivide;
    STATUS_FLOAT_OVERFLOW:
      Result := System.reOverflow;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:
      Result := System.reUnderflow;
    STATUS_ACCESS_VIOLATION:
      Result := System.reAccessViolation;
    STATUS_PRIVILEGED_INSTRUCTION:
      Result := System.rePrivInstruction;
    STATUS_CONTROL_C_EXIT:
      Result := System.reControlBreak;
    STATUS_STACK_OVERFLOW:
      Result := System.reStackOverflow;
    else
      Result := System.reExternalException;
  end;
end;

function GetExceptionClass(P: PExceptionRecord): ExceptClass;
var
  ErrorCode: Byte;
begin
  ErrorCode := Byte(MapException(P));
  Result := ExceptMap[ErrorCode].EClass;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  ErrorCode: Integer;

  function CreateAVObject: Exception;
  var
    AccessOp: string; // string ID indicating the access type READ or WRITE
    AccessAddress: Pointer;
    MemInfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of Char;
  begin
    with P^ do
    begin
      if ExceptionInformation[0] = 0 then
        AccessOp := SReadAccess
      else
        AccessOp := SWriteAccess;
      AccessAddress := Pointer(ExceptionInformation[1]);
      VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
      if (MemInfo.State = MEM_COMMIT) and
         (GetModuleFileName(THandle(MemInfo.AllocationBase), ModName, SizeOf(ModName)) <> 0) then
        Result := EAccessViolation.CreateFmt(sModuleAccessViolation,
          [ExceptionAddress, ExtractFileName(ModName), AccessOp,
          AccessAddress])
      else
        Result := EAccessViolation.CreateFmt(SAccessViolationArg3,
          [ExceptionAddress, AccessOp, AccessAddress]);
    end;
  end;

begin
  ErrorCode := Byte(MapException(P));
  case ErrorCode of
    3..10, 12..21:
      with ExceptMap[ErrorCode] do Result := EClass.Create(EIdent);
    11: Result := CreateAVObject;
  else
    Result := EExternalException.CreateFmt(SExternalException, [P.ExceptionCode]);
  end;
  if Result is EExternal then EExternal(Result).ExceptionRecord := P;
end;
{$ENDIF} { WIN32 }

{$IFDEF LINUX}
{
  The ErrorCode has the translated error code in the low byte and the
  original signal number in the high word.
}
function GetExceptionObject(ExceptionAddress: LongWord; AccessAddress: LongWord; ErrorCode: LongWord): Exception;
begin
  case (ErrorCode and $ff) of
    3..10, 12..21, 25:
      begin
        with ExceptMap[ErrorCode and $ff] do
          Result := EClass.Create(EIdent);
      end;
    11:
      Result := EAccessViolation.CreateFmt(SAccessViolationArg2, [Pointer(ExceptionAddress), Pointer(AccessAddress)]);
  else
//    Result := EExternalException.CreateFmt(SExternalException, [P.ExceptionCode]);
{ Not quite right - we need the original trap code, but that's lost }
    Result := EExternalException.CreateFmt(SExternalException, [ErrorCode and $ff]);
  end;

  EExternal(Result).ExceptionAddress := ExceptionAddress;
  EExternal(Result).AccessAddress := AccessAddress;
  EExternal(Result).SignalNumber := ErrorCode shr 16;
end;
{$ENDIF}

{ RTL exception handler }

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
begin
  ShowException(ExceptObject, ExceptAddr);
  Halt(1);
end;

{$IFDEF LINUX}
{$IFDEF DEBUG}
{
  Used for debugging the signal handlers.
}
procedure DumpContext(SigNum: Integer; context : PSigContext);
var
   Buff: array [0..128] of char;
begin
   StrFmt(Buff, 'Context for signal: %d', [SigNum]);
   Writeln(Buff);
   StrFmt(Buff, 'CS = %04X  DS = %04X  ES = %04X  FS = %04X  GS = %04X  SS = %04X',
          [context^.cs, context^.ds, context^.es, context^.fs, context^.gs, context^.ss]);
   WriteLn(Buff);
   StrFmt(Buff, 'EAX = %08X  EBX = %08X  ECX = %08X  EDX = %08X',
          [context^.eax, context^.ebx, context^.ecx, context^.edx]);
   WriteLn(Buff);
   StrFmt(Buff, 'EDI = %08X  ESI = %08X  EBP = %08X  ESP = %08X',
          [context^.edi, context^.esi, context^.ebp, context^.esp]);
   WriteLn(Buff);
   StrFmt(Buff, 'EIP = %08X  EFLAGS = %08X  ESP(signal) = %08X  CR2 = %08X',
          [context^.eip, context^.eflags, context^.esp_at_signal, context^.cr2]);
   WriteLn(Buff);
   StrFmt(Buff, 'trapno = %d, err = %08x', [context^.trapno, context^.err]);
   WriteLn(Buff);
end;
{$ENDIF}


{
  RaiseSignalException is called from SignalConverter, once we've made things look
  like there's a legitimate stack frame above us.  Now we will just create
  an exception object, and raise it via a software raise.
}
procedure RaiseSignalException(ExceptionEIP: LongWord; FaultAddr: LongWord; ErrorCode: LongWord);
begin
  raise GetExceptionObject(ExceptionEIP, FaultAddr, ErrorCode);
end;

{
  SignalConverter is where we come when a signal is raised that we want to convert
  to an exception.  This function stands the best chance of being called with a
  useable stack frame behind it for the purpose of stack unwinding.  We can't
  guarantee that, though.  The stack was modified by the baseline signal handler
  to make it look as though we were called by the faulting instruction.  That way
  the unwinder stands a chance of being able to clean things up.
}
procedure SignalConverter(ExceptionEIP: LongWord; FaultAddr: LongWord; ErrorCode: LongWord);
asm
        {
          Here's the tricky part.  We arrived here directly by virtue of our
          signal handler tweaking the execution context with our address.  That
          means there's no return address on the stack.  The unwinder needs to
          have a return address so that it can unwind past this function when
          we raise the Delphi exception.  We will use the faulting instruction
          pointer as a fake return address.  Because of the fencepost conditions
          in the Delphi unwinder, we need to have an address that is strictly
          greater than the actual faulting instruction, so we increment that
          address by one.  This may be in the middle of an instruction, but we
          don't care, because we will never be returning to that address.
          Finally, the way that we get this address onto the stack is important.
          The compiler will generate unwind information for SignalConverter that
          will attempt to undo any stack modifications that are made by this
          function when unwinding past it.  In this particular case, we don't want
          that to happen, so we use some assembly language tricks to get around
          the compiler noticing the stack modification.
        }
        MOV EBX, ESP      // Get the current stack pointer
        SUB EBX, 4        // Effectively decrement the stack by 4
        MOV ESP, EBX      //   by doing a move to ESP with a register value
        MOV [ESP], EAX    // Store the instruction pointer into the new stack loc
        INC [ESP]         // Increment by one to keep the unwinder happy

        { Reset the FPU, or things can go south down the line from here }
        FNINIT
        FWAIT
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     EAX, [EAX].offset Default8087CW
        FLDCW   [EAX]
        POP     ECX
        POP     EAX
{$ELSE}
        FLDCW   Default8087CW
{$ENDIF}
        PUSH    EBP
        MOV     EBP, ESP
        CALL    RaiseSignalException
end;

function TlsGetValue(Key: Integer): Pointer; cdecl;
  external libpthreadmodulename name 'pthread_getspecific';

{
  Under Linux, we crawl out from underneath the OS signal handler before
  we attempt to do anything with the signal.  This is because the stack
  has a bunch of OS frames on there that we cannot possibly unwind from.
  So we use this routine to accomplish the dispatch, and then another
  routine to handle the language level of the exception handling.
}
procedure SignalDispatcher(SigNum: Integer; SigInfo: PSigInfo; UContext: PUserContext); cdecl;
type
  PGeneralRegisters = ^gregset_t;
var
  GeneralRegisters: PGeneralRegisters;
begin
//DumpContext(SigNum, @context);

  {
    Some of the ways that we get here are can lead us to big trouble.  For
    example, if the signal is SIGINT or SIGQUIT, these will commonly be raised
    to all threads in the process if the user generated them from the
    keyboard.  This is handled well by the Delphi threads, but if a non-Delphi
    thread lets one of these get by unhandled, terrible things will happen.
    So we look for that case, and eat SIGINT and SIGQUIT that have been issued
    on threads that are not Delphi threads.  If the signal is a SIGSEGV, or
    other fatal sort of signal, and the thread that we're running on is not
    a Delphi thread, then we are completely without options.  We have no
    recovery means, and we have to take the app down hard, right away.
  }
  if TlsGetValue(TlsIndex) = nil then
  begin
    if (SigNum = SIGINT) or (SigNum = SIGQUIT) then
      Exit;
    RunError(232);
  end;

  {
    If we are processing another exception right now, we definitely do not
    want to be dispatching any exceptions that are async, like SIGINT and
    SIGQUIT.  So we have check to see if OS signals are blocked.  If they are,
    we have to eat this signal right now.
  }
  if AreOSExceptionsBlocked and ((SigNum = SIGINT) or (SigNum = SIGQUIT)) then
    Exit;

  {
    If someone wants to delay the handling of SIGINT or SIGQUIT until such
    time as it's safe to handle it, they set DeferUserInterrupts to True.
    Then we just set a global variable saying that a SIGINT or SIGQUIT was
    issued.  It is the responsibility of some other body of code at this
    point to poll for changes to SIG(INT/QUIT)Issued
  }
  if DeferUserInterrupts then
  begin
    if SigNum = SIGINT then
    begin
      SIGINTIssued := True;
      Exit;
    end;
    if SigNum = SIGQUIT then
    begin
      SIGQUITIssued := True;
      Exit;
    end;
  end;

  BlockOSExceptions;

  GeneralRegisters := @UContext^.uc_mcontext.gregs;

  GeneralRegisters^[REG_EAX] := GeneralRegisters^[REG_EIP];
  GeneralRegisters^[REG_EDX] := UContext^.uc_mcontext.cr2;
  GeneralRegisters^[REG_ECX] := MapSignal(SigNum, PSigContext(GeneralRegisters));

  GeneralRegisters^[REG_EIP] := LongWord(@SignalConverter);
end;

type
  TSignalMap = packed record
    SigNum: Integer;
    Abandon: Boolean;
    OldAction: TSigAction;
    Hooked: Boolean;
  end;

var
  Signals: array [0..RTL_SIGLAST] of TSignalMap =
    ( (SigNum: SIGINT;),
      (SigNum: SIGFPE;),
      (SigNum: SIGSEGV;),
      (SigNum: SIGILL;),
      (SigNum: SIGBUS;),
      (SigNum: SIGQUIT;) );

function InquireSignal(RtlSigNum: Integer): TSignalState;
var
  Action: TSigAction;
begin
  if sigaction(Signals[RtlSigNum].SigNum, nil, @Action) = -1 then
    raise Exception.CreateRes(@SSigactionFailed);
  if (@Action.__sigaction_handler <> @SignalDispatcher) then
  begin
    if Signals[RtlSigNum].Hooked then
      Result := ssOverridden
    else
      Result := ssNotHooked;
  end
  else
    Result := ssHooked;
end;

procedure AbandonSignalHandler(RtlSigNum: Integer);
var
  I: Integer;
begin
  if RtlSigNum = RTL_SIGDEFAULT then
  begin
    for I := 0 to RTL_SIGLAST do
      AbandonSignalHandler(I);
    Exit;
  end;
  Signals[RtlSigNum].Abandon := True;
end;

procedure HookSignal(RtlSigNum: Integer);
var
  Action: TSigAction;
  I: Integer;
begin
  if RtlSigNum = RTL_SIGDEFAULT then
  begin
    for I := 0 to RTL_SIGLAST do
      HookSignal(I);
    Exit;
  end;

  FillChar(Action, SizeOf(Action), 0);
  Action.__sigaction_handler := @SignalDispatcher;
  Action.sa_flags := SA_SIGINFO;
  sigaddset(Action.sa_mask, SIGINT);
  sigaddset(Action.sa_mask, SIGQUIT);
  if sigaction(Signals[RtlSigNum].SigNum, @Action, @Signals[RtlSigNum].OldAction) = -1 then
    raise Exception.CreateRes(@SSigactionFailed);
  Signals[RtlSigNum].Hooked := True;
end;

procedure UnhookSignal(RtlSigNum: Integer; OnlyIfHooked: Boolean);
var
  I: Integer;
begin
  if RtlSigNum = RTL_SIGDEFAULT then
  begin
    for I := 0 to RTL_SIGLAST do
      UnhookSignal(I, OnlyIfHooked);
    Exit;
  end;
  if not Signals[RtlSigNum].Abandon then
  begin
    if OnlyIfHooked and (InquireSignal(RtlSigNum) <> ssHooked) then
      Exit;
    if sigaction(Signals[RtlSigNum].SigNum, @Signals[RtlSigNum].OldAction, Nil) = -1 then
      raise Exception.CreateRes(@SSigactionFailed);
    Signals[RtlSigNum].Hooked := False;
  end;
end;

procedure UnhookOSExceptions;
begin
  if not Assigned(HookOSExceptionsProc) then
    UnhookSignal(RTL_SIGDEFAULT, True);
end;

procedure HookOSExceptions;
begin
  if Assigned(HookOSExceptionsProc) then
    HookOSExceptionsProc
  else
  begin
    HookSignal(RTL_SIGDEFAULT);
  end;
end;
{$ENDIF} // LINUX

procedure InitExceptions;
begin
  OutOfMemory := EOutOfMemory.CreateRes(@SOutOfMemory);
  InvalidPointer := EInvalidPointer.CreateRes(@SInvalidPointer);
  ErrorProc := ErrorHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception;

{$IFDEF MSWINDOWS}
  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
{$ENDIF}

  AssertErrorProc := @AssertErrorHandler;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
  // We don't hook this under PC mapped exceptions, because
  // we have no idea what the parameters were to the procedure
  // in question.  Hence we cannot hope to unwind the stack in
  // our handler.  Since we just throw an exception from our
  // handler, that pretty much rules out using this without
  // exorbitant compiler support.  If you do hook AbstractErrorProc,
  // you must make sure that you never throw an exception from
  // your handler if PC_MAPPED_EXCEPTIONS is defined.
  AbstractErrorProc := @AbstractErrorHandler;
{$ENDIF}

{$IFDEF LINUX}
  if not IsLibrary then
    HookOSExceptions;
{$ENDIF}
end;

procedure DoneExceptions;
begin
  if Assigned(OutOfMemory) then
  begin
    OutOfMemory.AllowFree := True;
    OutOfMemory.FreeInstance;
    OutOfMemory := nil;
  end;
  if Assigned(InvalidPointer) then
  begin
    InvalidPointer.AllowFree := True;
    InvalidPointer.Free;
    InvalidPointer := nil;
  end;
  ErrorProc := nil;
  ExceptProc := nil;
  ExceptionClass := nil;
{$IFDEF MSWINDOWS}
  ExceptClsProc := nil;
  ExceptObjProc := nil;
{$ENDIF}
  AssertErrorProc := nil;
{$IFDEF LINUX}
  if not IsLibrary then
    UnhookOSExceptions;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure InitPlatformId;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    with OSVersionInfo do
    begin
      Win32Platform := dwPlatformId;
      Win32MajorVersion := dwMajorVersion;
      Win32MinorVersion := dwMinorVersion;
      if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        Win32BuildNumber := dwBuildNumber and $FFFF
      else
        Win32BuildNumber := dwBuildNumber;
      Win32CSDVersion := szCSDVersion;
    end;
end;

function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;
begin
  Result := (Win32MajorVersion > AMajor) or
            ((Win32MajorVersion = AMajor) and
             (Win32MinorVersion >= AMinor));
end;

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

procedure Beep;
begin
  MessageBeep(0);
end;
{$ENDIF}
{$IFDEF LINUX}
procedure Beep;
var
  ch: Char;
  FileDes: Integer;
begin
  if isatty(STDERR_FILENO) = 1 then
    FileDes := STDERR_FILENO
  else
  if isatty(STDOUT_FILENO) = 1 then
    FileDes := STDOUT_FILENO
  else
  begin
    // Neither STDERR_FILENO nor STDOUT_FILENO are open
    // terminals (TTYs). It is not possible to safely
    // write the beep character.
    Exit;
  end;

  ch := #7;
  __write(FileDes, ch, 1);
end;
{$ENDIF}

{ MBCS functions }

function ByteTypeTest(P: PChar; Index: Integer): TMbcsByteType;
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  Result := mbSingleByte;
  if (P = nil) or (P[Index] = #$0) then Exit;
  if (Index = 0) then
  begin
    if P[0] in LeadBytes then Result := mbLeadByte;
  end
  else
  begin
    I := Index - 1;
    while (I >= 0) and (P[I] in LeadBytes) do Dec(I);
    if ((Index - I) mod 2) = 0 then Result := mbTrailByte
    else if P[Index] in LeadBytes then Result := mbLeadByte;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I, L: Integer;
begin
  Result := mbSingleByte;
  if (P = nil) or (P[Index] = #$0) then Exit;

  I := 0;
  repeat
    if P[I] in LeadBytes then
      L := StrCharLength(P + I)
    else
      L := 1;
    Inc(I, L);
  until (I > Index);

  if (L <> 1) then
    if (I - L = Index) then
      Result := mbLeadByte
    else
      Result := mbTrailByte;
end;
{$ENDIF}

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  if SysLocale.FarEast then
    Result := ByteTypeTest(PChar(S), Index-1);
end;

function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
begin
  Result := mbSingleByte;
  if SysLocale.FarEast then
    Result := ByteTypeTest(Str, Index);
end;

function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
begin
  if Length(S) < MaxLen then MaxLen := Length(S);
  Result := ByteToCharIndex(S, MaxLen);
end;

function ByteToCharIndex(const S: string; Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Index <= 0) or (Index > Length(S)) then Exit;
  Result := Index;
  if not SysLocale.FarEast then Exit;
  I := 1;
  Result := 0;
  while I <= Index do
  begin
    if S[I] in LeadBytes then
      I := NextCharIndex(S, I)
    else
      Inc(I);
    Inc(Result);
  end;
end;

procedure CountChars(const S: string; MaxChars: Integer; var CharCount, ByteCount: Integer);
var
  C, L, B: Integer;
begin
  L := Length(S);
  C := 1;
  B := 1;
  while (B < L) and (C < MaxChars) do
  begin
    Inc(C);
    if S[B] in LeadBytes then
      B := NextCharIndex(S, B)
    else
      Inc(B);
  end;
  if (C = MaxChars) and (B < L) and (S[B] in LeadBytes) then
    B := NextCharIndex(S, B) - 1;
  CharCount := C;
  ByteCount := B;
end;

function CharToByteIndex(const S: string; Index: Integer): Integer;
var
  Chars: Integer;
begin
  Result := 0;
  if (Index <= 0) or (Index > Length(S)) then Exit;
  if (Index > 1) and SysLocale.FarEast then
  begin
    CountChars(S, Index-1, Chars, Result);
    if (Chars < (Index-1)) or (Result >= Length(S)) then
      Result := 0  // Char index out of range
    else
      Inc(Result);
  end
  else
    Result := Index;
end;

function CharToByteLen(const S: string; MaxLen: Integer): Integer;
var
  Chars: Integer;
begin
  Result := 0;
  if MaxLen <= 0 then Exit;
  if MaxLen > Length(S) then MaxLen := Length(S);
  if SysLocale.FarEast then
  begin
    CountChars(S, MaxLen, Chars, Result);
    if Result > Length(S) then
      Result := Length(S);
  end
  else
    Result := MaxLen;
end;

{ MBCS Helper functions }

function StrCharLength(const Str: PChar): Integer;
begin
{$IFDEF LINUX}
  Result := mblen(Str, MB_CUR_MAX);
  if (Result = -1) then Result := 1;
{$ENDIF}
{$IFDEF MSWINDOWS}
  if SysLocale.FarEast then
    Result := Integer(CharNext(Str)) - Integer(Str)
  else
    Result := 1;
{$ENDIF}
end;

function StrNextChar(const Str: PChar): PChar;
begin
{$IFDEF LINUX}
  Result := Str + StrCharLength(Str);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := CharNext(Str);
{$ENDIF}
end;

function CharLength(const S: string; Index: Integer): Integer;
begin
  Result := 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := StrCharLength(PChar(S) + Index - 1);
end;

function NextCharIndex(const S: string; Index: Integer): Integer;
begin
  Result := Index + 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := Index + StrCharLength(PChar(S) + Index - 1);
end;

function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim)
    and (ByteType(S, Index) = mbSingleByte);
end;

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Length(S)) or (ByteType(S, Index) <> mbSingleByte) then exit;
  Result := StrScan(PChar(Delimiters), S[Index]) <> nil;
end;

function IncludeTrailingBackslash(const S: string): string;
begin
  Result := IncludeTrailingPathDelimiter(S);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

function ExcludeTrailingBackslash(const S: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(S);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function AnsiPos(const Substr, S: string): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := AnsiStrPos(PChar(S), PChar(SubStr));
  if P <> nil then
    Result := Integer(P) - Integer(PChar(S)) + 1;
end;

function AnsiCompareFileName(const S1, S2: string): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := AnsiCompareStr(AnsiLowerCaseFileName(S1), AnsiLowerCaseFileName(S2));
{$ENDIF}
{$IFDEF LINUX}
  Result := AnsiCompareStr(S1, S2);
{$ENDIF}
end;

function SameFileName(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareFileName(S1, S2) = 0;
end;

function AnsiLowerCaseFileName(const S: string): string;
{$IFDEF MSWINDOWS}
var
  I,L: Integer;
begin
  if SysLocale.FarEast then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['A'..'Z'] then Inc(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := AnsiLowerCase(S);
end;
{$ENDIF}

function AnsiUpperCaseFileName(const S: string): string;
{$IFDEF MSWINDOWS}
var
  I,L: Integer;
begin
  if SysLocale.FarEast then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['a'..'z'] then Dec(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiUpperCase(S);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := AnsiUpperCase(S);
end;
{$ENDIF}

function AnsiStrPos(Str, SubStr: PChar): PChar;
var
  L1, L2: Cardinal;
  ByteType : TMbcsByteType;
begin
  Result := nil;
  if (Str = nil) or (Str^ = #0) or (SubStr = nil) or (SubStr^ = #0) then Exit;
  L1 := StrLen(Str);
  L2 := StrLen(SubStr);
  Result := StrPos(Str, SubStr);
  while (Result <> nil) and ((L1 - Cardinal(Result - Str)) >= L2) do
  begin
    ByteType := StrByteType(Str, Integer(Result-Str));
{$IFDEF MSWINDOWS}
    if (ByteType <> mbTrailByte) and
      (CompareString(LOCALE_USER_DEFAULT, 0, Result, L2, SubStr, L2) = 2) then Exit;
    if (ByteType = mbLeadByte) then Inc(Result);
{$ENDIF}
{$IFDEF LINUX}
    if (ByteType <> mbTrailByte) and
      (strncmp(Result, SubStr, L2) = 0) then Exit;
{$ENDIF}
    Inc(Result);
    Result := StrPos(Result, SubStr);
  end;
  Result := nil;
end;

function AnsiStrRScan(Str: PChar; Chr: Char): PChar;
begin
  Str := AnsiStrScan(Str, Chr);
  Result := Str;
  if Chr <> #$0 then
  begin
    while Str <> nil do
    begin
      Result := Str;
      Inc(Str);
      Str := AnsiStrScan(Str, Chr);
    end;
  end
end;

function AnsiStrScan(Str: PChar; Chr: Char): PChar;
begin
  Result := StrScan(Str, Chr);
  while Result <> nil do
  begin
{$IFDEF MSWINDOWS}
    case StrByteType(Str, Integer(Result-Str)) of
      mbSingleByte: Exit;
      mbLeadByte: Inc(Result);
    end;
{$ENDIF}
{$IFDEF LINUX}
    if StrByteType(Str, Integer(Result-Str)) = mbSingleByte then Exit;
{$ENDIF}
    Inc(Result);
    Result := StrScan(Result, Chr);
  end;
end;

{$IFDEF MSWINDOWS}
function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;
{$ENDIF}

procedure InitSysLocale;
{$IFDEF MSWINDOWS}
var
  DefaultLCID: LCID;
  DefaultLangID: LANGID;
  AnsiCPInfo: TCPInfo;
  I: Integer;
  BufferA: array [128..255] of Char;
  BufferW: array [128..256] of Word;
  PCharA: PChar;

  procedure InitLeadBytes;
  var
    I: Integer;
    J: Byte;
  begin
    GetCPInfo(LCIDToCodePage(SysLocale.DefaultLCID), AnsiCPInfo);
    with AnsiCPInfo do
    begin
      I := 0;
      while (I < MAX_LEADBYTES) and ((LeadByte[I] or LeadByte[I + 1]) <> 0) do
      begin
        for J := LeadByte[I] to LeadByte[I + 1] do
          Include(LeadBytes, Char(J));
        Inc(I, 2);
      end;
    end;
  end;

  function IsWesternGroup: Boolean;
  type
    TLangGroup = $00..$1D;
    TLangGroups = set of TLangGroup;
  const
    lgNeutral = TLangGroup($00);
    lgDanish = TLangGroup($06);
    lgDutch = TLangGroup($13);
    lgEnglish  = TLangGroup($09);
    lgFinnish = TLangGroup($0B);
    lgFrench = TLangGroup($0C);
    lgGerman = TLangGroup($07);
    lgItalian  = TLangGroup($10);
    lgNorwegian = TLangGroup($14);
    lgPortuguese = TLangGroup($16);
    lgSpanish  = TLangGroup($0A);
    lgSwedish  = TLangGroup($1D);

    WesternGroups: TLangGroups = [
      lgNeutral,
      lgDanish,
      lgDutch,
      lgEnglish,
      lgFinnish,
      lgFrench,
      lgGerman,
      lgItalian,
      lgNorwegian,
      lgPortuguese,
      lgSpanish,
      lgSwedish
    ];
  begin
    Result := SysLocale.PriLangID in WesternGroups;
  end;

begin
  { Set default to English (US). }
  SysLocale.DefaultLCID := $0409;
  SysLocale.PriLangID := LANG_ENGLISH;
  SysLocale.SubLangID := SUBLANG_ENGLISH_US;

  DefaultLCID := GetThreadLocale;
  if DefaultLCID <> 0 then SysLocale.DefaultLCID := DefaultLCID;

  DefaultLangID := Word(DefaultLCID);
  if DefaultLangID <> 0 then
  begin
    SysLocale.PriLangID := DefaultLangID and $3ff;
    SysLocale.SubLangID := DefaultLangID shr 10;
  end;

  LeadBytes := [];
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if IsWesternGroup then
    begin
      SysLocale.MiddleEast := False;
      SysLocale.FarEast    := False;
    end
    else
    begin
      { Far East (aka MBCS)? - }
      InitLeadBytes;
      SysLocale.FarEast := LeadBytes <> [];
      if SysLocale.FarEast then
      begin
        SysLocale.MiddleEast := False;
        Exit;
      end;

      { Middle East? }
      for I := Low(BufferA) to High(BufferA) do
        BufferA[I] := Char(I);
      PCharA := @BufferA; { not null terminated: include length in GetStringTypeExA call }
      GetStringTypeEx(SysLocale.DefaultLCID, CT_CTYPE2, PCharA, High(BufferA) - Low(BufferA) + 1, BufferW);
      for I := Low(BufferA) to High(BufferA) do
      begin
        SysLocale.MiddleEast := BufferW[I] = C2_RIGHTTOLEFT;
        if SysLocale.MiddleEast then
          Exit;
      end;
    end;
  end
  else
  begin
    SysLocale.MiddleEast := GetSystemMetrics(SM_MIDEASTENABLED) <> 0;
    SysLocale.FarEast    := GetSystemMetrics(SM_DBCSENABLED) <> 0;
    if SysLocale.FarEast then
      InitLeadBytes;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I: Integer;
  buf: array [0..3] of char;
begin
  FillChar(SysLocale, sizeof(SysLocale), 0);
  SysLocale.FarEast := MB_CUR_MAX <> 1;
  if not SysLocale.FarEast then Exit;

  buf[1] := #0;
  for I := 1 to 255 do
  begin
    buf[0] := Chr(I);
    if mblen(buf, 1) <> 1 then Include(LeadBytes, Char(I));
  end;
end;
{$ENDIF}

procedure GetFormatSettings;
{$IFDEF MSWINDOWS}
var
  HourFormat, TimePrefix, TimePostfix: string;
  DefaultLCID: Integer;
begin
  InitSysLocale;
  GetMonthDayNames;
  if SysLocale.FarEast then GetEraNamesAndYearOffsets;
  DefaultLCID := GetThreadLocale;
  CurrencyString := GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '');
  CurrencyFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, '0'), 0);
  NegCurrFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, '0'), 0);
  ThousandSeparator := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
  DecimalSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
  CurrencyDecimals := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, '0'), 0);
  DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
  ShortDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy'));
  LongDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy'));
  TimeSeparator := GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
  TimeAMString := GetLocaleStr(DefaultLCID, LOCALE_S1159, 'am');
  TimePMString := GetLocaleStr(DefaultLCID, LOCALE_S2359, 'pm');
  TimePrefix := '';
  TimePostfix := '';
  if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0'), 0) = 0 then
    HourFormat := 'h' else
    HourFormat := 'hh';
  if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0'), 0) = 0 then
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
      TimePostfix := ' AMPM'
    else
      TimePrefix := 'AMPM ';
  ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  ListSeparator := GetLocaleChar(DefaultLCID, LOCALE_SLIST, ',');
end;
{$ELSE}
{$IFDEF LINUX}
const
  //first boolean is p_cs_precedes, second is p_sep_by_space
  CurrencyFormats: array[boolean, boolean] of byte = ((1, 3),(0, 2));
  //first boolean is n_cs_precedes, second is n_sep_by_space and finally n_sign_posn
  NegCurrFormats: array[boolean, boolean, 0..4] of byte =
    (((4,5,7,6,7),(15,8,10,13,10)),((0,1,3,1,2),(14,9,11,9,12)));

  function TranslateFormat(s: PChar; const Default: string): string;
  begin
    Result := '';
    while s^ <> #0 do
    begin
      if s^ = '%' then
      begin
        inc(s);
        case s^ of
          'a': Result := Result + 'ddd';
          'A': Result := Result + 'dddd';
          'b': Result := Result + 'MMM';
          'B': Result := Result + 'MMMM';
          'c': Result := Result + 'c';
//        'C':  year / 100 not supported
          'd': Result := Result + 'dd';
          'D': Result := Result + 'MM/dd/yy';
          'e': Result := Result + 'd';
//        'E': alternate format not supported
          'g': Result := Result + 'yy';
          'G': Result := Result + 'yyyy';
          'h': Result := Result + 'MMM';
          'H': Result := Result + 'HH';
          'I': Result := Result + 'hh';
//        'j': day of year not supported
          'k': Result := Result + 'H';
          'l': Result := Result + 'h';
          'm': Result := Result + 'MM';
          'M': Result := Result + 'nn';  // minutes! not months!
          'n': Result := Result + sLineBreak;  // line break
//        'O': alternate format not supported
          'P',   // P's implied lowercasing of locale string is not supported
          'p': Result := Result + 'AMPM';
          'r': Result := Result + TranslateFormat(nl_langInfo(T_FMT_AMPM),'');
          'R': Result := Result + 'HH:mm';
//        's': number of seconds since Epoch not supported
          'S': Result := Result + 'ss';
          't': Result := Result + #9;  // tab char
          'T': Result := Result + 'HH:mm:ss';
//        'u': day of week 1..7 not supported
//        'U': week number of the year not supported
//        'V': week number of the year not supported
//        'w': day of week 0..6 not supported
//        'W': week number of the year not supported
          'x': Result := Result + TranslateFormat(nl_langInfo(D_FMT),'');
          'X': Result := Result + TranslateFormat(nl_langinfo(T_FMT),'');
          'y': Result := Result + 'yy';
          'Y': Result := Result + 'yyyy';
//        'z': GMT offset is not supported
          '%': Result := Result + '%';
        end;
      end
      else
        Result := Result + s^;
      Inc(s);
    end;
    if Result = '' then
      Result := Default;
  end;

  function GetFirstCharacter(const SrcString, match: string): char;
  var
    i, p: integer;
  begin
    result := match[1];
    for i := 1 to length(SrcString) do begin
      p := Pos(SrcString[i], match);
      if p > 0 then
      begin
        result := match[p];
        break;
      end;
    end;
  end;

var
  P: PLConv;
begin
  InitSysLocale;
  GetMonthDayNames;
  if SysLocale.FarEast then InitEras;

  CurrencyString := '';
  CurrencyFormat := 0;
  NegCurrFormat := 0;
  ThousandSeparator := ',';
  DecimalSeparator := '.';
  CurrencyDecimals := 0;

  P := localeconv;
  if P <> nil then
  begin
    if P^.currency_symbol <> nil then
      CurrencyString := P^.currency_symbol;

    if (Byte(P^.p_cs_precedes) in [0..1]) and
       (Byte(P^.p_sep_by_space) in [0..1]) then
    begin
      CurrencyFormat := CurrencyFormats[P^.p_cs_precedes, P^.p_sep_by_space];
      if P^.p_sign_posn in [0..4] then
        NegCurrFormat := NegCurrFormats[P^.n_cs_precedes, P^.n_sep_by_space,
                                        P^.n_sign_posn];
    end;

    // #0 is valid for ThousandSeparator.  Indicates no thousand separator.
    ThousandSeparator := P^.thousands_sep^;

    // #0 is not valid for DecimalSeparator.
    if P^.decimal_point <> #0 then
      DecimalSeparator := P^.decimal_point^;
    CurrencyDecimals := P^.frac_digits;
  end;

  ShortDateFormat := TranslateFormat(nl_langinfo(D_FMT),'m/d/yy');
  LongDateFormat := TranslateFormat(nl_langinfo(D_T_FMT), ShortDateFormat);
  ShortTimeFormat := TranslateFormat(nl_langinfo(T_FMT), 'hh:mm AMPM');
  LongTimeFormat := TranslateFormat(nl_langinfo(T_FMT_AMPM), ShortTimeFormat);

  DateSeparator := GetFirstCharacter(ShortDateFormat, '/.-');
  TimeSeparator := GetFirstCharacter(ShortTimeFormat, ':.');

  TimeAMString := nl_langinfo(AM_STR);
  TimePMString := nl_langinfo(PM_STR);
  ListSeparator := ',';
end;
{$ELSE}
var
  HourFormat, TimePrefix, TimePostfix: string;
begin
  InitSysLocale;
  GetMonthDayNames;
  CurrencyString := '';
  CurrencyFormat := 0;
  NegCurrFormat := 0;
  ThousandSeparator := ',';
  DecimalSeparator := '.';
  CurrencyDecimals := 0;
  DateSeparator := '/';
  ShortDateFormat := 'm/d/yy';
  LongDateFormat := 'mmmm d, yyyy';
  TimeSeparator := ':';
  TimeAMString := 'am';
  TimePMString := 'pm';
  TimePrefix := '';
  TimePostfix := '';
  HourFormat := 'h';
  TimePostfix := ' AMPM';
  ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  ListSeparator := ',';
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure GetLocaleFormatSettings(LCID: Integer;
  var FormatSettings: TFormatSettings);
var
  HourFormat, TimePrefix, TimePostfix: string;
  DefaultLCID: Integer;
begin
  if IsValidLocale(LCID, LCID_INSTALLED) then
    DefaultLCID := LCID
  else
    DefaultLCID := GetThreadLocale;

  GetLocaleMonthDayNames(LCID, FormatSettings);
  with FormatSettings do
  begin
    CurrencyString := GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '');
    CurrencyFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, '0'), 0);
    NegCurrFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, '0'), 0);
    ThousandSeparator := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
    DecimalSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
    CurrencyDecimals := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, '0'), 0);
    DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
    ShortDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy'));
    LongDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy'));
    TimeSeparator := GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
    TimeAMString := GetLocaleStr(DefaultLCID, LOCALE_S1159, 'am');
    TimePMString := GetLocaleStr(DefaultLCID, LOCALE_S2359, 'pm');
    TimePrefix := '';
    TimePostfix := '';
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0'), 0) = 0 then
      HourFormat := 'h' else
      HourFormat := 'hh';
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0'), 0) = 0 then
      if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
        TimePostfix := ' AMPM'
      else
        TimePrefix := 'AMPM ';
    ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
    LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
    ListSeparator := GetLocaleChar(DefaultLCID, LOCALE_SLIST, ',');
  end;
end;
{$ENDIF}

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;
  MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  L: Integer;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := #0;
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar in LeadBytes then
    begin
      L := CharLength(Line, Pos) - 1;
      Inc(Pos, L);
      Inc(Col, L);
    end
    else
    begin
      if CurChar in QuoteChars then
        if QuoteChar = #0 then
          QuoteChar := CurChar
        else if CurChar = QuoteChar then
          QuoteChar := #0;
      if QuoteChar = #0 then   
      begin
        if CurChar = BreakStr[1] then
        begin
          ExistingBreak := StrLComp(Pointer(BreakStr), Pointer(@Line[Pos]), BreakLen) = 0;
          if ExistingBreak then
          begin
            Inc(Pos, BreakLen-1);
            BreakPos := Pos;
          end;
        end;
       
        if not ExistingBreak then
          if CurChar in BreakChars then
            BreakPos := Pos;
      end;
    end;

    Inc(Pos);
    Inc(Col);

    if not (QuoteChar in QuoteChars) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := 1;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (CurChar in QuoteChars) then
      begin
        while Pos <= LineLen do
        begin
          if Line[Pos] in BreakChars then
          begin
            Inc(Pos);
            ExistingBreak := False;
          end
          else
          begin
            ExistingBreak := StrLComp(Pointer(@Line[Pos]), sLineBreak, Length(sLineBreak)) = 0;
            if ExistingBreak then
              Inc(Pos, Length(sLineBreak))
            else
              Break;
          end;
        end;
      end;
      if (Pos <= LineLen) and not ExistingBreak then
        Result := Result + BreakStr;

      Inc(BreakPos);
      LinePos := BreakPos;
      Pos := LinePos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function WrapText(const Line: string; MaxCol: Integer): string;
begin
  Result := WrapText(Line, sLineBreak, [' ', '-', #9], MaxCol); { do not localize }
end;

function FindCmdLineSwitch(const Switch: string; const Chars: TSysCharSet;
  IgnoreCase: Boolean): Boolean;
var
  I: Integer;
  S: string;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (Chars = []) or (S[1] in Chars) then
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end
      else begin
        if (AnsiCompareStr(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end;
  end;
  Result := False;
end;

function FindCmdLineSwitch(const Switch: string): Boolean;
begin
  Result := FindCmdLineSwitch(Switch, SwitchChars, True);
end;

function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
begin
  Result := FindCmdLineSwitch(Switch, SwitchChars, IgnoreCase);
end;

{ Package info structures }

type
  PPkgName = ^TPkgName;
  TPkgName = packed record
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }
  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

  { Package flags:
    bit     meaning
    -----------------------------------------------------------------------------------------
    0     | 1: never-build                  0: always build
    1     | 1: design-time only             0: not design-time only      on => bit 2 = off
    2     | 1: run-time only                0: not run-time only         on => bit 1 = off
    3     | 1: do not check for dup units   0: perform normal dup unit check
    4..25 | reserved
    26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
    28..29| reserved
    30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
  }
  PPackageInfoHeader = ^TPackageInfoHeader;
  TPackageInfoHeader = packed record
    Flags: Cardinal;
    RequiresCount: Integer;
    {Requires: array[0..9999] of TPkgName;
    ContainsCount: Integer;
    Contains: array[0..9999] of TUnitName;}
  end;

function PackageInfoTable(Module: HMODULE): PPackageInfoHeader;
var
  ResInfo: HRSRC;
  Data: THandle;
begin
  Result := nil;
  ResInfo := FindResource(Module, 'PACKAGEINFO', RT_RCDATA);
  if ResInfo <> 0 then
  begin
    Data := LoadResource(Module, ResInfo);
    if Data <> 0 then
    try
      Result := LockResource(Data);
      UnlockResource(Data);
    finally
      FreeResource(Data);
    end;
  end;
end;

function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;

var
  Reserved: Integer;

procedure CheckForDuplicateUnits(Module: HMODULE);
var
  ModuleFlags: Cardinal;

  function IsUnitPresent(HC: Byte; UnitName: PChar; Module: HMODULE;
    const ModuleName: string; var UnitPackage: string): Boolean;
  var
    I: Integer;
    InfoTable: PPackageInfoHeader;
    LibModule: PLibModule;
    PkgName: PPkgName;
    UName : PUnitName;
    Count: Integer;
  begin
    Result := True;
    if (StrIComp(UnitName, 'SysInit') <> 0) and
      (StrIComp(UnitName, PChar(ModuleName)) <> 0) then
    begin
      LibModule := LibModuleList;
      while LibModule <> nil do
      begin
        if LibModule.Instance <> Cardinal(Module) then
        begin
          InfoTable := PackageInfoTable(HMODULE(LibModule.Instance));
          if (InfoTable <> nil) and (InfoTable.Flags and pfModuleTypeMask = pfPackageModule) and
            ((InfoTable.Flags and pfIgnoreDupUnits) = (ModuleFlags and pfIgnoreDupUnits)) then
          begin
            PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
            Count := InfoTable.RequiresCount;
            { Skip the Requires list }
            for I := 0 to Count - 1 do Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
            Count := Integer(Pointer(PkgName)^);
            UName := PUnitName(Integer(PkgName) + 4);
            for I := 0 to Count - 1 do
            begin
              with UName^ do
                // Test Flags to ignore weak package units
                if ((HashCode = HC) or (HashCode = 0) or (HC = 0)) and
                  ((Flags and $06) = 0) and (StrIComp(UnitName, Name) = 0) then
                begin
                  UnitPackage := ChangeFileExt(ExtractFileName(
                    GetModuleName(HMODULE(LibModule.Instance))), '');
                  Exit;
                end;
              Inc(Integer(UName), StrLen(UName.Name) + 3);
            end;
          end;
        end;
        LibModule := LibModule.Next;
      end;
    end;
    Result := False;
  end;

  function FindLibModule(Module: HModule): PLibModule;
  begin
    Result := LibModuleList;
    while Result <> nil do
    begin
      if Result.Instance = Cardinal(Module) then Exit;
      Result := Result.Next;
    end;
  end;

  procedure InternalUnitCheck(Module: HModule);
  var
    I: Integer;
    InfoTable: PPackageInfoHeader;
    UnitPackage: string;
    ModuleName: string;
    PkgName: PPkgName;
    UName: PUnitName;
    Count: Integer;
    LibModule: PLibModule;
  begin
    InfoTable := PackageInfoTable(Module);
    if (InfoTable <> nil) and (InfoTable.Flags and pfModuleTypeMask = pfPackageModule) then
    begin
      if ModuleFlags = 0 then ModuleFlags := InfoTable.Flags;
      ModuleName := ChangeFileExt(ExtractFileName(GetModuleName(Module)), '');
      PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
      Count := InfoTable.RequiresCount;
      for I := 0 to Count - 1 do
      begin
        with PkgName^ do
{$IFDEF MSWINDOWS}
          InternalUnitCheck(GetModuleHandle(PChar(ChangeFileExt(Name, '.bpl'))));
{$ENDIF}
{$IFDEF LINUX}
          InternalUnitCheck(GetModuleHandle(Name));
{$ENDIF}
          Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
      end;
      LibModule := FindLibModule(Module);
      if (LibModule = nil) or ((LibModule <> nil) and (LibModule.Reserved <> Reserved)) then
      begin
        if LibModule <> nil then LibModule.Reserved := Reserved;
        Count := Integer(Pointer(PkgName)^);
        UName := PUnitName(Integer(PkgName) + 4);
        for I := 0 to Count - 1 do
        begin
          with UName^ do
            // Test Flags to ignore weak package units
            if ((Flags and ufWeakPackageUnit) = 0 ) and
              IsUnitPresent(HashCode, Name, Module, ModuleName, UnitPackage) then
              raise EPackageError.CreateResFmt(@SDuplicatePackageUnit,
                [ModuleName, Name, UnitPackage]);
          Inc(Integer(UName), StrLen(UName.Name) + 3);
        end;
      end;
    end;
  end;

begin
  Inc(Reserved);
  ModuleFlags := 0;
  InternalUnitCheck(Module);
end;

{$IFDEF LINUX}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := HMODULE(dlopen(ModuleName, RTLD_LAZY));
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := LongBool(dlclose(Pointer(Module)));
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
var
  Info: TDLInfo;
  Error: PChar;
  ModHandle: HMODULE;
begin
  // dlsym doesn't clear the error state when the function succeeds
  dlerror;
  Result := dlsym(Pointer(Module), Proc);
  Error := dlerror;
  if Error <> nil then
    Result := nil
  else if dladdr(Result, Info) <> 0 then
  begin
{   In glibc 2.1.3 and earlier, dladdr returns a nil dli_fname
    for addresses in the main program file.  In glibc 2.1.91 and
    later, dladdr fills in the dli_fname for addresses in the
    main program file, but dlopen will segfault when given
    the main program file name.
    Workaround:  Check the symbol base address against the main
    program file's base address, and only call dlopen with a nil
    filename to get the module name of the main program.  }

    if Info.dli_fbase = ExeBaseAddress then
      Info.dli_fname := nil;

    ModHandle := HMODULE(dlopen(Info.dli_fname, RTLD_LAZY));
    if ModHandle <> 0 then
    begin
      dlclose(Pointer(ModHandle));
      if ModHandle <> Module then
        Result := nil;
    end;
  end else Result := nil;
end;

type
  plink_map = ^link_map;
  link_map = record
    l_addr: Pointer;
    l_name: PChar;
    l_ld: Pointer;
    l_next, l_prev: plink_map;
  end;

  pr_debug = ^r_debug;
  r_debug = record
    r_version: Integer;
    r_map: plink_map;
    r_brk: Pointer;
    r_state: Integer;
    r_ldbase: Pointer;
  end;

var
  _r_debug: pr_debug = nil;

function ScanLinkMap(Func: Pointer): plink_map;
var
  linkmap: plink_map;

  function Eval(linkmap: plink_map; Func: Pointer): Boolean;
  asm
//        MOV    ECX,[EBP]
        PUSH   EBP
        CALL   EDX
        POP    ECX
  end;

begin
  if _r_debug = nil then
    _r_debug := dlsym(RTLD_DEFAULT, '_r_debug');
  if _r_debug = nil then
  begin
    Assert(False, 'Unable to locate ''_r_debug'' symbol'); // do not localize
    Result := nil;
    Exit;
  end;
  linkmap := _r_debug.r_map;
  while linkmap <> nil do
  begin
    if not Eval(linkmap, Func) then Break;
    linkmap := linkmap.l_next;
  end;
  Result := linkmap;
end;

function InitModule(linkmap: plink_map): HMODULE;
begin
  if linkmap <> nil then
  begin
    Result := HMODULE(dlopen(linkmap.l_name, RTLD_LAZY));
    if Result <> 0 then
      dlclose(Pointer(Result));
  end else Result := 0;
end;

function GetModuleHandle(ModuleName: PChar): HMODULE;

  function CheckModuleName(linkmap: plink_map): Boolean;
  var
    BaseName: PChar;
  begin
    Result := True;
    if ((ModuleName = nil) and ((linkmap.l_name = nil) or (linkmap.l_name[0] = #0))) or
      ((ModuleName[0] = PathDelim) and (StrComp(ModuleName, linkmap.l_name) = 0)) then
    begin
      Result := False;
      Exit;
    end else
    begin
      // Locate the start of the actual filename
      BaseName := StrRScan(linkmap.l_name, PathDelim);
      if BaseName = nil then
        BaseName := linkmap.l_name
      else Inc(BaseName); // The filename is actually located at BaseName+1
      if StrComp(ModuleName, BaseName) = 0 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

begin
  Result := InitModule(ScanLinkMap(@CheckModuleName));
end;

function GetPackageModuleHandle(PackageName: PChar): HMODULE;
var
  PkgName: array[0..MAX_PATH] of Char;

  function CheckPackageName(linkmap: plink_map): Boolean;
  var
    BaseName: PChar;
  begin
    Result := True;
    if linkmap.l_name <> nil then
    begin
      // Locate the start of the actual filename
      BaseName := StrRScan(linkmap.l_name, PathDelim);
      if BaseName = nil then
        BaseName := linkmap.l_name  // If there is no path info, just use the whole name
      else Inc(BaseName); // The filename is actually located at BaseName+1
      Result := StrPos(BaseName, PkgName) = nil;
    end;
  end;

  procedure MakePkgName(Prefix, Name: PChar);
  begin
    StrCopy(PkgName, Prefix);
    StrLCat(PkgName, Name, sizeof(PkgName)-1);
    PkgName[High(PkgName)] := #0;
  end;

begin
  if (PackageName = nil) or (StrScan(PackageName, PathDelim) <> nil) then
    Result := 0
  else
  begin
    MakePkgName('bpl', PackageName); // First check the default prefix
    Result := InitModule(ScanLinkMap(@CheckPackageName));
    if Result = 0 then
    begin
      MakePkgName('dcl', PackageName); // Next check the design-time prefix
      Result := InitModule(ScanLinkMap(@CheckPackageName));
      if Result = 0 then
      begin
        MakePkgName('', PackageName);  // finally check without a prefix
        Result := InitModule(ScanLinkMap(@CheckPackageName));
      end;
    end;
  end;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
procedure Sleep; external kernel32 name 'Sleep'; stdcall;
{$ENDIF}
{$IFDEF LINUX}
procedure Sleep(milliseconds: Cardinal);
begin
  usleep(milliseconds * 1000);  // usleep is in microseconds
end;
{$ENDIF}

{ InitializePackage }

procedure InitializePackage(Module: HMODULE);
type
  TPackageLoad = procedure;
var
  PackageLoad: TPackageLoad;
begin
  CheckForDuplicateUnits(Module);
  @PackageLoad := GetProcAddress(Module, 'Initialize'); //Do not localize
  if Assigned(PackageLoad) then
    PackageLoad
  else
    raise EPackageError.CreateFmt(sInvalidPackageFile, [GetModuleName(Module)]);
end;

{ FinalizePackage }

procedure FinalizePackage(Module: HMODULE);
type
  TPackageUnload = procedure;
var
  PackageUnload: TPackageUnload;
begin
  @PackageUnload := GetProcAddress(Module, 'Finalize'); //Do not localize
  if Assigned(PackageUnload) then
    PackageUnload
  else
    raise EPackageError.CreateRes(@sInvalidPackageHandle);
end;

{ LoadPackage }

function LoadPackage(const Name: string): HMODULE;
{$IFDEF LINUX}
var
  DLErrorMsg: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := SafeLoadLibrary(Name);
{$ENDIF}
{$IFDEF LINUX}
  Result := HMODULE(dlOpen(PChar(Name), PkgLoadingMode));
{$ENDIF}
  if Result = 0 then
  begin
{$IFDEF LINUX}
    DLErrorMsg := dlerror;
{$ENDIF}
    raise EPackageError.CreateResFmt(@sErrorLoadingPackage,
      [Name,
       {$IFDEF MSWINDOWS}SysErrorMessage(GetLastError){$ENDIF}
       {$IFDEF LINUX}DLErrorMsg{$ENDIF}]);
  end;
  try
    InitializePackage(Result);
  except
{$IFDEF MSWINDOWS}
    FreeLibrary(Result);
{$ENDIF}
{$IFDEF LINUX}
    dlclose(Pointer(Result));
{$ENDIF}
    raise;
  end;
end;

{ UnloadPackage }

procedure UnloadPackage(Module: HMODULE);
begin
  FinalizePackage(Module);
{$IFDEF MSWINDOWS}
  FreeLibrary(Module);
{$ENDIF}
{$IFDEF LINUX}
  dlclose(Pointer(Module));
  InvalidateModuleCache;
{$ENDIF}
end;

{ GetPackageInfo }

procedure GetPackageInfo(Module: HMODULE; Param: Pointer; var Flags: Integer;
  InfoProc: TPackageInfoProc);
var
  InfoTable: PPackageInfoHeader;
  I: Integer;
  PkgName: PPkgName;
  UName: PUnitName;
  Count: Integer;
begin
  InfoTable := PackageInfoTable(Module);
  if not Assigned(InfoTable) then
    raise EPackageError.CreateFmt(SCannotReadPackageInfo,
      [ExtractFileName(GetModuleName(Module))]);
  Flags := InfoTable.Flags;
  with InfoTable^ do
  begin
    PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
    Count := RequiresCount;
    for I := 0 to Count - 1 do
    begin
      InfoProc(PkgName.Name, ntRequiresPackage, 0, Param);
      Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
    end;
    Count := Integer(Pointer(PkgName)^);
    UName := PUnitName(Integer(PkgName) + 4);
    for I := 0 to Count - 1 do
    begin
      InfoProc(UName.Name, ntContainsUnit, UName.Flags, Param);
      Inc(Integer(UName), StrLen(UName.Name) + 3);
    end;
    if Flags and pfPackageModule <> 0 then
    begin
      PkgName := PPkgName(UName);
      InfoProc(PkgName.Name, ntDcpBpiName, 0, Param);
    end;
  end;
end;

function GetPackageDescription(ModuleName: PChar): string;
var
  ResModule: HMODULE;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
{$IFDEF LINUX}
  DLErrorMsg: string;
{$ENDIF}
begin
  Result := '';
  ResModule := LoadResourceModule(ModuleName);
  if ResModule = 0 then
  begin
{$IFDEF MSWINDOWS}
    ResModule := LoadLibraryEx(ModuleName, 0, LOAD_LIBRARY_AS_DATAFILE);
{$ENDIF}
{$IFDEF LINUX}
    ResModule := HMODULE(dlopen(ModuleName, RTLD_LAZY));
{$ENDIF}
    if ResModule = 0 then
    begin
{$IFDEF LINUX}
      DLErrorMsg := dlerror;
{$ENDIF}
      raise EPackageError.CreateResFmt(@sErrorLoadingPackage,
        [ModuleName,
         {$IFDEF MSWINDOWS}SysErrorMessage(GetLastError){$ENDIF}
         {$IFDEF LINUX}DLErrorMsg{$ENDIF}]);
    end;
  end;
  try
    ResInfo := FindResource(ResModule, 'DESCRIPTION', RT_RCDATA);
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        Result := PWideChar(LockResource(ResData));
        UnlockResource(ResData);
      finally
        FreeResource(ResData);
      end;
    end;
  finally
{$IFDEF MSWINDOWS}
    FreeLibrary(ResModule);
{$ENDIF}
{$IFDEF LINUX}
    dlclose(Pointer(ResModule));
{$ENDIF}
  end;
end;

procedure RaiseLastOSError;
var
  LastError: Integer;
  Error: EOSError;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := EOSError.CreateResFmt(@SOSError, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;

{$IFDEF MSWINDOWS}
{ RaiseLastWin32Error }

procedure RaiseLastWin32Error;
begin
  RaiseLastOSError;
end;

{ Win32Check }

function Win32Check(RetVal: BOOL): BOOL;
begin
  if not RetVal then RaiseLastOSError;
  Result := RetVal;
end;
{$ENDIF}

type
  PTerminateProcInfo = ^TTerminateProcInfo;
  TTerminateProcInfo = record
    Next: PTerminateProcInfo;
    Proc: TTerminateProc;
  end;

var
  TerminateProcList: PTerminateProcInfo = nil;

procedure AddTerminateProc(TermProc: TTerminateProc);
var
  P: PTerminateProcInfo;
begin
  New(P);
  P^.Next := TerminateProcList;
  P^.Proc := TermProc;
  TerminateProcList := P;
end;

function CallTerminateProcs: Boolean;
var
  PI: PTerminateProcInfo;
begin
  Result := True;
  PI := TerminateProcList;
  while Result and (PI <> nil) do
  begin
    Result := PI^.Proc;
    PI := PI^.Next;
  end;
end;

procedure FreeTerminateProcs;
var
  PI: PTerminateProcInfo;
begin
  while TerminateProcList <> nil do
  begin
    PI := TerminateProcList;
    TerminateProcList := PI^.Next;
    Dispose(PI);
  end;
end;

{ --- }
function AL1(const P): LongWord;
asm
        MOV     EDX,DWORD PTR [P]
        XOR     EDX,DWORD PTR [P+4]
        XOR     EDX,DWORD PTR [P+8]
        XOR     EDX,DWORD PTR [P+12]
        MOV     EAX,EDX
end;

function AL2(const P): LongWord;
asm
        MOV     EDX,DWORD PTR [P]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+4]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+8]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+12]
        MOV     EAX,EDX
end;

const
  AL1s: array[0..3] of LongWord = ($FFFFFFF0, $FFFFEBF0, 0, $FFFFFFFF);
  AL2s: array[0..3] of LongWord = ($42C3ECEF, $20F7AEB6, $D1C2F74E, $3F6574DE);

procedure ALV;
begin
  raise Exception.CreateRes(@SNL);
end;

function ALR: Pointer;
var
  LibModule: PLibModule;
begin
  if MainInstance <> 0 then
    Result := Pointer(LoadResource(MainInstance, FindResource(MainInstance, 'DVCLAL',
      RT_RCDATA)))
  else
  begin
    Result := nil;
    LibModule := LibModuleList;
    while LibModule <> nil do
    begin
      with LibModule^ do
      begin
        Result := Pointer(LoadResource(Instance, FindResource(Instance, 'DVCLAL',
          RT_RCDATA)));
        if Result <> nil then Break;
      end;
      LibModule := LibModule.Next;
    end;
  end;
end;

function GDAL: LongWord;
type
  TDVCLAL = array[0..3] of LongWord;
  PDVCLAL = ^TDVCLAL;
var
  P: Pointer;
  A1, A2: LongWord;
  PAL1s, PAL2s: PDVCLAL;
  ALOK: Boolean;
begin
  P := ALR;
  if P <> nil then
  begin
    A1 := AL1(P^);
    A2 := AL2(P^);
    Result := A1;
    PAL1s := @AL1s;
    PAL2s := @AL2s;
    ALOK := ((A1 = PAL1s[0]) and (A2 = PAL2s[0])) or
            ((A1 = PAL1s[1]) and (A2 = PAL2s[1])) or
            ((A1 = PAL1s[2]) and (A2 = PAL2s[2]));
    FreeResource(Integer(P));
    if not ALOK then ALV;
  end else Result := AL1s[3];
end;

procedure RCS;
var
  P: Pointer;
  ALOK: Boolean;
begin
  P := ALR;
  if P <> nil then
  begin
    ALOK := (AL1(P^) = AL1s[2]) and (AL2(P^) = AL2s[2]);
    FreeResource(Integer(P));
  end else ALOK := False;
  if not ALOK then ALV;
end;

procedure RPR;
var
  AL: LongWord;
begin
  AL := GDAL;
  if (AL <> AL1s[1]) and (AL <> AL1s[2]) then ALV;
end;

{$IFDEF MSWINDOWS}
procedure InitDriveSpacePtr;
var
  Kernel: THandle;
begin
  Kernel := GetModuleHandle(Windows.Kernel32);
  if Kernel <> 0 then
    @GetDiskFreeSpaceEx := GetProcAddress(Kernel, 'GetDiskFreeSpaceExA');
  if not Assigned(GetDiskFreeSpaceEx) then
    GetDiskFreeSpaceEx := @BackfillGetDiskFreeSpaceEx;
end;
{$ENDIF}

// Win95 does not return the actual value of the result.
// These implementations are consistent on all platforms.
function InterlockedIncrement(var I: Integer): Integer;
asm
        MOV     EDX,1
        XCHG    EAX,EDX
  LOCK  XADD    [EDX],EAX
        INC     EAX
end;

function InterlockedDecrement(var I: Integer): Integer;
asm
        MOV     EDX,-1
        XCHG    EAX,EDX
  LOCK  XADD    [EDX],EAX
        DEC     EAX
end;

function InterlockedExchange(var A: Integer; B: Integer): Integer;
asm
        XCHG    [EAX],EDX
        MOV     EAX,EDX
end;

// The InterlockedExchangeAdd Win32 API is not available on Win95.
function InterlockedExchangeAdd(var A: Integer; B: Integer): Integer;
asm
        XCHG    EAX,EDX
  LOCK  XADD    [EDX],EAX
end;


{ TSimpleRWSync }

constructor TSimpleRWSync.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TSimpleRWSync.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(FLock);
end;

function TSimpleRWSync.BeginWrite: Boolean;
begin
  EnterCriticalSection(FLock);
  Result := True;
end;

procedure TSimpleRWSync.EndWrite;
begin
  LeaveCriticalSection(FLock);
end;

procedure TSimpleRWSync.BeginRead;
begin
  EnterCriticalSection(FLock);
end;

procedure TSimpleRWSync.EndRead;
begin
  LeaveCriticalSection(FLock);
end;

{ TThreadLocalCounter }

const
  Alive = High(Integer);

destructor TThreadLocalCounter.Destroy;
var
  P, Q: PThreadInfo;
  I: Integer;
begin
  for I := 0 to High(FHashTable) do
  begin
    P := FHashTable[I];
    FHashTable[I] := nil;
    while P <> nil do
    begin
      Q := P;
      P := P^.Next;
      FreeMem(Q);
    end;
  end;
  inherited Destroy;
end;

function TThreadLocalCounter.HashIndex: Byte;
var
  H: Word;
begin
  H := Word(GetCurrentThreadID);
  Result := (WordRec(H).Lo xor WordRec(H).Hi) and 15;
end;

procedure TThreadLocalCounter.Open(var Thread: PThreadInfo);
var
  P: PThreadInfo;
  CurThread: Cardinal;
  H: Byte;
begin
  H := HashIndex;
  CurThread := GetCurrentThreadID;

  P := FHashTable[H];
  while (P <> nil) and (P.ThreadID <> CurThread) do
    P := P.Next;

  if P = nil then
  begin
    P := Recycle;

    if P = nil then
    begin
      P := PThreadInfo(AllocMem(sizeof(TThreadInfo)));
      P.ThreadID := CurThread;
      P.Active := Alive;

      // Another thread could start traversing the list between when we set the
      // head to P and when we assign to P.Next.  Initializing P.Next to point
      // to itself will make others spin until we assign the tail to P.Next.
      P.Next := P;
      P.Next := PThreadInfo(InterlockedExchange(Integer(FHashTable[H]), Integer(P)));
    end;
  end;
  Thread := P;
end;

procedure TThreadLocalCounter.Close(var Thread: PThreadInfo);
begin
  Thread := nil;
end;

procedure TThreadLocalCounter.Delete(var Thread: PThreadInfo);
begin
  Thread.ThreadID := 0;
  Thread.Active := 0;
end;

function TThreadLocalCounter.Recycle: PThreadInfo;
var
  Gen: Integer;
begin
  Result := FHashTable[HashIndex];
  while (Result <> nil) do
  begin
    Gen := InterlockedExchange(Result.Active, Alive);
    if Gen <> Alive then
    begin
      Result.ThreadID := GetCurrentThreadID;
      Exit;
    end
    else
      Result := Result.Next;
  end;
end;


{$IFDEF MSWINDOWS}
{ TMultiReadExclusiveWriteSynchronizer }
const
  mrWriteRequest = $FFFF; // 65535 concurrent read requests (threads)
                          // 32768 concurrent write requests (threads)
                          // only one write lock at a time
                          // 2^32 lock recursions per thread (read and write combined)

constructor TMultiReadExclusiveWriteSynchronizer.Create;
begin
  inherited Create;
  FSentinel := mrWriteRequest;
  FReadSignal := CreateEvent(nil, True, True, nil);  // manual reset, start signaled
  FWriteSignal := CreateEvent(nil, False, False, nil); // auto reset, start blocked
  FWaitRecycle := INFINITE;
  tls := TThreadLocalCounter.Create;
end;

destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  BeginWrite;
  inherited Destroy;
  CloseHandle(FReadSignal);
  CloseHandle(FWriteSignal);
  tls.Free;
end;

procedure TMultiReadExclusiveWriteSynchronizer.BlockReaders;
begin
  ResetEvent(FReadSignal);
end;

procedure TMultiReadExclusiveWriteSynchronizer.UnblockReaders;
begin
  SetEvent(FReadSignal);
end;

procedure TMultiReadExclusiveWriteSynchronizer.UnblockOneWriter;
begin
  SetEvent(FWriteSignal);
end;

procedure TMultiReadExclusiveWriteSynchronizer.WaitForReadSignal;
begin
  WaitForSingleObject(FReadSignal, FWaitRecycle);
end;

procedure TMultiReadExclusiveWriteSynchronizer.WaitForWriteSignal;
begin
  WaitForSingleObject(FWriteSignal, FWaitRecycle);
end;

{$IFDEF DEBUG_MREWS}
var
  x: Integer;

procedure TMultiReadExclusiveWriteSynchronizer.Debug(const Msg: string);
begin
  OutputDebugString(PChar(Format('%d %s Thread=%x Sentinel=%d, FWriterID=%x',
    [InterlockedIncrement(x), Msg, GetCurrentThreadID, FSentinel, FWriterID])));
end;
{$ENDIF}

function TMultiReadExclusiveWriteSynchronizer.BeginWrite: Boolean;
var
  Thread: PThreadInfo;
  HasReadLock: Boolean;
  ThreadID: Cardinal;
  Test: Integer;
  OldRevisionLevel: Cardinal;
begin
  {
    States of FSentinel (roughly - during inc/dec's, the states may not be exactly what is said here):
    mrWriteRequest:         A reader or a writer can get the lock
    1 - (mrWriteRequest-1): A reader (possibly more than one) has the lock
    0:                      A writer (possibly) just got the lock, if returned from the main write While loop
    < 0, but not a multiple of mrWriteRequest: Writer(s) want the lock, but reader(s) have it.
          New readers should be blocked, but current readers should be able to call BeginRead     
    < 0, but a multiple of mrWriteRequest: Writer(s) waiting for a writer to finish
  }


{$IFDEF DEBUG_MREWS}
  Debug('Write enter------------------------------------');
{$ENDIF}
  Result := True;
  ThreadID := GetCurrentThreadID;
  if FWriterID <> ThreadID then  // somebody or nobody has a write lock
  begin
    // Prevent new readers from entering while we wait for the existing readers
    // to exit.
    BlockReaders;

    OldRevisionLevel := FRevisionLevel;

    tls.Open(Thread);
    // We have another lock already. It must be a read lock, because if it
    // were a write lock, FWriterID would be our threadid.
    HasReadLock := Thread.RecursionCount > 0;

    if HasReadLock then    // acquiring a write lock requires releasing read locks
      InterlockedIncrement(FSentinel);

{$IFDEF DEBUG_MREWS}
    Debug('Write before loop');
{$ENDIF}
    // InterlockedExchangeAdd returns prev value
    while InterlockedExchangeAdd(FSentinel, -mrWriteRequest) <> mrWriteRequest do
    begin
{$IFDEF DEBUG_MREWS}
      Debug('Write loop');
      Sleep(1000); // sleep to force / debug race condition
      Debug('Write loop2a');
{$ENDIF}

      // Undo what we did, since we didn't get the lock
      Test := InterlockedExchangeAdd(FSentinel, mrWriteRequest);
      // If the old value (in Test) was 0, then we may be able to
      // get the lock (because it will now be mrWriteRequest). So,
      // we continue the loop to find out. Otherwise, we go to sleep,
      // waiting for a reader or writer to signal us.

      if Test <> 0 then
      begin
        {$IFDEF DEBUG_MREWS}
        Debug('Write starting to wait');
        {$ENDIF}
        WaitForWriteSignal;
      end
      {$IFDEF DEBUG_MREWS}
      else
        Debug('Write continue')
      {$ENDIF}
    end;

    // At the EndWrite, first Writers are awoken, and then Readers are awoken.
    // If a Writer got the lock, we don't want the readers to do busy
    // waiting. This Block resets the event in case the situation happened.
    BlockReaders;

    // Put our read lock marker back before we lose track of it
    if HasReadLock then
      InterlockedDecrement(FSentinel);

    FWriterID := ThreadID;

    Result := Integer(OldRevisionLevel) = (InterlockedIncrement(Integer(FRevisionLevel)) - 1);
  end;

  Inc(FWriteRecursionCount);
{$IFDEF DEBUG_MREWS}
  Debug('Write lock-----------------------------------');
{$ENDIF}
end;

procedure TMultiReadExclusiveWriteSynchronizer.EndWrite;
var
  Thread: PThreadInfo;
begin
{$IFDEF DEBUG_MREWS}
  Debug('Write end');
{$ENDIF}
  assert(FWriterID = GetCurrentThreadID);
  tls.Open(Thread);
  Dec(FWriteRecursionCount);
  if FWriteRecursionCount = 0 then
  begin
    FWriterID := 0;
    InterlockedExchangeAdd(FSentinel, mrWriteRequest);
    {$IFDEF DEBUG_MREWS}
    Debug('Write about to UnblockOneWriter');
    {$ENDIF}
    UnblockOneWriter;
    {$IFDEF DEBUG_MREWS}
    Debug('Write about to UnblockReaders');
    {$ENDIF}
    UnblockReaders;
  end;
  if Thread.RecursionCount = 0 then
    tls.Delete(Thread);
{$IFDEF DEBUG_MREWS}
  Debug('Write unlock');
{$ENDIF}
end;

procedure TMultiReadExclusiveWriteSynchronizer.BeginRead;
var
  Thread: PThreadInfo;
  WasRecursive: Boolean;
  SentValue: Integer;
begin
{$IFDEF DEBUG_MREWS}
  Debug('Read enter');
{$ENDIF}

  tls.Open(Thread);
  Inc(Thread.RecursionCount);
  WasRecursive := Thread.RecursionCount > 1;

  if FWriterID <> GetCurrentThreadID then
  begin
{$IFDEF DEBUG_MREWS}
    Debug('Trying to get the ReadLock (we did not have a write lock)');
{$ENDIF}
    // In order to prevent recursive Reads from causing deadlock,
    // we need to always WaitForReadSignal if not recursive.
    // This prevents unnecessarily decrementing the FSentinel, and
    // then immediately incrementing it again.
    if not WasRecursive then
    begin
      // Make sure we don't starve writers. A writer will
      // always set the read signal when it is done, and it is initially on.
      WaitForReadSignal;
      while (InterlockedDecrement(FSentinel) <= 0) do
      begin
  {$IFDEF DEBUG_MREWS}
        Debug('Read loop');
  {$ENDIF}
        // Because the InterlockedDecrement happened, it is possible that
        // other threads "think" we have the read lock,
        // even though we really don't. If we are the last reader to do this,
        // then SentValue will become mrWriteRequest
        SentValue := InterlockedIncrement(FSentinel);
        // So, if we did inc it to mrWriteRequest at this point,
        // we need to signal the writer.
        if SentValue = mrWriteRequest then
          UnblockOneWriter;
        
        // This sleep below prevents starvation of writers
        Sleep(0);

  {$IFDEF DEBUG_MREWS}
        Debug('Read loop2 - waiting to be signaled');
  {$ENDIF}
        WaitForReadSignal;
  {$IFDEF DEBUG_MREWS}
        Debug('Read signaled');
  {$ENDIF}
      end;
    end;
  end;
{$IFDEF DEBUG_MREWS}
  Debug('Read lock');
{$ENDIF}
end;

procedure TMultiReadExclusiveWriteSynchronizer.EndRead;
var
  Thread: PThreadInfo;
  Test: Integer;
begin
{$IFDEF DEBUG_MREWS}
  Debug('Read end');
{$ENDIF}
  tls.Open(Thread);
  Dec(Thread.RecursionCount);
  if (Thread.RecursionCount = 0) then
  begin
     tls.Delete(Thread);

    // original code below commented out
    if (FWriterID <> GetCurrentThreadID) then
    begin
      Test := InterlockedIncrement(FSentinel);
      // It is possible for Test to be mrWriteRequest
      // or, it can be = 0, if the write loops:
      // Test := InterlockedExchangeAdd(FSentinel, mrWriteRequest) + mrWriteRequest;
      // Did not get executed before this has called (the sleep debug makes it happen faster)
      {$IFDEF DEBUG_MREWS}
      Debug(Format('Read UnblockOneWriter may be called. Test=%d', [Test]));
      {$ENDIF}
      if Test = mrWriteRequest then
        UnblockOneWriter
      else if Test <= 0 then // We may have some writers waiting
      begin
        if (Test mod mrWriteRequest) = 0 then
          UnblockOneWriter; // No more readers left (only writers) so signal one of them
      end;
    end;
  end;
{$IFDEF DEBUG_MREWS}
  Debug('Read unlock');
{$ENDIF}
end;
{$ENDIF} //MSWINDOWS for TMultiReadExclusiveWriteSynchronizer

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

{ Interface support routines }

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;

function Supports(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result := Supports(Instance, IID, Temp);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result := Supports(Instance, IID, Temp);
end;

function Supports(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(IID) <> nil;
end;

{$IFDEF MSWINDOWS}
{ TLanguages }

{ Query the OS for information for a specified locale. Unicode version. Works correctly on Asian WinNT. }
function GetLocaleDataW(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of WideChar;
begin
  Buffer[0] := #0;
  GetLocaleInfoW(ID, Flag, Buffer, SizeOf(Buffer) div 2);
  Result := Buffer;
end;

{ Query the OS for information for a specified locale. ANSI Version. Works correctly on Asian Win95. }
function GetLocaleDataA(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of Char;
begin
  Buffer[0] := #0;
  SetString(Result, Buffer, GetLocaleInfoA(ID, Flag, Buffer, SizeOf(Buffer)) - 1);
end;

{ Called for each supported locale. }
function TLanguages.LocalesCallback(LocaleID: PChar): Integer; stdcall;
var
  AID: LCID;
  ShortLangName: string;
  GetLocaleDataProc: function (ID: LCID; Flag: DWORD): string;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    GetLocaleDataProc := @GetLocaleDataW
  else
    GetLocaleDataProc := @GetLocaleDataA;
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  ShortLangName := GetLocaleDataProc(AID, LOCALE_SABBREVLANGNAME);
  if ShortLangName <> '' then
  begin
    SetLength(FSysLangs, Length(FSysLangs) + 1);
    with FSysLangs[High(FSysLangs)] do
    begin
      FName := GetLocaleDataProc(AID, LOCALE_SLANGUAGE);
      FLCID := AID;
      FExt := ShortLangName;
    end;
  end;
  Result := 1;
end;

constructor TLanguages.Create;
type
  TCallbackThunk = packed record
    POPEDX: Byte;
    MOVEAX: Byte;
    SelfPtr: Pointer;
    PUSHEAX: Byte;
    PUSHEDX: Byte;
    JMP: Byte;
    JmpOffset: Integer;
  end;
var
  Callback: TCallbackThunk;
begin
  inherited Create;
  Callback.POPEDX := $5A;
  Callback.MOVEAX := $B8;
  Callback.SelfPtr := Self;
  Callback.PUSHEAX := $50;
  Callback.PUSHEDX := $52;
  Callback.JMP     := $E9;
  Callback.JmpOffset := Integer(@TLanguages.LocalesCallback) - Integer(@Callback.JMP) - 5;
  EnumSystemLocales(TFNLocaleEnumProc(@Callback), LCID_SUPPORTED);
end;

function TLanguages.GetCount: Integer;
begin
  Result := High(FSysLangs) + 1;
end;

function TLanguages.GetExt(Index: Integer): string;
begin
  Result := FSysLangs[Index].FExt;
end;

function TLanguages.GetID(Index: Integer): string;
begin
  Result := HexDisplayPrefix + IntToHex(FSysLangs[Index].FLCID, 8);
end;

function TLanguages.GetLCID(Index: Integer): LCID;
begin
  Result := FSysLangs[Index].FLCID;
end;

function TLanguages.GetName(Index: Integer): string;
begin
  Result := FSysLangs[Index].FName;
end;

function TLanguages.GetNameFromLocaleID(ID: LCID): string;
var
  Index: Integer;
begin
  Index := IndexOf(ID);
  if Index <> - 1 then Result := Name[Index];
  if Result = '' then Result := sUnknown;
end;

function TLanguages.GetNameFromLCID(const ID: string): string;
begin
  Result := NameFromLocaleID[StrToIntDef(ID, 0)];
end;

function TLanguages.IndexOf(ID: LCID): Integer;
begin
  for Result := Low(FSysLangs) to High(FSysLangs) do
    if FSysLangs[Result].FLCID = ID then Exit;
  Result := -1;
end;

var
  FLanguages: TLanguages;

function Languages: TLanguages;
begin
  if FLanguages = nil then
    FLanguages := TLanguages.Create;
  Result := FLanguages;
end;

function SafeLoadLibrary(const Filename: string; ErrorMode: UINT): HMODULE;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
function SafeLoadLibrary(const FileName: string; Dummy: LongWord): HMODULE;
var
  FPUControlWord: Word;
begin
  asm
    FNSTCW  FPUControlWord
  end;
  try
    Result := LoadLibrary(PChar(Filename));
  finally
    asm
      FNCLEX
      FLDCW FPUControlWord
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function GetEnvironmentVariable(const Name: string): string;
const
  BufSize = 1024;
var
  Len: Integer;
  Buffer: array[0..BufSize - 1] of Char;
begin
  Result := '';
  Len := GetEnvironmentVariable(PChar(Name), @Buffer, BufSize);
  if Len < BufSize then
    SetString(Result, PChar(@Buffer), Len)
  else
  begin
    SetLength(Result, Len - 1);
    GetEnvironmentVariable(PChar(Name), PChar(Result), Len);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
function GetEnvironmentVariable(const Name: string): string;
begin
  Result := getenv(PChar(Name));
end;
{$ENDIF}

{$IFDEF LINUX}
procedure CheckLocale;
var
  P,Q: PChar;
begin
  P := gnu_get_libc_version();
  Q := getenv('LC_ALL');
  if (Q = nil) or (Q[0] = #0) then
    Q := getenv('LANG');

  //  2.1.3 <= current version < 2.1.91
  if (strverscmp('2.1.3', P) <= 0) and
     (strverscmp(P, '2.1.91') < 0) and
     ((Q = nil) or (Q[0] = #0)) then
  begin
    // GNU libc 2.1.3 will segfault in towupper() if environment variables don't
    // specify a locale.  This can happen when Apache launches CGI subprocesses.
    // Solution: set a locale if the environment variable is missing.
    // Works in 2.1.2, fixed in glibc 2.1.91 and later
    setlocale(LC_ALL, 'POSIX');
  end
  else
    // Configure the process locale settings according to
    // the system environment variables (LC_CTYPE, LC_COLLATE, etc)
    setlocale(LC_ALL, '');

  // Note:
  // POSIX/C is the default locale on many Unix systems, but its 7-bit charset
  // causes char to widechar conversions to fail on any high-ascii
  // character.  To support high-ascii charset conversions, set the
  // LC_CTYPE environment variable to something else or call setlocale to set
  // the LC_CTYPE information for this process.  It doesn't matter what
  // you set it to, as long as it's not POSIX.
  if StrComp(nl_langinfo(_NL_CTYPE_CODESET_NAME), 'ANSI_X3.4-1968') = 0 then
    setlocale(LC_CTYPE, 'en_US');  // selects codepage ISO-8859-1
end;

procedure PropagateSignals;
var
  Exc: TObject;
begin
  {
    If there is a current exception pending, then we're shutting down because
    it went unhandled.  If that exception is the result of a signal, then we
    need to propagate that back out to the world as a real signal death.  See
    the discussion at http://www2.cons.org/cracauer/sigint.html for more info.
  }
  Exc := ExceptObject;
  if (Exc <> nil) and (Exc is EExternal) then
     kill(getpid, EExternal(Exc).SignalNumber);
end;

{
    Under Win32, SafeCallError is implemented in ComObj.  Under Linux, we
    don't have ComObj, so we've substituted a similar mechanism here.
}
procedure SafeCallError(ErrorCode: Integer; ErrorAddr: Pointer);
var
  ExcMsg: String;
begin
  ExcMsg := GetSafeCallExceptionMsg;
  SetSafeCallExceptionMsg('');
  if ExcMsg <> '' then
  begin
    raise ESafeCallException.Create(ExcMsg) at GetSafeCallExceptionAddr;
  end
  else
    raise ESafeCallException.CreateRes(@SSafecallException);
end;
{$ENDIF}

initialization
{$IFNDEF NoInitialization}
  if ModuleIsCpp then HexDisplayPrefix := '0x';
  InitExceptions;

{$IFDEF LINUX}
  SafeCallErrorProc := @SafeCallError;
  ExitProcessProc := PropagateSignals;

  CheckLocale;
{$ENDIF}

{$IFDEF MSWINDOWS}
  InitPlatformId;
  InitDriveSpacePtr;
{$ENDIF}
  GetFormatSettings; { Win implementation uses platform id }
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
{$IFDEF MSWINDOWS}
  FreeAndNil(FLanguages);
{$ENDIF}
{$IFDEF LINUX}
  if libuuidHandle <> nil then
    dlclose(libuuidHandle);
{$ENDIF}
  FreeTerminateProcs;
  DoneExceptions;
{$ENDIF NoFinalization}

end.
