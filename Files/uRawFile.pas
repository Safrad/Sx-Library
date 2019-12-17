{ Example of use:
var
	F: TRawFile;
begin
	F := TRawFile.Create;
try
  F.FileName := DataDir + 'FileName.dat';
  F.FileMode := ffmReadOnly;
  F.Open;

	F.BlockRead(...);

	F.Close; // Optional
	F.Free;
end; }
unit uRawFile;

{$ifdef MSWINDOWS}
  {$DEFINE UseWINAPI}
{$endif}

{$ifndef UseWINAPI}
  {$IOCHECKS ON}
{$endif}

interface

uses
  SysUtils,

  uTypes,
  uFileStatistics,
  uDateTimeLogger;

const
	{
		Buffer Size, Performance
		1..16 * KB, low
		32 * KB, acceptable
		>= 64 * KB optimal
		16 * MB, Win XP x64 API maximum
		32 * MB, Win XP API maximum
	}

	DefFileBuffer = 256 * KB;
  // Flags
  FILE_FLAG_WRITE_THROUGH = $80000000; // For write only
  FILE_FLAG_OVERLAPPED = $40000000;
  FILE_FLAG_NO_BUFFERING = $20000000; // Be carefully for use this
  FILE_FLAG_RANDOM_ACCESS = $10000000;
  FILE_FLAG_SEQUENTIAL_SCAN = $8000000; // Default value

  FILE_FLAG_DELETE_ON_CLOSE = $4000000;
  FILE_FLAG_BACKUP_SEMANTICS = $2000000;
  FILE_FLAG_POSIX_SEMANTICS = $1000000;
  FILE_FLAG_OPEN_NO_RECALL = $00100000;
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;

type
	TFileMode = (fmReadOnly, fmRewrite, fmAppend, fmReadAndWrite);

var
	FileModeStr: array [TFileMode] of string;

type
	TRawFile = class(TObject)
	private
		FDeleteAfterClose: BG;
{$ifdef UseWINAPI}
		FHandle: THandle;
{$else}
    FFile: File;
    FOpened: BG;
{$endif}
		FFileName: TFileName;
    FFileSize: U8;

		FFileMode: TFileMode;
    FRandomAccess: BG;

    // Buffer start address and block sizes have to be aligned to sector size
    FUseBuffer: BG;

    // For write only, do not use buffer, only disk cache
    FWriteThrough: BG;

		FFilePos: U8;
    FLogger: TDateTimeLogger;
    FChangeDate: BG;
{$ifdef UseWINAPI}
    function GetFlags: U4;
{$endif}
		function GetOpened: BG;
    procedure SetLogger(const Value: TDateTimeLogger);
    procedure SetChangeDate(const Value: BG);
    procedure SetFileName(const Value: TFileName);
    procedure SetRandomAccess(const Value: BG);
    procedure SetUseBuffer(const Value: BG);
    procedure SetWriteThrough(const Value: BG);
  protected
    procedure SetFileMode(const Value: TFileMode); virtual;
    procedure MustBeClosed(const APropertyName: string);
	public
		constructor Create;
		destructor Destroy; override;

    // Input
		property DeleteAfterClose: BG read FDeleteAfterClose write FDeleteAfterClose;
		property FileName: TFileName read FFileName write SetFileName;
    property FileMode: TFileMode read FFileMode write SetFileMode;
    property RandomAccess: BG read FRandomAccess write SetRandomAccess;
    property UseBuffer: BG read FUseBuffer write SetUseBuffer;
    property WriteThrough: BG read FWriteThrough write SetWriteThrough;
    property Logger: TDateTimeLogger read FLogger write SetLogger;
    property ChangeDate: BG read FChangeDate write SetChangeDate;

    // Process
		procedure Open; virtual;
		procedure Close; virtual;

		procedure Seek(const APosition: U8); virtual;
		procedure SeekBegin;
		procedure SeekEnd;
		procedure BlockRead(const AData: Pointer; const ASize: UG); overload; virtual;
		procedure BlockWrite(const AData: Pointer; const ASize: UG); overload; virtual;
		procedure BlockRead(out AData; const ASize: UG); overload;
		procedure BlockWrite(const AData; const ASize: UG); overload;
		procedure FillWrite(const ACount: UG);
		procedure Truncate;
		procedure FlushFileBuffers; virtual;
		function Eof: BG; virtual;
{$ifdef UseWINAPI}
		procedure Lock(From, Count: U8);
		procedure UnLock(From, Count: U8);
{$endif}

    // Result
{$ifdef UseWINAPI}
		property Handle: THandle read FHandle;
{$endif}
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read GetOpened;
	end;

implementation

uses
	Math,
{$ifdef MSWINDOWS}
  Winapi.Windows,
{$endif}

  uStrings,
  uEIOException,
	uFiles,
  uOutputFormat;

{ TRawFile }

constructor TRawFile.Create;
begin
	inherited Create;

{$ifdef UseWINAPI}
	FHandle := INVALID_HANDLE_VALUE;
{$endif}
//  FLogger := MainLog;
  FFileMode := fmReadOnly;
  FUseBuffer := True;
end;

destructor TRawFile.Destroy;
begin
  try
    if GetOpened then
    begin
      if Logger.IsLoggerFor(mlWarning) then
        Logger.Add(ReplaceParam('Forcing close of file %1.', [FFileName]), mlWarning);
      Close;
    end;
  finally
  	inherited Destroy;
  end;
end;

function TRawFile.GetOpened: BG;
begin
{$ifdef UseWINAPI}
	Result := FHandle <> INVALID_HANDLE_VALUE;
{$else}
  Result := FOpened;
{$endif}
end;

procedure TRawFile.Open;
var
	CreationDistribution: U4;
	DesiredAccess, ShareMode: U4;
begin
	if GetOpened then
	begin
    raise EArgumentException.Create(ReplaceParam('Can not reopen file %1', [FFileName]));
	end;

	FFileName := FileName;
	FFilePos := 0;

	if FLogger.IsLoggerFor(mlDebug) then
    FLogger.Add('Opening for ' + FileModeStr[FFileMode] + ' ' + FFileName, mlDebug);

{$ifdef UseWINAPI}
	ShareMode := FILE_SHARE_READ;
	case FFileMode of
	fmReadOnly:
		begin
			DesiredAccess := GENERIC_READ;
			ShareMode := ShareMode or FILE_SHARE_WRITE;
		end;
	fmRewrite, fmAppend:
		begin
			DesiredAccess := GENERIC_WRITE;
		end;
	fmReadAndWrite:
		begin
			DesiredAccess := GENERIC_READ or GENERIC_WRITE;
		end
	else
	begin
		DesiredAccess := GENERIC_ALL;
	end;
	end;

	if (FileExists(FFileName) = False) and (FFileMode <> fmReadOnly) then
		CreationDistribution := CREATE_NEW
	else
		CreationDistribution := OPEN_EXISTING;

	FHandle := CreateFile(PChar(FFileName), // pointer to name of the file
		DesiredAccess, // access (read-write) mode
		ShareMode, // share mode
		nil, // pointer to security attributes
		CreationDistribution, // how to create
		FILE_ATTRIBUTE_NORMAL or GetFlags, // file attributes
		0 // handle to file with attributes to copy
		);
	if not GetOpened then
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
	FFileSize := HandleFileSize(FHandle, FFileName);
  if FFileMode = fmAppend then
    SeekEnd;
{$else}
  FOpened := False;
  AssignFile(FFile, FFileName);
  case FFileMode of
  fmReadOnly:
    System.Reset(FFile, 1);
  fmRewrite:
    System.Rewrite(FFile, 1);
  fmAppend:
  begin
    if FileExists(FFileName) then
      System.Reset(FFile, 1)
    else
      System.Rewrite(FFile, 1);
  end;
  fmReadAndWrite:
    System.Reset(FFile, 1);
  end;

  FFileSize := System.FileSize(FFile);

  if FFileMode = fmAppend then
    System.Seek(FFile, FFileSize);

  FOpened := True;
{$endif}
end;

{$ifdef UseWINAPI}
function TRawFile.GetFlags: U4;
begin
  if FRandomAccess then
    Result := FILE_FLAG_RANDOM_ACCESS
  else
    Result := FILE_FLAG_SEQUENTIAL_SCAN;

  if not FUseBuffer then
    Result := Result or FILE_FLAG_NO_BUFFERING;

  if not FWriteThrough then
    Result := Result or FILE_FLAG_WRITE_THROUGH;
end;
{$endif}

procedure TRawFile.Seek(const APosition: U8);
begin
{$ifdef UseWINAPI}
	if SetFilePointer(FHandle, // handle of file
		TU8(APosition).D0, // number of bytes to move file pointer
		@TU8(APosition).D1, // address of high-order word of distance to move
		FILE_BEGIN // how to move
		) <> $FFFFFFFF then
	begin
		FFilePos := APosition;
	end
	else
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
{$else}
  System.Seek(FFile, APosition);
{$endif}
end;

procedure TRawFile.SeekBegin;
begin
	Seek(0);
end;

procedure TRawFile.SeekEnd;
begin
	Seek(FFileSize);
end;

procedure TRawFile.BlockRead(const AData: Pointer; const ASize: UG);
var
	Suc: U4;
begin
{$ifdef UseWINAPI}
	if ReadFile(FHandle, AData^, ASize, Suc, nil) then
	begin
		Inc(FFilePos, Suc);
		FileStatistics.AddRead(Suc);

		if Suc <> ASize then
		begin
      raise Exception.Create(
        'Reading only ' + BToStr(Suc, ofIO) + '/' + BToStr(ASize, ofIO) + ' from ' + FFileName);
		end
		else
			if FLogger.IsLoggerFor(mlDebug) then
        FLogger.Add('Reading ' + BToStr(Suc, ofIO) + ' from ' + FFileName, mlDebug);
	end
	else
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
{$else}
  System.BlockRead(FFile, AData^, ASize, Suc);
{$endif}
end;

procedure TRawFile.BlockWrite(const AData: Pointer; const ASize: UG);
var
	Suc: U4;
begin
{$ifdef UseWINAPI}
  Suc := 0;
	if WriteFile(FHandle, AData^, ASize, Suc, nil) then
	begin
		Inc(FFilePos, Suc);
		FileStatistics.AddWrite(Suc);

    if Suc <> ASize then
    begin
      raise Exception.Create(
        'Writing only ' + BToStr(Suc, ofIO) + '/' + BToStr(ASize, ofIO) + ' to ' + FFileName);
    end
    else
      if FLogger.IsLoggerFor(mlDebug) then
        FLogger.Add('Writing ' + BToStr(Suc, ofIO) + ' to ' + FFileName, mlDebug);
	end
	else
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
{$else}
  System.BlockWrite(FFile, AData^, ASize, Suc);
{$endif}
end;

procedure TRawFile.BlockRead(out AData; const ASize: UG);
begin
  BlockRead(Addr(AData), ASize);
end;

procedure TRawFile.BlockWrite(const AData; const ASize: UG);
begin
  BlockWrite(Addr(AData), ASize);
end;

procedure TRawFile.FillWrite(const ACount: UG);
var
	Buf: Pointer;
	BufferSize: UG;
  Remain: UG;
begin
	BufferSize := Min(ACount, DefFileBuffer);
	Buf := AllocMem(BufferSize);
	try
    Remain := ACount;
		while Remain > 0 do
		begin
			BlockWrite(Buf, BufferSize);
			Dec(Remain, BufferSize);
		end;
	finally
		FreeMem(Buf);
	end;
end;

procedure TRawFile.Close;
{$ifdef UseWINAPI}
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
{$endif}
begin
	if not GetOpened then
	begin
		raise Exception.Create(ReplaceParam('Cannot again close file %1.', [FFileName]));
	end;
	if FLogger.IsLoggerFor(mlDebug) then
    FLogger.Add('Closing ' + FFileName, mlDebug);

	if FChangeDate then
		if FFileMode <> fmReadOnly then
		begin
      {$ifdef UseWINAPI}
			if GetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime) then
			begin
				GetSystemTimeAsFileTime(LastWriteTime);
				SetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
			end;
      {$else}
      SetFileModified(FFileName, GetFileModified(FFileName));
      {$endif}
		end;

  {$ifdef UseWINAPI}
	if CloseHandle(FHandle) then
	begin
		if FDeleteAfterClose then
			DeleteFileEx(FFileName);
	end
	else
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
	FHandle := INVALID_HANDLE_VALUE;
  {$else}
  CloseFile(FFile);
  FOpened := False;
  {$endif}
end;

procedure TRawFile.Truncate;
begin
  {$ifdef UseWINAPI}
	if not SetEndOfFile(FHandle) then
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
  {$else}
  System.Truncate(FFile);
  {$endif}
end;

procedure TRawFile.FlushFileBuffers;
begin
  {$ifdef UseWINAPI}
	if not Winapi.Windows.FlushFileBuffers(FHandle) then
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
  {$else}
//  Winapi.Windows.FlushFileBuffers(TFileRec(FFile).Handle);
  {$endif}
end;

function TRawFile.Eof: BG;
begin
	Result := FFilePos >= FFileSize;
end;

procedure TRawFile.MustBeClosed(const APropertyName: string);
begin
  if GetOpened then
    raise EArgumentException.Create('Property ' + APropertyName + 'can not be set if file is opened.');
end;

{$ifdef UseWINAPI}
procedure TRawFile.Lock(From, Count: U8);
begin
	if not LockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1) then
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
end;

procedure TRawFile.UnLock(From, Count: U8);
begin
	if not UnLockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1) then
	begin
    raise EIOException.Create(FFileName, GetLastError);
	end;
end;
{$endif}

procedure TRawFile.SetChangeDate(const Value: BG);
begin
  FChangeDate := Value;
end;

procedure TRawFile.SetFileMode(const Value: TFileMode);
begin
	MustBeClosed('FileMode');
  FFileMode := Value;
end;

procedure TRawFile.SetFileName(const Value: TFileName);
begin
	MustBeClosed('FileName');
  FFileName := Value;
end;

procedure TRawFile.SetLogger(const Value: TDateTimeLogger);
begin
  FLogger := Value;
end;

procedure TRawFile.SetRandomAccess(const Value: BG);
begin
	MustBeClosed('RandomAccess');
  FRandomAccess := Value;
end;

procedure TRawFile.SetUseBuffer(const Value: BG);
begin
	MustBeClosed('UseBuffer');
  FUseBuffer := Value;
end;

procedure TRawFile.SetWriteThrough(const Value: BG);
begin
	MustBeClosed('WriteThrough');
  FWriteThrough := Value;
end;

initialization
{$IFNDEF NoInitialization}
	EnumToStr(TypeInfo(TFileMode), FileModeStr);
{$ENDIF NoInitialization}
end.

