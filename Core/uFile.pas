{
	Result of Functions : False is Error, True is Ok
	Example:
	var
	F: TFile;
	begin
	F := TFile.Create;
	try
	// F.Charset := fcUTF8;
	if F.Open(WorkDir + 'FileName.txt', fmReadOnly) then
	begin
	F.BlockRead();
	F.Readln();
	F.Close
	end;
	finally
	F.Free;
	end;
	end;
}

unit uFile;

{$ZEROBASEDSTRINGS OFF}

interface

uses
	SysUtils,
  WinApi.Windows,

	uTypes, uStrings, uChar, uLogger, uDateTimeLogger, uFileCharset,
  uBackup;

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

	// For write FMode only, if enabled temporary file is used first
type
	TFileMode = (fmReadOnly, fmRewrite, fmAppend, fmReadAndWrite);

var
	FileModeStr: array [TFileMode] of string;

const
  // Flags
  FILE_FLAG_WRITE_THROUGH = DWORD($80000000); // For write only
  FILE_FLAG_OVERLAPPED = $40000000;
  FILE_FLAG_NO_BUFFERING = $20000000; // Be carefully for use this
  FILE_FLAG_RANDOM_ACCESS = $10000000;
  FILE_FLAG_SEQUENTIAL_SCAN = $8000000; // Default value

  FILE_FLAG_DELETE_ON_CLOSE = $4000000;
  FILE_FLAG_BACKUP_SEMANTICS = $2000000;
  FILE_FLAG_POSIX_SEMANTICS = $1000000;
  FILE_FLAG_OPEN_NO_RECALL = $00100000;
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;

	FILE_FLAG_NO_PREFIX = $8;

type
	TFile = class(TObject)
	private
		FCharset: TFileCharset;
		FProtection: BG;
		FDefaultCharset: TFileCharset; // Used if BOM not present.
		FDeleteAfterClose: BG;
		FHandle: THandle;
		FFileName: TFileName;
		FTempFileName: TFileName;

		FMode: TFileMode;
    FBackupFolder: TBackupFolder;
    FSkipSameData: BG;

		// For Read(ln), Write(ln) only
		FBuffer: array of AnsiChar;
		FBufStart, FBufEnd: U8;
		FBufferSize: U8;
		FNeedSaveBuffer: BG;

		FFileBegin: S8; // Size of Byte Order Mark
		FFilePos: U8;
		FFileSize: U8;
    FLogger: TDateTimeLogger;
		function IsOpened: BG;
		function ErrorRetry(const ErrorCode: U4): BG;
		procedure CreateBuffer;
		procedure FillBuffer;
		procedure DestroyBuffer;
		function SaveBuffer: BG;
		procedure SetProtection(const Value: BG);
		procedure ReadPrefix;
		procedure WritePrefix;
    function UpdateBuffer: SG;
    procedure SetLogger(const Value: TDateTimeLogger);
	public
		property Charset: TFileCharset read FCharset write FCharset;
		property DefaultCharset: TFileCharset read FDefaultCharset write FDefaultCharset;
		property Protection: BG read FProtection write SetProtection;
		property SkipSameData: BG read FSkipSameData write FSkipSameData;
		property DeleteAfterClose: BG read FDeleteAfterClose write FDeleteAfterClose;
		property Handle: THandle read FHandle;
		property FileName: TFileName read FFileName;
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read IsOpened;
    property BackupFolder: TBackupFolder read FBackupFolder write FBackupFolder;
		constructor Create;
		destructor Destroy; override;
		function Open(const FileName: TFileName; const Mode: TFileMode;
			Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG; overload;
		function Seek(const Pos: U8): BG;
		function SeekBegin: BG;
		function SeekEnd: BG;
		function BlockRead(out Buf; const Count: UG): BG;
		function BlockWrite(const Buf; const Count: UG): BG;
		function FillWrite(Count: UG): BG;
		function ReadlnNoConversion(out Line: AnsiString): BG;
		function Readln(out Line: AnsiString): BG; overload;
		function Readln(out Line: UnicodeString): BG; overload;
		function WriteNoConversion(const Line: PAnsiChar; const LineLength: SG): BG; overload;
		function WriteNoConversion(const Line: RawByteString): BG; overload;
		function Write(const Line: AnsiString): BG; overload;
		function Write(const Line: UnicodeString): BG; overload;
		function Writeln(const Line: AnsiString): BG; overload;
		function Writeln(const Line: UnicodeString): BG; overload;
		function Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
		function Truncate: BG;
		function FlushFileBuffers: BG;
		function Eof: BG;
		function Lock(From, Count: U8): BG;
		function UnLock(From, Count: U8): BG;
    property Logger: TDateTimeLogger read FLogger write SetLogger;
	end;

implementation

uses
	Math,

  uFileStatistics,
  uTemporaryDirectory,
  uFiles,
	uOutputFormat, uCharset, uMainLog, uMsg;

constructor TFile.Create;
begin
	inherited Create;
	FHandle := INVALID_HANDLE_VALUE;
	FProtection := True;
  FSkipSameData := True;
  FBackupFolder := bfNone;
	FDefaultCharset := fcAnsi;
	FCharset := DefaultFileCharset;
  FLogger := MainLog;
end;

destructor TFile.Destroy;
begin
	if IsOpened then
	begin
		Warning('Forcing close of file %1.', [FTempFileName]);
		Close(True, True);
	end;
	inherited Destroy;
end;

function TFile.ErrorRetry(const ErrorCode: U4): BG;
begin
	Result := IOErrorRetry(FTempFileName, ErrorCode);
end;

function TFile.IsOpened: BG;
begin
	Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function TFile.Open(const FileName: TFileName; const Mode: TFileMode;
	Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
label LRetry;
var
	CreationDistribution: U4;
	DesiredAccess, ShareMode: U4;
	ErrorCode: U4;
begin
	Result := False;

	if IsOpened then
	begin
		Warning('Forcing close file %1.', [FileName]);
		Close;
	end;

	FFileName := FileName;
	FFilePos := 0;
	FMode := Mode;
	CreateBuffer;

	if FProtection and (FMode in [fmRewrite, fmReadAndWrite]) then
	begin
		FTempFileName := TemporaryDirectory.ThreadTempDir + '~' + ExtractFileName(FileName);
		if FileExists(FTempFileName) then
		begin
			DeleteFileEx(FTempFileName);
		end;
		if FMode = fmReadAndWrite then
			if FileExists(FileName) then
				CopyFile(FileName, FTempFileName, False);
	end
	else
		FTempFileName := ExpandDir(FileName);
	if FLogger.IsLoggerFor(mlDebug) then
    FLogger.Add('Opening for ' + FileModeStr[Mode] + ' ' + FTempFileName, mlDebug);

LRetry :
	ShareMode := FILE_SHARE_READ;
	case Mode of
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

	if (FileExists(FTempFileName) = False) and (FMode <> fmReadOnly) then
		CreationDistribution := CREATE_NEW
	else
		CreationDistribution := OPEN_EXISTING;

	Flags := Flags and (not FILE_FLAG_OVERLAPPED);
	FHandle := CreateFile(PChar(FTempFileName), // pointer to name of the file
		DesiredAccess, // access (read-write) mode
		ShareMode, // share mode
		nil, // pointer to security attributes
		CreationDistribution, // how to create
		FILE_ATTRIBUTE_NORMAL or Flags, // file attributes
		0 // handle to file with attributes to copy
		);
	if not IsOpened then
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
				goto LRetry;
			Exit;
		end;
	end;
	FFileSize := HandleFileSize(FHandle, FTempFileName);
	FFileBegin := 0;
	case Mode of
	fmReadOnly, fmReadAndWrite:
		begin
			if ((Flags and FILE_FLAG_NO_BUFFERING) = 0) and ((Flags and FILE_FLAG_NO_PREFIX) = 0) then
				ReadPrefix;
		end;
	fmAppend:
		begin
			if FFileSize > 0 then
			begin
				// ReadPrefix;
				SeekEnd;
			end
			else if (Flags and FILE_FLAG_NO_PREFIX) = 0 then
				WritePrefix;
		end;
	fmRewrite:
		begin
			Truncate;
			if (Flags and FILE_FLAG_NO_PREFIX) = 0 then
				WritePrefix;
		end;
	end;

	Result := True;
end;

function TFile.Seek(const Pos: U8): BG;
label LRetry;
var
	ErrorCode: U4;
	Pos2: U8;
begin
LRetry :
	Result := False;
	Pos2 := Pos + U8(FFileBegin);
	if SetFilePointer(FHandle, // handle of file
		TU8(Pos2).D0, // number of bytes to move file pointer
		@TU8(Pos2).D1, // address of high-order word of distance to move
		FILE_BEGIN // how to move
		) <> $FFFFFFFF then
	begin
		Result := True;
		FFilePos := Pos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
				goto LRetry;
		end
		else
			Result := True;
	end;
end;

function TFile.SeekBegin: BG;
begin
	Result := Seek(0);
end;

function TFile.SeekEnd: BG;
begin
	Result := Seek(FFileSize);
end;

function TFile.BlockRead(out Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
LRetry :
	if ReadFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;
		FileStatistics.AddRead(Suc);

		if Suc <> Count then
		begin
			Warning('Reading only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO)
					+ ' from ' + FTempFileName);
			Result := False;
		end
		else
			if FLogger.IsLoggerFor(mlDebug) then
        FLogger.Add('Reading ' + BToStr(Suc, ofIO) + ' from ' + FTempFileName, mlDebug);

		Inc(FFilePos, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
			begin
				Seek(FFilePos);
				goto LRetry;
			end;
			Result := False;
		end
		else
			Result := True;
		Inc(FFilePos, Suc);
		Seek(FFilePos);
	end;
end;

function TFile.BlockWrite(const Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
LRetry :
	if WriteFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;
		FileStatistics.AddWrite(Suc);

    if Suc <> Count then
    begin
      if FLogger.IsLoggerFor(mlWarning) then
        FLogger.Add('Writing only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO)
          + ' to ' + FTempFileName, mlWarning);
    end
    else
      if FLogger.IsLoggerFor(mlDebug) then
        FLogger.Add('Writing ' + BToStr(Suc, ofIO) + ' to ' + FTempFileName, mlDebug);

		Inc(FFilePos, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
			begin
				Seek(FFilePos);
				goto LRetry;
			end;
			Result := False;
		end
		else
			Result := True;
		Inc(FFilePos, Suc);
		Seek(FFilePos);
	end;
  FFileSize := Max(FFileSize, FFilePos);
end;

function TFile.FillWrite(Count: UG): BG;
var
	Buf: Pointer;
	C: UG;
begin
	C := Min(Count, DefFileBuffer);
	Buf := AllocMem(C);
	try
		Result := True;
		while Count > 0 do
		begin
			Result := Result and BlockWrite(Buf^, C);
			Count := Count - C;
			if C > Count then
				C := Count;
			if Result = False then
				Break;
		end;
	finally
		FreeMem(Buf);
	end;
end;

function TFile.UpdateBuffer: SG;
begin
  if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
  begin
    Result := FFilePos - FBufStart;
  end
  else
  begin
    FBufStart := FBufferSize * (FFilePos div FBufferSize);
    FBufEnd := FBufStart + FBufferSize - 1;
    if FBufEnd >= FFileSize then
      FBufEnd := FFileSize - 1;
    Seek(FBufStart);
    Result := FFilePos;
    BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
    FFilePos := Result;
    Result := FFilePos - FBufStart;
  end;
end;

{
	File   |********************************|
	Buffer                 |*******|
}
function TFile.ReadlnNoConversion(out Line: AnsiString): BG;
var
	BufPos: SG;
	InLineIndex: SG;
	LineLength: SG;
  CharSize: SG;
  LastCharCR: BG;
begin
	Line := '';
	if Eof then
	begin
		Result := False;
		Exit;
	end;
	Result := True;
	FillBuffer;

  CharSize := CharsetSize[FCharset];

	LineLength := 256;
	SetLength(Line, LineLength);
	InLineIndex := 1;
	while not Eof do
	begin
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			BufPos := FFilePos - FBufStart;
(*      if (FBuffer[BufPos] = CharCR) then
      begin
				if (FCharset <> fcUTF16LE) or ((BufPos and 1 = 0) and (FBuffer[BufPos + 1] = #0)) then
				begin
					if FCharset = fcUTF16LE then
					begin
//						Dec(InLineIndex);
						Inc(FFilePos);
					end;
					Inc(FFilePos);
					// Seek(FFilePos);
					Break;
        end;
      end
			else*)
      case FCharset of
      fcAnsi, fcUTF8:
        if (FBuffer[BufPos] = CharCR) or (FBuffer[BufPos] = CharLF) then
        begin
          LastCharCR := FBuffer[BufPos] = CharCR;
          Inc(FFilePos);
          BufPos := UpdateBuffer;
          if (LastCharCR) and (FBuffer[BufPos] = CharLF) then
            Inc(FFilePos);
          Break;
        end;
      fcUTF16BE:
        if (FBuffer[BufPos + 1] = CharCR) or (FBuffer[BufPos + 1] = CharLF) then
        begin
          if (FBuffer[BufPos] = #0) then
          begin
            LastCharCR := FBuffer[BufPos + 1] = CharCR;
//            Dec(InLineIndex, CharSize);
            Inc(FFilePos, 2);
            BufPos := UpdateBuffer;
            if LastCharCR then
            if (FBuffer[BufPos + 1] = CharLF) then
            begin
              if (FBuffer[BufPos] = #0) then
              begin
                Inc(FFilePos, 2); // Skip LF after CR
              end;
            end;

            Break;
          end;
        end;
      fcUTF16LE:
        if (FBuffer[BufPos] = CharCR) or (FBuffer[BufPos] = CharLF) then
        begin
          if (FBuffer[BufPos + 1] = #0) then
          begin
            LastCharCR := FBuffer[BufPos] = CharCR;
//            Dec(InLineIndex, CharSize);
            Inc(FFilePos, 2);
            BufPos := UpdateBuffer;
            if LastCharCR then
            if (FBuffer[BufPos] = CharLF) then
            begin
              if (FBuffer[BufPos + 1] = #0) then
              begin
                Inc(FFilePos, 2); // Skip LF after CR
              end;
            end;
            Break;
          end;
        end;
      else
        if (FBuffer[BufPos] = CharLF) then
        begin
          Inc(FFilePos);
          Break;
        end;
      end;
		end
		else
		begin
			SaveBuffer;
			FBufStart := FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1;
			if FBufEnd >= FFileSize then
				FBufEnd := FFileSize - 1;
			Seek(FBufStart);
			BufPos := FFilePos;
			BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
			FFilePos := BufPos;
			// BufPos := FFilePos - FBufStart;
			Continue;
		end;
{		if (FBuffer[BufPos] <> CharCR) or
			((FCharset = fcUTF16LE) and ((BufPos and 1 <> 0) or (FBuffer[BufPos + 1] <> #0))) then
		begin}
			if InLineIndex > LineLength then
			begin
				LineLength := 2 * (InLineIndex - 1);
				SetLength(Line, LineLength);
			end;
			Line[InLineIndex] := FBuffer[BufPos];
      if CharSize > 1 then
  			Line[InLineIndex + 1] := FBuffer[BufPos + 1];

			Inc(InLineIndex, CharSize);
//		end;
		Inc(FFilePos, CharSize);
	end;
	SetLength(Line, InLineIndex - 1);
end;

function TFile.Readln(out Line: AnsiString): BG;
begin
	Result := ReadLnNoConversion(Line);
	case FCharset of
	fcAnsi: ;
	fcUTF8: Line := AnsiString(Utf8ToAnsi(Line));
	else
		Line := '';
		Warning('Unsupported charset in file %1', [FFileName]);
	end;
end;

function TFile.Readln(out Line: UnicodeString): BG;
var
	LineA: AnsiString;
  i: SG;
begin
	Result := ReadlnNoConversion(LineA);
	case FCharset of
	fcAnsi:
		Line := UnicodeString(LineA);
	fcUTF8:
		begin
			Line := ConvertUtf8ToUnicode(LineA);
		end;
	fcUTF16BE:
		begin
			SetLength(Line, Length(LineA) div SizeOf(WideChar));
			Move(LineA[1], Line[1], Length(LineA));
      for i := 1 to Length(Line) do
      begin
        Line[i] := WideChar(Swap(Ord(Line[i])));
      end;
{      for i := 1 to Length(Line) do
        Line[i] := WideChar(Ord(LineA[2 * i - 1]) + Ord(LineA[2 * i]) shl 8);}
		end;
  fcUTF16LE:
		begin
			SetLength(Line, Length(LineA) div SizeOf(WideChar));
			Move(LineA[1], Line[1], Length(LineA));
		end;
	else
		begin
			Line := '';
			Warning('Unsupported charset in file %1', [FFileName]);
		end;
	end;
end;

procedure TFile.ReadPrefix;
var
	ByteOrderMark: TByteOrderMark;
  FileExt: string;
begin
  if FFileSize > 0 then
  begin
    SetLength(ByteOrderMark, Min(FFileSize, MaxByteOrderMarkSize));
    BlockRead(ByteOrderMark[0], Length(ByteOrdermark));

    FCharset := FindFileCharset(ByteOrderMark);
    if FCharset <> fcUnknown then
    begin
      FFilePos := 0;
      FFileBegin := Length(ByteOrderMarks[FCharset]);
      Dec(FFileSize, FFileBegin);
    end
    else
    begin
      FileExt := UpperCase(ExtractFileExt(FFileName));
      if (FileExt = '.XML') or (FileExt = '.DPROJ') then
        FCharset := fcUTF8
      else
        FCharset := FDefaultCharset;
    end;

    SeekBegin;
  end
  else
    FCharset := FDefaultCharset;
end;

function TFile.WriteNoConversion(const Line: PAnsiChar; const LineLength: SG): BG;
var
	InLineIndex: SG;
begin
	Result := False;

	FillBuffer;

	InLineIndex := 0;
	while InLineIndex < LineLength do
	begin
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			FBuffer[FFilePos - FBufStart] := Line[InLineIndex];
			FNeedSaveBuffer := True;
			Inc(FFilePos);
			Inc(InLineIndex);
			FFileSize := Max(FFileSize, FFilePos);
		end
		else
		begin
			// Save previous buffer.
			if not SaveBuffer then
				Exit;
			// Create new buffer.
			FBufStart := FFilePos; // FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1; // , LineLength - InLineIndex);
		end;
	end;
end;

function TFile.WriteNoConversion(const Line: RawByteString): BG;
(*var
	InLineIndex, LineLength: SG;
begin
	Result := False;

	FillBuffer;

	InLineIndex := 1;
	LineLength := Length(Line);
	while InLineIndex <= LineLength do
	begin
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			FBuffer[FFilePos - FBufStart] := Line[InLineIndex];
			FNeedSaveBuffer := True;
			Inc(FFilePos);
			Inc(InLineIndex);
			FFileSize := Max(FFileSize, FFilePos);
		end
		else
		begin
			// Save previous buffer.
			if not SaveBuffer then
				Exit;
			// Create new buffer.
			FBufStart := FFilePos; // FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1; // , LineLength - InLineIndex);
		end;
	end;*)
begin
	Result := WriteNoConversion(PAnsiChar(Line), Length(Line));
end;

function TFile.Write(const Line: AnsiString): BG;
var
	u: UnicodeString;
  i: SG;
begin
	if Length(Line) = 0 then
	begin
		Result := True;
		Exit;
	end;

	case FCharset of
	fcUTF8:
		begin
			Result := WriteNoConversion(AnsiToUTF8(string(Line)));
		end;
	 fcUTF16BE:
		begin
			u := UnicodeString(Line);
      for i := 1 to Length(u) do
      begin
        u[i] := WideChar(Swap(Ord(u[i])));
      end;
			Result := WriteNoConversion(PAnsiChar(@u), Length(Line) * SizeOf(WideChar));
		end;
   fcUTF16LE:
		begin
			u := UnicodeString(Line);
			Result := WriteNoConversion(PAnsiChar(@u), Length(Line) * SizeOf(WideChar));
		end;
	fcAnsi:
		begin
			Result := WriteNoConversion(Line);
		end;
	else
		Result := False;
		Warning('Unsupported charset in file %1', [FFileName]);
	end;
end;

function TFile.Write(const Line: UnicodeString): BG;
var
  u: UnicodeString;
  i: SG;
begin
	if Length(Line) = 0 then
	begin
		Result := True;
		Exit;
	end;

	case FCharset of
	fcUTF8:
		begin
			Result := WriteNoConversion(AnsiString(ConvertUnicodeToUTF8(Line)));
		end;
	 fcUTF16BE:
		begin
      u := Line;
      for i := 1 to Length(u) do
      begin
        u[i] := WideChar(Swap(Ord(u[i])));
      end;
			Result := WriteNoConversion(PAnsiChar(@u[1]), Length(u) * SizeOf(WideChar));
		end;
   fcUTF16LE:
		begin
			Result := WriteNoConversion(PAnsiChar(@Line[1]), Length(Line) * SizeOf(WideChar));
		end;
	fcAnsi:
		begin
			Result := WriteNoConversion(AnsiString(Line));
		end;
	else
		Result := False;
		Warning('Unsupported charset in file %1', [FFileName]);
	end;
end;

function TFile.Writeln(const Line: AnsiString): BG;
begin
	Result := Write(Line + FileSep);
end;

function TFile.Writeln(const Line: UnicodeString): BG;
begin
	Result := Write(Line + FileSep);
end;

procedure TFile.WritePrefix;
var
	L: SG;
begin
	L := Length(ByteOrderMarks[FCharset]);
	if L > 0 then
		BlockWrite(ByteOrderMarks[FCharset][0], L);
end;

function TFile.Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
label LRetry;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	ErrorCode: U4;
  OriginalFileExists: BG;
begin
LRetry :
	Result := False;
	if not IsOpened then
	begin
		Warning('Cannot again close file %1.', [FTempFileName]);
		Exit;
	end;
	if FLogger.IsLoggerFor(mlDebug) then
    FLogger.Add('Closing ' + FTempFileName, mlDebug);

	DestroyBuffer;
	if ChangeDate then
		if FMode <> fmReadOnly then
		begin
			if GetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime) then
			begin
				GetSystemTimeAsFileTime(LastWriteTime);
				SetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
			end;
		end;

	if CloseHandle(FHandle) then
	begin
		if FProtection and (FMode in [fmRewrite, fmReadAndWrite]) and (Forced = False) then
		begin
			// RenameFileEx(FTempFileName, FFileName); only on same disk
			if DirectoryExists(ExtractFilePath(ExpandDir(FFileName))) = False then
				IOError(FFileName, 3)
			else
			begin
				if not FDeleteAfterClose then
        begin
          OriginalFileExists := FileExists(FFileName);
          if (SkipSameData = False) or (OriginalFileExists = False) or (not SameFiles(FTempFileName, FFileName)) then
          begin
            if OriginalFileExists then
		          BackupFile(FFileName, FBackupFolder);
						uFiles.CopyFile(FTempFileName, FFileName, False);
          end;
        end;
			end;
			DeleteFileEx(FTempFileName);
		end
		else if FDeleteAfterClose then
			DeleteFileEx(FFileName);
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
				goto LRetry;
			Result := False;
		end
		else
			Result := True;
	end;
	FHandle := INVALID_HANDLE_VALUE;
end;

function TFile.Truncate: BG;
label LRetry;
var
	ErrorCode: U4;
begin
LRetry :
	Result := SetEndOfFile(FHandle);
	if Result then
	begin
		FFileSize := FFilePos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then
			goto LRetry;
	end;
end;

function TFile.FlushFileBuffers: BG;
label LRetry;
var
	ErrorCode: U4;
begin
	SaveBuffer;
LRetry :
	Result := Winapi.Windows.FlushFileBuffers(FHandle);
	if (Result = False) and Assigned(FLogger) then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then
			goto LRetry;
	end;
end;

function TFile.Eof: BG;
begin
	Result := FFilePos >= FFileSize;
end;

function TFile.Lock(From, Count: U8): BG;
label LRetry;
var
	ErrorCode: U4;
begin
LRetry :
	Result := LockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then
			goto LRetry;
	end;
end;

function TFile.UnLock(From, Count: U8): BG;
label LRetry;
var
	ErrorCode: U4;
begin
LRetry :
	Result := UnLockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then
			goto LRetry;
	end;
end;

function TFile.SaveBuffer: BG;
begin
	if FNeedSaveBuffer then
	begin
		Seek(FBufStart);
		Result := BlockWrite(FBuffer[0], Min(FFileSize - 1, FBufEnd) - FBufStart + 1);
		if Result then
			FNeedSaveBuffer := False;
	end
	else
		Result := True;
end;

procedure TFile.CreateBuffer;
begin
	SetLength(FBuffer, 0);
	FBufferSize := DefFileBuffer;
	FBufStart := High(FBufStart);
	FBufEnd := 0;
	FNeedSaveBuffer := False;
end;

procedure TFile.DestroyBuffer;
begin
	SaveBuffer;
	SetLength(FBuffer, 0);
end;

procedure TFile.FillBuffer;
begin
	if FBufStart = High(FBufStart) then
	begin
		SetLength(FBuffer, FBufferSize);
	end;
end;

procedure TFile.SetLogger(const Value: TDateTimeLogger);
begin
  FLogger := Value;
end;

procedure TFile.SetProtection(const Value: BG);
begin
	Assert(IsOpened = False);
	if Value <> FProtection then
	begin
		FProtection := Value;
	end;
end;

end.
