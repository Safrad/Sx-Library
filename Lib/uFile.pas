//* File:     Lib\uFile.pas
//* Created:  1998-01-01
//* Modified: 2008-05-11
//* Version:  1.1.41.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

{
	Result of Functions : False: Error, True: Ok
	Example:
	var
		F: TFile;
	begin
		F := TFile.Create;
		try
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

interface

uses
	SysUtils, Windows,
	uTypes, uStrings;

const
{
	Buffer Size, Performance
	1..16 * KB, low
	32 * KB, acceptable
	64..128 * KB, the best
	32 * MB, Win API maximum
}
	DefFileBuffer = {$ifndef Console}64{$else}32{$endif} * KB;
	// For write FMode only, if enabled temporary file is used first
type
	TFileMode = (fmReadOnly, fmRewrite, fmAppend, fmReadAndWrite);
var
	FileModeStr: array[TFileMode] of string;
type
	{
		Flags:
				FILE_FLAG_OVERLAPPED // Async read - not implemented yet

				FILE_FLAG_RANDOM_ACCESS
				FILE_FLAG_SEQUENTIAL_SCAN // Default value

				FILE_FLAG_WRITE_THROUGH // For write only
				FILE_FLAG_NO_BUFFERING // Be carefully for use this
	}

	TFile = class(TObject)
	private
		FProtection: BG;
		FHandle: THandle;
		FFileName: TFileName;
		FTempFileName:  TFileName;

		FMode: TFileMode;

		// For Readln, Write(ln) only
		FBuffer: array of Char;
		FBufStart, FBufEnd: U8;
		FBufferSize: U8;
		FNeedSaveBuffer: BG;

		FFilePos: U8;
		FFileSize: U8;
		function GetFileSize(var Size: U8): BG;
		function IsOpened: BG;
		function ErrorRetry(const ErrorCode: U4): BG;
		procedure CreateBuffer;
		procedure FillBuffer;
		procedure DestroyBuffer;
		function SaveBuffer: BG;
		procedure SetProtection(const Value: BG);
	public
		property Protection: BG read FProtection write SetProtection;
		property Handle: THandle read FHandle;
		property FileName: TFileName read FFileName;
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read IsOpened;
		constructor Create;
		destructor Destroy; override;
		function Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG; overload;
		function Seek(const Pos: U8): BG;
		function SeekBegin: BG;
		function SeekEnd: BG;
		function BlockRead(out Buf; const Count: UG): BG;
		function BlockWrite(const Buf; const Count: UG): BG;
		function FillWrite(Count: UG): BG;
		function Readln(out Line: string): BG;
		function Write(const Line: string): BG;
		function Writeln(const Line: string): BG;
		function WritelnW(Line: WideString): BG;
		function Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
		function Truncate: BG;
		function FlushFileBuffers: BG;
		function Eof: BG;
		function Lock(From, Count: U8): BG;
		function UnLock(From, Count: U8): BG;
	end;

var
	ReadCount, WriteCount: UG;
	ReadBytes, WriteBytes: U8;

implementation

uses
	Math,
	uMsg, uFiles, uLog, uOutputFormat;

constructor TFile.Create;
begin
	inherited Create;
	FHandle := INVALID_HANDLE_VALUE;
	FProtection := True;
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

{
function TFile.Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
var
	FileName2: TFileName;
begin
	FileName2 := FileName;
	Result := Open(FileName2, Mode, Flags, False);
end; }

function TFile.Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
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
		FTempFileName := TempDir + '~' + ExtractFileName(FileName);
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
	MainLogAdd('Opening for ' + FileModeStr[Mode] + ' ' + FTempFileName, mlDebug);

	LRetry:
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
	FHandle := CreateFile(
		PChar(FTempFileName), // pointer to name of the file
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
			if ErrorRetry(ErrorCode) then goto LRetry;
			Exit;
		end;
	end;
	GetFileSize(FFileSize);
	if Mode = fmAppend then SeekEnd;
	if Mode = fmRewrite then Truncate;
	Result := True;
end;

function TFile.GetFileSize(var Size: U8): BG;
begin
	Size :=  HandleFileSize(FHandle);
	Result := Size >= 0;
end;

function TFile.Seek(const Pos: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := False;
	if SetFilePointer(
		FHandle,  // handle of file
		TU8(Pos).D0, // number of bytes to move file pointer
		@TU8(Pos).D1,  // address of high-order word of distance to move
		FILE_BEGIN    // how to move
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
			if ErrorRetry(ErrorCode) then goto LRetry;
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
	LRetry:
	if ReadFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;
		Inc(ReadCount);
		Inc(ReadBytes, Suc);

		if Suc <> Count then
		begin
			Warning('Reading only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO) + ' from ' + FTempFileName);
			Result := False;
		end
		else
			MainLogAdd('Reading ' + BToStr(Suc, ofIO) + ' from ' + FTempFileName, mlDebug);

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
	LRetry:
	if WriteFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;
		Inc(WriteCount);
		Inc(WriteBytes, Suc);

		if Suc <> Count then
			Warning('Writing only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO) + ' to ' + FTempFileName)
		else
			MainLogAdd('Writing ' + BToStr(Suc, ofIO) + ' to ' + FTempFileName, mlDebug);

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

function TFile.FillWrite(Count: UG): BG;
var
	Buf: Pointer;
	C: UG;
begin
	C := Min(Count, DefFileBuffer);
	Buf := AllocMem(C);
	Result := True;
	while Count > 0 do
	begin
		Result := Result and BlockWrite(Buf^, C);
		Count := Count - C;
		if C > Count then C := Count;
		if Result = False then Break;
	end;

	FreeMem(Buf);
end;

{
	File   |********************************|
	Buffer                 |*******|
}

function TFile.Readln(out Line: string): BG;
var
	BufPos: SG;
	InLineIndex: SG;
	LineLength: SG;
begin
	Line := '';
	if Eof then
	begin
		Result := False;
		Exit;
	end;
	Result := True;
	FillBuffer;

	LineLength := 256;
	SetLength(Line, LineLength);
	InLineIndex := 1;
	while not Eof do
	begin
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			BufPos := FFilePos - FBufStart;
			if {Eof or} (FBuffer[BufPos] = CharLF) then
			begin
				Inc(FFilePos);
//				Seek(FFilePos);
				Break;
			end;
		end
		else
		begin
			SaveBuffer;
			FBufStart := FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1;
			if FBufEnd >= FFileSize then FBufEnd := FFileSize - 1;
			Seek(FBufStart);
			BufPos := FFilePos;
			BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
			FFilePos := BufPos;
//      BufPos := FFilePos - FBufStart;
			Continue;
		end;
		if FBuffer[BufPos] <> CharCR then
		begin
			if InLineIndex > LineLength then
			begin
				LineLength := 2 * (InLineIndex - 1);
				SetLength(Line, LineLength);
			end;
			Line[InLineIndex] := FBuffer[BufPos];
			Inc(InLineIndex);
		end;
		Inc(FFilePos);
	end;
	SetLength(Line, InLineIndex - 1);
end;

function TFile.Write(const Line: string): BG;
var
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
			if not SaveBuffer then Exit;
			// Create new buffer.
			FBufStart := FFilePos; //FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1; //, LineLength - InLineIndex);
		end;
	end;
end;

function TFile.Writeln(const Line: string): BG;
begin
	Result := Write(Line + FileSep);
end;

function TFile.WritelnW(Line: WideString): BG;
begin
	Line := Line + CharNul + CharLF;
	Result := BlockWrite(Line[1], 2 * Length(Line));
end;

function TFile.Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
label LRetry;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	ErrorCode: U4;
begin
	LRetry:
	Result := False;
	if not IsOpened then
	begin
		Warning('Cannot again close file %1.', [FTempFileName]);
		Exit;
	end;
	MainLogAdd('Closing ' + FTempFileName, mlDebug);

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
//		RenameFileEx(FTempFileName, FFileName); only on same disk
			if DirectoryExists(ExtractFileDir(FFileName)) = False then
				IOError(FFileName, 3)
			else
			begin
				CopyFile(FTempFileName, FFileName, False);
				DeleteFileEx(FTempFileName);
			end;
		end;
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then goto LRetry;
			Result := False;
		end
		else
			Result := True;
	end;
	FHandle := INVALID_HANDLE_VALUE;
end;

function TFile.Truncate: BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := SetEndOfFile(FHandle);
	if Result then
	begin
		FFileSize := FFilePos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.FlushFileBuffers: BG;
label LRetry;
var ErrorCode: U4;
begin
	SaveBuffer;
	LRetry:
	Result := Windows.FlushFileBuffers(FHandle);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.Eof: BG;
begin
	Result := FFilePos >= FFileSize;
end;

function TFile.Lock(From, Count: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := LockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.UnLock(From, Count: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := UnLockFile(FHandle, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
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

procedure TFile.SetProtection(const Value: BG);
begin
	Assert(IsOpened = False);
	if Value <> FProtection then
	begin
		FProtection := Value;
	end;
end;

end.
