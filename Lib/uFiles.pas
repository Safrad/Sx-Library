// Build: 01/1998-03/2001 Author: Safranek David

unit uFiles;

interface

uses
	uAdd,
	SysUtils, Windows, Classes;

// File system
const
	DefFileBuffer = 32768; // Best Performance
type
	TString = string;
//	TLines = array of string;
	TFileNames = array of TFileName;

	TFileMode = (fmReadOnly, fmWriteOnly, fmReadAndWrite);

	{
		Flags:
				FILE_FLAG_OVERLAPPED // Async read - not implemented yet

				FILE_FLAG_RANDOM_ACCESS
				FILE_FLAG_SEQUENTIAL_SCAN

				FILE_FLAG_WRITE_THROUGH // For write only
				FILE_FLAG_NO_BUFFERING // Be carefully for use this
	}

	TFile = class(TObject)
	private
		HFile: THANDLE;
		FFileName: ^TFileName;
		FTempFileName:  TFileName;

		FMode: TFileMode;
		// For Write only
		FProtection: Boolean;

		// For Readln only
		FBuffer: array of Char;
		FBufStart, FBufEnd: U64;
		FBufferSize: U64;
		FFilePos: U64;
		FFileSize: U64;
		function GetFileSize(var Size: U64): Boolean;
	public
		ErrorCode: U32;
		EofStr: ShortString; // For Writeln only
		property FilePos: U64 read FFilePos;
		property FileSize: U64 read FFileSize;
		constructor Create;
		destructor Free;
		function Open(var FileName: TFileName; const Mode: TFileMode; Flags: U32; Protection: Boolean): Boolean;
		function Seek(const Pos: U64): Boolean;
		function BlockRead(var Buf; const Count: Cardinal): Boolean;
		function BlockWrite(var Buf; const Count: Cardinal): Boolean;
		function FillWrite(const Count: Cardinal): Boolean;
		function Readln(var Line: string): Boolean;
		function Write(Line: string): Boolean;
		function Writeln(Line: string): Boolean;
		function WritelnW(Line: WideString): Boolean;
		function Close: Boolean;
		function Truncate: Boolean;
		function FlushFileBuffers: Boolean;
		function Eof: Boolean;
		function Lock(From, Count: U64): Boolean;
		function UnLock(From, Count: U64): Boolean;
	end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): Boolean;
{
	Function Results: False: Error, True: Ok
	Example:
	label LRetry;
	var
		F: TFile;
		FileName: TFileName;
	begin
		F := TFile.Create;
		FileName := WorkDir + 'FileName.txt';
		LRetry:
		if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			if not F.BlockRead() then goto LRetry
			if not F.Readln() then goto LRetry;

			if not F.Close then goto LRetry;
		end;
		F.Free;
	end;
}
var
	StartDir, // Dir with Ini and configuratios files (read and write)
	WorkDir, // Dir with exe file, data files (read only)
	GraphDir,
	SoundsDir,
	DataDir,
	SysDir, WinDir: TString;
	ExeFileName: TFileName;

//procedure InitPaths; {$ifdef DLL}stdcall;{$endif}
function ShortDir(const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function FullDir (const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function DelFileExt(const FName: TString): TString; {$ifdef DLL}stdcall;{$endif}
function BackDir(const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function LegalFileName(const FileName: TString): TString; {$ifdef DLL}stdcall;{$endif}
procedure ReadDirectory(var FileNames: TFileNames; Path, Extension: string);
procedure ReadDirectory2(var FileNames: TFileNames; Path: string);
function GetFileSiz(const FileName: TFileName): U64; {$ifdef DLL}stdcall;{$endif}
function CreateDir(const Dir: string): Boolean;
function CopyFile(const Source, Dest: TFileName; const FailExist: Boolean): Boolean;
function DeleteFile(const FileName: TFileName): Boolean;
function CopyDir(const Source, Dest: string): Boolean;
function DeleteFolder(const Folder: string): Boolean;

function ReadBufferFromFile(var FileName: TFileName; var Buf; var Count: SG): BG;
function WriteBufferToFile(var FileName: TFileName; var Buf; var Count: SG): BG;
function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
function WriteLinesToFile(var FileName: TFileName; Lines: TStrings): BG;

{
	MapViewOfFile
	OpenFileMapping
	CreateFileMapping
}


implementation

uses
	Dialogs, FileCtrl,
	uError;

constructor TFile.Create;
begin
	EofStr := #13 + #10;
	HFile := INVALID_HANDLE_VALUE;
end;

destructor TFile.Free;
begin
	if HFile <> INVALID_HANDLE_VALUE then Close;
end;

function TFile.Open(var FileName: TFileName; const Mode: TFileMode; Flags: U32; Protection: Boolean): Boolean;
label LRetry;
var
	CreationDistribution: U32;
	DesiredAccess, ShareMode: U32;
begin
	Result := False;
	FFileName := Pointer(FileName);
	FFilePos := 0;
	FMode := Mode;
	SetLength(FBuffer, 0);
	FBufferSize := DefFileBuffer;
	SetLength(FBuffer, FBufferSize);
	FBufStart := 1;
	FBufEnd := 0;
	FProtection := Protection;

	if FProtection and (FMode <> fmReadOnly) then
	begin
		FTempFileName := ExtractFilePath(FileName) + '$' + ExtractFileName(FileName);
{   if FileExists(FTempFileName) then
		begin
			DeleteFile(FTempFileName);
		end;}
		if FileExists(FileName) then
			CopyFile(FileName, FTempFileName, False);

	end
	else
		FTempFileName := FileName;


	LRetry:
	case Mode of
	fmReadOnly:
	begin
		DesiredAccess := GENERIC_READ;
		ShareMode := FILE_SHARE_READ;
	end;
	fmWriteOnly:
	begin
		DesiredAccess := GENERIC_WRITE;
		ShareMode := 0;//FILE_SHARE_READ;
	end;
	fmReadAndWrite:
	begin
		DesiredAccess := GENERIC_READ or GENERIC_WRITE;
		ShareMode := 0;//FILE_SHARE_READ;
	end
	else
	begin
		DesiredAccess := 0;
		ShareMode := 0;
	end;
	end;
{ FILE_SHARE_READ
	FILE_SHARE_WRITE}

	if (FileExists(FTempFileName) = False) and (FMode <> fmReadOnly) then
		CreationDistribution := CREATE_NEW
	else
		CreationDistribution := OPEN_EXISTING;

	HFile := CreateFile(
		PChar(FTempFileName), // pointer to name of the file
		DesiredAccess,  // access (read-write) mode
		ShareMode,  // share mode
		nil,  // pointer to security attributes
		CreationDistribution, // how to create
		FILE_ATTRIBUTE_NORMAL or Flags, // file attributes
		0   // handle to file with attributes to copy
	 );
	if HFile = INVALID_HANDLE_VALUE then
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if IOErrorRetry(TFileName(FTempFileName), ErrorCode) then goto LRetry;
		end
		else
		begin
			GetFileSize(FFileSize);
			Result := True;
		end;
	end
	else
	begin
		GetFileSize(FFileSize);
		Result := True;
	end;
end;

function HandleFileSize(HFile: THandle; var Size: U64): U32;
begin
	TU64(Size).D0 := Windows.GetFileSize(HFile, @TU64(Size).D1);

	if TU64(Size).D0 = $FFFFFFFF then
	begin
		Result := GetLastError;
	end
	else
		Result := NO_ERROR;
end;

function TFile.GetFileSize(var Size: U64): Boolean;
begin
	Result := HandleFileSize(HFile, Size) = NO_ERROR;
	if Result = False then
		IOError(TFileName(FTempFileName), ErrorCode);
end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): Boolean;
var HFile: THANDLE;
begin
	Result := False;
	U64(CreationTime) := 0;
	U64(LastAccessTime) := 0;
	U64(LastWriteTime) := 0;
	HFile := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0,  // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0   // handle to file with attributes to copy
	 );
	if HFile <> INVALID_HANDLE_VALUE then
	begin
		Result := GetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime);
		CloseHandle(HFile);
	end;
end;

function TFile.Seek(const Pos: U64): Boolean;
begin
	Result := False;
	if SetFilePointer(
		HFile,  // handle of file
		TU64(Pos).D0, // number of bytes to move file pointer
		@TU64(Pos).D1,  // address of high-order word of distance to move
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
			IOError(TFileName(FTempFileName), ErrorCode);
		end
		else
			Result := True;
	end;
end;

function TFile.BlockRead(var Buf; const Count: Cardinal): Boolean;
var Suc: U32;
begin
	if ReadFile(HFile, Buf, Count, Suc, nil) then
	begin
		Inc(FFilePos, Suc);
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode)
		end
		else
			Result := True;
	end;
end;

function TFile.BlockWrite(var Buf; const Count: Cardinal): Boolean;
var Suc: U32;
begin
	if WriteFile(HFile, Buf, Count, Suc, nil) then
	begin

		Inc(FFilePos, Suc);
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode)
		end
		else
			Result := True;
	end;
end;

function TFile.FillWrite(const Count: Cardinal): Boolean;
var
	Buf: PArrayByte;
begin
	GetMem(Buf, Count);
	FillChar(Buf^, Count, 0);
	Result := BlockWrite(Buf^, Count);
	FreeMem(Buf, Count);
end;

function TFile.Readln(var Line: string): Boolean;
label LN;
var BufPos: Integer;
begin
	Line := '';
	Result := True;
	while True do
	begin
		LN:
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			BufPos := FFilePos - FBufStart;
			if Eof or (FBuffer[BufPos] = #10) then
			begin
				Inc(FFilePos);
				Seek(FFilePos);
				Break;
			end;
		end
		else
		begin
			FBufStart := FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1;
			if FBufEnd > FFileSize then FBufEnd := FFileSize;
			Seek(FBufStart);
			BufPos := FFilePos;
			BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
			FFilePos := BufPos;
//      BufPos := FFilePos - FBufStart;
			goto LN;
		end;
		if FBuffer[BufPos] <> #13 then
			Line := Line + FBuffer[BufPos];
		Inc(FFilePos);
	end;
end;

function TFile.Write(Line: string): Boolean;
begin
	if Length(Line) > 0 then
		Result := BlockWrite(Line[1], Length(Line))
	else
		Result := True;
end;

function TFile.Writeln(Line: string): Boolean;
begin
	if Length(Line) > 0 then
		BlockWrite(Line[1], Length(Line));
	Result := BlockWrite(EofStr[1], Length(EofStr));
end;

function TFile.WritelnW(Line: WideString): Boolean;
var v: string;
begin
	if Length(Line) > 0 then
		BlockWrite(Line[1], 2 * Length(Line));
	v := #0 + #10;
	Result := BlockWrite(v[1], SizeOf(v));
//  Inc(FFilePos, 2 * Length(Line) + Length(EofStr));
end;

function TFile.Close: Boolean;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	SetLength(FBuffer, 0);
	if FMode <> fmReadOnly then
	begin
		if GetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime) then
		begin
			GetSystemTimeAsFileTime(LastWriteTime);
			SetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime);
		end;
	end;
	if CloseHandle(HFile) then
	begin
		if FProtection and (FMode <> fmReadOnly) then
		begin
			if CopyFile(PChar(FTempFileName), PChar(FFileName), False) then
				DeleteFile(PChar(FTempFileName));
		end;
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode);
		end
		else
		begin
			Result := True;
		end;
	end;
	HFile := INVALID_HANDLE_VALUE;
end;

function TFile.Truncate: Boolean;
begin
	if SetEndOfFile(HFile) then
	begin
		Result := False;
		FFileSize := FFilePos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := True;
			IOError(TFileName(FTempFileName), ErrorCode);
		end
		else
		begin
			Result := False;
			FFileSize := FFilePos;
		end;
	end;
end;

function TFile.FlushFileBuffers: Boolean;
begin
	Result := Windows.FlushFileBuffers(HFile);
	if Result = False then
		ErrorCode := GetLastError;
end;

function TFile.Eof: Boolean;
begin
	Result := FFilePos >= FFileSize;
end;

function TFile.Lock(From, Count: U64): Boolean;
begin
	Result := LockFile(HFile, TU64(From).D0, TU64(From).D1, TU64(Count).D0, TU64(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
	end;
end;

function TFile.UnLock(From, Count: U64): Boolean;
begin
	Result := UnLockFile(HFile, TU64(From).D0, TU64(From).D1, TU64(Count).D0, TU64(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
	end;
end;

// Utils

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
begin
	GetDir(0, StartDir);
	if StartDir[Length(StartDir)] <> '\' then StartDir := StartDir + '\';

	WorkDir := GetCommandLine;
	if WorkDir[1] = '"' then
	begin
		Delete(WorkDir, 1, 1);
		i := Pos('"', WorkDir);
		if i > 0 then Delete(WorkDir, i, Length(WorkDir) - i + 1);
	end
	else
	begin
		i := Pos(' ', WorkDir);
		Delete(WorkDir, i, Length(WorkDir) - i + 1);
	end;

	ExeFileName := WorkDir;

	for i := Length(WorkDir) downto 0 do
	begin
		if i = 0 then
		begin
			WorkDir := '';
			Break;
		end;
		if (WorkDir[i] = '\') then
		begin
			SetLength(WorkDir, i);
			Break;
		end;
	end;
	if (WorkDir = '') then WorkDir := StartDir;
//  SharedDir := BackDir(WorkDir) + 'Shared\';
	GraphDir := WorkDir + 'Graphics\';
	SoundsDir := WorkDir + 'Sounds\';
	DataDir := WorkDir + 'Data\';

	SetLength(SysDir, MAX_PATH);
	NewLength := GetSystemDirectory(PChar(SysDir), MAX_PATH);
	SetLength(SysDir, NewLength);
	if (Length(SysDir) > 0) and (SysDir[Length(SysDir)] <> '\') then
		SysDir := SysDir + '\';

	SetLength(WinDir, MAX_PATH);
	NewLength := GetwindowsDirectory(PChar(WinDir), MAX_PATH);
	SetLength(WinDir, NewLength);
	if (Length(WinDir) > 0) and (WinDir[Length(WinDir)] <> '\') then
		WinDir := WinDir + '\';
end;

function ShortDir(const Dir: TString): TString;
var
	i: Integer;
begin
	Result := Dir;
	if Length(WorkDir) <= Length(Dir) then
	begin
		for i := 1 to Length(WorkDir) do
		begin
			if UpCase(Dir[i]) <> UpCase(WorkDir[i]) then
			begin
				Exit;
			end;
		end;
		for i := 1 to Length(Dir) - Length(WorkDir) do
		begin
			Result[i] := Dir[i + Length(WorkDir)];
		end;
		SetLength(Result, Length(Dir) - Length(WorkDir));
	end;
end;

function FullDir(const Dir: TString): TString;
var
	i: Integer;
begin
	for i := 1 to Length(Dir) do
	begin
		if Dir[i] = ':' then
		begin
			Result := Dir;
			Exit;
		end;
	end;
	Result := WorkDir + Dir;
end;

function DelFileExt(const FName: TString): TString;
var
	Ext: TString;
begin
	Result := FName;
	Ext := ExtractFileExt(FName);
	if Length(Ext) > 0 then SetLength(Result, Length(Result) - Length(Ext));
end;

function BackDir(const Dir: TString): TString;
var i: Integer;
begin
	Result := Dir;
	for i := Length(Result) - 1 downto 1 do
	begin
		if Result[i] = '\' then
		begin
			SetLength(Result, i);
			Exit;
		end;
	end;
end;

function LegalFileName(const FileName: TString): TString;
var
	i: Integer;
	StrLength: Integer;
begin
	Result := FileName;
	if Length(Result) = 0 then
	begin
		Result := '';
		Exit;
	end
	else if Length(Result) > 63 then
	begin
		SetLength(Result, 63);
	end;

	i := 1;
	StrLength := Length(Result);
	while i <= StrLength do
	begin
		case Result[i] of
		'a'..'z', 'A'..'Z', '0'..'9', '_', '.', '-', ' ', #160, {special space}
		'+', '=', '`',
		'~', '!', '@', '#', '$', '%', '^', '&', '(', ')',
		'{', '}', '''', #180, ';', '[', ']', ',',
		'\', ':': // 'C:\'
		begin
			Inc(i);
		end
		else
		begin
			Delete(Result, i, 1);
			Dec(StrLength);
		end;
		end;
	end;
end;

procedure ReadDirectory(var FileNames: TFileNames; Path, Extension: string);
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;

	i: Integer;
	Offset: Integer;
	MaxLimit: Integer;
	Switch: Integer;
	FileName: TFileName;
begin
	if Length(Extension) > 1 then
		if Extension[1] <> '.' then Extension := '.' + Extension;
	if Length(Path) > 1 then
		if Path[Length(Path)] <> '\' then
			Path := Path + '\';

	ErrorCode := FindFirst(Path + '*.*', faReadOnly or faArchive, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (UpperCase(ExtractFileExt(SearchRec.Name)) = UpperCase(Extension)) then
		begin
			SetLength(FileNames, Length(FileNames) + 1);
			FileNames[Length(FileNames) - 1] := SearchRec.Name;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> 18 then IOError(Path, ErrorCode);
	SysUtils.FindClose(SearchRec);

	// Sort
	Offset := Length(FileNames) div 2;
	while Offset > 0 do
	begin
		MaxLimit := Length(FileNames) - Offset - 1;
		repeat
			Switch := 0;
			for i := 0 to MaxLimit do
			begin
				if FileNames[i] > FileNames[i + Offset] then
				begin
					FileName := FileNames[i];
					FileNames[i] := FileNames[i + Offset];
					FileNames[i + Offset] := FileName;
					Switch := i;
				end;
			end;
			MaxLimit := Switch - Offset;
		until Switch = 0;
		Offset := 2 * Offset div 3;
	end;
end;

procedure ReadDirectory2(var FileNames: TFileNames; Path: string);
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
	i: SG;
begin
	if Length(Path) > 1 then
		if Path[Length(Path)] <> '\' then
			Path := Path + '\';

	ErrorCode := FindFirst(Path + '*.*', faReadOnly or faHidden or faSysFile or faArchive or faDirectory, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
				ReadDirectory2(FileNames, Path + SearchRec.Name + '\')
		end
		else
		begin
			i := Length(FileNames);
{			NewSize := i + 1;
			if AllocByEx(i, NewSize, 4) then}
				SetLength(FileNames, i + 1{NewSize});
			FileNames[i] := Path + SearchRec.Name;
		end;

		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> 18 then IOError(Path, ErrorCode);
	SysUtils.FindClose(SearchRec);
end;

function GetFileSiz(const FileName: TFileName): U64;
var
	HFile: THandle;
begin
	Result := 0;
	HFile := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0,  // access (read-write) mode
		0,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0 // handle to file with attributes to copy
	 );
	if HFile = INVALID_HANDLE_VALUE then
	begin
		GetLastError;
	end
	else
	begin
		HandleFileSize(HFile, Result);
		if CloseHandle(HFile) = False then
			GetLastError;
	end;
end;

function CreateDir(const Dir: string): Boolean;
begin
	if DirectoryExists(Dir) then
		Result := True
	else
	begin
		Result := CreateDirectory(PChar(Dir), nil);
		if Result = False then
			IOError(Dir, GetLastError);
	end;
end;

function CopyFile(const Source, Dest: TFileName; const FailExist: Boolean): Boolean;
begin
	Result := Windows.CopyFile(PChar(Source), PChar(Dest), FailExist);
	if Result = False then
		IOError(Source + #13 + #10 + Dest, GetLastError);
end;

function DeleteFile(const FileName: TFileName): Boolean;
begin
	Result := Windows.DeleteFile(PChar(FileName));
	if Result = False then
		IOError(FileName, GetLastError);
end;

function CopyDir(const Source, Dest: string): Boolean;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CreateDir(Dest);

	ErrorCode := FindFirst(Source + '*.*', faReadOnly or faHidden or faSysFile or faArchive or faDirectory, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
				CopyDir(Source + SearchRec.Name + '\', Dest + SearchRec.Name + '\')
		end
		else
		begin
			CopyFile(Source + SearchRec.Name, Dest + SearchRec.Name, False);
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
	if ErrorCode <> 18 then IOError(Source, ErrorCode);
	SysUtils.FindClose(SearchRec);
end;

function DeleteFolder(const Folder: string): Boolean;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	ErrorCode := FindFirst(Folder + '*.*', faReadOnly or faHidden or faSysFile or faArchive or faDirectory, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
			begin
				DeleteFolder(Folder + SearchRec.Name + '\');
				RemoveDir(Folder + SearchRec.Name)
			end;
		end
		else
		begin
			DeleteFile(Folder + SearchRec.Name);
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
	if ErrorCode <> 18 then IOError(Folder, ErrorCode);
	SysUtils.FindClose(SearchRec);
end;

function ReadBufferFromFile(var FileName: TFileName; var Buf; var Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		GetMem(Pointer(Buf), F.FileSize);
		F.BlockRead(Pointer(Buf)^, F.FileSize);
		Count := F.FileSize;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function WriteBufferToFile(var FileName: TFileName; var Buf; var Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		F.BlockWrite(Pointer(Buf)^, Count);
		F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
label LRetry;
var
	F: TFile;
	Line: string;
begin
	Lines.Clear;
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			F.Readln(Line);
			Lines.Add(Line);
		end;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function WriteLinesToFile(var FileName: TFileName; Lines: TStrings): BG;
label LRetry;
var
	F: TFile;
	i: SG;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		i := 0;
		while i < Lines.Count do
		begin
			F.Writeln(Lines[i]);
			Inc(i);
		end;
		F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

initialization
	InitPaths;
end.
