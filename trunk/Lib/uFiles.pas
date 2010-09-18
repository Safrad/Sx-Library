// Build: 01/1998-11/1999 Author: Safranek David

unit uFiles;

interface

uses SysUtils;

// File system
type
	TString = ShortString;
	TFileNames = array of TFileName;

	TFileMode = (fmReadOnly, fmWriteOnly, fmReadAndWrite);
	TFile = class(TObject)
	private
		f: file;
		FName: ^TFileName;
		ErrorCode: Integer;
	public
		function Open(var FileName: TFileName; const Mode: TFileMode): Boolean;
		function FileSize(var Size: Cardinal): Boolean;
		function Seek(const Pos: Integer): Boolean;
		function BlockRead(var Buf; const Count: Cardinal): Boolean;
		function BlockWrite(var Buf; const Count: Cardinal): Boolean;
		function Close: Boolean;
	end;

var
	StartDir, // Dir with Ini and configuratios files (read and write)
	WorkDir, // Dir with exe file, data files (read only)
	SharedDir, // Dir with common sounds, fonts uDBitBtn
	ExeFileName: TString;

//procedure InitPaths; {$ifdef DLL}stdcall;{$endif}
function ShortDir(const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function FullDir (const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function DelFileExt(const FName: TString): TString; {$ifdef DLL}stdcall;{$endif}
function BackDir(const Dir: TString): TString; {$ifdef DLL}stdcall;{$endif}
function LegalFileName(const FileName: TString): TString; {$ifdef DLL}stdcall;{$endif}
procedure ReadDirectory(var FileNames: TFileNames; Path, Extension: string);

implementation

uses
	Dialogs, Windows,
	uError;

function TFile.Open(var FileName: TFileName; const Mode: TFileMode): Boolean;
label LRetry;
begin
	FName := Pointer(FileName);
	LRetry:
	System.AssignFile(f, FileName);
	System.FileMode := Integer(Mode);
	System.Reset(f, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(TFileName(FName), ErrorCode) then goto LRetry;
		Result := True;
	end
	else
		Result := False;
end;

function TFile.FileSize(var Size: Cardinal): Boolean;
begin
	Size := System.FileSize(f);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		Result := True;
		IOError(TFileName(FName), ErrorCode);
	end
	else
		Result := False;
end;

function TFile.Seek(const Pos: Integer): Boolean;
begin
	System.Seek(f, Pos);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		Result := True;
		IOError(TFileName(FName), ErrorCode);
	end
	else
		Result := False;
end;

function TFile.BlockRead(var Buf; const Count: Cardinal): Boolean;
begin
	System.BlockRead(f, Buf, Count);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
		Result := IOErrorRetry(TFileName(FName), ErrorCode)
	else
		Result := False;
end;

function TFile.BlockWrite(var Buf; const Count: Cardinal): Boolean;
begin
	System.BlockWrite(f, Buf, Count);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
		Result := IOErrorRetry(TFileName(FName), ErrorCode)
	else
		Result := False;
end;

function TFile.Close: Boolean;
begin
	System.CloseFile(f);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		Result := True;
		IOError(TFileName(FName), ErrorCode);
	end
	else
		Result := False;
end;

procedure InitPaths;
var
	i: Integer;
begin
	GetDir(0, StartDir);
	if StartDir[Length(StartDir)] <> '\' then StartDir := StartDir + '\';

	WorkDir := GetCommandLine;
	if WorkDir[1] = '"' then Delete(WorkDir, 1, 1);
	i := Pos('"', WorkDir);
	if i > 0 then Delete(WorkDir, i, Length(WorkDir) - i + 1);

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
	SharedDir := BackDir(WorkDir) + 'Shared\';
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
	ErrorCode := FindFirst(Path + '*.*', faReadOnly or faArchive, SearchRec);
	while ErrorCode = 0 do
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

initialization
	InitPaths;
end.
