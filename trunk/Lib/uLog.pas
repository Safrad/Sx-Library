//* File:     Lib\uLog.pas
//* Created:  2006-05-03
//* Modified: 2006-05-03
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uLog;

interface

uses
	uTypes, uFiles,
	SysUtils;

type
	TLogType = (
		ltDebug, // Debug-level messages (Opening file)
		ltInfo, // Informational (Started, Finished)
		ltNotice, // (ltHint) Normal but significant condition
		ltWarn, // Warning conditions (File already opened)
		ltError, // Error conditions (File not found)
		ltCrit, // (ltFatalError) Critical conditions
		// ltAlert, // Action must be taken immediately
		// ltEmerg, // Emergencies - system is unusable
		ltNone);
const
	DefLogType = ltInfo;
var
	LogTypeStr: array[TLogType] of string;

type
	TLog = class(TObject)
	private
		FData: string;
		FFileName: TFileName;
		FFile: TFile;
		FDirectWrite: BG;
		FLoggingLevel: TLogType;
		procedure WriteLine(const Line: string; const FileBuffers: BG = False);
	public
		property FileName: TFileName read FFileName;
		constructor Create(const FileName: TFileName; const LogLevel: TLogType = DefLogType; const DirectWrite: BG= True);
		destructor Destroy; override;
		procedure Add(const Line: string; const LogType: TLogType = DefLogType; const FileBuffers: BG = False);
		procedure Flush;
		property LoggingLevel: TLogType read FLoggingLevel write FLoggingLevel default DefLogType;
	end;

// Add startup parameter -LogDebug for all debuging information
	procedure MainLogAdd(const Line: string; const LogType: TLogType = DefLogType; const FileBuffers: BG = False);
	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
	uParams,
	uFormat, uStrings, uInput, uProjectInfo,
	Windows, TypInfo;

procedure TLog.WriteLine(const Line: string; const FileBuffers: BG = False);
var LLog: TLog;
begin
	if not FFile.Opened then Exit;
	if FDirectWrite then
	begin
		LLog := MainLog;
		MainLog := nil;
		try
			FFile.Write(Line);
		finally
			MainLog := LLog;
		end;
	end
	else
	begin
		FData := FData + Line;
	end;
	if FileBuffers then Flush;
end;

const
	IdLine = ';Local Date Time	Type	Message' + FileSep;

constructor TLog.Create(const FileName: TFileName; const LogLevel: TLogType = DefLogType; const DirectWrite: BG = True);
var
	NewFileName: TFileName;
	Instance: SG;
	Info: TInfo;
begin
	inherited Create;
	FLoggingLevel := LogLevel;
	FFileName := FileName;
	FDirectWrite := DirectWrite;

	FFile := TFile.Create;
	for Instance := 1 to 9 do
	begin
		if FFile.Open(FFileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then Break;
		FFileName := DelFileExt(FFileName) +  IntToStr(Instance) + ExtractFileExt(FFileName);
		if Instance = 9 then Exit;
	end;

	if FFile.FileSize > 4 * MB then
	begin
		FFile.Close;
		NewFileName := DelFileExt(FFileName) + '_' + DateToS(FileTimeToDateTime(GetFileModified(FFileName))) + ExtractFileExt(FFileName);
		if FileExists(NewFileName) = False then
		begin
			RenameFile(FFileName, NewFileName);
		end;
		FFile.Open(FFileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False)
	end;

	if FFile.FileSize = 0 then
		WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine)
	else
		FFile.SeekEnd;
	Info := TInfo.Create(nil);
	Add('Started Version ' + Info.GetProjectInfo(piFileVersion), ltInfo, True);
	Info.Free;
end;

destructor TLog.Destroy;
begin
	Add('Finished', ltInfo, True);
	Flush;
	if Assigned(FFile) then
	begin
		FFile.Close;
		FreeAndNil(FFile);
	end;
	FFileName := '';
	inherited;
end;

procedure TLog.Add(const Line: string; const LogType: TLogType = DefLogType; const FileBuffers: BG = False);
begin
	Assert(LogType <> ltNone);
	if LogType >= FLoggingLevel then
	begin
		WriteLine(DateTimeToS(Now, 3) + CharTab + FirstChar(LogTypeStr[LogType]) + CharTab + AddEscape(Line) + FileSep, FileBuffers);
	end;
end;

procedure TLog.Flush;
var LLog: TLog;
begin
	LLog := MainLog;
	MainLog := nil;
	try
		if FDirectWrite then
		begin
			FFile.FlushFileBuffers;
		end
		else
		begin
			WriteStringToFile(FFileName, FData, True);
			FData := '';
		end;
	finally
		MainLog := LLog;
	end;
end;

function StrIndex(const s: string; const AStrings: array of string): SG;
var i: SG;
begin
	Result := -1;
	for i := 0 to Length(AStrings) - 1 do
		if s = AStrings[i] then
		begin
			Result := i;
			Break;
		end;
end;

function GetLoggingLevel(const s: string): TLogType;
var Index: SG;
begin
	Result := DefLogType;
	if s = '' then Exit;
	if (s[1] = '0') then
		Result := ltNone
	else
	begin
		Index := StrIndex(CapitalCase(s), LogTypeStr);
		if (Index >= SG(Low(Result))) and (Index <= SG(High(Result))) then
			Result := TLogType(Index);
	end;
end;

procedure MainLogAdd(const Line: string; const LogType: TLogType = ltInfo; const FileBuffers: BG = False);
begin
	if Assigned(MainLog) then
		MainLog.Add(Line, LogType, FileBuffers);
end;

procedure SetLoggingLevel(const Value: string);
begin
	MainLog.LoggingLevel := GetLoggingLevel(Value);
end;

procedure InitializeLog;
var
	i: SG;
	s: string;
	OldFileName: TFileName;
	OutStr, Line, IdStr, DTStr: string;
	LineIndex, InLineIndex: SG;
begin
	CreateDirEx(ExtractFilePath(MainLogFileName));
	OldFileName := WorkDir + AppName + '.log';
	if FileExists(OldFileName) then
	begin
		// Convert Old Format
		if ReadStringFromFile(OldFileName, s) then
		begin
			if (Length(s) > 0) and (s[1] <> ';') then
			begin
				OutStr := IdLine;
				LineIndex := 1;
				while LineIndex <= Length(s) do
				begin
					Line := ReadToNewLine(s, LineIndex);
					if Line = '' then Continue;

					InLineIndex := 1;
					IdStr := ReadToChar(Line, InLineIndex, CharTab);
					DTStr := Copy(Line, InLineIndex, MaxInt);

					Replace(IdStr, ['S', 'F'], ['Started', 'Finished']);

					OutStr := OutStr + DTStr + CharTab + 'I' + CharTab + IdStr + FileSep;
				end;
				if WriteStringToFile(MainLogFileName, OutStr, False) then
					if OldFileName <> MainLogFileName then
						DeleteFileEx(PChar(OldFileName));
			end
			else
			begin
				RenameFile(OldFileName, MainLogFileName);
			end;
		end;
	end;

	for i := 0 to Length(LogTypeStr) - 1 do
	begin
		LogTypeStr[TLogType(i)] := Copy(GetEnumName(TypeInfo(TLogType), i), 3, MaxInt);
		s := s + LogTypeStr[TLogType(i)] + '|';
	end;
	DelLastChar(s);

	MainLog := TLog.Create(MainLogFileName); //, GetLoggingLevel(GetParamValue('LOG')), True);

	RegisterParam('Log', 'Logging level: Log[' + s + ']', SetLoggingLevel);
end;

initialization

finalization
	FreeAndNil(MainLog);
end.
