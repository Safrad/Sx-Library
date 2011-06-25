//* File:     Lib\uLog.pas
//* Created:  2006-05-03
//* Modified: 2007-05-27
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uLog;

interface

uses
	uTypes, uFiles, uMsg,
	SysUtils;

const
	DefaultLoggingLevel = mtInformation;

type
	TLog = class(TObject)
	private
		FData: string;
		FFileName: TFileName;
		FFile: TFile;
		FDirectWrite: BG;
		FLoggingLevel: TMsgType;
		procedure WriteLine(const Line: string; const FileBuffers: BG = False);
	public
		property FileName: TFileName read FFileName;
		constructor Create(const FileName: TFileName; const LogLevel: TMsgType = DefaultLoggingLevel; const DirectWrite: BG= True);
		destructor Destroy; override;
		procedure Add(const Line: string; const LogType: TMsgType = DefaultLoggingLevel; const FileBuffers: BG = False);
		procedure Flush;
		property LoggingLevel: TMsgType read FLoggingLevel write FLoggingLevel default DefaultLoggingLevel;
	end;

// Add startup parameter -LogDebug for all debuging information
	procedure MainLogAdd(const Line: string; const LogType: TMsgType = DefaultLoggingLevel; const FileBuffers: BG = False);
	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
	uParams,
	uOutputFormat, uEscape, uStrings, uProjectInfo,
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

constructor TLog.Create(const FileName: TFileName; const LogLevel: TMsgType = DefaultLoggingLevel; const DirectWrite: BG = True);
var
	NewFileName: TFileName;
	Instance: SG;
begin
	inherited Create;
	FLoggingLevel := LogLevel;
	FFileName := FileName;
	FDirectWrite := DirectWrite;

	FFile := TFile.Create;
	for Instance := 1 to 9 do
	begin
		if FFile.Open(FFileName, fmAppend) then Break;
		FFileName := DelFileExt(FFileName) +  IntToStr(Instance) + ExtractFileExt(FFileName);
		if Instance = 9 then Exit;
	end;

	if FFile.FileSize > 4 * MB then
	begin
		FFile.Close;
		NewFileName := DelFileExt(FFileName) + '_' + DateToS(FileTimeToDateTime(GetFileModified(FFileName)), ofIO) + ExtractFileExt(FFileName);
		if FileExists(NewFileName) = False then
		begin
			RenameFileEx(FFileName, NewFileName);
		end;
		FFile.Open(FFileName, fmAppend);
	end;

	if FFile.FileSize = 0 then
		WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine); // First line
	Add('Started Version ' + GetProjectInfo(piFileVersion), mtInformation, True);
end;

destructor TLog.Destroy;
begin
	Add('Finished', mtInformation, True);
	Flush;
	if Assigned(FFile) then
	begin
		FFile.Close;
		FreeAndNil(FFile);
	end;
	FFileName := '';
	inherited;
end;

procedure TLog.Add(const Line: string; const LogType: TMsgType = DefaultLoggingLevel; const FileBuffers: BG = False);
begin
	Assert(LogType <> mtNone);
	if LogType >= FLoggingLevel then
	begin
		WriteLine(DateTimeToS(Now, 3, ofIO) + CharTab + FirstChar(MsgTypeStr[LogType]) + CharTab + AddEscape(Line) + FileSep, FileBuffers);
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

function GetLoggingLevel(const s: string): TMsgType;
var Index: SG;
begin
	Result := DefaultLoggingLevel;
	if s = '' then Exit;
	if (s[1] = '0') then
		Result := mtNone
	else
	begin
		Index := StrIndex(CapitalCase(s), MsgTypeStr);
		if (Index >= SG(Low(Result))) and (Index <= SG(High(Result))) then
			Result := TMsgType(Index);
	end;
end;

procedure MainLogAdd(const Line: string; const LogType: TMsgType = DefaultLoggingLevel; const FileBuffers: BG = False);
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
	OldFileName := WorkDir + GetProjectInfo(piInternalName) + '.log';
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
				RenameFileEx(OldFileName, MainLogFileName);
			end;
		end;
	end;

	s := '';
	for i := 0 to Length(MsgTypeStr) - 1 do
	begin
		s := s + MsgTypeStr[TMsgType(i)] + '|';
	end;
	s := DelLastChar(s);

	MainLog := TLog.Create(MainLogFileName); //, GetLoggingLevel(GetParamValue('LOG')), True);

	RegisterParam('Log', 'Logging level: Log[' + s + ']', SetLoggingLevel);
end;

initialization

finalization
	FreeAndNil(MainLog);
end.
