unit uLog;

interface

uses
	uTypes, uFile, uMsg, uLogger,
	SysUtils;

type
	TLog = class(TLogger)
	private
		FData: string;
		FFileName: TFileName;
		FFile: TFile;
		FDirectWrite: BG;
		FLoggingLevel: TMessageLevel;
		procedure WriteLine(const Line: string);
	public
		property FileName: TFileName read FFileName;
		constructor Create(const FileName: TFileName; const LogLevel: TMessageLevel = DefaultLoggingLevel; const DirectWrite: BG = True);
		destructor Destroy; override;
		procedure Add(const Line: string; const MessageLevel: TMessageLevel); override;
		procedure Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); override;
		procedure Flush;
		property LoggingLevel: TMessageLevel read FLoggingLevel write FLoggingLevel default DefaultLoggingLevel;
	end;

	TLogMessageProcedure = procedure(const Line: string; const LogType: TMessageLevel);

	// Add startup parameter -LogDebug for all debuging information
	function MainLogWrite(const MessageLevel: TMessageLevel): BG;
	procedure MainLogAdd(const Line: string; const LogType: TMessageLevel);
	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
	uParams, uFiles, uCharset,
	uOutputFormat, uEscape, uStrings, {$ifndef Console}uProjectInfo,{$endif}
	Windows, TypInfo;

procedure TLog.WriteLine(const Line: string);
var
	LLog: TLog;
	LineA: AnsiString;
begin
	if Length(Line) > 0 then
	begin
		if FDirectWrite then
		begin
			if not FFile.Opened then Exit;
			LLog := MainLog;
			MainLog := nil;
			try
			//	FFile.Write(Line);
				LineA := ConvertUnicodeToUTF8(Line);
				FFile.BlockWrite(LineA[1], Length(LineA));
			finally
				MainLog := LLog;
			end;
		end
		else
		begin
			FData := FData + Line;
		end;
	end;
end;

const
	IdLine = ';Local Date Time	Type	Message' + FileSep;

constructor TLog.Create(const FileName: TFileName; const LogLevel: TMessageLevel = DefaultLoggingLevel; const DirectWrite: BG = True);
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
	Add('Started'{$ifndef Console} + ' Version ' + GetProjectInfo(piFileVersion){$endif}, mlInformation);
end;

destructor TLog.Destroy;
begin
	Add('Finished', mlInformation);
	if Assigned(FFile) then
	begin
		Flush;
		FFile.Close;
		FreeAndNil(FFile);
	end;
	FFileName := '';
	inherited;
end;

procedure TLog.Add(const Line: string; const MessageLevel: TMessageLevel);
begin
	Add(Now, Line, MessageLevel);
end;

procedure TLog.Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel);
begin
	Assert(MessageLevel <> mlNone);
	if MessageLevel >= FLoggingLevel then
	begin
		WriteLine(DateTimeToS(LogTime, 3, ofIO) + CharTab + FirstChar(MessageLevelStr[MessageLevel]) + CharTab + AddEscape(Line, True) + FileSep);
//		Flush;
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
			FFile.Write(FData);
			FData := '';
			FFile.FlushFileBuffers;
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

function GetLoggingLevel(const s: string): TMessageLevel;
var Index: SG;
begin
	Result := DefaultLoggingLevel;
	if s = '' then Exit;
	if (s[1] = '0') then
		Result := mlNone
	else
	begin
		Index := StrIndex(CapitalCase(s), MessageLevelStr);
		if (Index >= SG(Low(Result))) and (Index <= SG(High(Result))) then
			Result := TMessageLevel(Index);
	end;
end;

function MainLogWrite(const MessageLevel: TMessageLevel): BG;
begin
	Result := Assigned(MainLog) and (MessageLevel >= MainLog.LoggingLevel);
end;

procedure MainLogAdd(const Line: string; const LogType: TMessageLevel);
begin
	if Assigned(MainLog) then
		MainLog.Add(Line, LogType);
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
	OldFileName := WorkDir + {$ifndef Console}GetProjectInfo(piInternalName){$else}''{$endif} + '.log';
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
	for i := 0 to Length(MessageLevelStr) - 1 do
	begin
		s := s + MessageLevelStr[TMessageLevel(i)] + '|';
	end;
	s := DelLastChar(s);

	MainLog := TLog.Create(MainLogFileName); //, GetLoggingLevel(GetParamValue('LOG')), True);

	RegisterParam('Log', 'Logging level: Log[' + s + ']', SetLoggingLevel);
end;

initialization

finalization
	FreeAndNil(MainLog);
end.
