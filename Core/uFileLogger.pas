unit uFileLogger;

interface

uses
  SysUtils,
  SyncObjs,

  uTypes,
  uFile,
  uDateTimeLogger;

type
  TFileLogger = class(TDateTimeLogger)
	private
  	FCriticalSection: TCriticalSection;
		FData: string;
		FFileName: TFileName;
		FFile: TFile;
		FDirectWrite: BG;
		procedure WriteLine(const Line: string);
	public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;

    // Thread safe
		procedure Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); override;
		procedure Flush;

		property FileName: TFileName read FFileName;
  end;

implementation

uses
  uMsg,
	uFiles, uCharset,
  uMainTimer,
	uOutputFormat, uEscape, uStrings;

const
  MinLogFileSize = 4 * MB;
  MaxLogFileSize = 16 * MB; // > MinLogFileSize

{ TFileLogger }

constructor TFileLogger.Create(const FileName: TFileName);

  function GetLogWritableFileName: TFileName;
  var
    Instance: SG;
    NewFileName: TFileName;
  begin
    Result := '';
    NewFileName := FileName;
    for Instance := 1 to 10 do
    begin
      if IsFileWritable(NewFileName) then Break;

      if Instance = 10 then
        raise Exception.Create('Can not create logger file ' + AddQuoteF(FileName));
      NewFileName := DelFileExt(FileName) +  IntToStr(Instance) + ExtractFileExt(FileName);
    end;
    Result := NewFileName;
  end;

const
	IdLine = ';Local Date Time	Type	Message' + FileSep;
var
	NewFileName: TFileName;
//	DeleteOptions: TDeleteOptions;
begin
	inherited Create;

	FCriticalSection := TCriticalSection.Create;

	FFileName := GetLogWritableFileName;
	FDirectWrite := True;

	if GetFileSizeU(FFileName) >= MinLogFileSize then
	begin
		NewFileName := DelFileExt(FFileName) + '_' + DateToS(FileTimeToDateTime(GetFileModified(FFileName)), ofIO) + ExtractFileExt(FFileName);
		if FileExists(NewFileName) = False then
		begin
			RenameFileEx(FFileName, NewFileName);
//      if MaxLogFiles > 0 then
//      begin
//				DeleteOptions.Mask := '*.log';
//        DeleteOptions.MaxDirs := MaxLogFiles;
//        DeleteOptions.SelectionType := stOld;
//        DeleteOptions.AcceptFiles := True;
//        DeleteOptions.Test := False;
//        DeleteOptions.DisableLog := True;
//      	SxDeleteDirs(ExtractFilePath(FFileName), DeleteOptions);
//			end;
		end;
	end;

	FFile := TFile.Create;
  FFile.Logger := nil;
  if not FFile.Open(FFileName, fmAppend) then Exit;

	if FFile.FileSize = 0 then
		WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine); // First line
end;

destructor TFileLogger.Destroy;
begin
  try
    if Assigned(FFile) then
    begin
      Flush;
      FFile.Close;
      FreeAndNil(FFile);
    end;
    FFileName := '';

    FCriticalSection.Free;
  finally
  	inherited;
  end;
end;

procedure TFileLogger.Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel);
begin
  Assert(IsLoggerFor(MessageLevel));
  WriteLine(DateTimeToS(LogTime, MainTimer.PrecisionDigits, ofIO) + CharTab + FirstChar(MessageLevelStr[MessageLevel]) + CharTab + AddEscape(Line, True) + FileSep);
end;

procedure TFileLogger.Flush;
begin
  FCriticalSection.Enter;
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
    FCriticalSection.Leave;
  end;
end;

procedure TFileLogger.WriteLine(const Line: string);
var
	LineA: AnsiString;
begin
  if Length(Line) > 0 then
  begin
    if FDirectWrite then
    begin
      if not FFile.Opened then Exit;

      if FFile.FileSize + U8(Length(Line)) > MaxLogFileSize then
      begin
        LoggingLevel := mlNone;
        Exit;
      end;

    //	FFile.Write(Line);
      LineA := ConvertUnicodeToUTF8(Line);
      FCriticalSection.Enter;
      try
        FFile.BlockWrite(LineA[1], Length(LineA));
      finally
        FCriticalSection.Leave;
      end;
    end
    else
    begin
      if Length(FData) + Length(Line) > MaxLogFileSize then
      begin
        LoggingLevel := mlNone;
        Exit;
      end;
      FCriticalSection.Enter;
      try
        FData := FData + Line;
      finally
        FCriticalSection.Leave;
      end;
    end;
  end;
end;

end.
