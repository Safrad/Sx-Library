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
    FMinLogFileSize: U8;
    FMaxLogFileSize: U8;
    procedure CreateFile;
    function GetLogWritableFileName(const AFileName: TFileName): TFileName;
    procedure RenameOldLogIfNeeded;
    procedure RenameOldLog;
		procedure WriteLine(const Line: string);
    procedure SetMinLogFileSize(const Value: U8);
    procedure SetMaxLogFileSize(const Value: U8);
    procedure SetFileName(const Value: TFileName);
    procedure FreeFile;
	public
		constructor Create(const AFileName: TFileName);
		destructor Destroy; override;

    // Thread safe
		procedure Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); override;
		procedure Flush;

		property FileName: TFileName read FFileName write SetFileName;
    property MinLogFileSize: U8 read FMinLogFileSize write SetMinLogFileSize;
    property MaxLogFileSize: U8 read FMaxLogFileSize write SetMaxLogFileSize;
  end;

implementation

uses
  Math,

  uMsg,
	uFiles, uCharset,
  uMainTimer,
	uOutputFormat, uEscape, uStrings;

{ TFileLogger }

constructor TFileLogger.Create(const AFileName: TFileName);
begin
	inherited Create;

	FCriticalSection := TCriticalSection.Create;

  SetFileName(AFileName);
	FDirectWrite := True;
  FMinLogFileSize := 4 * MB;
  FMaxLogFileSize := 16 * MB; // > FMinLogFileSize
end;

procedure TFileLogger.CreateFile;
const
	IdLine = ';Local Date Time	Type	Message' + FileSep;
begin
  try
    RenameOldLogIfNeeded;

    FFile := TFile.Create;
    FFile.Logger := nil;
    if not FFile.Open(FFileName, fmAppend) then Exit;

    if FFile.FileSize = 0 then
      WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine); // First line
  except
    // No code
  end;
end;

destructor TFileLogger.Destroy;
begin
  try
    FreeFile;

    FCriticalSection.Free;
  finally
  	inherited;
  end;
end;

procedure TFileLogger.Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel);
begin
  Assert(IsLoggerFor(MessageLevel));
  if FFile = nil then
    CreateFile;
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

procedure TFileLogger.FreeFile;
begin
  if Assigned(FFile) then
  begin
    Flush;
    FFile.Close;
    FreeAndNil(FFile);
  end;
end;

function TFileLogger.GetLogWritableFileName(const AFileName: TFileName): TFileName;
const
  MaxInstances = 16;
var
  Instance: SG;
  NewFileName: TFileName;
begin
  Result := '';
  NewFileName := AFileName;
  for Instance := 1 to MaxInstances do
  begin
    if IsFileWritable(NewFileName) then Break;

    if Instance = MaxInstances then
      raise Exception.Create('Can not create logger file ' + AddQuoteF(FileName));
    NewFileName := DelFileExt(FileName) + '-' + IntToStr(Instance) + ExtractFileExt(FileName);
  end;
  Result := NewFileName;
end;

procedure TFileLogger.RenameOldLog;
var
  NewFileName: string;
//	DeleteOptions: TDeleteOptions;
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

procedure TFileLogger.RenameOldLogIfNeeded;
begin
	if GetFileSizeU(FFileName) >= Min(FMinLogFileSize, FMaxLogFileSize) then
	begin
    RenameOldLog;
	end;
end;

procedure TFileLogger.SetFileName(const Value: TFileName);
var
  NewFileName: TFileName;
begin
  NewFileName := GetLogWritableFileName(Value);
  if FFileName <> NewFileName then
  begin
    FreeFile;

    FFileName := NewFileName;
    if Assigned(FFile) then
      FreeAndNil(FFile);
  end;
end;

procedure TFileLogger.SetMaxLogFileSize(const Value: U8);
begin
  FMaxLogFileSize := Value;
end;

procedure TFileLogger.SetMinLogFileSize(const Value: U8);
begin
  FMinLogFileSize := Value;
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

      if FFile.FileSize + U8(Length(Line)) > FMaxLogFileSize then
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
      if Length(FData) + Length(Line) > FMaxLogFileSize then
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
