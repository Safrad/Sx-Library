unit uFileLogger;

{$ZEROBASEDSTRINGS OFF}

interface

uses
  SysUtils,
  SyncObjs,

  uTypes,
  uRawFile,
  uDateTimeLogger;

type
  TFileLogger = class(TDateTimeLogger)
	private
  	FCriticalSection: TCriticalSection;
		FData: string;
		FFileName: TFileName;
		FFile: TRawFile;
		FDirectWrite: BG;
    FMinLogFileSize: U8;
    FMaxLogFileSize: U8;
    procedure CreateFile;
    function GetLogWritableFileName(const AFileName: TFileName): TFileName;
    function IsFileReady: BG;
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
  uFileCharset,
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
	IdLine = ';Local Date Time' + CharTab + 'Type' + CharTab + 'Message' + FileSep;
begin
  Assert(FFile = nil);
  try
    RenameOldLogIfNeeded;

    FFile := TRawFile.Create;
    FFile.Logger := nil;
    FFile.FileName := FFileName;
    FFile.FileMode := fmReadAndWrite;
    FFile.Open;
    FFile.SeekEnd;

    if FFile.FileSize = 0 then
    begin
      FFile.BlockWrite(ByteOrderMarks[fcUTF8][1], Length(ByteOrderMarks[fcUTF8]));
      WriteLine(IdLine); // First line
    end;
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
var
  RawByteLine: RawByteString;
begin
  if not IsFileReady then
    Exit;

  FCriticalSection.Enter;
  try
    if FDirectWrite then
    begin
      FFile.FlushFileBuffers;
    end
    else
    begin
      RawByteLine := ConvertUnicodeToUTF8(FData);
      FData := '';
      FFile.BlockWrite(RawByteLine[1], Length(RawByteLine));
      FFile.FlushFileBuffers;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TFileLogger.FreeFile;
begin
  if IsFileReady then
  begin
    Flush;
    FFile.Close;
  end;
  FreeAndNil(FFile);
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
    if IsFileWritable(NewFileName) then
      Break;

    if Instance = MaxInstances then
      raise Exception.Create('Can not create logger file ' + AddQuoteF(AFileName));
    NewFileName := DelFileExt(AFileName) + '-' + IntToStr(Instance) + ExtractFileExt(AFileName);
  end;
  Result := NewFileName;
end;

function TFileLogger.IsFileReady: BG;
begin
  Result := Assigned(FFile) and FFile.Opened;
end;

procedure TFileLogger.RenameOldLog;
var
  NewFileName: string;
//	DeleteOptions: TDeleteOptions;
begin
  NewFileName := DelFileExt(FFileName) + CharSpace + '(' + ReplaceF(DateTimeToS(FileTimeToDateTime(GetFileModified(FFileName)), 0, ofIO), ':', '-') + ')' + ExtractFileExt(FFileName);
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
	RawByteLine: RawByteString;
begin
  if (Length(Line) > 0) and IsFileReady then
  begin
    if FDirectWrite then
    begin
      if FFile.FileSize + U8(Length(Line)) > FMaxLogFileSize then
      begin
        FreeFile;
        LoggingLevel := mlNone; // Speedup
        Exit;
      end;

    //	FFile.Write(Line);
      RawByteLine := ConvertUnicodeToUTF8(Line);
      FCriticalSection.Enter;
      try
        FFile.BlockWrite(RawByteLine[1], Length(RawByteLine));
      finally
        FCriticalSection.Leave;
      end;
    end
    else
    begin
      if Length(FData) + Length(Line) > FMaxLogFileSize then
      begin
        FreeFile;
        LoggingLevel := mlNone; // Speedup
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
