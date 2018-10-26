// Main file logging

unit uLog;

interface

uses
	uTypes, uFile, uMsg, uFileLogger,
	SysUtils;

type
	TLog = class(TFileLogger)
	public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;
	end;

  // Optimization purposes only
	procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel);
	function MainLogWrite(const AMessageLevel: TMessageLevel): BG;

  function LogFatalError: BG;
  function LogError: BG;
  function LogWarning: BG;
  function LogInformation: BG;
  function LogDebug: BG;
  function LogConfirmation: BG;

  procedure LogException(const E: Exception);

	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
  uOperatingSystem,
	uFiles,
	uProjectInfo;

var
  InitializingLog: BG; // or DeinitializingLog

procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel);
begin
  if InitializingLog then Exit;

	if Assigned(MainLog) then
  	if AMessageLevel >= MainLog.LoggingLevel then
	    MainLog.Add(ALine, AMessageLevel);
end;

function MainLogWrite(const AMessageLevel: TMessageLevel): BG;
begin
	Result := Assigned(MainLog) and (MainLog.LoggingLevel <= AMessageLevel);
end;

function LogFatalError: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlFatalError);
end;

function LogError: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlError);
end;

function LogWarning: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlWarning);
end;

function LogInformation: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlInformation);
end;

function LogDebug: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlDebug);
end;

function LogConfirmation: BG;
begin
  Result := Assigned(MainLog) and (MainLog.LoggingLevel <= mlConfirmation);
end;

procedure LogException(const E: Exception);
begin
  if Assigned(MainLog) and (MainLog.LoggingLevel <= mlFatalError) then
    MainLog.LogException(E);
end;

procedure InitializeLog;
begin
  InitializingLog := True;

	CreateDirEx(ExtractFilePath(MainLogFileName));
	MainLog := TLog.Create(MainLogFileName);
  InitializingLog := False;
end;

function ApplicationPlatform: string;
begin
  {$ifdef CPUx64}
  Result := 'x64';
  {$else}
  Result := 'x86';
  {$endif}
end;

{ TLog }

constructor TLog.Create(const FileName: TFileName);
begin
  inherited;

	Add('Started Version ' + GetProjectInfo(piFileVersion) + ' ' + ApplicationPlatform, mlInformation);
  if IsLoggerFor(mlDebug) then
    Add('Operating System Version: ' + OperatingSystem.VersionAsString, mlDebug);
end;

destructor TLog.Destroy;
begin
  try
  	Add('Finished', mlInformation);
  finally
    inherited;
  end;
end;

initialization

finalization
{$IFNDEF NoFinalization}
	FreeAndNil(MainLog);
{$ENDIF NoFinalization}
end.
