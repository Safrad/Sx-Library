unit uMainLog;

interface

uses
  SysUtils,

  uFileLogger;

type
  /// <summary>Main Log File</summary>
  TMainLog = class(TFileLogger)
  public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;
  end;

procedure InitializeMainLog;

var
	MainLog: TMainLog;


implementation

uses
  uTypes,
  uOperatingSystem,
  uFiles,
  uProjectInfo;

procedure InitializeMainLog;
var
  MainLogFileName: TFileName;
begin
	MainLogFileName := LocalAppDataDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
	CreateDirsEx(ExtractFilePath(MainLogFileName));
	MainLog := TMainLog.Create(MainLogFileName);
end;

{ TMainLog }

constructor TMainLog.Create(const FileName: TFileName);
begin
  inherited;

	Add('Started Version ' + GetProjectInfo(piProductVersion), mlInformation);
  if IsLoggerFor(mlDebug) then
    Add('Operating System Version: ' + OperatingSystem.VersionAsString, mlDebug);
end;

destructor TMainLog.Destroy;
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
