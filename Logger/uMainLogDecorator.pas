unit uMainLogDecorator;

interface

uses
  SysUtils,
  uFileLogger;

type
  TMainLogDecorator = class(TFileLogger)
  public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;
  end;

procedure InitializeMainLog;

implementation

uses
  uTypes,
  uFiles,
  uSystemPaths,
  uOperatingSystem,
  uProjectInfo,
  uMainLog;

procedure InitializeMainLog;
var
  MainLogFileName: TFileName;
begin
	MainLogFileName := SystemPaths.LocalAppDataDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
	CreateDirsEx(ExtractFilePath(MainLogFileName));
	MainLog := TMainLogDecorator.Create(MainLogFileName);
end;

{ TMainLog }

constructor TMainLogDecorator.Create(const FileName: TFileName);
begin
  inherited;

	Add('Started Version ' + GetProjectInfo(piProductVersion), mlInformation);
  if IsLoggerFor(mlDebug) then
    Add('Operating System Version: ' + OperatingSystem.VersionAsString, mlDebug);
end;

destructor TMainLogDecorator.Destroy;
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
