unit uLog deprecated 'Use uMainLog';

interface

uses
	SysUtils,

	uTypes,
  uMainLog;

type
	TLog = class(TMainLog);

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

// Backward compatibility
function MainLog: TMainLog;

implementation

procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel);
begin
	if Assigned(uMainLog.MainLog) then
  	if AMessageLevel >= MainLog.LoggingLevel then
	    MainLog.Add(ALine, AMessageLevel);
end;

function MainLogWrite(const AMessageLevel: TMessageLevel): BG;
begin
	Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= AMessageLevel);
end;

function LogFatalError: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlFatalError);
end;

function LogError: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlError);
end;

function LogWarning: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlWarning);
end;

function LogInformation: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlInformation);
end;

function LogDebug: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlDebug);
end;

function LogConfirmation: BG;
begin
  Result := Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlConfirmation);
end;

procedure LogException(const E: Exception);
begin
  if Assigned(uMainLog.MainLog) and (MainLog.LoggingLevel <= mlFatalError) then
    MainLog.LogException(E);
end;

function MainLog: TMainLog;
begin
  Result := uMainLog.MainLog;
end;

end.
