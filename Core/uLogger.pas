unit uLogger;

interface

uses
  SysUtils,
  uTypes;

type
	PLogMessage = ^TLogMessage;
	TLogMessage = record
		DateTime: TDateTime;
		Level: TMessageLevel;
		Text: string;
	end;

	TLogger = class
  private
		FLoggingLevel: TMessageLevel;
    procedure UpdateLoggingLevelFromEnvironmentVariable;
    procedure SetLoggingLevelInternal(const Value: TMessageLevel);
	public
    constructor Create;

    procedure Add(const Line: string; const MessageLevel: TMessageLevel); overload; virtual; abstract;
		procedure Add(const MessageDateTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); overload; virtual; abstract;

    function FatalError: BG; deprecated 'Use IsLoggerFor';
    function Error: BG; deprecated 'Use IsLoggerFor';
    function Warning: BG; deprecated 'Use IsLoggerFor';
    function Information: BG; deprecated 'Use IsLoggerFor';
    function Debug: BG; deprecated 'Use IsLoggerFor';
    function Confirmation: BG; deprecated 'Use IsLoggerFor';
    function IsLoggerFor(const AMessageLevel: TMessageLevel): BG;

    procedure LogEnter(const AName: string);
    procedure LogLeave(const AName: string);
    procedure LogException(const E: Exception);

		property LoggingLevel: TMessageLevel read FLoggingLevel write SetLoggingLevelInternal;

    procedure SetLoggingLevel(const Value: string);
	end;

function GetLoggingLevel(const s: string): TMessageLevel;

implementation

uses
  uStrings,
  uMsg;

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
	Result := mlDebug;
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

{ TLogger }

function TLogger.FatalError: BG;
begin
  Result := False;
  if mlFatalError >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLogger.Error: BG;
begin
  Result := False;
  if mlError >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLogger.Warning: BG;
begin
  Result := False;
  if mlWarning >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLogger.Information: BG;
begin
  Result := False;
  if mlInformation >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLogger.Debug: BG;
begin
  Result := False;
  if mlDebug >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLogger.Confirmation: BG;
begin
  Result := False;
  if mlConfirmation >= FLoggingLevel then
  begin
    Result := True;
  end;
end;

constructor TLogger.Create;
begin
  inherited;

  if IsDebug then
    FLoggingLevel := mlDebug
  else
    FLoggingLevel := mlInformation;
  UpdateLoggingLevelFromEnvironmentVariable;
end;

function TLogger.IsLoggerFor(const AMessageLevel: TMessageLevel): BG;
begin
  Result := Assigned(Self) and (AMessageLevel >= FLoggingLevel);
end;

procedure TLogger.LogEnter(const AName: string);
const
  EnterPrefix = '>';
begin
  Add(EnterPrefix + AName, mlDebug);
end;

procedure TLogger.LogLeave(const AName: string);
const
  LeavePrefix = '<';
begin
  Add(LeavePrefix + AName, mlDebug);
end;

procedure TLogger.SetLoggingLevel(const Value: string);
begin
	FLoggingLevel := GetLoggingLevel(Value);
end;

procedure TLogger.SetLoggingLevelInternal(const Value: TMessageLevel);
begin
  FLoggingLevel := Value;
end;

procedure TLogger.UpdateLoggingLevelFromEnvironmentVariable;
var
  Value: string;
begin
  Value := GetEnvironmentVariable('LoggingLevel');
  if Value <> '' then
    LoggingLevel := GetLoggingLevel(Value);
end;

procedure TLogger.LogException(const E: Exception);
begin
  Add(E.Message + ' (' + E.ClassName + ')', mlFatalError);
end;

end.


