unit uWindowsPowerRequest;

interface

uses
  uTypes,
  uCustomPowerRequest,

  Winapi.Windows;

type
  /// Actual system Windows power requests command: "powercfg /requests"
  TWindowsPowerRequest = class(TCustomPowerRequest)
  private
    FReasonContext: TReasonContext;
    FHandle: THandle;

    procedure Initialize;
    function GetRequestType: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Increment; override;
    procedure Decrement; override;
  end;

implementation

uses
  SysUtils,

  uLog;

type
  TPowerCreateRequest = function(_Context: PReasonContext): THandle; stdcall;
  TPowerSetRequest = function(_Handle: THandle; _RequestType: Integer): LongBool; stdcall;
  TPowerClearRequest = function(_Handle: THandle; _RequestType: Integer): LongBool; stdcall;

var
  GPowerCreateRequest: TPowerCreateRequest;
  GPowerSetRequest:  TPowerSetRequest;
  GPowerClearRequest: TPowerClearRequest;

{ TWindowsPowerRequest }

constructor TWindowsPowerRequest.Create;
begin
  inherited;

  FHandle := INVALID_HANDLE_VALUE;
end;

procedure TWindowsPowerRequest.Decrement;
begin
  if LogDebug then
    MainLog.LogEnter('WindowsPowerRequest.Decrement');

  if FCount <= 0 then
    raise Exception.Create('Can not decrement request which hasn''t been incremented.');

  Dec(FCount);
  if Assigned(GPowerClearRequest) then
    if not GPowerClearRequest(FHandle, GetRequestType) then
      RaiseLastOSError;

  if LogDebug then
    MainLog.LogLeave('WindowsPowerRequest.Decrement');
end;

destructor TWindowsPowerRequest.Destroy;
begin
  try
    if FHandle <> INVALID_HANDLE_VALUE then
      CloseHandle(FHandle);
  finally
    inherited;
  end;
end;

function TWindowsPowerRequest.GetRequestType: Integer;
begin
  if FRequestType = PowerRequestExecutionRequired then
  begin
    if ((Win32MajorVersion >= 7) or ((Win32MajorVersion = 6) and (Win32MinorVersion >= 2))) { Windows 8 or newer } then
      Result := Integer(PowerRequestExecutionRequired)
    else
      Result := Integer(PowerRequestSystemRequired);
  end
  else
    Result := Integer(FRequestType);
end;

procedure TWindowsPowerRequest.Increment;
begin
  if LogDebug then
    MainLog.LogEnter('WindowsPowerRequest.Increment');

  if (FCount = 0) and (FHandle = INVALID_HANDLE_VALUE) then
  begin
    Initialize;
  end;

  Inc(FCount);
  if Assigned(GPowerSetRequest) then
    if not GPowerSetRequest(FHandle, GetRequestType) then
      RaiseLastOSError;

  if LogDebug then
    MainLog.LogLeave('WindowsPowerRequest.Increment');
end;

procedure TWindowsPowerRequest.Initialize;
const
  POWER_REQUEST_CONTEXT_VERSION = 0;
  POWER_REQUEST_CONTEXT_SIMPLE_STRING = 1;
  POWER_REQUEST_CONTEXT_DETAILED_STRING = 2;
begin
  if Assigned(GPowerCreateRequest) then
  begin
    FReasonContext.Version := POWER_REQUEST_CONTEXT_VERSION;
    FReasonContext.Flags := POWER_REQUEST_CONTEXT_SIMPLE_STRING;
    FReasonContext.SimpleReasonString := PWideChar(FTitle);

    FHandle := GPowerCreateRequest(@FReasonContext);
    if FHandle = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
  end;
end;

initialization
  GPowerCreateRequest := GetProcAddress(GetModuleHandle(kernel32), 'PowerCreateRequest');
  GPowerSetRequest := GetProcAddress(GetModuleHandle(kernel32), 'PowerSetRequest');
  GPowerClearRequest := GetProcAddress(GetModuleHandle(kernel32), 'PowerClearRequest');
end.
