// See "powercfg /requests"

unit uWindowsPowerRequest;

interface

uses
  uTypes,
  uCustomPowerRequest,

  Windows;

function PowerCreateRequest(_Context: PReasonContext): THandle; stdcall; external kernel32;
function PowerSetRequest(_Handle: THandle; _RequestType: Integer): LongBool; stdcall; external kernel32;
function PowerClearRequest(_Handle: THandle; _RequestType: Integer): LongBool; stdcall; external kernel32;


type
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
  SysUtils;

{ TWindowsPowerRequest }

constructor TWindowsPowerRequest.Create;
begin
  inherited;

  FHandle := INVALID_HANDLE_VALUE;
end;

procedure TWindowsPowerRequest.Decrement;
begin
  if FCount <= 0 then
    raise Exception.Create('Can not decrement request which hasn''t been incremented.');

  Dec(FCount);
  if not PowerClearRequest(FHandle, GetRequestType) then
    RaiseLastOSError;
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
  if (FCount = 0) and (FHandle = INVALID_HANDLE_VALUE) then
  begin
    Initialize;
  end;

  Inc(FCount);
  if not PowerSetRequest(FHandle, GetRequestType) then
    RaiseLastOSError;
end;

procedure TWindowsPowerRequest.Initialize;
const
  POWER_REQUEST_CONTEXT_VERSION = 0;
  POWER_REQUEST_CONTEXT_SIMPLE_STRING = 1;
  POWER_REQUEST_CONTEXT_DETAILED_STRING = 2;
begin
  FillChar(FReasonContext, SizeOf(FReasonContext), 0);

  FReasonContext.Version := POWER_REQUEST_CONTEXT_VERSION;
  FReasonContext.Flags := POWER_REQUEST_CONTEXT_SIMPLE_STRING;
  FReasonContext.SimpleReasonString := PWideChar(FTitle);

  FHandle := PowerCreateRequest(@FReasonContext);
  if FHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
end;

end.
