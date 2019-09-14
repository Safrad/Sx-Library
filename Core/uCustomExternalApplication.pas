unit uCustomExternalApplication;

interface

uses
  SysUtils,

  uTypes,
  uStartupWindowState;

type
  TExitCode = U4;

  TCustomExternalApplication = class
  private
    FKeepRunning: BG;
    // Properties
    procedure SetFileName(const Value: TFileName);
    procedure SetParameters(const Value: string);
    procedure SetCurrentDirectory(const Value: string);
    procedure SetStartupWindowState(const Value: TStartupWindowState);
    procedure SetAllowOnlyOneInstance(const Value: BG);
    procedure SetKeepRunning(const Value: BG);
  protected
    // Input
    FFileName: TFileName;
    FParameters: string;
    FCurrentDirectory: string;
    FStartupWindowState: TStartupWindowState;
    FAllowOnlyOneInstance: BG;

    // Output
    FHandle: THandle;
    FErrorCode: U4;

    function GetExitCode: TExitCode;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property FileName: TFileName read FFileName write SetFileName;
    property Parameters: string read FParameters write SetParameters;
    property CurrentDirectory: string read FCurrentDirectory write SetCurrentDirectory;
    property StartupWindowState: TStartupWindowState read FStartupWindowState write SetStartupWindowState;
    property AllowOnlyOneInstance: BG read FAllowOnlyOneInstance write SetAllowOnlyOneInstance;
    property KeepRunning: BG read FKeepRunning write SetKeepRunning;

    // Process
    procedure Execute; virtual;
    procedure Terminate;

    // raise Exception
    procedure CheckErrorCode;

    // Display error code
    procedure ShowErrorCode;

    // @raise exception if Exit Code <> 0
    procedure CheckExitCode;

    // Output
    property ExitCode: TExitCode read GetExitCode;
    property ErrorCode: U4 read FErrorCode;
    property Handle: THandle read FHandle;
  end;

implementation

uses
  Windows,

  uMsg,
  uEExternalApplication,
  uEIOException;

{ TCustomExternalApplication }

procedure TCustomExternalApplication.ShowErrorCode;
begin
  if FErrorCode <> 0 then
    IOError(FFileName, FErrorCode);
end;

procedure TCustomExternalApplication.CheckErrorCode;
begin
  if FErrorCode <> 0 then
    raise EIOException.Create(FFileName, FErrorCode);
end;

procedure TCustomExternalApplication.Terminate;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if not FKeepRunning then
      TerminateProcess(FHandle, 1);
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TCustomExternalApplication.CheckExitCode;
var
  ExitCode: TExitCode;
begin
  ExitCode := GetExitCode;
  if (ExitCode <> 0) and (ExitCode <> STILL_ACTIVE) then
  begin
    raise EExternalApplication.Create(FFilename + ' ' + FParameters, ExitCode, '');
  end;
end;

constructor TCustomExternalApplication.Create;
begin
  inherited;

  FHandle := INVALID_HANDLE_VALUE;
  FStartupWindowState.WindowState := hwsNormal;
  FStartupWindowState.Active := True;
  FAllowOnlyOneInstance := True;
end;

destructor TCustomExternalApplication.Destroy;
begin
  try
    Terminate;
  finally
    inherited;
  end;
end;

procedure TCustomExternalApplication.Execute;
begin
  if FAllowOnlyOneInstance then
    Terminate;

  Assert(FFileName <> '');
  Assert(FCurrentDirectory <> '');
end;

function TCustomExternalApplication.GetExitCode: TExitCode;
begin
  GetExitCodeProcess(FHandle, Result);
end;

procedure TCustomExternalApplication.SetAllowOnlyOneInstance(const Value: BG);
begin
  FAllowOnlyOneInstance := Value;
end;

procedure TCustomExternalApplication.SetCurrentDirectory(const Value: string);
begin
  FCurrentDirectory := Value;
end;

procedure TCustomExternalApplication.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TCustomExternalApplication.SetKeepRunning(const Value: BG);
begin
  FKeepRunning := Value;
end;

procedure TCustomExternalApplication.SetParameters(const Value: string);
begin
  FParameters := Value;
end;

procedure TCustomExternalApplication.SetStartupWindowState(const Value: TStartupWindowState);
begin
  FStartupWindowState := Value;
end;

end.
