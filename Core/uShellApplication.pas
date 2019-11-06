unit uShellApplication;

interface

uses
  uCustomExternalApplication;

type
  TShellApplication = class(TCustomExternalApplication)
  public
    constructor Create;

    procedure Execute; override;
  end;

implementation

uses
  uTypes,
  uLog,

  Winapi.Windows,
  Winapi.ShellAPI;

{ TShellApplication }

constructor TShellApplication.Create;
begin
  inherited;

  KeepRunning := True;
end;

procedure TShellApplication.Execute;
var
	lpExecInfo: TShellExecuteInfo;
begin
  inherited;

  lpExecInfo := Default(TShellExecuteInfo);

  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  if IsConsole then
    lpExecInfo.fMask := lpExecInfo.fMask or SEE_MASK_FLAG_NO_UI;
  lpExecInfo.Wnd := GetActiveWindow();
  lpExecInfo.lpVerb := 'open';
  lpExecInfo.lpParameters := PChar(FParameters);
  lpExecInfo.lpFile := PChar(FFileName);
  lpExecInfo.nShow := FStartupWindowState.ToWindowsAPIParameter;
  lpExecInfo.hProcess := INVALID_HANDLE_VALUE;
  lpExecInfo.lpDirectory := PWideChar(FCurrentDirectory);

  if LogDebug then
    MainLogAdd('ShellExecuteEx ' + FFileName + ' ' + FParameters, mlDebug);
  if ShellExecuteEx(@lpExecInfo) then
  begin
    FHandle := lpExecInfo.hProcess;
  end
  else
  begin
    FHandle := INVALID_HANDLE_VALUE;
    FErrorCode := GetLastError;
  end;
end;

end.
