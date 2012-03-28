unit ExternalApp;

// ************************************************
// ** TExternalApp component for Borland Delphi **
// ** **
// ** Copyright (c) Jul 2004 Krzysztof Olczyk **
// ** **
// ** There is a permition granted to reuse ** 
// ** the code as long as this header message ** 
// ** is provided with source code ** 
// ** There is no limitiation on distribution ** 
// ** of compiled version of a program ** 
// ** using TExternalApp component ** 
// ************************************************ 

// Component allows execution of external programs 
// as new process managed by separate thread 
// Just set CommandLine property to the adequate 
// command and execute Run routine to start app 
// The stdin, stdout, stderr properties exposed 
// as a TStream objects may be used to control 
// standard streams. 

interface 

uses 
  SysUtils, Classes, Windows, Dialogs; 

type 
  EExternalApp = class(Exception); 
	TExternalApp = class;

	TExternalAppThread = class(TThread)
	private
		fExternalApp: TExternalApp;
	protected
		procedure Execute; override;
		procedure SetRunning;
		procedure SetNotRunning;
		procedure EventBreak;
	public
		fOnEventBreak: TNotifyEvent;
		fOnSynchronizedEventBreak: TNotifyEvent;
		constructor Create(AExternalApp: TExternalApp); reintroduce; virtual;
	end;

	TExternalApp = class(TComponent)
	private
		fSecurityAttributes: TSecurityAttributes;
		fStartupInfo: TStartupInfo;
		fProcessInformation: TProcessInformation;
		fStdIn: THandleStream;
		fStdOut: THandleStream;
		fStdErr: THandleStream;
		fModuleName: string;
		fCommandLine: string;
		fShowAppWindow: boolean;
		fCurrentDirectory: string;
		fInvokerThread: TExternalAppThread;
		fRunning: boolean;
		fStdInAvailable: boolean;
		fStdOutAvailable: boolean;
		fStdErrAvailable: boolean;
		fOnProcessStarted: TNotifyEvent;
		fOnProcessStopped: TNotifyEvent;
		fOnEventBreak: TNotifyEvent;
		fOnSynchronizedEventBreak: TNotifyEvent;
		procedure SetRunning(Value: boolean);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Run;
		procedure Stop;
		procedure WaitTillTerminated;
		procedure WaitForStdin;
		procedure WaitForStdout;
		function GetExitCode: Cardinal;
		property StdIn: THandleStream read fStdIn;
		property StdOut: THandleStream read fStdOut;
		property StdErr: THandleStream read fStdErr;
		property Running: boolean read fRunning write SetRunning;
//		procedure ExitCode: U4 read GetExitCode;
	published
		property ModuleName: string read fModuleName write fModuleName;
		property CommandLine: string read fCommandLine write fCommandLine;
		property ShowAppWindow: boolean read fShowAppWindow write fShowAppWindow;
		property ProcessInformation: TProcessInformation read fProcessInformation;
		property CurrentDirectory: string read fCurrentDirectory write fCurrentDirectory;
		property OnProcessStarted: TNotifyEvent read fOnProcessStarted write fOnProcessStarted;
		property OnProcessStopped: TNotifyEvent read fOnProcessStopped write fOnProcessStopped;
		property OnEventBreak: TNotifyEvent read fOnEventBreak write fOnEventBreak;
		property OnSynchronizedEventBreak: TNotifyEvent read fOnSynchronizedEventBreak write fOnSynchronizedEventBreak;
	end;

procedure Register; 

implementation 

uses Forms, Math; 

procedure Register; 
begin 
  RegisterComponents('ExternalApp', [TExternalApp]); 
end; 

procedure Check(const what: boolean);
begin 
  if not what then RaiseLastOSError; 
end; 

function IsNT: boolean;
var 
 _winVer: DWORD; 
begin 
  _winVer := GetVersion; 

  if (_winVer >= $80000000) then 
    result := false 
  else 
    result := true; 
end; 

{ TExternalApp } 

constructor TExternalApp.Create(AOwner: TComponent); 
begin 
  inherited Create(AOwner); 
  fCurrentDirectory := ExtractFilePath(Application.ExeName); 
  fRunning := false; 
  fStdInAvailable := false; 
  fStdOutAvailable := false; 
  fStdErrAvailable := false; 
  fSecurityAttributes.nLength := sizeof(fSecurityAttributes); 
  fSecurityAttributes.lpSecurityDescriptor := nil; 
  fSecurityAttributes.bInheritHandle := true; 
  fStartupInfo.cb := sizeof(fStartupInfo); 
end; 

destructor TExternalApp.Destroy; 
begin 
  if fRunning then 
	begin
		fInvokerThread.Terminate;
		fInvokerThread.WaitFor;
		fInvokerThread.Free;
	end;
  fStdIn.Free; 
  fStdOut.Free; 
  fStdErr.Free; 
  inherited; 
end; 

function TExternalApp.GetExitCode: Cardinal;
begin
	if GetExitCodeProcess(fProcessInformation.hProcess, Result) = False then
		GetLastError;
	if Result <> 259 then
		fRunning := False;
end;

procedure TExternalApp.Run;
var 
	_tmpStdOut, _tmpStdErr, _tmpStdIn: THandle;
	_StdOut, _StdErr, _StdIn: THandle;
	_stdOutH, _stdInH, _stdErrH: THandle;
	_hProcess: THandle;
begin 
  if fRunning then exit; 

  Check(CreatePipe(_stdInH, _tmpStdIn, @fSecurityAttributes, 0)); 
  Check(CreatePipe(_tmpStdOut, _stdOutH, @fSecurityAttributes, 0)); 
  Check(CreatePipe(_tmpStdErr, _stdErrH, @fSecurityAttributes, 0)); 

  _hProcess := GetCurrentProcess; 

	Check(DuplicateHandle(_hProcess, _tmpStdIn, _hProcess, @_StdIn, 0, false, DUPLICATE_SAME_ACCESS));
	Check(DuplicateHandle(_hProcess, _tmpStdOut, _hProcess, @_StdOut, 0, false, DUPLICATE_SAME_ACCESS));
	Check(DuplicateHandle(_hProcess, _tmpStdErr, _hProcess, @_StdErr, 0, false, DUPLICATE_SAME_ACCESS));

  Check(CloseHandle(_tmpStdIn)); 
  Check(CloseHandle(_tmpStdOut)); 
  Check(CloseHandle(_tmpStdErr)); 

  fStdIn := THandleStream.Create(_StdIn); 
  fStdOut := THandleStream.Create(_StdOut); 
  fStdErr := THandleStream.Create(_StdErr); 

	fStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;

	fStartupInfo.hStdOutput := _stdOutH;
  fStartupInfo.hStdInput := _stdInH; 
	fStartupInfo.hStdError := _stdErrH;

  if fShowAppWindow then 
    fStartupInfo.wShowWindow := SW_SHOW 
  else 
    fStartupInfo.wShowWindow := SW_HIDE; 

//	fRunning := true;
	fInvokerThread := TExternalAppThread.Create(self);
	fInvokerThread.fOnEventBreak := fOnEventBreak; 
end; 

procedure TExternalApp.SetRunning(Value: boolean); 
begin 
  if Value then 
    Run 
  else 
    Stop; 
end; 

procedure TExternalApp.Stop; 
begin 
  if not fRunning then exit; 
  fInvokerThread.Terminate; 
end; 

procedure TExternalApp.WaitForStdin; 
begin 
  WaitForSingleObject(fStdIn.Handle, INFINITE); 
end; 

procedure TExternalApp.WaitForStdout; 
begin 
  WaitForSingleObject(fStdOut.Handle, INFINITE); 
end; 

procedure TExternalApp.WaitTillTerminated;
begin 
  fInvokerThread.WaitFor; 
end; 

{ TExternalAppThread } 

constructor TExternalAppThread.Create(AExternalApp: TExternalApp); 
begin 
  inherited Create(false); 
  fExternalApp := AExternalApp; 
end; 

procedure TExternalAppThread.EventBreak;
begin
	fOnEventBreak(Self);
end;

procedure TExternalAppThread.Execute;
var 
  _sd: pointer; 
  _sa: PSecurityAttributes; 
  _res: boolean; 
  _pmodulename, _pcommandline: pchar; 
begin 
	if IsNT then
	begin
		_sd := pointer(GlobalAlloc(GPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
		Check(InitializeSecurityDescriptor(_sd, SECURITY_DESCRIPTOR_REVISION));
		Check(SetSecurityDescriptorDacl(_sd, true, nil, false));
		_sa := PSecurityAttributes(GlobalAlloc(GPTR, sizeof(TSecurityAttributes)));
		_sa.nLength := sizeof(TSecurityAttributes);
		_sa.lpSecurityDescriptor := _sd;
		_sa.bInheritHandle := true;
	end
	else
	begin
		_sd := nil;
		_sa := nil;
	end;

  if fExternalApp.fModuleName <> '' then 
    _pmodulename := pchar(fExternalApp.fModuleName) 
  else 
    _pmodulename := nil; 

  if fExternalApp.fCommandLine <> '' then 
    _pcommandline := pchar(fExternalApp.fCommandLine) 
  else 
		_pcommandline := nil;

	_res := CreateProcess(_pmodulename, _pcommandline, _sa, nil, true, CREATE_NEW_CONSOLE or IDLE_PRIORITY_CLASS, nil, pchar(fExternalApp.fCurrentDirectory), fExternalApp.fStartupInfo, fExternalApp.fProcessInformation);

  if (_sa <> nil) then 
	begin
		GlobalFree(dword(_sa));
		GlobalFree(dword(_sd));
	end;

	if (not _res) then
	begin
		fExternalApp.fStdIn.Free;
		fExternalApp.fStdOut.Free;
		fExternalApp.fStdErr.Free;
//		RaiseLastOSError;
		exit;
	end;

	Synchronize(SetRunning);

	while WaitForSingleObject(fExternalApp.fProcessInformation.hProcess, 40) = WAIT_TIMEOUT do
	begin
		if Terminated then break;
		if Assigned(fOnEventBreak) then
			EventBreak;
{		if Assigned(fOnSynchronizedEventBreak) then
			Synchronize(SynchronizedEventBreak);}
	end;

	Synchronize(SetNotRunning);
end;

procedure TExternalAppThread.SetNotRunning;
begin
	fExternalApp.fRunning := false;
	if Assigned(fExternalApp.fOnProcessStopped) then
		fExternalApp.fOnProcessStopped(fExternalApp);
end;

procedure TExternalAppThread.SetRunning;
begin
	fExternalApp.fRunning := true;
	if Assigned(fExternalApp.fOnProcessStarted) then
		fExternalApp.fOnProcessStarted(fExternalApp);
end;

end.