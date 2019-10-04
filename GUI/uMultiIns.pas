unit uMultiIns;

interface

uses
	uTypes,
	SysUtils;

{
	Return false if application instance already exists.
}
function InitInstance(const AllowMultiInstance: BG): BG;

implementation

uses
  Winapi.Windows,
  Forms,

  uMsg,
  uStrings,
  uFiles,
  uDForm;

var
	MessageId: UINT;
	WProc: TFNWndProc;
	MutHandle: THandle;

function NewWndProc(Handle: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
	CmdLine: string;
begin
	if (Msg = MessageID) then
	begin
		Result := 0;
		if IsIconic(Handle) = False then
		begin
      if Assigned(Application) and Application.ShowMainForm then
  			ActivateForm(Application.MainForm);
		end;

		SetLength(CmdLine, MAX_PATH);
		SetLength(CmdLine, GlobalGetAtomName(wParam, PChar(CmdLine), MAX_PATH));
		if Length(CmdLine) = 0 then
			ErrorMsg(GetLastError)
		else
		begin
			// ReadCommandLine(CmdLine); TODO :
			GlobalDeleteAtom(wParam);
		end;
	end
	else
		Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam); // pass message on to old window proc
end;

procedure BroadcastFocusMessage(wParam, lParam: LongInt);
var
	BSMRecipients: DWORD;
begin
	// Post message and inform other instance to focus itself
	BSMRecipients := BSM_APPLICATIONS;
	BroadCastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
		PDWORD(@BSMRecipients), MessageID, wParam, lParam);
end;

function InitInstance(const AllowMultiInstance: BG): BG;
var
	Atom: TAtom;
	UniqueAppStr: string;
begin
	Result := True;
	UniqueAppStr := DelCharsF(ShortToLongPath(ParamStr(0)), PathDelim);
	MessageID := RegisterWindowMessage(PChar(UniqueAppStr));
	MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(UniqueAppStr));
	if MutHandle = 0 then
	begin
		{
			Mutex object has not yet been created, meaning that no previous instance has been created.
			We subclass Application window procedure so that Application.OnMessage remains available for user.
		}
		WProc := TFNWndProc(SetWindowLong(Application.Handle, GWL_WNDPROC, Integer(@NewWndProc)));
		{ Set appropriate error flag if error condition occurred }
		if WProc = nil then
			ErrorMsg(GetLastError);

		MutHandle := CreateMutex(nil, False, PChar(UniqueAppStr));
		if MutHandle = 0 then
			ErrorMsg(GetLastError);
	end
	else
	begin
    if not AllowMultiInstance then
		begin
			Atom := GlobalAddAtom(PChar(GetCommandLine));
			BroadcastFocusMessage(Atom, 0);
			Result := False;
		end;
	end;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
	if WProc <> nil then
	begin
		SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(WProc));
		WProc := nil;
	end;
	if MutHandle <> 0 then
	begin
		CloseHandle(MutHandle);
		MutHandle := 0;
	end;
{$ENDIF NoFinalization}
end.
