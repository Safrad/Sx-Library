unit MultiIns;

interface

uses
	Forms, Windows, Dialogs, SysUtils,
	Messages;

var
	FirstInst: Boolean;
	MessageId: UINT;

procedure BroadcastFocusMessage(wParam, lParam: LongInt);
function InitInstance: Boolean;

implementation

uses uError, uStrings;

var
	WProc: TFNWndProc;
	MutHandle: THandle;

function NewWndProc(Handle: HWND; Msg: UINT; wParam, lParam: LongInt): LRESULT; StdCall;
begin
	Result := 0;
	{ If this is the registered message... }
	if (Msg = MessageID) and (wParam = 0) then
	begin
		Application.MainForm.Hide;
		Application.MainForm.Show;
//		SetForegroundWindow(Application.MainForm.Handle);
		{ if main form is minimized, normalize it }
		{ set focus to application }
{		if IsIconic(Application.MainForm.Handle) then
		begin
			Application.MainForm.WindowState := wsNormal;
			ShowWindow(Application.MainForm.Handle, SW_RESTORE);
		end;}
{		if Application.MainForm.WindowState = wsMinimized then
		begin
			Application.MainForm.WindowState := wsNormal;
			ShowWindow(Application.Mainform.Handle, SW_RESTORE);
		end;}
	end
	{ Otherwise, pass message on to old window proc }
	else
		Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam);
end;

procedure BroadcastFocusMessage(wParam, lParam: LongInt);
var
	BSMRecipients: DWORD;
begin
	{ Post message and inform other instance to focus itself }
	BSMRecipients := BSM_APPLICATIONS;
	BroadCastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
		@BSMRecipients, MessageID, wParam, lParam);
end;

function InitInstance: Boolean;
var
	UniqueAppStr: string;
begin
	UniqueAppStr := DelCharsF(Application.ExeName, '\');
	MessageID := RegisterWindowMessage(PChar(UniqueAppStr));
	MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(UniqueAppStr));
	if MutHandle = 0 then
	begin
		{ Mutex object has not yet been created, meaning that no previous }
		{ instance has been created. }
		{ We subclass Application window procedure so that }
		{ Application.OnMessage remains available for user. }
		WProc := TFNWndProc(SetWindowLong(Application.Handle, GWL_WNDPROC,
			LongInt(@NewWndProc)));
		{ Set appropriate error flag if error condition occurred }
		if WProc = nil then
		begin
			ErrorMessage(ErrorMes(GetLastError));
		end;
		
		MutHandle := CreateMutex(nil, False, PChar(UniqueAppStr));
		if MutHandle = 0 then
		begin
			ErrorMessage(ErrorMes(GetLastError));
		end;
		Result := True;
	end
	else
	begin
		Result := False;
	end;
	FirstInst := Result;
end;

initialization

finalization
	if WProc <> nil then
	begin
		{ Restore old window procedure }
		SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(WProc));
		WProc := nil;
	end;
	if MutHandle <> 0 then
	begin
		CloseHandle(MutHandle);
		MutHandle := 0;
	end;
end.
