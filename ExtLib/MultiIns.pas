unit MultiIns;
{
	Taken from Delphi 2 Developers Guide by Pacheco and Teixeira
	With heavy Modifications.

	Usage:
	In the Project source change to the following

	if InitInstance then
	begin
		 Application.Initialize;
		 Application.CreateForm(TFrmSelProject, FrmSelProject);
		 Application.Run;
	end;

	 That's all folks ( I hope ;()
}


interface

uses Forms, Windows, Dialogs, SysUtils,
	Messages;

const
	MI_NO_ERROR          = 0;
	MI_FAIL_SUBCLASS     = 1;
	MI_FAIL_CREATE_MUTEX = 2;

{ Query this function to determine if error occurred in startup. }
{ Value will be one or more of the MI_* error flags. }
var
	FirstInst: Boolean;

function GetMIError: Integer;
Function InitInstance : Boolean;

implementation


var
	UniqueAppStr : PChar;   {Change for every Application}
	MessageId: Integer;
	WProc: TFNWndProc = Nil;
	MutHandle: THandle = 0;
	MIError: Integer = 0;

function GetMIError: Integer;
begin
  Result := MIError;
end;

function NewWndProc(Handle: HWND; Msg: Integer; wParam,
                    lParam: Longint): Longint; StdCall;
begin
	Result := 0;
	{ If this is the registered message... }
	if Msg = MessageID then begin
		{ if main form is minimized, normalize it }
		{ set focus to application }
		if IsIconic(Application.Handle) then begin
			Application.MainForm.WindowState := wsNormal;
			ShowWindow(Application.Mainform.Handle, sw_restore);
		end;
		SetForegroundWindow(Application.MainForm.Handle);
	end
	{ Otherwise, pass message on to old window proc }
	else
		Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam);
end;

procedure SubClassApplication;
begin
	{ We subclass Application window procedure so that }
	{ Application.OnMessage remains available for user. }
	WProc := TFNWndProc(SetWindowLong(Application.Handle, GWL_WNDPROC,
																		Longint(@NewWndProc)));
  { Set appropriate error flag if error condition occurred }
  if WProc = Nil then
    MIError := MIError or MI_FAIL_SUBCLASS;
end;

procedure DoFirstInstance;
begin
	SubClassApplication;
	MutHandle := CreateMutex(Nil, False, UniqueAppStr);
	if MutHandle = 0 then
    MIError := MIError or MI_FAIL_CREATE_MUTEX;
end;

procedure BroadcastFocusMessage;
{ This is called when there is already an instance running. }
var
  BSMRecipients: DWORD;
begin
  { Don't flash main form }
  Application.ShowMainForm := False;
  { Post message and inform other instance to focus itself }
  BSMRecipients := BSM_APPLICATIONS;
  BroadCastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
                         @BSMRecipients, MessageID, 0, 0);
end;

Function InitInstance : Boolean;
begin
	MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, UniqueAppStr);
	if MutHandle = 0 then
	begin
		{ Mutex object has not yet been created, meaning that no previous }
		{ instance has been created. }
//		ShowWindow(Application.Handle, SW_ShowNormal);
//		Application.ShowMainForm:=True;
		DoFirstInstance;
		result := True;
	end
	else
	begin
		BroadcastFocusMessage;
//		SendMessage(MutHandle, WM_USER, 1, 1); // MessageId = 58998
		result := False;
	end;
	FirstInst := Result;
end;

initialization

begin
	 UniqueAppStr := PChar(Application.Exename);
	 MessageID := RegisterWindowMessage(UniqueAppStr);
	 ShowWindow(Application.Handle, SW_Hide);
	 Application.ShowMainForm:=FALSE;
end;

finalization
begin
	if WProc <> Nil then
		{ Restore old window procedure }
		SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(WProc));
end;
end.
