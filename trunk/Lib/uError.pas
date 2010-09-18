// Build: 12/1999-04/2000 Author: Safranek David

unit uError;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDBitBtn, ComCtrls, uDPanel, uDLabel, uWave, uAdd;

type
	TfIOError = class(TForm)
		ButtonRetry: TDBitBtn;
		ButtonIgnore: TDBitBtn;
		ButtonIgnoreAll: TDBitBtn;
		ButtonClose: TDBitBtn;
		Image1: TImage;
		ButtonFile: TDBitBtn;
		OpenDialogFile: TOpenDialog;
		MemoMsg: TMemo;
		UpDown1: TUpDown;
		Label1: TDLabel;
		PanelIndex: TDPanel;
		PanelCount: TDPanel;
		ImageBackground: TImage;
		procedure ButtonFileClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
			NewValue: SmallInt; Direction: TUpDownDirection);
		procedure ButtonCloseClick(Sender: TObject);
	private
		{ Private declarations }
		ErrorFileName: TFileName;
	public
		{ Public declarations }
	end;

var
	IgnoreAll: Boolean;
	SndError: PWave;

function ErrorMes(const ErrorCode: U32): string;

// IO Error
procedure IOError(FName: TFileName; const ErrorCode: U32);
function IOErrorRetry(var FName: TFileName; const ErrorCode: U32): Boolean;
// File Error
procedure IOErrorMessage(FName: TFileName; const ErrorMsg: string);
function IOErrorMessageRetry(var FName: TFileName; const ErrorMsg: string): Boolean;
// Internal Error
procedure ErrorMessage(ErrorMsg: string);
function ErrorMessageRetry(ErrorMsg: string): Boolean;

implementation

{$R *.DFM}
uses
	uGraph, uGraph24, uTexture,
	Registry, MMSystem;
var
	fIOError: TfIOError;
	IOErrorMessages: TStrings;
type
	TStyle = (stIO, stFile, stInternal);

function ErrorMes(const ErrorCode: U32): string;
begin
	case ErrorCode of
	000, $ffffffff: Result := 'No error';
	001: Result := 'Invalid function number';
	002: Result := 'File not found';
	003: Result := 'Path not found';
	004: Result := 'Too many open files'; // (no handles available)
	005: Result := 'File access denied'; // Access denied
	006: Result := 'Invalid file handle'; // Invalid file descriptor
	007: Result := 'Memory control block destroyed';
	008: Result := 'Out of memory';
	009: Result := 'Memory block address invalid';
	010: Result := 'Invalid environment'; // (usually >32K in length)
	011: Result := 'Invalid format';
	012: Result := 'Invalid file access code';
	013: Result := 'Data invalid';
	014: Result := 'Reserved';
	015: Result := 'Invalid drive number';
	016: Result := 'Cannot remove current directory'; // attempted to remove current directory
	017: Result := 'Cannot rename across drives'; // not same device
	018: Result := 'No more files';
	019: Result := 'Disk write-protected';
	020: Result := 'Unknown unit';
	021: Result := 'Disk not ready';
	022: Result := 'Unknown command';
	023: Result := 'Data error (CRC)';
	024: Result := 'Bad request structure length';
	025: Result := 'Seek error';
	026: Result := 'Unknown media type (non-DOS disk)';
	027: Result := 'Sector not found';
	028: Result := 'Printer out of paper';
	029: Result := 'Write fault';
	030: Result := 'Read fault';
	031: Result := 'General failure';
	032: Result := 'Sharing violation';
	033: Result := 'Lock violation';
	034: Result := 'Disk change invalid (ES:DI -> media ID structure)(see #0981)';
	035: Result := 'FCB unavailable';
	036: Result := 'Sharing buffer overflow';
	037: Result := 'Code page mismatch';
	038: Result := 'Cannot complete file operation (out of input)';
	039: Result := 'Insufficient disk space';
	040..049: Result := 'Reserved';
	050: Result := 'Network request not supported';
	051: Result := 'Remote computer not listening';
	052: Result := 'Duplicate name on network';
	053: Result := 'Network name not found';
	054: Result := 'Network busy';
	055: Result := 'Network device no longer exists';
	056: Result := 'Network BIOS command limit exceeded';
	057: Result := 'Network adapter hardware error';
	058: Result := 'Incorrect response from network';
	059: Result := 'Unexpected network error';
	060: Result := 'Incompatible remote adapter';
	061: Result := 'Print queue full';
	062: Result := 'Queue not full';
	063: Result := 'Not enough space to print file';
	064: Result := 'Network name was deleted';
	065: Result := 'Network: Access denied';
	066: Result := 'Network device type incorrect';
	067: Result := 'Network name not found';
	068: Result := 'Network name limit exceeded';
	069: Result := 'Network BIOS session limit exceeded';
	070: Result := 'Temporarily paused';
	071: Result := 'Network request not accepted';
	072: Result := 'Network print/disk redirection paused';
	073: Result := 'Network software not installed'; // (LANtastic) invalid network version
	074: Result := 'Unexpected adapter close'; // (LANtastic) account expired
	075: Result := '(LANtastic) password expired';
	076: Result := '(LANtastic) login attempt invalid at this time';
	077: Result := '(LANtastic v3+) disk limit exceeded on network node';
	078: Result := '(LANtastic v3+) not logged in to network node';
	079: Result := 'Reserved';
	080: Result := 'File exists';
	081: Result := 'Reserved';
	082: Result := 'Cannot make directory';
	083: Result := 'Fail on INT 24h';
	084: Result := 'Too many redirections';
	085: Result := 'Duplicate redirection';
	086: Result := 'Invalid password';
	087: Result := 'Invalid parameter';
	088: Result := 'Network write fault';
	089: Result := 'Function not supported on network';
	090: Result := 'Required system component not installed';

	100: Result := 'Disk read error';
	101: Result := 'Disk write error';
	102: Result := 'File not assigned';
	103: Result := 'File not open';
	104: Result := 'File not open for input';
	105: Result := 'File not open for output';
	106: Result := 'Invalid numeric format';

	112: Result := 'Disk is full';

	150: Result := 'Disk is write-protected';
	151: Result := 'Bad drive request struct length';
	152: Result := 'Drive not ready'; // Disk not ready
	154: Result := 'CRC error in data';
	155: Result := 'Bad drive request structure length';
	156: Result := 'Disk seek error';
	157: Result := 'Unknown media type';
	158: Result := 'Sector not Found';
	159: Result := 'Printer out of paper';
	160: Result := 'Device write fault';
	161: Result := 'Device read fault';
	162: Result := 'Hardware failure';

	176: Result := 'Volume is not locked';
	177: Result := 'Volume is locked in drive';
	178: Result := 'Volume is not removable';
	180: Result := 'Lock count has been exceeded';
	181: Result := 'A valid eject request failed';

	183: Result := 'Rename file error';

	200: Result := 'Division by zero';
	201: Result := 'Range check error';
	202: Result := 'Stack overflow error';
	203: Result := 'Heap overflow error';
	204: Result := 'Invalid pointer operation';
	205: Result := 'Floating point overflow';
	206: Result := 'Floating point underflow';
	207: Result := 'Invalid floating point operation';
	208: Result := 'Overlay manager not installed';
	209: Result := 'Overlay file read error';
	210: Result := 'Object not initialized';
	211: Result := 'Call to abstract method';
	212: Result := 'Stream registration error';
	213: Result := 'Collection index out of range';
	214: Result := 'Collection overflow error';
	215: Result := 'Arithmetic overflow error';
	216: Result := 'Access violation'; // General Protection fault
	217: Result := 'Control-C';
	218: Result := 'Privileged instruction';
	219: Result := 'Invalid typecast';
	220: Result := 'Invalid variant typecast';
	221: Result := 'Invalid variant operation';
	222: Result := 'No variant method call dispatcher';
	223: Result := 'Cannot create variant array';
	224: Result := 'Variant does not contain array';
	225: Result := 'Variant array bounds error';
	226: Result := 'TLS initialization error';
	227: Result := 'Assertion failed';
	228: Result := 'Interface Cast Error';
	229: Result := 'Safecall error';
	else Result := 'Unknown error';
	end;
	Result := Result + ' (' + IntToStr(ErrorCode) + ')';
end;

procedure LoadSnd;
var
	Reg: TRegistry;
	Key: string;
	SndName: TFileName;
begin
	Reg := TRegistry.Create;
	Reg.RootKey := HKEY_CURRENT_USER;
	Key := 'AppEvents\Schemes\Apps\.Default\AppGPFault\.Current';
	if Reg.OpenKey(Key, False) then
	begin
		SndName := Reg.ReadString('');
		if FileExists(SndName) then
			WaveReadFromFile(SndError, SndName);
		Reg.CloseKey;
	end;
end;

function DoForm(const Style: TStyle; var FName: TFileName; const ErrorCode: U32; const ErrorMsg: string; const Retry: Boolean): Boolean;
var s: string;
begin
	Result := False;
	if IOErrorMessages = nil then IOErrorMessages := TStringList.Create;
	if Style = stIO then
		s := ErrorMes(ErrorCode)
	else
		s := ErrorMsg;
	if FName <> '' then s := s + #13 + #10 + FName;
	IOErrorMessages.Add(s);

	if IgnoreAll = False then
	begin
		if SndError = nil then LoadSnd;
		if Assigned(SndError) then
			PlaySound(PChar(SndError), 0, snd_ASync or snd_Memory);
			
		if not Assigned(fIOError) then
		begin
			fIOError := TfIOError.Create(Application.MainForm);
			FormImage(fIOError.ImageBackground);
		end;
		fIOError.ErrorFileName := FName;
		case Style of
		stIO: fIOError.Caption := 'IO Error';
		stFile: fIOError.Caption := 'File Error';
		stInternal: fIOError.Caption := 'Internal Error';
		end;

		if Style = stIO then
		begin
			fIOError.Image1.Width := fIOError.Image1.Picture.Width;
			fIOError.Image1.Height := fIOError.Image1.Picture.Height;
		end
		else
		begin
			fIOError.Image1.Width := 32;
			fIOError.Image1.Height := 32;
		end;

		fIOError.MemoMsg.Lines.Clear;
		fIOError.MemoMsg.Lines.Add(s);

		fIOError.ButtonRetry.Enabled := Retry;
		fIOError.ButtonFile.Enabled := Retry and ((Style = stIO) or (Style = stFile));
		fIOError.ButtonIgnore.Default := not Retry;
		fIOError.UpDown1.OnChangingEx := nil;
		fIOError.UpDown1.Max := IOErrorMessages.Count - 1;
		fIOError.UpDown1.Position := IOErrorMessages.Count - 1;
		fIOError.UpDown1.OnChangingEx := fIOError.UpDown1ChangingEx;
		fIOError.PanelIndex.Caption := IntToStr(IOErrorMessages.Count);
		fIOError.PanelCount.Caption := IntToStr(IOErrorMessages.Count);
		fIOError.ModalResult := mrNone;

		CorrectPos(fIOError);
		if Application.Terminated then
		begin
			fIOError.FormStyle := fsStayOnTop;
			fIOError.Show;
			repeat
				Application.HandleMessage;
			until fIOError.ModalResult <> mrNone;
			fIOError.Hide;
		end
		else
		begin
			fIOError.FormStyle := fsNormal;
			fIOError.ShowModal;
		end;

		case fIOError.ModalResult of
		mrAll: IgnoreAll := True;
//    mrAbort: TerminateProcess(GetCurrentProcess, 0); //Halt;
		mrRetry:
		begin  
			if fIOError.ErrorFileName <> '' then FName := fIOError.ErrorFileName;
			Result := True;
		end;
		end;
		if IsMultiThread then
		begin
			fIOError.Free;
			fIOError := nil;
		end;
	end;
end;

procedure IOError(FName: TFileName; const ErrorCode: U32);
begin
	DoForm(stIO, FName, ErrorCode, '', False);
end;

function IOErrorRetry(var FName: TFileName; const ErrorCode: U32): Boolean;
begin
	Result := DoForm(stIO, FName, ErrorCode, '', True);
end;

procedure IOErrorMessage(FName: TFileName; const ErrorMsg: string);
begin
	DoForm(stFile, FName, 0, ErrorMsg, False);
end;

function IOErrorMessageRetry(var FName: TFileName; const ErrorMsg: string): Boolean;
begin
	Result := DoForm(stFile, FName, 0, ErrorMsg, True);
end;

procedure ErrorMessage(ErrorMsg: string);
var FName: TFileName;
begin
	DoForm(stInternal, FName, 0, ErrorMsg, False);
end;

function ErrorMessageRetry(ErrorMsg: string): Boolean;
var FName: TFileName;
begin
	Result := DoForm(stInternal, FName, 0, ErrorMsg, True);
end;

procedure TfIOError.ButtonFileClick(Sender: TObject);
var FExt: string;
begin
	FExt := ExtractFileExt(ErrorFileName);
	if Length(FExt) > 0 then
		OpenDialogFile.Filter := '(*' + FExt + ')|*' + FExt + '|Any file (*.*)|*.*'
	else
		OpenDialogFile.Filter := 'Any file (*.*)|*.*';
	OpenDialogFile.FileName := ErrorFileName;
	if OpenDialogFile.Execute then
	begin
		ErrorFileName := OpenDialogFile.FileName;
		ModalResult := mrRetry;
	end;
end;

procedure TfIOError.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if ModalResult = mrNone then ModalResult := mrCancel;
end;

procedure TfIOError.UpDown1ChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: SmallInt;
	Direction: TUpDownDirection);
begin
	if NewValue < 0 then
	begin
		AllowChange := False;
		Exit;
	end;
	if NewValue >= IOErrorMessages.Count then
	begin
		AllowChange := False;
		Exit;
	end;
	PanelIndex.Caption := IntToStr(NewValue + 1);
	MemoMsg.Lines.Clear;
	MemoMsg.Lines.Insert(0, IOErrorMessages[NewValue]);
end;

procedure TfIOError.ButtonCloseClick(Sender: TObject);
begin
	if Assigned(Application.MainForm) and (GetAsyncKeyState(VK_SHIFT) = 0) then
		Application.MainForm.Close
	else
		TerminateProcess(GetCurrentProcess, 1); //Halt;
end;

end.
