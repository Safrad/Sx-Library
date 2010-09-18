//* File:     Lib\uMidi.pas
//* Created:  2000-01-01
//* Modified: 2004-08-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMidi;

interface

uses uAdd, MMSystem, SysUtils, Windows;

function MidiMCICallBack: Boolean;
procedure MidiMCIOpen(FileName: TFileName);
procedure MidiMCISeek(const SeekTo: U4);
function MidiMCIGetPos: U4;
procedure MidiMCIPause;
procedure MidiMCIStop;
procedure MidiMCIResume;
procedure MidiMCIPlay;
procedure MidiMCIClose;
function MCIError(const ErrorCode: U4): string;

{
procedure MidiReadFromFile(var Midi: PMidi; FName: TFileName);
procedure MidiWriteToFile(var Midi: PMidi; FName: TFileName);

procedure MidiOpen;
procedure MidiPrepareHeader(var Header: PMidiHdr; Midi: PMidi);
procedure MidiUnprepareHeader(var Header: PMidiHdr);
procedure MidiPlay(Header: PMidiHdr);
procedure MidiClose;
}

var
	MidiHandle: HWnd;
	MidiPlaying: Boolean;
	MidiOpened: Boolean;
{ HW: PHMidiOUT;
	kd: SG;}

implementation

uses
	uError;
var
	OpenParm: TMCI_Open_Parms;
	PlayParm: TMCI_Play_Parms;
	GenParm: TMCI_Generic_Parms;
	SeekParm: TMCI_Seek_Parms;

function MCIError(const ErrorCode: U4): string;
begin
{	case ErrorCode of
	000: Result := 'The specified command was carried out.';
	001: Result := 'Undefined external error.';
	002: Result := 'A device ID has been used that is out of range for your system.';
	003: Result := 'The driver was not enabled.';
	004: Result := 'The specified device is already in use. Wait until it is free, and then try again.';
	005: Result := 'The specified device handle is invalid.';
	006: Result := 'There is no driver installed on your system.';
	007: Result := 'There is not enough memory available for this task. Quit one or more programs, and then try again.';
	008: Result := 'This function is not supported. Use the Capabilities function to determine what this driver supports.';
	009: Result := 'An error number was specified that is not defined in the system.';
	010: Result := 'An invalid flag was passed to a system function.';
	011: Result := 'An invalid parameter was passed to a system function.';
	012: Result := 'Handle being used simultaneously on another thread (eg callback).';
	013: Result := 'Specified alias not found in WIN.INI.';
	014: Result := 'The registry database is corrupt.';
	015: Result := 'The specified registry key was not found.';
	016: Result := 'The registry could not be opened or could not be read.';
	017: Result := 'The registry could not be written to.';
	018: Result := 'The specified registry key could not be deleted.';
	019: Result := 'The specified registry key value could not be found.';
	020: Result := 'The driver did not generate a valid OPEN callback.';

	032: Result := 'The specified format cannot be translated or supported. Use the Capabilities function to view supported formats.';
	033: Result := 'Media data is still playing. Reset the device, or wait until the data is finished playing.';
	034: Result := 'The wave header was not prepared. Use the Prepare function to prepare the header, and then try again.';
	035: Result := 'Cannot open the device without using the WAVE_ALLOWSYNC flag. Use the flag, and then try again.';

	064: Result := 'The MIDI header was not prepared. Use the Prepare function to prepare the header, and then try again.';
	065: Result := 'Media data is still playing. Reset the device, or wait until the data is finished playing.';
	066: Result := 'There are no instruments defined for MIDI mapper. Use the MIDI tab in ''Multimedia Properties'' to set up instruments.';
	067: Result := 'The port is transmitting data to the device. Wait until the data has been transmitted, and then try again.';

	069: Result := 'The MIDI Instrument Description File (.IDF) is invalid. For more information, contact the file''s manufacturer.';
	070: Result := 'A MIDI call was made which is invalid with the current open mode. Reopen the device with the correct mode.';
	071: Result := 'Driver condition - do not callback this input event';

	257: Result := 'Invalid MCI device ID. Use the ID returned when opening the MCI device.';

	259: Result := 'The command parameter used is not defined in the MCI command set.';

	261: Result := 'The command used is not a valid MCI command.';
	262: Result := 'There is a problem with your media device. Make sure it is working properly or contact the device manufacturer.';
	263: Result := 'This is not a registered MCI device.';
	264: Result := 'There is not enough memory available for this task. Quit one or more programs, and then try again.';
	265: Result := 'This alias is already in use by this program. Use a unique alias rather than the device name.';
	266: Result := 'The device could not be loaded. Verify that the driver is installed correctly.';
	267: Result := 'The command string is empty.';
	268: Result := 'The output string was too large to fit in the return buffer. Increase the size of the buffer.';
	269: Result := 'The specified command requires a character-string parameter. Please provide one.';
	270: Result := 'The SG entered is invalid for this command. Please enter a number.';
	271: Result := 'The device driver returned an invalid return type. To obtain a new driver, contact the device manufacturer.';
	272: Result := 'There is a problem with the device driver. Check with the device manufacturer about obtaining a new driver.';
	273: Result := 'The specified command is missing a paramter. Please enter one.';
	274: Result := 'The MCI device you are using does not support the specified command.';
	275: Result := 'Cannot find the specified file. Make sure the path and filename are correct.';
	276: Result := 'The device driver is not ready. Wait a minute and try again.';
	277: Result := 'A problem occurred in initializing MCI. Try restarting Windows.';
	278: Result := 'This is a driver specific error and the driver is closed. Try the command again.';
	279: Result := 'Use a specific device name for this command.';
	280: Result := 'Errors occurred in more than one device. Specify each command and device separately to determine which devices caused the errors.';
	281: Result := 'This file could not be played. Check the filename or install a driver that supports this type of file.';
	282: Result := 'The parameter is out of range for the specified command.';

	284: Result := 'The specified parameters cannot be used together.';

	286: Result := 'Cannot save the specified file. Make sure you have enough disk space or are still connected to the network.';
	287: Result := 'Cannot find the specified device. Make sure it is installed or that the device name is spelled correctly.';
	288: Result := 'The specified device is now being closed. Wait a few seconds, and then try again.';
	289: Result := 'The specified alias is already being used in this application. Use a unique alias.';
	290: Result := 'The constant used is invalid for this command.';
	291: Result := 'The device driver is already in use. To share it, use the ''shareable'' parameter with each ''open'' command.';
	292: Result := 'The specified command requires an alias, file, driver, or device name. Please supply one.';
	293: Result := 'The specified value for the time format is invalid. Refer to the MCI documentation for valid formats.';
	294: Result := 'A closing double-quotation mark is missing from the parameter value. Please supply one.';
	295: Result := 'A parameter or value was specified twice. Only specify it once.';
	296: Result := 'The file cannot be played on the specified MCI device. The file may be corrupt, or not in the correct format.';
	297: Result := 'A null parameter block was passed to MCI.';
	298: Result := 'Cannot save an unnamed file. Supply a filename.';
	299: Result := 'You must specify an alias when using the ''new'' parameter.';
	300: Result := 'Cannot use the ''notify'' flag with auto-opened devices.';
	301: Result := 'Cannot use a filename with the specified device.';
	302: Result := 'Cannot carry out the commands in the order specified. Correct the command sequence, and then try again.';
	303: Result := 'Cannot carry out this command on an auto-opened device. Wait until the device is closed, and then try again.';
	304: Result := 'The filename is invalid.';
	305: Result := 'Cannot specify extra characters after a string enclosed in quotation marks.';
	306: Result := 'This device is not installed. To install a new driver, double-click the Add New Hardware icon in Control Panel.';
	307: Result := 'Cannot access the specified file or MCI device. Try changing directories or restarting your computer.';
	308: Result := 'Cannot access the specified file or MCI device because the application cannot change directories.';
	309: Result := 'Cannot access specified file or MCI device because the application cannot change drives.';
	310: Result := 'Specify a device or driver name that is less than 79 characters.';
	311: Result := 'Specify a device or driver name that is less than 69 characters.';
	312: Result := 'The specified command requires a numeric parameter. For example, ''play to 10''. Please provide one.';

	320: Result := 'All wave devices that can play files in this format are in use. Try again when a wave device is free.';
	321: Result := 'The current wave device is in use. Wait until the device is free, and then try again.';
	322: Result := 'All wave devices that can record files in this format are in use. Try again when a wave device is free.';
	323: Result := 'The current wave device is in use. Wait until the device is free, and then try again.';
	324: Result := 'The playback device is in use. Wait until it is finished, and try again.';
	325: Result := 'The recording device is in use. Wait until it is finished, and try again.';
	326: Result := 'No wave device that can play files in the current format is installed.';
	327: Result := 'This device cannot recognize the current file format. Select a different device, and then try again.';
	328: Result := 'No wave device that can record files in the current format is installed.';
	329: Result := 'This device cannot recognize the current file format. Select a different device, and then try again.';

	336: Result := 'Cannot use the song-pointer time format and the SMPTE time-format together.';
	337: Result := 'The specified MIDI device is already in use. Wait until it is free, and then try again.';
	338: Result := 'This device is not installed. To install a new driver, double- click the Add New Hardware icon in Control Panel.';
	346: Result := 'There is no display window.';
	347: Result := 'Could not create or use window.';
	348: Result := 'Cannot read this file. Make sure the file hasn''t been deleted, or check your disk or network connection.';
	349: Result := 'Cannot write to the specified file. Make sure you have enough disk space or are still connected to the network.';
	350: Result := 'This CD disc does not contain a universal product code.';
	else
	begin
		Result := 'Unknown error';
	end;
	end;}
	SetLength(Result, 128);
	if mciGetErrorString(ErrorCode, PChar(Result), 128) then
		Result := PChar(Result)
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' Unknown error';
end;

function MidiMCIError(ErrorCode: SG): Boolean;
begin
	if ErrorCode <> 0 then
	begin
		Result := ErrorMessageRetry(MCIError(ErrorCode));
	end
	else
		Result := False;
end;

function MidiMCICallBack: Boolean;
begin
	Result := MidiPlaying;
end;

procedure MidiMCIOpen(FileName: TFileName);
label LRetry, LRetrySend;
var
	FFlags: U4;
	FError: SG;
	F: file;
	ErrorCode: SG;
begin
	MidiPlaying := False;
	LRetry:
	AssignFile(F, FileName);
	FileMode := 0; Reset(F, 1);
	ErrorCode := IOResult;
	CloseFile(F);
	IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FileName, ErrorCode) then goto LRetry;
		Exit;
	end;
	LRetrySend:
	FillChar(OpenParm, SizeOf(TMCI_Open_Parms), 0);

	OpenParm.dwCallback := 0;
	OpenParm.lpstrDeviceType := 'WaveAudio';
	OpenParm.lpstrElementName := PChar(FileName);
	FFlags := MCI_OPEN_ELEMENT or MCI_NOTIFY;

	FError := mciSendCommand(0, mci_Open, FFlags, U4(@OpenParm));
	MidiOpened := FError = 0;
	if MidiMCIError(FError) then goto LRetrySend;
end;

procedure MidiMCISeek(const SeekTo: U4);
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	SeekParm.dwTo := SeekTo;
	SeekParm.dwCallback := 0;
	FFlags := 0;
	FFlags := FFlags or mci_To;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Seek, FFlags, U4(@SeekParm));
	if MidiMCIError(FError) then goto LRetrySend;
end;

function MidiMCIGetPos: U4;
label LRetrySeek;
var
	FFlags: U4;
	FError: SG;
	StatusParm: TMCI_Status_Parms;
begin
	Result := 0;
	if MidiOpened = False then Exit;
	LRetrySeek:
	StatusParm.dwItem := mci_Status_Position;
	StatusParm.dwTrack := 0;
	StatusParm.dwReturn := 0;
	FFlags := mci_Wait or mci_Status_Item;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Status, FFlags, U4(@StatusParm));
	if MidiMCIError(FError) then goto LRetrySeek;
	Result := StatusParm.dwReturn;
end;

procedure MidiMCIPause;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Pause, FFlags, U4(@GenParm));
	if MidiMCIError(FError) then goto LRetrySend;
	MidiPlaying := False;
end;

procedure MidiMCIStop;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	MidiPlaying := False;
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Stop, FFlags, U4(@GenParm));
	if MidiMCIError(FError) then goto LRetrySend;
end;

procedure MidiMCIResume;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	GenParm.dwCallback := MidiHandle;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Resume, FFlags, U4(@GenParm));
	if MidiMCIError(FError) then goto LRetrySend;
	MidiPlaying := False;
	MidiMCIPlay; // Need for Callback
end;

procedure MidiMCIPlay;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	if MidiPlaying = True then Exit;
	LRetrySend:
	MidiPlaying := True;
	FFlags := mci_Notify;
	PlayParm.dwCallback := MidiHandle;

	FError := mciSendCommand(OpenParm.wDeviceID, mci_Play, FFlags, U4(@PlayParm));
	if MidiMCIError(FError) then goto LRetrySend;
end;

procedure MidiMCIClose;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	PlayParm.dwCallback := OpenParm.dwCallback;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Close, FFlags, U4(@GenParm));
	if FError = 0 then
	begin
		MidiOpened := False;
		MidiPlaying := False;
	end;
	if MidiMCIError(FError) then goto LRetrySend;
end;

(*
procedure MidiReadFromFile(var Midi: PMidi; FName: TFileName);
label LRetry, LFin;
var
	FSize: U32;
	F: file;
	ErrorCode: SG;
begin
	LRetry:
	AssignFile(F, FName);
	FileMode := 0;
	Reset(F, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FName, ErrorCode) then goto LRetry;
	end
	else
	begin
		FSize := FileSize(F);
		GetMem(Midi, FSize);
		BlockRead(F, Midi^, FSize);
		ErrorCode := IOResult; if ErrorCode <> 0 then goto LFin;
		LFin:
		CloseFile(F);
		IOResult;
		ErrorCode := ErrorCode; // Delphi error !!!
		if ErrorCode <> 0 then // Delphi error !!! +CB is error, +D1 is ok
		begin
			if IOErrorRetry(FName, ErrorCode) then goto LRetry;
		end;
	end;
end;

procedure MidiWriteToFile(var Midi: PMidi; FName: TFileName);
label LRetry;
var
	F: file;
	ErrorCode: SG;
begin
	LRetry:
	AssignFile(F, FName);
	if FileExists(FName) then
	begin
		FileMode := 1; Reset(F, 1);
	end
	else
		Rewrite(F, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FName, ErrorCode) then goto LRetry;
	end
	else
	begin
//    BlockWrite(F, Midi^, Midi^.BytesFollowing + 8);
		Truncate(F);
		ErrorCode := IOResult;
		CloseFile(F);
		IOResult;
		if ErrorCode <> 0 then
		begin
			if IOErrorRetry(FName, ErrorCode) then goto LRetry;
		end;
	end;
end;

function MidiErrorText(ErrorCode: SG): ShortString;
var P: PChar;
begin
	P := '...';
	MidiOutGetErrorText(ErrorCode, P, 255);
	Result := string(P) + IntToStr(ErrorCode);
end;

procedure MidiOpen;
var
	ErrorCode: SG;
begin
	HW := New(PHMidiOUT);
	ErrorCode := MidiOutOpen(HW, Midi_MAPPER,
		0, 0, CALLBACK_NULL{Midi_MAPPED{Midi_ALLOWSYNC});
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));
end;

procedure MidiPrepareHeader(var Header: PMidiHdr; Midi: PMidi);
var ErrorCode: SG;
begin
	New(Header);
	FillChar(Header^, SizeOf(Header^), 0);
	Header.lpData := Pointer(SG(@Midi.Data) + kd);
	Header.dwBufferLength := 4096;//Midi.DataBytes; //SizeOf(PMemBlock^);
	Header.dwBytesRecorded := 0;
	Header.dwUser := 4096;//Midi.DataBytes; // SizeOf(PMemBlock^);
	Header.dwFlags := WHDR_DONE; //0 + WHDR_PREPARED  + WHDR_INQUEUE;
	Header.lpNext := nil;//Header;
	Header.dwOffset := 0;

	ErrorCode := MidiOutPrepareHeader(HW^, Header, SizeOf(TMidiHdr));
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));
end;

procedure MidiUnprepareHeader(var Header: PMidiHdr);
var ErrorCode: SG;
begin
	ErrorCode := MidiOutUnprepareHeader(HW^, Header, SizeOf(TMidiHdr));
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));
	Dispose(Header); Header := nil;
end;


procedure MidiPlay(Header: PMidiHdr);
var
	ErrorCode: SG;
begin
	ErrorCode := MidiOutReset(HW^);
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));

	ErrorCode := MidiOutLongMsg(HW^, Header, SizeOf(TMidiHdr));
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));
end;

procedure MidiClose;
var
	ErrorCode: SG;
begin
	ErrorCode := MidiOutClose(HW^);
	if ErrorCode <> 0 then InternalError(MidiErrorText(ErrorCode));

	if ErrorCode = 0 then
	begin
		Dispose(HW); HW := nil;
	end;
end;
*)
end.
