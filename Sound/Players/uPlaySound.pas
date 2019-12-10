unit uPlaySound;

interface

uses
	SysUtils;

type
	TWinSound = (
		wsAsterisk, // Done
		wsCloseProgram,
		wsCriticalStop, // uMsgDlg
		wsDefaultSound, // Beep
		wsExclamation,  // uMsgDlg
		wsExitWindows,
		wsMaximize,
		wsMenuCommand,
		wsMenuPopup,
		wsMinimize,
		wsOpenProgram,
		wsProgramError,
		wsQuestion, // uMsgDlg
		wsRestoreDown,
		wsRestoreUp,
//		wsRingIn,
//		wsRingout,
//		wsSelect,
//		wsShowToolbarBand,
		wsStartWindows
//		wsSystemDefault
		);
const
	WinSoundNames: array[TWinSound] of string = (
		'SystemAsterisk',
		'Close',
		'SystemHand',
		'.Default',
		'SystemExclamation',
		'SystemExit',
		'Maximize',
		'MenuCommand',
		'MenuPopup',
		'Minimize',
		'Open',
		'AppGPFault',
		'SystemQuestion',
		'RestoreDown',
		'RestoreUp',
//		'RingIn',
//		'RingOut',
//		'CCSelect',
//		'ShowBand',
		'SystemStart'
//		'SystemDefault'
		);

procedure Beep;
procedure StopPlayWave;
function GetWinSoundFileName(const WinSound: TWinSound): TFileName;
procedure PlayWinSound(const WinSound: TWinSound);
procedure PlayWaveFile(const WaveName: TFileName);
procedure PlayWave(const PWave: Pointer);

implementation

uses
  Winapi.Windows,
  System.Win.Registry,
  Winapi.MMSystem,

  uTypes,
  uFiles,
	uMsg,
  uMainLog;

function GetWinSoundFileName(const WinSound: TWinSound): TFileName;
var
	Reg: TRegistry;
	Key: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		Key := 'AppEvents' + PathDelim + 'Schemes' + PathDelim + 'Apps' + PathDelim + '.Default' + PathDelim + WinSoundNames[WinSound] + PathDelim + '.Current';
		if Reg.OpenKeyReadOnly(Key) then
		begin
			if Reg.ValueExists('') then
			begin
				Result := Reg.ReadString('');
			end;
			Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

procedure PlayWinSound(const WinSound: TWinSound);
begin
	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Play windows sound ' + WinSoundNames[WinSound] + '.', mlDebug);
	PlayWaveFile(GetWinSoundFileName(WinSound));
end;

procedure Beep;
begin
	PlayWinSound(wsDefaultSound);
end;

procedure StopPlayWave;
begin
	if PlaySound(nil, 0, SND_MEMORY or SND_NODEFAULT) = False then
	begin
		ErrorMsg(GetLastError);
	end;
end;

procedure PlayWaveFile(const WaveName: TFileName);
begin
	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Play sound ' + WaveName + '.', mlDebug);
	if WaveName <> '' then
		if PlaySound(PChar(ExpandDir(WaveName)), 0, SND_ASYNC {and SND_FILENAME}) = False then
		begin
			ErrorMsg(GetLastError);
		end;
end;

procedure PlayWave(const PWave: Pointer);
begin
	if PWave = nil then Exit;
	if PlaySound(PChar(PWave), 0, SND_ASYNC or SND_MEMORY or SND_NODEFAULT) = False then
	begin
		ErrorMsg(GetLastError);
	end;
end;

initialization
{$IFNDEF NoInitialization}
	if IsRelease then
		StopPlayWave; // First time takes long
{$ENDIF NoInitialization}
end.
