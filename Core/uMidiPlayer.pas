unit uMidiPlayer;

interface

uses
  uTypes,
  Windows,
  Controls,
  SysUtils,
  Classes,
  Messages,
  MMSystem;

type
  TMidiPlayer = class
  private
    FPlaying: Boolean;
    FOpened: Boolean;

    FWindowHandle: HWND;
    FOnFinished: TNotifyEvent;
    procedure WndProc(var Msg: TMessage);
    procedure SetOnFinished(const Value: TNotifyEvent);
  public
    procedure Seek(const SeekTo: U4);
    function GetPos: U4;
    function GetLength: U4;

    procedure Open(FileName: TFileName);
    procedure Close;

    procedure Play;
    procedure Stop;

    procedure Pause;
    procedure Resume;

    property Opened: Boolean read FOpened;
    property Playing: Boolean read FPlaying;
    property OnFinished: TNotifyEvent read FOnFinished write SetOnFinished;
  end;

implementation

uses
  Forms,
  uOutputFormat,
  uFiles,
  uMsg;

var
	OpenParm: TMCI_Open_Parms;
	PlayParm: TMCI_Play_Parms;
	GenParm: TMCI_Generic_Parms;
	SeekParm: TMCI_Seek_Parms;

function MCIErrorToStr(const ErrorCode: U4): string;
begin
	SetLength(Result, 255);
	if mciGetErrorString(ErrorCode, PChar(Result), 255) then
		Result := PChar(Result)
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' Unknown error';
end;

procedure MCIError(const ErrorCode: SG);
begin
	if ErrorCode <> 0 then
	begin
		ErrorMsg(MCIErrorToStr(ErrorCode));
  end;
end;

{ TMidiPlayer }

procedure TMidiPlayer.Open(FileName: TFileName);
var
	FFlags: U4;
	FError: SG;
begin
  if FOpened then
    Close;
	FPlaying := False;
  TestFileReadable(FileName);

  FillChar(OpenParm, SizeOf(TMCI_Open_Parms), 0);

  OpenParm.dwCallback := 0;
  OpenParm.lpstrDeviceType := 'WaveAudio';
  OpenParm.lpstrElementName := PChar(FileName);
  FFlags := MCI_OPEN_ELEMENT or MCI_NOTIFY;

  FError := mciSendCommand(0, mci_Open, FFlags, U4(@OpenParm));
  FOpened := FError = 0;
  MCIError(FError);
	if FOpened then
    FWindowHandle := Classes.AllocateHWnd(WndProc);
end;

procedure TMidiPlayer.Close;
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
  Classes.DeallocateHWnd(FWindowHandle);
	FFlags := 0;
	PlayParm.dwCallback := OpenParm.dwCallback;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Close, FFlags, U4(@GenParm));
	if FError = 0 then
	begin
		FOpened := False;
		FPlaying := False;
	end;
	MCIError(FError);
end;

procedure TMidiPlayer.Seek(const SeekTo: U4);
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
	SeekParm.dwTo := SeekTo;
	SeekParm.dwCallback := 0;
	FFlags := 0;
	FFlags := FFlags or mci_To;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Seek, FFlags, U4(@SeekParm));
	MCIError(FError);
end;

function TMidiPlayer.GetPos: U4;
var
	FFlags: U4;
	FError: SG;
	StatusParm: TMCI_Status_Parms;
begin
	Result := 0;
	if FOpened = False then Exit;
	StatusParm.dwItem := MCI_STATUS_POSITION;
	StatusParm.dwTrack := 0;
	StatusParm.dwReturn := 0;
	FFlags := mci_Wait or mci_Status_Item;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Status, FFlags, U4(@StatusParm));
	MCIError(FError);
	Result := StatusParm.dwReturn;
end;

function TMidiPlayer.GetLength: U4;
var
	FFlags: U4;
	FError: SG;
	StatusParm: TMCI_Status_Parms;
begin
	Result := 0;
	if FOpened = False then Exit;
	StatusParm.dwItem := MCI_STATUS_LENGTH;
	StatusParm.dwTrack := 0;
	StatusParm.dwReturn := 0;
	FFlags := mci_Wait or mci_Status_Item;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Status, FFlags, U4(@StatusParm));
	MCIError(FError);
	Result := StatusParm.dwReturn;
end;

procedure TMidiPlayer.Pause;
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Pause, FFlags, U4(@GenParm));
	MCIError(FError);
	FPlaying := False;
end;

procedure TMidiPlayer.Stop;
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
	FPlaying := False;
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Stop, FFlags, U4(@GenParm));
	MCIError(FError);
end;

procedure TMidiPlayer.Resume;
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
	FFlags := 0;
	GenParm.dwCallback := FWindowHandle;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Resume, FFlags, U4(@GenParm));
	MCIError(FError);
	FPlaying := False;
	Play; // Need for Callback
end;

procedure TMidiPlayer.Play;
var
	FFlags: U4;
	FError: SG;
begin
	if FOpened = False then Exit;
	if FPlaying = True then Exit;
	FPlaying := True;
	FFlags := mci_Notify;
	PlayParm.dwCallback := FWindowHandle;

	FError := mciSendCommand(OpenParm.wDeviceID, mci_Play, FFlags, U4(@PlayParm));
	MCIError(FError);
end;

procedure TMidiPlayer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = MM_MCINOTIFY then
  begin
    // Song finished
    if Assigned(OnFinished) then
      OnFinished(Self);
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TMidiPlayer.SetOnFinished(const Value: TNotifyEvent);
begin
  FOnFinished := Value;
end;

end.
