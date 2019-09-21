{ *************************************************************************** }
{                                                                             }
{ NLDJoystick  -  www.nldelphi.com Open Source Delphi designtime component    }
{                                                                             }
{ Initiator: Albert de Weerd (aka NGLN)                                       }
{ License: Free to use, free to modify                                        }
{ Website: http://www.nldelphi.com/Forum/showthread.php?t=29812               }
{ SVN path: http://svn.nldelphi.com/nldelphi/opensource/ngln/NLDJoystick      }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Date: Januar 21, 2013                                                       }
{ Version: 1.2.0                                                              }
{                                                                             }
{ *************************************************************************** }

unit NLDJoystick;

interface

uses
  Winapi.MMSystem, Winapi.Windows, Winapi.Messages, SysUtils, Classes, Math;

const
  JOY_POVCENTERED = $FFFFFFFF;

type
  TNLDJoystick = class;

  TJoyID = JOYSTICKID1..JOYSTICKID2;

  TJoyRelPos = record
    X: Double;
    Y: Double;
    Z: Double;
    R: Double;
    U: Double;
    V: Double;
  end;

  TJoyAbsPos = packed record
    X: Word;
    Y: Word;
    Z: Word;
    R: Word;
    U: Word;
    V: Word;
  end;

  TJoyButton = (JoyBtn1, JoyBtn2, JoyBtn3, JoyBtn4, JoyBtn5, JoyBtn6, JoyBtn7,
    JoyBtn8, JoyBtn9, JoyBtn10, JoyBtn11, JoyBtn12, JoyBtn13, JoyBtn14,
    JoyBtn15, JoyBtn16, JoyBtn17, JoyBtn18, JoyBtn19, JoyBtn20, JoyBtn21,
    JoyBtn22, JoyBtn23, JoyBtn24, JoyBtn25, JoyBtn26, JoyBtn27, JoyBtn28,
    JoyBtn29, JoyBtn30, JoyBtn31, JoyBtn32);
  TJoyButtons = set of TJoyButton;

  TJoyAxis = (axX, axY, axZ, axR, axU, axV);
  TJoyAxises = set of TJoyAxis;

  TJoyButtonEvent = procedure(Sender: TNLDJoystick;
    const Buttons: TJoyButtons) of object;
  TJoyMoveEvent = procedure(Sender: TNLDJoystick; const JoyPos: TJoyRelPos;
    const Buttons: TJoyButtons) of object;
  TJoyPOVChangedEvent = procedure(Sender: TNLDJoystick;
    Degrees: Single) of object;

  TMMJoyMsg = record
    Msg: Cardinal;
    Buttons: WPARAM; {wParam}
    XZPos: Word;       {LoWord(lParam)}
    YPos: Word;        {HiWord(lParam)}
{$ifdef CPUX64}
    Reserved: LongWord;
{$endif}
    Result: LRESULT;
  end;

  TJoyRanges = packed record
    XDown: Word;
    XUp: Word;
    YDown: Word;
    YUp: Word;
    ZDown: Word;
    ZUp: Word;
    RDown: Word;
    RUp: Word;
    UDown: Word;
    UUp: Word;
    VDown: Word;
    VUp: Word;
  end;

  ENLDJoystickError = class(EComponentError);

  TNLDJoystick = class(TComponent)
  private
    FActive: Boolean;
    FAdvanced: Boolean;
    FAxisCount: Byte;
    FAxises: TJoyAxises;
    FButtonCount: Byte;
    FCenter: TJoyAbsPos;
    FHasPOV: Boolean;
    FID: TJoyID;
    FInterval: Integer;
    FMax: TJoyAbsPos;
    FMin: TJoyAbsPos;
    FOnButtonDown: TJoyButtonEvent;
    FOnButtonUp: TJoyButtonEvent;
    FOnIdle: TNotifyEvent;
    FOnMove: TJoyMoveEvent;
    FOnPOVChanged: TJoyPOVChangedEvent;
    FPrevButtons: UINT;
    FPrevButtonTick: Cardinal;
    FPrevIdleTick: Cardinal;
    FPrevMoveTick: Cardinal;
    FPrevPos: TJoyRelPos;
    FPrevPOV: Cardinal;
    FPrevPOVTick: Cardinal;
    FProcessedButtonOnce: Boolean;
    FProcessedMoveOnce: Boolean;
    FProcessedPOVOnce: Boolean;
    FRanges: TJoyRanges;
    FRepeatButtonDelay: Cardinal;
    FRepeatMoveDelay: Cardinal;
    FRepeatPOVDelay: Cardinal;
    FSuspendScreensaver: Boolean;
    FThreshold: Double;
    FWindowHandle: HWND;
    function GetInterval: Integer;
    function Initialize(NeedAdvanced: Boolean = False): Boolean;
    procedure InitTimer;
    procedure ProcessAdvanced;
    procedure ProcessSimple(var Message: TMMJoyMsg);
    procedure SetActive(Value: Boolean);
    procedure SetAdvanced(Value: Boolean);
    procedure SetInterval(Value: Integer);
    procedure SetThreshold(Value: Double);
  protected
    procedure DoButtonDown(Buttons: Cardinal); virtual;
    procedure DoButtonUp(Buttons: Cardinal); virtual;
    procedure DoIdle; virtual;
    procedure DoMove(const JoyPos: TJoyRelPos; Buttons: Cardinal); virtual;
    procedure DoPOVChanged(POV: Cardinal); virtual;
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AbsCenter: TJoyAbsPos read FCenter;
    property AbsMax: TJoyAbsPos read FMax;
    property AbsMin: TJoyAbsPos read FMin;
    property Active: Boolean read FActive write SetActive default False;
    property Advanced: Boolean read FAdvanced write SetAdvanced default False;
    property AxisCount: Byte read FAxisCount;
    property Axises: TJoyAxises read FAxises;
    property ButtonCount: Byte read FButtonCount;
    property HasPOV: Boolean read FHasPOV;
    property ID: TJoyID read FID;
    property OnButtonDown: TJoyButtonEvent read FOnButtonDown
      write FOnButtonDown;
    property OnButtonUp: TJoyButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnMove: TJoyMoveEvent read FOnMove write FOnMove;
    property OnPOVChanged: TJoyPOVChangedEvent read FOnPOVChanged
      write FOnPOVChanged;
    property PollingInterval: Integer read GetInterval write SetInterval
      default 40;
    property RepeatButtonDelay: Cardinal read FRepeatButtonDelay
      write FRepeatButtonDelay default 350;
    property RepeatMoveDelay: Cardinal read FRepeatMoveDelay
      write FRepeatMoveDelay default 350;
    property RepeatPOVDelay: Cardinal read FRepeatPOVDelay
      write FRepeatPOVDelay default 350;
    property SuspendScreensaver: Boolean read FSuspendScreensaver
      write FSuspendScreensaver default False;
    property ThresholdFactor: Double read FThreshold write SetThreshold;
  end;

function Joystick: TNLDJoystick;
function Joystick1: TNLDJoystick;
function Joystick2: TNLDJoystick;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NLDelphi', [TNLDJoystick]);
end;

var
  FJoystick1: TNLDJoystick = nil;
  FJoystick2: TNLDJoystick = nil;

function Joystick: TNLDJoystick;
begin
  Result := Joystick1;
end;

function Joystick1: TNLDJoystick;
begin
  if FJoystick1 = nil then
    FJoystick1 := TNLDJoystick.Create(nil);
  Result := FJoystick1;
end;

function Joystick2: TNLDJoystick;
begin
  if FJoystick2 = nil then
    FJoystick2 := TNLDJoystick.Create(nil);
  if FJoystick1 = FJoystick2 then
    FJoystick1 := nil;
  Result := FJoystick2;
end;

procedure NotifyKeyboardActivity;
begin
  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 1, nil, 0);
end;

{ TNLDJoystick }

const
  DefTimerID = 1;

resourcestring
  SErrNoTimersAvail = 'Not enough timers available for joystick support';
  SErrNoJoystickAvail = 'Not enough joysticks available for another ' +
    'TNLDJoystick instance. Maximum joystick count is two.';

constructor TNLDJoystick.Create(AOwner: TComponent);
begin
  if FJoystick1 = nil then
  begin
    FJoystick1 := Self;
    FID := JOYSTICKID1;
  end
  else if FJoystick2 = nil then
  begin
    FJoystick2 := Self;
    FID := JOYSTICKID2;
  end
  else
    raise ENLDJoystickError.Create(SErrNoJoystickAvail);
  inherited Create(AOwner);
  FInterval := 40;
  FRepeatButtonDelay := 350;
  FRepeatMoveDelay := 350;
  FRepeatPOVDelay := 350;
  FWindowHandle := AllocateHWnd(WndProc);
  FActive := Initialize(FAdvanced);
end;

destructor TNLDJoystick.Destroy;
begin
  SetActive(False);
  DeallocateHWnd(FWindowHandle);
  if FJoystick1 = Self then
    FJoystick1 := nil
  else
    FJoystick2 := nil;
  inherited Destroy;
end;

procedure TNLDJoystick.DoButtonDown(Buttons: Cardinal);
begin
  if Assigned(FOnButtonDown) then
    FOnButtonDown(Self, TJoyButtons(Buttons));
  if FSuspendScreensaver then
    NotifyKeyboardActivity;
end;

procedure TNLDJoystick.DoButtonUp(Buttons: Cardinal);
begin
  if Assigned(FOnButtonUp) then
    FOnButtonUp(Self, TJoyButtons(Buttons));
  if FSuspendScreensaver then
    NotifyKeyboardActivity;
end;

procedure TNLDJoystick.DoIdle;
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

procedure TNLDJoystick.DoMove(const JoyPos: TJoyRelPos; Buttons: Cardinal);
begin
  if Assigned(FOnMove) then
    FOnMove(Self, JoyPos, TJoyButtons(Buttons));
  if FSuspendScreensaver then
    NotifyKeyboardActivity;
end;

procedure TNLDJoystick.DoPOVChanged(POV: Cardinal);
begin
  if Assigned(FOnPOVChanged) then
    FOnPOVChanged(Self, POV/100);
  if FSuspendScreensaver then
    NotifyKeyboardActivity;
end;

function TNLDJoystick.GetInterval: Integer;
begin
  if FAdvanced then
    Result := FInterval
  else
    Result := -1;
end;

function TNLDJoystick.Initialize(NeedAdvanced: Boolean = False): Boolean;
var
  JoyInfo: TJoyInfoEx;
  JoyCaps: TJoyCaps;
begin
  joyReleaseCapture(FID);
  ZeroMemory(@FPrevPos, SizeOf(FPrevPos));
  ZeroMemory(@JoyInfo, SizeOf(JoyInfo));
  JoyInfo.dwSize := SizeOf(JoyInfo);
  JoyInfo.dwFlags := JOY_RETURNCENTERED;
  if (joyGetNumDevs <= FID) or
      (joyGetPosEx(FID, @JoyInfo) <> JOYERR_NOERROR) then
    Result := False
  else
  begin
    joyGetDevCaps(FID, @JoyCaps, SizeOf(JoyCaps));
    FAxisCount := Min(JoyCaps.wNumAxes, JoyCaps.wMaxAxes);
    FButtonCount := Min(JoyCaps.wNumButtons, JoyCaps.wMaxButtons);
    FAxises := [axX, axY];
    FCenter.X := JoyInfo.wXpos;
    FCenter.Y := JoyInfo.wYpos;
    FMax.X := JoyCaps.wXmax;
    FMax.Y := JoyCaps.wYmax;
    FMin.X := JoyCaps.wXmin;
    FMin.Y := JoyCaps.wYmin;
    FRanges.XDown := FCenter.X - FMin.X;
    FRanges.XUp := FMax.X - FCenter.X;
    FRanges.YDown := FCenter.Y - FMin.Y;
    FRanges.YUp := FMax.Y - FCenter.Y;
    if (JOYCAPS_HASZ and JoyCaps.wCaps) = JOYCAPS_HASZ then
    begin
      Include(FAxises, axZ);
      FCenter.Z := JoyInfo.wZpos;
      FMax.Z := JoyCaps.wZmax;
      FMin.Z := JoyCaps.wZmin;
      FRanges.ZDown := FCenter.Z - FMin.Z;
      FRanges.ZUp := FMax.Z - FCenter.Z;
    end;
    if (not NeedAdvanced) or ((FButtonCount <= 4) and (FAxisCount <= 3)) then
    begin
      FAdvanced := False;
      FHasPOV := False;
      joySetCapture(FWindowHandle, FID, 0, True);
    end
    else
    begin
      FAdvanced := True;
      FInterval := Max(JoyCaps.wPeriodMin, Min(FInterval, JoyCaps.wPeriodMax));
      if (JOYCAPS_HASR and JoyCaps.wCaps) = JOYCAPS_HASR then
      begin
        Include(FAxises, axR);
        FCenter.R := JoyInfo.dwRpos;
        FMax.R := JoyCaps.wRmax;
        FMin.R := JoyCaps.wRmin;
        FRanges.RDown := FCenter.R - FMin.R;
        FRanges.RUp := FMax.R - FCenter.R;
      end;
      if (JOYCAPS_HASU and JoyCaps.wCaps) = JOYCAPS_HASU then
      begin
        Include(FAxises, axU);
        FCenter.U := JoyInfo.dwUpos;
        FMax.U := JoyCaps.wUmax;
        FMin.U := JoyCaps.wUmin;
        FRanges.UDown := FCenter.U - FMin.U;
        FRanges.UUp := FMax.U - FCenter.U;
      end;
      if (JOYCAPS_HASV and JoyCaps.wCaps) = JOYCAPS_HASV then
      begin
        Include(FAxises, axV);
        FCenter.V := JoyInfo.dwVpos;
        FMax.V := JoyCaps.wVmax;
        FMin.V := JoyCaps.wVmin;
        FRanges.VDown := FCenter.V - FMin.V;
        FRanges.VUp := FMax.V - FCenter.V;
      end;
      FHasPOV := (JOYCAPS_HASPOV and JoyCaps.wCaps) = JOYCAPS_HASPOV;
      InitTimer;
    end;
    Result := True;
  end;
end;

procedure TNLDJoystick.InitTimer;
begin
  KillTimer(FWindowHandle, DefTimerID);
  if SetTimer(FWindowHandle, DefTimerID, FInterval, nil) = 0 then
    raise ENLDJoystickError.Create(SErrNoTimersAvail);
end;

procedure TNLDJoystick.ProcessAdvanced;
const
  JOY_RETURN = JOY_RETURNX or JOY_RETURNY or JOY_RETURNZ or
    JOY_RETURNR or JOY_RETURNU or JOY_RETURNV or JOY_RETURNPOVCTS or
    JOY_RETURNBUTTONS;
  CenterJoyPos: TJoyRelPos = (X:0.0; Y:0.0; Z:0.0; R:0.0; U:0.0; V:0.0);
var
  JoyInfo: TJoyInfoEx;
  JoyPos: TJoyRelPos;
  CurrentTick: Cardinal;
  MustDelay: Boolean;
begin
  ZeroMemory(@JoyInfo, SizeOf(JoyInfo));
  JoyInfo.dwSize := SizeOf(JoyInfo);
  JoyInfo.dwFlags := JOY_RETURN;
  if joyGetPosEx(FID, @JoyInfo) = JOYERR_NOERROR then
    with JoyInfo do
    begin
      if (FPrevIdleTick = 0) and (LoWord(wXpos) = FCenter.X) and
        (LoWord(wYpos) = FCenter.Y) and (LoWord(wZpos) = FCenter.Z) and
        (LoWord(dwRpos) = FCenter.R) and (LoWord(dwUpos) = FCenter.U) and
        (LoWord(dwVpos) = FCenter.V) and (wButtons = 0) and
        ((not FHasPOV) or (dwPOV = JOY_POVCENTERED)) then
      begin
        FPrevIdleTick := GetTickCount;
        DoIdle;
      end
      else
        FPrevIdleTick := 0;
      JoyPos := FPrevPos;
      if LoWord(wXpos) < FCenter.X then
        JoyPos.X := (LoWord(wXpos) - FCenter.X) / FRanges.XDown
      else
        JoyPos.X := (LoWord(wXpos) - FCenter.X) / FRanges.XUp;
      if LoWord(wYpos) < FCenter.Y then
        JoyPos.Y := (LoWord(wYpos) - FCenter.Y) / FRanges.YDown
      else
        JoyPos.Y := (LoWord(wYpos) - FCenter.Y) / FRanges.YUp;
      if axZ in FAxises then
        if LoWord(wZpos) < FCenter.Z then
          JoyPos.Z := (LoWord(wZpos) - FCenter.Z) / FRanges.ZDown
        else
          JoyPos.Z := (LoWord(wZpos) - FCenter.Z) / FRanges.ZUp;
      if axR in FAxises then
        if LoWord(dwRpos) < FCenter.R then
          JoyPos.R := (LoWord(dwRpos) - FCenter.R) / FRanges.RDown
        else
          JoyPos.R := (LoWord(dwRpos) - FCenter.R) / FRanges.RUp;
      if axU in FAxises then
        if LoWord(dwUpos) < FCenter.U then
          JoyPos.U := (LoWord(dwUpos) - FCenter.U) / FRanges.UDown
        else
          JoyPos.U := (LoWord(dwUpos) - FCenter.U) / FRanges.UUp;
      if axV in FAxises then
        if LoWord(dwVpos) < FCenter.V then
          JoyPos.V := (LoWord(dwVpos) - FCenter.V) / FRanges.VDown
        else
          JoyPos.V := (LoWord(dwVpos) - FCenter.V) / FRanges.VUp;
      CurrentTick := GetTickCount;
      MustDelay := CurrentTick < FPrevButtonTick + FRepeatButtonDelay;
      if (wButtons > 0) or (wButtons <> FPrevButtons) then
      begin
        if (not MustDelay) or (not FProcessedButtonOnce) then
        begin
          if wButtons >= FPrevButtons then
            DoButtonDown(wButtons)
          else
            DoButtonUp(wButtons);
          FProcessedButtonOnce := True;
        end;
      end
      else
      begin
        FPrevButtonTick := CurrentTick;
        FProcessedButtonOnce := False;
      end;
      FPrevButtons := wButtons;
      MustDelay := CurrentTick < FPrevMoveTick + FRepeatMoveDelay;
      if not CompareMem(@JoyPos, @CenterJoyPos, SizeOf(TJoyRelPos)) then
      begin
        if (not MustDelay) or (not FProcessedMoveOnce) then
        begin
          DoMove(JoyPos, wButtons);
          FProcessedMoveOnce := True;
        end;
      end
      else
      begin
        FPrevMoveTick := CurrentTick;
        FProcessedMoveOnce := False;
      end;
      FPrevPos := JoyPos;
      MustDelay := CurrentTick < FPrevPOVTick + FRepeatPOVDelay;
      if FHasPOV and ((dwPOV <> JOY_POVCENTERED) or (dwPOV <> FPrevPOV)) then
      begin
        if (not MustDelay) or (not FProcessedPOVOnce) then
        begin
          DoPOVChanged(dwPOV);
          FProcessedPOVOnce := True;
        end;
      end
      else
      begin
        FPrevPOVTick := CurrentTick;
        FProcessedPOVOnce := False;
      end;
      FPrevPOV := dwPOV;
    end;
end;

procedure TNLDJoystick.ProcessSimple(var Message: TMMJoyMsg);
var
  JoyPos: TJoyRelPos;
begin
  with Message do
    case Msg of
      MM_JOY1BUTTONDOWN, MM_JOY2BUTTONDOWN:
        DoButtonDown(Buttons);
      MM_JOY1BUTTONUP, MM_JOY2BUTTONUP:
        DoButtonUp(Buttons);
      MM_JOY1MOVE, MM_JOY2MOVE:
        begin
          JoyPos := FPrevPos;
          if XZPos < FCenter.X then
            JoyPos.X := (XZPos - FCenter.X) / FRanges.XDown
          else
            JoyPos.X := (XZPos - FCenter.X) / FRanges.XUp;
          if YPos < FCenter.Y then
            JoyPos.Y := (YPos - FCenter.Y) / FRanges.YDown
          else
            JoyPos.Y := (YPos - FCenter.Y) / FRanges.YUp;
          FPrevPos := JoyPos;
          DoMove(JoyPos, Buttons);
        end;
      MM_JOY1ZMOVE, MM_JOY2ZMOVE:
        begin
          JoyPos := FPrevPos;
          if XZPos < FCenter.Z then
            JoyPos.Z := (XZPos - FCenter.Z) / FRanges.ZDown
          else
            JoyPos.Z := (XZPos - FCenter.Z) / FRanges.ZUp;
          FPrevPos := JoyPos;
          DoMove(JoyPos, Buttons);
        end;
      else
        Dispatch(Message);
    end;
end;

procedure TNLDJoystick.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      FActive := Initialize(FAdvanced)
    else
    begin
      joyReleaseCapture(FID);
      KillTimer(FWindowHandle, DefTimerID);
      FActive := False;
    end;
  end;
end;

procedure TNLDJoystick.SetAdvanced(Value: Boolean);
begin
  if FAdvanced <> Value then
  begin
    if not Value then
      FAdvanced := Value
    else
      if FActive then
        Initialize(Value)
      else
        FAdvanced := Value;
  end;
end;

procedure TNLDJoystick.SetInterval(Value: Integer);
var
  JoyCaps: TJoyCaps;
begin
  if Value <> FInterval then
  begin
    if (Value <> 0) and FAdvanced then
    begin
      joyGetDevCaps(FID, @JoyCaps, SizeOf(JoyCaps));
      FInterval := Max(JoyCaps.wPeriodMin, Min(Value, JoyCaps.wPeriodMax));
      InitTimer;
    end
    else
      FInterval := 0;
  end;
end;

procedure TNLDJoystick.SetThreshold(Value: Double);
var
  JoyThreshold: UINT;
begin
  if FThreshold <> Value then
  begin
    FThreshold := Max(0.0, Min(Value, 1.0));
    joySetThreshold(FID, Round(FThreshold * FRanges.XUp));
    if joyGetThreshold(FID, @JoyThreshold) = JOYERR_NOERROR then
      FThreshold := JoyThreshold / FRanges.XUp;
  end;
end;

procedure TNLDJoystick.WndProc(var Message: TMessage);
var
  MMJoyMsg: TMMJoyMsg absolute Message;
begin
  Assert(SizeOf(TMMJoyMsg) = SizeOf(TMessage));
  if not FAdvanced then
  begin
    ProcessSimple(MMJoyMsg)
  end
  else if Message.Msg = WM_TIMER then
    ProcessAdvanced
  else
    Dispatch(Message);
end;

initialization

finalization
  if FJoystick1 <> nil then
    FJoystick1.Free;
  if FJoystick2 <> nil then
    FJoystick2.Free;

end.
