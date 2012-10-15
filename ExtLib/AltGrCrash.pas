// WorkAround for bug:
// Keyboard layout has been removed from list of keyboards but it stays in registry
// Fixed in Delphi 2009

unit AltGrCrash;

interface

uses
  Windows, SysUtils;

type
  PHack = ^THook;
  THook = packed record
    OpCodeCall : Byte;
    OFFTo      : Integer;
    OpCodeRet  : Byte;
  end;
  TBackup = THook;

  TBigProcHook = class
  private
    FOldProc, FNewProc: Pointer;
    FBackupped: Boolean;
    FHooked: Boolean;
    FOriginal: TBackup;
    procedure SetHooked(const Value: Boolean);
  protected
    procedure InstallHook(Hook: THook);
    procedure OverwriteProc;
  public
    constructor Create(AOldProc, ANewProc: Pointer; Install: Boolean = True);
    property Hooked: Boolean read FHooked write SetHooked;
  end;

implementation

uses
  Menus;

var
  FHook: TBigProcHook;

constructor TBigProcHook.Create(AOldProc, ANewProc: Pointer;
  Install: Boolean);
begin
  inherited Create;

  FOldProc := AOldProc;
  FNewProc := ANewProc;

  if Install then
    SetHooked(True);
end;

procedure TBigProcHook.InstallHook(Hook: THook);
var
  OldProtect: Cardinal;
begin
  // Change protection of oldproc memory
  if VirtualProtect(FOldProc, SizeOf(THook), PAGE_EXECUTE_READWRITE, OldProtect) then
  try
    if not FBackupped then
    begin
      Move(FOldProc^, FOriginal, SizeOf(THook));
      FBackupped := True;
    end;
    // Overwrite the old procedure
    Move(Hook, FOldProc^, SizeOf(THook));
  finally
    VirtualProtect(FOldProc, SizeOf(THook), OldProtect, OldProtect);
  end
  else
  begin
    RaiseLastOSError;
  end;
end;

procedure TBigProcHook.OverwriteProc;
// Overwrites the first few calls of OldProc with a call to NewProc and a Ret.
var
  Hook: THook;
begin
  // Create a tiny little redirection
  with Hook do begin
    OpCodeCall := $E8; // = CALL}
    OFFTo      := PAnsiChar(FNewProc) - PAnsiChar(FOldProc) - 5;
    OpCodeRet  := $C3; // = RET
  end;

  InstallHook(Hook);
end;

procedure TBigProcHook.SetHooked(const Value: Boolean);
begin
  // Toggle hook.
  if FHooked <> Value then
    if Value then
      OverwriteProc
    else
      InstallHook(FOriginal);

  FHooked := Value;
end;


// The replacement function
function SafeIsAltGrPressed: Boolean;
begin
  try
    FHook.Hooked := False;
    try
      Result := IsAltGRPressed;
    finally
      FHook.Hooked := True;
    end;
  except
    // Exception: Error when reading keyboard layout dll.
    Result := False;
  end;
end;

initialization
{$IFNDEF NoInitialization}
// only <= Delphi 2007
{$IFNDEF UNICODE}
  FHook := TBigProcHook.Create(@IsAltGRPressed, @SafeIsAltGrPressed);
{$ENDIF}
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
{$IFNDEF UNICODE}
  FHook.Hooked := False;
  FHook.Free;
{$ENDIF}
{$ENDIF NoFinalization}
end.
