unit uWindowsKeyboard;

interface

uses
  uTypes,
  uCustomKeyboard,
  Winapi.Windows;

type
  TWindowsKeyboard = class(TCustomKeyboard)
  private
    FKeyboardState: TKeyboardState;
    FUpdated: BG;
  public
    procedure ReadState; override;
    function IsKeyDown(const AKeyIndex: SG): BG; override;
  end;

implementation

{ TWindowsKeyboard }

function TWindowsKeyboard.IsKeyDown(const AKeyIndex: SG): BG;
begin
  if FUpdated then
    Result := (FKeyboardState[AKeyIndex] and $80) <> 0
  else
    Result := GetKeyState(AKeyIndex) <> 0;
end;

procedure TWindowsKeyboard.ReadState;
var
  i: SG;
  FNewKeyboardState: TKeyboardState;
begin
  FUpdated := True;
  FNewKeyboardState := Default(TKeyboardState);
  GetKeyboardState(FNewKeyboardState);
  for i := Low(FNewKeyboardState) to High(FNewKeyboardState) do
  begin
    if FKeyboardState[i] <> FNewKeyboardState[i] then
    begin
      if FNewKeyboardState[i] and $80 <> 0 then
      begin
        if Assigned(OnKeyDown) then
          OnKeyDown(Self, i);
      end
      else if FNewKeyboardState[i] and $80 = 0 then
      begin
        if Assigned(OnKeyUp) then
          OnKeyUp(Self, i);
      end;
    end;
  end;
  FKeyboardState := FNewKeyboardState;
end;

end.
