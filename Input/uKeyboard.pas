unit uKeyboard;

interface

uses
  uTypes,
  Windows;

type
  TOnKeyUpDownEvent = procedure(Sender: TObject; const AKeyCode: Word) of object;

  TKeyboard = class
  private
    FKeyboardState: TKeyboardState;
    FOnKeyDown: TOnKeyUpDownEvent;
    FOnKeyUp: TOnKeyUpDownEvent;
    procedure SetOnKeyDown(const Value: TOnKeyUpDownEvent);
    procedure SetOnKeyUp(const Value: TOnKeyUpDownEvent);
  public
    procedure ReadState;
    function IsKeyDown(const AKeyIndex: SG): BG;
    property OnKeyDown: TOnKeyUpDownEvent read FOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TOnKeyUpDownEvent read FOnKeyUp write SetOnKeyUp;
  end;

implementation

{ TKeyboard }

function TKeyboard.IsKeyDown(const AKeyIndex: SG): BG;
begin
  Result := (FKeyboardState[AKeyIndex] and $80) <> 0;
end;

procedure TKeyboard.ReadState;
var
  i: SG;
  FNewKeyboardState: TKeyboardState;
begin
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

procedure TKeyboard.SetOnKeyDown(const Value: TOnKeyUpDownEvent);
begin
  FOnKeyDown := Value;
end;

procedure TKeyboard.SetOnKeyUp(const Value: TOnKeyUpDownEvent);
begin
  FOnKeyUp := Value;
end;

end.
