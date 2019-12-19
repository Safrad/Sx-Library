unit uCustomKeyboard;

interface

uses
  uTypes;

type
  TOnKeyUpDownEvent = procedure(Sender: TObject; const AKeyCode: Word) of object;

  TCustomKeyboard = class
  private
    FOnKeyDown: TOnKeyUpDownEvent;
    FOnKeyUp: TOnKeyUpDownEvent;
    procedure SetOnKeyDown(const Value: TOnKeyUpDownEvent);
    procedure SetOnKeyUp(const Value: TOnKeyUpDownEvent);
  public
    procedure ReadState; virtual;
    function IsKeyDown(const AKeyIndex: SG): BG; virtual;

    property OnKeyDown: TOnKeyUpDownEvent read FOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TOnKeyUpDownEvent read FOnKeyUp write SetOnKeyUp;
  end;

implementation

{ TCustomKeyboard }

function TCustomKeyboard.IsKeyDown(const AKeyIndex: SG): BG;
begin
  Result := False;
end;

procedure TCustomKeyboard.ReadState;
begin
  // No code
end;

procedure TCustomKeyboard.SetOnKeyDown(const Value: TOnKeyUpDownEvent);
begin
  FOnKeyDown := Value;
end;

procedure TCustomKeyboard.SetOnKeyUp(const Value: TOnKeyUpDownEvent);
begin
  FOnKeyUp := Value;
end;

end.
