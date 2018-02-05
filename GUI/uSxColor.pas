unit uSxColor;

interface

uses
  Graphics,
  uDButton;

type
  TSxColor = class(TDButton)
  private
    FColor: TColor;
    procedure Init;
    function ColorToStringNamed: string;
    procedure SetColor(const Value: TColor);

  public
    property Color: TColor read FColor write SetColor;
  end;

implementation

uses
  SysUtils,
  uColor,
  uNamedColors;

{ TSxColor }

function TSxColor.ColorToStringNamed: string;
begin
	Result := TNamedColors.GetName(Color);
  if Result = '' then
    Result := '#' + IntToHex(TRGBA(Color).R, 2) + IntToHex(TRGBA(Color).G, 2) + IntToHex(TRGBA(Color).B, 2)
end;

procedure TSxColor.Init;
begin
	if Color = clNone then
	begin
		Font.Color := clWindowText;
		Caption := 'None';
	end
	else
	begin
		Font.Color := NegMonoColor(Color);
		Caption := ColorToStringNamed;
	end;
end;

procedure TSxColor.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    inherited Color := FColor;
    Init;
  end;
end;

end.
