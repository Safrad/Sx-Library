unit uSxColor;

interface

uses
  UITypes,
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
    Result := ColorToHTMLString(Color);
end;

procedure TSxColor.Init;
begin
	if Color = TColorRec.SysNone then
	begin
		Font.Color := TColorRec.SysWindowText;
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
