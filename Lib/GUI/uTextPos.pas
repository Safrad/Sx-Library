unit uTextPos;

interface

uses
	uTypes,
	Windows, Graphics;

type
	TTextPos = class
	private
		FText: string;
		FTextOutput: BG;
		function GetLength(const Input: string): SG;

	public
		// Options
		Bmp: TBitmap;
		Offset: TPoint;
		MaxLineWidth: SG;
		LineHeight: SG;

		TextPos: TPoint;
		MaxTextPos: TPoint;
		procedure AddText(Input: string);
		procedure AddTextCenter(const Input: string; const Width: SG);
		procedure AddTextRight(const Input: string; const Width: SG);
		procedure AddLines(const Input: string);
		procedure AddLine(Input: string);
		procedure NewLine;
		function PosForRect(const Width, Height: SG): BG;
		procedure AddRect(const Width, Height: SG);

		property Text: string read FText;
		property TextOutput: BG read FTextOutput write FTextOutput;
	end;

implementation

uses
	uStrings,
	SysUtils, Math;

{ TTextPos }

procedure TTextPos.AddLine(Input: string);
begin
	AddText(Input);
	NewLine;
end;

procedure TTextPos.AddLines(const Input: string);
var
	InLineIndex: SG;
	InputLength: SG;
begin
	InLineIndex := 1;
	InputLength := Length(Input);
	while InLineIndex < InputLength do
	begin
		if InLineIndex <> 1 then
			NewLine;
		AddText(ReadToNewLine(Input, InLineIndex));
	end;
end;

procedure TTextPos.AddRect(const Width, Height: SG);
begin
	Inc(TextPos.X, Width);
end;

procedure TTextPos.AddText(Input: string);
var
	l: SG;
begin
	l := GetLength(Input);

	if PosForRect(l, 0) then
	begin
		DelBeginSpace(Input);
		l := GetLength(Input);
	end;

	if Assigned(Bmp) then
		Bmp.Canvas.TextOut(TextPos.X - Offset.X, TextPos.Y - Offset.Y, Input);
	if FTextOutput then
		FText := FText + Input;

	Inc(TextPos.X, l);
	MaxTextPos.X := Max(MaxTextPos.X, TextPos.X);
	MaxTextPos.Y := TextPos.Y + LineHeight - 1;
end;

procedure TTextPos.AddTextCenter(const Input: string; const Width: SG);
var
	LastX: SG;
begin
	LastX := TextPos.X;
	if Assigned(Bmp) then
	begin
		AddRect((Width - Bmp.Canvas.TextWidth(Input)) div 2, 0);
	end;
	AddText(Input);
	TextPos.X := LastX + Width;
end;

procedure TTextPos.AddTextRight(const Input: string; const Width: SG);
var
	LastX: SG;
begin
	LastX := TextPos.X;
	if Assigned(Bmp) then
		AddRect(Width - Bmp.Canvas.TextWidth(Input), 0);
	AddText(Input);
	TextPos.X := LastX + Width;
end;

function TTextPos.GetLength(const Input: string): SG;
begin
	Result := -1;
	if Assigned(Bmp) then
		Result := Bmp.Canvas.TextWidth(Input);
	if FTextOutput then
		Result := Length(Input);
end;

procedure TTextPos.NewLine;
begin
	TextPos.X := 0;
	if Assigned(Bmp) then
		Inc(TextPos.Y, LineHeight)
	else if FTextOutput then
		Inc(TextPos.Y);

	if FTextOutput then
		FText := FText + FullSep;
end;

function TTextPos.PosForRect(const Width, Height: SG): BG;
begin
	if (TextPos.X <> 0) and ((TextPos.X + Width) >= MaxLineWidth) then
	begin
		NewLine;
		Result := True;
	end
	else
		Result := False;
end;

end.
