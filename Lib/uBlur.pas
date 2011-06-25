// * File:     Lib\uBlur.pas
// * Created:  2009-12-31
// * Modified: 2009-12-31
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uBlur;

interface

uses uTypes;

type
	// TFlo = F4;
	TFlo = F8;
	// TFlo = FA;

	PPointDS = ^TPointDS;

	TPointDS = record
		X, Y: TFlo;
		Selected: BG;
	end;

	PVector = ^TVector;

	TVector = record
		S: TPointDS;
//		M: TPointDS;
		E: TPointDS;
	end;

	PBlur = ^TBlur;
	TBlur = record
		Intensity: TFlo;
		Direction: TFlo;
		x, y: TFlo;
//		Fixed: BG;
	end;

	TBlurArray = class
		FData: PBlur;
		FWidth: UG;
		FHeight: UG;
	public
		constructor Create;
		destructor Destroy; override;
		procedure SetSize(const Width, Height: UG);
		function GetAddr(const x, y: UG): PBlur;
		function GetAddrChecked(const x, y: SG): PBlur;
		property Data: PBlur read FData;
	end;

function GetLength(const FloPoint: TFloPoint; const P: TPointDS): TFlo; overload;
function GetLength(const X, Y: SG; const P: TPointDS): TFlo;  overload;

implementation

{ BlurClass }

constructor TBlurArray.Create;
begin
	inherited;
	FData := nil;
end;

destructor TBlurArray.Destroy;
begin
	FreeMem(FData);
	FData := nil;
	inherited;
end;

function TBlurArray.GetAddr(const x, y: UG): PBlur;
begin
	Result := PBlur(UG(FData) + (y * FWidth + x) * SizeOf(TBlur));
end;

function TBlurArray.GetAddrChecked(const x, y: SG): PBlur;
begin
	if (x >= 0) and (x < SG(FWidth)) and (y >= 0) and (y < SG(FHeight)) then
		Result := PBlur(UG(FData) + (UG(y) * FWidth + UG(x)) * SizeOf(TBlur))
	else
		Result := nil;
end;

procedure TBlurArray.SetSize(const Width, Height: UG);
begin
	if (Width <> FWidth) or (Height <> FHeight) then
	begin
		FWidth := Width;
		FHeight := Height;
		FreeMem(FData);
		GetMem(FData, Width * Height * SizeOf(TBlur));
	end;
end;


function GetLength(const FloPoint: TFloPoint; const P: TPointDS): TFlo;
begin
	Result := Sqr(FloPoint.X - P.X) + Sqr(FloPoint.Y - P.Y);
end;

function GetLength(const X, Y: SG; const P: TPointDS): TFlo;
begin
	Result := Sqr(X - P.X) + Sqr(Y - P.Y);
end;

end.
