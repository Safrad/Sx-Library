//* File:     Lib\GUI\uDispl.pas
//* Created:  1999-07-01
//* Modified: 2007-08-20
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDispl;

interface

uses
	uTypes,
	uDBitmap, Windows, Graphics, Classes, StdCtrls;

const
	clAVideo = $009fff1f;
	clDVideo = $004f7f0f;
	clBVideo = $0000003f;
	clAClock = $002f2fff;
	clDClock = $0000008f;
	clBClock = $0000007f;
	clACalc = $000f0f0f;
	clDCalc = $00a0C0b4;
	clBCalc = $00a8C8bc;

type
	TDispl = class(TGraphicsObject)
	private
		FEnabled: Boolean;
		FFormat: string;
		FSizeX,
		FSizeY,
		FSpaceSX,
		FSpaceSY,
		FSizeT,
		FSpacing: U1;

		FSize: U1;
		FColorA,
		FColorD: TColor;

		procedure SetEnabled(Value: Boolean);
		procedure SetFormat(Value: string);
		procedure SetSizeX(Value: U1);
		procedure SetSizeY(Value: U1);
		procedure SetSpaceSX(Value: U1);
		procedure SetSpaceSY(Value: U1);
		procedure SetSizeT(Value: U1);
		procedure SetSpacing(Value: U1);
		procedure SetColorA(Value: TColor);
		procedure SetColorD(Value: TColor);
		procedure SetSize(Value: U1);
	public
		constructor Create;
		destructor Destroy; override;
	published
		property Enabled: Boolean read FEnabled write SetEnabled default False;
		property Format: string read FFormat write SetFormat;
		property SizeX: U1 read FSizeX write SetSizeX default 4;
		property SizeY: U1 read FSizeY write SetSizeY default 4;
		property SpaceSX: U1 read FSpaceSX write SetSpaceSX default 2;
		property SpaceSY: U1 read FSpaceSY write SetSpaceSY default 2;
		property SizeT: U1 read FSizeT write SetSizeT default 1;
		property Spacing: U1 read FSpacing write SetSpacing default 0;
		property ColorA: TColor read FColorA write SetColorA default clRed;
		property ColorD: TColor read FColorD write SetColorD default clMaroon;
		property Size: U1 read FSize write SetSize default 0;
	end;

procedure DisplDraw(BmpD: TDBitmap; Caption: string; X1, Y1: SG;
	Format: string;
	SizeX, SizeY,
	SpaceSX, SpaceSY, SizeT, Spacing: U1; ColorA, ColorD: TColor; Effect: TEffect;
	InfoOnly: Boolean); overload;

procedure DisplDraw(BmpD: TDBitmap; const Caption: string; const Displ: TDispl;
	X1, Y1: SG;
	Effect: TEffect); overload;

procedure DisplSize(const Displ: TDispl; out DisplWidth, DisplHeight: Integer);

procedure DisplDraw(BmpD: TDBitmap; const Caption: string; const Displ: TDispl;
	const Recta: TRect; const Alignment: TAlignment; const Layout: TTextLayout;
	const Effect: TEffect); overload;

implementation

uses
	SysUtils,
	uGraph;
const
	MaxChar = 2 + 10 + 26 - 1;
type
	TOneDisp = array[0..14] of U1;
const
	DispC: array[0..MaxChar] of TOneDisp = (
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), //
		(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2), //-
		(3, 3, 3, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0), //0
		(0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0), //1
		(3, 0, 3, 1, 3, 0, 3, 0, 0, 0, 0, 0, 0, 2, 2), //2
		(3, 0, 3, 1, 0, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //3
		(0, 3, 3, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 2, 2), //4
		(3, 3, 0, 1, 0, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //5
		(3, 3, 0, 1, 3, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //6
		(3, 0, 3, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0), //7
		(3, 3, 3, 1, 3, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //8
		(3, 3, 3, 1, 0, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //9
		(3, 3, 3, 1, 3, 3, 0, 0, 0, 0, 0, 0, 0, 2, 2), //A
		(2, 1, 2, 1, 1, 3, 3, 0, 2, 0, 0, 2, 0, 0, 2), //B
		(3, 3, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0), //C
		(2, 0, 3, 1, 1, 3, 3, 0, 2, 0, 0, 2, 0, 0, 0), //D
		(3, 3, 0, 1, 3, 0, 3, 0, 0, 0, 0, 0, 0, 2, 0), //E
		(3, 3, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0), //F
		(3, 3, 0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 2), //G
		(0, 3, 2, 1, 3, 3, 0, 0, 0, 0, 0, 0, 0, 2, 2), //H
		(2, 0, 1, 0, 0, 1, 2, 0, 2, 0, 0, 2, 0, 0, 0), //I
		(0, 0, 3, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0), //J
		(0, 1, 1, 1, 1, 1, 0, 0, 2, 2, 0, 2, 2, 0, 0), //K
		(0, 3, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0), //L
		(1, 3, 3, 0, 3, 3, 0, 2, 0, 2, 0, 0, 0, 0, 0), //M
		(0, 2, 2, 1, 3, 3, 0, 2, 0, 0, 0, 0, 2, 0, 0), //N
		(0, 0, 0, 1, 3, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //O
		(3, 3, 3, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2), //P
		(3, 3, 3, 1, 2, 3, 2, 0, 0, 0, 0, 0, 2, 0, 0), //Q
		(2, 2, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2), //R
		(3, 3, 0, 1, 0, 3, 3, 0, 0, 0, 0, 0, 0, 2, 2), //S
		(2, 1, 0, 1, 1, 0, 1, 0, 2, 0, 0, 2, 0, 0, 0), //T
		(0, 3, 3, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0), //U
		(0, 2, 0, 0, 3, 1, 1, 0, 0, 2, 2, 0, 0, 0, 0), //V
		(0, 3, 3, 1, 3, 3, 3, 0, 0, 0, 0, 2, 0, 0, 0), //W
		(0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 0, 2, 0, 0), //X
		(0, 1, 1, 1, 0, 1, 1, 2, 0, 2, 0, 2, 0, 0, 0), //Y
		(3, 0, 1, 1, 1, 0, 3, 0, 0, 2, 2, 0, 0, 0, 0));//Z

function Conv(C: Char): U1;
begin
	case C of
	'0'..'9': Result := 2 + Ord(C) - Ord('0');
	' ': Result := 0;
	'.', ',': Result := 0;
	'-': Result := 1;
	'A'..'Z': Result := 2 + 10 + Ord(C) - Ord('A');
	'a'..'z': Result := 2 + 10 + Ord(C) - Ord('a');
	else Result := 0;
	end;
	if Result > MaxChar then Result := MaxChar;
end;

constructor TDispl.Create;
begin
	inherited;
	Enabled := False;
	Format := '';
	SizeT := 1;
	SizeX := 4;
	SizeY := 4;
	SpaceSX := 2;
	SpaceSY := 2;
	Spacing := 0;
	ColorA := clRed;
	ColorD := clMaroon;
end;

destructor TDispl.Destroy;
begin
	inherited;
end;

procedure TDispl.SetEnabled(Value: Boolean);
begin
	if FEnabled <> Value then
	begin
		FEnabled := Value;
		Changed;
	end;
end;

procedure TDispl.SetFormat(Value: string);
begin
	if FFormat <> Value then
	begin
		FFormat := Value;
		Changed;
	end;
end;

procedure TDispl.SetSizeX(Value: U1);
begin
	if FSizeX <> Value then
	begin
		FSizeX := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSizeY(Value: U1);
begin
	if FSizeY <> Value then
	begin
		FSizeY := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSpaceSX(Value: U1);
begin
	if FSpaceSX <> Value then
	begin
		FSpaceSX := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSpaceSY(Value: U1);
begin
	if FSpaceSY <> Value then
	begin
		FSpaceSY := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSizeT(Value: U1);
begin
	if FSizeT <> Value then
	begin
		FSizeT := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSpacing(Value: U1);
begin
	if FSpacing <> Value then
	begin
		FSpacing := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetColorA(Value: TColor);
begin
	if FColorA <> Value then
	begin
		FColorA := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetColorD(Value: TColor);
begin
	if FColorD <> Value then
	begin
		FColorD := Value;
		if FEnabled then Changed;
	end;
end;

procedure TDispl.SetSize(Value: U1);
begin
	if FSize <> Value then
	begin
		case Value of
		0: 
		begin
			FSizeT := 0;
			FSpacing := 0;
			FSizeX := 0;
			FSizeY := 0;
			FSpaceSX := 0;
			FSpaceSY := 0;
		end;
		1: 
		begin
			FSizeT := 1;
			FSpacing := 1;
			FSizeX := 3;
			FSizeY := 3;
			FSpaceSX := 1;
			FSpaceSY := 1;
		end;
		2: 
		begin
			FSizeT := 2;
			FSpacing := 2;
			FSizeX := 5;
			FSizeY := 5;
			FSpaceSX := 1;
			FSpaceSY := 1;
		end;
		3:
		begin
			FSizeT := 3;
			FSpacing := 3;
			FSizeX := 6;
			FSizeY := 6;
			FSpaceSX := 2;
			FSpaceSY := 2;
		end;
		4:
		begin
			FSizeT := 4;
			FSpacing := 4;
			FSizeX := 10;
			FSizeY := 10;
			FSpaceSX := 3;
			FSpaceSY := 3;
		end;
		end;
		FSize := Value;
		if FEnabled then Changed;
	end;
end;
{ Segments: 8(7), #(14)

//    0
//  1   2
//    3
//  4   5
//    6

//      0
//    7   9
//  1   8   2
//
//   13   14
//
//  4  11   5
//   10  12
//      6

}
procedure DispXY(DC: U1;
	var SX1, SY1, SX2, SY2: SG; sx, sy, SizeX, SizeY, n, SizeT: U1);
var
	mx, my1, my2, my3: U1;
	t3: U1;
begin
	mx := (sx + SizeX shr 1);
	if (SizeT and 1) <> (sy and 1) then
	begin
		t3 := sy + 1;
	end
	else
		t3 := sy;
	//t3:=sy; if (SizeT>sy) and (SizeT and 1=0) then Inc(t3);
	my3 := SizeY + SizeY - 1 + sy + sy + t3;
	my1 := my3 shr 1;
	my2 := my3 shr 1 + (SizeT - 1) and 1;
	case DC of
	0: 
	begin
		SX1 := n + sx;
		SY1 := n;
		SX2 := SizeX - 1 - n + sx;
		SY2 := n;
	end;
	1: 
	begin
		SX1 := n;
		SY1 := n + sy;
		SX2 := n;
		SY2 := SizeY - 1 - n + sy;
	end;
	2: 
	begin
		SX1 := SizeX - 1 - n + sx + sx;
		SY1 := n + sy;
		SX2 := SizeX - 1 - n + sx + sx;
		SY2 := SizeY - 1 - n + sy;
	end;
	3: 
	begin
		if n and 1 = 0 then
		begin
			SX1 := n shr 1 + sx;
			SY1 := my1 - n shr 1;
			SX2 := SizeX - 1 - n shr 1 + sx;
			SY2 := my1 - n shr 1;
		end
		else
		begin
			SX1 := n shr 1 + sx + SizeT and 1;
			SY1 := my2 + n shr 1 + SizeT and 1;
			SX2 := SizeX - 1 - n shr 1 + sx - SizeT and 1;
			SY2 := my2 + n shr 1 + SizeT and 1;
		end;
	end;
	4: 
	begin
		SX1 := 0 + n;
		SY1 := SizeY - 0 + n + sy + t3;
		SX2 := 0 + n;
		SY2 := SizeY - 1 + SizeY - n + sy + t3;
	end;
	5: 
	begin
		SX1 := SizeX - 1 - n + sx + sx;
		SY1 := SizeY - 0 + n + sy + t3;
		SX2 := SizeX - 1 - n + sx + sx;
		SY2 := SizeY - 1 + SizeY - n + sy + t3;
	end;
	6: 
	begin
		SX1 := n + sx;
		SY1 := SizeY - 1 + SizeY - n + sy + sy + t3;
		SX2 := SizeX - 1 - n + sx;
		SY2 := SizeY - 1 + SizeY - n + sy + sy + t3;
	end;
	7:
	begin
		SX1 := 2 + n;
		SY1 := 2;
		SX2 := SizeX shr 1;
		SY2 := SizeY + 2 - n;
	end;
	8: 
	begin
		SX1 := mx + n;
		SY1 := sy;
		SX2 := mx + n;
		SY2 := my1 - 1;
	end;
	9: 
	begin
		SX1 := SizeX + 1 - n;
		SY1 := sy;
		SX2 := mx + 1;
		SY2 := my1 - 1 - n;
	end;
	10:
	begin
		SX1 := SizeX shr 1 + 1 + n;
		SY1 := SizeY;
		SX2 := 1;
		SY2 := 2 * SizeY + n;
	end;
	11: 
	begin
		SX1 := mx + n;
		SY1 := my2 + 1;
		SX2 := mx + n;
		SY2 := my3 - sy;
	end;
	12: 
	begin
		SX1 := mx + 1;
		SY1 := my2 + 1;
		SX2 := SizeX + sx;
		SY2 := my3 - 1 + n;
	end;
	13: 
	begin
		SX1 := 0;
		SY1 := SizeY + 1 + n;
		SX2 := SizeX shr 1 - 1;
		SY2 := SizeY + 1 + n;
	end;
	14: 
	begin
		SX1 := SizeX shr 1 + 1;
		SY1 := SizeY + 1 + n;
		SX2 := SizeX + 1;
		SY2 := SizeY + 1 + n;
	end;
	end;
end;

(*nction MyLength(var S: string): Integer;
var i: Integer;
begin
	Result := 0;
	for i := 1 to Length(S) do
	begin
		if S[i] <> '.' then Inc(Result);
	end;
end;

function MyLength2(var S: string): Integer;
var i: Integer;
begin
	Result := 0;
	for i := 1 to Length(S) do
	begin
		if {(S[i] <> '.') and} (S[i] <> ' ') {and (S[i] <> ':') then }Inc(Result);
	end;
end;
*)

procedure DisplDraw(BmpD: TDBitmap; Caption: string; X1, Y1: SG;
	Format: string;
	SizeX, SizeY,
	SpaceSX, SpaceSY, SizeT, Spacing: U1; CA, CD: TColor; Effect: TEffect;
	InfoOnly: Boolean; out DisplWidth, DisplHeight: Integer); overload;
label LPoint;
var
	B, D, D2, DC: U1;
	C: TColor;
	SX1, SY1, SX2, SY2: SG;
	CaptionIndex, MaxCaption: Integer;
	MaxS: U1;
	A: U1;
	X, Y: Integer;

	BmpWidth, BmpHeight: SG;
begin
	if SizeX < 1 then SizeX := 1;
	if SizeY < 1 then SizeY := 1;
	if SizeT < 1 then SizeT := 1;
	if SizeT > (SizeX + 1) shr 1 then SizeT := (SizeX + 1) shr 1;
	if SizeT > (SizeY + 1) shr 1 then SizeT := (SizeY + 1) shr 1;

	if InfoOnly = False then
	begin
//    PD := BmpD.ScanLine[0];
		BmpWidth := BmpD.Width;
		BmpHeight := BmpD.Height;
//    ByteXD := WidthToByteX(BmpWidth);
		Dec(BmpWidth);
		Dec(BmpHeight);
		CA := ColorToRGB(CA);
		CD := ColorToRGB(CD);
	end
	else
	begin
//    PD := nil;
		BmpWidth := 0;
		BmpHeight := 0;
//    ByteXD := 0;
	end;

	CaptionIndex := 1 + Length(Caption) - Length(Format);
	MaxCaption := Length(Caption);
	for B := 1 to Length(Format) do
	begin
		if (CaptionIndex > 0) and (CaptionIndex <= MaxCaption) then DC := Conv(Caption[CaptionIndex]) else DC := 0;
		case Format[B] of
		' ':
		begin
			if (CaptionIndex > 0) and (CaptionIndex <= MaxCaption) then Inc(CaptionIndex);
			Inc(X1, SizeX + Spacing + SpaceSX + SpaceSX);
		end;
		'.':
		begin
			LPoint:
			if (CaptionIndex <= 0) or (CaptionIndex > MaxCaption) or ((Caption[CaptionIndex] <> '.') and (Caption[CaptionIndex] <> DecimalSeparator)) then
				C := CD
			else
			begin
				Inc(CaptionIndex);
				C := CA;
			end;
			if InfoOnly = False then
				BmpD.Bar(X1, Y1 + 2 * SizeY + 3 * SpaceSY - 1,
					X1 + SizeT - 1, Y1 + 2 * SizeY + 3 * SpaceSY - 2 + SizeT, C, ef16);
			if SizeT + 2 > 0 then Inc(X1, SizeT + 2);
//      if Spacing < SizeT + 2 then Inc(X1, SizeT + 2 - Spacing);
		end;
		',':
		begin
			if (CaptionIndex <= 0) or (CaptionIndex > MaxCaption) or (Caption[CaptionIndex] <> ',') then
				C := CD
			else
			begin
				C := CA;
			end;
			Inc(CaptionIndex);
			if InfoOnly = False then
				BmpD.Bar(X1, Y1 + 2 * SizeY + 3 * SpaceSY - 1,
					X1 + SizeT - 1, Y1 + 2 * SizeY + 3 * SpaceSY - 2 + SizeT + SizeT, C, Effect);
			if Spacing < SizeT + 2 then Inc(X1, SizeT + 2 - Spacing);
		end;
		':':
		begin
			if (CaptionIndex <= 0) or (CaptionIndex > MaxCaption) or (Caption[CaptionIndex] <> ':') then
				C := CD
			else
			begin
				C := CA;
			end;
			Inc(CaptionIndex);
			if InfoOnly = False then
			begin
				X := X1;
				Y := Y1 + 3 * SpaceSY - 1;
				BmpD.Bar(X, Y,
					X + SizeT - 1, Y + SizeT - 1, C, ef16);
				Y := Y1 + SizeY + SizeY div 2 + 2 * SpaceSY - 1;
				BmpD.Bar(X, Y,
					X + SizeT - 1, Y + SizeT - 1, C, ef16);
			end;
			if SizeT + 2 > 0 then Inc(X1, SizeT + 2);
		end
		else
		begin
			if Format[B] = DecimalSeparator then
			begin
				goto LPoint;
				A := 0; // Delphi Warning
			end
			else if Format[B] = '8' then
			begin
				A := 1;
				MaxS := 6
			end
			else
			begin
				A := 2;
				MaxS := 14;
			end;
			for D := 0 to MaxS do
			begin
				if DispC[DC, D] and A = 0 then
				begin
					C := ColorToRGB(CD);
					if CD = clNone then Continue;
				end
				else
				begin
					C := ColorToRGB(CA);
					if CA = clNone then Continue;
				end;
				if (D = 3) and (Format[B] <> '8') then Continue;
				for D2 := 0 to SizeT - 1 do
				begin
					DispXY(D, SX1, SY1, SX2, SY2,
						SpaceSX, SpaceSY, SizeX, SizeY, D2, SizeT);
					if InfoOnly then
					begin
						if SY2 >= DisplHeight then DisplHeight := SY2 + 1;
					end
					else
					begin
						Inc(SX1, X1);
						Inc(SY1, Y1);
						Inc(SX2, X1);
						Inc(SY2, Y1);
						{$ifndef NoCheck}
						if (SX1 < 0) then
							SX1 := 0
						else if (SX1 > BmpWidth) then
							SX1 := BmpWidth;
						if (SY1 < 0) then
							SY1 := 0
						else if (SY1 > BmpHeight) then
							SY1 := BmpHeight;

						if (SX2 < 0) then
							SX2 := 0
						else if (SX2 > BmpWidth) then
						SX2 := BmpWidth;
						if (SY2 < 0) then
							SY2 := 0
						else if (SY2 > BmpHeight) then
							SY2 := BmpHeight;
						{$endif}
						BmpD.Line(SX1, SY1, SX2, SY2, C, Effect);
					end;
				end;
			end;
			Inc(X1, SizeX + Spacing + SpaceSX + SpaceSX);
			Inc(CaptionIndex);
		end;
		end;
	end;
	DisplWidth := X1;
end;

procedure DisplSize(const Displ: TDispl; out DisplWidth, DisplHeight: Integer);
begin
	DisplDraw(nil, '', 0, 0,
		Displ.FFormat,
		Displ.SizeX, Displ.SizeY,
		Displ.SpaceSX, Displ.SpaceSY,
		Displ.SizeT, Displ.Spacing,
		clBlack, clBlack, ef00,
		True, DisplWidth, DisplHeight);
end;

procedure DisplDraw(BmpD: TDBitmap; const Caption: string; const Displ: TDispl;
	const Recta: TRect; const Alignment: TAlignment; const Layout: TTextLayout;
	const Effect: TEffect);
var
	X1, Y1: Integer;
	DisplWidth, DisplHeight: Integer;
begin
	DisplWidth := 0;
	DisplHeight := 0;
	if (LayOut <> tlTop) or (Alignment <> taLeftJustify) then
		DisplSize(Displ, DisplWidth, DisplHeight);
	case Layout of
	tlTop: 
		Y1 := Recta.Top + 2;
	tlBottom: 
		Y1 := Recta.Bottom - DisplHeight - 2;
	else
		Y1 := (Recta.Top + Recta.Bottom - DisplHeight) div 2;
	end;

	case Alignment of
	taLeftJustify: 
		X1 := 2 + Recta.Left;
	taRightJustify: 
		X1 := Recta.Right - DisplWidth - 2;
	else
		X1 := (Recta.Left + Recta.Right - DisplWidth) div 2;
	end;

	DisplDraw(BmpD, Caption, X1, Y1,
		Displ.FFormat,
		Displ.FSizeX, Displ.FSizeY,
		Displ.FSpaceSX, Displ.FSpaceSY, Displ.FSizeT, Displ.FSpacing,
		Displ.FColorA, Displ.FColorD, Effect, False, DisplWidth, DisplHeight);
end;

procedure DisplDraw(BmpD: TDBitmap; const Caption: string; const Displ: TDispl;
	X1, Y1: SG;
	Effect: TEffect);
var DisplWidth, DisplHeight: Integer;
begin
	DisplWidth := 0;
	DisplHeight := 0;
	DisplDraw(BmpD, Caption, X1, Y1, Displ.FFormat, Displ.FSizeX, Displ.FSizeY,
		Displ.FSpaceSX, Displ.FSpaceSY, Displ.FSizeT, Displ.FSpacing,
		Displ.FColorA, Displ.FColorD,
		Effect, False, DisplWidth, DisplHeight);
end;

procedure DisplDraw(BmpD: TDBitmap; Caption: string; X1, Y1: SG;
	Format: string;
	SizeX, SizeY,
	SpaceSX, SpaceSY, SizeT, Spacing: U1; ColorA, ColorD: TColor; Effect: TEffect;
	InfoOnly: Boolean);
var DisplWidth, DisplHeight: Integer;
begin
	DisplWidth := 0;
	DisplHeight := 0;
	DisplDraw(BmpD, Caption, X1, Y1, Format, SizeX, SizeY,
		SpaceSX, SpaceSY, SizeT, Spacing,
		ColorA, ColorD,
		Effect, False, DisplWidth, DisplHeight);
end;

end.
