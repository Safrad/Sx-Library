//* File:     Lib\uSunInfo.pas
//* Created:  1999-07-01
//* Modified: 2004-09-26
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSunInfo;

interface

// 17.12.1999 sunrise at 7:52 -> 15, -49.7
type TSunPos = (spUD, spU, spD, spUA, spDA);

procedure SunOnOff(const LongitudeX, LatitudeY, TimeZone: Extended;
	Year: Integer; Month, Day: Byte;
	var TimeOn, TimeOff: Extended;
	var AzimuthOn, AzimuthOff: Extended;
	var SunPos: TSunPos);

implementation

uses uAdd;

procedure SunOnOff(const LongitudeX, LatitudeY, TimeZone: Extended;
	Year: Integer; Month, Day: Byte;
	var TimeOn, TimeOff: Extended;
	var AzimuthOn, AzimuthOff: Extended;
	var SunPos: TSunPos);
{ This program by Roger W. Sinnott calculates the times of sunrise
	and sunset on any date, accurate to the minute within several
	centuries of the present. It correctly describes what happens in the
	arctic and antarctic regions, where the Sun may not rise or set on
	a given date. Enter north latitudes positive, west longitudes
	negative. for the time zone, enter the number of hours west of
	Greenwich (e.g., 5 for EST, 4 for EDT). The calculation is
	discussed in Sky & Telescope for August 1994, page 84. }
const
	P2 = 2 * Pi;
	DR = Pi / 180;
	K1 = 15 * DR * 1.0027379;
var
	L5: Extended;
	aA, aD: array[1..2] of Extended;
	F, J, S, A, Z0, T, TT, T0, L, V, U, W, A5, D5, R5,
	H2, D2, V0, C, Z, A0, D0, A2,
	V2, V1, B, D, M8, W8, E, T3, H7, N7, D7, AZ, Z1, DA, DD,
	P: Extended;
	C0: Byte;

	procedure Calendar;
	var J3, D1, G: Extended;
	begin
		// Calendar --> JD
		if Year < 1583 then G := 0 else G := 1;
		D1 := INT(Day); F := Day - D1 - 0.5;
		J := -INT(7 * (INT((Month + 9) / 12) + Year) / 4);
		if G <> 0 then
		begin
			S := Sgn(Month - 9);
			A := ABS(Month - 9);
			J3 := INT(Year + S * INT(A / 7));
			J3 := -INT((INT(J3 / 100) + 1) * 3 / 4)
		end
		else
		begin
			S := 0;
			A := 0;
			J3 := 0;
		end;
		J := J + INT(275 * Month / 9) + D1 + G * J3;
		J := J + 1721027 + 2 * G + 367 * Year;
		if F >= 0 then Exit;
		F := F + 1; J := J - 1;
	end;

	procedure LST;
	begin
		// LST at 0h zone time
		T0 := T / 36525;
		S := 24110.5 + 8640184.813 * T0;
		S := S + 86636.6 * Z0 + 86400 * L5;
		S := S / 86400; S := S - INT(S);
		T0 := S * 360 * DR;
	end;

	procedure ProcSunPos;
	// Fundamental arguments (Van Flandern & Pulkkinen, 1979)
	var G: Extended;
	begin
		L := 0.779072 + 0.00273790931 * T;
		G := 0.993126 + 0.0027377785 * T;
		L := L - INT(L); G := G - INT(G);
		L := L * P2; G := G * P2;
		V := 0.39785 * SIN(L);
		V := V - 0.01000 * SIN(L - G);
		V := V + 0.00333 * SIN(L + G);
		V := V - 0.00021 * TT * SIN(L);
		U := 1 - 0.03349 * COS(G);
		U := U - 0.00014 * COS(2 * L);
		U := U + 0.00008 * COS(L);
		W := -0.00010 - 0.04129 * SIN(2 * L);
		W := W + 0.03211 * SIN(G);
		W := W + 0.00104 * SIN(2 * L - G);
		W := W - 0.00035 * SIN(2 * L + G);
		W := W - 0.00008 * TT * SIN(G);
		// Compute Sun's RA and Dec
		S := W / Sqrt(U - V * V);
		A5 := L + ArcTan(S / SQRt(1 - S * S));
		S := V / SQRt(U); D5 := ArcTan(S / SQRt(1 - S * S));
		R5 := 1.00021 * Sqrt(U);
	end;

	procedure TestHour;
	var L0, L2, H0, H1, D1: Extended;
	begin
		// Test an hour for an event
		L0 := T0 + C0 * K1; L2 := L0 + K1;
		H0 := L0 - A0; H2 := L2 - A2;
		H1 := (H2 + H0) / 2; // Hour angle,
		D1 := (D2 + D0) / 2; // declination, at half hour
		if C0 = 0 then V0 := S * SIN(D0) + C * COS(D0) * COS(H0) - Z;
		V2 := S * SIN(D2) + C * COS(D2) * COS(H2) - Z;
		if Sgn(V0) = Sgn(V2) then Exit;
		V1 := S * SIN(D1) + C * COS(D1) * COS(H1) - Z;
		A := 2 * V2 - 4 * V1 + 2 * V0; B := 4 * V1 - 3 * V0 - V2;
		D := B * B - 4 * A * V0; if D < 0 then Exit;
		D := SQRt(D);

		E := ( - B + D) / (2 * A);
		if (E > 1) or (E < 0) then E := ( - B - D) / (2 * A);
		T3 := C0 + E + 1 / 120; // Round off

		H7 := H0 + E * (H2 - H0);
		N7 := -COS(D1) * SIN(H7);
		D7 := C * SIN(D1) - S * COS(D1) * COS(H7);
		AZ := ArcTan(N7 / D7) / DR;
		if D7 < 0 then AZ := AZ + 180;
		if AZ < 0 then AZ := AZ + 360;
		if AZ > 360 then AZ := AZ - 360;

		if (V0 < 0) and (V2 > 0) then
		begin
			TimeOn := T3;
			AzimuthOn := AZ;
		end;
		if (V0 < 0) and (V2 > 0) then M8 := 1;
		if (V0 > 0) and (V2 < 0) then
		begin
			TimeOff := T3;
			AzimuthOff := AZ;
		end;
		if (V0 > 0) and (V2 < 0) then W8 := 1;
	end;

begin
	if Abs(Year) > 900000 then Year := Sgn(Year) * 900000;
	L5 := LongitudeX / 360; Z0 := TimeZone / 24;
	Calendar;
	T := (J - 2451545) + F;
	TT := T / 36525 + 1; // TT = centuries from 1900.0
	LST; T := T + Z0;
	// Get Sun's Position
	ProcSunPos; aA[1] := A5; aD[1] := D5;
	T := T + 1;
	ProcSunPos; aA[2] := A5; aD[2] := D5;
	if aA[2] < aA[1] then aA[2] := aA[2] + P2;
	Z1 := DR * 90.833; // Zenith dist.
	S := SIN(LatitudeY * DR); C := COS(LatitudeY * DR);
	Z := COS(Z1); M8 := 0; W8 := 0;
	A0 := aA[1]; D0 := aD[1];
	DA := aA[2] - aA[1]; DD := aD[2] - aD[1];
	C0 := 0;
	repeat
		P := (C0 + 1) / 24;
		A2 := aA[1] + P * DA; D2 := aD[1] + P * DD;
		TestHour;
		A0 := A2; D0 := D2; V0 := V2;
		Inc(C0);
	until C0 = 24;
	// Special-message routine
	SunPos := spUD;
	if (M8 = 0) and (W8 = 0) then
	begin
		if V2 < 0 then SunPos := spUA;
		if V2 > 0 then SunPos := spDA;
	end
	else
	begin
		if M8 = 0 then SunPos := spU;
		if W8 = 0 then SunPos := spD;
	end;
end;

end.
