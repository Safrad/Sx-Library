//* File:     Lib\uMatrix.pas
//* Created:  2004-03-19
//* Modified: 2005-08-28
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMatrix;

interface

uses uTypes;

type
	TMatrix = class(TObject)
	private
		FXC, FYC: SG;
	public
		Data: array of array of F8; // [y, x]

		constructor Create;
		destructor Destroy; override;

		procedure Clear;
		procedure Mul(M0, M1: TMatrix);

		function IsEmpty: Boolean;
		function ToString(MultiLines: BG): string;

		property XC: SG read FXC;
		property YC: SG read FYC;
		procedure SetItemSize(X, Y: SG);
{		property ItemSize: UG read FItemSize write SetItemSize;
		property ItemSh: UG read FItemSh;
		property ItemMemSize: UG read FItemMemSize;
		property Count: UG read FItemCount;}
	end;

implementation

uses uStrings, uFormat;

// TMatrix

constructor TMatrix.Create;
begin
	inherited Create;
	FXC := 0;
	FYC := 0;
end;

destructor TMatrix.Destroy;
begin
	Clear;
	inherited Destroy;
end;

procedure TMatrix.Clear;
begin
{	for i := 0 to FY - 1 do
		SetLength(Data[i], 0);


	SetLength(Data, 0);}
	SetItemSize(0, 0);
end;

procedure TMatrix.SetItemSize(X, Y: SG);
var i: SG;
begin
	if Y <> FYC then
	begin
		if Y > FYC then
		begin
			SetLength(Data, Y);
			for i := FYC to Y - 1 do
				SetLength(Data[i], X);
		end
		else // Y < FY
		begin
			for i := FYC - 1 downto Y do
				SetLength(Data[i], 0);
			SetLength(Data, Y);
		end;
		FYC := Y;
	end;
	if X <> FXC then
	begin
		for i := 0 to Y - 1 do
			SetLength(Data[i], X);

		FXC := X;
	end;
end;

procedure TMatrix.Mul(M0, M1: TMatrix);
var
	x, y, i: SG;
	r: F8;
begin
	if (M0.XC = M1.YC) and (M0.XC = M1.YC) then
	begin
		SetItemSize(M1.XC, M0.YC);
		for y := 0 to {M0}YC - 1 do
		begin
			for x := 0 to {M1}XC - 1 do
			begin
				r := 0;
				for i := 0 to M0.XC{M1.YC} - 1 do
				begin
					r := r + M0.Data[i, y] * M1.Data[x, i];
				end;
				Data[x, y] := r;
			end;
		end;
	end;
end;

function TMatrix.IsEmpty: Boolean;
begin
	Result := (FXC = 0) and (FYC = 0);
end;

function TMatrix.ToString(MultiLines: BG): string;
var x, y: SG;
begin
	Result := {ClassName + ' ' +} NToS(FXC) + ' ' + CharTimes + ' ' + NToS(FYC) + ' (';
	if MultiLines then Result := Result + LineSep;
	for y := 0 to FYC - 1 do
	begin
		Result := Result + '(';
		for x := 0 to FXC - 1 do
		begin
			Result := Result + FToS(Data[x, y], ofDisplay) + ', ';
		end;
		if FXC > 0 then
			SetLength(Result, Length(Result) - 2);
		Result := Result + '), ';
		if MultiLines then Result := Result + LineSep;
	end;
	if FYC > 0 then
	begin
		SetLength(Result, Length(Result) - 2 - SG(MultiLines));
	end;
	Result := Result + ');';
end;

end.
