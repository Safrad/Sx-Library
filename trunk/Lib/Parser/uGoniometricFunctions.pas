//* File:     Lib\Parser\uGoniometricFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-05-27
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uGoniometricFunctions;

interface

type
	TGoniometricFormat = (gfRad, gfGrad, gfCycle, gfDeg);
var
	GoniometricFormat: TGoniometricFormat;

implementation

uses
	Math,
	uNamespace, uVector;

function CorrectFormat(const X: TVector): TVector;
begin
	case GoniometricFormat of
	gfRad: Result := X;
	gfGrad: Result := MultiplyVector(X, NumToVector(((2 * pi) / 400)));
	gfCycle: Result := MultiplyVector(X, NumToVector((2 * pi)));
	gfDeg: Result := MultiplyVector(X, NumToVector((2 * pi) / 360));
	end;
end;

function PICOnstant: TVector;
begin
	Result := NumToVector(pi);
end;

function Sine(const X: TVector): TVector;
begin
	Result := NumToVector(Sin(VectorToNum(CorrectFormat(X))));
end;

function Cosine(const X: TVector): TVector;
begin
	Result := NumToVector(Cos(VectorToNum(X)));
end;

function Tangent(const X: TVector): TVector;
begin
	Result := NumToVector(Tan(VectorToNum(X)));
end;

function ArcSin(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcSin(VectorToNum(X)));
end;

function ArcCos(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcCos(VectorToNum(X)));
end;

{function ArcTan(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcTan(VectorToNum(X)));
end;}

function Sinh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Sinh(VectorToNum(X)));
end;

function Cosh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Cosh(VectorToNum(X)));
end;

function Tanh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Tanh(VectorToNum(X)));
end;

function ArcSinh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcSinh(VectorToNum(X)));
end;

function ArcCosh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcCosh(VectorToNum(X)));
end;

function ArcTanh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcTanh(VectorToNum(X)));
end;

initialization
	AddFunction('Goniometic', 'pi', PIConstant, 'http://en.wikipedia.org/wiki/Pi');
	AddFunction('Goniometic', 'Sin', Sine, 'http://en.wikipedia.org/wiki/Sine');
	AddFunction('Goniometic', 'Cos', Cosine, 'http://en.wikipedia.org/wiki/Cosine');
	AddFunction('Goniometic', 'Tan', Tangent, 'http://en.wikipedia.org/wiki/Tangent');
	AddFunction('Goniometic', 'ArcSin', ArcSin, 'http://en.wikipedia.org/wiki/ArcSine');
	AddFunction('Goniometic', 'ArcCos', ArcCos, 'http://en.wikipedia.org/wiki/ArcCosine');

	// Hyperbolic
	AddFunction('Goniometic', 'Sinh', Sinh, 'http://en.wikipedia.org/wiki/Sineh');
	AddFunction('Goniometic', 'Cosh', Cosh, 'http://en.wikipedia.org/wiki/Cosineh');
	AddFunction('Goniometic', 'Tanh', Tanh, 'http://en.wikipedia.org/wiki/Tangenth');
	AddFunction('Goniometic', 'ArcSinh', ArcSinh, 'http://en.wikipedia.org/wiki/ArcSineh');
	AddFunction('Goniometic', 'ArcCosh', ArcCosh, 'http://en.wikipedia.org/wiki/ArcCosineh');
	AddFunction('Goniometic', 'ArcTanh', ArcTanh, 'http://en.wikipedia.org/wiki/ArcTangenth');
end.
