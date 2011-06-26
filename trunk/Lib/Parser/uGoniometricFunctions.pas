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

function CorrectFormat(const X: TVector): TVector; // XToRad
begin
	case GoniometricFormat of
	gfRad: Result := X;
	gfGrad: Result := MultiplyVector(X, NumToVector(((2 * pi) / 400)));
	gfCycle: Result := MultiplyVector(X, NumToVector((2 * pi)));
	gfDeg: Result := MultiplyVector(X, NumToVector((2 * pi) / 360));
	end;
end;

function CorrectFormat2(const X: TVector): TVector; // RadToResult
begin
	case GoniometricFormat of
	gfRad: Result := X;
	gfGrad: Result := DivideVector(X, NumToVector(((2 * pi) / 400)));
	gfCycle: Result := DivideVector(X, NumToVector((2 * pi)));
	gfDeg: Result := DivideVector(X, NumToVector((2 * pi) / 360));
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
	Result := NumToVector(Cos(VectorToNum(CorrectFormat(X))));
end;

function Tangent(const X: TVector): TVector;
begin
	Result := NumToVector(Tan(VectorToNum(CorrectFormat(X))));
end;

function ArcSin(const X: TVector): TVector;
begin
	Result := CorrectFormat2(NumToVector(Math.ArcSin(VectorToNum(X))));
end;

function ArcCos(const X: TVector): TVector;
begin
	Result := CorrectFormat2(NumToVector(Math.ArcCos(VectorToNum(X))));
end;

{function ArcTan(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcTan(VectorToNum(X)));
end;}

function Sinh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Sinh(VectorToNum(CorrectFormat(X))));
end;

function Cosh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Cosh(VectorToNum(CorrectFormat(X))));
end;

function Tanh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Tanh(VectorToNum(CorrectFormat(X))));
end;

function ArcSinh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcSinh(VectorToNum(CorrectFormat(X))));
end;

function ArcCosh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcCosh(VectorToNum(CorrectFormat(X))));
end;

function ArcTanh(const X: TVector): TVector;
begin
	Result := NumToVector(Math.ArcTanh(VectorToNum(CorrectFormat(X))));
end;

initialization
	AddFunction('Goniometic', 'PI', PIConstant, 'http://en.wikipedia.org/wiki/Pi');
	AddFunction('Goniometic', 'Sin', Sine, 'http://en.wikipedia.org/wiki/Sine');
	AddFunction('Goniometic', 'Cos', Cosine, 'http://en.wikipedia.org/wiki/Cosine');
	AddFunction('Goniometic', 'Tan', Tangent, 'http://en.wikipedia.org/wiki/Tangent');
	AddFunction('Goniometic', 'ArcSin', ArcSin, 'http://en.wikipedia.org/wiki/Arcsine');
	AddFunction('Goniometic', 'ArcCos', ArcCos, 'http://en.wikipedia.org/wiki/Arccosine');

	// Hyperbolic
	AddFunction('Goniometic', 'Sinh', Sinh, 'http://en.wikipedia.org/wiki/Hyperbolic_sine');
	AddFunction('Goniometic', 'Cosh', Cosh, 'http://en.wikipedia.org/wiki/Hyperbolic_cosine');
	AddFunction('Goniometic', 'Tanh', Tanh, 'http://en.wikipedia.org/wiki/Hyperbolic_tangent');
	AddFunction('Goniometic', 'ArcSinh', ArcSinh, 'http://en.wikipedia.org/wiki/Arcsinh');
	AddFunction('Goniometic', 'ArcCosh', ArcCosh, 'http://en.wikipedia.org/wiki/Arccosh');
	AddFunction('Goniometic', 'ArcTanh', ArcTanh, 'http://en.wikipedia.org/wiki/Arctanh');
end.
