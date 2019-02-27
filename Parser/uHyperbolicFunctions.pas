unit uHyperbolicFunctions;

interface

implementation

uses
  uNamespace,
  uStrings,
  uBigDecimalHelper,
  uVector;

function Sinh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Sinh);
end;

function Cosh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Cosh);
end;

function Tanh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Tanh);
end;

function Csch(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Csch);
end;

function Sech(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Sech);
end;

function Coth(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).Coth);
end;

function ArSinh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArSinh);
end;

function ArCosh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArCosh);
end;

function ArTanh(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArTanh);
end;

function ArCsch(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArSinh);
end;

function ArSech(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArCosh);
end;

function ArCoth(const X: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(X).ArTanh);
end;

initialization
{$IFNDEF NoInitialization}
	AddFunction('Hyperbolic', 'Sinh', Sinh, WikipediaURLPrefix + 'Hyperbolic_sine');
	AddFunction('Hyperbolic', 'Cosh', Cosh, WikipediaURLPrefix + 'Hyperbolic_cosine');
	AddFunction('Hyperbolic', 'Tanh', Tanh, WikipediaURLPrefix + 'Hyperbolic_tangent');
	AddFunction('Hyperbolic', 'Csch', Csch, WikipediaURLPrefix + 'Hyperbolic_sine');
	AddFunction('Hyperbolic', 'Sech', Sech, WikipediaURLPrefix + 'Hyperbolic_cosine');
	AddFunction('Hyperbolic', 'Coth', Coth, WikipediaURLPrefix + 'Hyperbolic_tangent');
	AddFunction('Hyperbolic', 'ArSinh', ArSinh, WikipediaURLPrefix + 'Arsinh');
	AddFunction('Hyperbolic', 'ArCosh', ArCosh, WikipediaURLPrefix + 'Arcosh');
	AddFunction('Hyperbolic', 'ArTanh', ArTanh, WikipediaURLPrefix + 'Artanh');
	AddFunction('Hyperbolic', 'ArCsch', ArCsch, WikipediaURLPrefix + 'Arcsch');
	AddFunction('Hyperbolic', 'ArSech', ArSech, WikipediaURLPrefix + 'Arsech');
	AddFunction('Hyperbolic', 'ArCoth', ArCoth, WikipediaURLPrefix + 'Arcoth');
{$ENDIF NoInitialization}
end.
