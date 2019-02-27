unit uGoniometricFunctions;

interface

type
	TGoniometricFormat = (gfRad, gfGrad, gfCycle, gfDeg);
var
	GoniometricFormat: TGoniometricFormat;

implementation

uses
  SysUtils,
  Math,

  Velthuis.BigDecimals,

  uBigDecimalConsts,
  uBigDecimalUtils,
	uNamespace,
  uVector,
  uStrings;

function VectorToExtended(const AVector: TVector): Extended;
begin
  Result := TBigDecimalUtils.ToFloat(VectorToNum(AVector));
end;

function CorrectFormatArgument(const AVector: TVector): Extended; // XToRad
var
  E: Extended;
begin
  E := VectorToExtended(AVector);
	case GoniometricFormat of
	gfRad: Result := E;
	gfGrad: Result := E * (2 * pi) / 400;
	gfCycle: Result := E * (2 * pi);
	gfDeg: Result := E * (2 * pi) / 360;
  else
    raise EArgumentException.Create('Invalid Goniometric Format');
	end;
end;

function CorrectFormatResult(const X: Extended): TVector; // RadToResult
var
  R: BigDecimal;
begin
	case GoniometricFormat of
	gfRad: R := X;
	gfGrad: R := BigDecimal(X) * (400 / (2 * pi));
	gfCycle: R := BigDecimal(X) * (1 / (2 * pi));
	gfDeg: R := BigDecimal(X) * (360 / (2 * pi));
  else
    raise EArgumentException.Create('Invalid Goniometric Format');
	end;
  SetLength(Result, 1);
  Result[0] := R;
end;

function PIConstant: TVector;
begin
	Result := NumToVector(TBigDecimalConsts.NumberPi);
end;

function Sine(const X: TVector): TVector;
var
  E: Extended;
begin
  E := System.Sin(CorrectFormatArgument(X));
	Result := NumToVector(E);
end;

function Cosine(const X: TVector): TVector;
begin
	Result := NumToVector(System.Cos(CorrectFormatArgument(X)));
end;

function Tangent(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Tan(CorrectFormatArgument(X)));
end;

function Cosecant(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Cosecant(CorrectFormatArgument(X)));
end;

function Secant(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Secant(CorrectFormatArgument(X)));
end;

function Cotangent(const X: TVector): TVector;
begin
	Result := NumToVector(Math.Cotan(CorrectFormatArgument(X)));
end;

function ArcSin(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(Math.ArcSin(VectorToExtended(X)));
end;

function ArcCos(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(Math.ArcCos(VectorToExtended(X)));
end;

function ArcTan(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(System.ArcTan(VectorToExtended(X)));
end;

function ArcCsc(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(Math.ArcCsc(VectorToExtended(X)));
end;

function ArcSec(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(Math.ArcSec(VectorToExtended(X)));
end;

function ArcCot(const X: TVector): TVector;
begin
	Result := CorrectFormatResult(Math.ArcCot(VectorToExtended(X)));
end;

initialization
{$IFNDEF NoInitialization}
	AddFunction('Trigonometric', 'Pi', PIConstant, WikipediaURLPrefix + 'Pi');
	AddFunction('Trigonometric', 'π', PIConstant, WikipediaURLPrefix + 'Pi');
	AddFunction('Trigonometric', 'Sin', Sine, WikipediaURLPrefix + 'Sine');
	AddFunction('Trigonometric', 'Cos', Cosine, WikipediaURLPrefix + 'Cosine');
	AddFunction('Trigonometric', 'Tan', Tangent, WikipediaURLPrefix + 'Tangent');
	AddFunction('Trigonometric', 'Csc', Cosecant, WikipediaURLPrefix + 'Cosecant');
	AddFunction('Trigonometric', 'Sec', Secant, WikipediaURLPrefix + 'Secant');
	AddFunction('Trigonometric', 'Cot', Cotangent, WikipediaURLPrefix + 'Cotangent');
	AddFunction('Trigonometric', 'ArcSin', ArcSin, WikipediaURLPrefix + 'Arcsine');
	AddFunction('Trigonometric', 'ArcCos', ArcCos, WikipediaURLPrefix + 'Arccosine');
	AddFunction('Trigonometric', 'ArcTan', ArcTan, WikipediaURLPrefix + 'Arctangent');
	AddFunction('Trigonometric', 'ArcCsc', ArcCsc, WikipediaURLPrefix + 'Arccosecant');
	AddFunction('Trigonometric', 'ArcSec', ArcSec, WikipediaURLPrefix + 'Arcsecant');
	AddFunction('Trigonometric', 'ArcCot', ArcCot, WikipediaURLPrefix + 'Arccotangent');
{$ENDIF NoInitialization}
end.
