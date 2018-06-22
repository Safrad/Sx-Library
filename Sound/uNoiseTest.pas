unit uNoiseTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TNoiseTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  Complexs,
  FFTs,
  uWhiteNoise;

procedure TNoiseTest.Test;
const
  BufferSizeM = 1024;
var
  Noise: TNoise;
  Data, Dest: array of TComplex;
  i: SG;
  x: FG;
  Amplitude: FG;
begin
  Noise := TNoise.Create;
  try
		SetLength(Data, BufferSizeM);
		for i := 0 to BufferSizeM - 1 do
		begin
//      x := sin(8 * 2 * pi * i / BufferSizeM); //Noise.GetSample(0);
      if i mod 2 = 0 then
        x := -10
      else
        x := 10;
			// Blackmann
			Data[i].Re := (0.42 - 0.5 * cos(2 * pi * i / BufferSizeM) + 0.08 * cos(4 * pi * i / BufferSizeM)) * x;
			Data[i].Im := Data[i].Re;
		end;

		SetLength(Dest, BufferSizeM);
//		FFTs.ForwardFFT(PArrayComplex(@Data[0]), PArrayComplex(@Dest[0]), Length(Data));
		ForwardFFT(PComplexArray(@Data[0]), PComplexArray(@Dest[0]), BufferSizeM);

    // Dest: 0 1 2 3 4 5 .. Hz

		for i := 0 to BufferSizeM - 1 do
		begin
      Amplitude := Abs(Sqrt(Sqr(Dest[i].Re) + Sqr(Dest[i].Im)));

    end;

  finally
    Noise.Free;
  end;
end;

initialization
	RegisterTest('Noise Test', TNoiseTest.Suite);
end.
