// A 32 bit random number generator. An implementation in C of the algorithm given by Knuth, the art of computer programming, vol. 2

unit uKnuthRandomGenerator;

{$Q-}

interface

uses
  uTypes,
  uRandomGenerator;

type
  TKnutRandomGenerator = class(TRandomGenerator)
  private
    InitJ: SG;
    InitK: SG;
    InitX: array[0..54] of U4;
  public
    constructor Create;

    function RandomU4: U4; override;
    function RandomU8: U8; override;
  end;

implementation

var
  cInitX: array[0..54] of U4 = (
    1410651636, 3012776752, 3497475623, 2892145026, 1571949714,
    3253082284, 3489895018, 387949491, 2597396737, 1981903553,
    3160251843, 129444464, 1851443344, 4156445905, 224604922,
    1455067070, 3953493484, 1460937157, 2528362617, 317430674,
    3229354360, 117491133, 832845075, 1961600170, 1321557429,
    747750121, 545747446, 810476036, 503334515, 4088144633,
    2824216555, 3738252341, 3493754131, 3672533954, 29494241,
    1180928407, 4213624418, 33062851, 3221315737, 1145213552,
    2957984897, 4078668503, 2262661702, 65478801, 2527208841,
    1960622036, 315685891, 1196037864, 804614524, 1421733266,
    2017105031, 3882325900, 810735053, 384606609, 2393861397);
{ TKnutRandomGenenrator }

//	random numbers from Mathematica 2.0.
//	SeedRandom = 1;
//	Table[Random[SG, {0, 2^32 - 1}]
constructor TKnutRandomGenerator.Create;
begin
  inherited;

  InitJ := 24 - 1;
  InitK := 55 - 1;
  Move(cInitX, InitX, SizeOf(InitX));
end;

function TKnutRandomGenerator.RandomU4: U4;
begin
	Result := (InitX[InitJ] + InitX[InitK]);
	InitX[InitJ] := Result;
	if InitJ = 0 then
		InitJ := High(InitX)
	else
		Dec(InitJ);
	if InitK = 0 then
		InitK := High(InitX)
	else
		Dec(InitK);
end;

function TKnutRandomGenerator.RandomU8: U8;
begin
  TU8(Result).D0 := RandomU4;
  TU8(Result).D1 := RandomU4;
end;

end.
