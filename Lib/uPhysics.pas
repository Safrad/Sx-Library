unit uPhysics;

interface

implementation

uses uParser, uTypes, uVector;

function Gravity(const Args: array of TVector): TVector;
var
	ArgCount: SG;
	e: FA;
begin
	ArgCount := Length(Args);
	if ArgCount < 1 then
		Result := NumToVector(10)
	else
	begin
		e := VectorToNum(Args[0]);
		if e > 0 then
			Result := NumToVector(10 / (e / 6378));
	end;
end;

initialization
	AddFunction('Physics', 'Gravity', Gravity);
end.
