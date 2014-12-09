unit uPhysicsFunctions;

interface

implementation

uses uNamespace, uTypes, uVector;

const
	GravityConst = 9.80665; // in height 6378000 m
	EarthRadius = 6378000;
{
const
	LightLMin = 390; // nm
	LightLMax = 760; // nm
}

function Gravity(const Args: array of TVector): TVector;
var
	ArgCount: SG;
	e: FA;
begin
	ArgCount := Length(Args);
	if ArgCount < 1 then
		Result := NumToVector(GravityConst)
	else
	begin
		e := VectorToNum(Args[0]);
		if e > 0 then
			Result := NumToVector(GravityConst / (e / EarthRadius));
	end;
end;

initialization
{$IFNDEF NoInitialization}
	AddFunction('Physics', 'Gravity', Gravity, 'http://en.wikipedia.org/wiki/Earth''s_gravity'); //'Gravitation [m/s]. If no parameter is specified, it mean on surface on th Earth, otherwise parameter is height in [m].');
{$ENDIF NoInitialization}
end.
