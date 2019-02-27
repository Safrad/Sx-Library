unit uPhysicsFunctions;

interface



implementation

uses
  Velthuis.BigDecimals,
  uNamespace, uTypes, uVector, uStrings;

{
const
	LightLMin = 390; // nm
	LightLMax = 760; // nm
}

const
	EarthRadiusConst = 6378000;
	EarthGravityAccelerationString = '9.806650'; // [m / s ^ 2] in surface (height 6378000 m)

var
  GEarthGravityOnSurfaceConst: BigDecimal;

function GetEarthGravityOnSurface: BigDecimal;
begin
  if GEarthGravityOnSurfaceConst.IsZero then
    GEarthGravityOnSurfaceConst := BigDecimal(EarthGravityAccelerationString);
  Result := GEarthGravityOnSurfaceConst;
end;

function EarthRadius: TVector;
begin
	Result := NumToVector(EarthRadiusConst);
end;

function EarthGravity(const ARadius: TVector): TVector;
var
	e: BigDecimal;
begin
  e := VectorToNum(ARadius);
  if e > EarthRadiusConst then
    Result := NumToVector(GetEarthGravityOnSurface * BigDecimal.Sqr(EarthRadiusConst / e))
  else if e > 0 then
    Result := NumToVector(GetEarthGravityOnSurface * e / BigDecimal(EarthRadiusConst));
end;

function EarthGravityOnSurface: TVector;
begin
	Result := NumToVector(GetEarthGravityOnSurface);
end;


initialization
{$IFNDEF NoInitialization}
	AddFunction('Physics', 'EarthRadius', EarthRadius, WikipediaURLPrefix + 'https://en.wikipedia.org/wiki/Earth_radius');
	AddFunction('Physics', 'EarthGravityOnSurface', EarthGravityOnSurface, 'Gravitation [m/s] on surface on the Earth. ' + WikipediaURLPrefix + 'Earth''s_gravity');
	AddFunction('Physics', 'EarthGravity', EarthGravity, 'Gravitation [m/s] parameter is radius in [m]. ' + WikipediaURLPrefix + 'Earth''s_gravity');
{$ENDIF NoInitialization}
end.

