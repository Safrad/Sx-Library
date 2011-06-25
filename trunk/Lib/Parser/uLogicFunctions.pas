//* File:     Lib\Parser\uLogicFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-05-12
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uLogicFunctions;

interface

implementation

uses uTypes, uVector, uNamespace;

function ConstantFalse: TVector;
begin
	Result := NumToVector(0);
end;

function ConstantTrue: TVector;
begin
	Result := NumToVector(1);
end;

type
	TOperation = (opShl, opShr, opAnd, opOr, opXor, opXnor);

function Logic(const Operation: TOperation; const X: array of TVector): TVector;
var
	R: TVector;
	i: SG;
begin
	if Length(X) > 0 then
	begin
		R := X[0];
		for i := 1 to Length(X) - 1 do
		begin
{ TODO:			case Operation of
			opShl: R := R shl Round(X[i]);
			opShr: R := R shr Round(X[i]);
			opAnd:
			begin
				if R = 0 then Break;
				R := R and Round(X[i]);
			end;
			opOr:
			begin
				if R = $ffffffffffffffff then Break;
				R := R or Round(X[i]);
			end;
			opXor: R := R xor Round(X[i]);
			opXnor: R := not (R xor Round(X[i]));
			end;}
		end;
		Result := R;
	end
	else
		Result := nil;
end;

initialization
	AddFunction('Logic', 'false', ConstantFalse, '');
	AddFunction('Logic', 'true', ConstantTrue, '');
{	AddFunction('Logic', 'shl', FunctionShl, '');
	AddFunction('Logic', 'shr', FunctionShr, '');
	AddFunction('Logic', 'and', FunctionAnd, '');
	AddFunction('Logic', 'or', FunctionOr, '');
	AddFunction('Logic', 'xor', FunctionXor, '');
	AddFunction('Logic', 'xnor', FunctionXnor, '');}
end.
