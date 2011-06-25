//* File:     Lib\Parser\uLogicFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-11-27
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uLogicFunctions;

interface

implementation

uses uTypes, uVector, uNamespace;

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
			case Operation of
			opShl: R := ShlVector(R, X[i]);
			opShr: R := ShrVector(R, X[i]);
			opAnd:
			begin
//				if R = 0 then Break;
				R := AndVector(R, X[i]);
			end;
			opOr:
			begin
//				if R = $ffffffffffffffff then Break;
				R := OrVector(R, X[i]);
			end;
			opXor: R := XorVector(R, X[i]);
			opXnor: R := XnorVector(R, X[i]);
			end;
		end;
		Result := R;
	end
	else
		Result := nil;
end;

function ConstantFalse: TVector;
begin
	Result := NumToVector(0);
end;

function ConstantTrue: TVector;
begin
	Result := NumToVector(1);
end;

function FunctionShl(const Data: array of TVector): TVector;
begin
	Result := Logic(opShl, Data);
end;

function FunctionShr(const Data: array of TVector): TVector;
begin
	Result := Logic(opShr, Data);
end;

function FunctionAnd(const Data: array of TVector): TVector;
begin
	Result := Logic(opAnd, Data);
end;

function FunctionOr(const Data: array of TVector): TVector;
begin
	Result := Logic(opOr, Data);
end;

function FunctionXor(const Data: array of TVector): TVector;
begin
	Result := Logic(opXor, Data);
end;

function FunctionXnor(const Data: array of TVector): TVector;
begin
	Result := Logic(opXnor, Data);
end;

initialization
	AddFunction('Logic', 'false', ConstantFalse, '');
	AddFunction('Logic', 'true', ConstantTrue, '');
	AddFunction('Logic', 'shl', FunctionShl, '');
	AddFunction('Logic', 'shr', FunctionShr, '');
	AddFunction('Logic', 'and', FunctionAnd, '');
	AddFunction('Logic', 'or', FunctionOr, '');
	AddFunction('Logic', 'xor', FunctionXor, '');
	AddFunction('Logic', 'xnor', FunctionXnor, '');
end.
