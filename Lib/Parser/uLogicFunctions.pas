unit uLogicFunctions;

interface

implementation

uses uTypes, uVector, uNamespace;

type
	TOperation = (opShl, opShr, opAnd, opOr, opNor, opXor, opXnor);

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
			opNor:
			begin
				R := NotVector(OrVector(R, X[i]));
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

function FunctionNor(const Data: array of TVector): TVector;
begin
	Result := Logic(opNor, Data);
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
{$IFNDEF NoInitialization}
	AddFunction('Logic', 'false', ConstantFalse, 'http://en.wikipedia.org/wiki/Boolean_algebras_canonically_defined');
	AddFunction('Logic', 'true', ConstantTrue, 'http://en.wikipedia.org/wiki/Truth_value');
	AddFunction('Logic', 'shl', FunctionShl, 'http://en.wikipedia.org/wiki/Logical_shift');
	AddFunction('Logic', 'shr', FunctionShr, 'http://en.wikipedia.org/wiki/Logical_shift');
	AddFunction('Logic', 'and', FunctionAnd, 'http://en.wikipedia.org/wiki/Logical_conjunction');
	AddFunction('Logic', 'or', FunctionOr, 'http://en.wikipedia.org/wiki/Logical_disjunction');
	AddFunction('Logic', 'nor', FunctionNor, 'http://en.wikipedia.org/wiki/Logical_NOR');
	AddFunction('Logic', 'xor', FunctionXor, 'http://en.wikipedia.org/wiki/Exclusive_or');
	AddFunction('Logic', 'xnor', FunctionXnor, 'http://en.wikipedia.org/wiki/Logical_equality');
{$ENDIF NoInitialization}
end.
