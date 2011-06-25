//* File:     Lib\Parser\uNamespace.pas
//* Created:  2004-03-07
//* Modified: 2007-05-12
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uNamespace;

interface

uses
	uTypes,
	uHashTable,
	uVector;

// Registration of external functions
type
	TConstantFunction = function: TVector;
	TUnaryFunction = function(const X: TVector): TVector;
	TBinaryFunction = function(const X, Y: TVector): TVector;
	TNaryFunction = function(const X: array of TVector): TVector;

procedure AddFunction(const UnitName, FunctionName: string; const ConstantFunction: TConstantFunction; const Description: string); overload;
procedure AddFunction(const UnitName, FunctionName: string; const UnaryFunction: TUnaryFunction; const Description: string); overload;
procedure AddFunction(const UnitName, FunctionName: string; const BinaryFunction: TBinaryFunction; const Description: string); overload;
procedure AddFunction(const UnitName, FunctionName: string; const NaryFunction: TNaryFunction; const Description: string); overload;

function CorrectParamCount(const UnitName, FunctionName: string; const ArgCount: SG): BG;
function FunctionExists(const UnitName, FunctionName: string): BG;
function CallFunction(const UnitName, FunctionName: string; const Args: array of TVector): TVector;

var
	Namespace: THashTable; // Read only
type
	PFunction = ^TFunction;
	TFunction = record
		Name: string;
		ArgCount: SG;
		Address: Pointer;
		Description: string;
	end;

implementation

uses
	SysUtils,
	uFind,
	uStrings,
	// Addons:
	uMathFunctions,
	uLogicFunctions,
	uGoniometricFunctions,
	uStatisticsFunctions,
	uPhysicsFunctions,
	uEloFunctions;

const
	AnyParameters = -1;

procedure AddFunctionEx(const UnitName, FunctionName: string; const FunctionAddress: Pointer; const ArgCount: SG; const Description: string);
var
	F: TFunction;
begin
	if Namespace = nil then
		Namespace := THashTable.Create(512, SizeOf(TFunction));

	F.Name := FunctionName;
	F.ArgCount := ArgCount;
	F.Address := FunctionAddress;
	F.Description := Description;
	Namespace.Add(HashCode(UpperCase(FunctionName)), @F);
end;

procedure AddFunction(const UnitName, FunctionName: string; const ConstantFunction: TConstantFunction; const Description: string);
begin
	AddFunctionEx(UnitName, FunctionName, @ConstantFunction, 0, Description);
end;

procedure AddFunction(const UnitName, FunctionName: string; const UnaryFunction: TUnaryFunction; const Description: string);
begin
	AddFunctionEx(UnitName, FunctionName, @UnaryFunction, 1, Description);
end;

procedure AddFunction(const UnitName, FunctionName: string; const BinaryFunction: TBinaryFunction; const Description: string);
begin
	AddFunctionEx(UnitName, FunctionName, @BinaryFunction, 2, Description);
end;

procedure AddFunction(const UnitName, FunctionName: string; const NaryFunction: TNaryFunction; const Description: string);
begin
	AddFunctionEx(UnitName, FunctionName, @NaryFunction, AnyParameters, Description);
end;

function FindFunction(const UnitName, FunctionName: string; const ArgCount: SG): PFunction;
var
	F: PFunction;
begin
	F := Namespace.Find(HashCode(UpperCase(FunctionName)));
	if (F <> nil) and (ArgCount <> AnyParameters) and (F.ArgCount <> AnyParameters) then // Any parameters
	begin
		Result := nil;
		while F <> nil do
		begin
			if ArgCount = F.ArgCount then
			begin
				Result := F;
				Break;
			end;
			F := Namespace.FindNext;
		end;
	end
	else
		Result := F;
end;

function CorrectParamCount(const UnitName, FunctionName: string; const ArgCount: SG): BG;
begin
	Result := FindFunction(UnitName, FunctionName, ArgCount) <> nil;
end;

function FunctionExists(const UnitName, FunctionName: string): BG;
begin
	Result := FindFunction(UnitName, FunctionName, AnyParameters) <> nil;
end;

function CallFunction(const UnitName, FunctionName: string; const Args: array of TVector): TVector;
var
	F: PFunction;
begin
	F := FindFunction(UnitName, FunctionName, Length(Args));
	if (F <> nil) and (F.Address <> nil) then
	begin
		case F.ArgCount of
		0: Result := TConstantFunction(F.Address);
		1: Result := TUnaryFunction(F.Address)(Args[0]);
		2: Result := TBinaryFunction(F.Address)(Args[0], Args[1]);
		else
		begin
			Result := TNaryFunction(F.Address)(Args);
		end;
		end;
	end
	else
	begin
		Result := nil;
	end;
end;

initialization

finalization
	FreeAndNil(Namespace);
end.
