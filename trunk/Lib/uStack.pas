// * File:     Lib\uStack.pas
// * Created:  2009-09-09
// * Modified: 2009-11-05
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uStack;

interface

uses
	uTypes,
	SysUtils;

type
	PPointerList = ^TPointerList;
	TPointerList = array [0 .. 256 * MB - 1] of Pointer;

	TStack = class // TODO (TData)
	private
		FList: PPointerList;
		FCapacity, FCount: Cardinal;
		procedure Grow;
	public
		destructor Destroy; override;
		procedure Push(const Data: Pointer);
		function Pop: Pointer;
	end;

implementation

{ TStack }

{
	function Push(Value: Variant; NewValue: Variant): Variant;
	var
	tmp: PVariant;
	begin
	GetMem(tmp, SizeOf(Variant));
	tmp^ := Value;
	Stack.Push(tmp);
	Result := NewValue;
	end;

	function Pop: Variant;
	var
	tmp: PVariant;
	begin
	if Stack.Count > 0 then begin
	tmp := Stack.Pop;
	Result := tmp^;
	tmp^ := Null;
	FreeMem(tmp, SizeOf(Variant));
	end
	else
	Result := Null;
	end;

	The line
	tmp^ := Null;
	is necessary to prevent memory leaks. For example, if the stack element holds a string,
	FreeMem(tmp, SizeOf(Variant));
	will only free the pointer to the string, not the string itself.

}

destructor TStack.Destroy;
begin
	FreeMem(FList);
	inherited;
end;

procedure TStack.Grow;
begin
	if FCapacity > 0 then
		Inc(FCapacity, FCapacity div 4)
	else { if FCapacity > 8 then
			Inc(FCapacity, 16)
			else }
		Inc(FCapacity, 16);
	ReallocMem(FList, FCapacity * SizeOf(Pointer));
end;

function TStack.Pop: Pointer;
begin
	if FCount > 0 then
	begin
		Dec(FCount);
		Result := FList^[FCount];
	end
	else
		Result := nil;
end;

procedure TStack.Push(const Data: Pointer);
begin
	if FCapacity = FCount then
		Grow;
	FList^[FCount] := Data;
	Inc(FCount);
end;

end.
