//* File:     Lib\uData.pas
//* Created:  1998-01-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uData;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls,
	uAdd;

type
	TIndex = UG;

	TData = class(TObject)
	private
		FFrag: Boolean;
		Data: Pointer; // FFrag = False
		Item: Pointer; // FFrag = True
//		Indexes: TData;
		FItemSize: UG;
		FItemSh: UG;
		FItemMemSize: UG;
		FItemCount: UG;
		procedure Ins(Index: TIndex);
		procedure SetItemSize(Value: UG);
	protected
	public
		constructor Create;
		destructor Free;

		procedure Clear;

		procedure Add(var Value); overload;
		function Add: Pointer; overload;

		procedure Delete(Index: TIndex);
		procedure DeleteFirst;
		procedure DeleteLast;

		procedure Insert(var Value; Index: TIndex); overload;
		function Insert(Index: TIndex): Pointer; overload;

		procedure Replace(var Value; Index: TIndex);

		procedure Get(var Value; Index: TIndex); overload;
		function Get(Index: TIndex): Pointer; overload;
		procedure GetFirst(var Value); overload;
		function GetFirst: Pointer; overload;
		procedure GetLast(var Value); overload;
		function GetLast: Pointer; overload;

		function IsEmpty: Boolean;
		function ToString: string;

		property ItemSize: UG read FItemSize write SetItemSize;
		property ItemSh: UG read FItemSh;
		property ItemMemSize: UG read FItemMemSize;
		property Count: UG read FItemCount;
	end;

	TA4 = class(TObject)
	private
//		Data: PArrayS4;
		FItemCount: UG;
	public
		constructor Create;
		destructor Free;

		procedure Clear;

		procedure Add(var Value); overload;
		function Add: Pointer; overload;

		procedure Delete(Index: TIndex);
		procedure DeleteFirst;
		procedure DeleteLast;

		procedure Insert(var Value; Index: TIndex); overload;
		function Insert(Index: TIndex): Pointer; overload;

		procedure Replace(var Value; Index: TIndex);

		procedure Get(var Value; Index: TIndex); overload;
		function Get(Index: TIndex): Pointer; overload;
		procedure GetFirst(var Value); overload;
		function GetFirst: Pointer; overload;
		procedure GetLast(var Value); overload;
		function GetLast: Pointer; overload;

		function IsEmpty: Boolean;

		property Count: UG read FItemCount;
	end;

implementation

{$ifopt d+}
uses uError;
{$endif}

type
	PItem = ^TItem;
	TItem = record
		Next: PItem;
		OneData: Pointer;
	end;

// TData

constructor TData.Create;
begin
	FItemSize := 0;
	FItemSh := 0;
	FItemCount := 0;
	FFrag := False;
	if FFrag then
	begin
		New(Item);
		FillChar(Item^, SizeOf(Item^), 0);
	end;
end;

destructor TData.Free;
begin
	Clear;
	if FFrag then
		Dispose(Item);
end;

procedure TData.Clear;
var
	It, It2: PItem;
begin
	if FFrag = False then
	begin
		ReallocMem(Data, 0);
		FItemCount := 0;
	end
	else
	begin
		It := Item;
		while It.Next <> nil do
		begin
			It2 := It.Next.Next;
			ReallocMem(It.Next, 0);
			It.Next := It2;
		end;
	end;
end;

procedure TData.SetItemSize(Value: UG);
begin
	if FItemSize <> Value then
	begin
		FItemSize := Value;
		Clear;
		FItemSh := CalcShr(Value);
		FItemMemSize := 1 shl FItemSh;
		{$ifopt d+}
{		if (1 shl Sh) <> Value then
		begin
			ErrorMessage('Bad AllocBy block size ' + NToS(Value) + ' bytes');
		end;}
		{$endif}
	end;
end;

function TData.Add: Pointer;
begin
	Result := Insert(FItemCount);
end;

procedure TData.Add(var Value);
begin
	Insert(Value, FItemCount);
end;

procedure TData.Delete(Index: TIndex);
var
	It, It2: PItem;
	i: TIndex;
begin
	if FFrag = False then
	begin
		if (Index < FItemCount) then
		begin
			Move(Pointer(UG(Data) + (Index + 1) shl FItemSh)^,
				Pointer(UG(Data) + Index shl FItemSh)^, (FItemCount - Index - 1) shl FItemSh);
			Dec(FItemCount);
		end;
	end
	else
	begin
		It := Item;
		i := 0;
		while It.Next <> nil do
		begin
			if i = Index then
			begin
				It2 := It.Next.Next;
				ReallocMem(It.Next, 0);
				It.Next := It2;
				Dec(FItemCount);
				Break;
			end
			else
				It := It.Next;
			Inc(i);
		end;
	end;
end;

procedure TData.DeleteFirst;
begin
	Delete(0);
end;

procedure TData.DeleteLast;
begin
	Delete(FItemCount - 1);
end;

procedure TData.Ins(Index: TIndex);
const
	AllocBy = 1024;
begin
	if FItemCount mod AllocBy = 0 then ReallocMem(Data, (FItemCount + AllocBy) shl FItemSh);
	if Index < FItemCount then
	begin
		Move(Pointer(UG(Data) + Index shl FItemSh)^,
			Pointer(UG(Data) + (Index + 1) shl FItemSh)^, (FItemCount - Index) shl FItemSh);
	end;
	FillChar(Pointer(UG(Data) + Index shl FItemSh)^, ItemSize, 0); // D???
	Inc(FItemCount);
end;

procedure TData.Insert(var Value; Index: TIndex);
var
	It: PItem;
begin
	if FFrag = False then
	begin
		if FItemSize <> 0 then
		begin
			Ins(Index);
			Move(Value, Pointer(UG(Data) + Index shl FItemSh)^, ItemSize);
		end;
	end
	else
	begin
		It := Item;
		while It.Next <> nil do
		begin
			It := It.Next;
		end;
		It.Next := AllocMem(SizeOf(PItem) + ItemSize);
		It := It.Next;
		Move(Value, Pointer(UG(It) + SizeOf(PItem))^, ItemSize);
		It.Next := nil;
		Inc(FItemCount);
	end;
end;

function TData.Insert(Index: TIndex): Pointer;
begin
	if FFrag = False then
	begin
		if FItemSize <> 0 then
		begin
			Ins(Index);
			Result := Pointer(UG(Data) + Index shl FItemSh);
		end
		else
			Result := nil;
	end
	else
	begin
		Result := nil;
		Inc(FItemCount);
	end;
end;

procedure TData.Replace(var Value; Index: TIndex);
begin
	if FFrag = False then
	begin
		if (Index < FItemCount) then
			Move(Value, Pointer(UG(Data) + Index shl FItemSh)^, ItemSize);
	end
	else
	begin

	end;
end;

procedure TData.Get(var Value; Index: TIndex);
begin
	if FFrag = False then
	begin
		if (Index < FItemCount) then
			Move(Pointer(UG(Data) + Index shl FItemSh)^, Value, ItemSize);
	end;
end;

function TData.Get(Index: TIndex): Pointer;
var
	It: PItem;
	i: TIndex;
begin
	if FFrag = False then
	begin
		if (Index >= FItemCount) then
			Result := nil
		else
//		Move(Pointer(TIndex(Data) + Index shl FItemSh)^, Value^, ItemSize);
			Result := Pointer(TIndex(Data) + Index shl FItemSh);
	end
	else
	begin
		Result := nil;
		i := 0;
		It := Item;
		while It <> nil do
		begin
			if i = Index then
			begin
//				Move(Pointer(UG(It) + SizeOf(PItem))^, Value^, ItemSize);
				Break;
			end;
			It := It.Next;
			Inc(i);
		end;
	end;
end;

procedure TData.GetFirst(var Value);
begin
	Get(Value, 0);
end;

function TData.GetFirst: Pointer;
begin
	Result := Get(0);
end;

procedure TData.GetLast(var Value);
begin
	Get(Value, FItemCount - 1)
end;

function TData.GetLast: Pointer;
begin
	if FItemCount > 0 then
		Result := Get(FItemCount - 1)
	else
		Result := nil;
end;

function TData.IsEmpty: Boolean;
begin
	Result := FItemCount = 0;
end;

function TData.ToString: string;
var
	i: SG;
	D: PS4;
begin
	if FItemSize <> 0 then
	begin
		Result := 'ItemSize: ' + NToS(FItemSize);
	end
	else
		Result := 'VariableSize';
	Result := Result + ', ItemCount: ' + NToS(FItemCount);
	if FItemSize = 4 then
	begin
		Result := Result + ', Items: ';
		D := Data;
		for i := 0 to FItemCount - 1 do
		begin
			Result := Result + NToS(D^) + ',';

			Inc(D, 1); //  shl FItemSh
		end;
		SetLength(Result, Length(Result) - 1);
	end;
end;

// TA4

constructor TA4.Create;
begin
	FItemCount := 0;
end;

destructor TA4.Free;
begin
	Clear;
end;

procedure TA4.Clear;
begin

end;

procedure TA4.Add(var Value);
begin

end;

function TA4.Add: Pointer;
begin
	Result := nil;
end;

procedure TA4.Delete(Index: TIndex);
begin

end;

procedure TA4.DeleteFirst;
begin

end;

procedure TA4.DeleteLast;
begin

end;

procedure TA4.Insert(var Value; Index: TIndex);
begin

end;

function TA4.Insert(Index: TIndex): Pointer;
begin
	Result := nil;

end;

procedure TA4.Replace(var Value; Index: TIndex);
begin

end;

procedure TA4.Get(var Value; Index: TIndex);
begin

end;

function TA4.Get(Index: TIndex): Pointer;
begin
	Result := nil;

end;

procedure TA4.GetFirst(var Value);
begin

end;

function TA4.GetFirst: Pointer;
begin
	Result := nil;

end;

procedure TA4.GetLast(var Value);
begin

end;

function TA4.GetLast: Pointer;
begin
	Result := nil;

end;

function TA4.IsEmpty: Boolean;
begin
	Result := True;

end;

end.
