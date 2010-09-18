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
//		DataCount: UG;
		Item: Pointer; // FFrag = True
//		Indexes: TData;
		FItemSize: UG;
		FItemCount: UG;
	protected
	public
		constructor Create;
		destructor Free;

		procedure Clear;
		function Add: Pointer;
		procedure Push(var Value);
		procedure Pop(Value: Pointer); // LIFO
		procedure Delete(i: TIndex);
		procedure Insert(Value: Pointer; Index: TIndex);
		function Get(Index: TIndex): Pointer;
		function GetFirst: Pointer;
		function GetLast: Pointer;
//		procedure Put(Index: Integer; const S: string);
//		procedure Get(Value: Pointer; Index: TIndex);
		procedure Put(Value: Pointer; Index: TIndex);
		function IsEmpty: Boolean;

		property ItemSize: UG read FItemSize write FItemSize;
		property Count: UG read FItemCount;
//		property Strings[Index: Integer]: string read Get write Put; default;
	end;

implementation

type
	PItem = ^TItem;
	TItem = record
		Next: PItem;
		OneData: Pointer;
	end;

constructor TData.Create;
begin
	FItemSize := 1;
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

function TData.Add: Pointer;
var D: Pointer;
begin
	GetMem(D, ItemSize);
	Push(D);
	Result := Get(FItemCount - 1);
	FreeMem(D, ItemSize);
end;

procedure TData.Push(var Value);
var
	It: PItem;
begin
	if FFrag = False then
	begin
		if FItemSize <> 0 then
		begin
			if FItemCount mod 1024 = 0 then ReallocMem(Data, ItemSize * (FItemCount + 1024));
			Move(Addr(Value)^, Pointer(UG(Data) + ItemSize * FItemCount)^, ItemSize);
		end
		else
		begin
{			Indexes := TData.Create;
			Indexes.ItemSize := SizeOf(Pointer);
			ReallocMem(Data, DataCount + SizeOf(Value));
			Indexes.Push(DataCount);
			Move(Addr(Value)^, Pointer(UG(Data) + DataCount)^, SizeOf(Value));
			Inc(DataCount, SizeOf(Value));}
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
		Move(Addr(Value)^, Pointer(SG(It) + SizeOf(PItem))^, ItemSize);
		It.Next := nil;
	end;
	Inc(FItemCount);
end;

procedure TData.Pop(Value: Pointer);
begin
	Dec(FItemCount);
	if FFrag = False then
	begin
		Move(Pointer(UG(Data) + ItemSize * FItemCount)^, Value^, ItemSize);
		if FItemCount mod 1024 = 0 then ReallocMem(Data, ItemSize * FItemCount);
	end
	else
	begin

	end;
end;

procedure TData.Delete(i: TIndex);
var
	It, It2: PItem;
	Index: TIndex;
begin
	if FFrag = False then
	begin
		Move(Pointer(UG(Data) + ItemSize * (i + 1))^,
			Pointer(UG(Data) + ItemSize * i)^, ItemSize * (FItemCount - i - 1));
		Dec(FItemCount);
	end
	else
	begin
		It := Item;
		Index := 0;
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
			Inc(Index);
		end;
	end;
end;

procedure TData.Insert(Value: Pointer; Index: TIndex);
begin
	if FFrag = False then
	begin

	end
	else
	begin

	end;
end;

function TData.Get(Index: TIndex): Pointer;
var
	It: PItem;
	i: TIndex;
begin
	Result := nil;
	if FFrag = False then
	begin
		if (Index >= FItemCount) then
			Result := nil
		else
//		Move(Pointer(TIndex(Data) + ItemSize * Index)^, Value^, ItemSize);
			Result := Pointer(TIndex(Data) + ItemSize * Index);
	end
	else
	begin
		i := 0;
		It := Item;
		while It <> nil do
		begin
			if i = Index then
			begin
//				Move(Pointer(SG(It) + SizeOf(PItem))^, Value^, ItemSize);
				Break;
			end;
			It := It.Next;
			Inc(i);
		end;
	end;
end;

function TData.GetFirst: Pointer;
begin
	Result := Get(0);
end;

function TData.GetLast: Pointer;
begin
	Result := Get(FItemCount - 1);
end;

procedure TData.Put(Value: Pointer; Index: TIndex);
begin
	if FFrag = False then
	begin
		if (Index >= FItemCount) then Exit;
		Move(Pointer(UG(Data) + ItemSize * Index)^, Value^, ItemSize);
	end
	else
	begin

	end;
end;

function TData.IsEmpty: Boolean;
begin
	Result := FItemCount = 0;
end;

end.
