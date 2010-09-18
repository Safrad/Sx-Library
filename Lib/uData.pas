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

		property ItemSize: UG read FItemSize write SetItemSize;
		property Count: UG read FItemCount;
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
	FItemSize := 0;
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
			Move(Pointer(UG(Data) + ItemSize * (Index + 1))^,
				Pointer(UG(Data) + ItemSize * Index)^, ItemSize * (FItemCount - Index - 1));
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
begin
	if FItemCount mod 1024 = 0 then ReallocMem(Data, ItemSize * (FItemCount + 1024));
	if Index < FItemCount then
	begin
		Move(Pointer(UG(Data) + ItemSize * Index)^,
			Pointer(UG(Data) + ItemSize * (Index + 1))^, ItemSize * (FItemCount - Index));
	end;
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
			Move(Value, Pointer(UG(Data) + ItemSize * Index)^, ItemSize);
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
			Result := Pointer(UG(Data) + ItemSize * Index);
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
			Move(Value, Pointer(UG(Data) + ItemSize * Index)^, ItemSize);
	end
	else
	begin

	end;
end;

procedure TData.Get(var Value; Index: TIndex);
begin
	if FFrag = False then
	begin
		Move(Pointer(UG(Data) + ItemSize * Index)^, Value, ItemSize);
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
//		Move(Pointer(TIndex(Data) + ItemSize * Index)^, Value^, ItemSize);
			Result := Pointer(TIndex(Data) + ItemSize * Index);
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
	Get(Value, FItemCount - 1);
end;

function TData.GetLast: Pointer;
begin
	Result := Get(FItemCount - 1);
end;

function TData.IsEmpty: Boolean;
begin
	Result := FItemCount = 0;
end;

end.
