//* File:     Lib\uData.pas
//* Created:  1998-01-01
//* Modified: 2007-05-12
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uData;

interface

uses
	uTypes,
	SysUtils;
type
	TIndex = SG;

	{ Warning!
		Dynamic variables must be finalized manually before removed
	}

	TData = class(TObject) // TArrayList
	private
		FFrag: Boolean;
		Data: Pointer; // FFrag = False
		Item: Pointer; // FFrag = True
		FClearCreated: BG;
		FItemSize: UG;
		FItemSh: UG;
		FItemMemSize: UG;
		FItemCount: TIndex;
		FItemAlloc: SG; // FCapacity
		procedure NewData(const Index: TIndex);
		procedure SetItemSize(const Value: UG);
	public
		constructor Create(const ClearCreated: BG); overload;
		constructor Create; overload;
		destructor Destroy; override;

		// Data manipulation
		procedure Clear;

//		procedure Add(var Value); overload;
		function Add(P: Pointer): Pointer; overload;
		function Add: Pointer; overload;
		function Add(const Index: TIndex): Pointer; overload;
		procedure SetCount(NewCount: SG);

		procedure DeleteFirst;
		procedure DeleteLast;
		procedure Delete(const Index: TIndex);

//		procedure Insert(var Value; Index: TIndex); overload;
		procedure Replace(const Index: TIndex; const Item: Pointer);

//		procedure Replace(var Value; Index: TIndex);
		procedure Swap(const I1, I2: TIndex);

		// Data query
//		procedure Get(var Value; Index: TIndex); overload;
		function Get(const Index: TIndex): Pointer; overload;
//		procedure GetFirst(var Value); overload;
		function GetFirst: Pointer; overload;
//		procedure GetLast(var Value); overload;
		function GetLast: Pointer; overload;

		function IsEmpty: Boolean;
		function ToString: string;

		property ItemSize: UG read FItemSize write SetItemSize;
		property ItemSh: UG read FItemSh; // Binary shift
		property ItemMemSize: UG read FItemMemSize; // Item size in memory
		property Count: TIndex read FItemCount;
		property Items[const Index: TIndex]: Pointer read Get write Replace; default; // operator []
	end;


implementation

uses Math, uMath;

type
	PItem = ^TItem;
	TItem = record
		Next: PItem;
		OneData: Pointer;
	end;

{ TData }

constructor TData.Create(const ClearCreated: BG);
begin
	inherited Create;
	FClearCreated := ClearCreated;
	ItemSize := SizeOf(TObject);
	FItemCount := 0;
	FitemAlloc := 0;
	FFrag := False;
	if FFrag then
	begin
		New(Item);
		FillChar(Item^, SizeOf(Item^), 0);
	end;
end;

procedure TData.Clear;
var
	It, It2: PItem;
begin
	if FFrag = False then
	begin
		FreeMem(Data); Data := nil;
		FItemCount := 0;
		FItemAlloc := 0;
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

constructor TData.Create;
begin
	Create(True);
end;

destructor TData.Destroy;
begin
	Clear;
	if FFrag then
		Dispose(Item);
	inherited;
end;

function TData.Add: Pointer;
begin
	Result := Add(FItemCount);
end;

function TData.Add(P: Pointer): Pointer;
begin
	Result := Add;
	PPointer(Result)^ := P;
end;

{procedure TData.Add(var Value);
begin
	Insert(Value, FItemCount);
end;}

procedure TData.Delete(const Index: TIndex);
var
	It, It2: PItem;
	i: TIndex;
begin
	if FFrag = False then
	begin
		if (Index < FItemCount) then
		begin
			Move(Pointer(TIndex(Data) + (Index + 1) shl FItemSh)^,
				Pointer(TIndex(Data) + Index shl FItemSh)^, (FItemCount - Index - 1) shl FItemSh);
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

procedure TData.SetCount(NewCount: SG);
begin
	Assert(NewCount >= 0);
	FItemCount := NewCount;
	if AllocByExp(FItemAlloc, NewCount) then //if FItemCount mod AllocBy = 0 then
	begin
		ReallocMem(Data, NewCount shl FItemSh);
		if NewCount > FItemAlloc then
		begin
			if FClearCreated then
				FillChar(Pointer(TIndex(Data) + FItemAlloc shl FItemSh)^, (NewCount - FItemAlloc) shl FItemSh, 0);
		end;
		FItemAlloc := NewCount;
	end;
end;

procedure TData.NewData(const Index: TIndex);
var NewItemCount, OldItemCount: SG;
begin
	OldItemCount := FItemCount;
	NewItemCount := Max(Index + 1, FItemCount);
	SetCount(NewItemCount);
	if Index < OldItemCount then
	begin
		Move(Pointer(TIndex(Data) + Index shl FItemSh)^,
			Pointer(TIndex(Data) + (Index + 1) shl FItemSh)^, (OldItemCount - Index) shl FItemSh);
	end;

	if FClearCreated then
		FillChar(Pointer(TIndex(Data) + Index shl FItemSh)^, ItemSize, 0);
end;

{
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
end;}

function TData.Add(const Index: TIndex): Pointer;
begin
	if FFrag = False then
	begin
		if FItemSize <> 0 then
		begin
			NewData(Index);
			Result := Pointer(TIndex(Data) + Index shl FItemSh);
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

procedure TData.Replace(const Index: TIndex; const Item: Pointer);
var P: Pointer;
begin
	if FFrag = False then
	begin
		P := Get(Index);
		if (P <> nil) then
		begin
			Move(Item^, P^, FItemSize);
		end;
	end
	else
	begin

	end;
end;

{
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
end;}

{procedure TData.Get(var Value; Index: TIndex);
begin
	if FFrag = False then
	begin
		if (Index < FItemCount) then
			Move(Pointer(UG(Data) + Index shl FItemSh)^, Value, ItemSize);
	end;
end;}

function TData.Get(const Index: TIndex): Pointer;
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

{procedure TData.GetFirst(var Value);
begin
	Get(Value, 0);
end;}

function TData.GetFirst: Pointer;
begin
	Result := Get(0);
end;

{procedure TData.GetLast(var Value);
begin
	Get(Value, FItemCount - 1)
end;}

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

procedure TData.SetItemSize(const Value: UG);
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

procedure TData.Swap(const I1, I2: TIndex);
begin
	Exchange(Pointer(TIndex(Data) + I1 shl FItemSh), Pointer(TIndex(Data) + I2 shl FItemSh), FItemSize);
end;

function TData.ToString: string;
var
	i: SG;
	D: PS4;
begin
	if FItemSize <> 0 then
	begin
		Result := 'ItemSize: ' + IntToStr(FItemSize);
	end
	else
		Result := 'VariableSize';
	Result := Result + ', ItemCount: ' + IntToStr(FItemCount);
	if FItemSize = 4 then
	begin
		Result := Result + ', Items: ';
		D := Data;
		for i := 0 to FItemCount - 1 do
		begin
			Result := Result + IntToStr(D^) + ',';

			Inc(D, 1); //  shl FItemSh
		end;
		SetLength(Result, Length(Result) - 1);
	end;
end;

end.
