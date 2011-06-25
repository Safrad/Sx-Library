//* File:     Lib\uData.pas
//* Created:  1998-01-01
//* Modified: 2008-12-28
//* Version:  1.1.41.12
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
		Data: Pointer;
		FClearCreated: BG;
		FItemSize: UG;
		FItemSh: UG;
		FItemMemSize: UG;
		FItemCount: TIndex;
		FItemAlloc: SG; // FCapacity
		FObjectCounter: SG;
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
		function Insert(const Index: TIndex): Pointer; overload;
		procedure Add(const NewObject: TObject); overload;
		procedure SetCount(NewCount: SG);

		procedure DeleteFirst;
		procedure DeleteLast;
		procedure Delete(Index: TIndex);

//		procedure Insert(var Value; Index: TIndex); overload;
		procedure Replace(const Index: TIndex; const Item: Pointer);

//		procedure Replace(var Value; Index: TIndex);
		procedure Swap(const I1, I2: TIndex);

		// Data query
		function Get(const Index: TIndex): Pointer; overload;
		function GetFirst: Pointer; overload;
		function GetLast: Pointer; overload;

		procedure Next(var P); overload;

		function First: TObject;
		function Next: TObject; overload;

		function IsEmpty: Boolean;
		function ToString: string;

		property ItemSize: UG read FItemSize write SetItemSize;
		property ItemSh: UG read FItemSh; // Binary shift
		property ItemMemSize: UG read FItemMemSize; // Item size in memory
		property Index: SG read FObjectCounter;
		property Count: TIndex read FItemCount;
		property Items[const Index: TIndex]: Pointer read Get write Replace; default; // operator []

		// Import & Export
//		procedure Serialize(const IniFile: TDIniFile; const Save: BG);
	end;


implementation

uses
	Math,
	uMath;

{ TData }

constructor TData.Create(const ClearCreated: BG);
begin
	inherited Create;
	FClearCreated := ClearCreated;
	ItemSize := SizeOf(TObject);
	FItemCount := 0;
	FitemAlloc := 0;
end;

procedure TData.Clear;
begin
	FreeMem(Data); Data := nil;
	FItemCount := 0;
	FItemAlloc := 0;
end;

constructor TData.Create;
begin
	Create(True);
end;

destructor TData.Destroy;
begin
	Clear;
	inherited;
end;

function TData.Add: Pointer;
begin
	Result := Insert(FItemCount);
end;

function TData.Add(P: Pointer): Pointer;
begin
	Result := Add;
	PPointer(Result)^ := P;
end;

procedure TData.Add(const NewObject: TObject);
begin
	PPointer(Add)^ := NewObject;
end;

{procedure TData.Add(var Value);
begin
	Insert(Value, FItemCount);
end;}

procedure TData.Delete(Index: TIndex);
begin
	if (Index < FItemCount) then
	begin
		Move(Pointer(TIndex(Data) + (Index + 1) shl FItemSh)^,
			Pointer(TIndex(Data) + Index shl FItemSh)^, (FItemCount - Index - 1) shl FItemSh);
		Dec(FItemCount);
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
	NewItemCount := Max(Index, FItemCount) + 1;
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

function TData.Insert(const Index: TIndex): Pointer;
begin
	if FItemSize <> 0 then
	begin
		NewData(Index);
		Result := Pointer(TIndex(Data) + Index shl FItemSh);
	end
	else
		Result := nil;
end;

procedure TData.Replace(const Index: TIndex; const Item: Pointer);
var P: Pointer;
begin
	P := Get(Index);
	if (P <> nil) then
	begin
		Move(Item^{source}, P^{destination}, FItemSize);
	end;
end;

{
procedure TData.Replace(var Value; Index: TIndex);
begin
	if (Index < FItemCount) then
		Move(Value, Pointer(UG(Data) + Index shl FItemSh)^, ItemSize);
end;}

{procedure TData.Get(var Value; Index: TIndex);
begin
	if (Index < FItemCount) then
		Move(Pointer(UG(Data) + Index shl FItemSh)^, Value, ItemSize);
end;}

function TData.Get(const Index: TIndex): Pointer;
begin
	if (Index >= FItemCount) then
		Result := nil
	else
//		Move(Pointer(TIndex(Data) + Index shl FItemSh)^, Value^, ItemSize);
		Result := Pointer(TIndex(Data) + Index shl FItemSh);
end;

{procedure TData.GetFirst(var Value);
begin
	Get(Value, 0);
end;}

function TData.GetFirst: Pointer;
begin
	FObjectCounter := 0;
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

procedure TData.Next(var P);
begin
	Inc(FObjectCounter);
	Inc(SG(P), ItemMemSize);
	if SG(P) >= SG(Data) + SG(FItemMemSize) * FItemCount then
		Pointer(P) := nil;
end;

function TData.First: TObject;
begin
	FObjectCounter := 0;
	if FObjectCounter < FItemCount then
		Result := TObject(GetFirst^)
	else
		Result := nil;
end;

function TData.Next: TObject;
begin
	Inc(FObjectCounter);
	if FObjectCounter < FItemCount then
		Result := TObject(Get(FObjectCounter)^)
	else
		Result := nil;
end;

(*procedure TData.Serialize(const IniFile: TDIniFile; const Save: BG);
begin
	IniFile.RW
end; *)

end.
