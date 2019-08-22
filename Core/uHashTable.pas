unit uHashTable;

interface

uses
	uTypes,
	uData;

type
	TKey = U4;

	TArrayOfKey = array of TKey;

	THashTable = class
	private
		FCount: SG;
		FCapacity: SG; // Have to be 2^n
		FKeys: TData; // Used for resizing
		FValues: TData;
		FFindIndex: SG;
	public
		procedure Add(const Key: TKey; const Data: Pointer);
		procedure Clear;
		constructor Create(const InitialCapacity: SG; const ValueSize: SG);
		destructor Destroy; override;
		function Find(const Key: TKey): Pointer;
		function FindNext: Pointer;
		function Get(const Index: SG): Pointer;

		function LoadFactor: FG;
		function GetUsedKeyIndexes: TArrayOfKey;

		property Count: SG read FCount;
		property Capacity: SG read FCapacity;
	end;

implementation

uses
  SysUtils;

{ THashTable }

procedure THashTable.Add(const Key: U4; const Data: Pointer);
var Index: SG;
begin
	Assert(FCount <= FCapacity);
  {$ifopt d+}
	if Count >= Capacity div 8 then
  	raise Exception.Create('Increase hash table initial capacity.');
  {$endif}
	Inc(FCount);
	Index := Key mod UG(FCapacity);
  {$ifopt d+}
	if U4(FKeys[Index]^) <> 0 then
		raise Exception.Create('Duplicate item index.');
  {$endif}
	FKeys.Replace(Index, @Key);
	FValues.Replace(Index, Data);
end;

procedure THashTable.Clear;
begin
	FKeys.Clear;
	FValues.Clear;
	FCount := 0;
	FFindIndex := 0;
end;

constructor THashTable.Create(const InitialCapacity, ValueSize: SG);
begin
	FCapacity := InitialCapacity;

	FKeys := TData.Create;
	FKeys.ItemSize := SizeOf(TKey);

	FValues := TData.Create;
	FValues.ItemSize := ValueSize;

	FKeys.SetCount(InitialCapacity);
	FValues.SetCount(InitialCapacity);
end;

destructor THashTable.Destroy;
begin
	Clear;
	FreeAndNil(FKeys);
	FreeAndNil(FValues);
end;

function THashTable.Find(const Key: TKey): Pointer;
begin
	Result := nil;
	FFindIndex := Key mod UG(Capacity);
	if U4(FKeys[FFindIndex]^) = Key then
		Result := FValues[FFindIndex];
end;

function THashTable.FindNext: Pointer;
begin
//	Assert(FindIndex <> 0);
	Inc(FFindIndex);
	Result := nil; // TODO : FValues[FFindIndex];
end;

function THashTable.Get(const Index: SG): Pointer;
begin
	if (Index >= 0) and (Index < FCapacity) then
		Result := FValues[Index]
	else
		Result := nil;
end;

function THashTable.GetUsedKeyIndexes: TArrayOfKey;
var
	Count: SG;
	P: ^TKey;
begin
	SetLength(Result, FCapacity);
	Count := 0;
	P := FKeys.GetFirst;
	while P <> nil do
	begin
		if P^ <> 0 then
		begin
			Result[Count] := FKeys.Index;
			Inc(Count);
		end;
		FKeys.Next(P);
	end;
	SetLength(Result, Count);
end;

function THashTable.LoadFactor: FG;
begin
	if FCapacity <= 0 then
		Result := 1
	else
		Result := FCount / FCapacity;
end;

end.

