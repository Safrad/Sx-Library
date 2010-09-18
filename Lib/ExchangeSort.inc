// Complexity: k * (n^2), Changes k * n (Select Sort)
{
 ============================= ExchangeSort =================================
	 The ExchangeSort compares each element in SortArray - starting with
	 the first element - with every following element.  If any of the
	 following elements is smaller than the current element, it is exchanged
	 with the current element and the process is repeated for the next
	 element in SortArray.
 ============================================================================
}

	TIndex = Integer;
var
	HValue: TValue;
	HIndex: TIndex;
	i, j: Integer;
	SmallestRow: Integer;
	MaxIndex: Integer;
begin
	MaxIndex := Length(AIndex) - 1;
	for i := 0 to MaxIndex do
	begin
		SmallestRow := i;
		for J := i + 1 to MaxIndex do
		begin
			if AValue[J] < AValue[SmallestRow] then
			begin
				SmallestRow := J;
			end;
		end;
		{ Found a row shorter than the current row, so swap those
		two array elements: }
		if SmallestRow > i then
		begin
			HValue := AValue[i];
			AValue[i] := AValue[SmallestRow];
			AValue[SmallestRow] := HValue;
			HIndex := AIndex[i];
			AIndex[i] := AIndex[SmallestRow];
			AIndex[SmallestRow] := HIndex;
		end;
	end;
end;
