// * File:     Lib\SortS.inc
// * Created:  2008-06-03
// * Modified: 2005-10-08
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

var
	AVal: array of TValue1;

(*-------------------------------------------------------------------------*)
var
	c, MaxL: SG;
	i, j, next_index : TIndex;
	count_index      : TValue1;
	Counts: array of record
		Vals: array of TIndex;
		ValCount: TIndex;
	end;
	NewSize: SG;
	min_value, max_value: TValue1;
	i2: SG;
begin
	MaxIndex := Length(AValue) - 1;
	{$ifopt d+}
	SortSwaped := 0; SortCompared := 0; SortMaxDepth := 0;
	{$endif}
	if MaxIndex > MinIndex then
	begin
		SetLength(AVal, MaxIndex + 1);
		MaxL := 0;
		for i := MinIndex to MaxIndex do
			if Length(AValue[i]) > MaxL then MaxL := Length(AValue[i]);

		min_Value := Low(TValue1);
		max_Value := High(TValue1);
		// Create the Counts array.
		SetLength(Counts, (max_value - min_value + 1));

		for c := MaxL downto 1 do
		begin
			for i := MinIndex to MaxIndex do
			begin
				if Length(AValue[AIndex[i]]) < c then
					AVal[i] := 0
				else
					AVal[i] := TValue1(AValue[AIndex[i]][c]);
			end;

			// CountingSort
			min_Value := High(TValue1);
			max_Value := Low(TValue1);
			for i := MinIndex to MaxIndex do
			begin
				if AVal[i] < min_value then min_value := AVal[i];
				if AVal[i] > max_value then max_value := AVal[i];
			end;

			// Create the Counts array.
//			SetLength(Counts, (max_value - min_value + 1));

			// Initialize the counts to zero.
			for i := 0 to max_value - min_value do
			begin
				Counts[i].ValCount := 0;
				Counts[i].Vals := nil;
			end;

			// Count the items.
			for i := MinIndex to MaxIndex do
			begin
				count_index := AVal[i] - min_value;
				NewSize := Counts[count_index].ValCount + 1;
				if AllocByExp(Length(Counts[count_index].Vals), NewSize) then
					SetLength(Counts[count_index].Vals, NewSize);
				Counts[count_index].Vals[Counts[count_index].ValCount] := AIndex[i];
				Inc(Counts[count_index].ValCount);
			end;

			// Place the items in the sorted array.
			next_index := MinIndex;
			for i2 := min_value to max_value do
			begin
				for j := 0 to Counts[i2 - SG(min_value)].ValCount - 1 do
				begin
					AVal[next_index] := i2;
					AIndex[next_index] := Counts[i2 - SG(min_value)].Vals[j];
					Inc(next_index);
				end;
			end;

		end;
		// Free the memory allocated for the counts array.
		SetLength(Counts, 0);
		SetLength(AVal, 0);
	end;
	if Reverse then
	begin
		Reverse4(AIndex[0], MaxIndex - MinIndex + 1);
{		for i := 0 to (MaxIndex - 1) div 2 do
		begin
			Exchange(AIndex[i], AIndex[MaxIndex - i]);
		end;}
	end;
end;
